;;;; Tests for (DECLARE (SB-EXT:UNPIN-THREAD)) on x86-64: R13, which holds
;;;; the current thread, is allocated like any other register.

#-(and x86-64 sb-thread (not gs-seg)) (invoke-restart 'run-tests::skip-file)
#+interpreter (invoke-restart 'run-tests::skip-file)

(defun compile-capturing-unpin-notes (form)
  (let ((notes nil))
    (multiple-value-bind (fn warnings-p failure-p)
        (handler-bind ((sb-ext:compiler-note
                        (lambda (c)
                          (let ((msg (princ-to-string c)))
                            (when (search "Unable to unpin" msg)
                              (push msg notes))))))
          (compile nil form))
      (values fn (nreverse notes) warnings-p failure-p))))

(defun disassembly-contains-p (fn needle)
  (let ((text (with-output-to-string (*standard-output*)
                (disassemble fn))))
    (not (null (search needle text)))))

;;; Compile BODY (a lambda's declarations and forms) both ways.
;;; Return the pinned function, the unpinned one and the notes of the latter.
(defun compile-both-ways (lambda-list body)
  (let ((pinned (compile nil `(lambda ,lambda-list ,@body))))
    (multiple-value-bind (unpinned notes warnings-p failure-p)
        (compile-capturing-unpin-notes
         `(lambda ,lambda-list
            (declare (sb-ext:unpin-thread))
            ,@body))
      (assert (not warnings-p))
      (assert (not failure-p))
      (values pinned unpinned notes))))

(defparameter *pressure-vars*
  '(a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13))

;;; A loop that keeps more values live than there are registers when
;;; R13 isn't available, and that touches nothing of the thread.
(defun pressure-body ()
  `((declare (type (simple-array (unsigned-byte 64) (*)) buf)
             (type fixnum n)
             (optimize (speed 3) (safety 0) (debug 0) #+sb-safepoint (sb-c:insert-safepoints 0)))
    (let ,(loop for v in *pressure-vars*
                for i from 1
                collect `(,v ,i))
      (declare (type (unsigned-byte 64) ,@*pressure-vars*))
      (dotimes (i n)
        (let ((x (aref buf (logand i 255))))
          (setf ,@(loop for v in *pressure-vars*
                        for i from 1
                        append `(,v (logand (+ ,v (* x ,(+ i 2)) ,i)
                                            #xffffffffffffffff))))))
      (logand (+ ,@*pressure-vars*) most-positive-fixnum))))

(defun make-test-buffer ()
  (let ((buf (make-array 256 :element-type '(unsigned-byte 64))))
    (dotimes (i 256 buf)
      (setf (aref buf i) (1+ (* i 17))))))

(with-test (:name :unpin-thread-allocates-r13-in-leaf-loop)
  (let ((buf (make-test-buffer)))
    (multiple-value-bind (pinned unpinned notes)
        (compile-both-ways '(buf n) (pressure-body))
      (assert (null notes))
      (assert (not (disassembly-contains-p pinned "R13")))
      (assert (disassembly-contains-p unpinned "R13"))
      (assert (= (funcall unpinned buf 2000) (funcall pinned buf 2000)))
      ;; The thread register must have been put back: allocating and
      ;; binding a special both need it.
      (assert (equal (list (funcall unpinned buf 10)
                           (let ((*print-base* 16)) *print-base*))
                     (list (funcall pinned buf 10) 16))))))

(declaim (notinline opaque-mix)
         (ftype (function (t t) (unsigned-byte 32)) opaque-mix))
(defun opaque-mix (x y)
  (declare (type (unsigned-byte 32) x y))
  (logand (+ (* x 31) y) #xffffffff))

;;; Full calls from the unpinned code need the thread in R13 again for
;;; the callee: the loop below uses R13, the calls after it must not
;;; find it holding anything else, nor return with it changed.
(with-test (:name :unpin-thread-full-calls)
  (let ((buf (make-test-buffer))
        (body
         `((declare (type (simple-array (unsigned-byte 64) (*)) buf)
                    (type fixnum n)
                    (optimize (speed 3) (safety 0) (debug 0) #+sb-safepoint (sb-c:insert-safepoints 0)))
           (let ,(loop for v in *pressure-vars*
                       for i from 1
                       collect `(,v ,i))
             (declare (type (unsigned-byte 32) ,@*pressure-vars*))
             (dotimes (i n)
               (let ((x (logand (aref buf (logand i 255)) #xffffffff)))
                 (setf ,@(loop for v in *pressure-vars*
                               for i from 1
                               append `(,v (logand (+ ,v (* x ,i)) #xffffffff))))))
             (let ((s (logand (+ ,@*pressure-vars*) #xffffffff)))
               (declare (type (unsigned-byte 32) s))
               (1+ (opaque-mix (opaque-mix (opaque-mix s 1) 2)
                               (opaque-mix 3 s))))))))
    (multiple-value-bind (pinned unpinned notes)
        (compile-both-ways '(buf n) body)
      (assert (null notes))
      (assert (disassembly-contains-p unpinned "R13"))
      (dolist (n '(0 1 7 300))
        (assert (= (funcall unpinned buf n) (funcall pinned buf n))))
      (assert (= (let ((*print-base* 8)) (funcall unpinned buf 5))
                 (funcall pinned buf 5))))))

;;; Unknown values, and a tail call, return through the epilogue that
;;; puts R13 back.
(with-test (:name :unpin-thread-returns-restore-r13)
  (let ((buf (make-test-buffer)))
    (multiple-value-bind (pinned unpinned notes)
        (compile-both-ways
         '(buf n)
         `((declare (type (simple-array (unsigned-byte 64) (*)) buf)
                    (type fixnum n)
                    (optimize (speed 3) (safety 0) (debug 0) #+sb-safepoint (sb-c:insert-safepoints 0)))
           (let ,(loop for v in *pressure-vars*
                       for i from 1
                       collect `(,v ,i))
             (declare (type (unsigned-byte 32) ,@*pressure-vars*))
             (dotimes (i n)
               (let ((x (logand (aref buf (logand i 255)) #xffffffff)))
                 (setf ,@(loop for v in *pressure-vars*
                               for i from 1
                               append `(,v (logand (+ ,v (* x ,i))
                                                   #xffffffff))))))
             (values (logand (+ ,@*pressure-vars*) most-positive-fixnum)
                     a0 a13))))
      (assert (null notes))
      (dolist (n '(0 3 100))
        (assert (equal (multiple-value-list (funcall unpinned buf n))
                       (multiple-value-list (funcall pinned buf n)))))
      (let ((*print-base* 3))
        (funcall unpinned buf 3)
        (assert (= *print-base* 3))))))

;;; Anything that really needs the thread register, here a special
;;; variable, makes the component pinned again, with a note.
(with-test (:name :unpin-thread-falls-back-when-thread-is-needed)
  (let ((buf (make-test-buffer))
        (body
         `((declare (type (simple-array (unsigned-byte 64) (*)) buf)
                    (type fixnum n)
                    (optimize (speed 3) (safety 0) (debug 0) #+sb-safepoint (sb-c:insert-safepoints 0)))
           (let ,(loop for v in *pressure-vars*
                       for i from 1
                       collect `(,v ,i))
             (declare (type (unsigned-byte 64) ,@*pressure-vars*))
             (dotimes (i n)
               (let ((x (aref buf (logand i 255))))
                 (setf ,@(loop for v in *pressure-vars*
                               for i from 1
                               append `(,v (logand (+ ,v (* x ,i)
                                                      (the (integer 0 1000) *print-base*))
                                                   #xffffffffffffffff))))))
             (logand (+ ,@*pressure-vars*) most-positive-fixnum)))))
    (let ((pinned (compile nil `(lambda (buf n) ,@body))))
      (multiple-value-bind (unpinned notes)
          (compile-capturing-unpin-notes
           `(lambda (buf n)
              (declare (sb-ext:unpin-thread))
              ,@body))
        (assert (= (length notes) 1))
        (assert (search "the thread" (first notes)))
        (dolist (n '(0 5 50))
          (assert (= (funcall unpinned buf n) (funcall pinned buf n))))))))

;;; Both registers can be unpinned at once.
(with-test (:name :unpin-thread-and-nil-together)
  (let ((buf (make-test-buffer)))
    (let ((pinned (compile nil `(lambda (buf n) ,@(pressure-body)))))
      (multiple-value-bind (both notes)
          (compile-capturing-unpin-notes
           `(lambda (buf n)
              (declare (sb-ext:unpin-nil) (sb-ext:unpin-thread))
              ,@(pressure-body)))
        (assert (null notes))
        (assert (disassembly-contains-p both "R12"))
        (assert (disassembly-contains-p both "R13"))
        (assert (= (funcall both buf 1000) (funcall pinned buf 1000)))
        (assert (null nil))
        (assert (equal (cons 1 nil) '(1)))))))

;;; Allocation needs the thread too: the allocators put it back into R13
;;; for themselves, whatever a live value in R13 around them would be.
(with-test (:name :unpin-thread-allocation)
  (let* ((buf (make-test-buffer))
         (body
          `((declare (type (simple-array (unsigned-byte 64) (*)) buf)
                     (type fixnum n)
                     (optimize (speed 3) (safety 0) (debug 0) #+sb-safepoint (sb-c:insert-safepoints 0)))
            (let ((acc nil)
                  ,@(loop for v in *pressure-vars*
                          for i from 1
                          collect `(,v ,i)))
              (declare (type (unsigned-byte 32) ,@*pressure-vars*))
              (dotimes (i n)
                (let ((x (logand (aref buf (logand i 255)) #xffffffff)))
                  (setf ,@(loop for v in *pressure-vars*
                                for i from 1
                                append `(,v (logand (+ ,v (* x ,i)) #xffffffff))))
                  (when (zerop (logand i 63))
                    (push (list a0 (coerce a13 'double-float)) acc))))
              (values (logand (+ ,@*pressure-vars*) most-positive-fixnum)
                      (reverse acc)
                      (coerce a5 'double-float))))))
    (multiple-value-bind (pinned unpinned notes)
        (compile-both-ways '(buf n) body)
      (assert (null notes))
      (assert (disassembly-contains-p unpinned "R13"))
      (dolist (n '(0 1 64 5000))
        (assert (equal (multiple-value-list (funcall unpinned buf n))
                       (multiple-value-list (funcall pinned buf n)))))
      ;; Make the collector run while such frames are live.
      (let ((r nil))
        (dotimes (i 30)
          (setq r (multiple-value-list (funcall unpinned buf 20000)))
          (when (zerop (mod i 10)) (gc)))
        (assert (equal r (multiple-value-list (funcall pinned buf 20000))))))))

(defun pressure-loop-then (final)
  `((declare (type (simple-array (unsigned-byte 64) (*)) buf)
             (type fixnum n) (type function f)
             (optimize (speed 3) (safety 0) (debug 0) #+sb-safepoint (sb-c:insert-safepoints 0)))
    (let ,(loop for v in *pressure-vars*
                for i from 1
                collect `(,v ,i))
      (declare (type (unsigned-byte 32) ,@*pressure-vars*))
      (dotimes (i n)
        (let ((x (logand (aref buf (logand i 255)) #xffffffff)))
          (setf ,@(loop for v in *pressure-vars*
                        for i from 1
                        append `(,v (logand (+ ,v (* x ,i)) #xffffffff))))))
      (let ((s (logand (+ ,@*pressure-vars*) #xffffffff)))
        (declare (type (unsigned-byte 32) s))
        ,final))))

;;; A tail call out of the unpinned code reloads R13 before the jump, and
;;; there is no return to restore it.
(with-test (:name :unpin-thread-tail-call)
  (let ((buf (make-test-buffer)))
    (multiple-value-bind (pinned unpinned notes)
        (compile-both-ways '(buf n f) (pressure-loop-then '(funcall f s)))
      (assert (null notes))
      (assert (disassembly-contains-p unpinned "R13"))
      (dolist (n '(0 9 250))
        (assert (equal (multiple-value-list (funcall unpinned buf n #'list))
                       (multiple-value-list (funcall pinned buf n #'list)))))
      ;; The callee conses and binds a special, which need the thread.
      (let ((g (lambda (s) (let ((*print-base* 7)) (list s *print-base* (cons s s))))))
        (assert (equal (funcall unpinned buf 100 g) (funcall pinned buf 100 g)))
        (assert (= *print-base* 10))))))

;;; A throw out of a callee of the unpinned code lands in a pinned frame,
;;; which must still find the thread in R13 (specials, consing).
(with-test (:name :unpin-thread-throw-through)
  (let ((buf (make-test-buffer)))
    (multiple-value-bind (pinned unpinned notes)
        (compile-both-ways '(buf n f)
                           (pressure-loop-then '(1+ (the fixnum (funcall f s)))))
      (assert (null notes))
      (assert (disassembly-contains-p unpinned "R13"))
      (flet ((run (fn)
               (let ((*print-base* 2))
                 (let ((r (catch 'out (funcall fn buf 50 (lambda (s) (throw 'out (list s)))))))
                   (assert (= *print-base* 2))
                   (list r (cons 1 *print-base*) (make-list 3 :initial-element r))))))
        (assert (equal (run unpinned) (run pinned))))
      ;; And an error handled farther up.
      (dotimes (i 3)
        (assert (eq :caught
                    (handler-case (funcall unpinned buf 10 (lambda (s) (error "x ~a" s)))
                      (error () :caught))))
        (assert (equal (list (cons i i)) (list (cons i i))))))))

(declaim (notinline count-args))
(defun count-args (&rest args) (length args))

;;; Receiving an unknown number of values from a call, with the thread
;;; unpinned: the values are unpacked using the thread's return-value area.
(with-test (:name :unpin-thread-receives-unknown-values)
  (let ((buf (make-test-buffer)))
    (multiple-value-bind (pinned unpinned notes)
        (compile-both-ways
         '(buf n f)
         (pressure-loop-then '(multiple-value-call #'count-args (funcall f s))))
      (assert (null notes))
      (dolist (f (list (lambda (s) (declare (ignore s)) (values))
                       (lambda (s) (values s s s s s))
                       (lambda (s) (values-list (make-list (mod s 20) :initial-element s)))))
        (assert (= (funcall unpinned buf 30 f) (funcall pinned buf 30 f)))))))

;;; Returning words too big for a fixnum conses bignums through an assembly
;;; routine, which needs the thread. Only three values are returned: with
;;; --with-tls-based-mv-return a fourth goes in the thread's return area,
;;; which needs the thread, and the function would fall back to a pinned one.
(with-test (:name :unpin-thread-boxes-bignums)
  (let ((buf (make-test-buffer)))
    (multiple-value-bind (pinned unpinned notes)
        (compile-both-ways
         '(buf n)
         `((declare (type (simple-array (unsigned-byte 64) (*)) buf)
                    (type fixnum n)
                    (optimize (speed 3) (safety 0) (debug 0) #+sb-safepoint (sb-c:insert-safepoints 0)))
           (let ,(loop for v in *pressure-vars*
                       for i from 1
                       collect `(,v ,(logand (* i #x1234567890abcdef) #xffffffffffffffff)))
             (declare (type (unsigned-byte 64) ,@*pressure-vars*))
             (dotimes (i n)
               (let ((x (aref buf (logand i 255))))
                 (setf ,@(loop for v in *pressure-vars*
                               for i from 1
                               append `(,v (logand (+ ,v (* x ,(+ i 2000000000)))
                                                   #xffffffffffffffff))))))
             (values a0 a13 (logand (+ a1 a2) #xffffffffffffffff)))))
      (assert (null notes))
      (assert (disassembly-contains-p unpinned "R13"))
      (dolist (n '(0 1 77 3000))
        (let ((got (multiple-value-list (funcall unpinned buf n))))
          (assert (equal got (multiple-value-list (funcall pinned buf n))))
          (when (> n 1) (assert (some (lambda (x) (typep x '(and integer (not fixnum)))) got)))))
      (let ((*print-base* 5))
        (funcall unpinned buf 10)
        (assert (= *print-base* 5))))))

;;; Stack allocation checks for stack overflow against the thread's stack
;;; limit.
(with-test (:name :unpin-thread-stack-allocation)
  (let ((buf (make-test-buffer)))
    (multiple-value-bind (pinned unpinned notes)
        (compile-both-ways
         '(buf n)
         `((declare (type (simple-array (unsigned-byte 64) (*)) buf)
                    (type fixnum n)
                    (optimize (speed 3) (safety 0) (debug 0) #+sb-safepoint (sb-c:insert-safepoints 0)))
           (let ,(loop for v in *pressure-vars*
                       for i from 1
                       collect `(,v ,i))
             (declare (type (unsigned-byte 32) ,@*pressure-vars*))
             (dotimes (i n)
               (let* ((x (logand (aref buf (logand i 255)) #xffffffff))
                      (l (list x i))
                      (v (make-array 3 :element-type '(unsigned-byte 32)
                                       :initial-element (logand x #xffff))))
                 (declare (dynamic-extent l v))
                 (setf ,@(loop for var in *pressure-vars*
                               for i from 1
                               append `(,var (logand (+ ,var (* x ,i)
                                                        (the fixnum (car l))
                                                        (aref v 1))
                                                     #xffffffff))))))
             (logand (+ ,@*pressure-vars*) most-positive-fixnum))))
      ;; Values live across a stack allocation can't be in R13, so the
      ;; register may go unused here; what matters is no fallback.
      (assert (null notes))
      (dolist (n '(0 1 99 2000))
        (assert (= (funcall unpinned buf n) (funcall pinned buf n)))))))

;;; With both registers unpinned, boxing a bignum puts NIL and the thread
;;; back for the assembly routine, and leaves both usable afterwards.
(with-test (:name :unpin-thread-and-nil-box-bignums)
  (let ((buf (make-test-buffer))
        (body
         `((declare (type (simple-array (unsigned-byte 64) (*)) buf)
                    (type fixnum n)
                    (optimize (speed 3) (safety 0) (debug 0) #+sb-safepoint (sb-c:insert-safepoints 0)))
           (let ,(loop for v in *pressure-vars*
                       for i from 1
                       collect `(,v ,(logand (* i #x1234567890abcdef) #xffffffffffffffff)))
             (declare (type (unsigned-byte 64) ,@*pressure-vars*))
             (dotimes (i n)
               (let ((x (aref buf (logand i 255))))
                 (setf ,@(loop for v in *pressure-vars*
                               for i from 1
                               append `(,v (logand (+ ,v (* x ,(+ i 2000000000)))
                                                   #xffffffffffffffff))))))
             (values a0 a13 (logand (+ a1 a2) #xffffffffffffffff))))))
    (let ((pinned (compile nil `(lambda (buf n) ,@body))))
      (multiple-value-bind (both notes)
          (compile-capturing-unpin-notes
           `(lambda (buf n)
              (declare (sb-ext:unpin-nil) (sb-ext:unpin-thread))
              ,@body))
        (assert (null notes))
        (assert (disassembly-contains-p both "R12"))
        (dolist (n '(0 1 77 3000))
          (assert (equal (multiple-value-list (funcall both buf n))
                         (multiple-value-list (funcall pinned buf n)))))
        (assert (null nil))
        (assert (equal (cons 1 nil) '(1)))
        (assert (eq (car (list nil)) nil))))))
