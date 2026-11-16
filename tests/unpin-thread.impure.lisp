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
             (optimize (speed 3) (safety 0) (debug 0)))
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
                    (optimize (speed 3) (safety 0) (debug 0)))
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
                    (optimize (speed 3) (safety 0) (debug 0)))
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
                    (optimize (speed 3) (safety 0) (debug 0)))
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
                     (optimize (speed 3) (safety 0) (debug 0)))
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
