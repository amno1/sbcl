;;;; Concurrent lookup in a perfectly hashed symbol table while it is
;;;; converted back to open addressing.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(defun cl-externals-perfectly-hashed-p ()
  (functionp (car (sb-impl::symtbl-%cells
                   (sb-impl::package-external-symbols (find-package "CL"))))))

;;; EXPORTing a new symbol from CL turns its perfectly hashed external
;;; symbol table back into an open-addressing table, and the resize fills
;;; the old vector with 0. A reader still probing the old vector used to
;;; treat that 0 as a symbol (a memory fault), and a miss in the perfect
;;; hash was returned without noticing that the table had changed.
(defun perfect-hash-conversion-trial (iteration names syms n-threads)
  (declare (simple-vector names syms))
  (let* ((cl (find-package "CL"))
         (new-name (format nil "PERFECT-HASH-RACE-~D" iteration))
         (published nil)
         (stop nil)
         (start-sem (sb-thread:make-semaphore))
         (threads
           (loop repeat n-threads
                 collect
                 (sb-thread:make-thread
                  (lambda ()
                    (sb-thread:wait-on-semaphore start-sem)
                    (let ((bad 0))
                      (loop until stop
                            do (loop for n across names
                                     for s across syms
                                     unless (eq (find-symbol n cl) s)
                                       do (incf bad))
                               (when (and published
                                          (not (find-symbol new-name cl)))
                                 (incf bad)))
                      bad))))))
    (sb-thread:signal-semaphore start-sem n-threads)
    (sleep 0.001)
    (without-package-locks
      (export (intern new-name cl) cl))
    (setf published t)
    (sleep 0.002)
    (setf stop t)
    (reduce #'+ (mapcar #'sb-thread:join-thread threads))))

(with-test (:name (find-symbol :perfect-hash :concurrent-export)
            :skipped-on (:not :sb-thread))
  (let* ((names (let (l)
                  (do-external-symbols (s "CL") (push (copy-seq (symbol-name s)) l))
                  (coerce l 'simple-vector)))
         (syms (map 'simple-vector (lambda (n) (find-symbol n "CL")) names)))
    (dotimes (i 50)
      ;; Make CL perfectly hashed again, as SAVE-LISP-AND-DIE does.
      (sb-impl::tune-hashset-sizes-of-all-packages)
      (unless (cl-externals-perfectly-hashed-p)
        (return))
      (assert (= (perfect-hash-conversion-trial i names syms 4) 0)))))
