;;;; Two-part symbol tables: a perfectly hashed part built by
;;;; PERFECT-SYMBOL-TABLE (at save time, or on demand), plus an
;;;; open-addressing part for symbols added later.

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

(defun perfect-part (table) (sb-impl::symtbl-perfect table))

(defun make-perfect-package (name n)
  "Make package NAME with N internal and N external symbols, both tables
perfectly hashed."
  (let ((p (or (find-package name) (make-package name :use nil))))
    (dotimes (i n)
      (intern (format nil "INT-~D" i) p)
      (export (intern (format nil "EXT-~D" i) p) p))
    (sb-thread:with-recursive-lock (sb-impl::*package-graph-lock*)
      (assert (sb-impl::perfect-symbol-table (sb-impl::package-internal-symbols p)))
      (assert (sb-impl::perfect-symbol-table (sb-impl::package-external-symbols p))))
    p))

(defun package-symbol-set (p)
  (let (l) (do-symbols (s p) (push s l)) l))

(with-test (:name (find-symbol :perfect-hash :sequential))
  (let* ((p (make-perfect-package "PERFECT-SEQ" 500))
         (int (sb-impl::package-internal-symbols p))
         (ext (sb-impl::package-external-symbols p))
         (perfect-cells (sb-impl::psymtbl-cells (perfect-part ext)))
         (perfect-copy (copy-seq perfect-cells)))
    (assert (perfect-part int))
    (assert (= (sb-impl::%symtbl-count ext) 500))
    (assert (eq (find-symbol "EXT-7" p) (find-symbol "EXT-7" p)))
    (assert (eq (nth-value 1 (find-symbol "EXT-7" p)) :external))
    (assert (eq (nth-value 1 (find-symbol "INT-7" p)) :internal))
    (assert (null (find-symbol "NOPE" p)))
    ;; INTERN goes to the open-addressing part; the perfect part stays.
    (let ((new (intern "NEW-SYMBOL" p)))
      (assert (eq (find-symbol "NEW-SYMBOL" p) new))
      (assert (perfect-part int)))
    ;; EXPORT and UNEXPORT move symbols between the tables.
    (let ((s (find-symbol "INT-3" p)))
      (export s p)
      (assert (eq (nth-value 1 (find-symbol "INT-3" p)) :external))
      (unexport s p)
      (assert (eq (nth-value 1 (find-symbol "INT-3" p)) :internal)))
    ;; UNINTERN from the perfect part marks it deleted without writing the
    ;; perfect vector, which may be in read-only space.
    (let ((s (find-symbol "EXT-9" p)))
      (assert (unintern s p))
      (assert (null (find-symbol "EXT-9" p)))
      (assert (null (symbol-package s)))
      (assert (= (sb-impl::%symtbl-count ext) 499))
      (assert (not (member s (package-symbol-set p))))
      ;; Re-interning the name makes a new symbol in the open-addressing part.
      (let ((s2 (intern "EXT-9" p)))
        (assert (not (eq s s2)))
        (assert (eq (find-symbol "EXT-9" p) s2))))
    (assert (eq (sb-impl::psymtbl-cells (perfect-part ext)) perfect-cells))
    (assert (equalp perfect-cells perfect-copy))
    ;; DO-SYMBOLS sees each symbol once.
    (let ((all (package-symbol-set p)))
      (assert (= (length all) (length (remove-duplicates all)))))
    ;; Merging folds both parts into a new perfect part.
    (let ((before (sort (mapcar #'symbol-name (package-symbol-set p)) #'string<)))
      (sb-thread:with-recursive-lock (sb-impl::*package-graph-lock*)
        (assert (sb-impl::perfect-symbol-table int))
        (assert (sb-impl::perfect-symbol-table ext)))
      (assert (null (sb-impl::psymtbl-deleted (perfect-part ext))))
      (assert (equal before
                     (sort (mapcar #'symbol-name (package-symbol-set p)) #'string<))))))

;;; Readers look up a stable set of symbols, which must always be found,
;;; while a writer interns, exports, unexports and uninterns other symbols
;;; and periodically merges the tables into a new perfect part.
;;; Symbols the writer has published must also be found.
(with-test (:name (find-symbol :perfect-hash :concurrent)
            :skipped-on (:not :sb-thread))
  (let* ((p (make-perfect-package "PERFECT-CONC" 1000))
         (stable-names (loop for i below 1000 by 2
                             collect (format nil "EXT-~D" i)
                             collect (format nil "INT-~D" i)))
         (stable (map 'vector (lambda (n) (find-symbol n p)) stable-names))
         (stable-names (coerce stable-names 'vector))
         (published (make-array 2000 :initial-element nil))
         (n-published 0)
         (stop nil)
         (threads
           (loop repeat 4
                 collect
                 (sb-thread:make-thread
                  (lambda ()
                    (let ((bad 0))
                      (loop until stop
                            do (loop for n across stable-names
                                     for s across stable
                                     unless (eq (find-symbol n p) s)
                                       do (incf bad))
                               (dotimes (i n-published)
                                 (let ((name (svref published i)))
                                   (unless (find-symbol name p)
                                     (incf bad))))
                               (let ((seen (make-hash-table :test 'eq)))
                                 (do-symbols (s p) (setf (gethash s seen) t))
                                 (loop for s across stable
                                       unless (gethash s seen)
                                         do (incf bad))))
                      bad))))))
    (loop for round below 20
          do (loop for i from (* round 50) below (* (1+ round) 50)
                   for k = (+ 1 (* 2 i))
                   do (let ((ext (find-symbol (format nil "EXT-~D" k) p))
                            (int (find-symbol (format nil "INT-~D" k) p)))
                        (when ext (unintern ext p))
                        (when int (export int p)))
                      (let ((name (format nil "NEW-~D" i)))
                        (intern name p)
                        (setf (svref published n-published) name)
                        (sb-thread:barrier (:write))
                        (incf n-published)))
             (when (evenp round)
               (sb-thread:with-recursive-lock (sb-impl::*package-graph-lock*)
                 (sb-impl::perfect-symbol-table (sb-impl::package-internal-symbols p))
                 (sb-impl::perfect-symbol-table (sb-impl::package-external-symbols p)))))
    (setf stop t)
    (assert (= 0 (reduce #'+ (mapcar #'sb-thread:join-thread threads))))))
