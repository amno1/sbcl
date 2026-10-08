;;;; Live AVX-512 values must survive calls that preserve registers only
;;;; partially: allocation trampolines, alien calls, and :RAW assembly
;;;; routines whose FPR save covers only xmm/ymm0-15.

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

#-sb-simd-pack-512 (invoke-restart 'run-tests::skip-file)
#+interpreter (invoke-restart 'run-tests::skip-file)

(when (zerop (sb-alien:extern-alien "avx512_supported" int))
  (format t "~&INFO: simd-pack-512 not supported")
  (invoke-restart 'run-tests::skip-file))

(handler-case (require :sb-simd)
  (condition (c)
    (cond ((search "Don't know how" (princ-to-string c))
           (format t "~&Skipping test of sb-simd~%")
           (invoke-restart 'run-tests::skip-file))
          (t
           (error "Unexpected error: ~A" c)))))

(defpackage "SIMD-SAVE-TEST" (:use "CL" "SB-SIMD-AVX512F" "TEST-UTIL"))
(in-package "SIMD-SAVE-TEST")

;;; Overwrite zmm0-31 and k1-7 with all ones.
(sb-c:defknown trash-vector-regs () (values) ())
(sb-vm::define-vop (trash-vector-regs)
  (:translate trash-vector-regs)
  (:generator 1
    (loop for i below 32
          for r = (sb-c:make-random-tn (sb-c:sc-or-lose 'sb-vm::int-avx512-reg) i)
          do (sb-vm::inst vpternlogd r r r #xff))
    (loop for i from 1 to 7
          for k = (sb-c:make-random-tn (sb-c:sc-or-lose 'sb-vm::mask-reg) i)
          do (sb-vm::inst kxnorq k k k))))
(defun trash () (trash-vector-regs) (values))

;;; glibc's string functions use EVEX code touching ymm16-31 on AVX-512 CPUs
(defvar *buf* (sb-alien:make-alien (sb-alien:unsigned 8) 65536))
(declaim (inline libc-memset))
(defun libc-memset (sap)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "memset" (function sb-sys:system-area-pointer
                                             sb-sys:system-area-pointer
                                             sb-alien:int sb-alien:unsigned-long))
   sap 0 65536))

;;; Hold N live u64.8 values, value I being broadcast(BASE+I), across FORM,
;;; then store them into OUT. Nothing after FORM may allocate, as that would
;;; save the values for reasons unrelated to FORM.
(defmacro with-live-zmm ((n out) form)
  (let ((vs (loop for i below n collect (gensym "V"))))
    `(let ,(loop for v in vs for i from 1 collect `(,v (u64.8-broadcast (+ base ,i))))
       (let ((z (u64.8-broadcast (if ,form 0 1)))) ; uses come after FORM
         ,@(loop for v in vs for j from 0
                 collect `(setf (u64.8-aref ,out ,(* 8 j)) (u64.8+ z ,v)))
         ,out))))

(defun corrupted-values (out n)
  (loop for i from 1 to n
        unless (loop for j from (* 8 (1- i)) below (* 8 i)
                     always (= (aref out j) i))
          collect i))

(defun make-out (n) (make-array (* 8 n) :element-type '(unsigned-byte 64)))

;;; Repeat a case whose callee only sometimes reaches C (e.g. region refill)
(defun corrupted-iterations (fn n iterations)
  (let ((out (make-out n)))
    (loop repeat iterations
          count (corrupted-values (funcall fn 0 out) n))))

;;;; Alien calls

(defun alien-case (base out sap)
  (declare (optimize speed (safety 0))
           (type (unsigned-byte 32) base)
           (type (simple-array (unsigned-byte 64) (*)) out)
           (type sb-sys:system-area-pointer sap))
  (with-live-zmm (24 out) (progn (libc-memset sap) t)))

(with-test (:name (:simd-pack-512 :save :alien-funcall))
  (let ((out (alien-case 0 (make-out 24) (sb-alien:alien-sap *buf*))))
    (assert (null (corrupted-values out 24)))))

;;;; Allocation

(defvar *sink* nil)
(defun alloc-case (base out)
  (declare (optimize speed (safety 0))
           (type (unsigned-byte 32) base)
           (type (simple-array (unsigned-byte 64) (*)) out))
  (with-live-zmm (24 out) (progn (setq *sink* (make-array 300)) t)))

(with-test (:name (:simd-pack-512 :save :allocation))
  (assert (zerop (corrupted-iterations #'alloc-case 24 100000))))

;;;; UPDATE-OBJECT-LAYOUT

;;; Redefining the class makes *STREAM*'s layout obsolete, so the inline
;;; (TYPEP S 'STREAM) check calls UPDATE-OBJECT-LAYOUT, which runs
;;; UPDATE-INSTANCE-FOR-REDEFINED-CLASS.
(defclass save-test-stream (sb-gray:fundamental-stream) ())
(defvar *stream* (make-instance 'save-test-stream))
(defvar *clobber* #'trash)
(defmethod update-instance-for-redefined-class :before
    ((s save-test-stream) added discarded plist &rest initargs)
  (declare (ignore added discarded plist initargs))
  (funcall *clobber*))

(defvar *generation* 0)
(defun obsolete-stream-layout ()
  ;; each redefinition must differ, or the layout stays valid
  (let ((slot (intern (format nil "SLOT~D" (incf *generation*)))))
    (eval `(defclass save-test-stream (sb-gray:fundamental-stream)
             ((,slot :initform 0))))))

(defmacro def-layout-case (name n)
  `(defun ,name (s base out)
     (declare (optimize speed (safety 0)) (type (unsigned-byte 32) base)
              (type (simple-array (unsigned-byte 64) (*)) out))
     (with-live-zmm (,n out) (typep s 'stream))))
(def-layout-case layout-case-16 16) ; zmm0-15
(def-layout-case layout-case-32 32) ; zmm16-31 too

(defun layout-case (fn n clobber)
  (obsolete-stream-layout)
  (let ((*clobber* clobber))
    (corrupted-values (funcall fn *stream* 0 (make-out n)) n)))

(with-test (:name (:simd-pack-512 :save :update-object-layout :zmm0-15))
  (assert (null (layout-case #'layout-case-16 16 #'trash))))

(with-test (:name (:simd-pack-512 :save :update-object-layout :zmm16-31))
  (assert (null (layout-case #'layout-case-32 32 #'trash))))

(with-test (:name (:simd-pack-512 :save :update-object-layout :libc))
  (assert (null (layout-case #'layout-case-32 32
                             (lambda () (libc-memset (sb-alien:alien-sap *buf*)))))))

(defun layout-kmask-case (s base out)
  (declare (optimize speed (safety 0)) (type (unsigned-byte 32) base)
           (type (simple-array (unsigned-byte 64) (*)) out))
  (let ((k1 (sb-ext:%make-simd-pack-512-mask (+ base 1)))
        (k2 (sb-ext:%make-simd-pack-512-mask (+ base 2)))
        (k3 (sb-ext:%make-simd-pack-512-mask (+ base 3)))
        (k4 (sb-ext:%make-simd-pack-512-mask (+ base 4)))
        (k5 (sb-ext:%make-simd-pack-512-mask (+ base 5)))
        (k6 (sb-ext:%make-simd-pack-512-mask (+ base 6)))
        (k7 (sb-ext:%make-simd-pack-512-mask (+ base 7))))
    (let ((d (if (typep s 'stream) 0 1)))
      (macrolet ((store (&rest ks)
                   `(progn ,@(loop for k in ks for i from 0
                                   collect `(setf (aref out ,i)
                                                  (+ d (sb-kernel:%simd-pack-512-mask-value ,k)))))))
        (store k1 k2 k3 k4 k5 k6 k7))
      out)))

(with-test (:name (:simd-pack-512 :save :update-object-layout :kmask))
  (obsolete-stream-layout)
  (let ((out (layout-kmask-case *stream* 0 (make-array 7 :element-type '(unsigned-byte 64)))))
    (assert (equalp out #(1 2 3 4 5 6 7)))))

;;;; INSTALL-HASH-TABLE-LOCK

;;; The lock is created lazily; making the mutex occasionally reaches C
(defun hash-lock-case (base out)
  (declare (optimize speed (safety 0))
           (type (unsigned-byte 32) base)
           (type (simple-array (unsigned-byte 64) (*)) out))
  (let ((ht (make-hash-table)))
    (with-live-zmm (24 out) (sb-ext:with-locked-hash-table (ht) t))))

(with-test (:name (:simd-pack-512 :save :install-hash-table-lock))
  (assert (zerop (corrupted-iterations #'hash-lock-case 24 200000))))

;;;; SWITCH-TO-ARENA

#+system-tlabs
(progn
(defvar *arena* (sb-vm:new-arena (* 1024 1024)))
(defun arena-case (base out)
  (declare (optimize speed (safety 0))
           (type (unsigned-byte 32) base)
           (type (simple-array (unsigned-byte 64) (*)) out))
  (with-live-zmm (24 out) (progn (sb-vm:with-arena (*arena*) nil) t)))

;; Not known to fail; the C callee doesn't happen to touch vector registers
(with-test (:name (:simd-pack-512 :save :switch-to-arena))
  (assert (zerop (corrupted-iterations #'arena-case 24 10000)))))
