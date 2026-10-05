(in-package #:sb-simd-internals)

;;; For each value record we define a function of the same name that will
;;; either suitably convert its argument to that value record's type, or
;;; signal an error.

(defgeneric mask-value (mask)
  (:documentation "Returns the integer bit pattern of MASK."))

#+x86-64
(defmethod mask-value ((mask sb-ext:simd-pack-512-mask))
  (sb-kernel:%simd-pack-512-mask-value mask))

#-x86-64
(defmethod mask-value ((mask phony-simd-pack-512-mask))
  (phony-simd-pack-512-mask-value mask))

(macrolet
    ((call-vop (instruction-record-name &rest arguments)
       (with-accessors ((instruction-set instruction-record-instruction-set)
                        (vop instruction-record-vop))
           (find-function-record instruction-record-name)
         (if (instruction-set-available-p instruction-set)
             `(,vop ,@arguments)
             `(progn
                (missing-instruction
                 (load-time-value
                  (find-function-record ',instruction-record-name)))
                (touch ,@arguments)))))
     (define-scalar-cast (scalar-cast-record-name)
       (with-accessors ((name scalar-cast-record-name)
                        (instruction-set scalar-cast-record-instruction-set))
           (find-function-record scalar-cast-record-name)
         (let ((err (mksym (symbol-package name) "CANNOT-CONVERT-TO-" name)))
           `(progn
              (define-notinline ,err (x)
                (error "Cannot convert ~S to ~S." x ',name))
              (sb-c:defknown ,name (t) (values ,name &optional)
                  (,@(when (instruction-set-available-p instruction-set)
                       '(sb-c:foldable)))
                :overwrite-fndb-silently t)
              (sb-c:deftransform ,name ((x) (,name) *)
                'x)
              ,@(when (instruction-set-available-p instruction-set)
                  (case name
                    (sb-simd:f32
                     `((sb-c:deftransform ,name ((x) (double-float) *)
                         '(coerce x 'single-float))))
                    (sb-simd:f64
                     `((sb-c:deftransform ,name ((x) (single-float) *)
                         '(coerce x 'double-float))))
                    #+x86-64
                    (sb-simd-sse:f32
                     `((sb-c:deftransform ,name ((x) (double-float) *)
                         '(sb-kernel:%single-float x))
                       (sb-c:deftransform ,name ((x) ((signed-byte 64)) *)
                         '(sb-simd-sse::f32-from-s64 x))))
                    #+x86-64
                    (sb-simd-sse2:f64
                     `((sb-c:deftransform ,name ((x) (single-float) *)
                         '(sb-simd-sse2::f64-from-f32 x))
                       (sb-c:deftransform ,name ((x) ((signed-byte 64)) *)
                         '(sb-simd-sse2::f64-from-s64 x))))
                    #+x86-64
                    (sb-simd-avx:f32
                     `((sb-c:deftransform ,name ((x) (double-float) *)
                         '(sb-simd-avx::f32-from-f64 x))
                       (sb-c:deftransform ,name ((x) ((signed-byte 64)) *)
                         '(sb-simd-avx::f32-from-s64 x))))
                    #+x86-64
                    (sb-simd-avx:f64
                     `((sb-c:deftransform ,name ((x) (single-float) *)
                         '(sb-simd-avx::f64-from-f32 x))
                       (sb-c:deftransform ,name ((x) ((signed-byte 64)) *)
                         '(sb-simd-avx::f64-from-s64 x))))
                    #+x86-64
                    ((sb-simd-avx512f:m64.8
                      sb-simd-avx512f:m32.16
                      sb-simd-avx512bw:m16.32
                      sb-simd-avx512bw:m8.64)
                     `((sb-c:deftransform ,name ((x) ((unsigned-byte 64)) *)
                         '(sb-ext:%make-simd-pack-512-mask x))))))
              (defun ,name (x)
                (typecase x
                  (,name x)
                  ,@(case name
                      (sb-simd:f32
                       `((double-float (coerce x 'single-float))
                         (real (coerce x ',name))))
                      (sb-simd:f64
                       `(#+x86-64
                         (sb-simd-sse2:f32 (coerce x 'double-float))
                         (real (coerce x ',name))))
                      #+x86-64
                      (sb-simd-sse:f32
                       `((double-float (sb-kernel:%single-float x))
                         (sb-simd-sse:s64 (call-vop sb-simd-sse::f32-from-s64 x))
                         (real (coerce x ',name))))
                      #+x86-64
                      (sb-simd-sse2:f64
                       `((sb-simd-sse2:f32 (call-vop sb-simd-sse2::f64-from-f32 x))
                         (sb-simd-sse2:s64 (call-vop sb-simd-sse2::f64-from-s64 x))
                         (real (coerce x ',name))))
                      #+x86-64
                      (sb-simd-avx:f32
                       `((sb-simd-avx:f64 (call-vop sb-simd-avx::f32-from-f64 x))
                         (sb-simd-avx:s64 (call-vop sb-simd-avx::f32-from-s64 x))
                         (real (coerce x ',name))))
                      #+x86-64
                      (sb-simd-avx:f64
                       `((sb-simd-avx:f32 (call-vop sb-simd-avx::f64-from-f32 x))
                         (sb-simd-avx:s64 (call-vop sb-simd-avx::f64-from-s64 x))
                         (real (coerce x ',name))))
                      #+x86-64
                      ((sb-simd-avx512f:m64.8
                        sb-simd-avx512f:m32.16
                        sb-simd-avx512bw:m16.32
                        sb-simd-avx512bw:m8.64)
                       `(((unsigned-byte 64) (sb-ext:%make-simd-pack-512-mask x)))))
                  (otherwise (,err x))))
              ,@(when (and (mask-record-p (find-value-record name nil))
                           (instruction-set-available-p instruction-set))
                  (let* ((rec (find-value-record name))
                         (width (mask-record-width rec))
                         (val-fn (mksym (symbol-package name) name "-VALUE"))
                         (cnt-fn (mksym (symbol-package name) name "-COUNT"))
                         (zero-fn (mksym (symbol-package name) name "-ZEROP"))
                         (all-fn (mksym (symbol-package name) name "-ALL-P")))
                    `((declaim (inline ,val-fn ,cnt-fn ,zero-fn ,all-fn))
                      (defun ,val-fn (mask)
                        (declare (type ,name mask))
                        #+x86-64
                        (ldb (byte ,width 0) (sb-kernel:%simd-pack-512-mask-value mask))
                        #-x86-64
                        (ldb (byte ,width 0) (phony-simd-pack-512-mask-value mask)))
                      (defun ,cnt-fn (mask)
                        (declare (type ,name mask))
                        (logcount (mask-value mask)))
                      (defun ,zero-fn (mask)
                        (declare (type ,name mask))
                        (zerop (mask-value mask)))
                      (defun ,all-fn (mask)
                        (declare (type ,name mask))
                        (= (logcount (mask-value mask)) ,width)))))))))
     (define-scalar-casts ()
       `(progn
          ,@(loop for scalar-cast-record in (filter-function-records #'scalar-cast-record-p)
                  collect `(define-scalar-cast ,(function-record-name scalar-cast-record))))))
  (define-scalar-casts))
