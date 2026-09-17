(in-package #:sb-simd-internals)

(macrolet
    ((define-vref (name kind)
       (with-accessors ((name vref-record-name)
                        (instruction-set vref-record-instruction-set)
                        (value-record vref-record-value-record)
                        (vector-record vref-record-vector-record)
                        (vop vref-record-vop)
                        (sap vref-record-sap-ref))
           (find-function-record name)
         (let* ((simd-width (value-record-simd-width value-record))
                (element-type
                  (second
                   (value-record-type vector-record)))
                (sap-vop (when sap (mksym (symbol-package name) (if (eq kind :store) "%SET-" "%") sap))))
           (declare (ignorable simd-width element-type))
           (ecase kind
             (:load
              (if (not (instruction-set-available-p instruction-set))
                  `(progn
                     (define-missing-instruction ,name
                       :required-arguments (array index))
                     ,@(when sap
                         `((define-missing-instruction ,sap
                             :required-arguments (sap index)))))
                  `(progn
                    (define-inline ,name (array index)
                       (declare (type (array ,element-type) array)
                                (index index))
                       (sb-kernel:check-bound array (array-total-size array) (+ index ,(1- simd-width)))
                       (multiple-value-bind (vector index)
                           (sb-kernel:%data-vector-and-index array index)
                         (declare (type (simple-array ,element-type (*)) vector))
                         (,vop vector index 0)))
                    ,@(when sap
                        `((define-inline ,sap (sap index)
                            (declare (type sb-alien:system-area-pointer sap) (type index index))
                            (,sap-vop sap index 0)))))))
             (:store
              (if (not (instruction-set-available-p instruction-set))
                  `(progn
                     (define-missing-instruction ,name
                       :required-arguments (value array index))
                     ,@(when sap
                         `((define-missing-instruction (setf ,sap)
                             :required-arguments (value sap index)))))
                  `(progn
                    (define-inline ,name (value array index)
                       (declare (type (array ,element-type) array)
                                (index index))
                       (sb-kernel:check-bound array (array-total-size array) (+ index ,(1- simd-width)))
                       (multiple-value-bind (vector index)
                           (sb-kernel:%data-vector-and-index array index)
                         (declare (type (simple-array ,element-type (*)) vector))
                         (,vop (,(value-record-name value-record) value) vector
                               index 0)))
                    ,@(when sap
                        `((define-inline (setf ,sap) (value sap index)
                            (declare (type sb-alien:system-area-pointer sap) (type index index))
                            (,sap-vop (,(value-record-name value-record) value) sap index 0)))))))))))
     (define-vrefs ()
       `(progn
          ,@(loop for load-record in (filter-function-records #'load-record-p)
                  for name = (load-record-name load-record)
                  for sap = (vref-record-sap-ref load-record)
                  collect `(define-vref ,name :load))
          ,@(loop for store-record in (filter-function-records #'store-record-p)
                  for name = (store-record-name store-record)
                  for sap = (vref-record-sap-ref store-record)
                  collect `(define-vref ,name :store)))))
  (define-vrefs))

#+x86-64
(macrolet
    ((def-masked-vref (type element-type mask-type)
       (let* ((pkg (find-package "SB-SIMD-AVX512F"))
              (load-z-name     (mksym pkg type "-LOAD-MASKED-Z"))
              (load-m-name     (mksym pkg type "-LOAD-MASKED"))
              (store-name      (mksym pkg type "-STORE-MASKED"))
              (sap-load-z-name (mksym pkg type "-SAP-REF-MASKED-Z"))
              (sap-load-m-name (mksym pkg type "-SAP-REF-MASKED"))
              (vop-load-z      (mksym pkg "%" type "-LOAD-MASKED-Z"))
              (vop-load-m      (mksym pkg "%" type "-LOAD-MASKED"))
              (vop-store       (mksym pkg "%" type "-STORE-MASKED"))
              (vop-sap-load-z  (mksym pkg "%" type "-SAP-REF-MASKED-Z"))
              (vop-sap-load-m  (mksym pkg "%" type "-SAP-REF-MASKED"))
              (vop-sap-store   (mksym pkg "%SET-" type "-SAP-REF-MASKED"))
              (type-cast       (mksym pkg type))
              (mask-cast       (mksym pkg mask-type)))
         (if (not (instruction-set-available-p (find-instruction-set :avx512f)))
             `(progn
                (defun ,load-z-name (mask array index)
                  (declare (ignore mask array index))
                  (error "AVX512F instruction ~S not supported." ',load-z-name))
                (defun ,load-m-name (mask default array index)
                  (declare (ignore mask default array index))
                  (error "AVX512F instruction ~S not supported." ',load-m-name))
                (defun ,store-name (mask value array index)
                  (declare (ignore mask value array index))
                  (error "AVX512F instruction ~S not supported." ',store-name))
                (defun ,sap-load-z-name (mask sap index)
                  (declare (ignore mask sap index))
                  (error "AVX512F instruction ~S not supported." ',sap-load-z-name))
                (defun ,sap-load-m-name (mask default sap index)
                  (declare (ignore mask default sap index))
                  (error "AVX512F instruction ~S not supported." ',sap-load-m-name))
                (defun (setf ,sap-load-m-name) (value mask sap index)
                  (declare (ignore value mask sap index))
                  (error "AVX512F instruction ~S not supported." '(setf ,sap-load-m-name)))
                (defun (setf ,sap-load-z-name) (value mask sap index)
                  (declare (ignore value mask sap index))
                  (error "AVX512F instruction ~S not supported." '(setf ,sap-load-z-name))))
             `(progn
                ;; Zeroing load
                (define-inline ,load-z-name (mask array index)
                  (declare (type (array ,element-type) array)
                           (type index index))
                  (let ((int-mask (if (integerp mask) mask (mask-value mask))))
                    (when (plusp int-mask)
                      (sb-kernel:check-bound array (array-total-size array) (+ index (1- (integer-length int-mask))))))
                  (multiple-value-bind (vector index)
                      (sb-kernel:%data-vector-and-index array index)
                    (declare (type (simple-array ,element-type (*)) vector))
                    (,vop-load-z (,mask-cast mask) vector index 0)))

                ;; Merging load
                (define-inline ,load-m-name (mask default array index)
                  (declare (type (array ,element-type) array)
                           (type index index))
                  (let ((int-mask (if (integerp mask) mask (mask-value mask))))
                    (when (plusp int-mask)
                      (sb-kernel:check-bound array (array-total-size array) (+ index (1- (integer-length int-mask))))))
                  (multiple-value-bind (vector index)
                      (sb-kernel:%data-vector-and-index array index)
                    (declare (type (simple-array ,element-type (*)) vector))
                    (,vop-load-m (,mask-cast mask) (,type-cast default) vector index 0)))

                ;; Masked store
                (define-inline ,store-name (mask value array index)
                  (declare (type (array ,element-type) array)
                           (type index index))
                  (let ((int-mask (if (integerp mask) mask (mask-value mask))))
                    (when (plusp int-mask)
                      (sb-kernel:check-bound array (array-total-size array) (+ index (1- (integer-length int-mask))))))
                  (multiple-value-bind (vector index)
                      (sb-kernel:%data-vector-and-index array index)
                    (declare (type (simple-array ,element-type (*)) vector))
                    (,vop-store (,mask-cast mask) (,type-cast value) vector index 0)))

                ;; SAP zeroing load
                (define-inline ,sap-load-z-name (mask sap index)
                  (declare (type sb-alien:system-area-pointer sap)
                           (type index index))
                  (,vop-sap-load-z (,mask-cast mask) sap index 0))

                ;; SAP merging load
                (define-inline ,sap-load-m-name (mask default sap index)
                  (declare (type sb-alien:system-area-pointer sap)
                           (type index index))
                  (,vop-sap-load-m (,mask-cast mask) (,type-cast default) sap index 0))

                ;; SAP store: (setf (<type>-sap-ref-masked mask sap index) value)
                (define-inline (setf ,sap-load-m-name) (value mask sap index)
                  (declare (type sb-alien:system-area-pointer sap)
                           (type index index))
                  (,vop-sap-store (,mask-cast mask) (,type-cast value) sap index 0))

                ;; SAP store alias: (setf (<type>-sap-ref-masked-z mask sap index) value)
                (define-inline (setf ,sap-load-z-name) (value mask sap index)
                  (declare (type sb-alien:system-area-pointer sap)
                           (type index index))
                  (,vop-sap-store (,mask-cast mask) (,type-cast value) sap index 0)))))))
  (def-masked-vref f32.16 single-float          m32.16)
  (def-masked-vref f64.8  double-float          m64.8)
  (def-masked-vref u32.16 (unsigned-byte 32)    m32.16)
  (def-masked-vref s32.16 (signed-byte 32)      m32.16)
  (def-masked-vref u64.8  (unsigned-byte 64)    m64.8)
  (def-masked-vref s64.8  (signed-byte 64)      m64.8))

