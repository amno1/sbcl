(in-package #:sb-simd-avx512vbmi)

(define-instruction-set :avx512vbmi
  (:test (avx512vbmi-supported-p))
  (:include :avx512bitalg)
  (:instructions
   ;; Byte permute
   (u8.64-permute         #:vpermb       (u8.64)  (u8.64 u8.64)         :cost 1)
   (s8.64-permute         #:vpermb       (s8.64)  (u8.64 s8.64)         :cost 1)

   ;; Two-source byte permute
   (u8.64-permi2          #:vpermi2b     (u8.64)  (u8.64 u8.64 u8.64)   :cost 1 :encoding :fma)
   (s8.64-permi2          #:vpermi2b     (s8.64)  (u8.64 s8.64 s8.64)   :cost 1 :encoding :fma)
   (u8.64-permt2          #:vpermt2b     (u8.64)  (u8.64 u8.64 u8.64)   :cost 1 :encoding :fma)
   (s8.64-permt2          #:vpermt2b     (s8.64)  (s8.64 u8.64 s8.64)   :cost 1 :encoding :fma)

   ;; Multi-shift bytes across qwords
   (u8.64-multishiftqb    #:vpmultishiftqb (u8.64) (u8.64 u8.64)        :cost 1)
   (u8.64-multishift-u64.8 #:vpmultishiftqb (u8.64) (u8.64 u64.8)       :cost 1)))
