(in-package #:sb-simd-avx512ifma)

(define-instruction-set :avx512ifma
  (:test (avx512ifma-supported-p))
  (:include :avx512vnni)
  (:instructions
   ;; Fused multiply-add of unsigned 52-bit integers, accumulate low 52 bits
   (u64.8-madd52lo        #:vpmadd52luq  (u64.8)  (u64.8 u64.8 u64.8)    :cost 1 :encoding :fma)
   (s64.8-madd52lo        #:vpmadd52luq  (s64.8)  (s64.8 u64.8 u64.8)    :cost 1 :encoding :fma)

   ;; Fused multiply-add of unsigned 52-bit integers, accumulate high 52 bits
   (u64.8-madd52hi        #:vpmadd52huq  (u64.8)  (u64.8 u64.8 u64.8)    :cost 1 :encoding :fma)
   (s64.8-madd52hi        #:vpmadd52huq  (s64.8)  (s64.8 u64.8 u64.8)    :cost 1 :encoding :fma)))
