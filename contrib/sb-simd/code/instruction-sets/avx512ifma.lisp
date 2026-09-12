(in-package #:sb-simd-avx512ifma)

(define-instruction-set :avx512ifma
  (:test (avx512ifma-supported-p))
  (:include :avx512bitalg)
  (:instructions
   ;; 52x52->104-bit unsigned multiply, accumulating the low or high 52
   ;; bits of the product into a 64-bit lane -- the same
   ;; accumulator-is-also-the-result shape as VNNI, but with all three
   ;; operands (and the result) sharing one type, exactly like every
   ;; other :ENCODING :FMA instruction in this file. Per the SDM
   ;; (VPMADD52LUQ/HUQ), only the low 52 bits of each multiplicand are
   ;; used (bits 52-63 are ignored, not an error); the accumulator's
   ;; full 64 bits participate in the add.
   (u64.8-madd52luq #:vpmadd52luq (u64.8) (u64.8 u64.8 u64.8) :cost 1 :encoding :fma)
   (u64.8-madd52huq #:vpmadd52huq (u64.8) (u64.8 u64.8 u64.8) :cost 1 :encoding :fma)))
