(in-package #:sb-simd-avx512vpopcntdq)

(define-instruction-set :avx512vpopcntdq
  (:test (avx512vpopcntdq-supported-p))
  (:include :avx512dq)
  (:instructions
   ;; Per-lane population count. VPOPCNTD/Q count bits, not numbers, so
   ;; the result is always unsigned regardless of the argument's
   ;; signedness -- the same convention already used for this file's
   ;; comparisons (e.g. TWO-ARG-S32.16= also returns U32.16).
   (u32.16-count #:vpopcntd (u32.16) (u32.16) :cost 1)
   (s32.16-count #:vpopcntd (u32.16) (s32.16) :cost 1)
   (u64.8-count  #:vpopcntq (u64.8)  (u64.8)  :cost 1)
   (s64.8-count  #:vpopcntq (u64.8)  (s64.8)  :cost 1)))
