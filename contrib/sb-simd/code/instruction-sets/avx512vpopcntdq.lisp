(in-package #:sb-simd-avx512vpopcntdq)

(define-instruction-set :avx512vpopcntdq
  (:test (avx512vpopcntdq-supported-p))
  (:include :avx512cd)
  (:instructions
   ;; Population count
   (u32.16-popcnt         #:vpopcntd     (u32.16) (u32.16)        :cost 1)
   (s32.16-popcnt         #:vpopcntd     (s32.16) (s32.16)        :cost 1)
   (u64.8-popcnt          #:vpopcntq     (u64.8)  (u64.8)         :cost 1)
   (s64.8-popcnt          #:vpopcntq     (s64.8)  (s64.8)         :cost 1)))
