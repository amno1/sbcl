(in-package #:sb-simd-avx512bitalg)

(define-instruction-set :avx512bitalg
  (:test (avx512bitalg-supported-p))
  (:include :avx512vpopcntdq)
  (:instructions
   ;; Byte and word population count
   (u8.64-popcnt          #:vpopcntb     (u8.64)  (u8.64)         :cost 1)
   (s8.64-popcnt          #:vpopcntb     (s8.64)  (s8.64)         :cost 1)
   (u16.32-popcnt         #:vpopcntw     (u16.32) (u16.32)        :cost 1)
   (s16.32-popcnt         #:vpopcntw     (s16.32) (s16.32)        :cost 1)

   ;; Bit shuffle across qwords into mask
   (m8.64-shufbitqmb      #:vpshufbitqmb (m8.64)  (u8.64 u8.64)   :cost 1)
   (u8.64-shufbitqmb      #:vpshufbitqmb (m8.64)  (u8.64 u8.64)   :cost 1)))
