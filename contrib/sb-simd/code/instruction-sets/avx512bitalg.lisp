(in-package #:sb-simd-avx512bitalg)

(define-instruction-set :avx512bitalg
  (:test (avx512bitalg-supported-p))
  (:include :avx512vnni)
  (:instructions
   ;; Per-lane population count, same shape and signedness convention as
   ;; U32.16-COUNT/etc. in AVX512VPOPCNTDQ -- just at byte/word width via
   ;; VPOPCNTB/VPOPCNTW instead of dword/qword via VPOPCNTD/VPOPCNTQ.
   (u8.64-count  #:vpopcntb (u8.64)  (u8.64)  :cost 1)
   (s8.64-count  #:vpopcntb (u8.64)  (s8.64)  :cost 1)
   (u16.32-count #:vpopcntw (u16.32) (u16.32) :cost 1)
   (s16.32-count #:vpopcntw (u16.32) (s16.32) :cost 1)))
