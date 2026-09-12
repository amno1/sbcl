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
   (s16.32-count #:vpopcntw (u16.32) (s16.32) :cost 1)
   ;; Bit gather: for each of the 8 64-bit qword lanes independently,
   ;; and for each of the 8 bytes within that lane, pick 1 bit out of
   ;; DATA's 64-bit qword using a 6-bit index taken from the
   ;; corresponding byte of INDICES, and pack the 64 result bits 1:1
   ;; with byte position into the returned MASK64.
   ;;
   ;; Argument order matches the assembler's own (dst src1 src2): src1
   ;; (first vector argument) is DATA, src2 (second) is INDICES --
   ;; confirmed both against Intel's documented pseudocode and by
   ;; direct execution on real AVX512_BITALG hardware.
   (u8.64-bit-gather #:vpshufbitqmb (mask64) (u8.64 u8.64) :cost 1)))
