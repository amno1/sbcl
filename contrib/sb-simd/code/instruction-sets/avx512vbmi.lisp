(in-package #:sb-simd-avx512vbmi)

(define-instruction-set :avx512vbmi
  (:test (avx512vbmi-supported-p))
  (:include :avx512gfni)
  (:instructions
   ;; Full byte permute: DST[i] = DATA[INDICES[i] & 0x3F]
   (u8.64-permute #:vpermb (u8.64) (u8.64 u8.64) :cost 1)
   ;; Two-table permute: DST[i] = (INDICES[i] has bit 6 set) ? TABLE2[idx]
   ;; : TABLE1[idx], idx = INDICES[i] & 0x3F. VPERMT2B/VPERMI2B compute
   ;; the identical result -- they differ only in which operand is
   ;; read-modify-write (TABLE1 vs INDICES), an aliasing/register-
   ;; pressure choice, not a semantic one. Both are exposed since both
   ;; are real hardware instructions.
   ;;
   ;; Confirmed on real hardware that the SDM's own pseudocode for
   ;; VPERMI2B is internally inconsistent: its index-extraction line
   ;; names SRC1, but the prose description, the final byte-select
   ;; line, and direct execution all agree the destination's ORIGINAL
   ;; content (not SRC1) supplies the indices.
   (u8.64-permute2   #:vpermt2b (u8.64) (u8.64 u8.64 u8.64) :cost 1 :encoding :fma)
   (u8.64-permute2-i #:vpermi2b (u8.64) (u8.64 u8.64 u8.64) :cost 1 :encoding :fma)
   ;; Multi-shift: for each of the 8 64-bit qword lanes independently,
   ;; and for each of the 8 bytes within that lane, rotate DATA's qword
   ;; right by a 6-bit amount taken from the corresponding byte of
   ;; CONTROL, and keep the low byte -- the same "8x8 lanes" qword
   ;; grouping as U8.64-BIT-GATHER, with DATA typed U64.8 for the same
   ;; reason as GFNI's affine matrix: it's 8 independent 64-bit values,
   ;; not 64 independent bytes.
   (u8.64-multishift #:vpmultishiftqb (u8.64) (u8.64 u64.8) :cost 1)))
