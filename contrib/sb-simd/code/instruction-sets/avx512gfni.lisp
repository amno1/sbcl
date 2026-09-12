(in-package #:sb-simd-avx512gfni)

(define-instruction-set :avx512gfni
  (:test (gfni-supported-p) (avx512f-supported-p))
  (:include :avx512ifma)
  (:instructions
   ;; Per-lane multiply in GF(2^8), reduction polynomial x^8+x^4+x^3+x+1
   ;; -- a plain, symmetric per-byte-lane op like any other TWO-ARG-*.
   (u8.64-gf2p8mulb #:vgf2p8mulb (u8.64) (u8.64 u8.64) :cost 1)
   ;; Affine transform A*x+b in GF(2^8): DATA holds "x" byte-per-lane;
   ;; MATRIX holds eight independent 64-bit "A" matrices, one per
   ;; 8-byte group of DATA (hence U64.8, not U8.64 -- its lanes don't
   ;; line up 1:1 with DATA's); IMM8 is the constant "b", shared by
   ;; every lane. GF2P8AFFINEINVQB is the same transform with each
   ;; DATA byte first replaced by its GF(2^8) multiplicative inverse.
   (u8.64-gf2p8affineqb    #:vgf2p8affineqb    (u8.64) (u8.64 u64.8 imm8) :cost 1)
   (u8.64-gf2p8affineinvqb #:vgf2p8affineinvqb (u8.64) (u8.64 u64.8 imm8) :cost 1)))
