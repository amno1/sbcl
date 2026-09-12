(in-package #:sb-simd-avx512vbmi2)

(define-instruction-set :avx512vbmi2
  (:test (avx512vbmi2-supported-p))
  (:include :avx512vbmi)
  (:instructions
   ;; COMPRESS/EXPAND only mean anything with a mask, there's no
   ;; useful "unmasked" form, so unlike every other instruction in this
   ;; tree these take a MASK64 argument directly instead of going
   ;; through the -IF/blend abstraction. Only the zero-masking form is
   ;; exposed: COMPRESS's whole purpose is variable-length output (the
   ;; caller only cares about the first POPCOUNT(mask) result lanes),
   ;; and EXPAND's un-selected lanes are equally uninteresting to a
   ;; caller who doesn't already have an old value in mind for them; a
   ;; real need for merge-masking here can still be built with -IF on
   ;; top of the zero-masking form.
   (u8.64-compress   nil (u8.64)  (u8.64  mask64) :cost 2 :encoding :custom)
   (s8.64-compress   nil (s8.64)  (s8.64  mask64) :cost 2 :encoding :custom)
   (u16.32-compress  nil (u16.32) (u16.32 mask64) :cost 2 :encoding :custom)
   (s16.32-compress  nil (s16.32) (s16.32 mask64) :cost 2 :encoding :custom)
   (u8.64-expand     nil (u8.64)  (u8.64  mask64) :cost 2 :encoding :custom)
   (s8.64-expand     nil (s8.64)  (s8.64  mask64) :cost 2 :encoding :custom)
   (u16.32-expand    nil (u16.32) (u16.32 mask64) :cost 2 :encoding :custom)
   (s16.32-expand    nil (s16.32) (s16.32 mask64) :cost 2 :encoding :custom)

   ;; Funnel shift: concatenate two same-width values and shift, taking
   ;; the half nearer the shift direction -- SHLDI/SHRDI (imm8 shift
   ;; count, no destination read) and SHLDV/SHRDV (per-lane shift count
   ;; from a vector; the destination's PRE-EXISTING content is one of
   ;; the two concatenated values, aliased with the result exactly like
   ;; VNNI/IFMA's accumulate). Names match the Intel intrinsics'
   ;; own I/V suffix (_mm512_shldi_epi16 / _mm512_shldv_epi16).
   (u16.32-shldi #:vpshldw (u16.32) (u16.32 u16.32 imm8) :cost 1)
   (s16.32-shldi #:vpshldw (s16.32) (s16.32 s16.32 imm8) :cost 1)
   (u32.16-shldi #:vpshldd (u32.16) (u32.16 u32.16 imm8) :cost 1)
   (s32.16-shldi #:vpshldd (s32.16) (s32.16 s32.16 imm8) :cost 1)
   (u64.8-shldi  #:vpshldq (u64.8)  (u64.8  u64.8  imm8) :cost 1)
   (s64.8-shldi  #:vpshldq (s64.8)  (s64.8  s64.8  imm8) :cost 1)
   (u16.32-shrdi #:vpshrdw (u16.32) (u16.32 u16.32 imm8) :cost 1)
   (s16.32-shrdi #:vpshrdw (s16.32) (s16.32 s16.32 imm8) :cost 1)
   (u32.16-shrdi #:vpshrdd (u32.16) (u32.16 u32.16 imm8) :cost 1)
   (s32.16-shrdi #:vpshrdd (s32.16) (s32.16 s32.16 imm8) :cost 1)
   (u64.8-shrdi  #:vpshrdq (u64.8)  (u64.8  u64.8  imm8) :cost 1)
   (s64.8-shrdi  #:vpshrdq (s64.8)  (s64.8  s64.8  imm8) :cost 1)
   (u16.32-shldv #:vpshldvw (u16.32) (u16.32 u16.32 u16.32) :cost 1 :encoding :fma)
   (s16.32-shldv #:vpshldvw (s16.32) (s16.32 s16.32 u16.32) :cost 1 :encoding :fma)
   (u32.16-shldv #:vpshldvd (u32.16) (u32.16 u32.16 u32.16) :cost 1 :encoding :fma)
   (s32.16-shldv #:vpshldvd (s32.16) (s32.16 s32.16 u32.16) :cost 1 :encoding :fma)
   (u64.8-shldv  #:vpshldvq (u64.8)  (u64.8  u64.8  u64.8)  :cost 1 :encoding :fma)
   (s64.8-shldv  #:vpshldvq (s64.8)  (s64.8  s64.8  u64.8)  :cost 1 :encoding :fma)
   (u16.32-shrdv #:vpshrdvw (u16.32) (u16.32 u16.32 u16.32) :cost 1 :encoding :fma)
   (s16.32-shrdv #:vpshrdvw (s16.32) (s16.32 s16.32 u16.32) :cost 1 :encoding :fma)
   (u32.16-shrdv #:vpshrdvd (u32.16) (u32.16 u32.16 u32.16) :cost 1 :encoding :fma)
   (s32.16-shrdv #:vpshrdvd (s32.16) (s32.16 s32.16 u32.16) :cost 1 :encoding :fma)
   (u64.8-shrdv  #:vpshrdvq (u64.8)  (u64.8  u64.8  u64.8)  :cost 1 :encoding :fma)
   (s64.8-shrdv  #:vpshrdvq (s64.8)  (s64.8  s64.8  u64.8)  :cost 1 :encoding :fma)))
