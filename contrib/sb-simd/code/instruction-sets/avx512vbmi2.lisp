(in-package #:sb-simd-avx512vbmi2)

(define-instruction-set :avx512vbmi2
  (:test (avx512vbmi2-supported-p))
  (:include :avx512vbmi)
  (:instructions
   ;; COMPRESS/EXPAND only mean anything with a mask -- there's no
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
   (s16.32-expand    nil (s16.32) (s16.32 mask64) :cost 2 :encoding :custom)))
