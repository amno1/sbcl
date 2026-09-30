(in-package #:sb-simd-avx512vbmi2)

(define-instruction-set :avx512vbmi2
  (:test (avx512vbmi2-supported-p))
  (:include :avx512vbmi)
  (:instructions
   ;; Double shifts by immediate
   (u16.32-shld           #:vpshldw      (u16.32) (u16.32 u16.32 imm8)   :cost 1)
   (s16.32-shld           #:vpshldw      (s16.32) (s16.32 s16.32 imm8)   :cost 1)
   (u32.16-shld           #:vpshldd      (u32.16) (u32.16 u32.16 imm8)   :cost 1)
   (s32.16-shld           #:vpshldd      (s32.16) (s32.16 s32.16 imm8)   :cost 1)
   (u64.8-shld            #:vpshldq      (u64.8)  (u64.8 u64.8 imm8)     :cost 1)
   (s64.8-shld            #:vpshldq      (s64.8)  (s64.8 s64.8 imm8)     :cost 1)

   (u16.32-shrd           #:vpshrdw      (u16.32) (u16.32 u16.32 imm8)   :cost 1)
   (s16.32-shrd           #:vpshrdw      (s16.32) (s16.32 s16.32 imm8)   :cost 1)
   (u32.16-shrd           #:vpshrdd      (u32.16) (u32.16 u32.16 imm8)   :cost 1)
   (s32.16-shrd           #:vpshrdd      (s32.16) (s32.16 s32.16 imm8)   :cost 1)
   (u64.8-shrd            #:vpshrdq      (u64.8)  (u64.8 u64.8 imm8)     :cost 1)
   (s64.8-shrd            #:vpshrdq      (s64.8)  (s64.8 s64.8 imm8)     :cost 1)

   ;; Double shifts by variable count
   (u16.32-shldv          #:vpshldvw     (u16.32) (u16.32 u16.32 u16.32) :cost 1 :encoding :fma)
   (s16.32-shldv          #:vpshldvw     (s16.32) (s16.32 s16.32 u16.32) :cost 1 :encoding :fma)
   (u32.16-shldv          #:vpshldvd     (u32.16) (u32.16 u32.16 u32.16) :cost 1 :encoding :fma)
   (s32.16-shldv          #:vpshldvd     (s32.16) (s32.16 s32.16 u32.16) :cost 1 :encoding :fma)
   (u64.8-shldv           #:vpshldvq     (u64.8)  (u64.8 u64.8 u64.8)    :cost 1 :encoding :fma)
   (s64.8-shldv           #:vpshldvq     (s64.8)  (s64.8 s64.8 u64.8)    :cost 1 :encoding :fma)

   (u16.32-shrdv          #:vpshrdvw     (u16.32) (u16.32 u16.32 u16.32) :cost 1 :encoding :fma)
   (s16.32-shrdv          #:vpshrdvw     (s16.32) (s16.32 s16.32 u16.32) :cost 1 :encoding :fma)
   (u32.16-shrdv          #:vpshrdvd     (u32.16) (u32.16 u32.16 u32.16) :cost 1 :encoding :fma)
   (s32.16-shrdv          #:vpshrdvd     (s32.16) (s32.16 s32.16 u32.16) :cost 1 :encoding :fma)
   (u64.8-shrdv           #:vpshrdvq     (u64.8)  (u64.8 u64.8 u64.8)    :cost 1 :encoding :fma)
   (s64.8-shrdv           #:vpshrdvq     (s64.8)  (s64.8 s64.8 u64.8)    :cost 1 :encoding :fma)))
