(in-package #:sb-simd-avx512vnni)

(define-instruction-set :avx512vnni
  (:test (avx512vnni-supported-p))
  (:include :avx512vbmi2)
  (:instructions
   ;; Dot product of unsigned and signed bytes, accumulate into dwords
   (s32.16-dpbusd         #:vpdpbusd     (s32.16) (s32.16 u8.64 s8.64)   :cost 1 :encoding :fma)
   (u32.16-dpbusd         #:vpdpbusd     (u32.16) (u32.16 u8.64 s8.64)   :cost 1 :encoding :fma)
   (s32.16-dpbusds        #:vpdpbusds    (s32.16) (s32.16 u8.64 s8.64)   :cost 1 :encoding :fma)
   (u32.16-dpbusds        #:vpdpbusds    (u32.16) (u32.16 u8.64 s8.64)   :cost 1 :encoding :fma)

   ;; Dot product of signed words, accumulate into dwords
   (s32.16-dpwssd         #:vpdpwssd     (s32.16) (s32.16 s16.32 s16.32) :cost 1 :encoding :fma)
   (u32.16-dpwssd         #:vpdpwssd     (u32.16) (u32.16 s16.32 s16.32) :cost 1 :encoding :fma)
   (s32.16-dpwssds        #:vpdpwssds    (s32.16) (s32.16 s16.32 s16.32) :cost 1 :encoding :fma)
   (u32.16-dpwssds        #:vpdpwssds    (u32.16) (u32.16 s16.32 s16.32) :cost 1 :encoding :fma)))
