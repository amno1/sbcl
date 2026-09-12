(in-package #:sb-simd-avx512vnni)

(define-instruction-set :avx512vnni
  (:test (avx512vnni-supported-p))
  (:include :avx512vpopcntdq)
  (:instructions
   ;; Dot-product-and-accumulate: DST is both an input (the running
   ;; accumulator) and the output, exactly like an FMA -- hence
   ;; :ENCODING :FMA, which aliases the first argument with the result
   ;; instead of requiring all three operands to share one type. Here
   ;; they don't: the accumulator is S32.16 (4 narrower lanes fold into
   ;; each dword), while the two dot-product operands are the
   ;; instruction's own narrower, differently-signed lane types.
   ;;
   ;; VPDPBUSD/-BUSDS: 4 (unsigned byte x signed byte) products per
   ;; output dword lane.
   (s32.16-dpbusd  #:vpdpbusd  (s32.16) (s32.16 u8.64  s8.64)  :cost 1 :encoding :fma)
   (s32.16-dpbusds #:vpdpbusds (s32.16) (s32.16 u8.64  s8.64)  :cost 1 :encoding :fma)
   ;; VPDPWSSD/-WSSDS: 2 (signed word x signed word) products per
   ;; output dword lane.
   (s32.16-dpwssd  #:vpdpwssd  (s32.16) (s32.16 s16.32 s16.32) :cost 1 :encoding :fma)
   (s32.16-dpwssds #:vpdpwssds (s32.16) (s32.16 s16.32 s16.32) :cost 1 :encoding :fma)))
