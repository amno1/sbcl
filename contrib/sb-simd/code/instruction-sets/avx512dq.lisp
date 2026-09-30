(in-package #:sb-simd-avx512dq)

(define-instruction-set :avx512dq
  (:test (avx512dq-supported-p))
  (:include :avx512bw)
  (:instructions
   ;; Multiplication (64-bit full multiply, 32-bit to 64-bit multiply)
   (two-arg-u64.8*        #:vpmullq      (u64.8)  (u64.8 u64.8)   :cost 4 :associative t)
   (two-arg-s64.8*        #:vpmullq      (s64.8)  (s64.8 s64.8)   :cost 4 :associative t)
   (two-arg-u64.8-mullo   #:vpmullq      (u64.8)  (u64.8 u64.8)   :cost 4 :associative t)
   (two-arg-s64.8-mullo   #:vpmullq      (s64.8)  (s64.8 s64.8)   :cost 4 :associative t)
   (two-arg-s64.8-mul     #:vpmuldq      (s64.8)  (s64.8 s64.8)   :cost 1 :associative t)

   ;; Mask <-> Vector transfers (vpmovd2m, vpmovq2m)
   (m32.16-from-u32.16    #:vpmovd2m     (m32.16) (u32.16)        :cost 1)
   (m32.16-from-s32.16    #:vpmovd2m     (m32.16) (s32.16)        :cost 1)
   (m64.8-from-u64.8      #:vpmovq2m     (m64.8)  (u64.8)         :cost 1)
   (m64.8-from-s64.8      #:vpmovq2m     (m64.8)  (s64.8)         :cost 1)

   ;; Conversions between Float and 64-bit Integer
   (s64.8-from-f64.8      #:vcvtpd2qq    (s64.8)  (f64.8)         :cost 5)
   (u64.8-from-f64.8      #:vcvtpd2uqq   (u64.8)  (f64.8)         :cost 5)
   (s64.8-from-f32.8      #:vcvtps2qq    (s64.8)  (f32.8)         :cost 5)
   (u64.8-from-f32.8      #:vcvtps2uqq   (u64.8)  (f32.8)         :cost 5)
   (f32.8-from-s64.8      #:vcvtqq2ps    (f32.8)  (s64.8)         :cost 5)
   (f32.8-from-u64.8      #:vcvtuqq2ps   (f32.8)  (u64.8)         :cost 5)

   ;; Range and Reduction
   (f32.16-range          #:vrangeps     (f32.16) (f32.16 f32.16 imm8) :cost 1)
   (f64.8-range           #:vrangepd     (f64.8)  (f64.8 f64.8 imm8)   :cost 1)
   (f32.16-reduce         #:vreduceps    (f32.16) (f32.16 imm8)        :cost 1)
   (f64.8-reduce          #:vreducepd    (f64.8)  (f64.8 imm8)        :cost 1)

   ;; Float Class testing (returns mask)
   (f32.16-fpclass        #:vfpclassps   (m32.16) (f32.16 imm8)   :cost 1)
   (f64.8-fpclass         #:vfpclasspd   (m64.8)  (f64.8 imm8)    :cost 1)

   ;; 128-bit Subvector Extract and Insert
   (f64.2-from-f64.8      #:vextractf64x2 (f64.2) (f64.8 imm2)    :cost 1)
   (f64.8-insert-f64.2    #:vinsertf64x2  (f64.8) (f64.8 f64.2 imm2) :cost 1)
   (u64.2-from-u64.8      #:vextracti64x2 (u64.2) (u64.8 imm2)    :cost 1)
   (u64.8-insert-u64.2    #:vinserti64x2  (u64.8) (u64.8 u64.2 imm2) :cost 1)
   (s64.2-from-s64.8      #:vextracti64x2 (s64.2) (s64.8 imm2)    :cost 1)
   (s64.8-insert-s64.2    #:vinserti64x2  (s64.8) (s64.8 s64.2 imm2) :cost 1))
  (:associatives
   (u64.8* two-arg-u64.8* 1)
   (s64.8* two-arg-s64.8* 1)
   (u64.8-mullo two-arg-u64.8-mullo 1)
   (s64.8-mullo two-arg-s64.8-mullo 1)
   (s64.8-mul two-arg-s64.8-mul 1)))
