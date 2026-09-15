(in-package #:sb-simd-test-suite)

;;; Roundtrips and Constructors
(define-test mask-roundtrip-m64.8
  (let ((open-sb-simd-avx512f (find-package "SB-SIMD-AVX512F")))
    (when (and open-sb-simd-avx512f (sb-simd-internals:instruction-set-available-p (sb-simd-internals:find-instruction-set :avx512f)))
      (is (= 0 (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8 0))))
      (is (= #xFF (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8 #xFF))))
      (is (= #xAA (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8 #xAA)))))))

(define-test mask-roundtrip-m32.16
  (let ((open-sb-simd-avx512f (find-package "SB-SIMD-AVX512F")))
    (when (and open-sb-simd-avx512f (sb-simd-internals:instruction-set-available-p (sb-simd-internals:find-instruction-set :avx512f)))
      (is (= 0 (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:m32.16 0))))
      (is (= #xFFFF (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:m32.16 #xFFFF))))
      (is (= #x5555 (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:m32.16 #x5555)))))))

(define-test mask-roundtrip-m16.32
  (let ((open-sb-simd-avx512bw (find-package "SB-SIMD-AVX512BW")))
    (when (and open-sb-simd-avx512bw (sb-simd-internals:instruction-set-available-p (sb-simd-internals:find-instruction-set :avx512bw)))
      (is (= 0 (sb-simd-avx512bw:m16.32-value (sb-simd-avx512bw:m16.32 0))))
      (is (= #xFFFFFFFF (sb-simd-avx512bw:m16.32-value (sb-simd-avx512bw:m16.32 #xFFFFFFFF))))
      (is (= #x12345678 (sb-simd-avx512bw:m16.32-value (sb-simd-avx512bw:m16.32 #x12345678)))))))

(define-test mask-roundtrip-m8.64
  (let ((open-sb-simd-avx512bw (find-package "SB-SIMD-AVX512BW")))
    (when (and open-sb-simd-avx512bw (sb-simd-internals:instruction-set-available-p (sb-simd-internals:find-instruction-set :avx512bw)))
      (is (= 0 (sb-simd-avx512bw:m8.64-value (sb-simd-avx512bw:m8.64 0))))
      (is (= #xFFFFFFFFFFFFFFFF (sb-simd-avx512bw:m8.64-value (sb-simd-avx512bw:m8.64 #xFFFFFFFFFFFFFFFF))))
      (is (= (ash 1 37) (sb-simd-avx512bw:m8.64-value (sb-simd-avx512bw:m8.64 (ash 1 37))))))))

;;; Boolean Algebra and Associatives
(define-test mask-boolean-algebra-avx512f
  (let ((open-sb-simd-avx512f (find-package "SB-SIMD-AVX512F")))
    (when (and open-sb-simd-avx512f (sb-simd-internals:instruction-set-available-p (sb-simd-internals:find-instruction-set :avx512f)))
      ;; m64.8
      (let ((a (sb-simd-avx512f:m64.8 #b11110000))
            (b (sb-simd-avx512f:m64.8 #b10101010)))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8-and a b)) #b10100000))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8-or a b))  #b11111010))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8-xor a b)) #b01011010))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8-andc1 a b))       #b00001010))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8-not a))           #b00001111))
        ;; associatives
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8-and)) sb-simd-internals:+m64.8-true+))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8-and a)) #b11110000))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8-and a b (sb-simd-avx512f:m64.8 #b11001100))) #b10000000))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8-or)) sb-simd-internals:+m64.8-false+))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:m64.8-xor)) sb-simd-internals:+m64.8-false+))
        (is (sb-simd-avx512f:m64.8-zerop (sb-simd-avx512f:m64.8-xor a a)))
        (is (sb-simd-avx512f:m64.8-all-p (sb-simd-avx512f:m64.8-or (sb-simd-avx512f:m64.8 #b11110000) (sb-simd-avx512f:m64.8 #b00001111))))
        (is (= (sb-simd-avx512f:m64.8-count a) 4)))

      ;; m32.16
      (let ((a (sb-simd-avx512f:m32.16 #xF0F0))
            (b (sb-simd-avx512f:m32.16 #xAAAA)))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:m32.16-and a b)) #xA0A0))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:m32.16-or a b))  #xFAFA))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:m32.16-xor a b)) #x5A5A))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:m32.16-andc1 a b))       #x0A0A))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:m32.16-not a))           #x0F0F))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:m32.16-and)) sb-simd-internals:+m32.16-true+))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:m32.16-or)) sb-simd-internals:+m32.16-false+))
        (is (sb-simd-avx512f:m32.16-zerop (sb-simd-avx512f:m32.16-xor a a)))
        (is (sb-simd-avx512f:m32.16-all-p (sb-simd-avx512f:m32.16-or (sb-simd-avx512f:m32.16 #xFF00) (sb-simd-avx512f:m32.16 #x00FF))))
        (is (= (sb-simd-avx512f:m32.16-count a) 8))))))

(define-test mask-boolean-algebra-avx512bw
  (let ((open-sb-simd-avx512bw (find-package "SB-SIMD-AVX512BW")))
    (when (and open-sb-simd-avx512bw (sb-simd-internals:instruction-set-available-p (sb-simd-internals:find-instruction-set :avx512bw)))
      ;; m16.32
      (let ((a (sb-simd-avx512bw:m16.32 #xF0F0F0F0))
            (b (sb-simd-avx512bw:m16.32 #xAAAAAAAA)))
        (is (= (sb-simd-avx512bw:m16.32-value (sb-simd-avx512bw:m16.32-and a b)) #xA0A0A0A0))
        (is (= (sb-simd-avx512bw:m16.32-value (sb-simd-avx512bw:m16.32-or a b))  #xFAFAFAFA))
        (is (= (sb-simd-avx512bw:m16.32-value (sb-simd-avx512bw:m16.32-xor a b)) #x5A5A5A5A))
        (is (= (sb-simd-avx512bw:m16.32-value (sb-simd-avx512bw:m16.32-andc1 a b))       #x0A0A0A0A))
        (is (= (sb-simd-avx512bw:m16.32-value (sb-simd-avx512bw:m16.32-not a))           #x0F0F0F0F))
        (is (= (sb-simd-avx512bw:m16.32-value (sb-simd-avx512bw:m16.32-and)) sb-simd-internals:+m16.32-true+))
        (is (= (sb-simd-avx512bw:m16.32-value (sb-simd-avx512bw:m16.32-or)) sb-simd-internals:+m16.32-false+))
        (is (sb-simd-avx512bw:m16.32-zerop (sb-simd-avx512bw:m16.32-xor a a)))
        (is (sb-simd-avx512bw:m16.32-all-p (sb-simd-avx512bw:m16.32-or (sb-simd-avx512bw:m16.32 #xFFFF0000) (sb-simd-avx512bw:m16.32 #x0000FFFF))))
        (is (= (sb-simd-avx512bw:m16.32-count a) 16)))

      ;; m8.64
      (let ((a (sb-simd-avx512bw:m8.64 #xF0F0F0F0F0F0F0F0))
            (b (sb-simd-avx512bw:m8.64 #xAAAAAAAAAAAAAAAA)))
        (is (= (sb-simd-avx512bw:m8.64-value (sb-simd-avx512bw:m8.64-and a b)) #xA0A0A0A0A0A0A0A0))
        (is (= (sb-simd-avx512bw:m8.64-value (sb-simd-avx512bw:m8.64-or a b))  #xFAFAFAFAFAFAFAFA))
        (is (= (sb-simd-avx512bw:m8.64-value (sb-simd-avx512bw:m8.64-xor a b)) #x5A5A5A5A5A5A5A5A))
        (is (= (sb-simd-avx512bw:m8.64-value (sb-simd-avx512bw:m8.64-andc1 a b))       #x0A0A0A0A0A0A0A0A))
        (is (= (sb-simd-avx512bw:m8.64-value (sb-simd-avx512bw:m8.64-not a))           #x0F0F0F0F0F0F0F0F))
        (is (= (sb-simd-avx512bw:m8.64-value (sb-simd-avx512bw:m8.64-and)) sb-simd-internals:+m8.64-true+))
        (is (= (sb-simd-avx512bw:m8.64-value (sb-simd-avx512bw:m8.64-or)) sb-simd-internals:+m8.64-false+))
        (is (sb-simd-avx512bw:m8.64-zerop (sb-simd-avx512bw:m8.64-xor a a)))
        (is (sb-simd-avx512bw:m8.64-all-p (sb-simd-avx512bw:m8.64-or (sb-simd-avx512bw:m8.64 #xFFFFFFFF00000000) (sb-simd-avx512bw:m8.64 #x00000000FFFFFFFF))))
        (is (= (sb-simd-avx512bw:m8.64-count a) 32))))))

;;; Mask Comparisons
(define-test vector-mask-comparisons-avx512f
  (let ((open-sb-simd-avx512f (find-package "SB-SIMD-AVX512F")))
    (when (and open-sb-simd-avx512f (sb-simd-internals:instruction-set-available-p (sb-simd-internals:find-instruction-set :avx512f)))
      ;; f32.16
      (let* ((v1 (sb-simd-avx512f:make-f32.16 1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0 9f0 10f0 11f0 12f0 13f0 14f0 15f0 16f0))
             (v2 (sb-simd-avx512f:make-f32.16 1f0 0f0 3f0 0f0 5f0 0f0 7f0 0f0 9f0  0f0 11f0  0f0 13f0  0f0 15f0  0f0)))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:f32.16-mask= v1 v2)) #x5555))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:f32.16-mask/= v1 v2)) #xAAAA))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:f32.16-mask<= v2 v1)) #xFFFF)))

      ;; f64.8
      (let* ((v1 (sb-simd-avx512f:make-f64.8 1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0))
             (v2 (sb-simd-avx512f:make-f64.8 1d0 0d0 3d0 0d0 5d0 0d0 7d0 0d0)))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:f64.8-mask= v1 v2)) #x55))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:f64.8-mask/= v1 v2)) #xAA))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:f64.8-mask< v2 v1)) #xAA)))

      ;; u32.16
      (let* ((v1 (sb-simd-avx512f:make-u32.16 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160))
             (v2 (sb-simd-avx512f:make-u32.16 10  0 30  0 50  0 70  0 90   0 110   0 130   0 150   0)))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:u32.16-mask= v1 v2)) #x5555))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:u32.16-mask/= v1 v2)) #xAAAA))
        (is (= (sb-simd-avx512f:m32.16-value (sb-simd-avx512f:u32.16-mask> v1 v2)) #xAAAA)))

      ;; s64.8
      (let* ((v1 (sb-simd-avx512f:make-s64.8 -10 20 -30 40 -50 60 -70 80))
             (v2 (sb-simd-avx512f:make-s64.8 -10  0 -30  0 -50  0 -70  0)))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:s64.8-mask= v1 v2)) #x55))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:s64.8-mask> v1 v2)) #xAA))
        (is (= (sb-simd-avx512f:m64.8-value (sb-simd-avx512f:s64.8-mask>= v1 v2)) #xFF))))))

(define-test vector-mask-comparisons-avx512bw
  (let ((open-sb-simd-avx512bw (find-package "SB-SIMD-AVX512BW")))
    (when (and open-sb-simd-avx512bw (sb-simd-internals:instruction-set-available-p (sb-simd-internals:find-instruction-set :avx512bw)))
      ;; u8.64
      (let* ((bytes1 (loop for i from 0 below 64 collect (if (evenp i) 42 100)))
             (bytes2 (loop for i from 0 below 64 collect (if (evenp i) 42 0)))
             (v1 (apply #'sb-simd-avx512bw:make-u8.64 bytes1))
             (v2 (apply #'sb-simd-avx512bw:make-u8.64 bytes2)))
        (is (= (sb-simd-avx512bw:m8.64-value (sb-simd-avx512bw:u8.64-mask= v1 v2)) #x5555555555555555))
        (is (= (sb-simd-avx512bw:m8.64-value (sb-simd-avx512bw:u8.64-mask/= v1 v2)) #xAAAAAAAAAAAAAAAA))
        (is (= (sb-simd-avx512bw:m8.64-count (sb-simd-avx512bw:u8.64-mask= v1 v2)) 32)))

      ;; u16.32
      (let* ((words1 (loop for i from 0 below 32 collect (if (evenp i) 1000 2000)))
             (words2 (loop for i from 0 below 32 collect (if (evenp i) 1000 0)))
             (v1 (apply #'sb-simd-avx512bw:make-u16.32 words1))
             (v2 (apply #'sb-simd-avx512bw:make-u16.32 words2)))
        (is (= (sb-simd-avx512bw:m16.32-value (sb-simd-avx512bw:u16.32-mask= v1 v2)) #x55555555))
        (is (= (sb-simd-avx512bw:m16.32-value (sb-simd-avx512bw:u16.32-mask/= v1 v2)) #xAAAAAAAA))
        (is (= (sb-simd-avx512bw:m16.32-count (sb-simd-avx512bw:u16.32-mask= v1 v2)) 16))))))
