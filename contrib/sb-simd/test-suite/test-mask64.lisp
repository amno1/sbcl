;;; Regression tests for MASK64 (AVX-512 opmask/k-register) support in
;;; SB-SIMD-AVX512BW: the boxed scalar type itself, its boolean algebra
;;; and shifts, the mask-producing U8.64 comparisons that stop at the
;;; k-register instead of widening back into a full vector, and the
;;; N-ary/chained sugar generated on top of all of the above.

(in-package #:sb-simd-avx512bw)

(sb-simd-test-suite:define-test mask64-roundtrip
  (sb-simd-test-suite:is (= 0 (mask64-value (mask64 0))))
  (sb-simd-test-suite:is (= #xFF (mask64-value (mask64 #xFF))))
  (sb-simd-test-suite:is (= (1- (ash 1 64)) (mask64-value (mask64 (1- (ash 1 64))))))
  (sb-simd-test-suite:is (= (ash 1 37) (mask64-value (mask64 (ash 1 37))))))

;;; Boolean algebra, cross-checked against plain LOGAND/LOGIOR/LOGXOR.
(sb-simd-test-suite:define-test mask64-boolean-algebra
  (dolist (case '((#x0F0F0F0F0F0F0F0F . #xFF00FF00FF00FF00)
                   (0                 . 0)
                   (#xFFFFFFFFFFFFFFFF . #xFFFFFFFFFFFFFFFF)
                   (#xAAAAAAAAAAAAAAAA . #x5555555555555555)))
    (let* ((a (car case)) (b (cdr case))
           (ma (mask64 a)) (mb (mask64 b)))
      (sb-simd-test-suite:is (= (logand a b) (mask64-value (mask64-and ma mb))))
      (sb-simd-test-suite:is (= (logior a b) (mask64-value (mask64-or  ma mb))))
      (sb-simd-test-suite:is (= (logxor a b) (mask64-value (mask64-xor ma mb))))
      (sb-simd-test-suite:is
       (= (logand (logxor a (1- (ash 1 64))) b) (mask64-value (mask64-andc1 ma mb))))
      (sb-simd-test-suite:is
       (= (logand (lognot (logxor a b)) (1- (ash 1 64))) (mask64-value (mask64-xnor ma mb))))
      (sb-simd-test-suite:is
       (= (logand (lognot a) (1- (ash 1 64))) (mask64-value (mask64-not ma))))
      (sb-simd-test-suite:is (= (logcount a) (mask64-count ma))))))

(sb-simd-test-suite:define-test mask64-shifts
  (sb-simd-test-suite:is (= (ash #x1 5) (mask64-value (mask64-shiftl (mask64 1) 5))))
  (sb-simd-test-suite:is (= 1 (mask64-value (mask64-shiftr (mask64 (ash 1 5)) 5))))
  (sb-simd-test-suite:is
   (= 0 (mask64-value (mask64-shiftl (mask64 (ash 1 63)) 1)))))

;;; N-ary MASK64-AND/-OR/-XOR sugar (:ASSOCIATIVES), cross-checked
;;; against the underlying TWO-ARG-MASK64-* instructions applied
;;; pairwise, plus the 0/1-argument identity-element cases.
(sb-simd-test-suite:define-test mask64-associatives
  (let ((vals '(#x0F0F0F0F0F0F0F0F #xFF00FF00FF00FF00 #xAAAAAAAAAAAAAAAA
                #x1234567890ABCDEF)))
    (sb-simd-test-suite:is (= (1- (ash 1 64)) (mask64-value (mask64-and))))
    (sb-simd-test-suite:is (= 0 (mask64-value (mask64-or))))
    (sb-simd-test-suite:is (= 0 (mask64-value (mask64-xor))))
    (sb-simd-test-suite:is (= (first vals) (mask64-value (mask64-and (mask64 (first vals))))))
    (sb-simd-test-suite:is (= (first vals) (mask64-value (mask64-or  (mask64 (first vals))))))
    (sb-simd-test-suite:is (= (first vals) (mask64-value (mask64-xor (mask64 (first vals))))))
    (sb-simd-test-suite:is
     (= (reduce #'logand vals) (mask64-value (apply #'mask64-and (mapcar #'mask64 vals)))))
    (sb-simd-test-suite:is
     (= (reduce #'logior vals) (mask64-value (apply #'mask64-or  (mapcar #'mask64 vals)))))
    (sb-simd-test-suite:is
     (= (reduce #'logxor vals) (mask64-value (apply #'mask64-xor (mapcar #'mask64 vals)))))))

(sb-simd-test-suite:define-test mask64-predicates
  (sb-simd-test-suite:is (mask64-zerop (mask64 0)))
  (sb-simd-test-suite:is (not (mask64-zerop (mask64 1))))
  (sb-simd-test-suite:is (not (mask64-zerop (mask64 (1- (ash 1 64))))))
  (sb-simd-test-suite:is (mask64-all-p (mask64 (1- (ash 1 64)))))
  (sb-simd-test-suite:is (not (mask64-all-p (mask64 0))))
  (sb-simd-test-suite:is (not (mask64-all-p (mask64 (1- (1- (ash 1 64))))))))

;;; Mask-producing U8.64 comparisons that stop at the k-register instead
;;; of widening back into a full 0x00/0xFF vector, cross-checked against
;;; the existing (already-shipping) U8.64= etc., which do that widening.
(defun random-u8-64-bytes (&optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat 64 collect (random 256 state))))

(defun mask64-bits-matching (predicate bytes-a bytes-b)
  (loop for i from 0 below 64
        for a in bytes-a
        for b in bytes-b
        when (funcall predicate a b)
          sum (ash 1 i)))

(sb-simd-test-suite:define-test u8.64-mask-comparisons
  (dolist (seed '(1 2 3 4 5))
    (let* ((bytes-a (random-u8-64-bytes seed))
           (bytes-b (random-u8-64-bytes (+ seed 100)))
           (va (apply #'make-u8.64 bytes-a))
           (vb (apply #'make-u8.64 bytes-b)))
      (sb-simd-test-suite:is
       (= (mask64-bits-matching #'= bytes-a bytes-b) (mask64-value (two-arg-u8.64-mask= va vb))))
      (sb-simd-test-suite:is
       (= (mask64-bits-matching #'/= bytes-a bytes-b) (mask64-value (two-arg-u8.64-mask/= va vb))))
      (sb-simd-test-suite:is
       (= (mask64-bits-matching #'< bytes-a bytes-b) (mask64-value (two-arg-u8.64-mask< va vb))))
      (sb-simd-test-suite:is
       (= (mask64-bits-matching #'<= bytes-a bytes-b) (mask64-value (two-arg-u8.64-mask<= va vb))))
      (sb-simd-test-suite:is
       (= (mask64-bits-matching #'> bytes-a bytes-b) (mask64-value (two-arg-u8.64-mask> va vb))))
      (sb-simd-test-suite:is
       (= (mask64-bits-matching #'>= bytes-a bytes-b) (mask64-value (two-arg-u8.64-mask>= va vb))))
      ;; Ties MASK64-COUNT back to the already-shipping vector path.
      (sb-simd-test-suite:is
       (= (logcount (mask64-bits-matching #'= bytes-a bytes-b))
          (mask64-count (two-arg-u8.64-mask= va vb)))))))

;;; N-ary chained U8.64-MASK{=,<,<=,>,>=} (:COMPARISONS) and U8.64-MASK/=
;;; (:UNEQUALS), cross-checked against manually chaining the two-arg
;;; mask-producing comparisons with MASK64-AND.
(sb-simd-test-suite:define-test u8.64-mask-chained-comparisons
  (dolist (seed '(11 12 13))
    (let* ((bytes-a (random-u8-64-bytes seed))
           (bytes-b (random-u8-64-bytes (+ seed 100)))
           (bytes-c (random-u8-64-bytes (+ seed 200)))
           (va (apply #'make-u8.64 bytes-a))
           (vb (apply #'make-u8.64 bytes-b))
           (vc (apply #'make-u8.64 bytes-c)))
      (sb-simd-test-suite:is
       (= (mask64-value (mask64-and (two-arg-u8.64-mask= va vb) (two-arg-u8.64-mask= vb vc)))
          (mask64-value (u8.64-mask= va vb vc))))
      (sb-simd-test-suite:is
       (= (mask64-value (mask64-and (two-arg-u8.64-mask< va vb) (two-arg-u8.64-mask< vb vc)))
          (mask64-value (u8.64-mask< va vb vc))))
      (sb-simd-test-suite:is
       (= (mask64-value (mask64-and (two-arg-u8.64-mask<= va vb) (two-arg-u8.64-mask<= vb vc)))
          (mask64-value (u8.64-mask<= va vb vc))))
      (sb-simd-test-suite:is
       (= (mask64-value (mask64-and (two-arg-u8.64-mask> va vb) (two-arg-u8.64-mask> vb vc)))
          (mask64-value (u8.64-mask> va vb vc))))
      (sb-simd-test-suite:is
       (= (mask64-value (mask64-and (two-arg-u8.64-mask>= va vb) (two-arg-u8.64-mask>= vb vc)))
          (mask64-value (u8.64-mask>= va vb vc))))
      (sb-simd-test-suite:is
       (= (mask64-value
           (mask64-and (two-arg-u8.64-mask/= va vb)
                       (mask64-and (two-arg-u8.64-mask/= va vc)
                                   (two-arg-u8.64-mask/= vb vc))))
          (mask64-value (u8.64-mask/= va vb vc)))))))
