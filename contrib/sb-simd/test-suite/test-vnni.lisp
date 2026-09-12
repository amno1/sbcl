;;; Regression tests for AVX512_VNNI dot-product-and-accumulate support
;;; in SB-SIMD-AVX512VNNI. All four instructions (VPDPBUSD/-BUSDS,
;;; VPDPWSSD/-WSSDS) alias their first argument with the result via
;;; :ENCODING :FMA even though the three operands don't share a type --
;;; this exercises that aliasing across mismatched types, which nothing
;;; else in the tree does yet.

(in-package #:sb-simd-avx512vnni)

;; Reduce X modulo 2^32 and reinterpret as a signed 32-bit integer,
;; matching the non-saturating VPDPBUSD/VPDPWSSD accumulate.
(defun s32-wrap (x)
  (let ((m (ldb (byte 32 0) x)))
    (if (>= m #x80000000) (- m #x100000000) m)))

;; Clamp X into the signed 32-bit range, matching the saturating
;; VPDPBUSDS/VPDPWSSDS accumulate.
(defun s32-saturate (x)
  (cond ((> x #x7FFFFFFF) #x7FFFFFFF)
        ((< x #x-80000000) #x-80000000)
        (t x)))

(defun random-s32-16-values (&optional (seed 1) (limit (ash 1 31)))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat 16 collect (- (random (* 2 limit) state) limit))))

(defun random-u8-64-values (&optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat 64 collect (random 256 state))))

(defun random-s8-64-values (&optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat 64 collect (- (random 256 state) 128))))

(defun random-s16-32-values (&optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat 32 collect (- (random 65536 state) 32768))))

;;; Reference dot-product-accumulate: NARROW-VALUES-PER-LANE bytes/words
;;; of each source vector fold into one output lane, in step with
;;; accumulator lane K's own initial value.
(defun reference-dpaccumulate (accs xs ys narrow-values-per-lane clamp)
  (loop for k below (length accs)
        for acc in accs
        collect (funcall clamp
                         (+ acc (loop for i below narrow-values-per-lane
                                      for idx = (+ (* k narrow-values-per-lane) i)
                                      sum (* (nth idx xs) (nth idx ys)))))))

(sb-simd-test-suite:define-test s32.16-dpbusd
  (dolist (seed '(1 2 3 4 5))
    (let* ((accs (random-s32-16-values seed))
           (u8s (random-u8-64-values seed))
           (s8s (random-s8-64-values (+ seed 100)))
           (expected (reference-dpaccumulate accs u8s s8s 4 #'s32-wrap))
           (result (multiple-value-list
                    (s32.16-values
                     (s32.16-dpbusd (apply #'make-s32.16 accs)
                                    (apply #'make-u8.64 u8s)
                                    (apply #'make-s8.64 s8s))))))
      (sb-simd-test-suite:is (equal expected result)))))

(sb-simd-test-suite:define-test s32.16-dpwssd
  (dolist (seed '(1 2 3 4 5))
    (let* ((accs (random-s32-16-values seed))
           (s16a (random-s16-32-values seed))
           (s16b (random-s16-32-values (+ seed 100)))
           (expected (reference-dpaccumulate accs s16a s16b 2 #'s32-wrap))
           (result (multiple-value-list
                    (s32.16-values
                     (s32.16-dpwssd (apply #'make-s32.16 accs)
                                    (apply #'make-s16.32 s16a)
                                    (apply #'make-s16.32 s16b))))))
      (sb-simd-test-suite:is (equal expected result)))))

;;; Saturating variants: random cases (where no overflow occurs, so
;;; wrap and saturate agree) plus deliberately constructed cases that
;;; push the accumulator to the edge of the signed 32-bit range.
(sb-simd-test-suite:define-test s32.16-dpbusds
  (dolist (seed '(1 2 3 4 5))
    (let* ((accs (random-s32-16-values seed 1000))
           (u8s (random-u8-64-values seed))
           (s8s (random-s8-64-values (+ seed 100)))
           (expected (reference-dpaccumulate accs u8s s8s 4 #'s32-saturate))
           (result (multiple-value-list
                    (s32.16-values
                     (s32.16-dpbusds (apply #'make-s32.16 accs)
                                     (apply #'make-u8.64 u8s)
                                     (apply #'make-s8.64 s8s))))))
      (sb-simd-test-suite:is (equal expected result))))
  ;; Force positive and negative saturation explicitly.
  (let* ((accs (list* #x7FFFFF00 #x-7FFFFF00 (make-list 14 :initial-element 0)))
         (u8s (append '(255 255 255 255 255 255 255 255) (make-list 56 :initial-element 0)))
         (s8s (append '(127 127 127 127 -128 -128 -128 -128) (make-list 56 :initial-element 0)))
         (expected (reference-dpaccumulate accs u8s s8s 4 #'s32-saturate))
         (result (multiple-value-list
                  (s32.16-values
                   (s32.16-dpbusds (apply #'make-s32.16 accs)
                                   (apply #'make-u8.64 u8s)
                                   (apply #'make-s8.64 s8s))))))
    (sb-simd-test-suite:is (= (first expected) #x7FFFFFFF))
    (sb-simd-test-suite:is (= (second expected) #x-80000000))
    (sb-simd-test-suite:is (equal expected result))))

(sb-simd-test-suite:define-test s32.16-dpwssds
  (dolist (seed '(1 2 3 4 5))
    (let* ((accs (random-s32-16-values seed 1000))
           (s16a (random-s16-32-values seed))
           (s16b (random-s16-32-values (+ seed 100)))
           (expected (reference-dpaccumulate accs s16a s16b 2 #'s32-saturate))
           (result (multiple-value-list
                    (s32.16-values
                     (s32.16-dpwssds (apply #'make-s32.16 accs)
                                     (apply #'make-s16.32 s16a)
                                     (apply #'make-s16.32 s16b))))))
      (sb-simd-test-suite:is (equal expected result))))
  ;; Force positive and negative saturation explicitly.
  (let* ((accs (list* #x7FFFFF00 #x-7FFFFF00 (make-list 14 :initial-element 0)))
         (s16a (append '(32767 32767 -32768 -32768) (make-list 28 :initial-element 0)))
         (s16b (append '(32767 32767 32767 32767) (make-list 28 :initial-element 0)))
         (expected (reference-dpaccumulate accs s16a s16b 2 #'s32-saturate))
         (result (multiple-value-list
                  (s32.16-values
                   (s32.16-dpwssds (apply #'make-s32.16 accs)
                                   (apply #'make-s16.32 s16a)
                                   (apply #'make-s16.32 s16b))))))
    (sb-simd-test-suite:is (= (first expected) #x7FFFFFFF))
    (sb-simd-test-suite:is (= (second expected) #x-80000000))
    (sb-simd-test-suite:is (equal expected result))))
