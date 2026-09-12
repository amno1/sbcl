;;; Regression test for the COMPRESS/EXPAND half of AVX512_VBMI2 in
;;; SB-SIMD-AVX512VBMI2. These are the only instructions in the tree
;;; that take a MASK64 argument directly (everywhere else, masking goes
;;; through the -IF/blend abstraction instead) -- COMPRESS/EXPAND are
;;; meaningless without a mask, so there's no unmasked form to build a
;;; -IF on top of. Only the zero-masking form is exposed; see the
;;; comment in instruction-sets/avx512vbmi2.lisp for why.
;;;
;;; The custom VOPs also have to defend against the mask64 argument
;;; landing in K0 (a legal, if usually rare, register-allocation
;;; outcome that would be an invalid predicate for these instructions);
;;; that fallback path was verified directly against real hardware
;;; before being wired in here.

(in-package #:sb-simd-avx512vbmi2)

(defun reference-compress (data mask-value)
  (let ((selected (loop for i below (length data)
                         when (logbitp i mask-value)
                           collect (nth i data))))
    (append selected (make-list (- (length data) (length selected)) :initial-element 0))))

(defun reference-expand (data mask-value)
  (let ((remaining data))
    (loop for i below (length data)
          collect (if (logbitp i mask-value)
                      (pop remaining)
                      0))))

(defun random-bytes (n &optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat n collect (random 256 state))))

(defun random-signed-bytes (n &optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat n collect (- (random 256 state) 128))))

(defun random-mask (bits &optional (seed 1))
  (random (ash 1 bits) (sb-ext:seed-random-state seed)))

(sb-simd-test-suite:define-test u8.64-compress
  (dolist (seed '(1 2 3 4 5))
    (let* ((data (random-bytes 64 seed))
           (mask-value (random-mask 64 (+ seed 100)))
           (expected (reference-compress data mask-value))
           (result (multiple-value-list
                    (u8.64-values (u8.64-compress (apply #'make-u8.64 data) (mask64 mask-value))))))
      (sb-simd-test-suite:is (equal expected result))))
  ;; All-ones / all-zeros edges.
  (let ((data (random-bytes 64 9)))
    (sb-simd-test-suite:is
     (equal data (multiple-value-list (u8.64-values (u8.64-compress (apply #'make-u8.64 data) (mask64 (1- (ash 1 64))))))))
    (sb-simd-test-suite:is
     (equal (make-list 64 :initial-element 0)
            (multiple-value-list (u8.64-values (u8.64-compress (apply #'make-u8.64 data) (mask64 0))))))))

(sb-simd-test-suite:define-test u8.64-expand
  (dolist (seed '(1 2 3 4 5))
    (let* ((mask-value (random-mask 64 (+ seed 100)))
           (data (random-bytes 64 seed))
           (expected (reference-expand data mask-value))
           (result (multiple-value-list
                    (u8.64-values (u8.64-expand (apply #'make-u8.64 data) (mask64 mask-value))))))
      (sb-simd-test-suite:is (equal expected result))))
  (let ((data (random-bytes 64 9)))
    (sb-simd-test-suite:is
     (equal data (multiple-value-list (u8.64-values (u8.64-expand (apply #'make-u8.64 data) (mask64 (1- (ash 1 64))))))))
    (sb-simd-test-suite:is
     (equal (make-list 64 :initial-element 0)
            (multiple-value-list (u8.64-values (u8.64-expand (apply #'make-u8.64 data) (mask64 0))))))))

;; Round-tripping COMPRESS then EXPAND with the same mask recovers the
;; original values in the selected lanes (the unselected lanes are lost
;; by COMPRESS, so those come back as 0, not their original values).
(sb-simd-test-suite:define-test u8.64-compress-expand-round-trip
  (dolist (seed '(1 2 3 4 5))
    (let* ((data (random-bytes 64 seed))
           (mask-value (random-mask 64 (+ seed 100)))
           (compressed (u8.64-compress (apply #'make-u8.64 data) (mask64 mask-value)))
           (round-tripped (multiple-value-list (u8.64-values (u8.64-expand compressed (mask64 mask-value)))))
           (expected (loop for i below 64 collect (if (logbitp i mask-value) (nth i data) 0))))
      (sb-simd-test-suite:is (equal expected round-tripped)))))

;; S8.64/U16.32/S16.32 share the same underlying instructions as U8.64
;; (byte-identical mechanics, just a different accessor); one spot
;; check per type confirms the wiring rather than repeating the full
;; U8.64 sweep.
(sb-simd-test-suite:define-test compress-expand-other-widths
  (let* ((data (random-signed-bytes 64 21))
         (mask-value (random-mask 64 22))
         (expected (reference-compress data mask-value)))
    (sb-simd-test-suite:is
     (equal expected (multiple-value-list (s8.64-values (s8.64-compress (apply #'make-s8.64 data) (mask64 mask-value)))))))
  (let* ((data (random-bytes 32 23))
         (mask-value (random-mask 32 24))
         (expected (reference-compress data mask-value)))
    (sb-simd-test-suite:is
     (equal expected (multiple-value-list (u16.32-values (u16.32-compress (apply #'make-u16.32 data) (mask64 mask-value))))))
    (sb-simd-test-suite:is
     (equal (reference-expand data mask-value)
            (multiple-value-list (u16.32-values (u16.32-expand (apply #'make-u16.32 data) (mask64 mask-value)))))))
  (let* ((data (random-signed-bytes 32 25))
         (mask-value (random-mask 32 26))
         (expected (reference-expand data mask-value)))
    (sb-simd-test-suite:is
     (equal expected (multiple-value-list (s16.32-values (s16.32-expand (apply #'make-s16.32 data) (mask64 mask-value))))))))
