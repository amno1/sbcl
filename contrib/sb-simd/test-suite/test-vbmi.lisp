;;; Regression test for AVX512_VBMI in SB-SIMD-AVX512VBMI. None of these
;;; four instructions are per-lane-1:1 (a permute/gather reads DATA at a
;;; position other than the output lane, and MULTISHIFT's DATA operand
;;; is qword-grouped like GFNI's affine matrix), so -- like MASK64,
;;; VNNI, BITALG, and GFNI before it -- this is hand-written rather than
;;; run through the generic per-lane property harness.

(in-package #:sb-simd-avx512vbmi)

(defun random-bytes (n &optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat n collect (random 256 state))))

(defun random-qwords (n &optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat n collect (random (ash 1 64) state))))

(defun reference-permute (indices data)
  (loop for idx in indices collect (nth (logand idx #x3F) data)))

(defun reference-permute2 (table1 indices table2)
  (loop for idx in indices
        collect (nth (logand idx #x3F) (if (logbitp 6 idx) table2 table1))))

(defun reference-multishift (control data-qwords)
  (loop for j below 8
        for tcur = (nth j data-qwords)
        append (loop for b below 8
                     for ctrl = (logand (nth (+ (* j 8) b) control) #x3F)
                     collect (loop for k below 8
                                   sum (ash (ldb (byte 1 (mod (+ ctrl k) 64)) tcur) k)))))

(sb-simd-test-suite:define-test u8.64-permute
  (dolist (seed '(1 2 3 4 5))
    (let* ((indices (random-bytes 64 seed))
           (data (random-bytes 64 (+ seed 100)))
           (expected (reference-permute indices data))
           (result (multiple-value-list
                    (u8.64-values (u8.64-permute (apply #'make-u8.64 indices) (apply #'make-u8.64 data))))))
      (sb-simd-test-suite:is (equal expected result))))
  ;; Index bytes are masked to 6 bits: byte value 68 (=64+4) must select
  ;; the same element as 4.
  (let* ((indices (append '(68 4) (make-list 62 :initial-element 0)))
         (data (random-bytes 64 9))
         (result (multiple-value-list
                  (u8.64-values (u8.64-permute (apply #'make-u8.64 indices) (apply #'make-u8.64 data))))))
    (sb-simd-test-suite:is (eql (first result) (second result)))))

(sb-simd-test-suite:define-test u8.64-permute2
  (dolist (seed '(1 2 3 4 5))
    (let* ((table1 (random-bytes 64 seed))
           (indices (random-bytes 64 (+ seed 100)))
           (table2 (random-bytes 64 (+ seed 200)))
           (expected (reference-permute2 table1 indices table2))
           (result (multiple-value-list
                    (u8.64-values
                     (u8.64-permute2 (apply #'make-u8.64 table1)
                                     (apply #'make-u8.64 indices)
                                     (apply #'make-u8.64 table2))))))
      (sb-simd-test-suite:is (equal expected result))))
  ;; Explicit table-select-bit check: bit 6 clear picks TABLE1, set
  ;; picks TABLE2, for the same low-6-bit index.
  (let* ((table1 (random-bytes 64 11))
         (table2 (random-bytes 64 12))
         (indices (append (list 5 (logior 5 #x40)) (make-list 62 :initial-element 0)))
         (result (multiple-value-list
                  (u8.64-values
                   (u8.64-permute2 (apply #'make-u8.64 table1)
                                   (apply #'make-u8.64 indices)
                                   (apply #'make-u8.64 table2))))))
    (sb-simd-test-suite:is (eql (first result) (nth 5 table1)))
    (sb-simd-test-suite:is (eql (second result) (nth 5 table2)))))

(sb-simd-test-suite:define-test u8.64-permute2-i
  ;; U8.64-PERMUTE2-I (VPERMI2B) computes the identical logical
  ;; operation as U8.64-PERMUTE2 (VPERMT2B), only which operand is
  ;; read-modify-write differs, so it's checked against the same
  ;; reference function.
  (dolist (seed '(1 2 3 4 5))
    (let* ((table1 (random-bytes 64 seed))
           (indices (random-bytes 64 (+ seed 100)))
           (table2 (random-bytes 64 (+ seed 200)))
           (expected (reference-permute2 table1 indices table2))
           (result (multiple-value-list
                    (u8.64-values
                     (u8.64-permute2-i (apply #'make-u8.64 indices)
                                       (apply #'make-u8.64 table1)
                                       (apply #'make-u8.64 table2))))))
      (sb-simd-test-suite:is (equal expected result)))))

(sb-simd-test-suite:define-test u8.64-multishift
  (dolist (seed '(1 2 3 4 5))
    (let* ((control (random-bytes 64 seed))
           (data-qwords (random-qwords 8 (+ seed 100)))
           (expected (reference-multishift control data-qwords))
           (result (multiple-value-list
                    (u8.64-values
                     (u8.64-multishift (apply #'make-u8.64 control) (apply #'make-u64.8 data-qwords))))))
      (sb-simd-test-suite:is (equal expected result))))
  ;; Control byte >= 64 wraps mod 64: 69 (=64+5) must behave like 5.
  (let* ((control (append '(69 5) (make-list 62 :initial-element 0)))
         (data-qwords (cons #xDEADBEEFCAFEBABE (make-list 7 :initial-element 0)))
         (result (multiple-value-list
                  (u8.64-values (u8.64-multishift (apply #'make-u8.64 control) (apply #'make-u64.8 data-qwords))))))
    (sb-simd-test-suite:is (eql (first result) (second result)))))
