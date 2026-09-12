;;; Regression test for U8.64-BIT-GATHER (VPSHUFBITQMB) in
;;; SB-SIMD-AVX512BITALG. Not a per-lane 1:1 shape (like the generic
;;; harness assumes) or a matching-type FMA-ish shape (like VNNI) --
;;; each of the 8 independent 64-bit qword lanes gathers its own 8
;;; result bits from a full 64-bit DATA qword, indexed 6 bits at a time
;;; by the corresponding byte of INDICES. Hand-written, like MASK64 and
;;; VNNI before it.
;;;
;;; Argument order is (data indices), matching the assembler's own
;;; (dst src1 src2): src1 is DATA, src2 is INDICES. Confirmed against
;;; Intel's documented pseudocode and by directly executing the
;;; instruction on real AVX512_BITALG hardware (a throwaway custom VOP,
;;; probing 14 distinct index values one at a time against a data
;;; operand with a unique bit pattern per position) -- worth recording
;;; because a first, sloppier hardware probe pointed the wrong way, not
;;; because the hardware disagreed with itself.

(in-package #:sb-simd-avx512bitalg)

(defun reference-bit-gather (data-bytes idx-bytes)
  "DATA-BYTES and IDX-BYTES are lists of 64 (unsigned-byte 8); returns
the (unsigned-byte 64) VPSHUFBITQMB would produce."
  (loop for j below 8
        for data-qword = (loop for i below 8
                                sum (ash (nth (+ (* j 8) i) data-bytes) (* i 8)))
        sum (loop for i below 8
                  for byte-pos = (+ (* j 8) i)
                  for m = (logand (nth byte-pos idx-bytes) #x3F)
                  when (logbitp m data-qword)
                    sum (ash 1 byte-pos))))

(defun random-bytes (&optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat 64 collect (random 256 state))))

(sb-simd-test-suite:define-test u8.64-bit-gather-known-answer
  ;; qword 0: data qword 0 has only bit 0 set, so index 0 selects 1 and
  ;; every other index selects 0. Index byte 0 = 5 (selects bit 5 = 0);
  ;; every other index byte in qword 0 is 0 (selects bit 0 = 1).
  (let* ((data (append '(1 0 0 0 0 0 0 0) (make-list 56 :initial-element 0)))
         (idx (append '(5 0 0 0 0 0 0 0) (make-list 56 :initial-element 0)))
         (expected (reference-bit-gather data idx))
         (result (mask64-value
                  (u8.64-bit-gather (apply #'make-u8.64 data) (apply #'make-u8.64 idx)))))
    (sb-simd-test-suite:is (= expected result))
    (sb-simd-test-suite:is (= #b11111110 (ldb (byte 8 0) result)))))

(sb-simd-test-suite:define-test u8.64-bit-gather-index-wraps-mod-64
  ;; An index byte >= 64 must behave as (index mod 64), i.e. only the
  ;; low 6 bits matter -- byte value 69 (= 64+5) must act exactly like 5.
  (let* ((data (append '(#x20 #x20) (make-list 62 :initial-element 0)))
         (idx (append '(69 5) (make-list 62 :initial-element 0)))
         (r (mask64-value (u8.64-bit-gather (apply #'make-u8.64 data) (apply #'make-u8.64 idx)))))
    (sb-simd-test-suite:is (eql (logbitp 0 r) (logbitp 1 r)))))

(sb-simd-test-suite:define-test u8.64-bit-gather-random
  (dolist (seed '(1 2 3 4 5 6 7))
    (let* ((data (random-bytes seed))
           (idx (random-bytes (+ seed 100)))
           (expected (reference-bit-gather data idx))
           (result (mask64-value
                    (u8.64-bit-gather (apply #'make-u8.64 data) (apply #'make-u8.64 idx)))))
      (sb-simd-test-suite:is (= expected result)))))
