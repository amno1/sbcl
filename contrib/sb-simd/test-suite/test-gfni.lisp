;;; Regression test for GFNI (Galois Field New Instructions) in
;;; SB-SIMD-AVX512GFNI. GF2P8MULB is a plain per-lane U8.64 op, but the
;;; affine transforms mix U8.64 (per-byte DATA) with U64.8 (one 64-bit
;;; matrix per 8-byte group of DATA) plus an IMM8 constant -- not a
;;; per-lane-1:1 shape, so all three get hand-written references here,
;;; matching every SDM pseudocode operand and byte index literally
;;; (including the AFFINE_BYTE definition's reversed BYTE[7-I]
;;; indexing, which is easy to get backwards).

(in-package #:sb-simd-avx512gfni)

(defun gf2p8mul-byte (a b)
  "GF(2^8) multiply of two bytes, reduction polynomial x^8+x^4+x^3+x+1
(0x11B), per the SDM's GF2P8MULB pseudocode."
  (let ((tword 0))
    (dotimes (i 8)
      (when (logbitp i b)
        (setf tword (logxor tword (ash a i)))))
    (loop for i from 14 downto 8
          for p = (ash #x11B (- i 8))
          when (logbitp i tword)
            do (setf tword (logxor tword p)))
    (ldb (byte 8 0) tword)))

(defun gf2p8-inverse (x)
  "GF(2^8) multiplicative inverse of X, 0 for X=0 by convention (as in
the SDM's own inverse-byte table), found by brute force against
GF2P8MUL-BYTE rather than transcribing the 256-entry SDM table by hand."
  (if (zerop x)
      0
      (loop for y from 1 to 255
            when (= 1 (gf2p8mul-byte x y))
              return y)))

;; Cross-check the brute-force inverse against entries read directly off
;; the SDM's own table (row = upper nibble, column = lower nibble):
;; inv(0x00)=0x00, inv(0x01)=0x01, inv(0x95)=0x8A (the SDM's own worked
;; example), inv(0xFF)=0x1C.
(sb-simd-test-suite:define-test gf2p8-inverse-matches-sdm-table
  (sb-simd-test-suite:is (= #x00 (gf2p8-inverse #x00)))
  (sb-simd-test-suite:is (= #x01 (gf2p8-inverse #x01)))
  (sb-simd-test-suite:is (= #x8A (gf2p8-inverse #x95)))
  (sb-simd-test-suite:is (= #x1C (gf2p8-inverse #xFF))))

(defun parity8 (x)
  (mod (logcount x) 2))

(defun affine-byte (tsrc2qw src1byte imm)
  "AFFINE_BYTE(tsrc2qw, src1byte, imm) from the SDM: note byte[7-I], not
byte[I] -- matrix byte I is paired with output bit (7-I)'s complement,
i.e. the matrix is consulted back-to-front relative to the output bit."
  (let ((retbyte 0))
    (dotimes (i 8)
      (let ((bit (logxor (parity8 (logand (ldb (byte 8 (* 8 (- 7 i))) tsrc2qw) src1byte))
                          (ldb (byte 1 i) imm))))
        (setf retbyte (logior retbyte (ash bit i)))))
    retbyte))

(defun reference-gf2p8affine (data-bytes matrix-qwords imm &optional invert)
  (loop for j below 8
        for tsrc2qw = (nth j matrix-qwords)
        append (loop for b below 8
                     for x = (nth (+ (* j 8) b) data-bytes)
                     collect (affine-byte tsrc2qw (if invert (gf2p8-inverse x) x) imm))))

(defun random-bytes (n &optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat n collect (random 256 state))))

(defun random-qwords (n &optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat n collect (random (ash 1 64) state))))

(sb-simd-test-suite:define-test u8.64-gf2p8mulb
  (dolist (seed '(1 2 3 4 5))
    (let* ((a (random-bytes 64 seed))
           (b (random-bytes 64 (+ seed 100)))
           (expected (mapcar #'gf2p8mul-byte a b))
           (result (multiple-value-list
                    (u8.64-values (u8.64-gf2p8mulb (apply #'make-u8.64 a) (apply #'make-u8.64 b))))))
      (sb-simd-test-suite:is (equal expected result))))
  ;; 0 is the multiplicative absorbing element, and 1 the identity, in
  ;; any field -- cheap, meaningful edge cases beyond pure randomness.
  (let* ((a (random-bytes 64 7))
         (zeros (make-list 64 :initial-element 0))
         (ones (make-list 64 :initial-element 1)))
    (sb-simd-test-suite:is
     (equal zeros (multiple-value-list (u8.64-values (u8.64-gf2p8mulb (apply #'make-u8.64 a) (apply #'make-u8.64 zeros))))))
    (sb-simd-test-suite:is
     (equal a (multiple-value-list (u8.64-values (u8.64-gf2p8mulb (apply #'make-u8.64 a) (apply #'make-u8.64 ones))))))))

(sb-simd-test-suite:define-test u8.64-gf2p8affineqb
  (dolist (seed '(1 2 3 4 5))
    (let* ((data (random-bytes 64 seed))
           (matrix (random-qwords 8 (+ seed 100)))
           (imm (random 256 (sb-ext:seed-random-state (+ seed 200))))
           (expected (reference-gf2p8affine data matrix imm))
           (result (multiple-value-list
                    (u8.64-values
                     (u8.64-gf2p8affineqb (apply #'make-u8.64 data) (apply #'make-u64.8 matrix) imm)))))
      (sb-simd-test-suite:is (equal expected result)))))

(sb-simd-test-suite:define-test u8.64-gf2p8affineinvqb
  (dolist (seed '(1 2 3 4 5))
    (let* ((data (random-bytes 64 seed))
           (matrix (random-qwords 8 (+ seed 100)))
           (imm (random 256 (sb-ext:seed-random-state (+ seed 200))))
           (expected (reference-gf2p8affine data matrix imm t))
           (result (multiple-value-list
                    (u8.64-values
                     (u8.64-gf2p8affineinvqb (apply #'make-u8.64 data) (apply #'make-u64.8 matrix) imm)))))
      (sb-simd-test-suite:is (equal expected result))))
  ;; The identity matrix (each qword = 0x8040201008040201, i.e. matrix
  ;; row i has only bit (7-i) set) with imm=0 makes AFFINE_BYTE just
  ;; return its SRC1BYTE argument unchanged, so AFFINEINVQB with the
  ;; identity matrix and imm=0 must equal the plain per-byte inverse.
  (let* ((data (random-bytes 64 9))
         ;; byte j = (1 << (7-j)) for j=0..7, so that AFFINE_BYTE's
         ;; byte[7-i]-indexed parity check degenerates to picking out
         ;; SRC1BYTE's own bit i directly.
         (identity-qword #x0102040810204080)
         (matrix (make-list 8 :initial-element identity-qword))
         (expected (mapcar #'gf2p8-inverse data))
         (result (multiple-value-list
                  (u8.64-values
                   (u8.64-gf2p8affineinvqb (apply #'make-u8.64 data) (apply #'make-u64.8 matrix) 0)))))
    (sb-simd-test-suite:is (equal expected result))))
