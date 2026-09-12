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

;;; Funnel shift: concatenate two same-width lanes and shift, taking
;;; the half nearer the shift direction. SHLDI/SHRDI take an imm8
;;; count (no destination read); SHLDV/SHRDV take a per-lane count
;;; vector, with the destination's pre-existing content standing in
;;; for one of the two concatenated values -- confirmed on real
;;; hardware (a throwaway custom VOP) before wiring this in.

(defun funshiftl (primary fill count width)
  (let* ((n (logand count (1- width)))
         (concat (logior (ash primary width) fill))
         (mod (ash 1 (* 2 width))))
    (ldb (byte width width) (mod (ash concat n) mod))))

(defun funshiftr (primary fill count width)
  (let* ((n (logand count (1- width)))
         (concat (logior (ash fill width) primary)))
    (ldb (byte width 0) (ash concat (- n)))))

(defun random-words (n bits &optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat n collect (random (ash 1 bits) state))))

(defun random-signed-words (n bits &optional (seed 1))
  (let ((state (sb-ext:seed-random-state seed))
        (half (ash 1 (1- bits))))
    (loop repeat n collect (- (random (ash 1 bits) state) half))))

(defun to-unsigned (x bits)
  (ldb (byte bits 0) x))

(defun to-signed (x bits)
  (if (logbitp (1- bits) x) (- x (ash 1 bits)) x))

(sb-simd-test-suite:define-test u16.32-shldi
  (dolist (seed '(1 2 3 4 5))
    (let* ((primary (random-words 32 16 seed))
           (fill (random-words 32 16 (+ seed 100)))
           (count (random 16 (sb-ext:seed-random-state (+ seed 200))))
           (expected (mapcar (lambda (p f) (funshiftl p f count 16)) primary fill))
           (result (multiple-value-list
                    (u16.32-values (u16.32-shldi (apply #'make-u16.32 primary) (apply #'make-u16.32 fill) count)))))
      (sb-simd-test-suite:is (equal expected result))))
  ;; count=0 must leave PRIMARY unchanged.
  (let ((primary (random-words 32 16 9)) (fill (random-words 32 16 10)))
    (sb-simd-test-suite:is
     (equal primary (multiple-value-list (u16.32-values (u16.32-shldi (apply #'make-u16.32 primary) (apply #'make-u16.32 fill) 0)))))))

(sb-simd-test-suite:define-test u16.32-shrdi
  (dolist (seed '(1 2 3 4 5))
    (let* ((primary (random-words 32 16 seed))
           (fill (random-words 32 16 (+ seed 100)))
           (count (random 16 (sb-ext:seed-random-state (+ seed 200))))
           (expected (mapcar (lambda (p f) (funshiftr p f count 16)) primary fill))
           (result (multiple-value-list
                    (u16.32-values (u16.32-shrdi (apply #'make-u16.32 primary) (apply #'make-u16.32 fill) count)))))
      (sb-simd-test-suite:is (equal expected result))))
  (let ((primary (random-words 32 16 9)) (fill (random-words 32 16 10)))
    (sb-simd-test-suite:is
     (equal primary (multiple-value-list (u16.32-values (u16.32-shrdi (apply #'make-u16.32 primary) (apply #'make-u16.32 fill) 0)))))))

(sb-simd-test-suite:define-test u16.32-shldv
  (dolist (seed '(1 2 3 4 5))
    (let* ((primary (random-words 32 16 seed))
           (fill (random-words 32 16 (+ seed 100)))
           (count (random-words 32 16 (+ seed 200)))
           (expected (mapcar (lambda (p f c) (funshiftl p f c 16)) primary fill count))
           (result (multiple-value-list
                    (u16.32-values (u16.32-shldv (apply #'make-u16.32 primary) (apply #'make-u16.32 fill) (apply #'make-u16.32 count))))))
      (sb-simd-test-suite:is (equal expected result))))
  ;; Count bytes >= 16 must wrap mod 16 (only the low 4 bits matter).
  (let* ((primary (random-words 32 16 9))
         (fill (random-words 32 16 10))
         (count (cons 19 (make-list 31 :initial-element 0)))
         (count2 (cons 3 (make-list 31 :initial-element 0)))
         (r1 (multiple-value-list (u16.32-values (u16.32-shldv (apply #'make-u16.32 primary) (apply #'make-u16.32 fill) (apply #'make-u16.32 count)))))
         (r2 (multiple-value-list (u16.32-values (u16.32-shldv (apply #'make-u16.32 primary) (apply #'make-u16.32 fill) (apply #'make-u16.32 count2))))))
    (sb-simd-test-suite:is (equal r1 r2))))

(sb-simd-test-suite:define-test u16.32-shrdv
  (dolist (seed '(1 2 3 4 5))
    (let* ((primary (random-words 32 16 seed))
           (fill (random-words 32 16 (+ seed 100)))
           (count (random-words 32 16 (+ seed 200)))
           (expected (mapcar (lambda (p f c) (funshiftr p f c 16)) primary fill count))
           (result (multiple-value-list
                    (u16.32-values (u16.32-shrdv (apply #'make-u16.32 primary) (apply #'make-u16.32 fill) (apply #'make-u16.32 count))))))
      (sb-simd-test-suite:is (equal expected result)))))

;; U32.16/U64.8 (dword/qword width) and the signed siblings share the
;; same VPSHLD/VPSHRD family, just a different opcode selected by
;; width -- one spot check per width/signedness confirms the wiring.
(sb-simd-test-suite:define-test funshift-other-widths
  (let* ((primary (random-words 16 32 31)) (fill (random-words 16 32 32)) (count (random 32 (sb-ext:seed-random-state 33)))
         (expected (mapcar (lambda (p f) (funshiftl p f count 32)) primary fill)))
    (sb-simd-test-suite:is
     (equal expected (multiple-value-list (u32.16-values (u32.16-shldi (apply #'make-u32.16 primary) (apply #'make-u32.16 fill) count))))))
  (let* ((primary (random-signed-words 16 32 34)) (fill (random-signed-words 16 32 35)) (count (random-words 16 32 36))
         (expected (mapcar (lambda (p f c) (to-signed (funshiftr (to-unsigned p 32) (to-unsigned f 32) c 32) 32))
                            primary fill count)))
    (sb-simd-test-suite:is
     (equal expected (multiple-value-list (s32.16-values (s32.16-shrdv (apply #'make-s32.16 primary) (apply #'make-s32.16 fill) (apply #'make-u32.16 count)))))))
  (let* ((primary (random-words 8 64 37)) (fill (random-words 8 64 38)) (count (random 64 (sb-ext:seed-random-state 39)))
         (expected (mapcar (lambda (p f) (funshiftr p f count 64)) primary fill)))
    (sb-simd-test-suite:is
     (equal expected (multiple-value-list (u64.8-values (u64.8-shrdi (apply #'make-u64.8 primary) (apply #'make-u64.8 fill) count))))))
  (let* ((primary (random-signed-words 8 64 40)) (fill (random-signed-words 8 64 41)) (count (random-words 8 64 42))
         (expected (mapcar (lambda (p f c) (to-signed (funshiftl (to-unsigned p 64) (to-unsigned f 64) c 64) 64))
                            primary fill count)))
    (sb-simd-test-suite:is
     (equal expected
            (multiple-value-list (s64.8-values (s64.8-shldv (apply #'make-s64.8 primary) (apply #'make-s64.8 fill) (apply #'make-u64.8 count))))))))
