;;;; Shortest round-trip decimal conversion of SINGLE-FLOAT and
;;;; DOUBLE-FLOAT using the Żmij algorithm.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; A port of Victor Zverovich's C implementation from
;;;; https://github.com/vitaut/zmij, MIT license
;;;;
;;;; For details on the algorithm see
;;;; https://vitaut.net/posts/2025/faster-dtoa/

(in-package "SB-IMPL")

;;;; Tables

(defconstant +zmij-pow10-min-exp+ -307)
(defconstant +zmij-pow10-max-exp+ 341)
(defconstant +zmij-double-exp-offset+ 1075) ; bias + 52 significand bits
(defconstant +zmij-float-exp-offset+ 150)   ; bias + 23 significand bits

(declaim (inline zmij-dec-exp zmij-exp-shift))
(defun zmij-dec-exp (bin-exp regular)
  (declare (type (integer -2000 2000) bin-exp))
  (ash (- (* bin-exp 315653) (if regular 0 131072)) -20))

(defun zmij-exp-shift (bin-exp dec-exp)
  (declare (type (integer -2000 2000) bin-exp)
           (type (integer -1000 1000) dec-exp))
  (+ bin-exp (ash (* (- dec-exp) 217707) -16) 1))

;;; 128-bit significands of 10^k for k in [-307, 341], stored as
;;; interleaved (hi, lo) pairs. Each entry is reconstructed from a
;;; coarse power (MAJOR) times a small power (MINOR), with a per-entry
;;; correction bit (FIXUPS), exactly as the C implementation does.
(define-load-time-global *zmij-pow10*
  #.(let ((minor
            '(#x8000000000000000 #xa000000000000000 #xc800000000000000
              #xfa00000000000000 #x9c40000000000000 #xc350000000000000
              #xf424000000000000 #x9896800000000000 #xbebc200000000000
              #xee6b280000000000 #x9502f90000000000 #xba43b74000000000
              #xe8d4a51000000000 #x9184e72a00000000 #xb5e620f480000000
              #xe35fa931a0000000 #x8e1bc9bf04000000 #xb1a2bc2ec5000000
              #xde0b6b3a76400000 #x8ac7230489e80000 #xad78ebc5ac620000
              #xd8d726b7177a8000 #x878678326eac9000 #xa968163f0a57b400
              #xd3c21bcecceda100 #x84595161401484a0 #xa56fa5b99019a5c8
              #xcecb8f27f4200f3a))
          (major
            '((#xaddcb9e83c6b1793 . #xdf4abe242a1bbf3e)
              (#xaf8e5410288e1b6f . #x07ecf0ae5ee44dda)
              (#xb1442798f49ffb4a . #x99cd11cfdf41779d)
              (#xb2fe3f0b8599ef07 . #x861fa7e6dcb4aa15)
              (#xb4bca50b065abe63 . #x0fed077a756b53aa)
              (#xb67f6455292cbf08 . #x1a3bc84c17b1d543)
              (#xb84687c269ef3bfb . #x3d5d514f40eea742)
              (#xba121a4650e4ddeb . #x92f34d62616ce413)
              (#xbbe226efb628afea . #x890489f70a55368c)
              (#xbdb6b8e905cb600f . #x5400e987bbc1c921)
              (#xbf8fdb78849a5f96 . #xde98520472bdd034)
              (#xc16d9a0095928a27 . #x75b7053c0f178294)
              (#xc350000000000000 . #x0000000000000000)
              (#xc5371912364ce305 . #x6c28000000000000)
              (#xc722f0ef9d80aad6 . #x424d3ad2b7b97ef6)
              (#xc913936dd571c84c . #x03bc3a19cd1e38ea)
              (#xcb090c8001ab551c . #x5cadf5bfd3072cc6)
              (#xcd036837130890a1 . #x36dba887c37a8c10)
              (#xcf02b2c21207ef2e . #x94f967e45e03f4bc)
              (#xd106f86e69d785c7 . #xe13336d701beba52)
              (#xd31045a8341ca07c . #x1ede48111209a051)
              (#xd51ea6fa85785631 . #x552a74227f3ea566)
              (#xd732290fbacaf133 . #xa97c177947ad4096)
              (#xd94ad8b1c7380874 . #x18375281ae7822bd)
              (#xdb68c2ca82ed2a05 . #xa67398db9f6820e1)))
          (fixups
            '(#x8d8fc810 #x06100293 #x19000000 #x00100000 #x00000908 #x00000000
              #x04e00300 #x3807e0b2 #x3d83d793 #x0006f5cc #x00000000 #xffff0000
              #x8076337d #x4ff45ba0 #x09405033 #x034376d9 #x09000000 #x4e100501
              #x076d14dc #xf964f45e #x0000003d))
          (table (sb-xc:make-array (* 2 (- 341 -307 -1))
                                   :element-type '(unsigned-byte 64))))
      (loop for i from 0 to (- 341 -307)
            do (let* ((j (+ i 24))
                      (m (nth (mod j 28) minor))
                      (h (nth (floor j 28) major))
                      (p (* (logior (ash (car h) 64) (cdr h)) m))
                      (n (if (logbitp 191 p)
                             (ash p -64)
                             (ldb (byte 128 0) (ash p -63))))
                      (hi (ldb (byte 64 64) n))
                      (lo (ldb (byte 64 0) n)))
                 (when (logbitp (logand i 31) (nth (ash i -5) fixups))
                   (setq lo (ldb (byte 64 0) (1- lo))))
                 (setf (aref table (* 2 i)) hi
                       (aref table (1+ (* 2 i))) lo)))
      table))
(declaim (type (simple-array (unsigned-byte 64) (1298)) *zmij-pow10*))

;;; Shift amounts for regular doubles, indexed by biased exponent.
(define-load-time-global *zmij-exp-shifts*
  #.(sb-xc:make-array
     2048
     :element-type '(unsigned-byte 8)
     :initial-contents
     (loop for raw-exp from 0 below 2048
           collect (let* ((bin-exp (- raw-exp 1075))
                          (dec-exp (ash (* bin-exp 315653) -20)))
                     (ldb (byte 8 0)
                          (+ bin-exp (ash (* (- (1+ dec-exp)) 217707) -16) 1 6))))))
(declaim (type (simple-array (unsigned-byte 8) (2048)) *zmij-exp-shifts*))

;;;; Types
;;;
;;; Ranges are exact for IEEE single and double: they let SBCL keep
;;; every intermediate in an unboxed register.

(deftype zmij-word () '(unsigned-byte 64))
(deftype zmij-double-raw-exp () '(integer 1 2046))
(deftype zmij-float-raw-exp () '(integer 1 254))
(deftype zmij-dec-exp () '(integer -324 292))
(deftype zmij-shift () '(integer 3 7))
(deftype zmij-digit () '(integer 0 10))
;;; Shortest significand: at most 17 digits, plus a possible round-up.
(deftype zmij-significand () '(integer 0 100000000000000010))

(declaim (inline zmij-pow10))
(defun zmij-pow10 (exp)
  (declare (type (integer -293 323) exp))
  (let ((idx (* 2 (- exp +zmij-pow10-min-exp+))))
    (values (aref *zmij-pow10* idx) (aref *zmij-pow10* (1+ idx)))))

(declaim (inline zmij-shift))
(defun zmij-shift (raw-exp)
  (declare (type (integer 0 2047) raw-exp))
  (truly-the zmij-shift (aref *zmij-exp-shifts* raw-exp)))

;;;; Fixed-width arithmetic

(declaim (inline zmij-mul-hi zmij-mul-add-hi zmij-mul192-hi128))
(defun zmij-mul-hi (x y)
  (declare (type zmij-word x y))
  (%multiply-high x y))

;;; (x*y + c) >> 64
(defun zmij-mul-add-hi (x y c)
  (declare (type zmij-word x y c))
  (multiple-value-bind (h l) (sb-bignum:%multiply x y)
    (declare (type zmij-word h l))
    (let ((l+c (logand (+ l c) #xFFFFFFFFFFFFFFFF)))
      (logand (+ h (if (< l+c l) 1 0)) #xFFFFFFFFFFFFFFFF))))

;;; High 128 bits of (x-hi<<64 | x-lo) * y
(defun zmij-mul192-hi128 (x-hi x-lo y)
  (declare (type zmij-word x-hi x-lo y))
  (multiple-value-bind (rh rl) (sb-bignum:%multiply y x-hi)
    (declare (type zmij-word rh rl))
    (let ((l (logand (+ rl (zmij-mul-hi y x-lo)) #xFFFFFFFFFFFFFFFF)))
      (values (logand (+ rh (if (< l rl) 1 0)) #xFFFFFFFFFFFFFFFF) l))))

;;;; Core
;;;
;;; Each returns (values integral dec-exp digit has-last-digit). The
;;; decimal value is integral * 10^(dec-exp+1) or, when has-last-digit,
;;; (integral * 10 + digit) * 10^dec-exp.

(declaim (inline zmij-double-irregular zmij-double-regular
                 zmij-float-irregular zmij-float-regular))

;;; Shared by the double and float irregular cases (a power-of-two
;;; significand, whose rounding interval is asymmetric).
(defmacro zmij-irregular-body (bin-sig raw-exp exp-offset integral-type)
  `(let* ((bin-exp (- ,raw-exp ,exp-offset))
          (dec-exp (zmij-dec-exp bin-exp nil))
          (shift (truly-the zmij-shift
                            (+ (zmij-exp-shift bin-exp (1+ dec-exp)) 6))))
     (multiple-value-bind (pow10-hi pow10-lo) (zmij-pow10 (- (1+ dec-exp)))
       (multiple-value-bind (p-hi p-lo)
           (zmij-mul192-hi128 pow10-hi pow10-lo (ash ,bin-sig shift))
         (let* ((integral (ash p-hi -6))
                (fractional (logior (logand (ash p-hi 58) #xFFFFFFFFFFFFFFFF)
                                    (ash p-lo -6)))
                (half-ulp (ash pow10-hi (- shift 7)))
                (round-up (> half-ulp (logxor #xFFFFFFFFFFFFFFFF fractional)))
                (round-down (> (ash half-ulp -1) fractional))
                (digit (truly-the zmij-digit
                                  (zmij-mul-add-hi fractional 10 (1- (ash 1 63)))))
                (lo (truly-the zmij-digit
                               (zmij-mul-add-hi
                                (logand (- fractional (ash half-ulp -1))
                                        #xFFFFFFFFFFFFFFFF)
                                10 #xFFFFFFFFFFFFFFFF))))
           (values (truly-the ,integral-type (+ integral (if round-up 1 0)))
                   (truly-the zmij-dec-exp dec-exp)
                   (max digit lo)
                   (not (or round-up round-down))))))))

(defun zmij-double-irregular (bin-sig raw-exp)
  (declare (type (unsigned-byte 53) bin-sig)
           (type zmij-double-raw-exp raw-exp))
  (zmij-irregular-body bin-sig raw-exp +zmij-double-exp-offset+
                       (integer 0 10000000000000000)))

(defun zmij-float-irregular (bin-sig raw-exp)
  (declare (type (unsigned-byte 24) bin-sig)
           (type zmij-float-raw-exp raw-exp))
  (zmij-irregular-body bin-sig raw-exp +zmij-float-exp-offset+
                       (integer 0 100000000)))

(defun zmij-double-regular (bin-sig raw-exp)
  (declare (type (unsigned-byte 53) bin-sig)
           (type zmij-double-raw-exp raw-exp))
  (let* ((bin-exp (- raw-exp +zmij-double-exp-offset+))
         (dec-exp (zmij-dec-exp bin-exp t))
         (shift (zmij-shift raw-exp))
         (even (- 1 (logand bin-sig 1))))
    (multiple-value-bind (pow10-hi pow10-lo) (zmij-pow10 (- (1+ dec-exp)))
      (multiple-value-bind (p-hi p-lo)
          (zmij-mul192-hi128 pow10-hi pow10-lo (ash bin-sig shift))
        (let* ((integral (ash p-hi -6))
               (fractional (logior (logand (ash p-hi 58) #xFFFFFFFFFFFFFFFF)
                                   (ash p-lo -6)))
               ;; pow10-hi >> (7 - shift), plus 1 when the interval is closed.
               (half-ulp (logand (+ (ash pow10-hi (- shift 7)) even)
                                 #xFFFFFFFFFFFFFFFF))
               (round-up (< (logand (+ fractional half-ulp) #xFFFFFFFFFFFFFFFF)
                              fractional))
               (round-down (> half-ulp fractional))
               (digit (truly-the zmij-digit
                                 (zmij-mul-add-hi fractional 10 (+ (ash 1 63) 6)))))
          (values (truly-the (integer 0 10000000000000000)
                             (+ integral (if round-up 1 0)))
                  (truly-the zmij-dec-exp dec-exp)
                  (if (= fractional (ash 1 62)) 2 digit) ; round 2.5 to 2
                  (not (or round-up round-down))))))))

(defun zmij-float-regular (bin-sig raw-exp)
  (declare (type (unsigned-byte 24) bin-sig)
           (type zmij-float-raw-exp raw-exp))
  (let* ((bin-exp (- raw-exp +zmij-float-exp-offset+))
         (dec-exp (zmij-dec-exp bin-exp t))
         ;; The double table works for floats too, with 34 bits of
         ;; fraction instead of 6.
         (shift (+ (zmij-shift (+ bin-exp +zmij-double-exp-offset+)) (- 34 6)))
         (even (- 1 (logand bin-sig 1)))
         (pow10-hi (values (zmij-pow10 (- (1+ dec-exp)))))
         (p (zmij-mul-hi (logand (1+ pow10-hi) #xFFFFFFFFFFFFFFFF)
                         (ash bin-sig shift)))
         (integral (ash p -34))
         (fractional (ldb (byte 34 0) p))
         (half-ulp (+ (ash pow10-hi (- shift 65)) even))
         (round-up (>= (+ fractional half-ulp) (ash 1 34)))
         (round-down (> half-ulp fractional))
         (digit (ash (+ (* fractional 10) (ash 1 33)) -34)))
    (values (truly-the (integer 0 100000000) (+ integral (if round-up 1 0)))
            (truly-the zmij-dec-exp dec-exp)
            (if (= fractional (ash 1 32)) 2 (truly-the zmij-digit digit))
            (not (or round-up round-down)))))

(declaim (inline zmij-decimal))
;;; Return (values M E) such that M * 10^E is the shortest decimal that
;;; reads back as the absolute value of FLOAT. M may have trailing
;;; zeros. FLOAT must be finite and nonzero.
(defun zmij-decimal (float)
  (declare (type (or single-float double-float) float))
  (multiple-value-bind (integral dec-exp digit has-last-digit)
      (etypecase float
        (double-float
         (let* ((bits (double-float-bits float))
                (frac (ldb (byte 52 0) bits))
                (biased (ldb (byte 11 52) bits)))
           (cond ((zerop biased)
                  (zmij-double-regular frac 1))
                 ((zerop frac)
                  (zmij-double-irregular (ash 1 52) biased))
                 (t
                  (zmij-double-regular (logior frac (ash 1 52)) biased)))))
        (single-float
         (let* ((bits (single-float-bits float))
                (frac (ldb (byte 23 0) bits))
                (biased (ldb (byte 8 23) bits)))
           (cond ((zerop biased)
                  (zmij-float-regular frac 1))
                 ((zerop frac)
                  (zmij-float-irregular (ash 1 23) biased))
                 (t
                  (zmij-float-regular (logior frac (ash 1 23)) biased))))))
    (if has-last-digit
        (values (truly-the zmij-significand (+ (* integral 10) digit)) dec-exp)
        (values integral (1+ dec-exp)))))

;;;; Digit output

;;; Convert X < 10^8 to eight BCD digits, one per byte, most significant
;;; digit in the most significant byte. Base 10000 -> 100 -> 10, each
;;; step computing quotient and remainder of all lanes at once:
;;;   (x / 10000) << 32 + (x % 10000) == x + (2^32 - 10000) * (x / 10000)
;;; where the division is a multiply and shift.
(declaim (inline zmij-bcd8))
(defun zmij-bcd8 (x)
  (declare (type (integer 0 99999999) x))
  (let* ((abcd-efgh (+ x (* (- (ash 1 32) 10000)
                            (ash (* x (1+ (floor (ash 1 40) 10000))) -40))))
         (ab-cd-ef-gh (+ abcd-efgh
                         (* (- (ash 1 16) 100)
                            (logand (ash (* abcd-efgh (1+ (floor (ash 1 19) 100)))
                                         -19)
                                    #x7f0000007f))))
         (a-b-c-d-e-f-g-h (+ ab-cd-ef-gh
                             (* (- (ash 1 8) 10)
                                (logand (ash (* ab-cd-ef-gh (1+ (floor (ash 1 10) 10)))
                                             -10)
                                        #xf000f000f000f)))))
    (logand a-b-c-d-e-f-g-h #xFFFFFFFFFFFFFFFF)))

;;; Portable version of the %ZMIJ-STORE-DIGITS VOP (see the x86-64
;;; backend for the SSE2 one).
(sb-c::unless-vop-existsp (:translate %zmij-store-digits)
  (defun %zmij-store-digits (string index hi lo)
    (declare (type simple-base-string string)
             (type index index)
             (type (integer 0 99999999) hi lo))
    (let ((mask 0))
      (declare (type (unsigned-byte 16) mask))
      (flet ((store (bcd offset)
               (declare (type (unsigned-byte 64) bcd)
                        (type (integer 0 8) offset))
               (dotimes (j 8)
                 (let ((digit (ldb (byte 4 (* 8 (- 7 j))) bcd)))
                   (setf (schar string (+ index offset j))
                         (code-char (+ (char-code #\0) digit)))
                   (unless (zerop digit)
                     (setf mask (logior mask (ash 1 (+ offset j)))))))))
        (store (zmij-bcd8 hi) 0)
        (store (zmij-bcd8 lo) 8))
      mask)))

;;; Length of a ZMIJ-DIGIT-STRING buffer.
(defconstant +zmij-buffer-length+ 18)

;;; Write the shortest decimal digits of FLOAT into BUFFER, a base
;;; string of +ZMIJ-BUFFER-LENGTH+ characters. Return (values START END
;;; K): the digits are (subseq BUFFER START END), without leading or
;;; trailing zeros, and K is the position of the decimal point (value =
;;; 0.d1d2... * 10^K).
;;;
;;; The significand (at most 18 digits) is TOP * 10^16 + HI * 10^8 + LO.
;;; TOP takes positions 0-1, HI and LO positions 2-17.
(declaim (inline zmij-digit-string))
(defun zmij-digit-string (buffer float)
  (declare (type (simple-base-string 18) buffer)
           (type (or single-float double-float) float))
  (multiple-value-bind (m e) (zmij-decimal float)
    (multiple-value-bind (top rest) (truncate m (expt 10 16))
      (multiple-value-bind (hi lo) (truncate rest (expt 10 8))
        (setf (schar buffer 0) (code-char (+ (char-code #\0) (truncate top 10)))
              (schar buffer 1) (code-char (+ (char-code #\0) (rem top 10))))
        (let* ((nonzero (%zmij-store-digits buffer 2 hi lo))
               (start (cond ((>= top 10) 0)
                            ((plusp top) 1)
                            (t (+ 2 (1- (integer-length
                                         (logand nonzero (- nonzero))))))))
               (end (cond ((plusp nonzero) (+ 2 (integer-length nonzero)))
                          ((= top 10) 1)
                          (t 2))))
          (declare (type (integer 0 17) start)
                   (type (integer 1 18) end))
          (values start end (+ e (- 18 start))))))))

(defmacro with-zmij-digit-string ((buffer start end k) float &body body)
  `(let ((,buffer (make-string +zmij-buffer-length+ :element-type 'base-char)))
     (declare (dynamic-extent ,buffer))
     (multiple-value-bind (,start ,end ,k) (zmij-digit-string ,buffer ,float)
       ,@body)))

;;; Call CHAR-FUN with each decimal digit of FLOAT, bracketed by
;;; PROLOGUE-FUN and EPILOGUE-FUN called with K, the position of the
;;; decimal point (value = 0.d1d2... * 10^K). Returns what EPILOGUE-FUN
;;; returns.
(declaim (inline zmij-digits))
(defun zmij-digits (char-fun prologue-fun epilogue-fun float)
  (declare (type function char-fun prologue-fun epilogue-fun)
           (type (or single-float double-float) float))
  (with-zmij-digit-string (buffer start end k) float
    (funcall prologue-fun k)
    (loop for i from start below end
          do (funcall char-fun (- (char-code (schar buffer i)) (char-code #\0))))
    (funcall epilogue-fun k)))
