;;;; Encoding and decoding support for VEX- and EVEX-encoded instructions.
;;;;
;;;; This file contains the vector runtime infrastructure:
;;;; - Disassembler argument types (define-arg-type) and operand decoders
;;;; - VEX and EVEX instruction format definitions (VEX2, VEX3, EVEX)
;;;; - Instruction emitters for VEX (emit-avx2-inst) and EVEX (emit-avx512-inst)
;;;; - Register printing utilities
;;;; - VEX-encoded general-purpose register instructions (BMI1, BMI2, ADX)
;;;;
;;;; All vector computational and data movement instruction definitions
;;;; are located in simd-insts.lisp (generated from LLVM TableGen).

(in-package "SB-X86-64-ASM")


;;; Declarations

;;; General indicator that we are decoding an EVEX instruction.
;;; EVEX has 128-, 256-, and 512-bit forms; L'L=00 is 128-bit and sets
;;; neither +vex-l+ nor +evex-l1+, so this separate bit is required for
;;; code that needs to know "EVEX vs VEX/legacy".
;;; (Bit 14 is currently unused by other dstate properties.)
(defconstant +evex+ #x4000)
(defconstant +vex-l+ #x400)
;; EVEX L'L=10 (512-bit) sets bit 11; L'L=01 (256-bit) sets bit 10 (=+vex-l+)
(defconstant +evex-l1+ #x800)
;; EVEX R' bit (reg bit 4, for registers 16-31 in ModR/M.reg)
(defconstant +evex-r-prime+ #x1000)
;; EVEX X bit as B' (r/m bit 4, for registers 16-31 in ModR/M.r/m, reg-direct only)
(defconstant +evex-b-prime+ #x2000)
;;; EVEX V' bit (vvvv bit 4, for registers 16-31 in the vvvv field).
;;; The 4-bit vvvv field in the EVEX prefix is extended with this bit.
;;; (Bit 15 is currently unused by other dstate properties.)
(defconstant +evex-v-prime+ #x8000)

(defconstant-eqx +avx-conditions+
    #(:eq :lt :le :unord :neq :nlt :nle :ord :eq_uq
      :nge :ngt :false :neq_oq :ge :gt :true :eq_os :lt_oq
      :le_oq :unord_s :neq_us :nlt_uq :nle_uq :ord_s :eq_us
      :nge_uq :ngt_uq :false_os :neq_os :ge_oq :gt_oq :true_us)
  #'equalp)

;;; VSIB support

;;; Mark that the instruction uses vector-index SIB addressing.
(defconstant +vsib+ #x10000)   ; choose an unused dstate property bit

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (t t t) *) print-vsib/mem)))


;; disassembler argument types

(define-arg-type k-vvvv-reg
  :prefilter #'invert-4
  :printer #'print-kreg)

(define-arg-type ymmreg
  :prefilter #'prefilter-reg-r
  :printer #'print-ymmreg)

(define-arg-type ymmreg-b
  :prefilter #'prefilter-reg-b
  :printer #'print-ymmreg)

(define-arg-type ymm-vvvv-reg
  :prefilter #'invert-4
  :printer #'print-ymmreg)

(define-arg-type vvvv-reg
  :prefilter #'invert-4
  :printer #'print-reg)

(define-arg-type ymm-reg-is4
  :printer #'print-ymmreg)

(define-arg-type ymmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-ymmreg/mem)

(define-arg-type half-ymmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-half-ymmreg/mem)

(macrolet ((define-disp-arg-type (name n &optional (printer '#'print-ymmreg/mem))
             `(define-arg-type ,name
                :prefilter (lambda (dstate mod r/m)
                             (decode-mod-r/m dstate mod r/m 'fpr :disp-n ,n))
                :printer ,printer)))
  (define-disp-arg-type evex-ymmreg/mem-disp1  1)
  (define-disp-arg-type evex-ymmreg/mem-disp2  2)
  (define-disp-arg-type evex-ymmreg/mem-disp4  4)
  (define-disp-arg-type evex-ymmreg/mem-disp8  8)
  (define-disp-arg-type evex-ymmreg/mem-disp16 16)
  (define-disp-arg-type evex-ymmreg/mem-disp32 32)
  (define-disp-arg-type evex-ymmreg/mem-disp64 64)

  ;; Narrower source operand of widening moves (VPMOVSX/ZX)
  (define-disp-arg-type evex-half-ymmreg/mem-disp8  8  #'print-half-ymmreg/mem)
  (define-disp-arg-type evex-half-ymmreg/mem-disp16 16 #'print-half-ymmreg/mem)
  (define-disp-arg-type evex-half-ymmreg/mem-disp32 32 #'print-half-ymmreg/mem)

  (define-disp-arg-type evex-xmmreg/mem-disp2  2  #'print-xmmreg/mem)
  (define-disp-arg-type evex-xmmreg/mem-disp4  4  #'print-xmmreg/mem)
  (define-disp-arg-type evex-xmmreg/mem-disp8  8  #'print-xmmreg/mem)
  (define-disp-arg-type evex-xmmreg/mem-disp16 16 #'print-xmmreg/mem))

(define-arg-type vm
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-ymmreg/mem)

(define-arg-type vmx/y
  :prefilter #'prefilter-reg/mem
  :printer #'print-vmx/y)

(define-arg-type vmx
  :prefilter #'prefilter-reg/mem
  :printer #'print-vmx)

(define-arg-type sized-xmmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-sized-xmmreg/mem)

(define-arg-type sized-byte-xmmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-sized-byte-xmmreg/mem)

(define-arg-type sized-word-xmmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-sized-word-xmmreg/mem)

(define-arg-type sized-dword-xmmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-sized-dword-xmmreg/mem)

(define-arg-type sized-xmmreg/mem-default-qword
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-sized-xmmreg/mem-default-qword)

(define-arg-type evex-vsib-disp4
  :prefilter (lambda (dstate mod r/m)
               (dstate-setprop dstate +vsib+)
               (decode-mod-r/m dstate mod r/m 'fpr :disp-n 4))
  :printer #'print-vsib/mem)

(define-arg-type evex-vsib-disp8
  :prefilter (lambda (dstate mod r/m)
               (dstate-setprop dstate +vsib+)
               (decode-mod-r/m dstate mod r/m 'fpr :disp-n 8))
  :printer #'print-vsib/mem)

(define-arg-type vex-l
  :prefilter  (lambda (dstate value)
                (dstate-setprop dstate (if (plusp value) +vex-l+ 0))))

(define-arg-type vex-w
  :prefilter  (lambda (dstate value)
                (dstate-setprop dstate (if (plusp value) +rex-w+ 0))))

(define-arg-type vex-r
  :prefilter  (lambda (dstate value)
                (dstate-setprop dstate (if (plusp value) 0 +rex-r+))))

(define-arg-type vex-x
  :prefilter  (lambda (dstate value)
                (dstate-setprop dstate (if (plusp value) 0 +rex-x+))))

(define-arg-type vex-b
  :prefilter  (lambda (dstate value)
                (dstate-setprop dstate (if (plusp value) 0 +rex-b+))))

(define-arg-type evex-ymmreg-b
  :prefilter (lambda (dstate value)
               (let ((full-reg (if (dstate-getprop dstate +rex-b+) (+ value 8) value)))
                 (get-fpr :xmm
                          (if (and (dstate-getprop dstate +evex+)
                                   (dstate-getprop dstate +rex-x+))
                              (+ full-reg 16)
                              full-reg))))
  :printer #'print-ymmreg-rm)

(define-arg-type avx-condition-code
  :type 'imm-byte
  :printer +avx-conditions+)

;;; EVEX arg-types

;; EVEX R' extends ModR/M.reg bit 4 (inverted in prefix)
;; R'=0 in prefix means bit4=1 (register 16-31)
(define-arg-type evex-r-prime
  :prefilter (lambda (dstate value)
               (dstate-setprop dstate (if (zerop value) +evex-r-prime+ 0))))

;;; Marks that the instruction being decoded uses an EVEX prefix.
;;; This is set by the EVEX fixed-bit field and is independent of L'L.
(define-arg-type evex-fixed
  :prefilter (lambda (dstate value)
               (declare (ignore value))
               (dstate-setprop dstate +evex+)))

;; EVEX V' extends vvvv bit 4 (inverted in prefix)
;; V'=0 means bit4=1 (register 16-31 in vvvv)
;; Note: the printer for vvvv (print-ymmreg via ymm-vvvv-reg) gets
;; a 4-bit value from the invert-4 prefilter. V' provides the 5th bit.
(define-arg-type evex-v-prime
  :prefilter (lambda (dstate value)
               (dstate-setprop dstate
                               (if (zerop value) +evex-v-prime+ 0))))

;; EVEX L'L: 2-bit vector length (00=128, 01=256, 10=512)
;; Stores into dstate bits 10-11: L'L=01 sets bit 10 (+vex-l+),
;; L'L=10 sets bit 11 (+evex-l1+), L'L=11 sets both.
(define-arg-type evex-ll
  :prefilter (lambda (dstate value)
               (dstate-setprop dstate (ash value 10))))

;; EVEX W: same semantics as VEX.W
(define-arg-type evex-w
  :prefilter (lambda (dstate value)
               (dstate-setprop dstate (if (plusp value) +rex-w+ 0))))

;;; EVEX vvvv register operand.
;;; The 4-bit value is inverted and extended with V' by the printer.
(define-arg-type evex-ymm-vvvv-reg
  :prefilter #'invert-4
  :printer #'print-ymmreg-vvvv)

;; Opmask register k0-k7
(define-arg-type opmask-reg
  :printer #'print-opmask-register)

;; EVEX {k} / {z} decoration, printed only when actually present. These
;; render nothing for aaa = 0 and z = 0, so ONE printer can cover both
;; the masked and unmasked encodings of an instruction rather than
;; needing a separate constant-matched printer per mask register (which
;; is what the hand-written VCMP below has to do with OPMASK-REG).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (t t t) *)
                  print-opmask-decoration print-evex-zeroing)))

(define-arg-type opmask-decoration
  :printer #'print-opmask-decoration)

(define-arg-type evex-zeroing
  :printer #'print-evex-zeroing)

;;; K register in a ModR/M register field.
(define-arg-type kreg
  :prefilter (lambda (dstate value)
               (declare (ignore dstate))
               (get-fpr :kreg value))
  :printer #'print-kreg)

;;; K register or memory in a ModR/M r/m field.
;;; This is only valid for instructions whose r/m operand can be K or memory.
(define-arg-type kreg/mem
  :prefilter (lambda (dstate mod r/m)
               (if (= mod #b11)
                   (get-fpr :kreg r/m)
                   (decode-mod-r/m dstate mod r/m 'gpr)))
  :printer #'print-kreg/mem)

;;; Instruction formats

(define-instruction-format (vex2 16)
                           (vex :field (byte 8 0) :value #xC5)
                           (r :field (byte 1 (+ 8 7)) :type 'vex-r)
                           (vvvv :field (byte 4 (+ 8 3)) :type 'ymm-vvvv-reg)
                           (l  :field (byte 1 (+ 8 2)) :type 'vex-l)
                           (pp :field (byte 2 (+ 8 0))))

(define-instruction-format (vex3 24)
                           (vex :field (byte 8 0) :value #xC4)
                           (r :field (byte 1 (+ 8 7)) :type 'vex-r)
                           (x :field (byte 1 (+ 8 6)) :type 'vex-x)
                           (b :field (byte 1 (+ 8 5)) :type 'vex-b)
                           (m-mmmm :field (byte 5 (+ 8 0)))
                           (w :field (byte 1 (+ 16 7)) :type 'vex-w)
                           (vvvv :field (byte 4 (+ 16 3)) :type 'ymm-vvvv-reg)
                           (l  :field (byte 1 (+ 16 2)) :type 'vex-l)
                           (pp :field (byte 2 (+ 16 0))))

(define-instruction-format (vex2-op 24
                            :include vex2
                            :default-printer '(:name))
  (op :field (byte 8 16)))

(defmacro define-vex-instruction-format ((format-name length-in-bits
                                          &key default-printer include)
                                         &body arg-specs)
  `(progn
     (define-instruction-format (,(symbolicate "VEX2-" format-name) (+ 16 ,length-in-bits)
                                  :include ,(if include
                                                (symbolicate "VEX2-" include)
                                                'vex2)
                                  :default-printer ,default-printer)
         ,@(subst 16 'start arg-specs))
     (define-instruction-format (,(symbolicate "VEX3-" format-name) (+ 24 ,length-in-bits)
                                  :include ,(if include
                                                (symbolicate "VEX3-" include)
                                                'vex3)
                                  :default-printer ,default-printer)
         ,@(subst 24 'start arg-specs))))

(define-vex-instruction-format (ymm-ymm/mem 16
                                :default-printer '(:name :tab reg ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'ymmreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'ymmreg)
  ;; optional fields
  (imm))

(define-vex-instruction-format (ymm-ymm/mem-imm 16
                                :default-printer '(:name :tab reg ", " vvvv ", " reg/mem ", " imm))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'ymmreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'ymmreg)
  (imm     :type 'imm-byte))

(define-vex-instruction-format (ymm-ymm/mem-ymm 24
                                :default-printer '(:name :tab reg  ", " vvvv ", " reg/mem ", " reg4))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'ymmreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'ymmreg)
  (reg4    :field (byte 4 (+ start 16 4))
           :type 'ymm-reg-is4))

;;; Same as ymm-ymm/mem etc., but with a direction bit.
(define-vex-instruction-format (ymm-ymm/mem-dir 16
                                :include ymm-ymm/mem
                                :default-printer
                                `(:name
                                  :tab
                                  (:if (reg/mem :test machine-ea-p)
                                       (:if (dir :constant 0)
                                            (reg ", " reg/mem)
                                            (reg/mem ", " reg))
                                       (reg ", " vvvv ", " reg/mem))))
    (op  :field (byte 7 (+ start 1)))
  (dir :field (byte 1 (+ start 0))))

(define-vex-instruction-format (ymm-ymm-imm 16
                                :default-printer '(:name :tab vvvv ", " reg ", " imm))
  (op  :field (byte 8 (+ start 0)))
  (/i  :field (byte 3 (+ start 11)))
  (b11 :field (byte 2 (+ start 14))
       :value #b11)
  (reg :field (byte 3 (+ start 8))
       :type 'ymmreg-b)
  (imm :type 'imm-byte))

(define-vex-instruction-format (reg-ymm/mem 16
                                :include ymm-ymm/mem
                                :default-printer '(:name :tab reg ", " reg/mem))
  (reg :field (byte 3 (+ start 11))
       :type 'reg))

(define-vex-instruction-format (kreg-kreg/mem 16
                                :default-printer '(:name :tab reg ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'kreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'kreg))

(define-vex-instruction-format (kreg-reg/mem 16
                                :default-printer '(:name :tab reg ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'reg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'kreg))

(define-vex-instruction-format (reg-kreg/mem 16
                                :default-printer '(:name :tab reg ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'kreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'reg))

(define-vex-instruction-format (kreg-kreg/mem-k 16
                                :default-printer '(:name :tab reg ", " vvvv ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'kreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'kreg)
  (vvvv    :type 'k-vvvv-reg))

(define-vex-instruction-format (kreg-kreg/mem-imm 16
                                :default-printer '(:name :tab reg ", " reg/mem ", " imm))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'kreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'kreg)
  (imm     :type 'imm-byte))

;;; EVEX instruction formats for disassembly
;;; EVEX prefix is 4 bytes (32 bits):
;;; Byte 0: #x62
;;; Byte 1: R(7) X(6) B(5) R'(4) 00(3:2) mm(1:0)
;;; Byte 2: W(7) vvvv(6:3) 1(2) pp(1:0)
;;; Byte 3: z(7) L'(6) L(5) b(4) V'(3) aaa(2:0)

(define-instruction-format (evex 32)
  (evex-prefix :field (byte 8 0) :value #x62)
  ;; Byte 1
  (r        :field (byte 1 15) :type 'vex-r)
  (x        :field (byte 1 14) :type 'vex-x)
  (b        :field (byte 1 13) :type 'vex-b)
  (r-prime  :field (byte 1 12) :type 'evex-r-prime)
  (reserved :field (byte 1 11) :value 0) ; bit 11 reserved (0 in EVEX)
  (mm       :field (byte 3 8))          ; bits 10:8 are mmm (maps 1, 2, 3, 5, 6)
  ;; Byte 2
  (w          :field (byte 1 23) :type 'evex-w)
  (vvvv       :field (byte 4 19) :type 'evex-ymm-vvvv-reg)
  (evex-fixed :field (byte 1 18) :value 1 :type 'evex-fixed) ; must be 1 for EVEX
  (pp         :field (byte 2 16))
  ;; Byte 3
  (z-bit   :field (byte 1 31) :type 'evex-zeroing)
  (ll      :field (byte 2 29) :type 'evex-ll)
  (evex-b  :field (byte 1 28))
  (v-prime :field (byte 1 27) :type 'evex-v-prime)
  (aaa     :field (byte 3 24) :type 'opmask-decoration))

(defmacro define-evex-instruction-format ((format-name length-in-bits
                                           &key default-printer include)
                                          &body arg-specs)
  ;; Append the {k}/{z} decoration to every EVEX format's default
  ;; printer. AAA and Z-BIT render nothing when zero, so this is
  ;; correct for unmasked encodings too, and it means an instruction
  ;; whose generated :PRINTER supplies no explicit expression still
  ;; shows its mask.
  (let ((default-printer
          (if (and (consp default-printer) (eq (car default-printer) 'quote))
              `',(append (second default-printer) '(aaa z-bit))
              default-printer)))
    `(define-instruction-format (,(symbolicate "EVEX-" format-name) (+ 32 ,length-in-bits)
                                 :include ,(if include
                                               (symbolicate "EVEX-" include)
                                               'evex)
                                 :default-printer ,default-printer)
       ,@(subst 32 'start arg-specs))))

;;; EVEX Mask Register Formats (Exclusively EVEX - No VEX macro)
(define-evex-instruction-format (kreg-kreg/mem 16
                                :default-printer '(:name :tab reg ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'kreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'kreg))

(define-evex-instruction-format (kreg-reg/mem 16
                                :default-printer '(:name :tab reg ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'reg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'kreg))

(define-evex-instruction-format (reg-kreg/mem 16
                                :default-printer '(:name :tab reg ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'kreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'reg))

(define-evex-instruction-format (kreg-kreg/mem-k 16
                                :default-printer '(:name :tab reg ", " vvvv ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'kreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'kreg)
  (vvvv    :type 'k-vvvv-reg))

(define-evex-instruction-format (kreg-kreg/mem-imm 16
                                :default-printer '(:name :tab reg ", " reg/mem ", " imm))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'kreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'kreg)
  (imm     :type 'imm-byte))

(define-evex-instruction-format (kreg-reg/mem-imm 16
                                :default-printer '(:name :tab reg ", " reg/mem ", " imm))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'reg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'kreg)
  (imm     :type 'imm-byte))

(define-evex-instruction-format (vsib-disp4 16
                                 :default-printer '(:name :tab reg ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'evex-vsib-disp4)
  (reg     :field (byte 3 (+ start 11))
           :type 'ymmreg))

(define-evex-instruction-format (vsib-disp8 16
                                 :default-printer '(:name :tab reg ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'evex-vsib-disp8)
  (reg     :field (byte 3 (+ start 11))
           :type 'ymmreg))

(define-evex-instruction-format (vsib-prefetch-disp4 16
                                 :default-printer '(:name :tab reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'evex-vsib-disp4)
  (/i      :field (byte 3 (+ start 11))))

(define-evex-instruction-format (vsib-prefetch-disp8 16
                                 :default-printer '(:name :tab reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'evex-vsib-disp8)
  (/i      :field (byte 3 (+ start 11))))


(define-evex-instruction-format (ymm-ymm/mem 16
                                 :default-printer '(:name :tab reg ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'ymmreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'ymmreg)
  ;; optional fields
  (imm))

(define-evex-instruction-format (ymm-ymm/mem-imm 16
                                 :default-printer '(:name :tab reg ", " vvvv ", " reg/mem ", " imm))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'ymmreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'ymmreg)
  (imm     :type 'imm-byte))

(define-evex-instruction-format (ymm-ymm/mem-dir 16
                                 :include ymm-ymm/mem
                                 :default-printer
                                 `(:name
                                   :tab
                                   (:if (reg/mem :test machine-ea-p)
                                        (:if (dir :constant 0)
                                             (reg ", " reg/mem)
                                             (reg/mem ", " reg))
                                        (reg ", " vvvv ", " reg/mem))))
    (op  :field (byte 7 (+ start 1)))
  (dir :field (byte 1 (+ start 0))))

(define-evex-instruction-format (ymm-ymm-imm 16
                                 :default-printer '(:name :tab vvvv ", " reg ", " imm))
  (op  :field (byte 8 (+ start 0)))
  (/i  :field (byte 3 (+ start 11)))
  (b11 :field (byte 2 (+ start 14))
       :value #b11)
  (reg :field (byte 3 (+ start 8))
       :type 'evex-ymmreg-b)
  (imm :type 'imm-byte))

(define-evex-instruction-format (reg-ymm/mem 16
                                 :include ymm-ymm/mem
                                 :default-printer '(:name :tab reg ", " reg/mem))
  (reg :field (byte 3 (+ start 11))
       :type 'reg))

;;; Stub EVEX formats for VEX-only instruction format stems.
(define-evex-instruction-format (ymm-ymm/mem-ymm 24
                                 :default-printer '(:name :tab reg  ", " vvvv ", " reg/mem ", " reg4))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'ymmreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'ymmreg)
  (reg4    :field (byte 4 (+ start 16 4))
           :type 'ymm-reg-is4))

(define-instruction-format (evex-vex-gpr (+ 32 16)
                                         :include evex
                                         :default-printer '(:name :tab reg ", " vvvv ", " reg/mem))
  (op      :field (byte 8 (+ 32 0)))
  (vvvv    :type 'vvvv-reg)
  (reg/mem :fields (list (byte 2 (+ 32 14)) (byte 3 (+ 32 8)))
           :type 'reg/mem)
  (reg     :field (byte 3 (+ 32 11))
           :type 'reg))


(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun vex-encode-pp (pp)
    (ecase pp
      ((nil) 0)
      (#x66 #b01)
      (#xF3 #b10)
      (#xF2 #b11)))

  (defun vex-encode-m-mmmm (m-mmmm)
    (ecase m-mmmm
      (#x0F #b00001)
      (#x0F38 #b00010)
      (#x0F3A #b00011)))

  (defun evex-encode-mm (m-mmmm)
    (ecase m-mmmm
      ((#x0F 1)   #b001)
      ((#x0F38 2) #b010)
      ((#x0F3A 3) #b011)
      ((:map5 5 9) #b101)
      ((:map6 6 10) #b110))))

(defun emit-two-byte-vex (segment r vvvv l pp)
  (emit-bytes segment
              #xC5
              (logior (ash (logxor 1 r) 7)
                      (ash (logxor vvvv #b1111)
                           3)
                      (ash L 2)
                      (vex-encode-pp pp))))

(defun emit-three-byte-vex (segment r x b m-mmmm w vvvv l pp)
  (emit-bytes segment
              #xC4
              (logior (ash (logxor 1 r) 7)
                      (ash (logxor 1 x) 6)
                      (ash (logxor 1 b) 5)
                      (vex-encode-m-mmmm m-mmmm))
              (logior (ash w 7)
                      (ash (logxor vvvv #b1111) 3)
                      (ash L 2)
                      (vex-encode-pp pp))))

(defun determine-vex-flags (thing reg l)
  (flet ((reg-7-p (reg-id)
           (if (<= (reg-id-num reg-id) 7)
               0
               1))
         (xmm-size (r)
           (cond ((is-ymm-id-p (reg-id r))
                  1)
                 ((xmm-register-p r)
                  0))))
    (let ((l (cond ((eq l :from-thing)
                    (xmm-size thing))
                   (l)
                   ((xmm-register-p reg)
                    (xmm-size reg))
                   ((xmm-register-p thing)
                    (xmm-size thing))
                   (t
                    0)))
          (r (if (or (null reg)
                     (integerp reg))
                 0
                 (reg-7-p (reg-id reg))))
          (x (cond ((and (ea-p thing)
                         (ea-index thing))
                    (let ((index (ea-index thing)))
                      (cond ((gpr-p index)
                             (reg-7-p (reg-id (tn-reg index))))
                            ((<= (tn-offset index) 7)
                             0)
                            (t
                             1))))
                   (t 0)))
          (b
            (reg-7-p
             (cond ((ea-p thing)
                    (let ((base (ea-base thing)))
                      (if (and base (neq base rip-tn))
                          (reg-id (tn-reg base))
                          0)))
                   ((register-p thing)
                    (reg-id thing))
                   (t 0)))))
      (values l r x b))))

(defun emit-vex (segment vvvv thing reg prefix opcode-prefix l w)
  (multiple-value-bind (l r x b) (determine-vex-flags thing reg l)
    (let ((vvvv (if vvvv
                    (reg-id-num (reg-id vvvv))
                    0)))
      (if (and (= 0 x w b)
               (= opcode-prefix #x0F))
          (emit-two-byte-vex segment r vvvv l prefix)
          (emit-three-byte-vex segment r x b opcode-prefix
                               w vvvv l prefix)))))

;;; EVEX prefix encoding for AVX-512
;;; EVEX is a 4-byte prefix: 62h | P1 | P2 | P3
;;; P1: R(7) X(6) B(5) R'(4) 0(3) mmm(2:0)
;;; P2: W(7) vvvv(6:3) 1(2) pp(1:0)
;;; P3: z(7) L'(6) L(5) b(4) V'(3) aaa(2:0)
;;; R, X, B, R', V' are inverted. vvvv is inverted.

(defun emit-evex (segment r x b r-prime opcode-prefix w vvvv pp z ll evex-b v-prime aaa)
  (emit-bytes segment
              #x62
              ;; P1: R X B R' 0 mmm
              (logior (ash (logxor 1 r) 7)
                      (ash (logxor 1 x) 6)
                      (ash (logxor 1 b) 5)
                      (ash (logxor 1 r-prime) 4)
                      ;; bit 3 is reserved (0), bits 2:0 are mmm (maps 1, 2, 3, 5, 6)
                      (evex-encode-mm opcode-prefix))
              ;; P2: W vvvv 1 pp
              (logior (ash w 7)
                      (ash (logandc1 vvvv #b1111) 3)
                      #b100  ; bit 2 always 1
                      (vex-encode-pp pp))
              ;; P3: z L'L b V' aaa
              (logior (ash z 7)
                      (ash ll 5)
                      (ash evex-b 4)
                      (ash (logxor 1 v-prime) 3)
                      aaa)))

;; Extract EVEX prefix flags from operands.
;; Returns: ll, r, x, b, r-prime, v-prime.
;; EVEX uses independent bit3 (R/B) and bit4 (R'/X) for 32-register encoding.
(defun determine-evex-flags (thing reg ll vvvv)
  (flet ((reg-bit3 (reg-id)
           ;; Extract bit 3 of 5-bit register number (for R, B in EVEX)
           (if (logbitp 3 (reg-id-num reg-id)) 1 0))
         (reg-bit4 (reg-id)
           ;; Extract bit 4 of 5-bit register number (for R', V', X in EVEX)
           (if (logbitp 4 (reg-id-num reg-id)) 1 0))
         (fpr-size (r)
           (cond ((is-zmm-id-p (reg-id r)) #b10)
                 ((is-ymm-id-p (reg-id r)) #b01)
                 ((xmm-register-p r)       #b00))))
    (let ((ll (cond (ll ll)
                    ;; EVEX.L'L is the width of the *vector operation*, which
                    ;; is the WIDEST vector operand - not whichever operand we
                    ;; happen to look at first. Mixed-width instructions are
                    ;; the whole point: VEXTRACTF32X4's ModRM.reg operand is
                    ;; the narrow XMM destination, so preferring REG encoded
                    ;; L'L=00, a length that does not exist for that opcode
                    ;; (#UD/SIGILL). Taking the maximum is also right for
                    ;; VINSERT* (wide destination), VBROADCAST*, VPMOVZX/SX
                    ;; and VPMOVWB, and for narrowing and widening converts
                    ;; alike (VCVTPD2PS, VCVTPS2PD), since in every case the
                    ;; documented length equals the wider of the two.
                    ;;
                    ;; Skip k-registers - they pass xmm-register-p but
                    ;; aren't XMM/YMM/ZMM, so fpr-size returns wrong value
                    (t
                     (let ((widest nil))
                       (dolist (r (list reg thing vvvv) (or widest #b00))
                         (when (and r (register-p r) (xmm-register-p r)
                                    (not (is-kreg-id-p (reg-id r))))
                           (let ((size (fpr-size r)))
                             (when (and size (or (null widest) (> size widest)))
                               (setf widest size)))))))))
          ;; R from reg (ModR/M reg field) - bit 3. REG may be a literal
          ;; 0-7 fixed opcode-extension digit instead of a real register
          ;; (e.g. PSRLDQ's /3) - same convention DETERMINE-VEX-FLAGS
          ;; already uses for its own REG argument.
          (r (if (or (null reg) (integerp reg)) 0 (reg-bit3 (reg-id reg))))
          ;; R' from reg - bit 4
          (r-prime (if (or (null reg) (integerp reg) (k-register-p reg)) 0 (reg-bit4 (reg-id reg))))
          ;; X from EA index, or bit 4 of r/m reg for register-direct
          ;; In EVEX, X doubles as B' (bit 4 of r/m) when mod=11 (reg-direct)
          (x (cond ((and (ea-p thing)
                         (ea-index thing))
                    (let ((index (ea-index thing)))
                      (cond ((gpr-p index)
                             (reg-bit3 (reg-id (tn-reg index))))
                            ((tn-p index)
                             (if (logbitp 3 (tn-offset index)) 1 0))
                            ((register-p index)
                             (if (logbitp 3 (reg-id-num (reg-id index))) 1 0))
                            (t 0))))
                   ((register-p thing)
                    (reg-bit4 (reg-id thing)))
                   (t 0)))
          ;; B from thing (ModR/M r/m field) - bit 3
          (b (reg-bit3
              (cond ((ea-p thing)
                     (let ((base (ea-base thing)))
                       (if (and base (neq base rip-tn))
                           (reg-id (tn-reg base))
                           0)))
                    ((register-p thing)
                     (reg-id thing))
                    (t 0))))
          ;; V' from vvvv - bit 4 of vvvv register number.
          ;; For VSIB (vector index in EA), V' encodes bit 4 of the vector index register.
          (v-prime (cond (vvvv (reg-bit4 (reg-id vvvv)))
                         ((and (ea-p thing)
                               (ea-index thing)
                               (not (gpr-p (ea-index thing))))
                          (let ((index (ea-index thing)))
                            (cond ((tn-p index)
                                   (if (logbitp 4 (tn-offset index)) 1 0))
                                  ((register-p index)
                                   (if (logbitp 4 (reg-id-num (reg-id index))) 1 0))
                                  (t 0))))
                         (t 0))))
      (values ll r x b r-prime v-prime))))

(defun emit-avx512-inst (segment thing reg prefix opcode
                         &key (remaining-bytes 0)
                              ll
                              map
                              (opcode-prefix #x0F)
                              (w 0)
                              vvvv
                              (aaa 0)
                              (z 0)
                              (evex-b 0)
                              vm
                              (disp-n 0))
  "Emit an EVEX-encoded instruction.
DISP-N is the compressed displacement scale factor (Intel tuple N).
Common values: 64 for full 512-bit loads, 32 for 256-bit or half-vector,
16 for 128-bit or quarter-vector, 8 for qword, 4 for dword.
Default is 0 (force disp32, never use disp8) for safety -- the CPU
always applies compression to EVEX disp8, so using the wrong N
produces silently wrong addresses."
  (let ((op-map (or map opcode-prefix))
        (is-vsib (or vm (and (ea-p thing)
                             (ea-index thing)
                             (not (gpr-p (ea-index thing)))))))
    (multiple-value-bind (ll r x b r-prime v-prime)
        (determine-evex-flags thing reg ll vvvv)
      (let ((vvvv-num (if vvvv (reg-id-num (reg-id vvvv)) 0)))
        (emit-evex segment r x b r-prime op-map w
                   vvvv-num prefix z ll evex-b v-prime aaa))
      (emit-bytes segment opcode)
      (emit-ea segment thing reg
               :remaining-bytes remaining-bytes
               :xmm-index is-vsib
               ;; VSIB operands compress too (Tuple1 Scalar: N is the
               ;; element size); the generated gathers/scatters pass it.
               :disp-n disp-n))))

(defun emit-avx2-inst (segment thing reg prefix opcode
                       &key (remaining-bytes 0)
                            l
                            (opcode-prefix #x0F)
                            (w 0)
                            evex-w
                            vvvv
                            is4
                            vm)
  ;; Auto-detect ZMM operands and delegate to EVEX encoding.
  (flet ((evex-reg-p (r)
           (and (register-p r)
                (or (is-zmm-id-p (reg-id r))
                    (is-kreg-id-p (reg-id r))
                    (>= (reg-id-num (reg-id r)) 16)))))
    (when (or (evex-reg-p reg)
              (evex-reg-p thing)
              (evex-reg-p vvvv))
      (return-from emit-avx2-inst
        (emit-avx512-inst segment thing reg prefix opcode
                          :remaining-bytes remaining-bytes
                          ;; When L is explicitly 0 (scalar or 128-bit-only instruction),
                          ;; preserve LL=0. Otherwise, let determine-evex-flags auto-detect
                          ;; the vector length from register types.
                          :ll (if (eql l 0) 0 nil)
                          :opcode-prefix opcode-prefix
                          :w (or evex-w w)
                          :vvvv vvvv
                          :vm vm
                          ;; Force disp32 for auto-promoted VEX instructions:
                          ;; the correct N depends on tuple type which varies
                          ;; per instruction. disp-n=0 disables disp8 entirely.
                          :disp-n 0))))
  (emit-vex segment vvvv thing reg prefix opcode-prefix l w)
  (emit-bytes segment opcode)
  (when is4
    (incf remaining-bytes))
  ;; FIXME: :xmm-index should be removed and we should alter the EA
  ;; to have the proper FPR as the index reg when appropriate.
  (emit-ea segment thing reg :remaining-bytes remaining-bytes :xmm-index vm)
  (when is4
    (emit-byte segment (ash (reg-id-num (reg-id is4)) 4))))

;; Emit EVEX-encoded instruction with /i field and immediate byte.
;; THING is the destination (NDD, encoded in vvvv).
;; REG is the source (encoded in ModR/M.r/m).
;; /I is encoded in ModR/M.reg bits 5:3.
(defun emit-avx512-inst-imm (segment thing reg imm prefix opcode /i
                             &key ll
                                  (w 0)
                                  (opcode-prefix #x0F)
                                  (aaa 0) (z 0) (evex-b 0))
  (aver (<= 0 /i 7))
  ;; thing = destination -> goes in vvvv (NDD encoding)
  ;; reg = source -> goes in r/m
  (multiple-value-bind (ll r x b r-prime v-prime)
      (determine-evex-flags reg nil ll thing)
    (let ((vvvv-num (if thing (reg-id-num (reg-id thing)) 0)))
      (emit-evex segment r x b r-prime opcode-prefix w
                 vvvv-num prefix z ll evex-b v-prime aaa)))
  (emit-bytes segment opcode)
  (emit-byte segment (logior (ash (logior #b11000 /i) 3)
                             (reg-encoding reg segment)))
  (emit-byte segment imm))

(defun emit-avx2-inst-imm (segment thing reg imm prefix opcode /i
                           &key l
                                (w 0)
                                evex-w
                                (opcode-prefix #x0F))
  (aver (<= 0 /i 7))
  (flet ((evex-reg-p (r)
           (and (register-p r)
                (or (is-zmm-id-p (reg-id r))
                    (>= (reg-id-num (reg-id r)) 16)))))
    ;; Auto-detect ZMM operands and delegate to EVEX encoding
    (when (or (evex-reg-p reg)
              (evex-reg-p thing))
      (return-from emit-avx2-inst-imm
        (emit-avx512-inst-imm segment thing reg imm prefix opcode /i
                              :ll l :w (or evex-w w) :opcode-prefix opcode-prefix))))
  (emit-vex segment thing reg nil prefix opcode-prefix l w)
  (emit-bytes segment opcode)
  (emit-byte segment (logior (ash (logior #b11000 /i) 3)
                             (reg-encoding reg segment)))
  (emit-byte segment imm))


(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  ;; if both disp-n and a *-mem-size are supplied, this version uses the disp-n
  ;; arg type and loses the explicit size indicator. For the common full-vector
  ;; load/store cases that is fine, but needs more work to fully support sized +
  ;; compressed displacement
  (defun avx512-inst-printer-list (inst-format-stem prefix opcode
                                 &key more-fields printer
                                      (opcode-prefix #x0F)
                                      reg-mem-size
                                      xmmreg-mem-size
                                      w
                                      ll
                                      nds
                                      disp-n)
  (let ((fields `((pp ,(vex-encode-pp prefix))
                  (mm ,(evex-encode-mm opcode-prefix))
                  (op ,opcode)
                  ,@(and w `((w ,w)))
                  ,@(and ll `((ll ,ll)))
                  ,@(cond
                      (disp-n
                       `((reg/mem nil :type
                          ',(ecase disp-n
                              (1  'evex-ymmreg/mem-disp1)
                              (2  'evex-ymmreg/mem-disp2)
                              (4  'evex-ymmreg/mem-disp4)
                              (8  'evex-ymmreg/mem-disp8)
                              (16 'evex-ymmreg/mem-disp16)
                              (32 'evex-ymmreg/mem-disp32)
                              (64 'evex-ymmreg/mem-disp64)))))
                      (xmmreg-mem-size
                       `((reg/mem nil :type
                          ',(case xmmreg-mem-size
                              (:qword 'sized-xmmreg/mem-default-qword)
                              (:dword 'sized-dword-xmmreg/mem)
                              (:word  'sized-word-xmmreg/mem)
                              (:byte  'sized-byte-xmmreg/mem)
                              (:sized 'sized-xmmreg/mem)))))
                      (reg-mem-size
                       `((reg/mem nil :type
                          ',(case reg-mem-size
                              (:qword 'sized-reg/mem-default-qword)
                              (:dword 'sized-dword-reg/mem)
                              (:word  'sized-word-reg/mem)
                              (:byte  'sized-byte-reg/mem)
                              (:sized 'sized-reg/mem))))))
                  ,@more-fields))
        (inst-format (symbolicate "EVEX-" inst-format-stem)))
    (list `(:printer ,inst-format ,fields
                     ,@(cond (printer
                              `(',printer))
                             ((eq nds 'to-mem)
                              `('(:name :tab reg/mem ", " vvvv ", " reg)))
                             (nds
                              `('(:name :tab reg ", " vvvv ", " reg/mem))))))))

  (defun avx2-inst-printer-list (inst-format-stem prefix opcode
                                 &key more-fields printer
                                      (opcode-prefix #x0F)
                                      reg-mem-size
                                      xmmreg-mem-size
                                      w
                                      l
                                      nds
                                      evex)
    (let ((fields `((pp ,(vex-encode-pp prefix))
                    (m-mmmm ,(vex-encode-m-mmmm opcode-prefix))
                    (op ,opcode)
                    ,@(and w `((w ,w)))
                    ,@(and l `((l ,l)))
                    ,@(cond (xmmreg-mem-size
                             `((reg/mem nil :type ',(case xmmreg-mem-size
                                                      (:qword 'sized-xmmreg/mem-default-qword)
                                                      (:dword 'sized-dword-xmmreg/mem)
                                                      (:word 'sized-word-xmmreg/mem)
                                                      (:byte 'sized-byte-xmmreg/mem)
                                                      (:sized 'sized-xmmreg/mem)))))
                            (reg-mem-size
                             `((reg/mem nil :type ',(case reg-mem-size
                                                      (:qword 'sized-reg/mem-default-qword)
                                                      (:dword 'sized-dword-reg/mem)
                                                      (:word 'sized-word-reg/mem)
                                                      (:byte 'sized-byte-reg/mem)
                                                      (:sized 'sized-reg/mem))))))
                    ,@more-fields))
          (inst-formats (if (or (eql w 1)
                                (/= opcode-prefix #x0F))
                            (list (symbolicate "VEX3-" inst-format-stem))
                            (list (symbolicate "VEX2-" inst-format-stem)
                                  (symbolicate "VEX3-" inst-format-stem)))))
      (append
       (mapcar (lambda (inst-format)
                 `(:printer ,inst-format ,fields
                            ,@(cond (printer
                                     `(',printer))
                                    ((eq nds 'to-mem)
                                     `('(:name :tab reg/mem ", " vvvv ", " reg)))
                                    (nds
                                     `('(:name :tab reg ", " vvvv ", " reg/mem))))))
               inst-formats)
       ;; Generate EVEX printer entries so VEX instructions auto-promoted
       ;; to EVEX can be disassembled. Map 0F is always safe (no EVEX-only
       ;; instructions reuse those opcodes). Map 0F38 has many conflicts
       ;; (broadcasts, vmaskmov vs vscalef, etc.), so we include the
       ;; sign/zero extension ranges (#x20-#x25, #x30-#x35) and the FMA
       ;; range (#x96-#xBF) which are safe. Map 0F3A is skipped entirely.
       (when (or evex
                 (= opcode-prefix #x0F)
                 (and (= opcode-prefix #x0F38)
                      (or (<= #x20 opcode #x25)
                          (<= #x30 opcode #x35)
                          (<= #x96 opcode #xbf))))
         (avx512-inst-printer-list inst-format-stem prefix opcode
                                   :more-fields more-fields
                                   :printer printer
                                   :opcode-prefix opcode-prefix
                                   :reg-mem-size reg-mem-size
                                   :xmmreg-mem-size xmmreg-mem-size
                                   :w w
                                   :nds nds)))))

  (defun kmov-printer-list (format-stem prefix opcode w &key printer)
    (let ((pp (vex-encode-pp prefix))
          (m-mmmm (vex-encode-m-mmmm #x0F)))
      (flet ((make-printer (inst-format fields)
               `(:printer ,inst-format ,fields
                          ,@(when printer `(',printer)))))
        (if (eql w 1)
            (list
             (make-printer
              (symbolicate "VEX3-" format-stem)
              `((pp ,pp)
                (m-mmmm ,m-mmmm)
                (w ,w)
                (l 0)
                (op ,opcode))))
            (list
             (make-printer
              (symbolicate "VEX2-" format-stem)
              `((pp ,pp)
                (l 0)
                (op ,opcode)))
             (make-printer
              (symbolicate "VEX3-" format-stem)
              `((pp ,pp)
                (m-mmmm ,m-mmmm)
                (w ,w)
                (l 0)
                (op ,opcode)))))))))



;;; Runtime support for vector instruction emitters.

(defun avx512-register-p (reg)
  (and reg
       (not (ea-p reg))
       (register-p reg)
       (>= (reg-id-num (reg-id reg)) 16)))

(defun avx512-width-register-p (reg)
  (and reg (not (ea-p reg)) (zmm-register-p reg)))

(defun needs-evex-p (k z bcast &rest regs)
  (or k z bcast
      (some (lambda (r) (or (avx512-register-p r) (avx512-width-register-p r)))
            (remove nil regs))))

(defun full-vector-disp-n (reg)
  (cond ((and reg (not (ea-p reg)) (zmm-register-p reg)) 64)
        ((and reg (not (ea-p reg)) (ymm-register-p reg)) 32)
        (t 16)))

(defun widest-vector-disp-n (&rest operands)
  (reduce #'max operands :key #'full-vector-disp-n :initial-value 16))

(defun check-evex-vector-length (name allowed &rest operands)
  (let ((width (apply #'widest-vector-disp-n operands)))
    (unless (member width allowed)
      (error "~A has no ~D-bit form (only ~{~D~^/~}-bit)"
             name (* 8 width) (mapcar (lambda (w) (* 8 w)) allowed)))))

(defun mask-arg-number (mask)
  (cond ((integerp mask) mask)
        ((k-register-p mask) (reg-id-num (reg-id mask)))
        ((sb-c:tn-p mask) (sb-c:tn-offset mask))
        (t (error "Invalid mask ~S" mask))))


(define-instruction-format (vex3-vex-gpr (+ 24 16) :include vex3
                                                   :default-printer '(:name :tab reg ", " vvvv ", " reg/mem))
  (op :field (byte 8 (+ 24 0)))
  (vvvv :type 'vvvv-reg)
  (reg/mem :fields (list (byte 2 (+ 24 14)) (byte 3 (+ 24 8))) :type 'reg/mem)
  (reg :field (byte 3 (+ 24 11)) :type 'reg))

;;; With a trailing imm8 (RORX), which the disassembler must consume.
(define-instruction-format (vex3-vex-gpr-imm (+ 24 16) :include vex3-vex-gpr
                                                       :default-printer '(:name :tab reg ", " reg/mem ", " imm))
  (imm :type 'imm-byte))


(define-instruction mulx (segment &prefix prefix hi lo src)
  (:emitter
   (emit-avx2-inst segment src hi #xF2 #xF6 :opcode-prefix #x0f38 :vvvv lo
                                            :w (ecase (pick-operand-size prefix lo src)
                                                 (:qword 1)
                                                 (:dword 0))))
  . #.(avx2-inst-printer-list 'vex-gpr #xF2 #xF6
                              :nds t
                              :opcode-prefix #x0f38))

(define-instruction adcx (segment &prefix prefix dst src)
  (:printer ext-2byte-prefix-reg-reg/mem
            ((prefix #x66) (op1 #x38) (op2 #xf6)))
  (:emitter
   (let ((size (pick-operand-size prefix dst src)))
     (aver (memq size '(:dword :qword)))
     (emit-sse-inst-2byte segment dst src #x66 #x38 #xf6
                          :operand-size size))))

(define-instruction adox (segment &prefix prefix dst src)
  (:printer ext-2byte-prefix-reg-reg/mem
            ((prefix #xf3) (op1 #x38) (op2 #xf6)))
  (:emitter
   (let ((size (pick-operand-size prefix dst src)))
     (aver (memq size '(:dword :qword)))
     (emit-sse-inst-2byte segment dst src #xf3 #x38 #xf6
                          :operand-size size))))

;;;; BMI1/2 instructions (VEX-encoded GPR operations)
;;;; All use VEX.LZ (L=0). W=1 for 64-bit, W=0 for 32-bit.

;;; Shifts with shift count in GPR (no flag side effects, not serialized on CL)
;;; SHRX/SHLX/SARX r64a, r/m64, r64b
;;;   dst = reg field, src = r/m, count = vvvv
(macrolet ((def (name prefix opcode third-op-name)
             `(define-instruction ,name (segment &prefix prefix dst src ,third-op-name)
                ,@(avx2-inst-printer-list 'vex-gpr prefix opcode
                                          :printer '(:name :tab reg ", " reg/mem ", " vvvv)
                                          :nds t
                                          :opcode-prefix #x0f38)
                (:emitter
                 (emit-avx2-inst segment src dst ,prefix ,opcode
                                 :opcode-prefix #x0f38
                                 :vvvv ,third-op-name
                                 :l 0
                                 :w (ecase (pick-operand-size prefix dst src)
                                      (:qword 1)
                                      (:dword 0)))))))
  (def shrx #xF2 #xF7 count)
  (def shlx #x66 #xF7 count)
  (def sarx #xF3 #xF7 count)
  (def bzhi nil #xF5 position)
  (def bextr nil #xF7 control))

;;; PEXT/PDEP r64a, r64b, r/m64
;;;   dst = reg field, src = vvvv, mask = r/m
(macrolet ((def (name prefix opcode &optional (third-op-name 'src2)
                                              printer)
             `(define-instruction ,name (segment &prefix prefix dst src ,third-op-name)
                ,@(avx2-inst-printer-list 'vex-gpr prefix opcode
                                          :nds t
                                          :printer printer
                                          :opcode-prefix #x0f38)
                (:emitter
                 (emit-avx2-inst segment ,third-op-name dst ,prefix ,opcode
                                 :opcode-prefix #x0f38
                                 :vvvv src
                                 :l 0
                                 :w (ecase (pick-operand-size prefix dst src)
                                      (:qword 1)
                                      (:dword 0)))))))
  (def pext #xF3 #xF5 mask)
  (def pdep #xF2 #xF5 mask)
  (def andn nil #xF2))

;;; RORX r64a, r/m64, imm8
;;;   dst = reg field, src = r/m, count = imm8, no vvvv
(define-instruction rorx (segment &prefix prefix dst src imm)
  (:emitter
   (emit-avx2-inst segment src dst #xF2 #xF0
                   :opcode-prefix #x0f3a
                   :l 0
                   :w (ecase (pick-operand-size prefix dst src)
                        (:qword 1)
                        (:dword 0))
                   :remaining-bytes 1)
   (emit-byte segment imm))
  . #.(avx2-inst-printer-list 'vex-gpr-imm #xF2 #xF0
                              :opcode-prefix #x0f3a
                              :printer '(:name :tab reg ", " reg/mem ", " imm)))
(macrolet ((def (name reg)
             `(define-instruction ,name (segment &prefix prefix dst src)
                ,@(avx2-inst-printer-list 'vex-gpr nil #xF3
                                          :opcode-prefix #x0f38
                                          :more-fields `((reg ,reg))
                                          :printer '(:name :tab vvvv ", " reg/mem))
                (:emitter
                 (emit-avx2-inst segment src ,reg nil #xF3
                                 :opcode-prefix #x0f38
                                 :vvvv dst
                                 :l 0
                                 :w (ecase (pick-operand-size prefix dst src)
                                      (:qword 1)
                                      (:dword 0)))))))
  (def blsr 1)
  (def blsmsk 2)
  (def blsi 3))
