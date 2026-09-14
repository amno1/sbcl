(in-package #:sb-simd-internals)

#+x86-64
(progn
  (defun cpuid (eax &optional (ecx 0))
    (declare (type (unsigned-byte 32) eax ecx))
    (sb-vm::%cpu-identification eax ecx))

  (defun sse-supported-p ()
    (and (>= (cpuid 0) 1)
         (logbitp 25 (nth-value 3 (cpuid 1)))))

  (defun sse2-supported-p ()
    (and (>= (cpuid 0) 1)
         (logbitp 26 (nth-value 3 (cpuid 1)))))

  (defun sse3-supported-p ()
    (and (>= (cpuid 0) 1)
         (logbitp 0 (nth-value 2 (cpuid 1)))))

  (defun ssse3-supported-p ()
    (and (>= (cpuid 0) 1)
         (logbitp 9 (nth-value 2 (cpuid 1)))))

  (defun sse4.1-supported-p ()
    (and (>= (cpuid 0) 1)
         (logbitp 19 (nth-value 2 (cpuid 1)))))

  (defun sse4.2-supported-p ()
    (and (>= (cpuid 0) 1)
         (logbitp 20 (nth-value 2 (cpuid 1)))))

  (defun avx-supported-p ()
    (and (>= (cpuid 0) 1)
         (logbitp 28 (nth-value 2 (cpuid 1)))))

  (defun avx2-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 5 (nth-value 1 (cpuid 7)))))

  (defun fma-supported-p ()
    (and (>= (cpuid 0) 1)
         (logbitp 12 (nth-value 2 (cpuid 1)))))

  (defun avx512f-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 16 (nth-value 1 (cpuid 7 0)))))

  (defun avx512dq-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 17 (nth-value 1 (cpuid 7 0)))))

  (defun avx512cd-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 28 (nth-value 1 (cpuid 7 0)))))

  (defun avx512bw-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 30 (nth-value 1 (cpuid 7 0)))))

  (defun avx512vl-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 31 (nth-value 1 (cpuid 7 0)))))

  (defun avx512vpopcntdq-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 14 (nth-value 2 (cpuid 7 0)))))

  (defun avx512vnni-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 11 (nth-value 2 (cpuid 7 0)))))

  (defun avx512bitalg-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 12 (nth-value 2 (cpuid 7 0)))))

  (defun avx512ifma-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 21 (nth-value 1 (cpuid 7 0)))))

  ;; GFNI is a separate CPUID feature from AVX-512, present on some
  ;; CPUs with only SSE/AVX (no AVX-512 at all). This checks only the
  ;; raw feature bit; the 512-bit ZMM forms wired up in
  ;; instruction-sets/avx512gfni.lisp additionally require AVX512F,
  ;; checked separately at that instruction set's :test clause.
  (defun gfni-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 8 (nth-value 2 (cpuid 7 0)))))

  (defun avx512vbmi-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 1 (nth-value 2 (cpuid 7 0)))))

  (defun avx512vbmi2-supported-p ()
    (and (>= (cpuid 0) 7)
         (logbitp 6 (nth-value 2 (cpuid 7 0)))))

  (defun avx512fp16-supported-p ()
    (and (>= (cpuid 0) 7)
         (avx512f-supported-p)
         (logbitp 23 (nth-value 3 (cpuid 7 0)))))

  (defun avx10-supported-p ()
    (and (>= (cpuid 0) 7)
         (>= (nth-value 0 (cpuid 7 0)) 1)
         (logbitp 19 (nth-value 3 (cpuid 7 1)))))

  (defun avx10.1-supported-p ()
    (and (avx10-supported-p)
         (>= (cpuid 0) #x24)
         (>= (ldb (byte 8 0) (nth-value 1 (cpuid #x24 0))) 1)))

  (defun avx10.2-supported-p ()
    (and (avx10-supported-p)
         (>= (cpuid 0) #x24)
         (>= (ldb (byte 8 0) (nth-value 1 (cpuid #x24 0))) 2)))

  (defun avx10-128-supported-p ()
    (and (avx10-supported-p)
         (>= (cpuid 0) #x24)
         (logbitp 16 (nth-value 1 (cpuid #x24 0)))))

  (defun avx10-256-supported-p ()
    (and (avx10-supported-p)
         (>= (cpuid 0) #x24)
         (logbitp 17 (nth-value 1 (cpuid #x24 0)))))

  (defun avx10-512-supported-p ()
    (and (avx10-supported-p)
         (>= (cpuid 0) #x24)
         (logbitp 18 (nth-value 1 (cpuid #x24 0))))))

#-x86-64
(progn
  (defun sse-supported-p ()
    nil)

  (defun sse2-supported-p ()
    nil)

  (defun sse3-supported-p ()
    nil)

  (defun ssse3-supported-p ()
    nil)

  (defun sse4.1-supported-p ()
    nil)

  (defun sse4.2-supported-p ()
    nil)

  (defun avx-supported-p ()
    nil)

  (defun avx2-supported-p ()
    nil)

  (defun fma-supported-p ()
    nil)

  (defun avx512f-supported-p ()
    nil)

  (defun avx512dq-supported-p ()
    nil)

  (defun avx512cd-supported-p ()
    nil)

  (defun avx512bw-supported-p ()
    nil)

  (defun avx512vl-supported-p ()
    nil)

  (defun avx512vpopcntdq-supported-p ()
    nil)

  (defun avx512vnni-supported-p ()
    nil)

  (defun avx512bitalg-supported-p ()
    nil)

  (defun avx512ifma-supported-p ()
    nil)

  (defun gfni-supported-p ()
    nil)

  (defun avx512vbmi-supported-p ()
    nil)

  (defun avx512vbmi2-supported-p ()
    nil)

  (defun avx512fp16-supported-p ()
    nil)

  (defun avx10-supported-p ()
    nil)

  (defun avx10.1-supported-p ()
    nil)

  (defun avx10.2-supported-p ()
    nil)

  (defun avx10-128-supported-p ()
    nil)

  (defun avx10-256-supported-p ()
    nil)

  (defun avx10-512-supported-p ()
    nil))

(defun neon-supported-p ()
  #+arm64 t)
