(in-package #:sb-simd-internals)

#+x86-64
(progn
  (defun cpuid (eax &optional (ecx 0))
    "Call cpuid instruction for a given EAX leaf and optional ECX sub-leaf.
Returns is returned in EAX, EBX, ECX, and EDX registers."
    (declare (type (unsigned-byte 32) eax ecx))
    (sb-vm::%cpu-identification eax ecx))

  (defun sse-supported-p ()
    "Streaming SIMD Extensions (SSE) instructions."
    (and (>= (cpuid 0) 1)
         (logbitp 25 (nth-value 3 (cpuid 1)))))

  (defun sse2-supported-p ()
    "Streaming SIMD Extensions 2 (SSE2) instructions."
    (and (>= (cpuid 0) 1)
         (logbitp 26 (nth-value 3 (cpuid 1)))))

  (defun sse3-supported-p ()
    "Streaming SIMD Extensions 3 (SSE3) instructions."
    (and (>= (cpuid 0) 1)
         (logbitp 0 (nth-value 2 (cpuid 1)))))

  (defun ssse3-supported-p ()
    "Supplemental Streaming SIMD Extensions 3 (SSSE3) are supported"
    (and (>= (cpuid 0) 1)
         (logbitp 9 (nth-value 2 (cpuid 1)))))

  (defun sse4.1-supported-p ()
    "Streaming SIMD Extensions 4.1 (SSE4.1) instructions."
    (and (>= (cpuid 0) 1)
         (logbitp 19 (nth-value 2 (cpuid 1)))))

  (defun sse4.2-supported-p ()
    "Streaming SIMD Extensions 4.2 (SSE4.2) instructions."
    (and (>= (cpuid 0) 1)
         (logbitp 20 (nth-value 2 (cpuid 1)))))

  (defun avx-supported-p ()
    "Advanced Vector Extensions (AVX) instructions."
    (and (>= (cpuid 0) 1)
         (logbitp 28 (nth-value 2 (cpuid 1)))))

  (defun avx2-supported-p ()
    "Advanced Vector Extensions 2 (AVX2) instructions."
    (and (>= (cpuid 0) 7)
         (logbitp 5 (nth-value 1 (cpuid 7)))))

  (defun fma-supported-p ()
    "Fused Multiply-Add 3 (FMA3) instructions instructions."
    (and (>= (cpuid 0) 1)
         (logbitp 12 (nth-value 2 (cpuid 1)))))

  (defun avx512f-supported-p ()
    "AVX-512 Foundation instructions"
    (and (>= (cpuid 0) 7)
         (logbitp 16 (nth-value 1 (cpuid 7)))))

  (defun avx512dq-supported-p ()
    "AVX-512 Doubleword and Quadword instructions"
    (and (>= (cpuid 0) 7)
         (logbitp 17 (nth-value 1 (cpuid 7)))))

  (defun avx512ifma-supported-p ()
    "AVX-512 Integer Fused Multiply-Add instructions"
    (and (>= (cpuid 0) 7)
         (logbitp 21 (nth-value 1 (cpuid 7)))))

  (defun avx512cd-supported-p ()
    "AVX-512 Conflict Detection instructions"
    (and (>= (cpuid 0) 7)
         (logbitp 28 (nth-value 1 (cpuid 7)))))

  (defun avx512bw-supported-p ()
    "AVX-512 Byte and Word instructions"
    (and (>= (cpuid 0) 7)
         (logbitp 30 (nth-value 1 (cpuid 7)))))

  (defun avx512vl-supported-p ()
    "AVX-512 Vector Length extensions (allowing 128/256-bit operations)"
    (and (>= (cpuid 0) 7)
         (logbitp 31 (nth-value 1 (cpuid 7))))))

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

  (defun avx512ifma-supported-p ()
    nil)

  (defun avx512cd-supported-p ()
    nil)

  (defun avx512bw-supported-p ()
    nil)

  (defun avx512vl-supported-p ()
    nil))
