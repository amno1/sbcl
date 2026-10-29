;;; Under the evaluator
;;; (SB-SIMD-TEST-SUITE::|SB-SIMD-SSE4.1:U32.4-IF|)
;;; failed for me with
;;;    RESULT-0 = #<SIMD-PACK   33554433        127       8191    2097153>,
;;;    OUTPUT-0 = #<SIMD-PACK          3       8193       2303 1073741823>.
;;; maybe there are other problems but I didn't investigate further.
#+(or interpreter gc-stress) (invoke-restart 'run-tests::skip-file)

(handler-case (require :sb-simd)
  (condition (c)
    (cond ((search "Don't know how" (princ-to-string c))
           (format t "~&Skipping test of sb-simd~%")
           (invoke-restart 'run-tests::skip-file))
          (t
           (error "Unexpected error: ~A" c)))))

(load "compiler-test-util.lisp")

(with-compilation-unit ()
  (dolist (file '("packages.lisp"
                  "numbers.lisp"
                  "utilities.lisp"
                  "test-suite.lisp"
                  "test-arefs.lisp"
                  #+x86-64 "test-arefs-x86-64.lisp"
                  #+arm64 "test-arefs-arm64.lisp"
                  "test-simple-simd-functions.lisp"
                  #+x86-64 "test-simple-simd-functions-x86-64.lisp"
                  #+arm64 "test-simple-simd-functions-arm64.lisp"
                  "test-horizontal-functions.lisp"
                  #+x86-64 "test-horizontal-functions-x86-64.lisp"
                  #+arm64 "test-horizontal-functions-arm64.lisp"
                  #+arm64 "test-arm64-regressions.lisp"
                  "test-hairy-simd-functions.lisp"
                  #+x86-64 "test-masks.lisp"
                  #+x86-64 "test-bmi.lisp"
                  "test-packages.lisp"))
    (load (merge-pathnames file #P"../contrib/sb-simd/test-suite/"))))
(sb-simd-test-suite::run-test-suite)

#+x86-64
(when (plusp (sb-alien:extern-alien "avx2_supported" int))
  (with-test (:name :sb-simd-constant-loop-hoist)
    (let* ((fun (checked-compile
                 '(lambda (sap n)
                    (declare (type sb-sys:system-area-pointer sap)
                             (type fixnum n)
                             (optimize (speed 3) (safety 0)))
                    (let ((c (sb-simd-avx2:u8.32 10))
                          (acc 0))
                      (declare (type sb-simd-avx2:u8.32 c)
                               (type (unsigned-byte 64) acc))
                      (dotimes (i n)
                        (incf acc (sb-simd-avx2:u8.32-movemask
                                   (sb-simd-avx2:u8.32=
                                    (sb-simd-avx2:u8.32-sap-ref sap 0) c)))
                        (incf acc (sb-simd-avx2:u8.32-movemask
                                   (sb-simd-avx2:u8.32=
                                    (sb-simd-avx2:u8.32-sap-ref sap 32) c))))
                      acc))))
           (lines (ctu:disassembly-lines fun))
           (head (position-if (lambda (line) (search "L0:" line)) lines))
           (tail (position-if (lambda (line) (search "L0" line)) lines :from-end t)))
      (assert head)
      ;; The vector constant is materialized once before the loop ...
      (assert (find-if (lambda (line) (search "[RIP" line)) (subseq lines 0 head)))
      ;; ... and never re-read from memory inside the loop body.
      (assert (notany (lambda (line) (search "[RIP" line))
                      (subseq lines head (1+ tail)))))))
