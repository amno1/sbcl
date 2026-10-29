(in-package #:sb-simd-bmi2)

(define-instruction-set :bmi2
  (:test (bmi2-supported-p))
  (:include :bmi1)
  (:instructions
   ;; u32
   (u32-bzhi    #:bzhi   (u32) (u32 u32)  :cost 1 :prefix '(:dword))
   (u32-pdep    #:pdep   (u32) (u32 u32)  :cost 3 :prefix '(:dword))
   (u32-pext    #:pext   (u32) (u32 u32)  :cost 3 :prefix '(:dword))
   (u32-rorx    #:rorx   (u32) (u32 imm5) :cost 1 :prefix '(:dword))
   (u32-sarx    #:sarx   (u32) (u32 u32)  :cost 1 :prefix '(:dword))
   (u32-shlx    #:shlx   (u32) (u32 u32)  :cost 1 :prefix '(:dword))
   (u32-shrx    #:shrx   (u32) (u32 u32)  :cost 1 :prefix '(:dword))
   (u32-mulx    nil      (u32 u32) (u32 u32) :cost 4 :encoding :custom)
   ;; u64
   (u64-bzhi    #:bzhi   (u64) (u64 u64)  :cost 1 :prefix '(:qword))
   (u64-pdep    #:pdep   (u64) (u64 u64)  :cost 3 :prefix '(:qword))
   (u64-pext    #:pext   (u64) (u64 u64)  :cost 3 :prefix '(:qword))
   (u64-rorx    #:rorx   (u64) (u64 imm6) :cost 1 :prefix '(:qword))
   (u64-sarx    #:sarx   (u64) (u64 u64)  :cost 1 :prefix '(:qword))
   (u64-shlx    #:shlx   (u64) (u64 u64)  :cost 1 :prefix '(:qword))
   (u64-shrx    #:shrx   (u64) (u64 u64)  :cost 1 :prefix '(:qword))
   (u64-mulx    nil      (u64 u64) (u64 u64) :cost 4 :encoding :custom)))
