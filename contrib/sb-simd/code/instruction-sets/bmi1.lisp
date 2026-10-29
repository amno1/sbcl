(in-package #:sb-simd-bmi1)

(define-instruction-set :bmi1
  (:test (bmi1-supported-p))
  (:include :x86-64)
  (:instructions
   ;; u32
   (u32-andn    #:andn   (u32) (u32 u32) :cost 1 :prefix '(:dword))
   (u32-bextr   #:bextr  (u32) (u32 u32) :cost 2 :prefix '(:dword))
   (u32-blsi    #:blsi   (u32) (u32)     :cost 1 :prefix '(:dword))
   (u32-blsmsk  #:blsmsk (u32) (u32)     :cost 1 :prefix '(:dword))
   (u32-blsr    #:blsr   (u32) (u32)     :cost 1 :prefix '(:dword))
   (u32-tzcnt   #:tzcnt  (u32) (u32)     :cost 1 :prefix '(:dword))
   ;; u64
   (u64-andn    #:andn   (u64) (u64 u64) :cost 1 :prefix '(:qword))
   (u64-bextr   #:bextr  (u64) (u64 u64) :cost 2 :prefix '(:qword))
   (u64-blsi    #:blsi   (u64) (u64)     :cost 1 :prefix '(:qword))
   (u64-blsmsk  #:blsmsk (u64) (u64)     :cost 1 :prefix '(:qword))
   (u64-blsr    #:blsr   (u64) (u64)     :cost 1 :prefix '(:qword))
   (u64-tzcnt   #:tzcnt  (u64) (u64)     :cost 1 :prefix '(:qword))))
