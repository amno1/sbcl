(in-package #:sb-simd-avx512cd)

(define-instruction-set :avx512cd
  (:test (avx512cd-supported-p))
  (:include :avx512dq)
  (:instructions
   ;; Conflict detection
   (u32.16-conflict       #:vpconflictd  (u32.16) (u32.16)        :cost 1)
   (s32.16-conflict       #:vpconflictd  (s32.16) (s32.16)        :cost 1)
   (u64.8-conflict        #:vpconflictq  (u64.8)  (u64.8)         :cost 1)
   (s64.8-conflict        #:vpconflictq  (s64.8)  (s64.8)         :cost 1)

   ;; Leading zero count
   (u32.16-lzcnt          #:vplzcntd     (u32.16) (u32.16)        :cost 1)
   (s32.16-lzcnt          #:vplzcntd     (s32.16) (s32.16)        :cost 1)
   (u64.8-lzcnt           #:vplzcntq     (u64.8)  (u64.8)         :cost 1)
   (s64.8-lzcnt           #:vplzcntq     (s64.8)  (s64.8)         :cost 1)))
