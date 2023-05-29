(define-module vbm
  (use binary.io)
  (use gauche.uvector)
  )
(select-module vbm)

(define-constant VBM_FLAG_HAS_VERTICES  #x01)
(define-constant VBM_FLAG_HAS_INDICES   #x02)
(define-constant VBM_FLAG_HAS_FRAMES    #x04)
(define-constant VBM_FLAG_HAS_MATERIALS #x08)

(define-class <vbm> ()
  ((size :init-keyword :size)
   (name :init-keyword :name)
   (num-attribs :init-keyword :num-attribs)
   (num-frames :init-keyword :num-frames)
   (num-vertices :init-keyword :num-vertices)
   (num-indices :init-keyword :num-indices)
   (index-type :init-keyword :index-type)
   (num-materials :init-keyword :num-materials)
   (flags :init-keyword :flags)))

;; Reaad the header
;; num-chunks field only exists in the old VBM header and no longer used
(define (read-vbm-header port)
  (let* ([magic         (read-u32 port)]
         [size          (read-u32 port)]
         [name          (read-uvector <u8vector> 64 port)]
         [num-attribs   (read-u32 port)]
         [num-frames    (read-u32 port)]
         [num-chunks    (if (eqv? magic #x314d4253)
                          #f              ;new header
                          (read-u32 port))]
         [num-vertices  (read-u32 port)]
         [num-indices   (read-u32 port)]
         [index-type    (read-u32 port)]
         [num-materials (read-u32 port)]
         [flags         (read-u32 port)])
    (make <vbm>
      :size size
      :name name             ;u8vector
      :num-attribs num-attribs
      :num-frames num-frames
      :num-vertices num-vertices
      :num-indices num-indices
      :index-type index-type
      :num-materials num-materials
      :flags flags)))
