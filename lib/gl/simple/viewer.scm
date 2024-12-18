;;;
;;; simple/viewer.scm - simple viewer
;;;
;;;   Copyright (c) 2008-2023  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; This is a simple viewer skeleton.  It is by no means intended for
;; general applications; it's rather a handy tool to quickly hack up
;; a throwaway script to visualize some data.

;; The API may seem a bit unusual.  When you create a viewer window, you don't
;; get a viewer object you draw in.  You register callbacks that are called
;; whenever the viewer needs to redraw its content, but you're mostly agnostic
;; about internal state of viewer; the concept is that you draw your world
;; in the model space, and viewers are user-controlled autonomous observers.
;; Not requiring accessing viewer's internal state in the display callbacks
;; has a performance advantage, though we lose some flexilibility.

;; Using simple viewers:
;; * You can create multiple GL windows (viewers), each has a unique name.
;;   Viewers are specified by its name.  Each viewer has reasonable
;;   default behaviors.
;;
;;     API: simple-viewer-window
;;          simple-viewer-window-2d
;;
;; * You can associate callback functions for display, reshape, key event, etc.
;;   for each viewer, or as a default behavior.
;;
;;     API: simple-viewer-display
;;          simple-viewer-reshape
;;          simple-viewer-grid
;;          simple-viewer-axis
;;          simple-viewer-set-key!
;;
;;   Those callback receives a vector ('vinfo') as the state of the viewer.
;;   We use a vector instead of a struct or an instance to minimize overhead
;;   of accessing it.  Provided accessor/mutator are inlined.
;;
;;     API: vinfo-projection-mode
;;
;;          vinfo-left vinfo-right vinfo-bottom
;;          vinfo-top vinfo-near vinfo-far
;;
;;          vinfo-tx vinfo-ty vinfo-tz
;;          vinfo-rx vinfo-ry vinfo-rz
;;          vinfo-sx vinfo-sy vinfo-sz
;;
;;          vinfo-br vinfo-bg vinfo-bb vinfo-ba
;;          vinfo-gr vinfo-gg vinfo-gb vinfo-ga
;;          vinfo-draw-ground?
;;
;;          make-vinfo
;;
;; * Calling simple-viewer-run enters main loop.
;;
;;     API: simple-viewer-run
;;

(define-module gl.simple.viewer
  (use gauche.uvector)
  (use gl)
  (use gl.glut)
  (use gl.math3d)
  (use srfi.42)
  (use util.match)
  (export simple-viewer-window
          simple-viewer-window-2d
          simple-viewer-set-window
          simple-viewer-get-window
          simple-viewer-display
          simple-viewer-reshape
          simple-viewer-grid
          simple-viewer-axis
          simple-viewer-set-key!
          simple-viewer-run

          vinfo-projection-mode
          vinfo-left vinfo-right vinfo-top vinfo-bottom vinfo-near vinfo-far
          vinfo-tx vinfo-ty vinfo-tz
          vinfo-rx vinfo-ry vinfo-rz
          vinfo-sx vinfo-sy vinfo-sz
          vinfo-br vinfo-bg vinfo-bb vinfo-ba
          vinfo-gr vinfo-gg vinfo-gb vinfo-ga
          vinfo-draw-ground?

          *projection-perspective*
          *projection-orthographic*
          )
  )
(select-module gl.simple.viewer)


(define *default-key-handlers*    (make-hash-table 'eqv?))
(define *default-display-proc*    #f)
(define *default-grid2-proc*      (^[v] (default-grid2 v)))
(define *default-grid3-proc*      (^[v] (default-grid3 v)))
(define *default-axis-proc*       (^[v] (default-axis v)))
(define *default-reshape3-proc*   (^[w h v] (default-reshape3 w h v)))
(define *default-reshape2-proc*   (^[w h v] (default-reshape2 w h v)))

;; Accessors to the viewer state vector
;; NB: If we make gauche.record inline pseudo-rtd accessors, we can
;; replace those procedures with cleaner record definition!
;;
;; The first element indicates the type of projection
(define-constant *projection-perspective* 0.0)
(define-constant *projection-orthographic* 1.0)

(define-macro (define-vinfo-accessors slots)
  (define (gen slot k)
    (let ([int-access (string->symbol #"%vinfo-~|slot|")]
          [int-modify (string->symbol #"%vinfo-~|slot|-set!")]
          [access (string->symbol #"vinfo-~|slot|")])
      ;; TRANSIENT: This verbose definition is to allow setter
      ;; to be inlined. As of 0.9.15, Gauche doens't inline
      ;; when you give lambda exprs directly to getter-with-setter.
      ;; See https://github.com/shirok/Gauche/issues/1076
      `((define-inline (,int-access v) (f32vector-ref v ,k))
        (define-inline (,int-modify v n) (f32vector-set! v ,k n))
        (define-inline ,access
          (getter-with-setter ,int-access ,int-modify)))))
  `(begin ,@(append-ec (: slot (index k) slots) (gen slot k))
          (define (make-vinfo)
            (make-f32vector ,(length slots)))))

;; The order of slots may change between versions; the user should always use
;; accessors.
;; The ground is only drawn when ga > 0
(define-vinfo-accessors
  (projection                           ; 0.0 or 1.0
   left right bottom top near far       ; View frustum
   tx ty tz                             ; Translation
   rx ry rz                             ; Rotation
   sx sy sz                             ; Scale
   br bg bb ba                          ; Background color
   gr gg gb ga                          ; Ground color (only in 3d)
   ))

(define-inline (vinfo-projection-mode vinfo)
  (if (= (vinfo-projection vinfo) *projection-perspective*)
    :perspective
    :orthographic))

(define-inline (vinfo-draw-ground? vinfo)
  (> (vinfo-ga vinfo) 0.0))

;;=============================================================
;; Wrapper of GLUT window
;;

;; We internally maintain <simple-viewer-window> instance to manage
;; GLUT windows created by simple viewer.  However, we only expose
;; window names (symbols) to the users.

(define-class <simple-viewer-window> ()
  (;; all slots private.  use API.
   (name    :init-keyword :name)        ; window name (symbol)
   (id      :init-keyword :id)          ; GLUT window id
   (parent  :init-keyword :parent)      ; parent window, if this is sub
   (closure :init-keyword :closure)     ; a closure to maintain the
                                        ;  internal state.
   (name-tab :allocation :class         ; name -> window
             :init-form (make-hash-table 'eq?))
   (id-tab   :allocation :class         ; id -> window
             :init-form (make-hash-table 'eqv?))
   ))

(define-method initialize ((win <simple-viewer-window>) args)
  (next-method)
  (hash-table-put! (ref win'name-tab) (ref win'name) win)
  (hash-table-put! (ref win'id-tab)   (ref win'id)   win))

(define (name->window name)
  (hash-table-get (class-slot-ref <simple-viewer-window> 'name-tab) name #f))
(define (id->window id)
  (hash-table-get (class-slot-ref <simple-viewer-window> 'id-tab) id #f))
(define (name->window-id name)
  (and-let* [(win (name->window name))] (ref win'id)))
(define (id->window-name id)
  (and-let* [(win (id->window id))] (ref win'name)))

;;=============================================================
;; Viewer constructor
;;

;; PROJECTION may be :perspective or :orthographic

(define (make-viewer name projection
                     :key
                     (parent #f)
                     (mode (logior GLUT_DOUBLE GLUT_RGBA
                                   (ecase projection
                                     [(:perspective) GLUT_DEPTH]
                                     [(:orthographic) 0])))
                     (title  (x->string name))
                     (width  300)
                     (height 300)
                     (x      #f)
                     (y      #f)
                     (zoom   1.0)
                     (background-color '#f32(0.0 0.0 0.0 1.0))
                     (ground-color '#f32(0.0 0.0 0.0 0.0))
                     )
  (define proj-mode
    (ecase projection
      [(:perspective)  *projection-perspective*]
      [(:orthographic) *projection-orthographic*]))
  (define (proj-choose pers ortho)
    (if (= proj-mode *projection-perspective*) pers ortho))

  ;; Internal state
  (define prev-x -1)
  (define prev-y -1)
  (define prev-b #f)
  (define rotx 20.0)
  (define roty -30.0)
  (define rotz 0.0)
  (define xlatx 0.0)
  (define xlaty 0.0)

  ;; Viewer info to pass to callbacks
  (define vinfo
    (rlet1 v (make-vinfo)
      (set! (vinfo-projection v) proj-mode)
      (set! (vinfo-sx v) zoom)
      (set! (vinfo-sy v) zoom)
      (set! (vinfo-sz v) (proj-choose zoom 1))
      (set! (vinfo-br v) (f32vector-ref background-color 0))
      (set! (vinfo-bg v) (f32vector-ref background-color 1))
      (set! (vinfo-bb v) (f32vector-ref background-color 2))
      (set! (vinfo-ba v) (if (<= 4 (f32vector-length background-color))
                          (f32vector-ref background-color 3)
                          1.0))
      (set! (vinfo-gr v) (f32vector-ref ground-color 0))
      (set! (vinfo-gg v) (f32vector-ref ground-color 1))
      (set! (vinfo-gb v) (f32vector-ref ground-color 2))
      (set! (vinfo-ga v) (if (<= 4 (f32vector-length ground-color))
                          (f32vector-ref ground-color 3)
                          1.0))
      ))

  (define key-handlers (hash-table-copy *default-key-handlers*))
  (define grid-proc    (proj-choose *default-grid3-proc* *default-grid2-proc*))
  (define axis-proc    *default-axis-proc*)
  (define display-proc
    (if (and *default-display-proc*
             (eqv? (arity *default-display-proc*) 0))
      (^_ (*default-display-proc*)) ; for the backward compatibility
      *default-display-proc*))
  (define reshape-proc
    (if (= proj-mode *projection-perspective*)
      *default-reshape3-proc*
      *default-reshape2-proc*))

  (define (display-setup-3d)
    (gl-clear-color (vinfo-br vinfo) (vinfo-bg vinfo)
                    (vinfo-bb vinfo) (vinfo-ba vinfo))
    (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    (gl-push-matrix)
    (gl-scale zoom zoom zoom)
    (gl-translate xlatx xlaty 0.0)
    (gl-rotate rotx 1.0 0.0 0.0)
    (gl-rotate roty 0.0 1.0 0.0)
    (gl-rotate rotz 0.0 0.0 1.0))
  (define (display-setup-2d)
    (gl-clear-color (vinfo-br vinfo) (vinfo-bg vinfo)
                    (vinfo-bb vinfo) (vinfo-ba vinfo))
    (gl-clear (logior GL_COLOR_BUFFER_BIT))
    (gl-push-matrix)
    (gl-scale zoom zoom 1.0)
    (gl-translate xlatx xlaty 0.0))
  (define (display-common)
    (gl-disable GL_LIGHTING)
    (when (vinfo-draw-ground? vinfo)
      (draw-ground-plane vinfo))
    (and grid-proc (grid-proc vinfo))
    (and axis-proc (axis-proc vinfo))
    (gl-color 1.0 1.0 1.0 1.0)
    (gl-line-width 1.0)
    (and display-proc (display-proc vinfo))
    (gl-pop-matrix)
    (glut-swap-buffers))

  ;; Callback closures
  (define (display-fn3)
    (display-setup-3d)
    (display-common))

  (define (display-fn2)
    (display-setup-2d)
    (display-common))

  (define (reshape-fn w h)
    (set! height h) (set! width w)
    (and reshape-proc (reshape-proc w h vinfo)))

  (define (mouse-fn button state x y)
    (cond [(= state GLUT_UP)
           (set! prev-x -1) (set! prev-y -1) (set! prev-b #f)]
          [else
           (set! prev-x x) (set! prev-y y) (set! prev-b button)]))

  (define (motion-fn3 x y)
    (cond [(eqv? prev-b GLUT_LEFT_BUTTON)
           (inc! rotx (* (/. (- y prev-y) height) 90.0))
           (inc! roty (* (/. (- x prev-x) width) 90.0))
           (set! (vinfo-rx vinfo) rotx)
           (set! (vinfo-ry vinfo) roty)]
          [(eqv? prev-b GLUT_MIDDLE_BUTTON)
           (inc! xlatx (* (/. (- x prev-x) width (sqrt zoom)) 12.0))
           (inc! xlaty (* (/. (- prev-y y) height (sqrt zoom)) 12.0))
           (set! (vinfo-tx vinfo) xlatx)
           (set! (vinfo-ty vinfo) xlaty)]
          [(eqv? prev-b GLUT_RIGHT_BUTTON)
           (set! zoom (clamp (* (+ 1.0 (* (/. (- prev-y y) height) 2.0))
                                zoom)
                             0.1 1000.0))
           (set! (vinfo-sx vinfo) zoom)
           (set! (vinfo-sy vinfo) zoom)
           (set! (vinfo-sz vinfo) zoom)])
    (set! prev-x x) (set! prev-y y)
    (glut-post-redisplay))

  (define (motion-fn2 x y)
    (cond [(or (= prev-b GLUT_LEFT_BUTTON)
               (= prev-b GLUT_MIDDLE_BUTTON))
           (inc! xlatx (/. (* 2 (- x prev-x)) zoom))
           (inc! xlaty (/. (* 2 (- prev-y y)) zoom))
           (set! (vinfo-tx vinfo) xlatx)
           (set! (vinfo-ty vinfo) xlaty)]
          [(= prev-b GLUT_RIGHT_BUTTON)
           (set! zoom (clamp (* (+ 1.0 (* (/. (- prev-y y) height) 2.0))
                                zoom)
                             0.1 1000.0))
           (set! (vinfo-sx vinfo) zoom)
           (set! (vinfo-sy vinfo) zoom)])
    (set! prev-x x) (set! prev-y y)
    (glut-post-redisplay))

  (define (keyboard-fn key x y)
    (common-keyboard-func key-handlers key x y))
  (define (special-fn key x y)
    (common-special-func key-handlers key x y))

  (define (closure . args)
    (match args
      [('grid proc)    (set! grid-proc proc)]
      [('axis proc)    (set! axis-proc proc)]
      [('display proc)
       ;; hack for the backward compatibility
       (if (eqv? (arity proc) 0)
         (set! display-proc (^_ (proc))) ; old API
         (set! display-proc proc))]
      [('reshape proc) (set! reshape-proc proc)]
      [('key-handlers) key-handlers]
      [_ (error "unrecognized simple-viewer-window message:" args)]))

  (glut-init-display-mode mode)
  ;; Register GLUT window id.
  (let* ([pwin (and parent (name->window parent))]
         [id   (cond [pwin
                      (glut-create-sub-window (ref pwin'id )
                                              (or x 0) (or y 0)
                                              width height)]
                     [else
                      (glut-init-window-size width height)
                      (when (and x y)
                        (glut-init-window-position x y))
                      (glut-create-window title)])])
    (make <simple-viewer-window>
      :name name :id id :parent pwin :closure closure))

  ;; Set up handlers.
  (glut-display-func
    (if (= proj-mode *projection-perspective*)
      display-fn3
      display-fn2))
  (glut-reshape-func  reshape-fn)
  (glut-mouse-func    mouse-fn)
  (glut-motion-func
    (if (= proj-mode *projection-perspective*)
      motion-fn3
      motion-fn2))
  (glut-keyboard-func keyboard-fn)
  (glut-special-func  special-fn)

  ;; Enable some commonly used stuff
  ;; TODO: make them customizable
  (if (= proj-mode *projection-perspective*)
    (begin
      (gl-enable GL_CULL_FACE)
      (gl-enable GL_DEPTH_TEST)
      (gl-enable GL_NORMALIZE))
    (begin
      (gl-enable GL_LINE_SMOOTH)
      (gl-enable GL_BLEND)
      (gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
      (gl-hint GL_LINE_SMOOTH_HINT GL_NICEST)))

  name)

;; Creates a GL window, 3D view
(define (simple-viewer-window name . keys)
  (apply make-viewer name :perspective keys))

(define (simple-viewer-window-2d name . keys)
  (apply make-viewer name :orthographic keys))

(define (simple-viewer-get-window)
  (id->window-name (glut-get-window)))

(define (simple-viewer-set-window name)
  (cond [(name->window-id name) => glut-set-window]))

;; Callback registrar.
(define-syntax define-registrar
  (syntax-rules ()
    [(_ varname key default2-var default3-var)
     (define (varname proc . opts)
       (match opts
         [() (set! default3-var proc)]
         [(':perspective)  (set! default3-var proc)]
         [(':orthographic) (set! default2-var proc)]
         [(name)
          (cond [(name->window name) => (^[win] ((ref win'closure) 'key proc))]
                [else
                 (errorf "~a: no such window with name: ~a" 'varname name)])]
         ))]))

(define-registrar simple-viewer-display
  display *default-display-proc* *default-display-proc*)
(define-registrar simple-viewer-reshape
  reshape *default-reshape3-proc* *default-reshape2-proc*)
(define-registrar simple-viewer-grid
  grid    *default-grid3-proc* *default-grid2-proc*)
(define-registrar simple-viewer-axis
  axis    *default-axis-proc* *default-axis-proc*)

(define (simple-viewer-set-key! window . args)
  (let1 tab (cond [(not window) *default-key-handlers*]
                  [(name->window window) => (^[win] ((ref win'closure) 'key-handlers))]
                  [else
                   (error "simple-viewer-set-key!: no such window:" window)])
    (let loop ([args args])
      (match args
        [() '()]
        [(key proc . rest)
         (if proc
           (hash-table-put! tab key proc)
           (hash-table-delete! tab key))
         (loop rest)]
        [else '()]))))

(define (simple-viewer-run :key (rescue-errors #t))
  (if rescue-errors
    (let1 eport (current-error-port)
      (let loop ()
        (guard (e [else (format eport "*** SIMPLE-VIEWER: ~a\n"
                                (ref e'message))])
          (glut-main-loop))
        (loop)))
    (glut-main-loop)))

;;
;; Default handlers (private)
;;
(define (default-reshape3 w h v)
  (let1 ratio (/ h w)
    (gl-viewport 0 0 w h)

    (gl-matrix-mode GL_PROJECTION)
    (gl-load-identity)
    (gl-frustum -1.0 1.0 (- ratio) ratio 5.0 10000.0)
    (set! (vinfo-left v) -1.0)
    (set! (vinfo-right v) -1.0)
    (set! (vinfo-bottom v) (- ratio))
    (set! (vinfo-top v) ratio)
    (set! (vinfo-near v) 5.0)
    (set! (vinfo-far v) 10000.0)

    (gl-matrix-mode GL_MODELVIEW)
    (gl-load-identity)
    (gl-translate 0.0 0.0 -40.0)
    ))

(define (default-reshape2 w h v)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-ortho-2d (- w) w (- h) h)
  (set! (vinfo-left v) (- w))
  (set! (vinfo-right v) w)
  (set! (vinfo-bottom v) (- h))
  (set! (vinfo-top v) h)
  (set! (vinfo-near v) 1.0)
  (set! (vinfo-far v) -1.0)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  (gl-translate 0.0 0.0 1.0))

(define-inline (default-grid-common v)
  (gl-color 0.5 0.5 0.5)
  (gl-line-width 0.6))

(define (default-grid2 v)
  (default-grid-common v)
  ;; For 2D: we draw grid to cover entire viewport
  (let ([sx (vinfo-sx v)]
        [sy (vinfo-sy v)])
    (let ([xmin (floor   (- (/ (vinfo-left v) sx) (vinfo-tx v)))]
          [xmax (ceiling (- (/ (vinfo-right v) sx) (vinfo-tx v)))]
          [ymin (floor   (- (/ (vinfo-bottom v) sy) (vinfo-ty v)))]
          [ymax (ceiling (- (/ (vinfo-top v) sy) (vinfo-ty v)))])
      (gl-begin* GL_LINES
        (do-ec (: x xmin (+ xmax 1))
               (begin (gl-vertex x ymin) (gl-vertex x ymax)))
        (do-ec (: y ymin (+ ymax 1))
               (begin (gl-vertex xmin y) (gl-vertex xmax y)))))))

(define (default-grid3 v)
  (default-grid-common v)
  ;; For 3D: we draw a grid plane near the origin
  ;; If we also draw the ground, we offset Y a bit to avoid flashing
  (let1 y (if (vinfo-draw-ground? v) 0.002 0.0)
    (gl-begin* GL_LINES
      (do-ec (: i -5 6)
             (begin
               (gl-vertex i y -5)
               (gl-vertex i y 5)
               (gl-vertex -5 y i)
               (gl-vertex 5  y i))))))

(define (draw-ground-plane v)
  (gl-color (vinfo-gr v) (vinfo-gg v) (vinfo-gb v) (vinfo-ga v))
  (gl-begin* GL_TRIANGLES
    (gl-vertex  0 0  0 1)
    (gl-vertex  0 0  1 0)
    (gl-vertex  1 0  0 0)
    (gl-vertex  0 0  0 1)
    (gl-vertex  0 0 -1 0)
    (gl-vertex -1 0  0 0)
    (gl-vertex  0 0  0 1)
    (gl-vertex  1 0  0 0)
    (gl-vertex  0 0 -1 0)
    (gl-vertex  0 0  0 1)
    (gl-vertex -1 0  0 0)
    (gl-vertex  0 0  1 0)
    ))

(define (default-axis v)
  (if (= (vinfo-projection v) *projection-perspective*)
    (let ()
      (define (axis a b c)
        (gl-color a b c)
        (gl-begin* GL_LINES
          (gl-vertex 0 0 0)
          (gl-vertex a b c)))
      (gl-line-width 3.0)
      (axis 1.0 0.0 0.0)
      (axis 0.0 1.0 0.0)
      (axis 0.0 0.0 1.0))
    (begin
      (gl-line-width 1.0)
      (gl-color 1 0 0)
      (gl-begin* GL_LINES
        (gl-vertex (- (/ (vinfo-left v) (vinfo-sx v)) (vinfo-tx v)) 0)
        (gl-vertex (- (/ (vinfo-right v) (vinfo-sx v)) (vinfo-tx v)) 0))
      (gl-color 0 1 0)
      (gl-begin* GL_LINES
        (gl-vertex 0 (- (/ (vinfo-bottom v) (vinfo-sy v)) (vinfo-ty v)))
        (gl-vertex 0 (- (/ (vinfo-top v) (vinfo-sy v)) (vinfo-ty v)))))))

(define (quit-loop)
  (cond-expand
   [gauche.sys.pthreads
    ;; If we're in primordial thread, just terminating the current thread
    ;; lets other threads linger, which may cause unwanted behavior.
    ;; NB: There should be a more certain way to determine if we're in
    ;; primordial thread or not.
    (if (equal? (slot-ref (current-thread) 'name) "root")
      (exit)
      (thread-terminate! (current-thread)))]
   [else (exit)]))

;; common key handler
(define (common-keyboard-func table keycode x y)
  (cond [(hash-table-get table (integer->char keycode) #f) => (cut <> x y)])
  (glut-post-redisplay))

(define (common-special-func table keycode x y)
  (cond [(hash-table-get table keycode #f) => (cut <> x y)])
  (glut-post-redisplay))

;;
;; Set up default keymaps
;;

(simple-viewer-set-key! #f #\escape (^ _ (quit-loop)))
