;;;
;;; simple/viewer.scm - simple viewer
;;;  
;;;   Copyright (c) 2008-2012  Shiro Kawai  <shiro@acm.org>
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
;;   Those callback receives a vector as the state of the viewer.
;;   The first element is the dimension (2 or 3), followed by 
;;   elements about projection, and the following 9
;;   elements about camera position & orientation.
;;
;;     #f32(<dim> <left> <right> <bottom> <top> <near> <far>
;;          <tx> <ty> <tz> <rx> <ry> <rz> <sx> <sy> <sz>)
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
  (use util.match)
  (use util.list)
  (use srfi-42)
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
          )
  )
(select-module gl.simple.viewer)

(define *default-key-handlers*    (make-hash-table 'eqv?))
(define *default-display-proc*    #f)
(define *default-grid-proc*       (^[v] (default-grid v)))
(define *default-axis-proc*       (^[v] (default-axis v)))
(define *default-reshape3-proc*   (^[w h v] (default-reshape3 w h v)))
(define *default-reshape2-proc*   (^[w h v] (default-reshape2 w h v)))

;; Accessors to the viewer state vector
;; NB: If we make gauche.record inline pseudo-rtd accessors, we can
;; replace those procedures with cleaner record definition!

(define-macro (define-viewer-accessors slots)
  (define (gen slot k)
    (let ([access (string->symbol #"viewer-~|slot|")]
          [modify (string->symbol #"viewer-~|slot|-set!")])
      `((define-inline (,access v) (f32vector-ref v ,k))
        (define-inline (,modify v n) (f32vector-set! v ,k n)))))
  `(begin ,@(append-ec (: slot (index k) slots) (gen slot k))))

(define-viewer-accessors
  (dimension left right bottom top near far
   tx ty tz rx ry rz sx sy sz))

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

;; Common viewer driver (both 2d and 3d)
(define (make-viewer name dimension
                     :key
                     (parent #f)
                     (mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))
                     (title  (x->string name))
                     (width  300)
                     (height 300)
                     (x      #f)
                     (y      #f)
                     (zoom   1.0))
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
  (define viewer-info (rlet1 v (make-f32vector 16)
                        (viewer-dimension-set! v dimension)))

  (define key-handlers (hash-table-copy *default-key-handlers*))
  (define grid-proc    *default-grid-proc*)
  (define axis-proc    *default-axis-proc*)
  (define display-proc *default-display-proc*)
  (define reshape-proc
    (ecase dimension
      [(2) *default-reshape2-proc*]
      [(3) *default-reshape3-proc*]))

  (define (display-setup-3d)
    (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    (gl-push-matrix)
    (gl-scale zoom zoom zoom)
    (gl-translate xlatx xlaty 0.0)
    (gl-rotate rotx 1.0 0.0 0.0)
    (gl-rotate roty 0.0 1.0 0.0)
    (gl-rotate rotz 0.0 0.0 1.0))
  (define (display-setup-2d)
    (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    (gl-push-matrix)
    (gl-scale zoom zoom 1.0)
    (gl-translate xlatx xlaty 0.0))
  (define (display-common)
    (gl-disable GL_LIGHTING)
    (and grid-proc (grid-proc viewer-info))
    (and axis-proc (axis-proc viewer-info))
    (gl-color 1.0 1.0 1.0 0.0)
    (gl-line-width 1.0)
    (and display-proc (display-proc))
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
    (and reshape-proc (reshape-proc w h viewer-info)))

  (define (mouse-fn button state x y)
    (cond [(= state GLUT_UP)
           (set! prev-x -1) (set! prev-y -1) (set! prev-b #f)]
          [else
           (set! prev-x x) (set! prev-y y) (set! prev-b button)]))

  (define (motion-fn3 x y)
    (cond [(eqv? prev-b GLUT_LEFT_BUTTON)
           (inc! rotx (* (/. (- y prev-y) height) 90.0))
           (inc! roty (* (/. (- x prev-x) width) 90.0))
           (viewer-rx-set! viewer-info rotx)
           (viewer-ry-set! viewer-info roty)]
          [(eqv? prev-b GLUT_MIDDLE_BUTTON)
           (inc! xlatx (* (/. (- x prev-x) width (sqrt zoom)) 12.0))
           (inc! xlaty (* (/. (- prev-y y) height (sqrt zoom)) 12.0))
           (viewer-tx-set! viewer-info xlatx)
           (viewer-ty-set! viewer-info xlaty)]
          [(eqv? prev-b GLUT_RIGHT_BUTTON)
           (set! zoom (clamp (* (+ 1.0 (* (/. (- prev-y y) height) 2.0))
                                zoom)
                             0.1 1000.0))
           (viewer-sx-set! viewer-info zoom)
           (viewer-sy-set! viewer-info zoom)
           (viewer-sz-set! viewer-info zoom)])
    (set! prev-x x) (set! prev-y y)
    (glut-post-redisplay))

  (define (motion-fn2 x y)
    (cond [(or (= prev-b GLUT_LEFT_BUTTON)
               (= prev-b GLUT_MIDDLE_BUTTON))
           (inc! xlatx (/. (* 2 (- x prev-x)) zoom))
           (inc! xlaty (/. (* 2 (- prev-y y)) zoom))
           (viewer-tx-set! viewer-info xlatx)
           (viewer-ty-set! viewer-info xlaty)]
          [(= prev-b GLUT_RIGHT_BUTTON)
           (set! zoom (clamp (* (+ 1.0 (* (/. (- prev-y y) height) 2.0))
                                zoom)
                             0.1 1000.0))
           (viewer-sx-set! viewer-info zoom)
           (viewer-sy-set! viewer-info zoom)])
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
      [('display proc) (set! display-proc proc)]
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
  (glut-display-func  (ecase dimension [(2) display-fn2] [(3) display-fn3]))
  (glut-reshape-func  reshape-fn)
  (glut-mouse-func    mouse-fn)
  (glut-motion-func   (ecase dimension [(2) motion-fn2] [(3) motion-fn3]))
  (glut-keyboard-func keyboard-fn)
  (glut-special-func  special-fn)

  ;; Enable some commonly used stuff
  ;; TODO: make them customizable
  (gl-enable GL_CULL_FACE)
  (gl-enable GL_DEPTH_TEST)
  (gl-enable GL_NORMALIZE)

  name)
                     
;; Creates a GL window, 3D view
(define (simple-viewer-window name . keys)
  (apply make-viewer name 3 keys))

(define (simple-viewer-window-2d name . keys)
  (apply make-viewer name 2 keys))

(define (simple-viewer-get-window)
  (id->window-name (glut-get-window)))

(define (simple-viewer-set-window name)
  (cond [(name->window-id name) => glut-set-window]))

;; Callback registrar.  
(define-syntax define-registrar
  (syntax-rules ()
    [(_ varname key default-var)
     (define (varname proc . opts)
       (match opts
         [() (set! default-var proc)]
         [(name)
          (cond [(name->window name) => (^[win] (ref win'closure) 'key proc)]
                [else
                 (errorf "~a: no such window with name: ~a" 'varname name)])]
         ))]))

(define-registrar simple-viewer-display    display *default-display-proc*)
(define-registrar simple-viewer-reshape    reshape *default-reshape3-proc*)
(define-registrar simple-viewer-reshape-2d reshape *default-reshape2-proc*)
(define-registrar simple-viewer-grid       grid    *default-grid-proc*)
(define-registrar simple-viewer-axis       axis    *default-axis-proc*)

(define (simple-viewer-set-key! window . args)
  (let1 tab (cond [(not window) *default-key-handlers*]
                  [(name->window window) => (cut ref <> 'key-handlers)]
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
    (viewer-left-set! v -1.0)
    (viewer-right-set! v -1.0)
    (viewer-bottom-set! v (- ratio))
    (viewer-top-set! v ratio)
    (viewer-near-set! v 5.0)
    (viewer-far-set! v 10000.0)

    (gl-matrix-mode GL_MODELVIEW)
    (gl-load-identity)
    (gl-translate 0.0 0.0 -40.0)
    ))

(define (default-reshape2 w h v)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-ortho-2d (- w) w (- h) h)
  (viewer-left-set! v (- w))
  (viewer-right-set! v w)
  (viewer-bottom-set! v (- h))
  (viewer-top-set! v h)
  (viewer-near-set! v 1.0)
  (viewer-far-set! v -1.0)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  (gl-translate 0.0 0.0 1.0))

(define (default-grid v)
  (gl-color 0.5 0.5 0.5 0.0)
  (gl-line-width 1.0)
  (gl-begin* GL_LINES
    (do-ec (: i -5 6)
           (begin
             (gl-vertex i 0 -5)
             (gl-vertex i 0 5)
             (gl-vertex -5 0 i)
             (gl-vertex 5  0 i)))))

(define (default-axis v)
  (define (axis a b c)
    (gl-color a b c 0.0)
    (gl-begin* GL_LINES
      (gl-vertex 0 0 0)
      (gl-vertex a b c)))
  (gl-line-width 3.0)
  (axis 1.0 0.0 0.0)
  (axis 0.0 1.0 0.0)
  (axis 0.0 0.0 1.0))

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
