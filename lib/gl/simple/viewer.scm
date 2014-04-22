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

;; Using simple viewers:
;; * You can create multiple GL windows (viewers), each has a unique name.
;;   Viewers are specified by its name.  Each viewer has reasonable
;;   default behaviors.
;;
;;     API: simple-viewer-window
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
;; * Calling simple-viewer-run enters main loop.
;;
;;     API: simple-viewer-run
;;

(define-module gl.simple.viewer
  (use gl)
  (use gl.glut)
  (use gl.math3d)
  (use util.match)
  (use util.list)
  (use srfi-42)
  (export simple-viewer-window
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

(define *default-key-handlers* (make-hash-table 'eqv?))
(define *default-display-proc* #f)
(define *default-grid-proc*    (^[] (default-grid)))
(define *default-axis-proc*    (^[] (default-axis)))
(define *default-reshape-proc* (^[w h] (default-reshape w h)))

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
      
;; Creates a GL window, 3D view
(define (simple-viewer-window name :key
                              (parent #f)
                              (mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))
                              (title  (x->string name))
                              (width  300)
                              (height 300)
                              (x      #f)
                              (y      #f))
  ;; Internal state
  (define prev-x -1)
  (define prev-y -1)
  (define prev-b #f)
  (define rotx 20.0)
  (define roty -30.0)
  (define rotz 0.0)
  (define xlatx 0.0)
  (define xlaty 0.0)
  (define zoom  1.0)

  (define key-handlers (hash-table-copy *default-key-handlers*))
  (define grid-proc    *default-grid-proc*)
  (define axis-proc    *default-axis-proc*)
  (define display-proc *default-display-proc*)
  (define reshape-proc *default-reshape-proc*)

  ;; Callback closures
  (define (display-fn)
    (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    (gl-push-matrix)
    (gl-scale zoom zoom zoom)
    (gl-translate xlatx xlaty 0.0)
    (gl-rotate rotx 1.0 0.0 0.0)
    (gl-rotate roty 0.0 1.0 0.0)
    (gl-rotate rotz 0.0 0.0 1.0)

    (gl-disable GL_LIGHTING)
    (and grid-proc (grid-proc))
    (and axis-proc (axis-proc))
    (gl-color 1.0 1.0 1.0 0.0)
    (gl-line-width 1.0)
    (and display-proc (display-proc))
    (gl-pop-matrix)
    (glut-swap-buffers))

  (define (reshape-fn w h)
    (set! height h) (set! width w)
    (and reshape-proc (reshape-proc w h)))

  (define (mouse-fn button state x y)
    (cond [(= state GLUT_UP)
           (set! prev-x -1) (set! prev-y -1) (set! prev-b #f)]
          [else
           (set! prev-x x) (set! prev-y y) (set! prev-b button)]))

  (define (motion-fn x y)
    (cond [(= prev-b GLUT_LEFT_BUTTON)
           (inc! rotx (* (/. (- y prev-y) height) 90.0))
           (inc! roty (* (/. (- x prev-x) width) 90.0))]
          [(= prev-b GLUT_MIDDLE_BUTTON)
           (inc! xlatx (* (/. (- x prev-x) width (sqrt zoom)) 12.0))
           (inc! xlaty (* (/. (- prev-y y) height (sqrt zoom)) 12.0))]
          [(= prev-b GLUT_RIGHT_BUTTON)
           (set! zoom (clamp (* (+ 1.0 (* (/. (- prev-y y) height) 2.0))
                                zoom)
                             0.1 1000.0))])
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
  (glut-display-func  display-fn)
  (glut-reshape-func  reshape-fn)
  (glut-mouse-func    mouse-fn)
  (glut-motion-func   motion-fn)
  (glut-keyboard-func keyboard-fn)
  (glut-special-func  special-fn)

  ;; Enable some commonly used stuff
  ;; TODO: make them customizable
  (gl-enable GL_CULL_FACE)
  (gl-enable GL_DEPTH_TEST)
  (gl-enable GL_NORMALIZE)

  name)

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

(define-registrar simple-viewer-display display *default-display-proc*)
(define-registrar simple-viewer-reshape reshape *default-reshape-proc*)
(define-registrar simple-viewer-grid    grid    *default-grid-proc*)
(define-registrar simple-viewer-axis    axis    *default-axis-proc*)

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
(define (default-reshape w h)
  (let1 ratio (/ h w)
    (gl-viewport 0 0 w h)
    (gl-matrix-mode GL_PROJECTION)
    (gl-load-identity)
    (gl-frustum -1.0 1.0 (- ratio) ratio 5.0 10000.0)
    (gl-matrix-mode GL_MODELVIEW)
    (gl-load-identity)
    (gl-translate 0.0 0.0 -40.0)
    ))

(define (default-grid)
  (gl-color 0.5 0.5 0.5 0.0)
  (gl-line-width 1.0)
  (gl-begin* GL_LINES
    (do-ec (: i -5 6)
           (begin
             (gl-vertex i 0 -5)
             (gl-vertex i 0 5)
             (gl-vertex -5 0 i)
             (gl-vertex 5  0 i)))))

(define (default-axis)
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
