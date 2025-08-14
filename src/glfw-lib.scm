;;;
;;; glfw-lib.scm - glfw functions for Gauche-gl
;;;
;;;  Copyright (c) 2020-2023  Shiro Kawai  <shiro@acm.org>
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
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
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(select-module gl.glfw)

(inline-stub
 (declcode (.include "gauche-glfw.h"))

 ;; Kludge - define-cstruct generates declaration including SCM_EXTERN
 ;; unconditionally.  It gets in our way here.  Egentually genstub
 ;; will take care of it.
 (declcode (.undef "SCM_EXTERN")
           (.define "SCM_EXTERN extern"))

 (define-type <glfw-window> GLFWwindow*
   "GLFWwindow"
   "SCM_GLFW_WINDOW_P"
   "SCM_GLFW_WINDOW"
   "Scm_MakeGlfwWindow")
 (define-type <glfw-monitor> GLFWmonitor*
   "GLFWmonitor"
   "SCM_GLFW_MONITOR_P"
   "SCM_GLFW_MONITOR"
   "Scm_MakeGlfwMonitor")
 (define-type <glfw-cursor> GLFWcursor*
   "GLFWcursor"
   "SCM_GLFW_CURSOR_P"
   "SCM_GLFW_CURSOR"
   "Scm_MakeGlfwCursor")
 (define-cstruct <glfw-vidmode> "GLFWvidmode"
   (width::<int>
    height::<int>
    red-bits::<int>    "redBits"
    green-bits::<int>  "greenBits"
    blue-bits::<int>   "blueBits"
    refresh-rate::<int> "refreshRate"))
 )

;;;
;;; Initialization and shutdown
;;;

(define-cproc glfw-init () ::<boolean>
  (let* ([r::int (glfwInit)])
    (unless (== r GLFW_TRUE)
      (Scm_GlfwError "glfwInit"))
    (return TRUE)))

(define-cproc glfw-terminate () ::<void>
  (glfwTerminate))

(define-cproc glfw-get-version () ::(<int> <int> <int>)
  (let* ([major::int] [minor::int] [rev::int])
    (glfwGetVersion (& major) (& minor) (& rev))
    (return major minor rev)))

(define-cproc glfw-get-version-string () ::<const-cstring>
  glfwGetVersionString)

;; (define-cproc glfw-get-error/description () ::(<int> <top>)
;;   (let* ([desc::(const char*)]
;;          [code::int (glfwGetError (& desc))])
;;     (return int
;;             (?: (== desc NULL)
;;                 SCM_FALSE
;;                 (SCM_MAKE_STR_COPYING desc)))))

;; (define-cproc glfw-get-error/code () ::<int>
;;   (return (glfwGetError NULL)))


;;;
;;; Global callbacks
;;;

(inline-stub
 (define-cvar global_cbs ::(.array ScmObj (SCM_GLFW_NUM_GLOBAL_CALLBACKS))
   :static)

 (initcode (dotimes [i SCM_GLFW_NUM_GLOBAL_CALLBACKS]
             (set! (aref global_cbs i) SCM_FALSE))
           (glfwSetErrorCallback error-cb)
           (glfwSetJoystickCallback joystick-cb))

 (define-cise-stmt call-global-cb
   [(_ enum . args)
    `(let* ([cb (aref global_cbs ,enum)])
       (unless (SCM_FALSEP cb)
         (,(string->symbol (format "Scm_ApplyRec~a" (length args)))
          cb ,@args)))])
 (define-cise-stmt set-global-cb
   [(_ enum proc)
    `(let* ([prev (aref global_cbs ,enum)])
       (set! (aref global_cbs ,enum) ,proc)
       (return prev))])

 (define-cfn error-cb (e::int desc::(const char*)) ::void
   (call-global-cb SCM_GLFW_ERROR_CALLBACK
                   (Scm_MakeInteger e)
                   (SCM_MAKE_STR desc)))
 (define-cproc glfw-set-error-callback (proc)
   (set-global-cb SCM_GLFW_ERROR_CALLBACK proc))

 (define-cfn joystick-cb (jid::int event::int) ::void
   (call-global-cb SCM_GLFW_JOYSTICK_CALLBACK
                   (Scm_MakeInteger jid)
                   (Scm_MakeInteger event)))
 (define-cproc glfw-set-joystick-callback (proc)
   (set-global-cb SCM_GLFW_JOYSTICK_CALLBACK proc))
 )

;;;
;;; Monitor
;;;

(define-cproc glfw-get-primary-monitor () ::<glfw-monitor>
  glfwGetPrimaryMonitor)

(define-cproc glfw-get-monitors ()
  (let* ([count::int]
         [ms::GLFWmonitor** (glfwGetMonitors (& count))]
         [r (Scm_MakeVector count SCM_FALSE)])
    (dotimes [i count]
      (set! (SCM_VECTOR_ELEMENT r i)
            (Scm_MakeGlfwMonitor (aref ms i))))
    (return r)))

(define-cproc glfw-get-monitor-pos (m::<glfw-monitor>) ::(<int> <int>)
  (let* ([xpos::int] [ypos::int])
    (glfwGetMonitorPos m (& xpos) (& ypos))
    (return xpos ypos)))

;; from 3.3
;; (define-cproc glfw-get-monitor-work-area (m::<glfw-monitor>)
;;   ::(<int> <int> <int> <int>)
;;   (let* ([xpos::int] [ypos::int] [width::int] [height::int])
;;     (glfwGetMonitorWorkArea m (& xpos) (& ypos) (& width) (& height))
;;     (return xpos ypos width height)))

(define-cproc glfw-get-monitor-physical-size (m::<glfw-monitor>) ::(<int> <int>)
  (let* ([wmm::int] [hmm::int])
    (glfwGetMonitorPhysicalSize m (& wmm) (& hmm))
    (return wmm hmm)))

;; from 3.3
;; (define-cproc glfw-get-monitor-content-scale (m::<glfw-monitor>)::(<double> <double>)
;;   (let* ([xscale::float] [yscale::float])
;;     (glfwGetMonitorContentScale m (& xscale) (& yscale))
;;     (return xscale yscale)))

(define-cproc glfw-get-monitor-name (m::<glfw-monitor>) ::<const-cstring>
  glfwGetMonitorName)

;; glfwSetMonitorCallback

(define-cproc glfw-get-video-modes (m::<glfw-monitor>)
  (let* ([count::int]
         [vms::(const GLFWvidmode*) (glfwGetVideoModes m (& count))]
         [r (Scm_MakeVector count SCM_FALSE)])
    (dotimes [i count]
      (set! (SCM_VECTOR_ELEMENT r i)
            (Scm_Make_glfw_vidmode (+ vms i))))
    (return r)))

(define-cproc glfw-get-video-mode (m::<glfw-monitor>)
  ;; We can't use autoboxing yet because of 'const' qualifier.
  (let* ([v::(const GLFWvidmode*) (glfwGetVideoMode m)])
    (return (Scm_Make_glfw_vidmode v))))

(define-cproc glfw-set-gamma (m::<glfw-monitor> gamma::<double>)
  (glfwSetGamma m (cast float gamma)))

;; glfwGetGammaRamp
;; glfwSetGammaRamp

;;;
;;; Window
;;;

(define-cproc glfw-default-window-hints () ::<void>
  (glfwDefaultWindowHints))

(define-cproc glfw-window-hint (hint::<int> value::<int>) ::<void>
  (glfwWindowHint hint value))

;; since 3.3
;; (define-cproc glfw-window-hint-string (hint::<int> value::<const-cstring>)
;;   ::<void>
;;   (glfwWindowHintString hint value))

(define-cproc glfw-create-window (w::<int>
                                  h::<int>
                                  title::<const-cstring>
                                  monitor::<glfw-monitor>?
                                  share::<glfw-window>?)
  ::<glfw-window>?
  Scm_CreateGlfwWindow)

(define-cproc glfw-destroy-window (w) ::<void>
  (Scm_GlfwWindowDestroy w))

(define-cproc glfw-window-should-close (w::<glfw-window>) ::<boolean>
  glfwWindowShouldClose)

(define-cproc glfw-set-window-should-close (w::<glfw-window> value::<boolean>)
  ::<void>
  glfwSetWindowShouldClose)

(define-cproc glfw-set-window-title (w::<glfw-window> title::<const-cstring>)
  ::<void>
  glfwSetWindowTitle)

;; glfwSetWindowIcon

(define-cproc glfw-get-window-pos (w::<glfw-window>) ::(<int> <int>)
  (let* ([xpos::int][ypos::int])
    (glfwGetWindowPos w (& xpos) (& ypos))
    (return xpos ypos)))

(define-cproc glfw-set-window-pos (w::<glfw-window> x::<int> y::<int>)
  ::<void>
  glfwSetWindowPos)

(define-cproc glfw-get-window-size (w::<glfw-window>) ::(<int> <int>)
  (let* ([width::int][height::int])
    (glfwGetWindowSize w (& width) (& height))
    (return width height)))

(define-cproc glfw-set-window-size (w::<glfw-window>
                                    width::<int>
                                    height::<int>)
  ::<void>
  glfwSetWindowSize)

(define-cproc glfw-set-window-size-limits (w::<glfw-window>
                                           min-w::<int>
                                           min-h::<int>
                                           max-w::<int>
                                           max-h::<int>)
  ::<void>
  glfwSetWindowSizeLimits)

(define-cproc glfw-set-window-aspect-ratio (w::<glfw-window>
                                            numer::<int>
                                            denom::<int>)
  ::<void>
  glfwSetWindowAspectRatio)

(define-cproc glfw-get-framebuffer-size (w::<glfw-window>) ::(<int> <int>)
  (let* ([width::int][height::int])
    (glfwGetFramebufferSize w (& width) (& height))
    (return width height)))

(define-cproc glfw-get-window-frame-size (w::<glfw-window>)
  ::(<int> <int> <int> <int>)
  (let* ([left::int][top::int][right::int][bottom::int])
    (glfwGetWindowFrameSize w (& left) (& top) (& right) (& bottom))
    (return left top right bottom)))

;; from 3.3
;; (define-cproc glfw-get-window-content-scale (w::<glfw-window>)
;;   ::(<double> <double>)
;;   (let* ([xscale::float][yscale::float])
;;     (glfwGetWindowContentScale w (& xscale) (& yscale))
;;     (return xscale yscale)))

;; from 3.3
;; (define-cproc glfw-get-window-opacity (w::<glfw-window>) ::<double>
;;   glfwGetWindowOpacity)

;; from 3.3
;; (define-cproc glfw-set-window-opaticy (w::<glfw-window>
;;                                        opacity::<double>)
;;   ::<void>
;;   glfwSetWindowOpacity)

(define-cproc glfw-iconify-window (w::<glfw-window>) ::<void>
  glfwIconifyWindow)

(define-cproc glfw-restore-window (w::<glfw-window>) ::<void>
  glfwRestoreWindow)

(define-cproc glfw-maximize-window (w::<glfw-window>) ::<void>
  glfwMaximizeWindow)

(define-cproc glfw-show-window (w::<glfw-window>) ::<void>
  glfwShowWindow)

(define-cproc glfw-hide-window (w::<glfw-window>) ::<void>
  glfwHideWindow)

(define-cproc glfw-focus-window (w::<glfw-window>) ::<void>
  glfwFocusWindow)

;; from 3.3
;; (define-cproc glfw-request-window-attention (w::<glfw-window>) ::<void>
;;   glfwRequestWindowAttention)

(define-cproc glfw-get-window-monitor (w::<glfw-window>) ::<glfw-monitor>?
  glfwGetWindowMonitor)

(define-cproc glfw-set-window-monitor (w::<glfw-window>
                                       m::<glfw-monitor>?
                                       xpos::<int> ypos::<int>
                                       width::<int> height::<int>
                                       refresh-rate::<int>)
  ::<void>
  glfwSetWindowMonitor)

(define-cproc glfw-get-window-attrib (w::<glfw-window> attrib::<int>)
  ::<int>
  glfwGetWindowAttrib)

;; from 3.3
;; (define-cproc glfw-set-window-attrib (w::<glfw-window>
;;                                       attrib::<int> value::<int>)
;;   ::<void>
;;   glfwSetWindowAttrib)

(define-cproc glfw-poll-events () ::<void> glfwPollEvents)
(define-cproc glfw-wait-events () ::<void> glfwWaitEvents)
(define-cproc glfw-wait-events-timeout (to::<double>) ::<void>
  glfwWaitEventsTimeout)
(define-cproc glfw-post-empty-event () ::<void> glfwPostEmptyEvent)
(define-cproc glfw-swap-buffers (w::<glfw-window>) ::<void> glfwSwapBuffers)

;;;
;;; Callbacks
;;;

;; callback must be an appropriate procedure or #f; we don't check it.

(inline-stub
 (define-cise-stmt call-window-cb
   [(_ w enum . args)
    `(let* ([d::ScmGlfwWindowData* (Scm_GlfwGetWindowData ,w)]
            [cb (aref (-> d window_cbs) ,enum)])
       (unless (SCM_FALSEP cb)
         (,(string->symbol (format "Scm_ApplyRec~a" (+ (length args) 1)))
          cb (Scm_MakeGlfwWindow ,w) ,@args)))])
 (define-cise-stmt set-window-cb
   [(_ w enum proc)
    `(let* ([d::ScmGlfwWindowData* (Scm_GlfwGetWindowData ,w)]
            [prev (aref (-> d window_cbs) ,enum)])
       (set! (aref (-> d window_cbs) ,enum) ,proc)
       (return prev))])

 (define-cfn pos-cb (w::GLFWwindow* xpos::int ypos::int) ::void
   (call-window-cb w SCM_GLFW_POS_CALLBACK
                   (Scm_MakeInteger xpos)
                   (Scm_MakeInteger ypos)))
 (define-cproc glfw-set-window-pos-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_POS_CALLBACK proc))

 (define-cfn size-cb (w::GLFWwindow* width::int height::int) ::void
   (call-window-cb w SCM_GLFW_SIZE_CALLBACK
                   (Scm_MakeInteger width)
                   (Scm_MakeInteger height)))
 (define-cproc glfw-set-window-size-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_SIZE_CALLBACK proc))

 (define-cfn close-cb (w::GLFWwindow*) ::void
   (call-window-cb w SCM_GLFW_CLOSE_CALLBACK))
 (define-cproc glfw-set-window-close-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_CLOSE_CALLBACK proc))

 (define-cfn refresh-cb (w::GLFWwindow*) ::void
   (call-window-cb w SCM_GLFW_REFRESH_CALLBACK))
 (define-cproc glfw-set-window-refresh-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_REFRESH_CALLBACK proc))

 (define-cfn focus-cb (w::GLFWwindow* focused::int) ::void
   (call-window-cb w SCM_GLFW_FOCUS_CALLBACK
                   (SCM_MAKE_BOOL (!= focused GLFW_FALSE))))
 (define-cproc glfw-set-window-focus-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_FOCUS_CALLBACK proc))

 (define-cfn iconify-cb (w::GLFWwindow* iconified::int) ::void
   (call-window-cb w SCM_GLFW_ICONIFY_CALLBACK
                   (SCM_MAKE_BOOL (!= iconified GLFW_FALSE))))
 (define-cproc glfw-set-window-iconify-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_ICONIFY_CALLBACK proc))

 (define-cfn framesize-cb (w::GLFWwindow* width::int height::int) ::void
   (call-window-cb w SCM_GLFW_FRAMESIZE_CALLBACK
                   (Scm_MakeInteger width)
                   (Scm_MakeInteger height)))
 (define-cproc glfw-set-framebuffer-size-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_FRAMESIZE_CALLBACK proc))

 (define-cfn key-cb (w::GLFWwindow* key::int scancode::int
                                    action::int mods::int)
   ::void
   (call-window-cb w SCM_GLFW_KEY_CALLBACK
                   (Scm_MakeInteger key)
                   (Scm_MakeInteger scancode)
                   (Scm_MakeInteger action)
                   (Scm_MakeInteger mods)))
 (define-cproc glfw-set-key-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_KEY_CALLBACK proc))

 (define-cfn char-cb (w::GLFWwindow* codepoint::u_int) ::void
   (call-window-cb w SCM_GLFW_CHAR_CALLBACK
                   (SCM_MAKE_CHAR (Scm_UcsToChar codepoint))))
 (define-cproc glfw-set-char-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_CHAR_CALLBACK proc))

 (define-cfn button-cb (w::GLFWwindow* button::int action::int mods::int)
   ::void
   (call-window-cb w SCM_GLFW_BUTTON_CALLBACK
                   (Scm_MakeInteger button)
                   (Scm_MakeInteger action)
                   (Scm_MakeInteger mods)))
 (define-cproc glfw-set-mouse-button-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_BUTTON_CALLBACK proc))

 (define-cfn cursorpos-cb (w::GLFWwindow* xpos::double ypos::double) ::void
   (call-window-cb w SCM_GLFW_CURSORPOS_CALLBACK
                   (Scm_MakeFlonum xpos)
                   (Scm_MakeFlonum ypos)))
 (define-cproc glfw-set-curosr-pos-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_CURSORPOS_CALLBACK proc))

 (define-cfn enter-cb (w::GLFWwindow* entered::int) ::void
   (call-window-cb w SCM_GLFW_ENTER_CALLBACK
                   (SCM_MAKE_BOOL (!= entered GLFW_FALSE))))
 (define-cproc glfw-set-cursor-enter-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_ENTER_CALLBACK proc))

 (define-cfn scroll-cb (w::GLFWwindow* xoff::double yoff::double) ::void
   (call-window-cb w SCM_GLFW_SCROLL_CALLBACK
                   (Scm_MakeFlonum xoff)
                   (Scm_MakeFlonum yoff)))
 (define-cproc glfw-set-scrool-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_SCROLL_CALLBACK proc))

 (define-cfn drop-cb (w::GLFWwindow* npaths::int paths::(const char**)) ::void
   (call-window-cb w SCM_GLFW_DROP_CALLBACK
                   (Scm_CStringArrayToList paths npaths SCM_STRING_COPYING)))
 (define-cproc glfw-set-drop-callback (w::<glfw-window> proc)
   (set-window-cb w SCM_GLFW_DROP_CALLBACK proc))

 (define-cfn Scm__SetupWindowCallbacks (w::GLFWwindow*) ::void
   (glfwSetWindowPosCallback w pos_cb)
   (glfwSetWindowSizeCallback w size_cb)
   (glfwSetWindowCloseCallback w close_cb)
   (glfwSetWindowRefreshCallback w refresh_cb)
   (glfwSetWindowFocusCallback w focus_cb)
   (glfwSetWindowIconifyCallback w iconify_cb)
   (glfwSetFramebufferSizeCallback w framesize_cb)

   (glfwSetKeyCallback w key_cb)
   (glfwSetCharCallback w char_cb)
   (glfwSetMouseButtonCallback w button_cb)
   (glfwSetCursorPosCallback w cursorpos_cb)
   (glfwSetCursorEnterCallback w enter_cb)
   (glfwSetScrollCallback w scroll_cb)
   (glfwSetDropCallback w drop_cb))
 )

;;;
;;; Input
;;;

(define-cproc glfw-get-input-mode (w::<glfw-window> mode::<int>) ::<int>
  glfwGetInputMode)

(define-cproc glfw-set-input-mode (w::<glfw-window> mode::<int> value::<int>)
  ::<void>
  glfwSetInputMode)

(define-cproc glfw-get-key-name (key::<int> scancode::<int>) ::<const-cstring>
  glfwGetKeyName)

;; from 3.3
;; (define-cproc glfw-get-key-scancode (key::<int>) ::<int>
;;   glfwGetKeyScancode)

(define-cproc glfw-get-key (w::<glfw-window> key::<int>) ::<int>
  glfwGetKey)

(define-cproc glfw-get-mouse-button (w::<glfw-window> button::<int>) ::<int>
  glfwGetMouseButton)

(define-cproc glfw-get-cursor-pos (w::<glfw-window>) ::(<double> <double>)
  (let* ([xpos::double] [ypos::double])
    (glfwGetCursorPos w (& xpos) (& ypos))
    (return xpos ypos)))

(define-cproc glfw-set-cursor-pos (w::<glfw-window> xpos::<double> ypos::<double>)
  ::<void>
  glfwSetCursorPos)

;; CreateCursor - needs GLFWimage

(define-cproc glfw-create-standard-cursor (shape::<int>) ::<glfw-cursor>
  glfwCreateStandardCursor)

(define-cproc glfw-destroy-cursor (c) ::<void>
  (Scm_GlfwCursorDestroy c))

(define-cproc glfw-joystick-present (jid::<int>) ::<boolean>
  glfwJoystickPresent)

;; Returns f32vector.  If jid isn't present, returns #f.
;; You can optionally pass in a f32vector to receive the results, avoiding
;; avoid allocation.   The length of vector must match the number of axes,
;; which can be known if you call this once with vec being #f.
;; (In fact, you can reuse the first-returned vector for the subsequent calls)
(define-cproc glfw-get-joystick-axes (jid::<int> :optional (vec #f))
  (let* ([count::int]
         [fs::(const float*) (glfwGetJoystickAxes jid (& count))])
    (cond [(== fs NULL) (return '#f)]
          [(SCM_FALSEP vec) (return (Scm_MakeF32VectorFromArray count fs))]
          [(not (SCM_F32VECTORP vec)) (SCM_TYPE_ERROR vec "f32vector or #f")]
          [(not (== count (SCM_F32VECTOR_SIZE vec)))
           (Scm_Error "length of vec %S doesn't match the number of axes %d"
                      vec count)]
          [else
           (dotimes [i count]
             (set! (SCM_F32VECTOR_ELEMENT vec i) (aref fs i)))
           (return vec)])))

;; Returns u8vector.  Same as glfw-get-joystick-axes above.
(define-cproc glfw-get-joystick-buttons (jid::<int> :optional (vec #f))
  (let* ([count::int]
         [bs::(const u_char*) (glfwGetJoystickButtons jid (& count))])
    (cond [(== bs NULL) (return '#f)]
          [(SCM_FALSEP vec) (return (Scm_MakeU8VectorFromArray count bs))]
          [(not (SCM_U8VECTORP vec)) (SCM_TYPE_ERROR vec "u8vector or #f")]
          [(not (== count (SCM_U8VECTOR_SIZE vec)))
           (Scm_Error "length of vec %S doesn't match the number of axes %d"
                      vec count)]
          [else
           (dotimes [i count]
             (set! (SCM_U8VECTOR_ELEMENT vec i) (aref bs i)))
           (return vec)])))

;; glfw-get-joystick-hats - from 3.3

(define-cproc glfw-get-joystick-name (jid::<int>) ::<const-cstring>?
  glfwGetJoystickName)

;; from 3.3
;; (define-cproc glf-wget-joystick-guid (jid::<int>) ::<const-cstring>?
;;   glfwGetJoystickGUID)

;; glfwJoystickIsGamepad - from 3.3

;; glfwUpdateGamepadMappings - from 3.3

;; glfwGetGamepadName - from 3.3

;; glfwGetGamepadState - from 3.3

(define-cproc glfw-set-clipboard-string (w::<glfw-window>?
                                         s::<const-cstring>)
  ::<void>
  glfwSetClipboardString)

(define-cproc glfw-get-clipboard-string (w::<glfw-window>?) ::<const-cstring>?
  glfwGetClipboardString)

(define-cproc glfw-get-time () ::<double>
  glfwGetTime)

(define-cproc glfw-set-time (time::<double>) ::<void>
  glfwSetTime)

(define-cproc glfw-get-timer-value ()
  (return (Scm_MakeIntegerU64 (glfwGetTimerValue))))

(define-cproc glfw-get-timer-frequency ()
  (return (Scm_MakeIntegerU64 (glfwGetTimerFrequency))))


;;;
;;; Context
;;;

(define-cproc glfw-make-context-current (w::<glfw-window>) ::<void>
  (glfwMakeContextCurrent w)
  (.when (defined HAVE_GL_GLEW_H)
    (let* ([r::GLenum (glewInit)])
      (if (!= r GLEW_OK)
        (Scm_Error "Initializing GLEW failed.")))))

(define-cproc glfw-get-current-context () ::<glfw-window>?
  glfwGetCurrentContext)

(define-cproc glfw-swap-interval (interval::<int>) ::<void>
  glfwSwapInterval)

(define-cproc glfw-extension-supported (ext::<const-cstring>) ::<boolean>
  glfwExtensionSupported)

;;;
;;; Enum
;;;

(define-enum GLFW_VERSION_MAJOR)
(define-enum GLFW_VERSION_MINOR)
(define-enum GLFW_VERSION_REVISION)

;; window creation hint deesignators
(define-enum GLFW_FOCUSED)
(define-enum GLFW_ICONIFIED)
(define-enum GLFW_RESIZABLE)
(define-enum GLFW_VISIBLE)
(define-enum GLFW_DECORATED)
(define-enum GLFW_AUTO_ICONIFY)
(define-enum GLFW_FLOATING)
(define-enum GLFW_MAXIMIZED)

(define-enum GLFW_RED_BITS)
(define-enum GLFW_GREEN_BITS)
(define-enum GLFW_BLUE_BITS)
(define-enum GLFW_ALPHA_BITS)
(define-enum GLFW_DEPTH_BITS)
(define-enum GLFW_STENCIL_BITS)
(define-enum GLFW_ACCUM_RED_BITS)
(define-enum GLFW_ACCUM_GREEN_BITS)
(define-enum GLFW_ACCUM_BLUE_BITS)
(define-enum GLFW_ACCUM_ALPHA_BITS)
(define-enum GLFW_AUX_BUFFERS)
(define-enum GLFW_STEREO)
(define-enum GLFW_SAMPLES)
(define-enum GLFW_SRGB_CAPABLE)
(define-enum GLFW_REFRESH_RATE)
(define-enum GLFW_DOUBLEBUFFER)

(define-enum GLFW_CLIENT_API)
(define-enum GLFW_CONTEXT_VERSION_MAJOR)
(define-enum GLFW_CONTEXT_VERSION_MINOR)
(define-enum GLFW_CONTEXT_REVISION)
(define-enum GLFW_CONTEXT_ROBUSTNESS)
(define-enum GLFW_OPENGL_FORWARD_COMPAT)
(define-enum GLFW_OPENGL_DEBUG_CONTEXT)
(define-enum GLFW_OPENGL_PROFILE)
(define-enum GLFW_CONTEXT_RELEASE_BEHAVIOR)
(define-enum GLFW_CONTEXT_NO_ERROR)
(define-enum GLFW_CONTEXT_CREATION_API)

;; values
(define-enum GLFW_NO_API)
(define-enum GLFW_OPENGL_API)
(define-enum GLFW_OPENGL_ES_API)

(define-enum GLFW_NO_ROBUSTNESS)
(define-enum GLFW_NO_RESET_NOTIFICATION)
(define-enum GLFW_LOSE_CONTEXT_ON_RESET)

(define-enum GLFW_OPENGL_ANY_PROFILE)
(define-enum GLFW_OPENGL_CORE_PROFILE)
(define-enum GLFW_OPENGL_COMPAT_PROFILE)

(define-enum GLFW_CURSOR)
(define-enum GLFW_STICKY_KEYS)
(define-enum GLFW_STICKY_MOUSE_BUTTONS)

(define-enum GLFW_CURSOR_NORMAL)
(define-enum GLFW_CURSOR_HIDDEN)
(define-enum GLFW_CURSOR_DISABLED)

(define-enum GLFW_ANY_RELEASE_BEHAVIOR)
(define-enum GLFW_RELEASE_BEHAVIOR_FLUSH)
(define-enum GLFW_RELEASE_BEHAVIOR_NONE)

(define-enum GLFW_NATIVE_CONTEXT_API)
(define-enum GLFW_EGL_CONTEXT_API)

(define-enum GLFW_ARROW_CURSOR)
(define-enum GLFW_IBEAM_CURSOR)
(define-enum GLFW_CROSSHAIR_CURSOR)
(define-enum GLFW_HAND_CURSOR)
(define-enum GLFW_HRESIZE_CURSOR)
(define-enum GLFW_VRESIZE_CURSOR)

(define-enum GLFW_CONNECTED)
(define-enum GLFW_DISCONNECTED)

(define-enum GLFW_DONT_CARE)

;;
;; Keys
;;

(define-enum GLFW_RELEASE)
(define-enum GLFW_PRESS)
(define-enum GLFW_REPEAT)

(define-enum GLFW_KEY_UNKNOWN)
(define-enum GLFW_KEY_SPACE)
(define-enum GLFW_KEY_APOSTROPHE)
(define-enum GLFW_KEY_COMMA)
(define-enum GLFW_KEY_MINUS)
(define-enum GLFW_KEY_PERIOD)
(define-enum GLFW_KEY_SLASH)
(define-enum GLFW_KEY_0)
(define-enum GLFW_KEY_1)
(define-enum GLFW_KEY_2)
(define-enum GLFW_KEY_3)
(define-enum GLFW_KEY_4)
(define-enum GLFW_KEY_5)
(define-enum GLFW_KEY_6)
(define-enum GLFW_KEY_7)
(define-enum GLFW_KEY_8)
(define-enum GLFW_KEY_9)
(define-enum GLFW_KEY_SEMICOLON)
(define-enum GLFW_KEY_EQUAL)
(define-enum GLFW_KEY_A)
(define-enum GLFW_KEY_B)
(define-enum GLFW_KEY_C)
(define-enum GLFW_KEY_D)
(define-enum GLFW_KEY_E)
(define-enum GLFW_KEY_F)
(define-enum GLFW_KEY_G)
(define-enum GLFW_KEY_H)
(define-enum GLFW_KEY_I)
(define-enum GLFW_KEY_J)
(define-enum GLFW_KEY_K)
(define-enum GLFW_KEY_L)
(define-enum GLFW_KEY_M)
(define-enum GLFW_KEY_N)
(define-enum GLFW_KEY_O)
(define-enum GLFW_KEY_P)
(define-enum GLFW_KEY_Q)
(define-enum GLFW_KEY_R)
(define-enum GLFW_KEY_S)
(define-enum GLFW_KEY_T)
(define-enum GLFW_KEY_U)
(define-enum GLFW_KEY_V)
(define-enum GLFW_KEY_W)
(define-enum GLFW_KEY_X)
(define-enum GLFW_KEY_Y)
(define-enum GLFW_KEY_Z)
(define-enum GLFW_KEY_LEFT_BRACKET)
(define-enum GLFW_KEY_BACKSLASH)
(define-enum GLFW_KEY_RIGHT_BRACKET)
(define-enum GLFW_KEY_GRAVE_ACCENT)
(define-enum GLFW_KEY_WORLD_1)
(define-enum GLFW_KEY_WORLD_2)
(define-enum GLFW_KEY_ESCAPE)
(define-enum GLFW_KEY_ENTER)
(define-enum GLFW_KEY_TAB)
(define-enum GLFW_KEY_BACKSPACE)
(define-enum GLFW_KEY_INSERT)
(define-enum GLFW_KEY_DELETE)
(define-enum GLFW_KEY_RIGHT)
(define-enum GLFW_KEY_LEFT)
(define-enum GLFW_KEY_DOWN)
(define-enum GLFW_KEY_UP)
(define-enum GLFW_KEY_PAGE_UP)
(define-enum GLFW_KEY_PAGE_DOWN)
(define-enum GLFW_KEY_HOME)
(define-enum GLFW_KEY_END)
(define-enum GLFW_KEY_CAPS_LOCK)
(define-enum GLFW_KEY_SCROLL_LOCK)
(define-enum GLFW_KEY_NUM_LOCK)
(define-enum GLFW_KEY_PRINT_SCREEN)
(define-enum GLFW_KEY_PAUSE)
(define-enum GLFW_KEY_F1)
(define-enum GLFW_KEY_F2)
(define-enum GLFW_KEY_F3)
(define-enum GLFW_KEY_F4)
(define-enum GLFW_KEY_F5)
(define-enum GLFW_KEY_F6)
(define-enum GLFW_KEY_F7)
(define-enum GLFW_KEY_F8)
(define-enum GLFW_KEY_F9)
(define-enum GLFW_KEY_F10)
(define-enum GLFW_KEY_F11)
(define-enum GLFW_KEY_F12)
(define-enum GLFW_KEY_F13)
(define-enum GLFW_KEY_F14)
(define-enum GLFW_KEY_F15)
(define-enum GLFW_KEY_F16)
(define-enum GLFW_KEY_F17)
(define-enum GLFW_KEY_F18)
(define-enum GLFW_KEY_F19)
(define-enum GLFW_KEY_F20)
(define-enum GLFW_KEY_F21)
(define-enum GLFW_KEY_F22)
(define-enum GLFW_KEY_F23)
(define-enum GLFW_KEY_F24)
(define-enum GLFW_KEY_F25)
(define-enum GLFW_KEY_KP_0)
(define-enum GLFW_KEY_KP_1)
(define-enum GLFW_KEY_KP_2)
(define-enum GLFW_KEY_KP_3)
(define-enum GLFW_KEY_KP_4)
(define-enum GLFW_KEY_KP_5)
(define-enum GLFW_KEY_KP_6)
(define-enum GLFW_KEY_KP_7)
(define-enum GLFW_KEY_KP_8)
(define-enum GLFW_KEY_KP_9)
(define-enum GLFW_KEY_KP_DECIMAL)
(define-enum GLFW_KEY_KP_DIVIDE)
(define-enum GLFW_KEY_KP_MULTIPLY)
(define-enum GLFW_KEY_KP_SUBTRACT)
(define-enum GLFW_KEY_KP_ADD)
(define-enum GLFW_KEY_KP_ENTER)
(define-enum GLFW_KEY_KP_EQUAL)
(define-enum GLFW_KEY_LEFT_SHIFT)
(define-enum GLFW_KEY_LEFT_CONTROL)
(define-enum GLFW_KEY_LEFT_ALT)
(define-enum GLFW_KEY_LEFT_SUPER)
(define-enum GLFW_KEY_RIGHT_SHIFT)
(define-enum GLFW_KEY_RIGHT_CONTROL)
(define-enum GLFW_KEY_RIGHT_ALT)
(define-enum GLFW_KEY_RIGHT_SUPER)
(define-enum GLFW_KEY_MENU)
(define-enum GLFW_KEY_LAST)

(define-enum GLFW_MOD_SHIFT)
(define-enum GLFW_MOD_CONTROL)
(define-enum GLFW_MOD_ALT)
(define-enum GLFW_MOD_SUPER)

(define-enum GLFW_MOUSE_BUTTON_1)
(define-enum GLFW_MOUSE_BUTTON_2)
(define-enum GLFW_MOUSE_BUTTON_3)
(define-enum GLFW_MOUSE_BUTTON_4)
(define-enum GLFW_MOUSE_BUTTON_5)
(define-enum GLFW_MOUSE_BUTTON_6)
(define-enum GLFW_MOUSE_BUTTON_7)
(define-enum GLFW_MOUSE_BUTTON_8)
(define-enum GLFW_MOUSE_BUTTON_LAST)
(define-enum GLFW_MOUSE_BUTTON_LEFT)
(define-enum GLFW_MOUSE_BUTTON_RIGHT)
(define-enum GLFW_MOUSE_BUTTON_MIDDLE)

(define-enum GLFW_JOYSTICK_1)
(define-enum GLFW_JOYSTICK_2)
(define-enum GLFW_JOYSTICK_3)
(define-enum GLFW_JOYSTICK_4)
(define-enum GLFW_JOYSTICK_5)
(define-enum GLFW_JOYSTICK_6)
(define-enum GLFW_JOYSTICK_7)
(define-enum GLFW_JOYSTICK_8)
(define-enum GLFW_JOYSTICK_9)
(define-enum GLFW_JOYSTICK_10)
(define-enum GLFW_JOYSTICK_11)
(define-enum GLFW_JOYSTICK_12)
(define-enum GLFW_JOYSTICK_13)
(define-enum GLFW_JOYSTICK_14)
(define-enum GLFW_JOYSTICK_15)
(define-enum GLFW_JOYSTICK_16)
(define-enum GLFW_JOYSTICK_LAST)
