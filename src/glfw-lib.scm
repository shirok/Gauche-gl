;;;
;;; glfw-lib.scm - glfw functions for GLUT
;;;
;;;  Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
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
 )

;;
;; Initialization and shutdown
;;

(define-cproc glfw-init () ::<boolean>
  (return (glfwInit)))

(define-cproc glfw-terminate () ::<void>
  (glfwTerminate))

;;
;; Monitor
;;

(define-cproc glfw-get-primary-monitor () ::<glfw-monitor>
  glfwGetPrimaryMonitor)

;;
;; Window
;;

(define-cproc glfw-create-window (w::<int>
                                  h::<int>
                                  title::<const-cstring>)
  ::<glfw-window>
  (return (glfwCreateWindow w h title NULL NULL)))

(define-cproc glfw-destroy-window (w) ::<void>
  (Scm_GlfwWindowDestroy w))

(define-cproc glfw-window-hint (hint::<int> value::<int>) ::<void>
  (glfwWindowHint hint value))
;; since 3.3
;; (define-cproc glfw-window-hint-string (hint::<int> value::<const-cstring>)
;;   ::<void>
;;   (glfwWindowHintString hint value))

(define-cproc glfw-default-window-hints () ::<void>
  (glfwDefaultWindowHints))

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
    (glfwGetWindowPos w (& width) (& height))
    (return width height)))

(define-cproc glfw-set-window-size-limits (w::<glfw-window>
                                           min-w::<int>
                                           min-h::<int>
                                           max-w::<int>
                                           max-h::<int>)
  ::<void>
  glfwSetWindowSizeLimits)

(define-cproc glfw-set-window-size (w::<glfw-window>
                                    width::<int>
                                    height::<int>)
  ::<void>
  glfwSetWindowSize)

(define-cproc glfw-set-window-aspect-ration (w::<glfw-window>
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
;;   ::(<real> <real>)
;;   (let* ([xscale::float][yscale::float])
;;     (glfwGetWindowContentScale w (& xscale) (& yscale))
;;     (return xscale yscale)))

;; from 3.3
;; (define-cproc glfw-get-window-opacity (w::<glfw-window>) ::<real>
;;   glfwGetWindowOpacity)

;; from 3.3
;; (define-cproc glfw-set-window-opaticy (w::<glfw-window> 
;;                                        opacity::<real>)
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
;; Key codes
;;

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
