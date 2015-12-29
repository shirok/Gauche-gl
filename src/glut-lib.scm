;;;
;;; glut-lib.scm - glue functions for GLUT
;;;
;;;  Copyright (c) 2001-2015  Shiro Kawai  <shiro@acm.org>
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

(select-module gl.glut)

(inline-stub

 (declcode "#include \"gauche-gl.h\""
           "#ifdef HAVE_GLUT_GLUT_H"
           "#include <GLUT/glut.h>"
           "#else"
           "#include <GL/glut.h>"
           "#endif"
           "#include \"gauche-glut.h\"")

(include "glcase.scm")

;;========================================================
;; Window manipulation
;;

;; glut-init
;;   Takes list of args instead of C-style argc/argv, and returns
;;   (possibly modified) args.

(define-cproc glut-init (args)
  (let* ([argc::int (Scm_Length args)]
         [argv::char**])
    (when (< argc 0) (SCM_TYPE_ERROR args "list"))
    (set! argv (Scm_ListToCStringArray args TRUE NULL))
    (glutInit (& argc) argv)
    ;; When using GLEW, 'glewInit' must be called after 'glutCreateWindow'
    ;; to use OpenGL extensions such as 'glTexImage3D'.
    ;; Thus, the following code was moved to 'glut-create-window'.
    ;(.if "defined(HAVE_GL_GLEW_H)"
    ;     (let* ([r::GLenum (glewInit)])
    ;       (if (!= r GLEW_OK)
    ;         (Scm_Error "Initializing GLEW failed."))))
    (result (Scm_CStringArrayToList (cast (const char**) argv) argc 0))))

;; For debugging
(define-cproc glew-init () ::<void>
  (.if "defined(HAVE_GL_GLEW_H)"
       (let* ([r::GLenum (glewInit)])
         (if (!= r GLEW_OK)
           (Scm_Error "Initializing GLEW failed.")))))

(define-cproc glut-init-display-mode (mode::<fixnum>)
  ::<void> glutInitDisplayMode)

(if "!(GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)"
  "#define glutInitDisplayString(x) /*ignore*/")
  
(define-cproc glut-init-display-string (string::<const-cstring>)
  ::<void> glutInitDisplayString)

(define-cproc glut-init-window-size (width::<int> height::<int>)
  ::<void> glutInitWindowSize)

(define-cproc glut-init-window-position (x::<int> y::<int>)
  ::<void> glutInitWindowPosition)

(define-cproc glut-main-loop () ::<void> glutMainLoop)

(define-cproc glut-create-window (name::<const-cstring>) ::<int>
  (let* ([r1::int (glutCreateWindow name)])
    (.if "defined(HAVE_GL_GLEW_H)"
         (let* ([r::GLenum (glewInit)])
           (if (!= r GLEW_OK)
             (Scm_Error "Initializing GLEW failed."))))
    (result r1)))

(define-cproc glut-create-sub-window (win::<int> x::<int> y::<int>
                                      width::<int> height::<int>)
  ::<int> glutCreateSubWindow)

(define-cproc glut-destroy-window (win::<int>)
  ::<void> glutDestroyWindow)

(define-cproc glut-post-redisplay ()
  ::<void> glutPostRedisplay)

(if "!(GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 11)"
  "#define glutPostWindowRedisplay(win) /*ignore*/")

(define-cproc glut-post-window-redisplay (win::<int>)
  ::<void> glutPostWindowRedisplay)

(define-cproc glut-swap-buffers () ::<void> glutSwapBuffers)

(define-cproc glut-get-window () ::<int> glutGetWindow)

(define-cproc glut-set-window (win::<int>) ::<void> glutSetWindow)

(define-cproc glut-set-window-title (title::<const-cstring>)
  ::<void> glutSetWindowTitle)

(define-cproc glut-set-icon-title (title::<const-cstring>)
  ::<void> glutSetIconTitle)

(define-cproc glut-position-window (x::<int> y::<int>)
  ::<void> glutPositionWindow)

(define-cproc glut-reshape-window (width::<int> height::<int>)
  ::<void> glutReshapeWindow)

(define-cproc glut-push-window () ::<void> glutPushWindow)
(define-cproc glut-pop-window () ::<void> glutPopWindow)
(define-cproc glut-iconify-window () ::<void> glutIconifyWindow)
(define-cproc glut-show-window () ::<void> glutShowWindow)
(define-cproc glut-hide-window () ::<void> glutHideWindow)

(if "!(GLUT_API_VERSION >= 3)"
  "#define glutFullScreen(x) /*ignore*/")

(define-cproc glut-full-screen () ::<void> glutFullScreen)

(if "!(GLUT_API_VERSION >= 3)"
  "#define glutSetCursor(x) /*ignore*/")

(define-cproc glut-set-cursor (cursor::<int>) ::<void> glutSetCursor)

(if "!(GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)"
  "#define glutWarpPointer(x, y) /*ignore*/")

(define-cproc glut-warp-pointer (x::<int> y::<int>) ::<void> glutWarpPointer)

;;========================================================
;; Overlay APIs
;;

(define-cproc glut-establish-overlay ()
  "#if (GLUT_API_VERSION >= 3)
   glutEstablishOverlay();
#endif
   SCM_RETURN(SCM_UNDEFINED);")

(define-cproc glut-remove-overlay ()
  "#if (GLUT_API_VERSION >= 3)
   glutRemoveOverlay();
#endif
   SCM_RETURN(SCM_UNDEFINED);")

(define-cproc glut-use-layer (layer::<fixnum>)
  "#if (GLUT_API_VERSION >= 3)
   glutUseLayer(layer);
#endif
   SCM_RETURN(SCM_UNDEFINED);")

(define-cproc glut-post-overlay-redisplay ()
  "#if (GLUT_API_VERSION >= 3)
   glutPostOverlayRedisplay();
#endif
   SCM_RETURN(SCM_UNDEFINED);")

(define-cproc glut-post-window-overlay-redisplay (win::<int>)
  "#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 11)
   glutPostWindowOverlayRedisplay(win);
#endif
   SCM_RETURN(SCM_UNDEFINED);")

(define-cproc glut-show-overlay ()
  "#if (GLUT_API_VERSION >= 3)
   glutShowOverlay();
#endif
   SCM_RETURN(SCM_UNDEFINED);")
  
(define-cproc glut-hide-overlay ()
  "#if (GLUT_API_VERSION >= 3)
   glutHideOverlay();
#endif
   SCM_RETURN(SCM_UNDEFINED);")

;;========================================================
;; Menu APIs
;;

;; Glut menu API doesn't passes the callback function which menu
;; it is invoked.  We need to keep an assoc list of menu id and
;; callback fn.

"static ScmObj menu_fn_assoc = SCM_NIL;"

(define-cfn menu-callback (item::int) ::void
  (let* ([menu::int (glutGetMenu)]
         [p (Scm_Assoc (SCM_MAKE_INT menu) menu_fn_assoc SCM_CMP_EQV)])
    (when (and (SCM_PAIRP p) (SCM_PROCEDUREP (SCM_CDR p)))
      (Scm_ApplyRec (SCM_CDR p) (SCM_LIST1 (SCM_MAKE_INT item))))))

(define-cproc glut-create-menu (callback::<procedure>)
  (let* ([menu::int (glutCreateMenu menu_callback)])
    (if (>= menu 0)
      (begin (set! menu_fn_assoc (Scm_Acons (SCM_MAKE_INT menu)
                                            (SCM_OBJ callback)
                                            menu_fn_assoc))
             (result (SCM_MAKE_INT menu)))
      (result '#f))))

(define-cproc glut-destroy-menu (menu::<fixnum>) ::<void>
  (set! menu_fn_assoc
        (Scm_AssocDelete (SCM_MAKE_INT menu) menu_fn_assoc SCM_CMP_EQV))
  (glutDestroyMenu menu))

(define-cproc glut-get-menu () ::<int> glutGetMenu)
(define-cproc glut-set-menu (menu::<fixnum>)::<void> glutSetMenu)

(define-cproc glut-add-menu-entry (label::<const-cstring> value::<int>)::<void>
  glutAddMenuEntry)
(define-cproc glut-add-sub-menu (label::<const-cstring> submenu::<int>)::<void>
  glutAddSubMenu)
(define-cproc glut-change-to-menu-entry (item::<int> label::<const-cstring>
                                         value::<int>)
  ::<void> glutChangeToMenuEntry)
(define-cproc glut-change-to-sub-menu (item::<int> label::<const-cstring>
                                       submenu::<int>)
  ::<void> glutChangeToSubMenu)

(define-cproc glut-remove-menu-item (item::<int>) ::<void> glutRemoveMenuItem)
(define-cproc glut-attach-menu (button::<int>) ::<void> glutAttachMenu)
(define-cproc glut-detach-menu (button::<int>) ::<void> glutDetachMenu)

;;========================================================
;; Callbacks
;;

;; Most Glut callbacks are associated to the "current window".
;; Scm_GlutRegisterCallback handles the association.

(define-cproc glut-display-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_DISPLAY fn 0 0))
(define-cproc glut-overlay-display-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_OVERLAY_DISPLAY fn 0 0))
(define-cproc glut-reshape-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_RESHAPE fn 0 0))
(define-cproc glut-keyboard-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_KEYBOARD fn 0 0))
(define-cproc glut-keyboard-up-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_KEYBOARD_UP fn 0 0))
(define-cproc glut-mouse-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_MOUSE fn 0 0))
(define-cproc glut-motion-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_MOTION fn 0 0))
(define-cproc glut-passive-motion-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_PASSIVE_MOTION fn 0 0))
(define-cproc glut-visibility-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_VISIBILITY fn 0 0))
(define-cproc glut-entry-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_ENTRY fn 0 0))
(define-cproc glut-special-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_SPECIAL fn 0 0))
(define-cproc glut-special-up-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_SPECIAL_UP fn 0 0))
(define-cproc glut-spaceball-motion-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_SPACEBALL_MOTION fn 0 0))
(define-cproc glut-spaceball-rotate-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_SPACEBALL_ROTATE fn 0 0))
(define-cproc glut-spaceball-button-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_SPACEBALL_BUTTON fn 0 0))
(define-cproc glut-button-box-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_BUTTON_BOX fn 0 0))
(define-cproc glut-dials (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_DIALS fn 0 0))
(define-cproc glut-tablet-motion-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_TABLET_MOTION fn 0 0))
(define-cproc glut-tablet-button-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_TABLET_BUTTON fn 0 0))
(define-cproc glut-menu-status (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_MENU_STATUS fn 0 0))
(define-cproc glut-window-status (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_WINDOW_STATUS fn 0 0))
(define-cproc glut-joystick-func (fn interval::<int>) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_JOYSTICK fn interval 0))


(define-cproc glut-idle-func (fn) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_IDLE fn 0 0))
(define-cproc glut-timer-func (millis::<int> fn value::<int>) ::<void>
  (Scm_GlutRegisterCallback SCM_GLUT_CB_TIMER fn millis value))


;;========================================================
;; Colormap
;;

(define-cproc glut-set-color (index::<int> r::<float> g::<float> b::<float>)
  ::<void> glutSetColor)

(define-cproc glut-get-color (index::<int> component::<fixnum>)
  ::<double> (result (cast double (glutGetColor index component))))

(define-cproc glut-copy-colormap (win::<int>) ::<void> glutCopyColormap)

;;========================================================
;; state retrieval
;;

(define-cproc glut-get (type::<int>) ::<int> glutGet)

(define-cproc glut-device-get (type::<int>) ::<int> glutDeviceGet)

(if "!(GLUT_API_VERSION >= 2)"
  "#define glutExtensionSupported(x) FALSE")

(define-cproc glut-extension-supported (name::<const-cstring>) ::<boolean>
  glutExtensionSupported)

(if "!(GLUT_API_VERSION >= 3)"
  "#define glutGetModifiers() 0")

(define-cproc glut-get-modifiers () ::<int> glutGetModifiers)

(if "!(GLUT_API_VERSION >= 3)"
  "#define glutLayerGet(x) 0")

(define-cproc glut-layer-get (type::<int>) ::<int> glutLayerGet)

;;========================================================
;; Font
;;

(define-type <glut-font> "ScmGlutFont*" #f
  "SCM_GLUT_FONT_P" "SCM_GLUT_FONT")

(define-cproc glut-bitmap-character (font::<glut-font> character::<int>)
  ::<void> (glutBitmapCharacter (-> font font) character))

(define-cproc glut-bitmap-width (font::<glut-font> character::<int>) ::<int>
  (result (glutBitmapWidth (-> font font) character)))

(define-cproc glut-stroke-character (font::<glut-font> character::<int>)
  ::<void> (glutStrokeCharacter (-> font font) character))

(define-cproc glut-stroke-width (font::<glut-font> character::<int>)
  ::<int> (result (glutStrokeWidth (-> font font) character)))

(if "!(GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)"
  "#define glutBitmapLength(x, y) 0")

(define-cproc glut-bitmap-length (font::<glut-font> string::<const-cstring>)
  ::<int> (result (glutBitmapLength (-> font font)
                                    (cast (const unsigned char*) string))))

(if "!(GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)"
  "#define glutStrokeLength(x, y) 0")

(define-cproc glut-stroke-length (font::<glut-font> string::<const-cstring>)
  ::<int> (result (glutStrokeLength (-> font font)
                                    (cast (const unsigned char*) string))))

;;========================================================
;; pre-built models
;;

(define-cproc glut-wire-sphere (radius::<real> slices::<int> stacks::<int>)
  ::<void> glutWireSphere)
(define-cproc glut-solid-sphere (radius::<real> slices::<int> stacks::<int>)
  ::<void> glutSolidSphere)

(define-cproc glut-wire-cone (radius::<real> height::<real>
                              slices::<int> stacks::<int>)
  ::<void> glutWireCone)
(define-cproc glut-solid-cone (radius::<real> height::<real>
                               slices::<int> stacks::<int>)
  ::<void> glutSolidCone)

(define-cproc glut-wire-cube (size::<real>) ::<void> glutWireCube)
(define-cproc glut-solid-cube (size::<real>) ::<void> glutSolidCube)

(define-cproc glut-wire-torus (inner::<real> outer::<real>
                               sides::<int> rings::<int>)
  ::<void> glutWireTorus)
(define-cproc glut-solid-torus (inner::<real> outer::<real>
                                sides::<int> rings::<int>)
  ::<void> glutSolidTorus)

(define-cproc glut-wire-dodecahedron () ::<void> glutWireDodecahedron)
(define-cproc glut-solid-dodecahedron () ::<void> glutSolidDodecahedron)

(define-cproc glut-wire-teapot (size::<real>) ::<void> glutWireTeapot)
(define-cproc glut-solid-teapot (size::<real>) ::<void> glutSolidTeapot)

(define-cproc glut-wire-octahedron () ::<void> glutWireOctahedron)
(define-cproc glut-solid-octahedron () ::<void> glutSolidOctahedron)

(define-cproc glut-wire-tetrahedron () ::<void> glutWireTetrahedron)
(define-cproc glut-solid-tetrahedron () ::<void> glutSolidTetrahedron)

(define-cproc glut-wire-icosahedron () ::<void> glutWireIcosahedron)
(define-cproc glut-solid-icosahedron () ::<void> glutSolidIcosahedron)

;;========================================================
;; Video resize
;;

;;========================================================
;; Debug
;;

;;========================================================
;; Device control
;;

;;========================================================
;; Constants
;;

;; Display mode bit masks
(define-enum GLUT_RGB)
(define-enum GLUT_RGBA)
(define-enum GLUT_INDEX)
(define-enum GLUT_SINGLE)
(define-enum GLUT_DOUBLE)
(define-enum GLUT_ACCUM)
(define-enum GLUT_ALPHA)
(define-enum GLUT_DEPTH)
(define-enum GLUT_STENCIL)
(if "(GLUT_API_VERSION >= 2)"
    (begin
      (define-enum GLUT_MULTISAMPLE)
      (define-enum GLUT_STEREO)
      ))
(if "(GLUT_API_VERSION >= 3)"
    (define-enum GLUT_LUMINANCE))

;; Mouse
(define-enum GLUT_LEFT_BUTTON)
(define-enum GLUT_MIDDLE_BUTTON)
(define-enum GLUT_RIGHT_BUTTON)
(define-enum GLUT_DOWN)
(define-enum GLUT_UP)

;; Function Keys
(if "(GLUT_API_VERSION >= 2)"
    (begin
      (define-enum GLUT_KEY_F1)
      (define-enum GLUT_KEY_F2)
      (define-enum GLUT_KEY_F3)
      (define-enum GLUT_KEY_F4)
      (define-enum GLUT_KEY_F5)
      (define-enum GLUT_KEY_F6)
      (define-enum GLUT_KEY_F7)
      (define-enum GLUT_KEY_F8)
      (define-enum GLUT_KEY_F9)
      (define-enum GLUT_KEY_F10)
      (define-enum GLUT_KEY_F11)
      (define-enum GLUT_KEY_F12)
      (define-enum GLUT_KEY_LEFT)
      (define-enum GLUT_KEY_UP)
      (define-enum GLUT_KEY_RIGHT)
      (define-enum GLUT_KEY_DOWN)
      (define-enum GLUT_KEY_PAGE_UP)
      (define-enum GLUT_KEY_PAGE_DOWN)
      (define-enum GLUT_KEY_HOME)
      (define-enum GLUT_KEY_END)
      (define-enum GLUT_KEY_INSERT)
      ))

;; Entry/exit  state.
(define-enum GLUT_LEFT)
(define-enum GLUT_ENTERED)

;; Menu usage  state.
(define-enum GLUT_MENU_NOT_IN_USE)
(define-enum GLUT_MENU_IN_USE)

;; Visibility  state.
(define-enum GLUT_NOT_VISIBLE)
(define-enum GLUT_VISIBLE)

;; Window status  state.
(define-enum GLUT_HIDDEN)
(define-enum GLUT_FULLY_RETAINED)
(define-enum GLUT_PARTIALLY_RETAINED)
(define-enum GLUT_FULLY_COVERED)

;; Color index component selection values
(define-enum GLUT_RED)
(define-enum GLUT_GREEN)
(define-enum GLUT_BLUE)

;; Layers for use
(define-enum GLUT_NORMAL)
(define-enum GLUT_OVERLAY)

;; glutGet parameters
(define-enum GLUT_WINDOW_X)
(define-enum GLUT_WINDOW_Y)
(define-enum GLUT_WINDOW_WIDTH)
(define-enum GLUT_WINDOW_HEIGHT)
(define-enum GLUT_WINDOW_BUFFER_SIZE)
(define-enum GLUT_WINDOW_STENCIL_SIZE)
(define-enum GLUT_WINDOW_DEPTH_SIZE)
(define-enum GLUT_WINDOW_RED_SIZE)
(define-enum GLUT_WINDOW_GREEN_SIZE)
(define-enum GLUT_WINDOW_BLUE_SIZE)
(define-enum GLUT_WINDOW_ALPHA_SIZE)
(define-enum GLUT_WINDOW_ACCUM_RED_SIZE)
(define-enum GLUT_WINDOW_ACCUM_GREEN_SIZE)
(define-enum GLUT_WINDOW_ACCUM_BLUE_SIZE)
(define-enum GLUT_WINDOW_ACCUM_ALPHA_SIZE)
(define-enum GLUT_WINDOW_DOUBLEBUFFER)
(define-enum GLUT_WINDOW_RGBA)
(define-enum GLUT_WINDOW_PARENT)
(define-enum GLUT_WINDOW_NUM_CHILDREN)
(define-enum GLUT_WINDOW_COLORMAP_SIZE)
(if "(GLUT_API_VERSION >= 2)"
    (begin
      (define-enum GLUT_WINDOW_NUM_SAMPLES)
      (define-enum GLUT_WINDOW_STEREO)
      ))
(if "(GLUT_API_VERSION >= 3)"
    (begin
      (define-enum GLUT_WINDOW_CURSOR)
      ))
(define-enum GLUT_SCREEN_WIDTH)
(define-enum GLUT_SCREEN_HEIGHT)
(define-enum GLUT_SCREEN_WIDTH_MM)
(define-enum GLUT_SCREEN_HEIGHT_MM)
(define-enum GLUT_MENU_NUM_ITEMS)
(define-enum GLUT_DISPLAY_MODE_POSSIBLE)
(define-enum GLUT_INIT_WINDOW_X)
(define-enum GLUT_INIT_WINDOW_Y)
(define-enum GLUT_INIT_WINDOW_WIDTH)
(define-enum GLUT_INIT_WINDOW_HEIGHT)
(define-enum GLUT_INIT_DISPLAY_MODE)
(if "(GLUT_API_VERSION >= 2)"
    (define-enum GLUT_ELAPSED_TIME)
    )
(if "(GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)"
    (define-enum GLUT_WINDOW_FORMAT_ID)
    )

;; glutDeviceGet parameters
(if "(GLUT_API_VERSION >= 2)"
    (begin
      (define-enum GLUT_HAS_KEYBOARD)
      (define-enum GLUT_HAS_MOUSE)
      (define-enum GLUT_HAS_SPACEBALL)
      (define-enum GLUT_HAS_DIAL_AND_BUTTON_BOX)
      (define-enum GLUT_HAS_TABLET)
      (define-enum GLUT_NUM_MOUSE_BUTTONS)
      (define-enum GLUT_NUM_SPACEBALL_BUTTONS)
      (define-enum GLUT_NUM_BUTTON_BOX_BUTTONS)
      (define-enum GLUT_NUM_DIALS)
      (define-enum GLUT_NUM_TABLET_BUTTONS)
      ))
(if "(GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)"
    (begin
      (define-enum GLUT_DEVICE_IGNORE_KEY_REPEAT)
      (define-enum GLUT_DEVICE_KEY_REPEAT)
      (define-enum GLUT_HAS_JOYSTICK)
      (define-enum GLUT_OWNS_JOYSTICK)
      (define-enum GLUT_JOYSTICK_BUTTONS)
      (define-enum GLUT_JOYSTICK_AXES)
      (define-enum GLUT_JOYSTICK_POLL_RATE)
      ))

;; glutLayerGet parameters.
(if "(GLUT_API_VERSION >= 3)"
    (begin
      (define-enum GLUT_OVERLAY_POSSIBLE)
      (define-enum GLUT_LAYER_IN_USE)
      (define-enum GLUT_HAS_OVERLAY)
      (define-enum GLUT_TRANSPARENT_INDEX)
      (define-enum GLUT_NORMAL_DAMAGED)
      (define-enum GLUT_OVERLAY_DAMAGED)
      ))

;; glutVideoResizeGet parameters
(if "(GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)"
    (begin
      (define-enum GLUT_VIDEO_RESIZE_POSSIBLE)
      (define-enum GLUT_VIDEO_RESIZE_IN_USE)
      (define-enum GLUT_VIDEO_RESIZE_X_DELTA)
      (define-enum GLUT_VIDEO_RESIZE_Y_DELTA)
      (define-enum GLUT_VIDEO_RESIZE_WIDTH_DELTA)
      (define-enum GLUT_VIDEO_RESIZE_HEIGHT_DELTA)
      (define-enum GLUT_VIDEO_RESIZE_X)
      (define-enum GLUT_VIDEO_RESIZE_Y)
      (define-enum GLUT_VIDEO_RESIZE_WIDTH)
      (define-enum GLUT_VIDEO_RESIZE_HEIGHT)
      ))

;; glutGetModifiers return mask
(if "(GLUT_API_VERSION >= 3)"
    (begin
      (define-enum GLUT_ACTIVE_SHIFT)
      (define-enum GLUT_ACTIVE_CTRL)
      (define-enum GLUT_ACTIVE_ALT)
      ))

;; glutSetCursor parameters
(if "(GLUT_API_VERSION >= 3)"
    (begin
      ;;  Basic arrows
      (define-enum GLUT_CURSOR_RIGHT_ARROW)
      (define-enum GLUT_CURSOR_LEFT_ARROW)
      ;;  Symbolic cursor shapes
      (define-enum GLUT_CURSOR_INFO)
      (define-enum GLUT_CURSOR_DESTROY)
      (define-enum GLUT_CURSOR_HELP)
      (define-enum GLUT_CURSOR_CYCLE)
      (define-enum GLUT_CURSOR_SPRAY)
      (define-enum GLUT_CURSOR_WAIT)
      (define-enum GLUT_CURSOR_TEXT)
      (define-enum GLUT_CURSOR_CROSSHAIR)
      ;;   Directional cursors
      (define-enum GLUT_CURSOR_UP_DOWN)
      (define-enum GLUT_CURSOR_LEFT_RIGHT)
      ;;   Sizing cursors
      (define-enum GLUT_CURSOR_TOP_SIDE)
      (define-enum GLUT_CURSOR_BOTTOM_SIDE)
      (define-enum GLUT_CURSOR_LEFT_SIDE)
      (define-enum GLUT_CURSOR_RIGHT_SIDE)
      (define-enum GLUT_CURSOR_TOP_LEFT_CORNER)
      (define-enum GLUT_CURSOR_TOP_RIGHT_CORNER)
      (define-enum GLUT_CURSOR_BOTTOM_RIGHT_CORNER)
      (define-enum GLUT_CURSOR_BOTTOM_LEFT_CORNER)
      ;;   Inherit from parent window
      (define-enum GLUT_CURSOR_INHERIT)
      ;;   Blank cursor
      (define-enum GLUT_CURSOR_NONE)
      ;;   Fullscreen crosshair (if available)
      (define-enum GLUT_CURSOR_FULL_CROSSHAIR)
      ))

) ;; end inline-stub

;; Local variables:
;; mode: scheme
;; end:
