;;;
;;; glut.scm - Gauche GLUT binding
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: glut.scm,v 1.1 2001-09-29 20:54:16 shirok Exp $
;;;

(define-module gl.glut
  (export glut-init glut-init-display-mode glut-init-display-string
          glut-init-window-size glut-init-window-position glut-main-loop
          glut-create-window glut-create-sub-window glut-destory-window
          glut-swap-buffers glut-get-window glut-set-window
          glut-set-window-title glut-set-icon-title glut-position-window
          glut-reshape-window glut-push-window glut-pop-window
          glut-iconify-window glut-show-window glut-hide-window
          glut-full-screen glut-set-cursor glut-warp-pointer
          glut-establish-overlay glut-remove-overlay glut-use-layer
          glut-post-overlay-redisplay glut-post-window-overlay-redisplay
          glut-show-overlay glut-hide-overlay glut-create-menu
          glut-destroy-menu glut-get-menu glut-set-menu
          glut-add-menu-entry glut-add-sub-menu glut-change-to-menu-entry
          glut-change-to-sub-menu glut-remove-menu-item
          glut-attach-menu glut-detach-menu glut-display-func
          glut-reshape-func glut-keyboard-func glut-mouse-func
          glut-motion-func glut-passive-motion-func glut-entry-func
          glut-visibility-func glut-idle-func glut-timer-func
          glut-menu-state-func glut-special-func glut-spaceball-motion-func
          glut-spaceball-rotate-func glut-spaceball-button-func
          glut-button-box-func glut-dials-func glut-tablet-motion-func
          glut-tablet-button-func glut-menu-status-func
          glut-ovelay-display-func glut-window-status-func
          glut-keyboard-up-func glut-special-up-func glut-joystick-func
          glut-set-color glut-get-color glut-copy-colormap glut-get
          glut-device-get glut-extension-supported glut-get-modifiers
          glut-layer-get glut-bitmap-character glut-bitmap-width
          glut-stroke-character glut-stroke-width glut-bitmap-length
          glut-stroke-length glut-wire-sphere glut-solid-sphere
          glut-wire-cone glut-solid-cone glut-wire-cube glut-solid-cube
          glut-wire-torus glut-solid-torus glut-wire-dodecahedron
          glut-solid-dodecahedron glut-wire-teapot glut-solid-teapot
          glut-wire-octahedron glut-solid-octahedron glut-wire-tetrahedron
          glut-solid-tetrahedron glut-wire-icosahedron
          glut-solid-icosahedron
          
          |GLUT_RGB| |GLUT_RGBA| |GLUT_INDEX| |GLUT_SINGLE| |GLUT_DOUBLE|
          |GLUT_ACCUM| |GLUT_ALPHA| |GLUT_DEPTH| |GLUT_STENCIL|
          |GLUT_MULTISAMPLE| |GLUT_STEREO| |GLUT_LUMINANCE|
          |GLUT_LEFT_BUTTON| |GLUT_MIDDLE_BUTTON| |GLUT_RIGHT_BUTTON|
          |GLUT_DOWN| |GLUT_UP| |GLUT_KEY_F1| |GLUT_KEY_F2| |GLUT_KEY_F3|
          |GLUT_KEY_F4| |GLUT_KEY_F5| |GLUT_KEY_F6| |GLUT_KEY_F7|
          |GLUT_KEY_F8| |GLUT_KEY_F9| |GLUT_KEY_F10| |GLUT_KEY_F11|
          |GLUT_KEY_F12| |GLUT_KEY_LEFT| |GLUT_KEY_UP| |GLUT_KEY_RIGHT|
          |GLUT_KEY_DOWN| |GLUT_KEY_PAGE_UP| |GLUT_KEY_PAGE_DOWN|
          |GLUT_KEY_HOME| |GLUT_KEY_END| |GLUT_KEY_INSERT| |GLUT_LEFT|
          |GLUT_ENTERED| |GLUT_MENU_NOT_IN_USE| |GLUT_MENU_IN_USE|
          |GLUT_NOT_VISIBLE| |GLUT_VISIBLE| |GLUT_HIDDEN|
          |GLUT_FULLY_RETAINED| |GLUT_PARTIALLY_RETAINED|
          |GLUT_FULLY_COVERED| |GLUT_RED| |GLUT_GREEN| |GLUT_BLUE|
          |GLUT_NORMAL| |GLUT_OVERLAY| |GLUT_STROKE_ROMAN|
          |GLUT_STROKE_MONO_ROMAN| |GLUT_BITMAP_9_BY_15|
          |GLUT_BITMAP_8_BY_13| |GLUT_BITMAP_TIMES_ROMAN_10|
          |GLUT_BITMAP_TIMES_ROMAN_24| |GLUT_BITMAP_HELVETICA_10|
          |GLUT_BITMAP_HELVETICA_12| |GLUT_BITMAP_HELVETICA_18|
          |GLUT_WINDOW_X| |GLUT_WINDOW_Y| |GLUT_WINDOW_WIDTH|
          |GLUT_WINDOW_HEIGHT| |GLUT_WINDOW_BUFFER_SIZE|
          |GLUT_WINDOW_STENCIL_SIZE| |GLUT_WINDOW_DEPTH_SIZE|
          |GLUT_WINDOW_RED_SIZE| |GLUT_WINDOW_GREEN_SIZE|
          |GLUT_WINDOW_BLUE_SIZE| |GLUT_WINDOW_ALPHA_SIZE|
          |GLUT_WINDOW_ACCUM_RED_SIZE| |GLUT_WINDOW_ACCUM_GREEN_SIZE|
          |GLUT_WINDOW_ACCUM_BLUE_SIZE| |GLUT_WINDOW_ACCUM_ALPHA_SIZE|
          |GLUT_WINDOW_DOUBLEBUFFER| |GLUT_WINDOW_RGBA| |GLUT_WINDOW_PARENT|
          |GLUT_WINDOW_NUM_CHILDREN| |GLUT_WINDOW_COLORMAP_SIZE|
          |GLUT_WINDOW_NUM_SAMPLES| |GLUT_WINDOW_STEREO| |GLUT_WINDOW_CURSOR|
          |GLUT_SCREEN_WIDTH| |GLUT_SCREEN_HEIGHT| |GLUT_SCREEN_WIDTH_MM|
          |GLUT_SCREEN_HEIGHT_MM| |GLUT_MENU_NUM_ITEMS|
          |GLUT_DISPLAY_MODE_POSSIBLE| |GLUT_INIT_WINDOW_X|
          |GLUT_INIT_WINDOW_Y| |GLUT_INIT_WINDOW_WIDTH|
          |GLUT_INIT_WINDOW_HEIGHT| |GLUT_INIT_DISPLAY_MODE|
          |GLUT_ELAPSED_TIME| |GLUT_WINDOW_FORMAT_ID| |GLUT_HAS_KEYBOARD|
          |GLUT_HAS_MOUSE| |GLUT_HAS_SPACEBALL| |GLUT_HAS_DIAL_AND_BUTTON_BOX|
          |GLUT_HAS_TABLET| |GLUT_NUM_MOUSE_BUTTONS|
          |GLUT_NUM_SPACEBALL_BUTTONS| |GLUT_NUM_BUTTON_BOX_BUTTONS|
          |GLUT_NUM_DIALS| |GLUT_NUM_TABLET_BUTTONS|
          |GLUT_DEVICE_IGNORE_KEY_REPEAT| |GLUT_DEVICE_KEY_REPEAT|
          |GLUT_HAS_JOYSTICK| |GLUT_OWNS_JOYSTICK| |GLUT_JOYSTICK_BUTTONS|
          |GLUT_JOYSTICK_AXES| |GLUT_JOYSTICK_POLL_RATE|
          |GLUT_OVERLAY_POSSIBLE| |GLUT_LAYER_IN_USE| |GLUT_HAS_OVERLAY|
          |GLUT_TRANSPARENT_INDEX| |GLUT_NORMAL_DAMAGED|
          |GLUT_OVERLAY_DAMAGED| |GLUT_VIDEO_RESIZE_POSSIBLE|
          |GLUT_VIDEO_RESIZE_IN_USE| |GLUT_VIDEO_RESIZE_X_DELTA|
          |GLUT_VIDEO_RESIZE_Y_DELTA| |GLUT_VIDEO_RESIZE_WIDTH_DELTA|
          |GLUT_VIDEO_RESIZE_HEIGHT_DELTA| |GLUT_VIDEO_RESIZE_X|
          |GLUT_VIDEO_RESIZE_Y| |GLUT_VIDEO_RESIZE_WIDTH|
          |GLUT_VIDEO_RESIZE_HEIGHT| |GLUT_NORMAL| |GLUT_OVERLAY|
          |GLUT_ACTIVE_SHIFT| |GLUT_ACTIVE_CTRL| |GLUT_ACTIVE_ALT|
          |GLUT_CURSOR_RIGHT_ARROW| |GLUT_CURSOR_LEFT_ARROW|
          |GLUT_CURSOR_INFO| |GLUT_CURSOR_DESTROY| |GLUT_CURSOR_HELP|
          |GLUT_CURSOR_CYCLE| |GLUT_CURSOR_SPRAY| |GLUT_CURSOR_WAIT|
          |GLUT_CURSOR_TEXT| |GLUT_CURSOR_CROSSHAIR| |GLUT_CURSOR_UP_DOWN|
          |GLUT_CURSOR_LEFT_RIGHT| |GLUT_CURSOR_TOP_SIDE|
          |GLUT_CURSOR_BOTTOM_SIDE| |GLUT_CURSOR_LEFT_SIDE|
          |GLUT_CURSOR_RIGHT_SIDE| |GLUT_CURSOR_TOP_LEFT_CORNER|
          |GLUT_CURSOR_TOP_RIGHT_CORNER| |GLUT_CURSOR_BOTTOM_RIGHT_CORNER|
          |GLUT_CURSOR_BOTTOM_LEFT_CORNER| |GLUT_CURSOR_INHERIT|
          |GLUT_CURSOR_NONE| |GLUT_CURSOR_FULL_CROSSHAIR|
           )
  )

(select-module gl.glut)

(dynamic-load "gauche-glut")

(provide "gl/glut")
