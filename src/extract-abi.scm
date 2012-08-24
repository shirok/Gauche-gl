;;;
;;; extract-abi.scm - Extract ABI of GL extension
;;;
;;;  Copyright (c) 2005-2012  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: extract-abi.scm,v 1.4 2008-06-04 11:50:01 shirok Exp $
;;;

;; We no longer count on glext.h on the target platform, since
;; it may be out of date and it is too much hassle to support
;; all variations.  Instead of putting #ifdef's here and there,
;; we shifted everthing to runtime---so we include all the
;; declarations of the most recent GL extension ABI, and the
;; code queries availability of each of them at runtime.
;;
;; This program scans glext.h as the authoritative source of GL
;; extension ABI.  We can use the most recent version from here:
;;  http://oss.sgi.com/projects/ogl-sample/ABI/
;;
;; It generates glext-abi.scm, which contains necessary information
;; in S-expressions.
;;
;; The format of glext-abi.scm is as follows.
;;
;;  * Constants
;;
;;      (define-constant GL_UNSIGNED_BYTE_3_3_2 #x8032)
;;
;;  * Extensions
;;
;;      (define-extension-name GL_ARB_fragment_program)
;;
;;  * Entry points
;;
;;      (define-entry-point glBindProgramNV "... typedef ...")
;;

(use srfi-1)
(use srfi-13)

;; Predefined OpenGL 1.0 and 1.1 symbols

(define *gl-syms*
  '(
    ;; Boolean values
    (GL_FALSE                           . #x0)
    (GL_TRUE                            . #x1)

    ;; Data types
    (GL_BYTE                            . #x1400)
    (GL_UNSIGNED_BYTE                   . #x1401)
    (GL_SHORT                           . #x1402)
    (GL_UNSIGNED_SHORT                  . #x1403)
    (GL_INT                             . #x1404)
    (GL_UNSIGNED_INT                    . #x1405)
    (GL_FLOAT                           . #x1406)
    (GL_DOUBLE                          . #x140A)
    (GL_2_BYTES                         . #x1407)
    (GL_3_BYTES                         . #x1408)
    (GL_4_BYTES                         . #x1409)

    ;; Primitives
    (GL_POINTS                          . #x0000)
    (GL_LINES                           . #x0001)
    (GL_LINE_LOOP                       . #x0002)
    (GL_LINE_STRIP                      . #x0003)
    (GL_TRIANGLES                       . #x0004)
    (GL_TRIANGLE_STRIP                  . #x0005)
    (GL_TRIANGLE_FAN                    . #x0006)
    (GL_QUADS                           . #x0007)
    (GL_QUAD_STRIP                      . #x0008)
    (GL_POLYGON                         . #x0009)

    ;; Vertex Arrays
    (GL_VERTEX_ARRAY                    . #x8074)
    (GL_NORMAL_ARRAY                    . #x8075)
    (GL_COLOR_ARRAY                     . #x8076)
    (GL_INDEX_ARRAY                     . #x8077)
    (GL_TEXTURE_COORD_ARRAY             . #x8078)
    (GL_EDGE_FLAG_ARRAY                 . #x8079)
    (GL_VERTEX_ARRAY_SIZE               . #x807A)
    (GL_VERTEX_ARRAY_TYPE               . #x807B)
    (GL_VERTEX_ARRAY_STRIDE             . #x807C)
    (GL_NORMAL_ARRAY_TYPE               . #x807E)
    (GL_NORMAL_ARRAY_STRIDE             . #x807F)
    (GL_COLOR_ARRAY_SIZE                . #x8081)
    (GL_COLOR_ARRAY_TYPE                . #x8082)
    (GL_COLOR_ARRAY_STRIDE              . #x8083)
    (GL_INDEX_ARRAY_TYPE                . #x8085)
    (GL_INDEX_ARRAY_STRIDE              . #x8086)
    (GL_TEXTURE_COORD_ARRAY_SIZE        . #x8088)
    (GL_TEXTURE_COORD_ARRAY_TYPE        . #x8089)
    (GL_TEXTURE_COORD_ARRAY_STRIDE      . #x808A)
    (GL_EDGE_FLAG_ARRAY_STRIDE          . #x808C)
    (GL_VERTEX_ARRAY_POINTER            . #x808E)
    (GL_NORMAL_ARRAY_POINTER            . #x808F)
    (GL_COLOR_ARRAY_POINTER             . #x8090)
    (GL_INDEX_ARRAY_POINTER             . #x8091)
    (GL_TEXTURE_COORD_ARRAY_POINTER     . #x8092)
    (GL_EDGE_FLAG_ARRAY_POINTER         . #x8093)
    (GL_V2F                             . #x2A20)
    (GL_V3F                             . #x2A21)
    (GL_C4UB_V2F                        . #x2A22)
    (GL_C4UB_V3F                        . #x2A23)
    (GL_C3F_V3F                         . #x2A24)
    (GL_N3F_V3F                         . #x2A25)
    (GL_C4F_N3F_V3F                     . #x2A26)
    (GL_T2F_V3F                         . #x2A27)
    (GL_T4F_V4F                         . #x2A28)
    (GL_T2F_C4UB_V3F                    . #x2A29)
    (GL_T2F_C3F_V3F                     . #x2A2A)
    (GL_T2F_N3F_V3F                     . #x2A2B)
    (GL_T2F_C4F_N3F_V3F                 . #x2A2C)
    (GL_T4F_C4F_N3F_V4F                 . #x2A2D)

    ;; Matrix Mode
    (GL_MATRIX_MODE                     . #x0BA0)
    (GL_MODELVIEW                       . #x1700)
    (GL_PROJECTION                      . #x1701)
    (GL_TEXTURE                         . #x1702)

    ;; Points
    (GL_POINT_SMOOTH                    . #x0B10)
    (GL_POINT_SIZE                      . #x0B11)
    (GL_POINT_SIZE_GRANULARITY          . #x0B13)
    (GL_POINT_SIZE_RANGE                . #x0B12)

    ;; Lines
    (GL_LINE_SMOOTH                     . #x0B20)
    (GL_LINE_STIPPLE                    . #x0B24)
    (GL_LINE_STIPPLE_PATTERN            . #x0B25)
    (GL_LINE_STIPPLE_REPEAT             . #x0B26)
    (GL_LINE_WIDTH                      . #x0B21)
    (GL_LINE_WIDTH_GRANULARITY          . #x0B23)
    (GL_LINE_WIDTH_RANGE                . #x0B22)

    ;; Polygons
    (GL_POINT                           . #x1B00)
    (GL_LINE                            . #x1B01)
    (GL_FILL                            . #x1B02)
    (GL_CW                              . #x0900)
    (GL_CCW                             . #x0901)
    (GL_FRONT                           . #x0404)
    (GL_BACK                            . #x0405)
    (GL_POLYGON_MODE                    . #x0B40)
    (GL_POLYGON_SMOOTH                  . #x0B41)
    (GL_POLYGON_STIPPLE                 . #x0B42)
    (GL_EDGE_FLAG                       . #x0B43)
    (GL_CULL_FACE                       . #x0B44)
    (GL_CULL_FACE_MODE                  . #x0B45)
    (GL_FRONT_FACE                      . #x0B46)
    (GL_POLYGON_OFFSET_FACTOR           . #x8038)
    (GL_POLYGON_OFFSET_UNITS            . #x2A00)
    (GL_POLYGON_OFFSET_POINT            . #x2A01)
    (GL_POLYGON_OFFSET_LINE             . #x2A02)
    (GL_POLYGON_OFFSET_FILL             . #x8037)

    ;; Display Lists
    (GL_COMPILE                         . #x1300)
    (GL_COMPILE_AND_EXECUTE             . #x1301)
    (GL_LIST_BASE                       . #x0B32)
    (GL_LIST_INDEX                      . #x0B33)
    (GL_LIST_MODE                       . #x0B30)

    ;; Depth buffer
    (GL_NEVER                           . #x0200)
    (GL_LESS                            . #x0201)
    (GL_EQUAL                           . #x0202)
    (GL_LEQUAL                          . #x0203)
    (GL_GREATER                         . #x0204)
    (GL_NOTEQUAL                        . #x0205)
    (GL_GEQUAL                          . #x0206)
    (GL_ALWAYS                          . #x0207)
    (GL_DEPTH_TEST                      . #x0B71)
    (GL_DEPTH_BITS                      . #x0D56)
    (GL_DEPTH_CLEAR_VALUE               . #x0B73)
    (GL_DEPTH_FUNC                      . #x0B74)
    (GL_DEPTH_RANGE                     . #x0B70)
    (GL_DEPTH_WRITEMASK                 . #x0B72)
    (GL_DEPTH_COMPONENT                 . #x1902)

    ;; Lighting
    (GL_LIGHTING                        . #x0B50)
    (GL_LIGHT0                          . #x4000)
    (GL_LIGHT1                          . #x4001)
    (GL_LIGHT2                          . #x4002)
    (GL_LIGHT3                          . #x4003)
    (GL_LIGHT4                          . #x4004)
    (GL_LIGHT5                          . #x4005)
    (GL_LIGHT6                          . #x4006)
    (GL_LIGHT7                          . #x4007)
    (GL_SPOT_EXPONENT                   . #x1205)
    (GL_SPOT_CUTOFF                     . #x1206)
    (GL_CONSTANT_ATTENUATION            . #x1207)
    (GL_LINEAR_ATTENUATION              . #x1208)
    (GL_QUADRATIC_ATTENUATION           . #x1209)
    (GL_AMBIENT                         . #x1200)
    (GL_DIFFUSE                         . #x1201)
    (GL_SPECULAR                        . #x1202)
    (GL_SHININESS                       . #x1601)
    (GL_EMISSION                        . #x1600)
    (GL_POSITION                        . #x1203)
    (GL_SPOT_DIRECTION                  . #x1204)
    (GL_AMBIENT_AND_DIFFUSE             . #x1602)
    (GL_COLOR_INDEXES                   . #x1603)
    (GL_LIGHT_MODEL_TWO_SIDE            . #x0B52)
    (GL_LIGHT_MODEL_LOCAL_VIEWER        . #x0B51)
    (GL_LIGHT_MODEL_AMBIENT             . #x0B53)
    (GL_FRONT_AND_BACK                  . #x0408)
    (GL_SHADE_MODEL                     . #x0B54)
    (GL_FLAT                            . #x1D00)
    (GL_SMOOTH                          . #x1D01)
    (GL_COLOR_MATERIAL                  . #x0B57)
    (GL_COLOR_MATERIAL_FACE             . #x0B55)
    (GL_COLOR_MATERIAL_PARAMETER        . #x0B56)
    (GL_NORMALIZE                       . #x0BA1)

    ;; User clipping planes
    (GL_CLIP_PLANE0                     . #x3000)
    (GL_CLIP_PLANE1                     . #x3001)
    (GL_CLIP_PLANE2                     . #x3002)
    (GL_CLIP_PLANE3                     . #x3003)
    (GL_CLIP_PLANE4                     . #x3004)
    (GL_CLIP_PLANE5                     . #x3005)

    ;; Accumulation buffer
    (GL_ACCUM_RED_BITS                  . #x0D58)
    (GL_ACCUM_GREEN_BITS                . #x0D59)
    (GL_ACCUM_BLUE_BITS                 . #x0D5A)
    (GL_ACCUM_ALPHA_BITS                . #x0D5B)
    (GL_ACCUM_CLEAR_VALUE               . #x0B80)
    (GL_ACCUM                           . #x0100)
    (GL_ADD                             . #x0104)
    (GL_LOAD                            . #x0101)
    (GL_MULT                            . #x0103)
    (GL_RETURN                          . #x0102)

    ;; Alpha testing
    (GL_ALPHA_TEST                      . #x0BC0)
    (GL_ALPHA_TEST_REF                  . #x0BC2)
    (GL_ALPHA_TEST_FUNC                 . #x0BC1)

    ;; Blending
    (GL_BLEND                           . #x0BE2)
    (GL_BLEND_SRC                       . #x0BE1)
    (GL_BLEND_DST                       . #x0BE0)
    (GL_ZERO                            . #x0)
    (GL_ONE                             . #x1)
    (GL_SRC_COLOR                       . #x0300)
    (GL_ONE_MINUS_SRC_COLOR             . #x0301)
    (GL_SRC_ALPHA                       . #x0302)
    (GL_ONE_MINUS_SRC_ALPHA             . #x0303)
    (GL_DST_ALPHA                       . #x0304)
    (GL_ONE_MINUS_DST_ALPHA             . #x0305)
    (GL_DST_COLOR                       . #x0306)
    (GL_ONE_MINUS_DST_COLOR             . #x0307)
    (GL_SRC_ALPHA_SATURATE              . #x0308)
    (GL_CONSTANT_COLOR                  . #x8001)
    (GL_ONE_MINUS_CONSTANT_COLOR        . #x8002)
    (GL_CONSTANT_ALPHA                  . #x8003)
    (GL_ONE_MINUS_CONSTANT_ALPHA        . #x8004)

    ;; Render Mode
    (GL_FEEDBACK                        . #x1C01)
    (GL_RENDER                          . #x1C00)
    (GL_SELECT                          . #x1C02)

    ;; Feedback
    (GL_2D                              . #x0600)
    (GL_3D                              . #x0601)
    (GL_3D_COLOR                        . #x0602)
    (GL_3D_COLOR_TEXTURE                . #x0603)
    (GL_4D_COLOR_TEXTURE                . #x0604)
    (GL_POINT_TOKEN                     . #x0701)
    (GL_LINE_TOKEN                      . #x0702)
    (GL_LINE_RESET_TOKEN                . #x0707)
    (GL_POLYGON_TOKEN                   . #x0703)
    (GL_BITMAP_TOKEN                    . #x0704)
    (GL_DRAW_PIXEL_TOKEN                . #x0705)
    (GL_COPY_PIXEL_TOKEN                . #x0706)
    (GL_PASS_THROUGH_TOKEN              . #x0700)
    (GL_FEEDBACK_BUFFER_POINTER         . #x0DF0)
    (GL_FEEDBACK_BUFFER_SIZE            . #x0DF1)
    (GL_FEEDBACK_BUFFER_TYPE            . #x0DF2)

    ;; Selection
    (GL_SELECTION_BUFFER_POINTER        . #x0DF3)
    (GL_SELECTION_BUFFER_SIZE           . #x0DF4)

    ;; Fog
    (GL_FOG                             . #x0B60)
    (GL_FOG_MODE                        . #x0B65)
    (GL_FOG_DENSITY                     . #x0B62)
    (GL_FOG_COLOR                       . #x0B66)
    (GL_FOG_INDEX                       . #x0B61)
    (GL_FOG_START                       . #x0B63)
    (GL_FOG_END                         . #x0B64)
    (GL_LINEAR                          . #x2601)
    (GL_EXP                             . #x0800)
    (GL_EXP2                            . #x0801)

    ;; Logic Ops
    (GL_LOGIC_OP                        . #x0BF1)
    (GL_INDEX_LOGIC_OP                  . #x0BF1)
    (GL_COLOR_LOGIC_OP                  . #x0BF2)
    (GL_LOGIC_OP_MODE                   . #x0BF0)
    (GL_CLEAR                           . #x1500)
    (GL_SET                             . #x150F)
    (GL_COPY                            . #x1503)
    (GL_COPY_INVERTED                   . #x150C)
    (GL_NOOP                            . #x1505)
    (GL_INVERT                          . #x150A)
    (GL_AND                             . #x1501)
    (GL_NAND                            . #x150E)
    (GL_OR                              . #x1507)
    (GL_NOR                             . #x1508)
    (GL_XOR                             . #x1506)
    (GL_EQUIV                           . #x1509)
    (GL_AND_REVERSE                     . #x1502)
    (GL_AND_INVERTED                    . #x1504)
    (GL_OR_REVERSE                      . #x150B)
    (GL_OR_INVERTED                     . #x150D)

    ;; Stencil
    (GL_STENCIL_TEST                    . #x0B90)
    (GL_STENCIL_WRITEMASK               . #x0B98)
    (GL_STENCIL_BITS                    . #x0D57)
    (GL_STENCIL_FUNC                    . #x0B92)
    (GL_STENCIL_VALUE_MASK              . #x0B93)
    (GL_STENCIL_REF                     . #x0B97)
    (GL_STENCIL_FAIL                    . #x0B94)
    (GL_STENCIL_PASS_DEPTH_PASS         . #x0B96)
    (GL_STENCIL_PASS_DEPTH_FAIL         . #x0B95)
    (GL_STENCIL_CLEAR_VALUE             . #x0B91)
    (GL_STENCIL_INDEX                   . #x1901)
    (GL_KEEP                            . #x1E00)
    (GL_REPLACE                         . #x1E01)
    (GL_INCR                            . #x1E02)
    (GL_DECR                            . #x1E03)

    ;; Buffers, Pixel Drawing/Reading
    (GL_NONE                            . #x0)
    (GL_LEFT                            . #x0406)
    (GL_RIGHT                           . #x0407)
    ;;GL_FRONT                          . #x0404
    ;;GL_BACK                           . #x0405
    ;;GL_FRONT_AND_BACK                 . #x0408
    (GL_FRONT_LEFT                      . #x0400)
    (GL_FRONT_RIGHT                     . #x0401)
    (GL_BACK_LEFT                       . #x0402)
    (GL_BACK_RIGHT                      . #x0403)
    (GL_AUX0                            . #x0409)
    (GL_AUX1                            . #x040A)
    (GL_AUX2                            . #x040B)
    (GL_AUX3                            . #x040C)
    (GL_COLOR_INDEX                     . #x1900)
    (GL_RED                             . #x1903)
    (GL_GREEN                           . #x1904)
    (GL_BLUE                            . #x1905)
    (GL_ALPHA                           . #x1906)
    (GL_LUMINANCE                       . #x1909)
    (GL_LUMINANCE_ALPHA                 . #x190A)
    (GL_ALPHA_BITS                      . #x0D55)
    (GL_RED_BITS                        . #x0D52)
    (GL_GREEN_BITS                      . #x0D53)
    (GL_BLUE_BITS                       . #x0D54)
    (GL_INDEX_BITS                      . #x0D51)
    (GL_SUBPIXEL_BITS                   . #x0D50)
    (GL_AUX_BUFFERS                     . #x0C00)
    (GL_READ_BUFFER                     . #x0C02)
    (GL_DRAW_BUFFER                     . #x0C01)
    (GL_DOUBLEBUFFER                    . #x0C32)
    (GL_STEREO                          . #x0C33)
    (GL_BITMAP                          . #x1A00)
    (GL_COLOR                           . #x1800)
    (GL_DEPTH                           . #x1801)
    (GL_STENCIL                         . #x1802)
    (GL_DITHER                          . #x0BD0)
    (GL_RGB                             . #x1907)
    (GL_RGBA                            . #x1908)

    ;; Implementation limits
    (GL_MAX_LIST_NESTING                . #x0B31)
    (GL_MAX_ATTRIB_STACK_DEPTH          . #x0D35)
    (GL_MAX_MODELVIEW_STACK_DEPTH       . #x0D36)
    (GL_MAX_NAME_STACK_DEPTH            . #x0D37)
    (GL_MAX_PROJECTION_STACK_DEPTH      . #x0D38)
    (GL_MAX_TEXTURE_STACK_DEPTH         . #x0D39)
    (GL_MAX_EVAL_ORDER                  . #x0D30)
    (GL_MAX_LIGHTS                      . #x0D31)
    (GL_MAX_CLIP_PLANES                 . #x0D32)
    (GL_MAX_TEXTURE_SIZE                . #x0D33)
    (GL_MAX_PIXEL_MAP_TABLE             . #x0D34)
    (GL_MAX_VIEWPORT_DIMS               . #x0D3A)
    (GL_MAX_CLIENT_ATTRIB_STACK_DEPTH   . #x0D3B)

    ;; Gets
    (GL_ATTRIB_STACK_DEPTH              . #x0BB0)
    (GL_CLIENT_ATTRIB_STACK_DEPTH       . #x0BB1)
    (GL_COLOR_CLEAR_VALUE               . #x0C22)
    (GL_COLOR_WRITEMASK                 . #x0C23)
    (GL_CURRENT_INDEX                   . #x0B01)
    (GL_CURRENT_COLOR                   . #x0B00)
    (GL_CURRENT_NORMAL                  . #x0B02)
    (GL_CURRENT_RASTER_COLOR            . #x0B04)
    (GL_CURRENT_RASTER_DISTANCE         . #x0B09)
    (GL_CURRENT_RASTER_INDEX            . #x0B05)
    (GL_CURRENT_RASTER_POSITION         . #x0B07)
    (GL_CURRENT_RASTER_TEXTURE_COORDS   . #x0B06)
    (GL_CURRENT_RASTER_POSITION_VALID   . #x0B08)
    (GL_CURRENT_TEXTURE_COORDS          . #x0B03)
    (GL_INDEX_CLEAR_VALUE               . #x0C20)
    (GL_INDEX_MODE                      . #x0C30)
    (GL_INDEX_WRITEMASK                 . #x0C21)
    (GL_MODELVIEW_MATRIX                . #x0BA6)
    (GL_MODELVIEW_STACK_DEPTH           . #x0BA3)
    (GL_NAME_STACK_DEPTH                . #x0D70)
    (GL_PROJECTION_MATRIX               . #x0BA7)
    (GL_PROJECTION_STACK_DEPTH          . #x0BA4)
    (GL_RENDER_MODE                     . #x0C40)
    (GL_RGBA_MODE                       . #x0C31)
    (GL_TEXTURE_MATRIX                  . #x0BA8)
    (GL_TEXTURE_STACK_DEPTH             . #x0BA5)
    (GL_VIEWPORT                        . #x0BA2)

    ;; Evaluators
    (GL_AUTO_NORMAL                     . #x0D80)
    (GL_MAP1_COLOR_4                    . #x0D90)
    (GL_MAP1_GRID_DOMAIN                . #x0DD0)
    (GL_MAP1_GRID_SEGMENTS              . #x0DD1)
    (GL_MAP1_INDEX                      . #x0D91)
    (GL_MAP1_NORMAL                     . #x0D92)
    (GL_MAP1_TEXTURE_COORD_1            . #x0D93)
    (GL_MAP1_TEXTURE_COORD_2            . #x0D94)
    (GL_MAP1_TEXTURE_COORD_3            . #x0D95)
    (GL_MAP1_TEXTURE_COORD_4            . #x0D96)
    (GL_MAP1_VERTEX_3                   . #x0D97)
    (GL_MAP1_VERTEX_4                   . #x0D98)
    (GL_MAP2_COLOR_4                    . #x0DB0)
    (GL_MAP2_GRID_DOMAIN                . #x0DD2)
    (GL_MAP2_GRID_SEGMENTS              . #x0DD3)
    (GL_MAP2_INDEX                      . #x0DB1)
    (GL_MAP2_NORMAL                     . #x0DB2)
    (GL_MAP2_TEXTURE_COORD_1            . #x0DB3)
    (GL_MAP2_TEXTURE_COORD_2            . #x0DB4)
    (GL_MAP2_TEXTURE_COORD_3            . #x0DB5)
    (GL_MAP2_TEXTURE_COORD_4            . #x0DB6)
    (GL_MAP2_VERTEX_3                   . #x0DB7)
    (GL_MAP2_VERTEX_4                   . #x0DB8)
    (GL_COEFF                           . #x0A00)
    (GL_DOMAIN                          . #x0A02)
    (GL_ORDER                           . #x0A01)

    ;; Hints
    (GL_FOG_HINT                        . #x0C54)
    (GL_LINE_SMOOTH_HINT                . #x0C52)
    (GL_PERSPECTIVE_CORRECTION_HINT     . #x0C50)
    (GL_POINT_SMOOTH_HINT               . #x0C51)
    (GL_POLYGON_SMOOTH_HINT             . #x0C53)
    (GL_DONT_CARE                       . #x1100)
    (GL_FASTEST                         . #x1101)
    (GL_NICEST                          . #x1102)

    ;; Scissor box
    (GL_SCISSOR_TEST                    . #x0C11)
    (GL_SCISSOR_BOX                     . #x0C10)

    ;; Pixel Mode / Transfer
    (GL_MAP_COLOR                       . #x0D10)
    (GL_MAP_STENCIL                     . #x0D11)
    (GL_INDEX_SHIFT                     . #x0D12)
    (GL_INDEX_OFFSET                    . #x0D13)
    (GL_RED_SCALE                       . #x0D14)
    (GL_RED_BIAS                        . #x0D15)
    (GL_GREEN_SCALE                     . #x0D18)
    (GL_GREEN_BIAS                      . #x0D19)
    (GL_BLUE_SCALE                      . #x0D1A)
    (GL_BLUE_BIAS                       . #x0D1B)
    (GL_ALPHA_SCALE                     . #x0D1C)
    (GL_ALPHA_BIAS                      . #x0D1D)
    (GL_DEPTH_SCALE                     . #x0D1E)
    (GL_DEPTH_BIAS                      . #x0D1F)
    (GL_PIXEL_MAP_S_TO_S_SIZE           . #x0CB1)
    (GL_PIXEL_MAP_I_TO_I_SIZE           . #x0CB0)
    (GL_PIXEL_MAP_I_TO_R_SIZE           . #x0CB2)
    (GL_PIXEL_MAP_I_TO_G_SIZE           . #x0CB3)
    (GL_PIXEL_MAP_I_TO_B_SIZE           . #x0CB4)
    (GL_PIXEL_MAP_I_TO_A_SIZE           . #x0CB5)
    (GL_PIXEL_MAP_R_TO_R_SIZE           . #x0CB6)
    (GL_PIXEL_MAP_G_TO_G_SIZE           . #x0CB7)
    (GL_PIXEL_MAP_B_TO_B_SIZE           . #x0CB8)
    (GL_PIXEL_MAP_A_TO_A_SIZE           . #x0CB9)
    (GL_PIXEL_MAP_S_TO_S                . #x0C71)
    (GL_PIXEL_MAP_I_TO_I                . #x0C70)
    (GL_PIXEL_MAP_I_TO_R                . #x0C72)
    (GL_PIXEL_MAP_I_TO_G                . #x0C73)
    (GL_PIXEL_MAP_I_TO_B                . #x0C74)
    (GL_PIXEL_MAP_I_TO_A                . #x0C75)
    (GL_PIXEL_MAP_R_TO_R                . #x0C76)
    (GL_PIXEL_MAP_G_TO_G                . #x0C77)
    (GL_PIXEL_MAP_B_TO_B                . #x0C78)
    (GL_PIXEL_MAP_A_TO_A                . #x0C79)
    (GL_PACK_ALIGNMENT                  . #x0D05)
    (GL_PACK_LSB_FIRST                  . #x0D01)
    (GL_PACK_ROW_LENGTH                 . #x0D02)
    (GL_PACK_SKIP_PIXELS                . #x0D04)
    (GL_PACK_SKIP_ROWS                  . #x0D03)
    (GL_PACK_SWAP_BYTES                 . #x0D00)
    (GL_UNPACK_ALIGNMENT                . #x0CF5)
    (GL_UNPACK_LSB_FIRST                . #x0CF1)
    (GL_UNPACK_ROW_LENGTH               . #x0CF2)
    (GL_UNPACK_SKIP_PIXELS              . #x0CF4)
    (GL_UNPACK_SKIP_ROWS                . #x0CF3)
    (GL_UNPACK_SWAP_BYTES               . #x0CF0)
    (GL_ZOOM_X                          . #x0D16)
    (GL_ZOOM_Y                          . #x0D17)

    ;; Texture mapping
    (GL_TEXTURE_ENV                     . #x2300)
    (GL_TEXTURE_ENV_MODE                . #x2200)
    (GL_TEXTURE_1D                      . #x0DE0)
    (GL_TEXTURE_2D                      . #x0DE1)
    (GL_TEXTURE_WRAP_S                  . #x2802)
    (GL_TEXTURE_WRAP_T                  . #x2803)
    (GL_TEXTURE_MAG_FILTER              . #x2800)
    (GL_TEXTURE_MIN_FILTER              . #x2801)
    (GL_TEXTURE_ENV_COLOR               . #x2201)
    (GL_TEXTURE_GEN_S                   . #x0C60)
    (GL_TEXTURE_GEN_T                   . #x0C61)
    (GL_TEXTURE_GEN_MODE                . #x2500)
    (GL_TEXTURE_BORDER_COLOR            . #x1004)
    (GL_TEXTURE_WIDTH                   . #x1000)
    (GL_TEXTURE_HEIGHT                  . #x1001)
    (GL_TEXTURE_BORDER                  . #x1005)
    (GL_TEXTURE_COMPONENTS              . #x1003)
    (GL_TEXTURE_RED_SIZE                . #x805C)
    (GL_TEXTURE_GREEN_SIZE              . #x805D)
    (GL_TEXTURE_BLUE_SIZE               . #x805E)
    (GL_TEXTURE_ALPHA_SIZE              . #x805F)
    (GL_TEXTURE_LUMINANCE_SIZE          . #x8060)
    (GL_TEXTURE_INTENSITY_SIZE          . #x8061)
    (GL_NEAREST_MIPMAP_NEAREST          . #x2700)
    (GL_NEAREST_MIPMAP_LINEAR           . #x2702)
    (GL_LINEAR_MIPMAP_NEAREST           . #x2701)
    (GL_LINEAR_MIPMAP_LINEAR            . #x2703)
    (GL_OBJECT_LINEAR                   . #x2401)
    (GL_OBJECT_PLANE                    . #x2501)
    (GL_EYE_LINEAR                      . #x2400)
    (GL_EYE_PLANE                       . #x2502)
    (GL_SPHERE_MAP                      . #x2402)
    (GL_DECAL                           . #x2101)
    (GL_MODULATE                        . #x2100)
    (GL_NEAREST                         . #x2600)
    (GL_REPEAT                          . #x2901)
    (GL_CLAMP                           . #x2900)
    (GL_S                               . #x2000)
    (GL_T                               . #x2001)
    (GL_R                               . #x2002)
    (GL_Q                               . #x2003)
    (GL_TEXTURE_GEN_R                   . #x0C62)
    (GL_TEXTURE_GEN_Q                   . #x0C63)

    ;; Utility
    (GL_VENDOR                          . #x1F00)
    (GL_RENDERER                        . #x1F01)
    (GL_VERSION                         . #x1F02)
    (GL_EXTENSIONS                      . #x1F03)

    ;; Errors
    (GL_NO_ERROR                        . #x0)
    (GL_INVALID_VALUE                   . #x0501)
    (GL_INVALID_ENUM                    . #x0500)
    (GL_INVALID_OPERATION               . #x0502)
    (GL_STACK_OVERFLOW                  . #x0503)
    (GL_STACK_UNDERFLOW                 . #x0504)
    (GL_OUT_OF_MEMORY                   . #x0505)

    ;; glPush/PopAttrib bits
    (GL_CURRENT_BIT                     . #x00000001)
    (GL_POINT_BIT                       . #x00000002)
    (GL_LINE_BIT                        . #x00000004)
    (GL_POLYGON_BIT                     . #x00000008)
    (GL_POLYGON_STIPPLE_BIT             . #x00000010)
    (GL_PIXEL_MODE_BIT                  . #x00000020)
    (GL_LIGHTING_BIT                    . #x00000040)
    (GL_FOG_BIT                         . #x00000080)
    (GL_DEPTH_BUFFER_BIT                . #x00000100)
    (GL_ACCUM_BUFFER_BIT                . #x00000200)
    (GL_STENCIL_BUFFER_BIT              . #x00000400)
    (GL_VIEWPORT_BIT                    . #x00000800)
    (GL_TRANSFORM_BIT                   . #x00001000)
    (GL_ENABLE_BIT                      . #x00002000)
    (GL_COLOR_BUFFER_BIT                . #x00004000)
    (GL_HINT_BIT                        . #x00008000)
    (GL_EVAL_BIT                        . #x00010000)
    (GL_LIST_BIT                        . #x00020000)
    (GL_TEXTURE_BIT                     . #x00040000)
    (GL_SCISSOR_BIT                     . #x00080000)
    (GL_ALL_ATTRIB_BITS                 . #x000FFFFF)


    ;; OpenGL 1.1
    (GL_PROXY_TEXTURE_1D                . #x8063)
    (GL_PROXY_TEXTURE_2D                . #x8064)
    (GL_TEXTURE_PRIORITY                . #x8066)
    (GL_TEXTURE_RESIDENT                . #x8067)
    (GL_TEXTURE_BINDING_1D              . #x8068)
    (GL_TEXTURE_BINDING_2D              . #x8069)
    (GL_TEXTURE_INTERNAL_FORMAT         . #x1003)
    (GL_ALPHA4                          . #x803B)
    (GL_ALPHA8                          . #x803C)
    (GL_ALPHA12                         . #x803D)
    (GL_ALPHA16                         . #x803E)
    (GL_LUMINANCE4                      . #x803F)
    (GL_LUMINANCE8                      . #x8040)
    (GL_LUMINANCE12                     . #x8041)
    (GL_LUMINANCE16                     . #x8042)
    (GL_LUMINANCE4_ALPHA4               . #x8043)
    (GL_LUMINANCE6_ALPHA2               . #x8044)
    (GL_LUMINANCE8_ALPHA8               . #x8045)
    (GL_LUMINANCE12_ALPHA4              . #x8046)
    (GL_LUMINANCE12_ALPHA12             . #x8047)
    (GL_LUMINANCE16_ALPHA16             . #x8048)
    (GL_INTENSITY                       . #x8049)
    (GL_INTENSITY4                      . #x804A)
    (GL_INTENSITY8                      . #x804B)
    (GL_INTENSITY12                     . #x804C)
    (GL_INTENSITY16                     . #x804D)
    (GL_R3_G3_B2                        . #x2A10)
    (GL_RGB4                            . #x804F)
    (GL_RGB5                            . #x8050)
    (GL_RGB8                            . #x8051)
    (GL_RGB10                           . #x8052)
    (GL_RGB12                           . #x8053)
    (GL_RGB16                           . #x8054)
    (GL_RGBA2                           . #x8055)
    (GL_RGBA4                           . #x8056)
    (GL_RGB5_A1                         . #x8057)
    (GL_RGBA8                           . #x8058)
    (GL_RGB10_A2                        . #x8059)
    (GL_RGBA12                          . #x805A)
    (GL_RGBA16                          . #x805B)
    (GL_CLIENT_PIXEL_STORE_BIT          . #x00000001)
    (GL_CLIENT_VERTEX_ARRAY_BIT         . #x00000002)
    (GL_ALL_CLIENT_ATTRIB_BITS          . #xFFFFFFFF)
    (GL_CLIENT_ALL_ATTRIB_BITS          . #xFFFFFFFF)
    ))

(define (scan)
  (let loop ((x '()) ;; extensions name
             (c (reverse *gl-syms*)) ;; constants [(name . value)]
             (p '())) ;; entry points [(name PFNNAMEPROC . typedef)]
    (rxmatch-case (read-line)
      (test eof-object?
            (values (reverse x) (reverse c) (reverse p)))
      (#/^#ifndef\s+(GL_\w+)/ (#f extname)
       (let1 en (string->symbol extname)
         (if (memq en x)
           (loop x c p)
           (loop (cons (string->symbol extname) x) c p))))
      (#/^#define\s+(GL_\w+)\s+(?:0x([\da-fA-F]+)|(\d+)|(\w+))/
       (#f cname xval dval ref)
       (let1 cn (string->symbol cname)
         (if (or (assq cn c) (memq cn x))
           (loop x c p)
           (let1 const (cons cn
                             (cond (xval (string->number xval 16))
                                   (dval (string->number dval))
                                   (else (string->symbol ref))))
             (loop x (cons const c) p)))))
      (#/^(?:extern|GLAPI)\s+[\w*]+\s+(?:APIENTRY\s+)?(gl\w+)/ (#f fname)
       (loop x c
             (cons (list fname (format "PFN~aPROC" (string-upcase fname)))
                   p)))
      (#/^typedef.*(PFN.*PROC).*/ (typedef name)
       (let1 apientry (find (lambda (e) (string=? name (cadr e))) p)
         (when apientry
           (set-cdr! (cdr apientry) typedef))
         (loop x c p)))
      (else (loop x c p)))))

(define (emit extensions constants entry-points)
  (print ";; This file should reflect the newest OpenGL extension ABI")
  (print ";; You can regenarate this by feeding glext.h to extract-abi.scm")
  (print ";; To obtain the newest glext.h, see")
  (print ";;   http://oss.sgi.com/projects/ogl-sample/ABI/")
  (print)
  (print ";; Extensions")
  (dolist (e extensions)
    (format #t "(define-extension-name ~a)\n" e))
  (print)
  (print ";; Constants")
  (dolist (c constants)
    (let loop ((val (cdr c)))
      (if (number? val)
        (format #t "(define-constant ~a #x~4,'0x)\n" (car c) val)
        (let1 p (assq val constants)
          (if p
            (loop (cdr p))
            (error "huh? " val))))))
  (print)
  (print ";; API entry points")
  (dolist (e entry-points)
    (format #t "(define-entry-point ~a ~a ~s)\n" (car e) (cadr e) (cddr e)))
  (print)
  )

(define (main args)
  (call-with-values scan emit)
  0)

    
                
           
