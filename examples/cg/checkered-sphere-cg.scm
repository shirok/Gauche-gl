
; Basic example of the use of the Cg runtime in a simple OpenGL program.
; 
; This demo originated from examples/runtime_ogl_vertex_fragment/ in the Cg
; distribution.  It was ported to Gauche by Issac Trotts in 2005.
;
; The port involved some changes.  
; The function cgSetErrorCallback is
; not explicitly called, since the Gauche Cg binding automatically
; sets things up to generate an informative exception whenever a Cg
; error occurs.  
; The vertex program and fragment program are
; kept in this file as strings rather than in separate files.
; Scheme's nice handling of multiline literal strings makes this a
; reasonable thing to do, though it does seem to play havoc on 
; syntax highlighting in emacs and Vim.
; The computation of the checkerboard was made more efficient to avoid a long
; wait at startup.  
; A display list was added to minimize the amount of traffic on the bus, in the hope
; that the occasional slow-downs would go away.  This didn't really work as well
; as I had hoped.  The C version doesn't seem to have this problem, at least not 
; as bad, so I guess it has something to do with garbage collection turning on from
; time to time.
; 
; -ijt

; Here is the original copyright notice:
; 
; Copyright NVIDIA Corporation 2002
; TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW THIS SOFTWARE IS PROVIDED
; *AS IS* AND NVIDIA AND ITS SUPPLIERS DISCLAIM ALL WARRANTIES EITHER EXPRESS
; OR IMPLIED INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL NVIDIA OR ITS SUPPLIERS
; BE LIABLE FOR ANY SPECIAL INCIDENTAL INDIRECT OR CONSEQUENTIAL DAMAGES
; WHATSOEVER (INCLUDING WITHOUT LIMITATION DAMAGES FOR LOSS OF BUSINESS PROFITS 
; BUSINESS INTERRUPTION LOSS OF BUSINESS INFORMATION OR ANY OTHER PECUNIARY LOSS)
; ARISING OUT OF THE USE OF OR INABILITY TO USE THIS SOFTWARE EVEN IF NVIDIA HAS
; BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.


(use gl)
(use gl.glut)
(use gl.math3d)
(use gl.cg)
(use gauche.sequence)
(use gauche.uvector)
(use gauche.array)

(define *context* #f) 

;; Choose the vertex and fragment profiles to use.  Try to use
;; CG_PROFILE_ARBVFP1 and CG_PROFILE_ARBFP1 depending on hardware support.
;; If those aren't available fall back to CG_PROFILE_VP30 and
;; CG_PROFILE_FP30 respectively.

(define *vertex-profile* #f)
(define *fragment-profile* #f)

(define *vertex-program-string*
"
void main(float4 Pobject      : POSITION,
          float3 Nobject      : NORMAL,
          float2 TexUV        : TEXCOORD0,
          float3 diffuse      : TEXCOORD1,
          float3 specular     : TEXCOORD2,
          uniform float4x4 ModelViewProj,
          uniform float4x4 ModelView,
          uniform float4x4 ModelViewIT,

          out float4 HPosition    : POSITION,
          out float3 Peye         : TEXCOORD0,
          out float3 Neye         : TEXCOORD1,
          out float2 uv           : TEXCOORD2,
          out float3 Kd           : COLOR0,
          out float3 Ks           : COLOR1)
{
    // compute homogeneous position of vertex for rasterizer
    HPosition = mul(ModelViewProj, Pobject);
    // transform position and normal from model-space to view-space
    Peye = mul(ModelView, Pobject).xyz;
    Neye = mul(ModelViewIT, float4(Nobject, 0)).xyz;
    // pass uv, Kd, and Ks through unchanged; if they are varying
    // per-vertex, however, they'll be interpolated before being
    // passed to the fragment program.
    uv = TexUV;
    Kd = diffuse;
    Ks = specular;
}
")

(define *fragment-program-string*
"
// Utility functions that return the appropriate components from the vector
// of lighting coefficients returned by the standard library lighting
// funciton, lit().

half diffuse(half4 l) { return l.y; }
half specular(half4 l) { return l.z; }

// Main shader.

half4 main(float3 Peye         : TEXCOORD0,
           half3 Neye         : TEXCOORD1,
           half2 uv           : TEXCOORD2,
           half3 Kd           : COLOR0,
           half3 Ks           : COLOR1,
           uniform sampler2D diffuseMap,
           uniform float3 Plight,
           uniform half3 lightColor,
           uniform half3 shininess) : COLOR
{
    // Normalize surface normal, vector to light source, and vector
    // to the viewer
    half3 N = normalize(Neye);
    half3 L = normalize(Plight - Peye);
    half3 V = normalize(-Peye);

    // Compute half-angle vector for specular lighting
    half3 H = normalize(L + V);

    // Compute lighting values.  lit() returns the diffuse coefficient
    // in y (or zero, if NdotL < 0), and the specular coefficient in z
    // (or zero, also if NdotL < 0).
    half NdotL = dot(N, L), NdotH = dot(N, H);
    half4 lighting = lit(NdotL, NdotH, shininess);

    // Compute overall color for the fragment.  Scale sum of diffuse
    // and specular contributions together and by the light color.

    half tmp = (half)tex2D(diffuseMap, uv).x;
    half3 tmp3 = half3(tmp,tmp,tmp);

    // half eps = 0.005;
    // half tmp = 0.5*(1.0+sin(16*3.1415927*uv.x)*sin(16*3.1415927*uv.y));
    // tmp = tmp < (0.5-eps) ? 0.1 : (tmp > 0.5+eps) ? 0.7 : 0.5;
    // half3 tmp3 = half3(tmp,tmp,tmp);

    half3 C = lightColor *
         (diffuse(lighting) * Kd * tmp3 +
         specular(lighting) * Ks);

    // Here is a way to do ad-hoc antialiasing by turning on alpha-blending 
    // wherever the normal is roughly perpendicular to the viewing vector.
    // -ijt
    // half epsz = 0.2; 
    // return half4(C, abs(N.z)<=epsz?0.2:1.0);  

    return half4(C, 1);     // Set the alpha value to 1.
}
")

(define *vertex-program* #f)
(define *fragment-program* #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Main program; do basic GLUT and Cg setup but leave most of the work
;; to the on-display() function.

(define (main args)
    (glut-init args)
    (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB GLUT_DEPTH GLUT_ALPHA))
    (glut-init-window-size 512 512)
    (glut-create-window "Cg checkered sphere demo")
    (glut-keyboard-func keyboard)
    (glut-display-func on-display)
    (glut-main-loop)
    0)

;; display callback function
(define on-display
  (let ((have-initialized #f)
        (cur-time 0))
    (lambda ()
      (if (not have-initialized)
        (begin
          ;; Do one-time setup only once; setup Cg programs and textures
          ;; and set up Open-gL state.
          (load-gpu-programs)
          (load-textures)
          (gl-enable GL_DEPTH_TEST)
          (gl-enable GL_BLEND)
          (gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
          (set! have-initialized #t)
          ))

      ;; The usual Open-gL stuff to clear the screen and set up viewing.
      (gl-clear-color .25 .25 .25 1.)
      (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

      (gl-matrix-mode GL_PROJECTION)
      (gl-load-identity)
      (glu-perspective 30. 1.0 .1 100)

      (gl-matrix-mode GL_MODELVIEW)
      (gl-load-identity)
      (glu-look-at  4 4 -4  0 0 0  0 1 0)

      ;; Make the object rotate a bit each time the display function
      ;; is called
      (gl-rotate (modulo cur-time 360) 0 1 0)

      ;; Now make sure that the vertex and fragment programs loaded
      ;; in Load-cg-programs) are bound.
      (cg-gl-bind-program *vertex-program*)
      (cg-gl-bind-program *fragment-program*)

      ;; Bind uniform parameters to vertex shader
      (cg-gl-set-state-matrix-parameter 
        (cg-get-named-parameter *vertex-program* "ModelViewProj") 
        CG_GL_MODELVIEW_PROJECTION_MATRIX 
        CG_GL_MATRIX_IDENTITY)
      (cg-gl-set-state-matrix-parameter 
        (cg-get-named-parameter *vertex-program* "ModelView") 
        CG_GL_MODELVIEW_MATRIX 
        CG_GL_MATRIX_IDENTITY)
      (cg-gl-set-state-matrix-parameter 
        (cg-get-named-parameter *vertex-program* "ModelViewIT") 
        CG_GL_MODELVIEW_MATRIX 
        CG_GL_MATRIX_INVERSE_TRANSPOSE)

      ;; We can also go ahead and bind varying parameters to vertex shader
      ;; that we just want to have the same value for all vertices.  The
      ;; vertex shader could be modified so that these were uniform for
      ;; better efficiency but this gives us flexibility for the future.
      (cg-gl-set-parameter
        (cg-get-named-parameter *vertex-program* "diffuse") 
        '#f32(.7 .2 .2))
      (cg-gl-set-parameter 
        (cg-get-named-parameter *vertex-program* "specular") 
        '#f32(.9 .9 .9))

      ;; Now bind uniform parameters to fragment shader
      (cg-gl-set-parameter 
        (cg-get-named-parameter *fragment-program* "Plight") 
        #,(vector4f 3 2 -3 1))
      (cg-gl-set-parameter 
        (cg-get-named-parameter *fragment-program* "lightColor") 
        '#f32(1 1 1))
      (cg-gl-set-parameter (cg-get-named-parameter *fragment-program* "shininess") 
                           40)

      ;; And finally enable the approprate texture for fragment shader; the
      ;; texture was originally set up in load-textures).
      (cg-gl-enable-texture-parameter 
        (cg-get-named-parameter *fragment-program* "diffuseMap"))

      ;; And go ahead and draw the scene geometry
      (draw-geometry)
      ;; Disable the texture now that we're done with it.
      (cg-gl-disable-texture-parameter 
        (cg-get-named-parameter *fragment-program* "diffuseMap"))

      (glut-swap-buffers)
      (inc! cur-time)

      ;; Force another display so that the object keeps moving
      (glut-post-redisplay)
      )))

(define GL_GENERATE_MIPMAP #x8191)

(define (load-textures)
  ;; There is only one texture needed here--we'll set up a basic
  ;; checkerboard--which is used to modulate the diffuse channel in the
  ;; fragment shader.
  (let ((handle (ref (gl-gen-textures 1) 0)))
    ;; Basic Open-gL texture state setup
    (gl-bind-texture GL_TEXTURE_2D handle)
    (auto-generate-mipmaps! #t)
    (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
    (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
    (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)

    ;; Fill in the texture map.
    (let* ((res 256)
           (data (make-f32array (shape 0 res  0 res) 0.0))
           (bleh (/ res 16)))
      (dotimes (i res)
        (dotimes (j res)
          (if (= 0 (modulo (+ (floor (/ i bleh)) (floor (/ j bleh))) 2))
            (array-set! data i j 0.1)
            (array-set! data i j 0.7)
            )))

      (gl-tex-image-2d GL_TEXTURE_2D 0 GL_LUMINANCE res res 0 GL_LUMINANCE GL_FLOAT 
                       (array->f32vector data))

      ;; Tell Cg which texture handle should be associated with the sampler2D
      ;; parameter to the fragment shader.
      (cg-gl-set-texture-parameter 
       (cg-get-named-parameter *fragment-program* "diffuseMap") 
       handle)
      )))

;; This seems inefficient.  Is there a way to get a pointer to the start of the
;; array's memory so we can just hand it off to OpenGL or Cg?  -ijt
(define (array->f32vector a)
  (vector->f32vector (array->vector a)))

(define pi (imag-part (log -1.0)))

(define nu 60)
(define nv 60)
(define us (tabulate-array (shape 0 nu)  (lambda (iu) (/ iu (- nu 1)))))
(define vs (tabulate-array (shape 0 nv)  (lambda (iv) (/ iv (- nv 1)))))
(define uvs
  (let ((uvs (make-f32array (shape 0 nu  0 nv  0 2) 0.0)))
    (dotimes (iu nu)
             (dotimes (iv nv)
                      (array-set! uvs iu iv 0 (array-ref us iu))
                      (array-set! uvs iu iv 1 (array-ref vs iv))
                      ))
    (array->f32vector uvs)))

(define thetas (array-map (cut * pi <>) us))
(define phis (array-map (cut * 2.0 pi <>) vs))
(define P   ; points
  (let ((xyz (make-f32array (shape 0 nu  0 nv  0 3) 0.0)))
    (dotimes (iu nu)
             (dotimes (iv nv)
                      (let* ((theta (array-ref thetas iu))
                             (phi (array-ref phis iv)))
                        (array-set! xyz iu iv 0 (* (sin theta) (sin phi)))
                        (array-set! xyz iu iv 1 (* (sin theta) (cos phi)))
                        (array-set! xyz iu iv 2 (* (cos theta))))))
    (array->f32vector xyz)))

(define N P)  ; normals

(define ntris (* 2 (- nu 1) (- nv 1)))
(define indices-per-tri 3)

(define indices
  (let* ((indices (make-u32vector (* ntris indices-per-tri)))
         (vert-index (lambda (iu iv) (+ iu (* iv nu))))
         (k -1)
         (set-next! (lambda (a b)
                      (u32vector-set! indices (inc! k)  (vert-index a b)))))
    (dotimes (iu (- nu 1))
             (dotimes (iv (- nv 1))
                      (set-next! iu iv)
                      (set-next! (+ iu 1) iv)
                      (set-next! (+ iu 1) (+ iv 1))

                      (set-next! iu iv)
                      (set-next! (+ iu 1) (+ iv 1))
                      (set-next! iu (+ iv 1))
                      ))
    indices))

;; Geometry creation and drawing function; we'll just draw a sphere.
(define draw-geometry
  (let ((first-time #t)
        (display-list #f))
    (lambda ()
      ;; Tell Cg which of these data pointers are associated with which
      ;; parameters to the vertex shader so that when we call
      ;; cg-gLEnable-client-state) and then gl-draw-elements) the shader
      ;; gets the right input information.
      (if first-time
          (begin
            (cg-gl-set-parameter-pointer 
             (cg-get-named-parameter *vertex-program* "Pobject")
             3 GL_FLOAT 0 P)

            (cg-gl-set-parameter-pointer
             (cg-get-named-parameter *vertex-program* "Nobject")
             3 GL_FLOAT 0 N)

            (cg-gl-set-parameter-pointer
             (cg-get-named-parameter *vertex-program* "TexUV")
             2 GL_FLOAT 0 uvs)
            
            ;; In an earlier version of the ported demo, I was passing
            ;; in (array->f32vector uvs) since uvs was an array.
            ;; This produced undesired results because the temporary f32vector 
            ;; was then garbage collected after a few seconds.  
            
            (set! first-time #f)))

      (let ((names (list "Pobject" "Nobject" "TexUV")))  
        ;; Enable the bindings to the parameters
        (for-each
         (lambda (name)
           (cg-gl-enable-client-state
            (cg-get-named-parameter *vertex-program* name)))
         names)

        ;; Enable the texture parameter as well.
        (cg-gl-enable-texture-parameter
         (cg-get-named-parameter *fragment-program* "diffuseMap"))

        ;; And now draw the geometry.
        (cond 
         (display-list (gl-call-list display-list))
         (else 
          (set! display-list (gl-gen-lists 1))
          (gl-new-list display-list GL_COMPILE_AND_EXECUTE)
          (gl-draw-elements GL_TRIANGLES indices)
          (gl-end-list)
          ))

        ;; Be a good citizen and disable the various bindings we set up above.
        (for-each 
         (lambda (name)
           (cg-gl-disable-client-state
            (cg-get-named-parameter *vertex-program* name)))
         names)

        (cg-gl-disable-texture-parameter 
         (cg-get-named-parameter *fragment-program* "diffuseMap"))
        ))))

(define (keyboard key x y)
  (let ((ikey (if (char? key) 
               (char->integer key) 
               key)))
    (case key
      (('q' 'Q' #\escape) 
       (cg-destroy-context *context*)
       (exit 0)))))

(define (load-gpu-programs)
  (set! *context* (cg-create-context))
  (set! *vertex-profile*
    (cond
     ((cg-gl-is-profile-supported CG_PROFILE_ARBVP1) CG_PROFILE_ARBVP1)
     ((cg-gl-is-profile-supported CG_PROFILE_VP30) CG_PROFILE_VP30)
     (else 
      (error "Neither arbvp1 or vp30 vertex profiles supported on this system."))))
  (set! *fragment-profile*
    (cond
     ((cg-gl-is-profile-supported CG_PROFILE_ARBFP1) CG_PROFILE_ARBFP1)
     ((cg-gl-is-profile-supported CG_PROFILE_FP30) CG_PROFILE_FP30)
     (else
      (error "Neither arbfp1 nor fp30 fragment profiles supported"))))
  (set! *vertex-program* (cg-create-program *context* 
                                            CG_SOURCE *vertex-program-string*
                                            *vertex-profile* "main" '() ))
  (set! *fragment-program* (cg-create-program *context* CG_SOURCE 
                                              *fragment-program-string*
                                              *fragment-profile* "main" '() ))
  (cg-compile-program *vertex-program*)
  (cg-compile-program *fragment-program*)
  (cg-gl-enable-profile *vertex-profile*)
  (cg-gl-enable-profile *fragment-profile*)
  (cg-gl-load-program   *vertex-program*)
  (cg-gl-load-program   *fragment-program*)
  )

