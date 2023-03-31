(define-module load-shaders
  (use gl)
  (use file.util)
  (use util.match)
  (export read-shader
          load-shaders))
(select-module load-shaders)

(define *source-dir* (sys-dirname (current-load-path)))

;; Read <filename> relative to the source
(define (read-shader filename)
  (let1 path (if (relative-path? filename)
               (build-path *source-dir* filename)
               filename)
    (list (file->string path :if-does-not-exist #f))))

;; shaders :
;;   ((type filename) ...)
;; returns two values;
;;   program, and list of shaders
(define (load-shaders shaders)
  (define program (gl-create-program))
  (define shader-ids
    (map (^[shader-info]
           (match-let1 [type filename] shader-info
             (and-let1 source (read-shader filename)
               (let1 shader-id (gl-create-shader type)
                 (gl-shader-source shader-id source)
                 (gl-compile-shader shader-id)
                 (if (zero? (gl-get-shader shader-id GL_COMPILE_STATUS))
                   (begin
                     (format (current-error-port)
                             "Shader compilation failed: ~s\n"
                             (gl-get-shader-info-log shader-id))
                     #f)
                   (begin
                     (gl-attach-shader program shader-id)
                     shader-id))))))
         shaders))
  (define (bail-out . args)
    (dolist [shader-id shader-ids]
      (when shader-id (gl-delete-shader shader-id)))
    (apply error args))

  (when (any not shader-ids)
    (bail-out "Couldn't load one or more shaders: " shaders))

  (gl-link-program program)
  (if (zero? (gl-get-program program GL_LINK_STATUS))
    (begin
      (format (current-error-port)
              "Shader linking failed: ~s\n"
              (gl-get-program-info-log program))
      (bail-out "Counldn't link shaders: " shaders))
    (values program shader-ids)))
