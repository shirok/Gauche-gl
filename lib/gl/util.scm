;;;
;;; gl/util.scm - some utilities
;;;
;;;  Copyright(C) 2004 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: util.scm,v 1.1 2004-08-22 12:24:32 shirok Exp $
;;;

(define-module gl.util
  (use gl)
  (use srfi-1)
  (use srfi-2)
  (export gl-extensions-supported?)
  )
(select-module gl.util)

;; Check if the extensions are supported

(define (gl-extensions-supported? required)
  (and-let* ((extstr (gl-get-string GL_EXTENSIONS))
             (exts   (string-split extstr #[\s])))
    (every (cut member <> exts)
           (map x->string required))))

(provide "gl/util")
