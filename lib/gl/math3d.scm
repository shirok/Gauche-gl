;;;
;;; gl/math3d.scm - auxiliary vector arithmetics for 3D graphics
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: math3d.scm,v 1.1 2002-09-27 10:29:10 shirok Exp $
;;;

(define-module gl.math3d
  (export-all)
  )

(select-module gl.math3d)

(dynamic-load "gauche-math3d")

(provide "gl/math3d")
