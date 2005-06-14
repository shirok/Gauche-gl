;;;
;;; cg.scm - Gauche CG binding
;;;
;;;  Copyright(C) 2005 by Issac Trotts (ijtrotts@ucdavis.edu)
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
;;;  $Id: cg.scm,v 1.1 2005-06-14 21:53:09 shirok Exp $
;;;

(define-module gl.cg
  (use gl)
  (use gl.math3d)
  (export-all)
  )

(select-module gl.cg)

(dynamic-load "libgauche-gl-cg")

(provide "gl/cg")

