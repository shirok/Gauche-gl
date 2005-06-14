/*
 *  gauche-cg.c - Gauche Cg binding
 *
 *  Copyright(C) 2005 by Issac Trotts (ijtrotts@ucdavis.edu)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: gauche-cg.c,v 1.1 2005-06-14 21:53:09 shirok Exp $
 */

#include <stdio.h>
#include <gauche.h>
#include <gauche/extend.h>
#include "gauche-cg.h"

CGcontext gauche_cg_context = NULL; 

SCM_DEFINE_BUILTIN_CLASS(Scm_CGcontextClass, NULL, NULL, NULL, NULL, NULL);
SCM_DEFINE_BUILTIN_CLASS(Scm_CGprogramClass, NULL, NULL, NULL, NULL, NULL);
SCM_DEFINE_BUILTIN_CLASS(Scm_CGparameterClass, NULL, NULL, NULL, NULL, NULL);

/* Error handling */
static void error_callback(void)
{
  CGerror e = cgGetError();
  if(e == CG_COMPILER_ERROR) {
    Scm_Error("%s",cgGetLastListing(gauche_cg_context));
  }
  else {
    Scm_Error("%s",cgGetErrorString(e));
  }
}

/* Initialization */
extern void Scm_Init_cg_lib(ScmModule *mod);

void Scm_Init_libgauche_gl_cg(void)
{
    ScmModule *mod;
    SCM_INIT_EXTENSION(gauche_cg);
    mod = SCM_MODULE(SCM_FIND_MODULE("gl.cg", TRUE));
    Scm_InitBuiltinClass(&Scm_CGcontextClass, "<cg-context>", NULL, 0, mod);
    Scm_InitBuiltinClass(&Scm_CGprogramClass, "<cg-program>", NULL, 0, mod);
    Scm_InitBuiltinClass(&Scm_CGparameterClass, "<cg-parameter>", NULL, 0, mod);
    Scm_Init_cg_lib(mod);

    cgSetErrorCallback(error_callback);
}

