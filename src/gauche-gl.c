/*
 * gauche-gl.c - Gauche GL binding
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: gauche-gl.c,v 1.1 2001-09-28 20:27:24 shirok Exp $
 */

#include <gauche.h>

extern void Scm_Init_gl_lib(ScmModule *mod);

void Scm_Init_gauche_gl(void)
{
    ScmModule *mod = SCM_MODULE(SCM_FIND_MODULE("gl", TRUE));
    Scm_Init_gl_lib(mod);
}
