/*
 * gauche-glut.c - Gauche GLUT binding
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
 *  $Id: gauche-glut.c,v 1.1 2001-09-29 11:56:39 shirok Exp $
 */

#include <gauche.h>

extern void Scm_Init_glut_lib(ScmModule *mod);

void Scm_Init_gauche_glut(void)
{
    ScmModule *mod = SCM_MODULE(SCM_FIND_MODULE("gl", TRUE));
    Scm_Init_glut_lib(mod);
}
