/*
 * gauche-cg.h - Gauche CG extension
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
 *  $Id: gauche-cg.h,v 1.1 2005-06-14 21:53:09 shirok Exp $
 */

#ifndef GAUCHE_CG_H
#define GAUCHE_CG_H

#include <Cg/cg.h>
#include <Cg/cgGL.h>

/* This is the context that was most recently used during a compilation. */
extern CGcontext gauche_cg_context; 

/* Context */
typedef struct Scm_CGcontextRec {
  SCM_HEADER;
  int is_valid;
  CGcontext context;
} Scm_CGcontext;

SCM_CLASS_DECL(Scm_CGcontextClass); 
#define SCM_CLASS_CG_CONTEXT   (&Scm_CGcontextClass) 
#define SCM_CG_CONTEXT(obj)    ((Scm_CGcontext*)(obj))   /* unboxer */
#define SCM_CG_CONTEXT_P(obj)  SCM_XTYPEP(obj, SCM_CLASS_CG_CONTEXT)

/* Program */
typedef struct Scm_CGprogramRec {
  SCM_HEADER;
  int is_valid;
  CGprogram program;
} Scm_CGprogram;

SCM_CLASS_DECL(Scm_CGprogramClass); 
#define SCM_CLASS_CG_PROGRAM    (&Scm_CGprogramClass)
#define SCM_CG_PROGRAM(obj)     ((Scm_CGprogram*)(obj))   
#define SCM_CG_PROGRAM_P(obj)   SCM_XTYPEP(obj, SCM_CLASS_CG_PROGRAM)

/* Parameter */
typedef struct Scm_CGparameterRec {
  SCM_HEADER;
  CGparameter parameter;
} Scm_CGparameter;

SCM_CLASS_DECL(Scm_CGparameterClass); 
#define SCM_CLASS_CG_PARAMETER    (&Scm_CGparameterClass)
#define SCM_CG_PARAMETER(obj)     ((Scm_CGparameter*)(obj))  
#define SCM_CG_PARAMETER_P(obj)   SCM_XTYPEP(obj, SCM_CLASS_CG_PARAMETER)

#endif /* GAUCHE_CG_H */

