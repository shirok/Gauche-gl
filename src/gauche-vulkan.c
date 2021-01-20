/*
 * gauche-vulkan.c - Gauche Vulkan binding
 *
 *   Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
 * 
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 * 
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <gauche.h>
#include <gauche/class.h>
#include <gauche/extend.h>
#include "gauche-vulkan.h"

extern void Scm_Init_vulkan_lib(ScmModule *mod);

/*================================================================
 * VkVoidPointer
 */

ScmClass *Scm_VkVoidPointerClass;

static void vk_void_pointer_print(ScmObj obj, ScmPort *sink, 
                                  ScmWriteContext *m SCM_UNUSED)
{
    Scm_Printf(sink, "#<vk-void* %p>", SCM_VK_VOID_POINTER(obj));
}

ScmObj Scm_MakeVkVoidPointer(const void *p)
{
    return Scm_MakeForeignPointer(Scm_VkVoidPointerClass, (void*)p);
}


/*================================================================
 * Initialization
 */
void Scm_Init_libgauche_vulkan(void)
{
    ScmModule *mod;
    SCM_INIT_EXTENSION(libgauche_vulkan);
    mod = SCM_MODULE(SCM_FIND_MODULE("gl.vulkan", TRUE));

    Scm_MakeForeignPointerClass(mod, "<vk-void*>",
                                vk_void_pointer_print,
                                NULL,
                                0);
    Scm_Init_vulkan_lib(mod);
}

