;;;
;;; vulkan-lib.scm - vulkan binding
;;;
;;;  Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(select-module gl.vulkan)

(inline-stub
 (declcode (.include "gauche-vulkan.h"))

 (define-type <vk-structure-type> "VkStructureType" #f
   "SCM_INTP" "SCM_INT_VALUE" "SCM_MAKE_INT")

 (define-cstruct <vk-application-info> "VkApplicationInfo"
   (stype::<vk-structure-type>         "sType"
    ;; pNext
    application-name::<const-cstring>  "pApplicationName"
    application-version::<uint32>      "applicationVersion"
    engine-name::<const-cstring>       "pEngineName"
    engine-version::<uint32>           "engineVersion"
    api-version::<uint32>              "apiVersion"
    )
   (initializer "obj->sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;\n\
                 obj->pNext = NULL;"))
 )
    

