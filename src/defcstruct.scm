;;
;; This extends cgen with define-cstruct form.  Eventually this will be
;; incorporated in Gauche.
;;

(use gauche.cgen.stub)
(select-module gauche.cgen.stub)
(use util.match)

;; (define-cstruct scheme-name c-struct-name
;;   (<slot-spec> ...)
;;   )
;;
;;   This is to generate a stub to access plain old C struct, wrapped
;;   in a Scheme Object.  The C struct is embedded in the Scheme object,
;;   and its memory and lifetime are goverened by Gauche GC.
;;   This isn't a form for a C struct that is allocated and owned by
;;   the external library. 
;;
;;   - The C struct is defined elsewhere (e.g. defined by the API).
;;     We call it c_struct_t here.
;;
;;   - The stub creates a Scheme object that embeds the structure:
;;
;;       struct {
;;          SCM_HEADER;
;;          c_struct_t data;
;;       };
;;
;;   - The Scheme class specified by scheme-name is defined to represent
;;     the above Scheme structure.
;;
;;   - When it is passed from Scheme world to C world, the pointer to
;;     the data field is passed.
;;
;;   - When C world returns it to the Scheme world, the content of the
;;     C struct is copied into a Scheme-owned structure.
;;
;;   <slot-spec> is similar to the cproc argument, optionally followed
;;   by a string c-name if the C member name differs from the Scheme name.
;;
;;       <slot-spec> := slot-name::stub-type ["c-name"]
;;
;;   NB: c-name shouldn't include 'data.' prefix.

(define-form-parser define-cstruct (scm-name c-struct-name slots)
  (check-arg symbol? scm-name)
  (assume-type c-struct-name <string>)
  (assume-type slots <list>)
  (let* ([TYPENAME ($ cgen-safe-name-friendly
                      $ (cut regexp-replace-all* <>  #/[<>]/ "")
                      $ symbol->string scm-name)]
         [ClassName #"Scm_~|TYPENAME|_Class"]
         [RecName #"Scm_~|TYPENAME|_Rec"]
         [BoxerName #"Scm_Make_~|TYPENAME|"]
         [type (make-cgen-type scm-name #"~|c-struct-name|*" #f
                               #f #f BoxerName)]
         [cclass (make <cclass>
                   :scheme-name scm-name 
                   :c-type #"~|c-struct-name|*"
                   :c-name ClassName
                   :qualifiers '()
                   :cpa '()
                   :direct-supers '()
                   :allocator #f
                   :printer #f
                   :comparer #f)])
    (set! (~ cclass'slot-spec) 
          (process-cclass-slots cclass (process-cstruct-slots slots)))
    (cgen-decl "#include <gauche/class.h>")
    (cgen-decl #"typedef struct {"
               #"  SCM_HEADER;"
               #"  ~c-struct-name data;"
               #"} ~|RecName|;")
    (cgen-decl #"SCM_CLASS_DECL(~ClassName);")
    (cgen-decl #"#define ~(~ type'c-predicate)(obj) \
                         SCM_ISA(obj,~|ClassName|)")
    (cgen-decl #"#define ~(~ type'unboxer)(obj) &(((~RecName *)(obj))->data)")
    (cgen-decl #"SCM_EXTERN ScmObj ~(~ type'boxer)(const ~|c-struct-name|*);")
    (cgen-body #"ScmObj ~(~ type'boxer)(const ~|c-struct-name| *v)"
               #"{"
               #"  ~RecName *z = SCM_NEW(~RecName);"
               #"  SCM_SET_CLASS(z, &~ClassName);"
               #"  z->data = *v;"
               #"  return SCM_OBJ(z);"
               #"}")
    (cgen-add! cclass)))

(define (process-cstruct-slots slots)
  (define (make-slot name c-name)
    ;; pasing of name::type is dupe from make-arg, but slightly differ.
    (let1 namestr (symbol->string name)
      (receive (realname-s typename-s) (string-scan namestr "::" 'both)
        (let* ([realname (if realname-s (string->symbol realname-s) name)]
               [typename (if typename-s (string->symbol typename-s) '<top>)]
               [c-name   (or c-name (symbol->string realname))])
          `(,realname :type ,typename :c-name ,c-name)))))
  (let loop ([slots slots] [r '()])
    (match slots
      [() (reverse r)]
      [((? symbol? y) (? string? n) . slots)
       (loop slots (cons (make-slot y n) r))]
      [((? symbol? y) . slots)
       (loop slots (cons (make-slot y #f) r))]
      [(bad . slots)
       (errorf <cgen-stub-error> "bad slot spec in define-cstruct: ~s" bad)])))
