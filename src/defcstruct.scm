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
;;   NB: c-name shouldn't include 'data.' prefix.
;;
;;       <slot-spec> := slot-name::<stub-type> ["c-name"]
;;                   |  slot-name::(.*array <stub-type>) "c-name[length-name]"
;;
;;   The second form handles pointer-to-array field, whose length is specified
;;   by another field "length-name" (it's a C field name, not Scheme one).
;;   The field of this memory is allocated and owned by Gauche.

(define-form-parser define-cstruct (scm-name c-struct-name slots . opts)
  (assume-type scm-name <symbol>)
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
         [initializer (assq 'initializer opts)]
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
    (set! (~ cclass'slot-spec) (process-cstruct-slots cclass RecName slots))
    (cgen-decl "#include <gauche/class.h>")
    (cgen-decl #"typedef struct {"
               #"  SCM_HEADER;"
               #"  ~c-struct-name data;"
               #"} ~|RecName|;")
    (cgen-decl #"SCM_CLASS_DECL(~ClassName);")
    (cgen-decl #"#define ~(~ type'c-predicate)(obj) \
                         SCM_ISA(obj,&~|ClassName|)")
    (cgen-decl #"#define ~(~ type'unboxer)(obj) &(((~RecName *)(obj))->data)")
    (cgen-decl #"SCM_EXTERN ScmObj ~(~ type'boxer)(const ~|c-struct-name|*);")
    (cgen-body #"ScmObj ~(~ type'boxer)(const ~|c-struct-name| *v)"
               #"{"
               #"  ~RecName *z = SCM_NEW(~RecName);"
               #"  SCM_SET_CLASS(z, &~ClassName);"
               #"  z->data = *v;"
               (if initializer
                 #"{ ~|c-struct-name| *obj = &z->data;\n~(cadr initializer)\n}"
                 "")
               #"  return SCM_OBJ(z);"
               #"}")
    (cgen-add! cclass)))

(define (process-cstruct-slots cclass cclass-cname slots)
  (define (make-slot name c-name)
    ;; pasing of name::type is dupe from make-arg, but slightly differ.
    (let1 namestr (symbol->string name)
      (receive (realname-s typename-s) (string-scan namestr "::" 'both)
        (let* ([realname (if realname-s (string->symbol realname-s) name)]
               [typename (if typename-s (string->symbol typename-s) '<top>)]
               [c-name   (or c-name (symbol->string realname))])
          `(,realname :type ,typename :c-name ,c-name)))))
  (define (symbol::? x)
    (and (symbol? x) (#/::$/ (symbol->string x))))
  (let loop ([slots slots] [r '()])
    (match slots
      [() (process-cclass-slots cclass (reverse r))]
      [((? symbol? y) (? string? n) . slots)
       (loop slots (cons (make-slot y n) r))]
      [((? symbol::? y) . rest)
       (match rest
         [(('.array* type) (? string? n) . slots)
          (loop slots (cons (make-ptr-to-array-slot cclass cclass-cname y type n) r))]
         [else
          (errorf <cgen-stub-error> "bad slot spec in define-cstruct: ~s"
                  (cons y (take* rest 2)))])]
      [((? symbol? y) . slots)
       (loop slots (cons (make-slot y #f) r))]
      [(bad . slots)
       (errorf <cgen-stub-error> "bad slot spec in define-cstruct: ~s" bad)])))

;; Handle slot::(.array* <elt-type>) "c-name"
;;   c-name : "c-field[c-length]"
;;   cclass-cname is the C typename of the wrapper.
;; Returns slot description.
;; TODO: Make c-length field read-only.
(define (make-ptr-to-array-slot cclass cclass-cname name:: 
                                elt-type-name slot-c-name)
  (define (parse-c-name c-name)
    (rxmatch-case c-name
      [#/^(\w+)\[(\w+)\]$/ (_ field length) (values field length)]
      [else (errorf <cgen-stub-error> 
                    "C field name for .array* slot needs to have \
                     \"field[length]\" format, but got:" c-name)]))
  (define etype (name->type elt-type-name))
  (define slot-name (string->symbol
                     (string-drop-right (symbol->string name::) 2)))
  (define (gen-getter c-field c-length) ; returns getter name
    (rlet1 getter-name #"~(~ cclass 'c-name)_~|c-field|_GET"
      (cgen-decl #"static ScmObj ~|getter-name|(ScmObj);")
      (cgen-body #"static ScmObj ~|getter-name|(ScmObj obj_s)"
                 #"{"
                 #"  ~|cclass-cname|* obj = (~|cclass-cname|*)obj_s;"
                 #"  if (obj->data.~|c-length| <= 0) return SCM_FALSE;"
                 #"  ScmObj v = Scm_MakeVector(obj->data.~|c-length|, SCM_FALSE);"
                 #"  for (ScmSmallInt i = 0; i < obj->data.~|c-length|; i++) {"
                 #"    SCM_VECTOR_ELEMENT(v, i) ="
                 #"      ~(~ etype'boxer)(obj->data.~|c-field|[i]);"
                 #"  }"
                 #"  return SCM_OBJ(v);"
                 #"}")))
  (define (gen-setter c-field c-length)
    (rlet1 setter-name #"~(~ cclass 'c-name)_~|c-field|_SET"
      (cgen-decl #"static void ~|setter-name|(ScmObj, ScmObj);")
      (cgen-body #"static void ~|setter-name|(ScmObj obj_s, ScmObj val)"
                 #"{"
                 #"  ~|cclass-cname|* obj = (~|cclass-cname|*)obj_s;"
                 #"  if (SCM_FALSEP(val)) {"
                 #"    obj->data.~|c-length| = 0;"
                 #"    obj->data.~|c-field| = NULL;"
                 #"    return;"
                 #"  }"
                 #"  if (!SCM_VECTORP(val)) SCM_TYPE_ERROR(val, \"vector\");"
                 #"  ScmSmallInt len = SCM_VECTOR_SIZE(val);"
                 #"  ~(~ etype'c-type) *vs = SCM_NEW_ARRAY(~(~ etype'c-type), len);"
                 #"  for (ScmSmallInt i = 0; i < len; i++) {"
                 #"    vs[i] = ~(~ etype'unboxer)(SCM_VECTOR_ELEMENT(val,i));"
                 #"  }"
                 #"  obj->data.~|c-length| = len;"
                 #"  obj->data.~|c-field| = vs;"
                 #"}")))
  (receive (c-field c-length) (parse-c-name slot-c-name)
    `(,slot-name :type <vector>
                 :c-name ,c-field
                 :getter (c ,(gen-getter c-field c-length))
                 :setter (c ,(gen-setter c-field c-length)))))

;; define-cenum scm-type c-type-name (enum ...)
;;   This combines define-type and define-enum.
(define-form-parser define-cenum (scm-name c-type-name enums)
  (assume-type scm-name <symbol>)
  (assume-type c-type-name <string>)
  (assume-type enums <list>)
  (make-cgen-type scm-name c-type-name #f
                  "SCM_INTP" "SCM_INT_VALUE" "SCM_MAKE_INT")
  (dolist [e enums]
    (variable-parser-common #t e `(c ,#"Scm_MakeInteger(~e)") '())))
