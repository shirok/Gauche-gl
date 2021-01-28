;;
;; This extends cgen with define-cstruct form.  Eventually this will be
;; incorporated in Gauche.
;;

(use gauche.cgen.stub)
(select-module gauche.cgen.stub)
(use util.match)

;; Enhance <cslot>

(define-class <cslot> ()
  ((cclass      :init-keyword :cclass)  ; <cclass>
   (scheme-name :init-keyword :scheme-name)
   (c-name      :init-keyword :c-name)
   (c-spec      :init-keyword :c-spec)
   (type        :init-keyword :type   :init-value '<top>)
   (getter      :init-keyword :getter :init-value #t)
   (setter      :init-keyword :setter :init-value #t)
   (init-cexpr  :init-keyword :init-cexpr :init-value #f)
   ))

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
;;   <slot-spec> is the following format:
;;
;;       <slot-spec> := slot-name::<slot-type> ["<c-spec>"]
;;
;;   slot-name is what you see in Scheme world.
;;
;;   <slot-type> specifies the type of slot, and how to box/unbox the
;;   value from C struct.
;;
;;       <slot-type> := <stub-type>
;;                   |  (.array* <stub-type>)
;;                   |  (.array <stub-type>)    ; to be written
;;                   |  &<stub-type>            ; to be written
;;
;;   The '.array*' type denotes a pointer to an array of <stub-type>.
;;   The size of the array must e specified by another field of the struct.
;;   see <c-spec> description below.
;;
;;   <c-spec> is a string encodes C-related info.
;;
;;       <c-spec> := <c-name>? <c-length>? <c-init>?
;;       <c-name> ; C field name.  If omitted, slot-name is used.
;;       <c-length> := '[' <c-name> ']'
;;                  ; This is used for '.array*' slot, specifies the
;;                  ; C field that for the length of the array.
;;       <c-init> := '=' C-literal
;;                  ; Specifies the initial value.
;;
;;   For the '.array*' type, the pointed array is allocated by Gauche
;;   and subject to GC.

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
                   :comparer #f)]
         [slot-specs (cstruct-grok-slot-specs cclass slots)])
    (set! (~ cclass'slot-spec)
          (append-map (cut process-cstruct-slot cclass RecName <>) slot-specs))
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
               #"  z->data = *v;")
    (dolist [slot (~ cclass'slot-spec)]
      (and-let1 init (~ slot'init-cexpr)
        (cgen-body #"  z->data.~(~ slot'c-name) = ~|init|;")))
    (cgen-body (if initializer
                 #"{ ~|c-struct-name| *obj = &z->data;\n~(cadr initializer)\n}"
                 "")
               #"  return SCM_OBJ(z);"
               #"}")
    (cgen-add! cclass)))

;; returns (<cslot> ...)
(define (process-cstruct-slot cclass cclass-cname slot-spec)
  (match-let1 (slot-name type c-field c-length c-init) slot-spec
    (receive (type getter setter)
        (match type
          [('& type)
           (error <cgen-stub-error> "embedding struct isn't supported yet:"
                  slot-spec)]
          [('.array* etype)
           (make-ptr-to-array-getter-setter cclass cclass-cname 
                                            slot-name etype
                                            c-field c-length c-init)]
          [_ (values type #t #t)])
      `(,(make <cslot>
           :cclass cclass :scheme-name slot-name :type (name->type type)
           :c-name (get-c-name "" c-field)
           :c-spec #f
           :getter getter :setter setter
           :init-cexpr c-init)))))

;; Returns ((slot-name type c-field c-length c-init) ...)
;; type can be (& type) for embedded types
(define (cstruct-grok-slot-specs cclass slots)
  ;; parse slot-name::type.  Returns slot name symbol and type symbol.
  (define (parse-symbol::type sym)
    (rxmatch-case (x->string sym)
      [#/^(.*?)(::(&)?(.*))?$/ (_ name-s _ embed type-s)
       (let* ([etype (if type-s (string->symbol type-s) '<top>)]
              [type (if (equal? embed "&") `(& ,etype) etype)])
         (values (string->symbol name-s) type))]
      [_ (error <cgen-stub-error> "bad slot name::type:" sym)]))
  ;; parse c-spec.  Returns c-field, c-length and init
  ;; cclass and slot-name are only for error message.
  (define (parse-c-spec slot-name c-spec)
    (rxmatch-case c-spec
      [#/^(\w+)?(?:\[(\w+)\])?(?:=(.*))?$/ (_ field length init)
          (values field length init)]
      [else
       (errorf "Bad c-spec ~s for a slot ~s of ~s" c-spec slot-name
               (~ cclass'scheme-name))]))    
  (define (grok-1 slots)
    (match slots
      [((? symbol? y) . rest)
       (if (#/::$/ (symbol->string y))
         (match rest
           [((and ('.array* etype) type) (? string? c-spec) . rest)
            (receive (slot-name _) (parse-symbol::type y)
              (receive (c-field c-length c-init)
                  (parse-c-spec slot-name c-spec)
                (values (list slot-name type c-field c-length c-init) rest)))]
           [_ (error <cgen-stub-error> "bad slot spec in define-cstruct:"
                     (take* slots 2))])
         (match rest
           [((? string? c-spec) . rest)
            (receive (slot-name type) (parse-symbol::type y)
              (receive (c-field c-length c-init)
                  (parse-c-spec slot-name c-spec)
                (values (list slot-name type
                              (or c-field (x->string slot-name))
                              c-length c-init)
                        rest)))]
           [_  (receive (slot-name type) (parse-symbol::type y)
                 (values (list slot-name type (x->string slot-name) #f #f) 
                         rest))]))]
      [_ (error <cgen-stub-error> "bad slot spec in define-cstruct:" slots)]))
  (let loop ([slots slots] [r '()])
    (if (null? slots)
      (reverse r)
      (receive (slot rest) (grok-1 slots)
        (loop rest (cons slot r))))))

;; Handle slot::(.array* <elt-type>) "c-name"
;;   c-name : "c-field[c-length]"
;;   cclass-cname is the C typename of the wrapper.
;; Returns stub-type, getter-name, setter-name
;; TODO: Make c-length field read-only.
(define (make-ptr-to-array-getter-setter cclass cclass-cname
                                         slot-name elt-type-name
                                         c-field c-length c-init)
  (define etype (name->type elt-type-name))
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
  (values '<vector>
          `(c ,(gen-getter c-field c-length))
          `(c ,(gen-setter c-field c-length))))

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
