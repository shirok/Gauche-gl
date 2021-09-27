;;
;; This extends cgen with define-cstruct form.  Eventually this will be
;; incorporated in Gauche.
;;

(use gauche.cgen.stub)
(select-module gauche.cgen.stub)
(use util.match)
(use gauche.mop.instance-pool)

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

;; Extend <cclass> with instance pools.

(define-class <cclass> (<stub> <instance-pool-mixin>)
  ((cpa        :init-keyword :cpa       :init-value '())
   (c-type     :init-keyword :c-type)
   (qualifiers :init-keyword :qualifiers)
   (allocator  :init-keyword :allocator :init-value #f)
   (printer    :init-keyword :printer   :init-value #f)
   (comparer   :init-keyword :comparer  :init-value #f)
   (slot-spec  :init-keyword :slot-spec :init-value '())
   (direct-supers :init-keyword :direct-supers :init-value '())
   ))

(define (scheme-name->cclass sym)
  (instance-pool-find <cclass> (^o (eq? (~ o'scheme-name) sym))))

;; To make this work with 0.9.10 and later
(define (cgen-boxer-name type)
  (if (assq '%boxer (class-slots <cgen-type>))
    (~ type'%boxer)
    (~ type'boxer)))

(define (cgen-unboxer-name type)
  (if (assq '%unboxer (class-slots <cgen-type>))
    (~ type'%unboxer)
    (~ type'unboxer)))

(define (cgen-pred-name type)
  (if (assq '%c-predicate (class-slots <cgen-type>))
    (~ type'%c-predicate)
    (~ type'c-predicate)))

(define (cgen-box-expr type c-expr)
  (if (memq (~ type'name) '(<real> <float>))
    #"Scm_MakeFlonum(~c-expr)"
    #"~(cgen-boxer-name type)(~c-expr)"))


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
    (cgen-decl #"#define ~(cgen-pred-name type)(obj) \
                         SCM_ISA(obj,&~|ClassName|)")
    (cgen-decl #"#define ~(cgen-unboxer-name type)(obj) \
                         &(((~RecName *)(obj))->data)")
    (cgen-decl #"SCM_EXTERN ScmObj ~(cgen-boxer-name type)(const ~|c-struct-name|*);")
    (cgen-body #"ScmObj ~(cgen-boxer-name type)(const ~|c-struct-name| *v)"
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
          [('& rtype)
           (make-embedded-struct-getter-setter cclass cclass-cname
                                               slot-name rtype
                                               c-field c-length c-init)]
          [('.array etype)
           (make-array-getter-setter cclass cclass-cname
                                     slot-name etype #f
                                     c-field c-length c-init)]
          [('.array* etype)
           (make-array-getter-setter cclass cclass-cname
                                     slot-name etype #t
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
          (values (or field (x->string slot-name))
                  (and length (or (string->number length) length))
                  init)]
      [else
       (errorf "Bad c-spec ~s for a slot ~s of ~s" c-spec slot-name
               (~ cclass'scheme-name))]))
  (define (grok-1 slots)
    (match slots
      [((? symbol? y) . rest)
       (if (#/::$/ (symbol->string y))
         (match rest
           [((and ((or '.array* '.array) etype) type) (? string? c-spec) . rest)
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

;; Handle slot::(.array[*] <elt-type>) "c-name"
;;   c-name : "c-field[c-length]"
;;   cclass-cname is the C typename of the wrapper.
;; Returns stub-type, getter-name, setter-name
;; TODO: Make c-length field read-only.
(define (make-array-getter-setter cclass cclass-cname
                                  slot-name elt-type-name ptr?
                                  c-field c-length c-init)
  (define etype (name->type elt-type-name))
  (define (gen-getter c-field c-length) ; returns getter name
    (rlet1 getter-name #"~(~ cclass 'c-name)_~|c-field|_GET"
      (cgen-decl   #"static ScmObj ~|getter-name|(ScmObj);")
      (cgen-body   #"static ScmObj ~|getter-name|(ScmObj obj_s)"
                   #"{"
                   #"  ~|cclass-cname|* obj = (~|cclass-cname|*)obj_s;")
      (if (string? c-length)
        (cgen-body #"  if (obj->data.~|c-length| <= 0) return SCM_FALSE;"
                   #"  ssize_t len = obj->data.~|c-length|;")
        (cgen-body #"  ssize_t len = ~|c-length|;"))
      (cgen-body   #"  ScmObj v = Scm_MakeVector(len, SCM_FALSE);"
                   #"  for (ScmSmallInt i = 0; i < len; i++) {"
                   #"    ~(~ etype'c-type) e = obj->data.~|c-field|[i];"
                   #"    SCM_VECTOR_ELEMENT(v, i) ="
                   #"      ~(cgen-box-expr etype \"e\");"
                   #"  }"
                   #"  return SCM_OBJ(v);"
                   #"}")))
  (define (gen-setter c-field c-length)
    (rlet1 setter-name #"~(~ cclass 'c-name)_~|c-field|_SET"
      (cgen-decl   #"static void ~|setter-name|(ScmObj, ScmObj);")
      (cgen-body   #"static void ~|setter-name|(ScmObj obj_s, ScmObj val)"
                   #"{"
                   #"  ~|cclass-cname|* obj = (~|cclass-cname|*)obj_s;")
      (when ptr?
        (cgen-body #"  if (SCM_FALSEP(val)) {"
                   #"    obj->data.~|c-length| = 0;"
                   #"    obj->data.~|c-field| = NULL;"
                   #"    return;"
                   #"  }"))
      (cgen-body   #"  if (!SCM_VECTORP(val)) SCM_TYPE_ERROR(val, \"vector\");"
                   #"  ScmSmallInt vlen = SCM_VECTOR_SIZE(val);")
      (when (number? c-length)
        (cgen-body #"  if (vlen != ~|c-length|) {"
                   #"     Scm_Error(\"Invalid length for ~|cclass-cname|.~|c-field|: %ld (must be ~|c-length|)\", vlen);"
                   #"  }"))
      (if ptr?
        (cgen-body #"  ~(~ etype'c-type) *vs = SCM_NEW_ARRAY(~(~ etype'c-type), vlen);")
        (cgen-body #"  ~(~ etype'c-type) *vs = obj->data.~|c-field|;"))
      (cgen-body   #"  for (ScmSmallInt i = 0; i < vlen; i++) {"
                   #"    ScmObj val_i = SCM_VECTOR_ELEMENT(val,i);"
                   #"    if (!~(cgen-pred-expr etype \"val_i\")) {"
                   #"      SCM_TYPE_ERROR(val_i, \"~(~ etype'name)\");"
                   #"    }"
                   #"    vs[i] = ~(cgen-unbox-expr etype \"val_i\");"
                   #"  }")
      (unless (number? c-length)
        (cgen-body #"  obj->data.~|c-length| = vlen;"))
      (when ptr?
        (cgen-body #"  obj->data.~|c-field| = vs;"))
      (cgen-body #"}")))

  (values '<vector>
          `(c ,(gen-getter c-field c-length))
          `(c ,(gen-setter c-field c-length))))

;; Handle slot::(& etype) "c-name"
;;   c-name : "c-field"
;;   cclass-cname is the C typename of the wrapper.
;; Returns stub-type, getter-name, setter-name
;; Whether the content of the embedded struct to be copied or not is
;; up to the boxer of the embedded type.  If the embedded type
;; is also a cstruct, the content is copied.  (That poses a problem when
;; the user want to modify its field.  We'll provide setters for
;; each individual subfields later, but the user code may want to carry
;; around the embedded structure for modification).
(define (make-embedded-struct-getter-setter cclass cclass-cname
                                            slot-name embedded-type-name
                                            c-field c-length c-init)
  (define eclass (scheme-name->cclass embedded-type-name))
  (define etype (name->type embedded-type-name))
  (define (gen-getter c-field c-length) ; returns getter name
    (rlet1 getter-name #"~(~ cclass 'c-name)_~|c-field|_GET"
      (cgen-decl #"static ScmObj ~|getter-name|(ScmObj);")
      (cgen-body #"static ScmObj ~|getter-name|(ScmObj obj_s)"
                 #"{"
                 #"  ~|cclass-cname|* obj = (~|cclass-cname|*)obj_s;"
                 #"  ~(~ etype'c-type) e = &(obj->data.~|c-field|);"
                 #"  return SCM_OBJ(~(cgen-box-expr etype \"e\"));"
                 #"}")))
  (define (gen-setter c-field c-length)
    (rlet1 setter-name #"~(~ cclass 'c-name)_~|c-field|_SET"
      (cgen-decl #"static void ~|setter-name|(ScmObj, ScmObj);")
      (cgen-body #"static void ~|setter-name|(ScmObj obj_s, ScmObj val)"
                 #"{"
                 #"  ~|cclass-cname|* obj = (~|cclass-cname|*)obj_s;"
                 #"  if (!~(cgen-pred-expr etype \"val\")) {"
                 #"    SCM_TYPE_ERROR(val, \"~(~ etype'name)\");"
                 #"  }"
                 #"  ~(~ etype'c-type) e = ~(cgen-unbox-expr etype \"val\");"
                 ;; We need customizable copyer, but for now...
                 #"  obj->data.~|c-field| = *e;"
                 #"}")))

  (unless (and eclass etype)
    (error <cgen-stub-error> "unknown type can't be embedded:"
           embedded-type-name))
  (values embedded-type-name
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
