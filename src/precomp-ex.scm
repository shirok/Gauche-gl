;;
;; Extending precomp
;;

(load "precomp")  ;; main is in here

;; We need to enhance gauche.cgen if we compile with 0.9.11_pre1 or before.
;; NB: We need some sort of (version<= ...) feature predicate!
(cond-expand
 [(not (or gauche-0.9.11_pre2 gauche-0.9.11))
  (add-load-path "." :relative)
  (load "defcstruct")]
 [else])
