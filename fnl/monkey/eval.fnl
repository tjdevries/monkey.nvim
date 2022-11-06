(local mod {})
;; (fn node [ty lit expr data]
;;  (var obj {:ty ty :lit lit :expr expr})
;;  (each [key value (pairs data)] (tset obj key value))
;;  obj)

(fn Object [ty inspect data]
 (let [obj { :ty ty :inspect inspect}]
  (each [key value (pairs data)] (tset obj key value))
  obj))

(fn Integer [val]
 (Object
  :obj/Integer
  (fn [self] (string.format "Integer(%s)" self.val))
  {:val val}))

(fn Boolean [val]
 (Object
  :obj/Boolean
  (fn [self] (tostring self.val))
  {:val val}))

;; (: (Integer 5) :inspect)

(fn eval-statements [statements]
 (var result nil)
 (each [_ v (ipairs statements)]
  (set result (mod.eval v)))

 result)

(fn eval-! [right]
 (match right.ty
  :expr-Boolean nil))

(fn eval-prefix [op right]
 (match op
  "!" (eval-! right)))
 


(set mod.eval
 (fn [node]
  (print "Evaling:" node.ty)
  (match node.ty
   :Program (eval-statements node.statements)

   ;; Expressions
   :expr-Number (Integer node.val)
   :expr-Boolean (Boolean node.val)
   :expr-Prefix (let
                 [right (mod.eval node.right)]
                 (eval-prefix node.op right))

   ;; Statements
   :stmt-Expression (mod.eval node.expr))))

mod
