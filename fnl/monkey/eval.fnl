(local mod {})
;; (fn node [ty lit expr data]
;;  (var obj {:ty ty :lit lit :expr expr})
;;  (each [key value (pairs data)] (tset obj key value))
;;  obj)

(fn Object [ty inspect data]
 (let [obj { :ty ty :inspect inspect}]
  (each [key value (pairs data)] (tset obj key value))
  obj))

(var NULL (Object :obj-Null (fn [] "NULL") {}))

(fn Boolean [val]
 (Object
  :obj-Boolean
  (fn [self] (tostring self.val))
  {:val val}))

(var TRUE (Boolean true))
(var FALSE (Boolean false))

(fn Integer [val]
 (Object
  :obj-Integer
  (fn [self] (string.format "Integer(%s)" self.val))
  {:val val}))

(fn eval-statements [statements]
 (var result nil)
 (each [_ v (ipairs statements)]
  (set result (mod.eval v)))

 result)


(macro make-prefix-fn [name body]
 `(var
   ,name
   (lambda [node#]
    (assert
     (vim.startswith node#.ty :obj-)
     ,(string.format "%s: only accepts objects" (tostring name)))

    (let [body# ,body]
     (body# node#)))))

(make-prefix-fn eval-bang
 (fn [node]
  (match node.ty
   :obj-Boolean (if node.val FALSE TRUE)
   :obj-Nil TRUE
   _ FALSE)))

(make-prefix-fn eval-minus
 (fn [node]
  (match node.ty
   :obj-Integer (Integer (- node.val))
   _ NULL)))

;; TODO: We need to make sure we know which things are objects
;; vs what things are expressions. that is what is making things
;; go wrong currently...
;;  but this is why it's good that we prefixed our keys!

;; (fn eval-! [right]
;;  (assert (vim.startswith right.ty :obj-) "eval-! accepts only objects")
;;
;;  (match right.ty
;;   :obj-Boolean (if right.val FALSE TRUE)
;;   :obj-Nil TRUE
;;   _ FALSE))

(fn eval-prefix [op right]
 (match op
  "!" (eval-bang right)
  "-" (eval-minus right)))

(fn eval-infix-integer [op left right]
 (let
  [l left.val r right.val]
  (Integer
   (match op
    "+" (+ l r)
    "-" (- l r)
    "*" (* l r)
    "/" (/ l r)
    _ (error "unhandled integer infix")))))

(fn eval-infix [op left right]
 (assert left "must have left expr")
 (assert right "must have right expr")
 (assert left.val "must have left expr val")
 (assert right.val "must have right expr val")

 (match [op left right]
  (where
   [op l r]
   (and
    (= left.ty :obj-Integer)
    (= right.ty :obj-Integer))) (eval-infix-integer op left right)))


(set mod.eval
 (fn [node]

  (assert
   (or
    (= node.ty :Program)
    (vim.startswith node.ty :expr-)
    (vim.startswith node.ty :stmt-))
   "types do not match")

  (var res
   (match node.ty
    :Program (eval-statements node.statements)

    ;; Expressions
    :expr-Boolean (if node.val TRUE FALSE)
    :expr-Number (Integer node.val)

    :expr-Prefix (let
                  [right (mod.eval node.right)]
                  (eval-prefix node.op right))

    :expr-Infix (let
                 [left (mod.eval node.left)
                  right (mod.eval node.right)]
                 (eval-infix node.op left right))

    :expr-Grouped (mod.eval node.val)

    ;; Statements
    :stmt-Expression (mod.eval node.val)
    _ (error (.. "Missing type: " node.ty))))

  (if (= res nil)
   (error (string.format "failed to eval this node: %s" (vim.inspect node)))
   res)))

mod
