(local L (require :monkey.lexer))

(local mod {})

(local
 prec-enum
 {
  :LOWEST 0
  :EQUALS 1
  :LESSGREATER 2
  :SUM 3
  :PRODUCT 4
  :PREFIX 5
  :CALL 6})

(local
 precedences
 {
  :Eq prec-enum.EQUALS
  :NotEq prec-enum.EQUALS
  :LT prec-enum.LESSGREATER
  :RT prec-enum.LESSGREATER
  :Plus prec-enum.SUM
  :Minus prec-enum.SUM
  :Slash prec-enum.PRODUCT
  :Asterisk prec-enum.PRODUCT})

;; lit: fn [data] -> string
(var [stmt expr]
 (do
  (fn node [ty lit expr data]
   (var obj {:ty ty :lit lit :expr expr})
   (each [key value (pairs data)] (tset obj key value))
   obj)

  (fn stmt [ty lit data] (node ty lit false data))
  (fn expr [ty lit data] (node ty lit true data))

  [stmt expr]))

(fn is-expr [val] (= val.expr true))
(fn is-stmt [val] (= val.expr false))

(fn PrefixExpression [tok op right]
 (assert (is-expr right))
 (expr
  :PrefixExpression
  (fn [data] (string.format "%s%s" op (right:lit)))
  {:tok tok :op op :right right}))

(fn InfixExpression [tok left op right]
 (assert (is-expr left))
 (assert (is-expr right))

 (expr
  :InfixExpression
  (fn [data]
   (string.format
    "(%s %s %s)"
    (data.left:lit)
    data.op
    (data.right:lit)))
  {:tok tok :left left :op op :right right}))

(fn Number [tok val]
 (expr
  :NumberExpr
  (fn [data] data.val)
  {:tok tok :val (tonumber val)}))

(fn Identifier [tok val]
 (assert (= (type val) "string"))

 (expr
  :IdentifierExpr
  (fn [data] data.val)
  {:tok tok :val val}))

(fn ExpressionStatement [tok expr]
 (assert (is-expr expr))

 (stmt
  :ExpressionStatement
  (fn [data] (string.format "%s;" (data.expr:lit)))
  {:tok tok :expr expr}))



(fn ReturnStatement [tok val]
 ;; (assert (is-expr val))

 (stmt
  :ReturnStatement
  (fn [data] (string.format "return %s" (data.val:lit)))
  {:tok tok :val val}))


(fn LetStatement [tok name val]
 ;; (assert (is-expr value))
 (assert (= name.ty :IdentifierExpr))

 (stmt
  :LetStatement
  (fn [data] (string.format "let %s = %s;" (data.name:lit) (data.val:lit)))
  {
   :tok tok
   :name name
   :val val}))


(fn Program [statements]
 (stmt
  :Program
  (fn [data]
   (table.concat
    (icollect [_ v (ipairs data.statements)]
     (v:lit))
    "\n"))
  {:statements statements}))


(macro ok-if [args cond body]
 (if body
  `(if ,cond
    ;; Insert the body into the arguments
    (do
     (var result# ,body)
     (if (~= result# nil)
      (table.insert ,args ,body))

     (values :ok ,args))
    ;; or fail
    :failed)
  `(if ,cond
    ;; Insert the body into the arguments
    (values :ok ,args)
    ;; or fail
    :failed)))



(set mod.parse
 (fn [input]
  (var lexer (L.get_lexer input))
  (var parser {:lexer lexer :curToken nil :peekToken nil :errors []})

  (fn make-error [...]
   (table.insert parser.errors (string.format ...))
   nil)

  (fn cur-type-is [ty] (= parser.curToken.ty ty))
  (fn cur-type-is-not [ty] (~= parser.curToken.ty ty))
  (fn peek-type-is [ty] (= parser.peekToken.ty ty))

  (fn peek-prec []
   (or
    (. precedences parser.peekToken.ty)
    prec-enum.LOWEST))

  (fn cur-prec []
   (or
    (. precedences parser.curToken.ty)
    prec-enum.LOWEST))

  (fn next-token []
    (set parser.curToken parser.peekToken)
    (set parser.peekToken (parser.lexer.next-token)))

  (fn expect-token [ty])

  (fn expect-peek [ty]
   (if
    (= parser.peekToken.ty ty)
    (do
     (parser.next-token)
     parser.curToken)
    (make-error "expect-peek %s %s" ty parser.peekToken)))

  (fn ok-peek [args ty]
   (ok-if args (expect-peek ty)))

  (fn read-until [ty]
   (while (and (cur-type-is-not ty) (cur-type-is-not :EndOfFile))
    (parser.next-token)))

  (fn parse-return-statement []
   (match-try (values :ok [(parser.next-token)])
    (:ok args) (do
                ;; todo: parse expression
                (read-until :Semicolon)

                (ReturnStatement (unpack args)))
    (catch
     (:failed) nil)))

  (fn parse-let-statement []
   (fn let-identifier [args]
    (var name (expect-peek :Ident))
    (ok-if args name (Identifier name name.lit)))

   (fn let-assign [args]
    (ok-peek args :Assign))

   (match-try (values :ok [parser.curToken])
    (:ok args) (let-identifier args)
    (:ok args) (let-assign args)
    (:ok args) (do
                ;; TODO Parse expression
                (read-until :Semicolon)
                (LetStatement (unpack args)))
    (catch
     (:failed) nil)))

  ;; forward delcare parse-expression
  (var parse-expression nil)

  (fn parse-infix-expression [left]
   (let
    [tok parser.curToken
     op parser.curToken.lit
     prec (cur-prec)
     _ (parser.next-token)
     right (parse-expression prec)]
    (if (= right nil)
     nil
     (InfixExpression tok left op right))))

  (local infix-fns
   {
    :Plus parse-infix-expression
    :Minus parse-infix-expression
    :Slash parse-infix-expression
    :Asterisk parse-infix-expression
    :Eq parse-infix-expression
    :NotEq parse-infix-expression
    :LT parse-infix-expression
    :GT parse-infix-expression})

  (fn parse-prefix-expression []
   (let
    [
     tok parser.curToken
     op parser.curToken.lit
     _ (parser.next-token)
     expr (parse-expression prec-enum.PREFIX)]

    (if (= expr nil)
     nil
     (PrefixExpression tok op expr))))

  (var prefix-fns
   {
    :Bang parse-prefix-expression
    :Minus parse-prefix-expression
    :Ident (fn [p] (Identifier p.curToken p.curToken.lit))
    :Integer (fn [p] (Number p.curToken p.curToken.lit))})

  (fn parse-prefix-inner [prec left]
   (if (or
        (peek-type-is :Semicolon)
        (>= prec (peek-prec)))
    left
    (let
     [infix (. infix-fns parser.peekToken.ty)]
     (if (not infix)
      left
      (do
       (parser.next-token)
       (parse-prefix-inner prec (infix left)))))))

  (set parse-expression
   (fn [prec]
    (let
     [prefix (. prefix-fns parser.curToken.ty)]

     (if (not prefix)
      (make-error "Failed to parse expression: %s" (vim.inspect parser))
      (let
       [left (prefix parser)]
       (parse-prefix-inner prec left))))))

  (fn ok-expression [args prec]
    (var expr (parse-expression prec))
    (ok-if args expr expr))

  (fn parse-expression-statement []
   (match-try (values :ok [parser.curToken])
    (:ok args) (ok-expression args prec-enum.LOWEST)
    (:ok args) (do
                (if (peek-type-is :Semicolon) (parser.next-token))
                (values :ok args))
    (:ok args) (ExpressionStatement (unpack args))
    (catch
     (:failed) nil)))

  (fn parse-statement []
   (match parser.curToken.ty
    :Let (parse-let-statement)
    :Return (parse-return-statement)
    _ (parse-expression-statement)))

  (fn parse-program []
   (var statements [])

   (while (~= parser.curToken.ty :EndOfFile)
    (var statement (parse-statement))
    (if statement
     (table.insert statements statement))

    (parser.next-token))

   (Program statements))

  (set parser.next-token (fn [] (next-token)))

  ;; Call next-token twice to initialize cur and peek tokens
  (parser.next-token)
  (parser.next-token)

  (var program (parse-program))
  (set program.errors parser.errors)
  program))

mod
