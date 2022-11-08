(local L (require :monkey.lexer))

(local mod {})

(macro def-enum [name ...]
  `(var
     ,name
     ,(collect [i v (ipairs [...])]
        (values (string.upper v) i))))

(def-enum prec-enum
  :LOWEST
  :EQUALS
  :LESSGREATER
  :SUM
  :PRODUCT
  :PREFIX
  :CALL)

(local
 precedences
 {
  :lex-Eq prec-enum.EQUALS
  :lex-NotEq prec-enum.EQUALS
  :lex-LT prec-enum.LESSGREATER
  :lex-RT prec-enum.LESSGREATER
  :lex-Plus prec-enum.SUM
  :lex-Minus prec-enum.SUM
  :lex-Slash prec-enum.PRODUCT
  :lex-Asterisk prec-enum.PRODUCT})

;; lit: fn [data] -> string
(var [make-stmt make-expr]
 (do
  (fn node [ty lit expr data]
   (var obj {:ty ty :lit lit :expr expr})
   (each [key value (pairs data)] (tset obj key value))
   obj)

  (fn make-stmt [ty lit data] (node ty lit false data))
  (fn make-expr [ty lit data] (node ty lit true data))

  [make-stmt make-expr]))

(fn is-expr [val] (= val.expr true))
(fn is-stmt [val] (not val.expr))

(fn PrefixExpression [tok op right]
 (assert (is-expr right) "prefix-expression")
 (make-expr
  :expr-Prefix
  (fn [data] (string.format "(%s%s)" op (right:lit)))
  {:tok tok :op op :right right}))

(fn GroupedExpression [tok op val]
 (assert (is-expr val) "grouped-expression")
 (var something
  (make-expr
   :expr-Grouped
   (fn [data] (string.format "%s" (val:lit)))
   {:tok tok :op op :val val}))

 something)


(fn InfixExpression [tok left op right]
 (assert (is-expr left) (string.format "infix-expression left: %s" (vim.inspect left)))
 (assert (is-expr right) "infix-expression right")

 (make-expr
  :expr-Infix
  (fn [data]
   (string.format
    "(%s %s %s)"
    (data.left:lit)
    data.op
    (data.right:lit)))
  {:tok tok :left left :op op :right right}))

(fn Number [tok val]
 (make-expr
  :expr-Number
  (fn [data] data.val)
  {:tok tok :val (tonumber val)}))

(fn Identifier [tok val]
 (assert (= (type val) "string") "identifier val")

 (make-expr
  :expr-Identifier
  (fn [data] data.val)
  {:tok tok :val val}))

(fn Boolean [tok val]
  (assert (= (type val) "boolean") "boolean val")
  (make-expr
    :expr-Boolean
    (fn [data] data.val)
    {:tok tok :val val}))

(fn ExpressionStatement [tok val]
 (assert (is-expr val) "expression-stmt expr")

 (make-stmt
  :stmt-Expression
  (fn [data] (string.format "%s;" (data.val:lit)))
  {:tok tok :val val}))



(fn ReturnStatement [tok val]
 ;; (assert (is-expr val))

 (make-stmt
  :stmt-Return
  (fn [data] (string.format "return %s" (data.val:lit)))
  {:tok tok :val val}))


(fn LetStatement [tok name val]
 ;; (assert (is-expr value))
 (assert (= name.ty :expr-Identifier))

 (make-stmt
  :stmt-Let
  (fn [data] (string.format "let %s = %s;" (data.name:lit) (data.val:lit)))
  {
   :tok tok
   :name name
   :val val}))


(fn Program [statements]
 (make-stmt
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

  (fn cur-type? [ty] (= parser.curToken.ty ty))
  (fn !cur-type? [ty] (not (cur-type? ty)))
  (fn peek-type? [ty] (= parser.peekToken.ty ty))
  (fn !peek-type? [ty] (not (peek-type? ty)))

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
   (while (and (!cur-type? ty) (!cur-type? :lex-EndOfFile))
    (parser.next-token)))

  (fn parse-return-statement []
   (match-try (values :ok [(parser.next-token)])
    (:ok args) (do
                ;; todo: parse expression
                (read-until :lex-Semicolon)

                (ReturnStatement (unpack args)))
    (catch
     (:failed) nil)))

  (fn parse-let-statement []
   (fn let-identifier [args]
    (var name (expect-peek :lex-Identifier))
    (ok-if args name (Identifier name name.lit)))

   (fn let-assign [args]
    (ok-peek args :lex-Assign))

   (match-try (values :ok [parser.curToken])
    (:ok args) (let-identifier args)
    (:ok args) (let-assign args)
    (:ok args) (do
                ;; TODO Parse expression
                (read-until :lex-Semicolon)
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

    ;; (if (= op :lex-Slash)
    ;;  (error "SLASH"))
    ;; (print op)

    (if right (InfixExpression tok left op right))))

  (local infix-fns
   {
    :lex-Plus parse-infix-expression
    :lex-Minus parse-infix-expression
    :lex-Slash parse-infix-expression
    :lex-Asterisk parse-infix-expression
    :lex-Eq parse-infix-expression
    :lex-NotEq parse-infix-expression
    :lex-LT parse-infix-expression
    :lex-GT parse-infix-expression})

  (fn parse-grouped-expression []
   (let
    [
     tok parser.curToken
      op parser.curToken.lit
      _ (parser.next-token)
      val (parse-expression prec-enum.LOWEST)]

    (var peeked (expect-peek :lex-RightParen))
    (if val
     (if peeked
      (GroupedExpression tok op val)))))

  (fn parse-prefix-expression []
   (let
    [
     tok parser.curToken
     op parser.curToken.lit
     _ (parser.next-token)
     val (parse-expression prec-enum.PREFIX)]

    (if val (PrefixExpression tok op val))))

  (fn parse-boolean-expression []
    (let
      [val (if (= parser.curToken.ty :lex-True) true false)]
      (Boolean parser.curToken val)))

  (var prefix-fns
   {
    :lex-Bang parse-prefix-expression
    :lex-Minus parse-prefix-expression
    :lex-Identifier (fn [p] (Identifier p.curToken p.curToken.lit))
    :lex-Integer (fn [p] (Number p.curToken p.curToken.lit))
    :lex-LeftParen parse-grouped-expression
    :lex-True parse-boolean-expression})

  (fn parse-prefix* [prec left]
   (if (or
        (peek-type? :lex-Semicolon)
        (>= prec (peek-prec)))
    left
    (let
     [infix (. infix-fns parser.peekToken.ty)]
     (if (not infix)
      left
      (do
       (parser.next-token)
       (parse-prefix* prec (infix left)))))))

  (set parse-expression
   (fn [prec]
    (let
     [prefix (. prefix-fns parser.curToken.ty)]

     (if (not prefix)
      (make-error "Failed to parse expression: %s" (vim.inspect parser))
      (let
       [left (prefix parser)]
       (parse-prefix* prec left))))))

  (fn ok-expression [args prec]
    (var val (parse-expression prec))
    (ok-if args val val))

  (fn parse-expression-statement []
   (match-try (values :ok [parser.curToken])
    (:ok args) (ok-expression args prec-enum.LOWEST)
    (:ok args) (do
                (if (peek-type? :lex-Semicolon) (parser.next-token))
                (values :ok args))
    (:ok args) (ExpressionStatement (unpack args))
    (catch
     (:failed) nil)))

  (fn parse-statement []
   (match parser.curToken.ty
    :lex-Let (parse-let-statement)
    :lex-Return (parse-return-statement)
    _ (parse-expression-statement)))

  (fn parse-program []
   (var statements [])

   (while (~= parser.curToken.ty :lex-EndOfFile)
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


;; (mod.parse "(5 + 10)")

mod
