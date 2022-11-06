(local mod {})

(fn token [ty lit] {:ty ty :lit lit})
(fn token-type [ty] (fn [lit] (token ty lit)))

(macro literal [name lit]
  `(var ,name (token ,(tostring name) ,lit)))

;; Token Types

(var KEYWORDS
  {
   :true :True
   :false :False
   :if :If
   :else :Else
   :return :Return
   :fn :Function
   :let :Let})


(var Ident (token-type :Ident))

(var Integer (token-type :Integer))
(var Illegal (token-type :illegal))

;; Literal types
(literal Assign "=")
(literal Semicolon ";")
(literal LeftParen "(")
(literal RightParen ")")
(literal Comma ",")
(literal LeftBrace "{")
(literal RightBrace "}")
(literal EndOfFile "\0")
(literal Plus "+")
(literal Minus "-")
(literal Bang "!")
(literal Asterisk "*")
(literal Slash "/")
(literal LT "<")
(literal GT ">")
(literal Eq "==")
(literal NotEq "!=")

;; Mutates lexer (hopefully)
(fn lexer-read-char [lexer]
  (if
    (> lexer.readPosition (# lexer.input))
    (set lexer.ch "\0")
    (set lexer.ch (string.sub lexer.input lexer.readPosition lexer.readPosition)))

  (set lexer.position lexer.readPosition)
  (set lexer.readPosition (+ lexer.readPosition 1)))

(fn lexer-peek-char [lexer]
  (if 
    (> lexer.readPosition (# lexer.input))
    "\0"
    (string.sub
      lexer.input
      lexer.readPosition
      lexer.readPosition)))

(fn is-letter [ch] (or (string.match ch "%a") (= ch "_")))
(fn is-number [ch] (string.match ch "%d"))

(fn ident-ty [ident]
  (if (. KEYWORDS ident)
    (. KEYWORDS ident)
    :Ident))

(fn lexer-read-identifier-text [lexer]
  (var position lexer.position)
  (while (is-letter lexer.ch)
    (lexer-read-char lexer))

  (string.sub lexer.input position (- lexer.position 1)))

(fn lexer-read-identifier [lexer]
  (var lit (lexer-read-identifier-text lexer))
  (token (ident-ty lit) lit))

(fn lexer-skip-whitespace [lexer]
  (while (match lexer.ch
           " " true
           "\t" true
           "\n" true
           "\r" true
           _ false)
   (lexer-read-char lexer)))

(fn lexer-read-number [lexer]
  (var position lexer.position)
  (while (is-number lexer.ch)
    (lexer-read-char lexer))

  (Integer (string.sub lexer.input position (- lexer.position 1))))

(fn if-peek [lexer peekChar t f]
  (if (= (lexer-peek-char lexer) peekChar)
    (do
      (lexer-read-char lexer)
      t)
    f))

(fn lexer-next-token [lexer]
  (lexer-skip-whitespace lexer)

  (var [token advance]
    (match lexer.ch
     "=" [(if-peek lexer "=" Eq Assign) true]
     "!" [(if-peek lexer "=" NotEq Bang) true]

     "+" [Plus true]
     "-" [Minus true]
     "*" [Asterisk true]
     "/" [Slash true]

     "<" [LT true]
     ">" [GT true]

     ";" [Semicolon true]
     "(" [LeftParen true]
     ")" [RightParen true]
     "," [Comma true]
     "{" [LeftBrace true]
     "}" [RightBrace true]
     (where ch (is-letter ch)) [(lexer-read-identifier lexer) false]
     (where ch (is-number ch)) [(lexer-read-number lexer) false]

     "\0" [EndOfFile true]
     _ (Illegal lexer.ch)))

  (if advance (lexer-read-char lexer))

  token)


;; Lexer
(set mod.lex
 (fn [input]
  (var lexer (mod.get_lexer input))

  (var tokens [])
  (while (~= lexer.ch "\0")
    (table.insert tokens (lexer.next-token)))

  tokens))

(set mod.get_lexer
 (fn [input]
  (var lexer {:input input :position 1 :readPosition 1 :ch ""})
  (lexer-read-char lexer)

  (set lexer.next-token (fn [] (lexer-next-token lexer)))

  lexer))



mod
