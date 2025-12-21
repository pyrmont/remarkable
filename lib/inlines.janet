(import ./state)
(import ./util)

(import ./punctuation)

# UTF-8 helpers for Unicode punctuation detection

(defn- bytes-to-codepoint [s]
  (def b0 (get s 0))
  (cond
    (< b0 0x80) b0  # 1-byte (ASCII)
    (< b0 0xE0)     # 2-byte
      (+ (blshift (band b0 0x1F) 6)
         (band (get s 1) 0x3F))
    (< b0 0xF0)     # 3-byte
      (+ (blshift (band b0 0x0F) 12)
         (blshift (band (get s 1) 0x3F) 6)
         (band (get s 2) 0x3F))
    :else           # 4-byte
      (+ (blshift (band b0 0x07) 18)
         (blshift (band (get s 1) 0x3F) 12)
         (blshift (band (get s 2) 0x3F) 6)
         (band (get s 3) 0x3F))))

(defn- check-unicode-punc [s]
  (when (punctuation/upunc? (bytes-to-codepoint s))
    s))

(util/add-to state/protocols @{:inlines @{}})

(def inline ~@[+])

(import ./inlines/autolink)
(import ./inlines/codespan)
(import ./inlines/emphasis)
(import ./inlines/link)
(import ./inlines/hardbreak)
(import ./inlines/rawhtml)

(array/push inline
  :codespan
  :rawhtml
  :autolink
  :hardbreak
  :emphasis
  :link
  :text)

(def grammar
  ~@{:main :inlines
     :alpha (range "AZ" "az")
     :alnum (+ :alpha (range "09"))
     :utf8-char (+ (range "\x00\x7F")                                                               # 1-byte ASCII
                   (* (range "\xC0\xDF") (range "\x80\xBF"))                                        # 2-byte
                   (* (range "\xE0\xEF") (range "\x80\xBF") (range "\x80\xBF"))                     # 3-byte
                   (* (range "\xF0\xF7") (range "\x80\xBF") (range "\x80\xBF") (range "\x80\xBF"))) # 4-byte
     # Unicode punctuation - cmt fails when function returns nil/false
     :upunc (cmt (capture :utf8-char) ,check-unicode-punc)
     # ASCII punctuation only - for escaping and delimiter detection
     :ascii-punc (set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
     # all punctuation (ASCII + Unicode) - for delimiter detection
     :punc (+ :ascii-punc (drop :upunc))
     :ws    (set " \n\r\v\t")
     :nl    "\n"
     :blank (* "\n" (any :space) "\n")
     :space (set " \t")
     :char    (if-not (+ (set "&<>*_[]!`\\") (* (at-least 2 :space) "\n")) '1)
     :entity  (/ (+ (* (constant :ent) '(* "&" (some (+ :w :d)) ";"))
                    (* (constant :dec) '(* "&#" (between 1 7 :d) ";"))
                    (* (constant :hex) '(* "&#" (set "Xx") (between 1 6 :h) ";")))
                 ,util/entity-decode)
     :escaped (+ (* "\\" ':ascii-punc)  # Only ASCII punctuation can be escaped
                 '(* "\\" (if-not :nl 1)))
     :inlines (any :inline)
     :inline  (+ ,(tuple ;inline))
     :trail (* :space ':nl)
     :text  (+ (% (some (+ :escaped :entity :trail :char))) '1)})

(util/add-to grammar autolink/grammar)
(util/add-to grammar codespan/grammar)
(util/add-to grammar emphasis/grammar)
(util/add-to grammar hardbreak/grammar)
(util/add-to grammar link/grammar)
(util/add-to grammar rawhtml/grammar)

## Inline functions

(defn- default-inline-close [node delim ancestors]
  (array/pop ancestors))

## Inline default protocol

(util/add-to state/protocols
  {:inlines
    {'default {:close default-inline-close}}})
