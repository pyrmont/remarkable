(use ./globals)
(use ./utilities)


(add-to rules @{:inlines @{}})


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

     :punc (set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")

     :ws    (set " \n\r\v\t")
     :nl    "\n"
     :blank (* "\n" (any :space) "\n")
     :space (set " \t")

     :char    (if-not (+ (set "&<>*_[]!`\\") (* (at-least 2 :space) "\n")) '1)
     :entity  (/ (+ (* (constant :ent) '(* "&" (some (+ :w :d)) ";"))
                    (* (constant :dec) '(* "&#" (between 1 7 :d) ";"))
                    (* (constant :hex) '(* "&#" (set "Xx") (between 1 6 :h) ";")))
                 ,entity-decode)
     :escaped (+ (* "\\" ':punc)
                 '(* "\\" (if-not :nl 1)))

     :inlines (any :inline)
     :inline  (+ ,(tuple ;inline))

     :trail (* :space ':nl)
     :text  (+ (% (some (+ :escaped :entity :trail :char))) '1)})


(add-to grammar autolink/grammar)
(add-to grammar codespan/grammar)
(add-to grammar emphasis/grammar)
(add-to grammar hardbreak/grammar)
(add-to grammar link/grammar)
(add-to grammar rawhtml/grammar)


## Inline functions

(defn- default-inline-close [node delim ancestors]
  (array/pop ancestors))


(add-to rules
  {:inlines
    {'default  {:close    default-inline-close}}})
