(import ../util)

## Grammar

(defn- autolink [scheme uri &opt email?]
  (def url (string scheme uri))
  [:link @{:url (util/uri-encode url)} @[(if email? uri url)]])

(def grammar
  ~{:autolink {:main   (/ (* "<" (+ :mail :other) ">") ,autolink)
               :mail   (* (constant "mailto:") :email (constant true))
               :email  '(* (some (+ :w :d (set ".!#$%&'*+/=?^_`{|}~-")))
                           "@"
                           (* :alnum (at-most 61 (+ :w :d "-"))
                           (any (* "." :alnum (at-most 61 (+ :w :d "-"))))))
               :other  (* :scheme :uri)
               :scheme '(* (+ ,(util/words "mailto")
                              (* :w (some (+ :w :d (set "+.-"))))) ":")
               :uri    '(any (if-not (+ (set "<>") (range "\x00\x20") "\x7F") 1))}})
