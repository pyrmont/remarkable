(import ../state)
(import ../util)

## Grammar

(defn- linkdef [content]
  [:linkdef @{:open? true} @[content]])

(def grammar
  ~{:linkdef (/ '(* "[" (to :eol)) ,linkdef)})

## Functions

(defn- linkdef-close [a-def &opt parent protocols]
  (defn register-link [ref-text dest &opt title]
    (def ref (util/normalise ref-text))
    (unless (get state/links ref)
      (put state/links ref {:url (util/uri-encode dest) :title title})))
  (def link-grammar
    ~{:main (* (/ (* :label ":" :gap :dest (+ (* (> 0 (+ :space :nl)) :gap :title (any :space) :eol) (* (any :space) :eol))) ,register-link) ($))
      :eol   (+ :nl -1)
      :nl    "\n"
      :space (set " \t")
      :blank (* :nl (any :space) :nl) # check whether parens balanced
      :gap   (* (any :space) (? :nl) (any :space))
      :escaped (+ (* "\\" '(set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")) '(* "\\" 1))
      :entity  (/ (+ (* (constant :ent) '(* "&" (some (+ :w :d)) ";"))
                     (* (constant :dec) '(* "&#" (between 1 7 :d) ";"))
                     (* (constant :hex) '(* "&#" (set "Xx") (between 1 6 :h) ";")))
                  ,util/entity-decode)
      :char    (+ :escaped :entity '1)
      :label (* "[" :gap '(some (+ (* "\\" 1) (if-not (+ (set "\\[]") :blank) 1))) :gap "]")
      :dest  (+ (* "<" (% (any (if-not (set "<>\n") :char))) ">")
                (* (not "<") (% (some (if-not (+ (range "\x00\x20") "\x7F") :char)))))
      :title (+ (* `"` (% (any (if-not (+ (set "\"") :blank) :char))) `"`)
                (* "'" (% (any (if-not (+ (set "'") :blank) :char))) "'")
                (* "(" (% (any (if-not (+ (set "()") :blank) :char))) ")"))})
  (when (util/attribute a-def :open?)
    (util/attribute a-def :open? false)
    (def all-text (-> (util/children-of a-def) (string/join "\n") string/trim))
    (var i 0)
    (while (< i (length all-text)) # a linkdef 'block' can contain multiple definitions as well as a follow-on paragraph
      (if (def res (peg/match link-grammar all-text i))
        (set i (get res 1))
        (break)))
    (array/pop (util/children-of parent))
    (when (< i (length all-text))
      (def content (string/slice all-text i))
      (array/push (util/children-of parent) [:paragraph @{:indent (util/attribute a-def :indent) :open? false :inlines? true} @[content]])))
  nil)

(defn- linkdef-lazy? [a-def]
  true)

(defn- linkdef-equal? [a-def block]
  (or (= :paragraph (util/type-of block))
      (= :linkdef (util/type-of block))))

(defn- linkdef-needs-nl? [a-def]
  true)

(defn- linkdef-next-block [a-def line pos grammar protocols]
  (def result (peg/match grammar line pos))
  (def block (get result 0))
  (def needs-nl-fn (util/get-fn :needs-nl? block protocols))
  (if (needs-nl-fn block)
    [(util/to-continuation :paragraph line pos) (length line)]
    result))

(util/add-to state/protocols
  {:blocks
    {:linkdef {:blank       linkdef-close
               :close       linkdef-close
               :equal?      linkdef-equal?
               :lazy?       linkdef-lazy?
               :needs-nl?   linkdef-needs-nl?
               :next-block  linkdef-next-block}}})
