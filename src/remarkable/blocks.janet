(use ./globals)
(use ./utilities)


(add-to rules @{:blocks @{}})


(def blocks ~@[+])


(import ./blocks/blank)
(import ./blocks/blockquote)
(import ./blocks/codeblock)
(import ./blocks/heading)
(import ./blocks/html)
(import ./blocks/linkdef)
(import ./blocks/list)
(import ./blocks/paragraph)
(import ./blocks/t-break)


(array/push blocks
  :blank
  :codeblock
  :t-break
  :html
  :linkdef
  :blockquote
  :list
  :heading
  :paragraph)


(def grammar
  ~@{:main  (* (/ :block ,update-col-pos) ($))

     :nl    "\n"
     :eol   (+ :nl -1)
     :space (set " \t")

     :char    (+ :escaped :entity (if-not :eol '1))
     :escaped (+ (* "\\" '(set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")) '(* "\\" 1))
     :entity  (/ (+ (* (constant :ent) '(* "&" (some (+ :w :d)) ";"))
                   (* (constant :dec) '(* "&#" (between 1 7 :d) ";"))
                   (* (constant :hex) '(* "&#" (set "Xx") (between 1 6 :h) ";")))
                ,entity-decode)

     :text (* '(some (if-not :eol 1)) (? :nl))

     :padding (drop (/ '(any :space) ,record-padding))

     :block (* :padding ,(tuple ;blocks))})


(add-to grammar blank/grammar)
(add-to grammar blockquote/grammar)
(add-to grammar codeblock/grammar)
(add-to grammar heading/grammar)
(add-to grammar html/grammar)
(add-to grammar list/grammar)
(add-to grammar linkdef/grammar)
(add-to grammar paragraph/grammar)
(add-to grammar t-break/grammar)


## Block functions

(defn- default-block-append [node block functions]
  (unless (= :blank (type-of block))
    (def replace-fn (get-fn :replace block functions))
    (if replace-fn
      (replace-fn block (children-of node))
      (if (def peer (next-container node))
        ((get-fn :continue peer functions) peer block)
        (array/push (children-of node) block)))))


(defn- default-block-blank [node parent functions]
  (next-container node))


(defn- default-block-close [node &opt parent]
  (attribute node :open? false))


(defn- default-block-continue [peer block]
  (array/concat (children-of peer) (children-of block)))


(defn- default-block-equal? [node block]
  (= (type-of node) (type-of block)))


(defn- default-block-follower [node block]
  nil)


(defn- default-block-lazy? [block]
  false)


(defn- default-block-needs-nl? [block]
  false)


(defn- default-block-next-block [node line pos grammar functions]
  (peg/match grammar line pos))


(defn- default-block-see-blank [node functions]
  nil)


## Block rules

(add-to rules
  {:blocks
    {'default    {:append      default-block-append
                  :blank       default-block-blank
                  :close       default-block-close
                  :continue    default-block-continue
                  :equal?      default-block-equal?
                  :follower    default-block-follower
                  :lazy?       default-block-lazy?
                  :needs-nl?   default-block-needs-nl?
                  :next-block  default-block-next-block
                  :see-blank   default-block-see-blank}}})
