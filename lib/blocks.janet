(import ./state)
(import ./util)

(util/add-to state/rules @{:blocks @{}})

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
  ~@{:main  (* (/ :block ,util/update-col-pos) ($))
     :nl    "\n"
     :eol   (+ :nl -1)
     :space (set " \t")
     :char    (+ :escaped :entity (if-not :eol '1))
     :escaped (+ (* "\\" '(set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")) '(* "\\" 1))
     :entity  (/ (+ (* (constant :ent) '(* "&" (some (+ :w :d)) ";"))
                   (* (constant :dec) '(* "&#" (between 1 7 :d) ";"))
                   (* (constant :hex) '(* "&#" (set "Xx") (between 1 6 :h) ";")))
                ,util/entity-decode)
     :text (* '(some (if-not :eol 1)) (? :nl))
     :padding (drop (/ '(any :space) ,util/record-padding))
     :block (* :padding ,(tuple ;blocks))})

(util/add-to grammar blank/grammar)
(util/add-to grammar blockquote/grammar)
(util/add-to grammar codeblock/grammar)
(util/add-to grammar heading/grammar)
(util/add-to grammar html/grammar)
(util/add-to grammar list/grammar)
(util/add-to grammar linkdef/grammar)
(util/add-to grammar paragraph/grammar)
(util/add-to grammar t-break/grammar)

## Block functions

(defn- default-block-append [node block functions]
  (unless (= :blank (util/type-of block))
    (def replace-fn (util/get-fn :replace block functions))
    (if replace-fn
      (replace-fn block (util/children-of node))
      (if (def peer (util/next-container node))
        ((util/get-fn :continue peer functions) peer block)
        (array/push (util/children-of node) block)))))

(defn- default-block-blank [node parent functions]
  (util/next-container node))

(defn- default-block-close [node &opt parent]
  (util/attribute node :open? false))

(defn- default-block-continue [peer block]
  (array/concat (util/children-of peer) (util/children-of block)))

(defn- default-block-equal? [node block]
  (= (util/type-of node) (util/type-of block)))

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

(util/add-to state/rules
  {:blocks
    {'default {:append      default-block-append
               :blank       default-block-blank
               :close       default-block-close
               :continue    default-block-continue
               :equal?      default-block-equal?
               :follower    default-block-follower
               :lazy?       default-block-lazy?
               :needs-nl?   default-block-needs-nl?
               :next-block  default-block-next-block
               :see-blank   default-block-see-blank}}})
