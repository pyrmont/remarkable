(import ../state)
(import ../util)
(import ../node)

## Grammar

(defn- paragraph [text]
  [:paragraph @{:open? true :inlines? true} @[text]])

(def grammar
  ~{:paragraph (/ :text ,paragraph)})

## Functions

(defn- paragraph-append [a-paragraph continuation protocols]
  (array/concat (node/children-of a-paragraph) (node/children-of continuation)))

(defn- paragraph-blank [a-paragraph parent protocols]
  (node/attribute a-paragraph :open? false)
  nil)

(defn- paragraph-equal? [a-paragraph block]
  (or (= :paragraph (node/type-of block))
      (and (= :heading (node/type-of block))
           (= :setext (node/attribute block :kind)))))

(defn- paragraph-follower [a-paragraph block]
  (when (= :paragraph (node/type-of block))
    a-paragraph))

(defn- paragraph-lazy? [a-paragraph]
  true)

(defn- paragraph-next-block [a-paragraph line pos grammar protocols]
  (def result (peg/match grammar line pos))
  (def block (get result 0))
  (defn heading? []
    (and (= :thematic-break (node/type-of block))
         (= 45 (node/attribute block :char))
         (peg/match ~(* (any " ") (some "-") (any " ") -1) (first (node/children-of block)))))
  (cond
    (heading?)
    [[:heading @{:level 2 :open? false :inlines? true :kind :setext} @["-"]] (length line)]
    ((node/get-fn :needs-nl? block protocols) block)
    [(util/to-continuation :paragraph line pos) (length line)]
    # default
    result))

(util/add-to state/protocols
  {:blocks
    {:paragraph {:append     paragraph-append
                 :blank      paragraph-blank
                 :equal?     paragraph-equal?
                 :follower   paragraph-follower
                 :lazy?      paragraph-lazy?
                 :next-block paragraph-next-block}}})
