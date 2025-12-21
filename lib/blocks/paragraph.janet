(import ../state)
(import ../util)

## Grammar

(defn- paragraph [text]
  [:paragraph @{:open? true :inlines? true} @[text]])

(def grammar
  ~{:paragraph (/ :text ,paragraph)})

## Functions

(defn- paragraph-append [a-paragraph continuation functions]
  (array/concat (util/children-of a-paragraph) (util/children-of continuation)))

(defn- paragraph-blank [a-paragraph parent functions]
  (util/attribute a-paragraph :open? false)
  nil)

(defn- paragraph-equal? [a-paragraph block]
  (or (= :paragraph (util/type-of block))
      (and (= :heading (util/type-of block))
           (= :setext (util/attribute block :kind)))))

(defn- paragraph-follower [a-paragraph block]
  (when (= :paragraph (util/type-of block))
    a-paragraph))

(defn- paragraph-lazy? [a-paragraph]
  true)

(defn- paragraph-next-block [a-paragraph line pos grammar functions]
  (def result (peg/match grammar line pos))
  (def block (get result 0))
  (defn heading? []
    (and (= :thematic-break (util/type-of block))
         (= 45 (util/attribute block :char))
         (peg/match ~(* (any " ") (some "-") (any " ") -1) (first (util/children-of block)))))
  (cond
    (heading?)
    [[:heading @{:level 2 :open? false :inlines? true :kind :setext} @["-"]] (length line)]
    ((util/get-fn :needs-nl? block functions) block)
    [(util/to-continuation :paragraph line pos) (length line)]
    # default
    result))

(util/add-to state/rules
  {:blocks
    {:paragraph {:append     paragraph-append
                 :blank      paragraph-blank
                 :equal?     paragraph-equal?
                 :follower   paragraph-follower
                 :lazy?      paragraph-lazy?
                 :next-block paragraph-next-block}}})
