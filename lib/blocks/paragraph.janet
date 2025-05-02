(use ../globals)
(use ../utilities)


## Grammar

(defn- paragraph [text]
  [:paragraph @{:open? true :inlines? true} @[text]])


(def grammar
  ~{:paragraph (/ :text ,paragraph)})


## Functions

(defn- paragraph-append [a-paragraph continuation functions]
  (array/concat (children-of a-paragraph) (children-of continuation)))


(defn- paragraph-blank [a-paragraph parent functions]
  (attribute a-paragraph :open? false)
  nil)


(defn- paragraph-equal? [a-paragraph block]
  (or (= :paragraph (type-of block))
      (and (= :heading (type-of block))
           (= :setext (attribute block :kind)))))


(defn- paragraph-follower [a-paragraph block]
  (when (= :paragraph (type-of block))
    a-paragraph))


(defn- paragraph-lazy? [a-paragraph]
  true)


(defn- paragraph-next-block [a-paragraph line pos grammar functions]
  (def result (peg/match grammar line pos))
  (def block (get result 0))

  (defn heading? []
    (and (= :thematic-break (type-of block))
         (= 45 (attribute block :char))
         (peg/match ~(* (any " ") (some "-") (any " ") -1) (first (children-of block)))))

  (cond
    (heading?)
    [[:heading @{:level 2 :open? false :inlines? true :kind :setext} @["-"]] (length line)]

    ((get-fn :needs-nl? block functions) block)
    [(to-continuation :paragraph line pos) (length line)]

    result))


(add-to rules
  {:blocks
     {:paragraph  {:append      paragraph-append
                  :blank       paragraph-blank
                  :equal?      paragraph-equal?
                  :follower    paragraph-follower
                  :lazy?       paragraph-lazy?
                  :next-block  paragraph-next-block}}})
