(use ../globals)
(use ../utilities)


## Grammar

(defn- heading-atx [level &opt content]
  (def text (if (nil? content) "" (string/trim content)))
  [:heading @{:level (length level) :open? false :inlines? true :kind :atx} @[text]])


(defn- heading-setext [chars]
  (def level (if (= 61 (first chars)) 1 2))
  [:heading @{:level level :open? false :inlines? true :kind :setext} @[chars]])


(def grammar
  ~{:heading {:main   (+ :atx :setext)
              :atx    {:main  (/ (* :open (? (* :space :text)) :close) ,heading-atx)
                       :open  '(between 1 6 "#")
                       :text  '(to :close)
                       :close (+ (* (any :space) :eol)
                                 (* (> -1 " ") (some "#") (any :space) :eol))}
              :setext (/ (* '(+ (some "=") (some "-")) (any :space) :eol) ,heading-setext)}})

## Functions


(defn- heading-equal? [a-heading block]
  (= :paragraph (type-of block)))


(defn- heading-lazy? [a-heading]
  (= :setext (attribute a-heading :kind)))


(defn- heading-follower [a-heading block]
  (when (= :paragraph (type-of block))
    a-heading))


(defn- heading-replace [a-heading siblings]
  (case (attribute a-heading :kind)
    :atx
    (array/push siblings a-heading)

    :setext
    (if (= :paragraph (type-of (array/peek siblings)))
      (do
        (def last-p (array/pop siblings))
        (def children (get a-heading 2))
        (array/clear children)
        (array/concat children (get last-p 2))
        (array/push siblings a-heading))
      (do
        (def text (first (get a-heading 2))) # TODO Can we assume a heading only has one element?
        (if (attribute (array/peek siblings) :open?)
          (do
            (def parent (last-descendant (array/peek siblings)))
            (array/push (get parent 2) text))
          (array/push siblings [:paragraph @{:open? true :inlines? true} @[text]]))))))


(add-to rules
  {:blocks
    {:heading {:equal?      heading-equal?
               :follower    heading-follower
               :lazy?       heading-lazy?
               :replace     heading-replace}}})
