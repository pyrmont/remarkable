(use ../globals)
(use ../utilities)


(defn- blockquote [trailing-space]
  (++ col-edge)
  (set col-pos col-edge)
  (record-padding trailing-space)
  (unless (empty? trailing-space)
    (++ col-pos))
  [:blockquote @{:container? true :open? true} @[]])


(def grammar
  ~{:blockquote (/ (* ">" '(any :space)) ,blockquote)})


(defn- blockquote-see-blank [a-blockquote functions]
  (when (attribute a-blockquote :open?)
    (attribute a-blockquote :open? false)
    (close-children a-blockquote functions)))


(add-to rules
  @{:blocks
    @{:blockquote {:see-blank   blockquote-see-blank}}})
