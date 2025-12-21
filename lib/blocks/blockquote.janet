(import ../state)
(import ../util)

(defn- blockquote [trailing-space]
  (++ state/col-edge)
  (set state/col-pos state/col-edge)
  (util/record-padding trailing-space)
  (unless (empty? trailing-space)
    (++ state/col-pos))
  [:blockquote @{:container? true :open? true} @[]])

(def grammar
  ~{:blockquote (/ (* ">" '(any :space)) ,blockquote)})

(defn- blockquote-see-blank [a-blockquote functions]
  (when (util/attribute a-blockquote :open?)
    (util/attribute a-blockquote :open? false)
    (util/close-children a-blockquote functions)))

(util/add-to state/rules
  @{:blocks
    @{:blockquote {:see-blank   blockquote-see-blank}}})
