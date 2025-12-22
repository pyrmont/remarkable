(import ../state)
(import ../util)
(import ../node)
(import ../container)

(defn- blockquote [trailing-space]
  (++ state/col-edge)
  (set state/col-pos state/col-edge)
  (util/record-padding trailing-space)
  (unless (empty? trailing-space)
    (++ state/col-pos))
  [:blockquote @{:container? true :open? true} @[]])

(def grammar
  ~{:blockquote (/ (* ">" '(any :space)) ,blockquote)})

(defn- blockquote-see-blank [a-blockquote protocols]
  (when (node/attribute a-blockquote :open?)
    (node/attribute a-blockquote :open? false)
    (node/close-children a-blockquote protocols)))

(util/add-to state/protocols
  {:blocks
    {:blockquote (container/make-protocol
                   {:see-blank blockquote-see-blank})}})
