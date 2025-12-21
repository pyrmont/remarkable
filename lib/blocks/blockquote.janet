(import ../state)
(import ../util)
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
  (when (util/attribute a-blockquote :open?)
    (util/attribute a-blockquote :open? false)
    (util/close-children a-blockquote protocols)))

(util/add-to state/protocols
  {:blocks
    {:blockquote (container/make-protocol
                   {:see-blank blockquote-see-blank})}})
