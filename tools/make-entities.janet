(import ../deps/medea/lib/decode :as json)


(def input "entities.json") # https://html.spec.whatwg.org/entities.json
(def output "lib/entities.janet")
(def indent "   ")


(def valid-entity-fn
  ```
  (defn valid-entity? [entity]
    (not= nil (get entity-map entity)))
  ```)


(def to-grapheme-fn
  ```
  (defn to-grapheme [entity]
    (get entity-map entity))
  ```)


(defn get-chars [mappings k]
  (-> (get mappings k) (get "characters")))


(with [dest (file/open output :w)]
  (def mappings (json/decode (slurp input)))
  (def ks (->> (keys mappings) (filter |(string/has-suffix? ";" $)) sort))
  (file/write dest "(def entity-map\n  {\n")
  (loop [k :in ks]
    (file/write dest indent)
    (file/write dest (string/format "%j %j\n" k (get-chars mappings k))))
  (file/write dest "})")
  (file/write dest "\n\n\n")
  (file/write dest valid-entity-fn)
  (file/write dest "\n\n\n")
  (file/write dest to-grapheme-fn))
