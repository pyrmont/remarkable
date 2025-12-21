(import ../state)

## Grammar

(defn- blank []
  (when (< (- state/col-edge state/col-pos) 4)
    [:blank]))

(def grammar
  ~{:blank (cmt :nl ,blank)})
