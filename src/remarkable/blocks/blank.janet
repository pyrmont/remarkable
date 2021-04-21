(use ../globals)
(use ../utilities)


## Grammar

(defn- blank []
  (when (< (- col-edge col-pos) 4)
    [:blank]))


(def grammar
  ~{:blank (cmt :nl ,blank)})
