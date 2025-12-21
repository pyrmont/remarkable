(def protocols @{})
(def priorities @{})

(def indents @[])
(def links @{})
(def delimiters @[])

(var col-edge 0)
(var col-pos 0)

(defn reset-block-globals []
  (array/clear indents)
  (each k (keys links)
    (put links k nil)))

(defn reset-inline-globals []
  (array/clear delimiters))

(defn reset-cols []
  (set col-edge 0)
  (set col-pos 0))
