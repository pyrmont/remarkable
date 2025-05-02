(use ../globals)
(use ../utilities)


(defn- t-break [chars]
  [:thematic-break @{:char (get chars 0)} @[chars]])


(def grammar
  ~@{:t-break (/ (* '(+ (at-least 3 (* "-" (any :space)))
                        (at-least 3 (* "_" (any :space)))
                        (at-least 3 (* "*" (any :space)))) :eol) ,t-break)})
