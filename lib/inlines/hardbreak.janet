## Grammar

(defn- hardbreak []
  [:hardbreak])

(def grammar
  ~{:hardbreak (/ (+ "\\\n" (* (at-least 2 :space) "\n")) ,hardbreak)})
