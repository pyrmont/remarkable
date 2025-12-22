(import ../res/helpers/util :as util)

(import ../init :as remark)

(def examples
  (parse (slurp "res/fixtures/cmark_spec.jdn")))

(defn render [input]
  (-> (remark/parse-md input) (remark/render-html {:start-nl? false})))

(if (def ex-num (-?> (dyn :args) (get 1) scan-number))
  (util/check-example render (get examples (dec ex-num)))
  (util/check-examples render examples))
