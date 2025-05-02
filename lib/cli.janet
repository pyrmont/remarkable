(import ../init :as remark)

(defn main [& args]
  (case (length args)
    1 (-> (file/read stdin :all) remark/parse-md remark/render-html prin)
    2 (-> (get args 1) remark/parse-md remark/render-html prin)))
