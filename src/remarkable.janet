(use ./remarkable/globals)


(import ./remarkable/blocks)
(import ./remarkable/inlines)
(import ./remarkable/parser)
(import ./remarkable/renderers/html)


(defn parse-md [input &opt your-blocks your-inlines your-functions your-priorities]
  (default your-blocks (-> (table/to-struct blocks/grammar) peg/compile))
  (default your-inlines (-> (table/to-struct inlines/grammar) peg/compile))
  (default your-functions rules)
  (default your-priorities priorities)
  (-> input
      (parser/parse-blocks your-blocks your-functions)
      (parser/parse-all-inlines your-inlines your-functions your-priorities)))


(defn render-html [node &opt opts]
  (html/render node opts))


(defn main [& args]
  (case (length args)
    1 (-> (file/read stdin :all) parse-md render-html prin)
    2 (-> (get args 1) parse-md render-html prin)))
