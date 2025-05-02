(use ./globals)


(import ./blocks)
(import ./inlines)
(import ./parser)
(import ./renderers/html)


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
