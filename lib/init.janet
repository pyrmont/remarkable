(import ./state)

(import ./blocks)
(import ./inlines)
(import ./parser)
(import ./renderers/html)

(defn parse-md
  [input &named blocks inlines functions priorities]
  (default blocks (peg/compile blocks/grammar))
  (default inlines (peg/compile inlines/grammar))
  (default functions state/rules)
  (default priorities state/priorities)
  (-> input
      (parser/parse-blocks blocks functions)
      (parser/parse-all-inlines inlines functions priorities)))

(defn render-html
  [node &opt opts]
  (html/render node opts))
