(import ./util)

(defn merge-grammars
  ```
  Merges custom grammar rules into a base grammar

  Returns a new grammar table suitable for use with parse-md's :blocks or
  :inlines parameter. When called, `base-grammar` is the base grammar (e.g.
  blocks/grammar or inlines/grammar) and `custom-grammar` is the custom grammar
  rules to add.

  For example:

  ``janet
  (def custom-grammar ~{:footnote {:main (* "[^" :text "]")}})
  (def merged (merge-grammars blocks/grammar custom-grammar))
  (parse-md text :blocks (peg/compile merged))
  ``
  ```
  [base-grammar custom-grammar]
  (def merged @{})
  (util/add-to merged base-grammar)
  (util/add-to merged custom-grammar)
  merged)

(defn merge-protocols
  ```
  Merges custom block/inline protocols into base protocols

  Returns a new protocols table suitable for use with parse-md's :protocols
  parameter. When called, `base-protocols` is the base protocols (e.g.
  state/protocols) and `custom-protocol` is the custom protocol to add.

  For example:

  ``janet
  (def custom-protocol
    {:blocks {:footnote {:equal? footnote-equal?}}})
  (def merged (merge-protocols state/protocols custom-protocol))
  (parse-md text :protocols merged)
  ``
  ```
  [base-protocols custom-protocol]
  (def merged @{})
  (util/add-to merged base-protocols)
  (util/add-to merged custom-protocol)
  merged)
