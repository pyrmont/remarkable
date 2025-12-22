(import ./state)

(import ./blocks)
(import ./inlines)
(import ./parser)
(import ./renderers/html)

(defn parse-md
  ```
  Parses Markdown into an AST

  This function transforms `input` into an AST. It optionally takes a number of
  named parameters for extending the parser:

  - `:blocks`: custom blocks PEG
  - `:inlines`: custom inlines PEG
  - `:protocols`: custom block/inline protocols
  - `:priorities`: custom delimiter priorities for inlines

  There are helper utilities for extension in lib/extend.janet.
  ```
  [input &named blocks inlines protocols priorities]
  (default blocks (peg/compile blocks/grammar))
  (default inlines (peg/compile inlines/grammar))
  (default protocols state/protocols)
  (default priorities state/priorities)
  (-> input
      (parser/parse-blocks blocks protocols)
      (parser/parse-all-inlines inlines protocols priorities)))

(defn render-html
  ```
  Renders an AST node to HTML

  This functions transforms `root` into an HTML string. It can also be called
  with a struct/table `opts` with the following keys:

  - `:renderers`: custom renderer functions
  - `:start-nl?`: start with newline
  - `:inner?`: skip wrapper tags for paragraphs

  Render functions have the signature `(defn my-renderer [node renderers opts] ...)`.
  ```
  [root &opt opts]
  (html/render root opts))

(defn reset-state
  ```
  Resets the global state to defaults

  This function clears all custom grammars and protocols that were added via
  the extend module, resetting the state to the default configuration. This is
  primarily useful for testing when you want to ensure a clean state between
  tests.
  ```
  []
  (array/clear state/blocks)
  (array/push state/blocks ;blocks/blocks)
  (array/clear state/inlines)
  (array/push state/inlines ;inlines/inlines)
  (table/clear state/custom-block-grammars)
  (table/clear state/custom-block-protocols)
  (table/clear state/custom-inline-grammars)
  (table/clear state/custom-inline-protocols)
  (buffer/clear state/custom-inline-delimiters))
