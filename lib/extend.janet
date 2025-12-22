(import ./util)
(import ./state)
(import ./blocks)
(import ./inlines)
(import ./container)

(defn add-block
  ```
  Adds a custom block type

  This function adds a custom block type to the parser. The block must have a
  unique `block-type` and a `block-grammar` comprised of PEG combinators.
  Optionally, a `block-protocol` table can be provided with protocol functions
  for the block type.

  If the block type has high precedence, the named parameter `:first?` can be
  set to `true` and the block type be matched before existing blocks.

  The block type, grammar, and protocol are stored in global state, which is
  used by build-blocks-grammar and build-protocols to construct the final
  grammar and protocols.
  ```
  [block-type block-grammar &opt block-protocol &named first?]
  (if first?
    (array/insert state/blocks 1 block-type)
    (array/insert state/blocks (dec (length state/blocks)) block-type))
  (put state/custom-block-grammars block-type block-grammar)
  (when block-protocol
    (put state/custom-block-protocols block-type block-protocol)))

(defn build-blocks-grammar
  ```
  Builds a complete block grammar from state/blocks and
  state/custom-block-grammars

  This function uses the custom grammar rules that were registered via
  add-block, merging them with the base blocks grammar. Returns a grammar table
  ready to be compiled with peg/compile
  ```
  []
  (def grammar @{})
  (util/add-to grammar blocks/grammar)
  # Add all custom block grammars from state
  (each block-type (keys state/custom-block-grammars)
    (util/add-to grammar (get state/custom-block-grammars block-type)))
  # Rebuild :block rule with current state/blocks
  (put grammar :block ~(* :padding ,(tuple ;state/blocks)))
  grammar)

(defn add-inline
  ```
  Adds a custom inline type

  This function adds a custom inline type to the parser. The inline must have a
  unique `inline-type` and a `inline-grammar` comprised of PEG combinators.
  Optionally, an `inline-protocol` table can be provided with protocol functions
  for the inline type.

  If the inline type has high precedence, the named parameter `:first?` can be
  set to `true` and the inline type be matched before existing inlines.

  The named parameter `:delimiters` must be used to specify characters that start
  this inline pattern if they are not existing delimiters (e.g. `_` and `*`).

  The inline type, grammar, and protocol are stored in global state, which is
  used by build-inlines-grammar and build-protocols to construct the final
  grammar and protocols.
  ```
  [inline-type inline-grammar &opt inline-protocol &named first? delimiters]
  (if first?
    (array/insert state/inlines 1 inline-type)
    (array/insert state/inlines (dec (length state/inlines)) inline-type))
  (put state/custom-inline-grammars inline-type inline-grammar)
  (when inline-protocol
    (put state/custom-inline-protocols inline-type inline-protocol))
  (when delimiters
    (buffer/push state/custom-inline-delimiters delimiters)))

(defn build-inlines-grammar
  ```
  Builds a complete inline grammar from state/inlines and
  state/custom-inline-grammars

  This function uses the custom grammar rules that were registered via
  add-inline, merging them with the base inlines grammar. If custom inline
  delimiters were specified, it creates a custom :char rule that excludes
  those characters. Returns a grammar table ready to be compiled with peg/compile
  ```
  []
  (def grammar @{})
  (util/add-to grammar inlines/grammar)
  # Add all custom inline grammars from state
  (each inline-type (keys state/custom-inline-grammars)
    (util/add-to grammar (get state/custom-inline-grammars inline-type)))
  # Build custom :char rule if there are custom delimiters
  (when (> (length state/custom-inline-delimiters) 0)
    (def excluded (string "&<>*_[]!`\\" state/custom-inline-delimiters))
    (put grammar :char
      ~(if-not (+ (set ,excluded)
                  (* (at-least 2 :space) "\n"))
           (quote 1))))
  # Rebuild :inline rule with current state/inlines
  (put grammar :inline ~(+ ,(tuple ;state/inlines)))
  grammar)

(defn build-protocols
  ```
  Builds complete protocols from state/protocols and custom protocols

  This function uses the custom protocols that were registered via add-block
  and add-inline, merging them with the base protocols. Returns a protocols
  table ready to be used with parse-md.
  ```
  []
  (def protocols @{})
  (util/add-to protocols state/protocols)
  # Add custom block protocols
  (def blocks-table (get protocols :blocks))
  (each block-type (keys state/custom-block-protocols)
    (put blocks-table block-type (get state/custom-block-protocols block-type)))
  # Add custom inline protocols
  (def inlines-table (get protocols :inlines))
  (each inline-type (keys state/custom-inline-protocols)
    (put inlines-table inline-type (get state/custom-inline-protocols inline-type)))
  protocols)

(defn make-container-protocol
  ```
  Creates a container block protocol with optional overrides

  This is a convenience wrapper around container/make-protocol for creating
  protocols for custom container blocks (blocks that can contain other blocks).

  Container blocks share common behavior for managing child blocks. This function
  returns base implementations that can be customized via the optional `overrides`
  parameter.

  Overrides:
  - `:equal?`: Custom equality check (default: type equality)
  - `:see-blank`: Called when blank line seen (default: close container)
  - `:blank`: Handle blank lines (default: pass to next container)
  - `:next-block`: Custom next block matching (default: delegate to grammar)
  - `:needs-nl?`: Whether container needs newline before closing
  ```
  [&opt overrides]
  (container/make-protocol overrides))
