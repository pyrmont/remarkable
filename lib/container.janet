(import ./state)
(import ./util)
(import ./node)

(defn- blank [node parent protocols]
  (node/next-container node))

(defn- equal? [node block]
  (= (node/type-of node) (node/type-of block)))

(defn- next-block [node line pos grammar protocols]
  (peg/match grammar line pos))

(defn- see-blank [node protocols]
  nil)

(defn make-protocol
  ```
  Creates a table of standard container block protocol functions

  Container blocks (e.g. blockquotes, lists) are block-level elements that can
  contain other blocks. They share common behavior for managing child blocks.
  This function returns base implementations that can be customized via the
  optional `overrides` parameter.

  Overrides:

  ``
  :equal?     - Custom equality check (default: type equality)
  :see-blank  - Called when blank line seen (default: close container)
  :blank      - Handle blank lines (default: pass to next container)
  :next-block - Custom next block matching (default: delegate to grammar)
  :needs-nl?  - Whether container needs newline before closing
  ``

  The base implementations:

  - `equal?` Checks if node type matches block type
  - `see-blank` Closes container and all open children
  - `blank` Continues to next open container
  - `next-block` Delegates to grammar for parsing

  Example (simple container):

  ``janet
    (util/add-to state/protocols
      {:blocks {:blockquote (container/make-protocol)}})
  ``

  Example (container with custom behavior):

  ``janet
    (util/add-to state/protocols
      {:blocks {:list (container/make-protocol
                        {:equal?     list-equal?
                         :blank      list-blank
                         :next-block list-next-block})}})
  ``
  ```
  [&opt overrides]
  (default overrides {})
  @{:equal?     (get overrides :equal? equal?)
    :see-blank  (get overrides :see-blank see-blank)
    :blank      (get overrides :blank blank)
    :next-block (get overrides :next-block next-block)
    :needs-nl?  (get overrides :needs-nl?)})
