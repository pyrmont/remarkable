(use ../deps/testament)

(import ../init :as remarkable)

(deftest custom-block-note
  (remarkable/reset-state)
  (defn- note-block [content]
    [:note @{:container? true :open? true} @[content]])
  (def note-grammar
    ~{:note (/ '(* "NOTE:" (to :eol)) ,note-block)})
  (def note-protocol
    {:blocks
      {:note (remarkable/extend/make-container-protocol)}})
  (defn- render-note [node renderers opts]
    (def [_ _ children] node)
    (def buf @"")
    (buffer/push buf "<div class=\"note\" style=\"border-left: 4px solid #007acc; padding-left: 1em; margin: 1em 0;\">")
    (buffer/push buf "<strong>Note:</strong> ")
    (each child children
      (buffer/push buf (string/slice child 6)))
    (buffer/push buf "</div>")
    (string buf))
  (def custom-renderers
    {:note render-note})
  (def text
    ```
    # Custom Block Example

    This is regular markdown.

    NOTE: This is a custom note block that will be styled differently!

    More regular markdown here.
    ```)
  # Add custom block type
  (remarkable/extend/add-block :note note-grammar note-protocol)
  (def custom-grammar (remarkable/extend/build-blocks-grammar))
  (def custom-protocols (remarkable/extend/build-protocols))
  (def ast (remarkable/parse-md text
             :blocks (peg/compile custom-grammar)
             :protocols custom-protocols))
  (def html (remarkable/render-html ast {:renderers custom-renderers}))
  (def expect
    ```
    <h1>Custom Block Example</h1>
    <p>This is regular markdown.</p>
    <div class="note" style="border-left: 4px solid #007acc; padding-left: 1em; margin: 1em 0;"><strong>Note:</strong> This is a custom note block that will be styled differently!</div>
    <p>More regular markdown here.</p>
    ```)
  (is (== (string expect "\n") html)))

(deftest custom-block-and-inline-footnotes
  (remarkable/reset-state)
  (defn- footnote-def [content]
    [:footnote-def @{:container? true :open? true} @[content]])
  (def footnote-block-grammar
    ~{:footnote-def (/ '(* "[^" (to :eol)) ,footnote-def)})
  (var footnote-defs @[])
  (defn- footnote-see-blank [node protocols]
    (when (remarkable/node/attribute node :open?)
      (remarkable/node/attribute node :open? false)
      (array/push footnote-defs node)
      (remarkable/node/close-children node protocols)))
  (defn- footnote-ref [label]
    [:footnote-ref @{} label])
  (def footnote-inline-grammar
    ~{:footnote-ref (/ (* "[^" '(some (if-not "]" 1)) "]") ,footnote-ref)})
  (def footnote-protocol (remarkable/extend/make-container-protocol
                           {:see-blank footnote-see-blank}))
  (defn- render-footnote-ref [node renderers opts]
    (def [_ _ lbl] node)
    (string "<sup id=\"fnref-" lbl "\"><a href=\"#fn-" lbl "\">" lbl "</a></sup>"))
  (defn- render-footnote-def [node renderers opts]
    (def [_ _ children] node)
    (def content (first children))
    (def m (peg/match ~(* "[^" '(to "]") "]: " '(to -1)) content))
    (def [lbl text] m)
    (def buf @"")
    (buffer/push buf "<li id=\"fn-" lbl "\">" text)
    (buffer/push buf " <a href=\"#fnref-" lbl "\">↩</a></li>")
    (string buf))
  (defn- render-document-with-footnotes [node renderers opts]
    (def [_ _ children] node)
    (def buf @"")
    (each child children
      (def child-type (get child 0))
      (unless (= child-type :footnote-def)
        (if-let [renderer (get renderers child-type)]
          (buffer/push buf (renderer child renderers {:start-nl? false}) "\n")
          (buffer/push buf (string child) "\n"))))
    (when (> (length footnote-defs) 0)
      (buffer/push buf "<div class=\"footnotes\">\n<ol>\n")
      (each footnote footnote-defs
        (buffer/push buf (render-footnote-def footnote renderers opts) "\n"))
      (buffer/push buf "</ol>\n</div>\n"))
    (string buf))
  (def footnote-renderers
    {:footnote-ref render-footnote-ref
     :footnote-def render-footnote-def
     :document render-document-with-footnotes})
  (def text
    ```
    # Example with Footnotes

    This is a paragraph with a footnote[^1] reference.

    Here's another footnote[^note] with a custom label.

    [^1]: This is the first footnote.

    [^note]: This is a named footnote with more content.

    And here's some text after the footnote definitions.
    ```)
  (remarkable/extend/add-block :footnote-def footnote-block-grammar footnote-protocol :first? true)
  (remarkable/extend/add-inline :footnote-ref footnote-inline-grammar nil :first? true)
  (def custom-blocks-grammar (remarkable/extend/build-blocks-grammar))
  (def custom-inlines-grammar (remarkable/extend/build-inlines-grammar))
  (def custom-protocols (remarkable/extend/build-protocols))
  (def ast (remarkable/parse-md text
             :blocks (peg/compile custom-blocks-grammar)
             :inlines (peg/compile custom-inlines-grammar)
             :protocols custom-protocols))
  (def html (remarkable/render-html ast {:renderers footnote-renderers}))
  (def expect
    ```
    <h1>Example with Footnotes</h1>
    <p>This is a paragraph with a footnote<sup id="fnref-1"><a href="#fn-1">1</a></sup> reference.</p>
    <p>Here's another footnote<sup id="fnref-note"><a href="#fn-note">note</a></sup> with a custom label.</p>
    <p>And here's some text after the footnote definitions.</p>
    <div class="footnotes">
    <ol>
    <li id="fn-1">This is the first footnote. <a href="#fnref-1">↩</a></li>
    <li id="fn-note">This is a named footnote with more content. <a href="#fnref-note">↩</a></li>
    </ol>
    </div>
    ```)
  (is (== (string expect "\n") html)))

(deftest custom-inline-with-delimiters
  (remarkable/reset-state)
  (defn- highlight [text]
    [:highlight @{} text])
  (def highlight-grammar
    ~{:highlight (/ (* "^^" '(to "^^") "^^") ,highlight)})
  (remarkable/extend/add-inline :highlight highlight-grammar nil :delimiters "^" :first? true)
  (def custom-inlines (remarkable/extend/build-inlines-grammar))
  (def text "This is ^^highlighted^^ text.")
  (def ast (remarkable/parse-md text :inlines (peg/compile custom-inlines)))
  (defn- render-highlight [node renderers opts]
    (def [_ _ content] node)
    (string "<mark>" content "</mark>"))
  (def html (remarkable/render-html ast {:renderers {:highlight render-highlight}}))
  (def expect "<p>This is <mark>highlighted</mark> text.</p>\n")
  (is (== expect html)))

(run-tests!)
