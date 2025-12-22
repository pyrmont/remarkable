(import ../util)
(import ../node)

(defn- html-content [buf line]
  (each c line
    (buffer/push buf
      (case c
        34 "&quot;"
        38 "&amp;"
        60 "&lt;"
        62 "&gt;"
        c)))
  buf)

(defn- to-fragment [node]
  (def buf @"")
  (each child (node/children-of node)
    (if (bytes? child)
      (buffer/push buf child)
      (buffer/push buf (to-fragment child))))
  buf)

# Forward declaration for mutual recursion
(var render-with-registry nil)

# Inline renderers
(defn- render-codespan [node renderers opts]
  (def [_ _ children] node)
  (def buf @"")
  (buffer/push buf "<code>")
  (html-content buf children)
  (buffer/push buf "</code>")
  (string buf))

(defn- render-emphasis [node renderers opts]
  (def [_ _ children] node)
  (def buf @"")
  (buffer/push buf "<em>")
  (each child children
    (if (bytes? child)
      (html-content buf child)
      (buffer/push buf (render-with-registry child renderers opts))))
  (buffer/push buf "</em>")
  (string buf))

(defn- render-image [node renderers opts]
  (def [_ attrs children] node)
  (def buf @"")
  (buffer/push buf "<img src=\"" (get attrs :url) "\"")
  (when (get attrs :title)
    (buffer/push buf " title=\"")
    (html-content buf (get attrs :title))
    (buffer/push buf "\""))
  (buffer/push buf " alt=\"")
  (each child children
    (if (bytes? child)
      (buffer/push buf child)
      (buffer/push buf (to-fragment child))))
  (buffer/push buf "\" />")
  (string buf))

(defn- render-hardbreak [node renderers opts]
  "<br />\n")

(defn- render-link [node renderers opts]
  (def [_ attrs children] node)
  (def buf @"")
  (buffer/push buf "<a href=\"" (get attrs :url) "\"")
  (when (get attrs :title)
    (buffer/push buf " title=\"")
    (html-content buf (get attrs :title))
    (buffer/push buf "\""))
  (buffer/push buf ">")
  (each child children
    (if (bytes? child)
      (html-content buf child)
      (buffer/push buf (render-with-registry child renderers opts))))
  (buffer/push buf "</a>")
  (string buf))

(defn- render-rawhtml [node renderers opts]
  (def [_ _ children] node)
  children)

(defn- render-strong [node renderers opts]
  (def [_ _ children] node)
  (def buf @"")
  (buffer/push buf "<strong>")
  (each child children
    (if (bytes? child)
      (html-content buf child)
      (buffer/push buf (render-with-registry child renderers opts))))
  (buffer/push buf "</strong>")
  (string buf))

# Block renderers
(defn- render-document [node renderers opts]
  (def [_ _ children] node)
  (def buf @"")
  (each child children
    (buffer/push buf (render-with-registry child renderers {:start-nl? false}) "\n"))
  (string buf))

(defn- render-blockquote [node renderers opts]
  (def [_ _ children] node)
  (def buf @"")
  (buffer/push buf "<blockquote>")
  (each child children
    (buffer/push buf (render-with-registry child renderers opts)))
  (buffer/push buf "\n</blockquote>")
  (string buf))

(defn- render-codeblock [node renderers opts]
  (def [_ _ children] node)
  (def info (node/attribute node :info))
  (def buf @"")
  (buffer/push buf
               "<pre><code"
               (if (nil? info) "" (string " class=\"language-" info "\""))
                ">")
  (each line children
    (html-content buf line))
  (buffer/push buf "</code></pre>")
  (string buf))

(defn- render-heading [node renderers opts]
  (def [_ _ children] node)
  (def level (string (node/attribute node :level)))
  (def buf @"")
  (buffer/push buf "<h" level ">")
  (each child children
    (if (bytes? child)
      (html-content buf child)
      (buffer/push buf (render-with-registry child renderers opts))))
  (buffer/push buf "</h" level ">")
  (string buf))

(defn- render-html [node renderers opts]
  (def [_ _ children] node)
  (def buf @"")
  (each line children
    (buffer/push buf line))
  (string buf))

(defn- render-list [node renderers opts]
  (def [_ _ children] node)
  (def list-tag (case (node/attribute node :kind) :bullet "ul" :ordinal "ol"))
  (def list-start (node/attribute node :start))
  (def list-attrs (if (or (nil? list-start) (= 1 list-start))
                    ""
                    (string " start=\"" list-start "\"")))
  (def buf @"")
  (buffer/push buf "<" list-tag list-attrs ">")
  (each item children
    (buffer/push buf "\n<li>")
    (var closing-nl? false)
    (each child (node/children-of item)
      (if (and (node/attribute node :tight?)
               (= :paragraph (get child 0)))
        (buffer/push buf (render-with-registry child renderers (merge opts {:start-nl? false :inner? true})))
        (do
          (buffer/push buf (render-with-registry child renderers opts))
          (set closing-nl? true))))
    (when closing-nl?
      (buffer/push buf "\n"))
    (buffer/push buf "</li>"))
  (buffer/push buf "\n</" list-tag ">")
  (string buf))

(defn- render-paragraph [node renderers opts]
  (def [_ _ children] node)
  (def {:inner? inner?} opts)
  (def buf @"")
  (unless inner?
    (buffer/push buf "<p>"))
  (each child children
    (if (bytes? child)
      (html-content buf child)
      (buffer/push buf (render-with-registry child renderers opts))))
  (unless inner?
    (buffer/push buf "</p>"))
  (string buf))

(defn- render-thematic-break [node renderers opts]
  "<hr />")

# Default renderer registry
(defn- default-renderers []
  {:document render-document
   :blockquote render-blockquote
   :codeblock render-codeblock
   :heading render-heading
   :html render-html
   :list render-list
   :paragraph render-paragraph
   :thematic-break render-thematic-break
   :codespan render-codespan
   :emphasis render-emphasis
   :image render-image
   :hardbreak render-hardbreak
   :link render-link
   :rawhtml render-rawhtml
   :strong render-strong})

# Render with registry (defined for mutual recursion)
(set render-with-registry
  (fn [node renderers opts]
    (def {:start-nl? start-nl?} opts)
    (def buf @"")
    (when start-nl? (buffer/push buf "\n"))
    (def [kind _ _] node)
    (if-let [renderer (get renderers kind)]
      (buffer/push buf (renderer node renderers opts))
      (errorf "No renderer for node type: %q" kind))
    (string buf)))

(defn render [node &opt opts]
  (default opts {})
  (def renderers (merge (default-renderers) (or (get opts :renderers) {})))
  (render-with-registry node renderers opts))
