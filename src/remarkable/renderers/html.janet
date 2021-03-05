(use ../utilities)


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
  (each child (children-of node)
    (if (bytes? child)
      (buffer/push buf child)
      (buffer/push buf (to-fragment child))))
  buf)


(defn- render-inline [node]
  (def buf @"")
  (def [kind attrs children] node)

  (case kind
    :codespan
    (do
      (buffer/push buf "<code>")
      (html-content buf children)
      (buffer/push buf "</code>"))

    :emphasis
    (do
      (buffer/push buf "<em>")
      (each child children
        (if (bytes? child)
          (html-content buf child)
          (buffer/push buf (render-inline child))))
      (buffer/push buf "</em>"))

    :image
    (do
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
      (buffer/push buf "\" />"))

    :hardbreak
    (buffer/push buf "<br />\n")

    :link
    (do
      (buffer/push buf "<a href=\"" (get attrs :url) "\"")
      (when (get attrs :title)
        (buffer/push buf " title=\"")
        (html-content buf (get attrs :title))
        (buffer/push buf "\""))
      (buffer/push buf ">")
      (each child children
        (if (bytes? child)
          (html-content buf child)
          (buffer/push buf (render-inline child))))
      (buffer/push buf "</a>"))

    :rawhtml
    (buffer/push buf children)

    :strong
    (do
      (buffer/push buf "<strong>")
      (each child children
        (if (bytes? child)
          (html-content buf child)
          (buffer/push buf (render-inline child))))
      (buffer/push buf "</strong>")))

  (string buf))


(defn render [node &opt opts]
  (default opts {})
  (def {:start-nl? start-nl?
        :inner?    inner?} opts)
  (def buf @"")
  (when start-nl? (buffer/push buf "\n"))

  (def [kind attrs children] node)

  (case kind
    :document
    (each child children
      (buffer/push buf (render child {:start-nl? false}) "\n"))

    :blockquote
    (do
      (buffer/push buf "<blockquote>")
      (each child children
        (buffer/push buf (render child)))
      (buffer/push buf "\n</blockquote>"))

    :codeblock
    (do
      (def info (attribute node :info))
      (buffer/push buf
                   "<pre><code"
                   (if (nil? info) "" (string " class=\"language-" info "\""))
                    ">")
      (each line children
        (html-content buf line))
      (buffer/push buf "</code></pre>"))

    :heading
    (do
      (def level (string (attribute node :level)))
      (buffer/push buf "<h" level ">")
      (each child children
        (if (bytes? child)
          (html-content buf child)
          (buffer/push buf (render-inline child))))
      (buffer/push buf "</h" level ">"))

    :html
    (each line children
      (buffer/push buf line))

    :list
    (do
      (def list-tag (case (attribute node :kind) :bullet "ul" :ordinal "ol"))
      (def list-attrs (if (def start (attribute node :start))
                        (string " start=\"" start "\"")
                        ""))
      (buffer/push buf "<" list-tag list-attrs ">")
      (each item children
        (buffer/push buf "\n<li>")
        (var closing-nl? false)
        (each child (children-of item)
          (if (and (attribute node :tight?)
                   (= :paragraph (get child 0)))
            (buffer/push buf (render child {:start-nl? false :inner? true}))
            (do
              (buffer/push buf (render child))
              (set closing-nl? true))))
        (when closing-nl?
          (buffer/push buf "\n"))
        (buffer/push buf "</li>"))
      (buffer/push buf "\n</" list-tag ">"))

    :paragraph
    (do
      (unless inner?
        (buffer/push buf "<p>"))
      (each child children
        (if (bytes? child)
          (html-content buf child)
          (buffer/push buf (render-inline child))))
      (unless inner?
        (buffer/push buf "</p>")))

    :thematic-break
    (buffer/push buf "<hr />"))

  (string buf))
