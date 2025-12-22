(def protocols @{})
(def priorities @{})

(def indents @[])
(def links @{})
(def delimiters @[])

# Default blocks list for building custom grammars
(def blocks @['+
              :blank
              :codeblock
              :t-break
              :html
              :linkdef
              :blockquote
              :list
              :heading
              :paragraph])

# Default inlines list for building custom grammars
(def inlines @['+
               :codespan
               :rawhtml
               :autolink
               :hardbreak
               :emphasis
               :link
               :text])

# Custom block grammars for building custom grammars
(def custom-block-grammars @{})

# Custom inline grammars for building custom grammars
(def custom-inline-grammars @{})

# Custom block protocols for building custom protocols
(def custom-block-protocols @{})

# Custom inline protocols for building custom protocols
(def custom-inline-protocols @{})

# Custom inline delimiters for excluding from :char
(def custom-inline-delimiters @"")

(var col-edge 0)
(var col-pos 0)

(defn reset-block-globals []
  (array/clear indents)
  (each k (keys links)
    (put links k nil)))

(defn reset-inline-globals []
  (array/clear delimiters))

(defn reset-cols []
  (set col-edge 0)
  (set col-pos 0))
