(use ./globals)
(use ./utilities)


(defn- blank []
  (when (< (- col-edge col-pos) 4)
    [:blank]))


(defn- blockquote [trailing-space]
  (++ col-edge)
  (set col-pos col-edge)
  (record-padding trailing-space)
  (unless (empty? trailing-space)
    (++ col-pos))
  [:blockquote @{:container? true :open? true} @[]])


(defn- heading-atx [level &opt content]
  (def text (if (nil? content) "" (string/trim content)))
  [:heading @{:level (length level) :open? false :inlines? true :kind :atx} @[text]])


(defn- heading-setext [chars]
  (def level (if (= 61 (first chars)) 1 2))
  [:heading @{:level level :open? false :inlines? true :kind :setext} @[chars]])


(defn- codeblock-f [delim fence info]
  (def num (inc (length fence)))
  (def [lang extra]
    (if (empty? info)
      [nil]
      (if (def first-space (string/find " " info))
        [(string/slice info 0 first-space) (string/slice info (inc first-space))]
        [info nil])))
  (def indent (- col-edge col-pos))
  [:codeblock @{:open? true :kind :fenced :delim delim :num num :info lang :extra extra :indent indent} @[]])


(defn- codeblock-i [text]
  (def indent (- col-edge col-pos))
  (when (>= indent 4)
    (def extra-cols (- indent 4))
    (def extra-space (when (> extra-cols 0) (string/repeat " " extra-cols)))
    (def line (string extra-space text))
    [:codeblock @{:open? true :kind :indented} @[line]]))


(defn- html [kind & args]
  (def indent (- col-edge col-pos))
  (def [close? text] (if (one? (length args)) [nil (first args)] args))
  (def line (string (when (> indent 0) (string/repeat " " indent))
                    (if close? (string/slice text 0 -2) text)))
  [:html @{:open? (if close? false true) :kind kind} @[line]])


(defn- linkdef [content]
  [:linkdef @{:open? true} @[content]])


(defn- list-item [marker-width trailing-space &opt starts-blank?]
  (def start-pos col-pos)
  (+= col-edge marker-width)
  (set col-pos col-edge)
  (record-padding trailing-space)
  (if (empty? trailing-space)
    (++ col-pos)
    (+= col-pos (if (or starts-blank? (> (- col-edge col-pos) 4)) 1 (- col-edge col-pos))))
  [:list-item @{:container? true :open? true :width (- col-pos start-pos) :starts-blank? starts-blank?} @[]])


(defn- list [kind marker delim trailing-space &opt starts-blank?]
  (def start (when (= :ordinal kind) (scan-number marker)))
  (def marker-width (+ (length marker) (if (nil? delim) 0 1)))
  (def item (list-item marker-width trailing-space starts-blank?))
  [:list @{:kind kind :marker marker :delim delim :start start :tight? true :container? true :open? true} @[item]])


(defn- paragraph [text]
  [:paragraph @{:open? true :inlines? true} @[text]])


(defn- t-break [chars]
  [:thematic-break @{:char (get chars 0)} @[chars]])


(def grammar
  ~{:main  (* (/ :block ,update-col-pos) ($))

    :nl    "\n"
    :eol   (+ :nl -1)
    :space (set " \t")

    :padding (drop (/ '(any :space) ,record-padding))

    :char    (+ :escaped :entity (if-not :eol '1))
    :escaped (+ (* "\\" '(set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")) '(* "\\" 1))
    :entity  (/ (+ (* (constant :ent) '(* "&" (some (+ :w :d)) ";"))
                   (* (constant :dec) '(* "&#" (between 1 7 :d) ";"))
                   (* (constant :hex) '(* "&#" (set "Xx") (between 1 6 :h) ";")))
                ,entity-decode)
    :special (+ (/ '"\"" "&quot;") (/ '"&" "&amp;") (/ '"<" "&lt;") (/ '">" "&gt;"))

    :text    (* '(some (if-not :eol 1)) (? :nl))

    :block (* :padding (+ :blank :codeblock :t-break :html :linkdef :blockquote :list :heading :paragraph))

    :blank (cmt :nl ,blank)

    :codeblock {:main     (+ :indented :fenced)
                :indented {:main (cmt (* :code :eol) ,codeblock-i)
                           :code '(thru :eol)}
                :fenced   {:main  (/ (* :fence :eol) ,codeblock-f)
                           :fence (+ :tilde :tick)
                           :tilde (* '"~" '(at-least 2 "~") (any :space) (% (any :char)))
                           :tick  (* '"`" '(at-least 2 "`") (any :space) (% (any (if-not "`" :char))))}}

    :t-break (/ (* '(+ (at-least 3 (* "-" (any :space)))
                       (at-least 3 (* "_" (any :space)))
                       (at-least 3 (* "*" (any :space)))) :eol) ,t-break)

    :heading {:main   (+ :atx :setext)
              :atx    {:main  (/ (* :open (? (* :space :text)) :close) ,heading-atx)
                       :open  '(between 1 6 "#")
                       :text  '(to :close)
                       :close (+ (* (any :space) :eol)
                                 (* (> -1 " ") (some "#") (any :space) :eol))}
              :setext (/ (* '(+ (some "=") (some "-")) (any :space) :eol) ,heading-setext)}

    :html {:main   (/ (+ :type-1 :type-2 :type-3 :type-4 :type-5 :type-6 :type-7) ,html)
           :attrs  (any (* :space :attr))
           :attr   {:main  (* :name :spec)
                    :name  (* (+ :w (set "_:")) (any (+ :w :d (set "_.:-"))))
                    :spec  (* (any :space) "=" (any :space) :value)
                    :value (+ (* "\"" (thru "\""))
                              (* "'" (thru "'"))
                              (some (if-not (+ :s (set "\"'=<>`")) 1)))}
           :type-1 {:main    (* (constant 1) '(* :open (? (thru :close)) (thru :eol)))
                    :open    (* "<" :keyword (? " >"))
                    :close   (* "</" :keyword ">" (constant true))
                    :keyword ,(words "script" "style" "pre")}
           :type-2 (* (constant 2) '(* "<!--" (? (* (thru "-->") (constant true))) (thru :eol)))
           :type-3 (* (constant 3) '(* "<?" (? (* (thru "?>") (constant true))) (thru :eol)))
           :type-4 (* (constant 4) '(* "<!" (range "AZ") (? (* (thru ">") (constant true))) (thru :eol)))
           :type-5 (* (constant 5) '(* "<![CDATA[" (? (* (thru "]]>") (constant true))) (thru :eol)))
           :type-6 {:main    (* (constant 6) '(* "<" (? "/") :keyword (? (+ ">" "/>")) (thru :eol)))
                    :keyword (* ,(words "address" "article" "aside" "base"
                                        "basefont" "blockquote" "body"
                                        "caption" "center" "col" "colgroup"
                                        "dd" "details" "dialog" "dir" "div"
                                        "dl" "dt" "fieldset" "figcaption"
                                        "figure" "footer" "form" "frame"
                                        "frameset" "h1" "h2" "h3" "h4" "h5"
                                        "h6" "head" "header" "hr" "html"
                                        "iframe" "legend" "li" "link" "main"
                                        "menu" "menuitem" "nav" "noframes" "ol"
                                        "optgroup" "option" "p" "param"
                                        "section" "source" "summary" "table"
                                        "tbody" "td" "tfoot" "th" "thead"
                                        "title" "tr" "track" "ul")
                                (> 0 (+ :eol (set " />"))))}
           :type-7 {:main  (* (constant 7) '(* (+ :open :close) (any :space) :eol))
                    :open  (* "<" :name :attrs (any :space) ">")
                    :close (* "</" :name (any :space) ">")
                    :name  (* (not ,(words "script" "style" "pre")) :w (any (+ :w :d "-")))}}

    :linkdef (/ '(* "[" (to :eol)) ,linkdef)

    :blockquote (/ (* ">" '(any :space)) ,blockquote)

    :list {:main    (cmt (* :marker :after) ,list)
           :marker  (+ :bullet :ordinal)
           :bullet  (* (constant :bullet) '(set "-+*") (constant nil))
           :ordinal (* (constant :ordinal) '(between 1 9 :d) '(set ".)"))
           :after   (+ (* '(any :space) :eol (constant true))
                       (* '(some :space)))}

    :paragraph (/ :text ,paragraph)})


## Block functions

(defn- default-block-append [node block functions]
  (unless (= :blank (type-of block))
    (def replace-fn (get-fn :replace block functions))
    (if replace-fn
      (replace-fn block (children-of node))
      (if (def peer (next-container node))
        ((get-fn :continue peer functions) peer block)
        (array/push (children-of node) block)))))


(defn- default-block-blank [node parent functions]
  (next-container node))


(defn- default-block-close [node &opt parent]
  (attribute node :open? false))


(defn- default-block-continue [peer block]
  (array/concat (children-of peer) (children-of block)))


(defn- default-block-equal? [node block]
  (= (type-of node) (type-of block)))


(defn- default-block-follower [node block]
  nil)


(defn- default-block-lazy? [block]
  false)


(defn- default-block-needs-nl? [block]
  false)


(defn- default-block-next-block [node line pos grammar functions]
  (peg/match grammar line pos))


(defn- default-block-see-blank [node functions]
  nil)


(defn- blockquote-see-blank [a-blockquote functions]
  (when (attribute a-blockquote :open?)
    (attribute a-blockquote :open? false)
    (close-children a-blockquote functions)))


(defn- codeblock-blank [a-codeblock parent functions]
  (when (attribute a-codeblock :open?)
    (array/push (children-of a-codeblock) "\n"))
  nil)


(defn- codeblock-close [a-codeblock &opt parent]
  (attribute a-codeblock :open? false)
  (when (= :indented (attribute a-codeblock :kind))
    (def lines (children-of a-codeblock))
    (def last-index (dec (length lines)))
    (var start 0)
    (var line (get lines start))
    (while (string/check-set " \t\n" line)
      (set line (get lines (++ start))))
    (var end last-index)
    (set line (get lines end))
    (while (string/check-set " \t\n" line)
      (set line (get lines (-- end))))
    (unless (and (= 0 start) (= last-index end))
      (array/remove lines (inc end) (- last-index end))
      (array/remove lines 0 start))))


(defn- codeblock-continue [a-codeblock block]
  (def lines (children-of block))
  (if (= :close (first lines))
    (codeblock-close a-codeblock)
    (array/concat (children-of a-codeblock) lines)))


(defn- codeblock-equal? [a-codeblock block]
  (and (= :codeblock (type-of block))
       (attribute a-codeblock :open?)))


(defn- codeblock-follower [a-codeblock block]
  (when (= :paragraph (type-of block))
    [:paragraph {} (children-of a-codeblock)]))


(defn- codeblock-lazy? [a-codeblock]
  (= :indented (attribute a-codeblock :kind)))


(defn- codeblock-needs-nl? [a-codeblock]
  (= :indented (attribute a-codeblock :kind)))


(defn- codeblock-next-block [a-codeblock line pos grammar functions]
  (if (= :indented (attribute a-codeblock :kind))
    (peg/match grammar line pos)
    (do
      (def delim (attribute a-codeblock :delim))
      (def delim-num (attribute a-codeblock :num))
      (def max-indent (attribute a-codeblock :indent))
      (defn fence []
        (when (< (- col-edge col-pos) 4)
          [:codeblock {} [:close]]))
      (defn code [text]
        (def extra-cols (- col-edge col-pos max-indent))
        (def extra-space (when (> extra-cols 0) (string/repeat " " extra-cols)))
        (def line (string extra-space text))
        [:codeblock {} @[line]])
      (def fence-grammar
        ~{:main    (* :padding (+ :fence :code) :eol ($))
          :eol     -1
          :ws      (set " \t\n")
          :padding (drop (/ '(any (set " \t")) ,record-padding))
          :fence   (cmt (* (at-least ,delim-num ,delim) (any :ws) (> 0 :eol)) ,fence)
          :code    (/ '(thru :eol) ,code)})
      (peg/match fence-grammar line pos))))


(defn- codeblock-see-blank [a-codeblock functions]
  (attribute a-codeblock :open?))


(defn- heading-equal? [a-heading block]
  (= :paragraph (type-of block)))


(defn- heading-lazy? [a-heading]
  (= :setext (attribute a-heading :kind)))


(defn- heading-follower [a-heading block]
  (when (= :paragraph (type-of block))
    a-heading))


(defn- heading-replace [a-heading siblings]
  (case (attribute a-heading :kind)
    :atx
    (array/push siblings a-heading)

    :setext
    (if (= :paragraph (type-of (array/peek siblings)))
      (do
        (def last-p (array/pop siblings))
        (def children (get a-heading 2))
        (array/clear children)
        (array/concat children (get last-p 2))
        (array/push siblings a-heading))
      (do
        (def text (first (get a-heading 2))) # TODO Can we assume a heading only has one element?
        (if (attribute (array/peek siblings) :open?)
          (do
            (def parent (last-descendant (array/peek siblings)))
            (array/push (get parent 2) text))
          (array/push siblings (paragraph text)))))))


(defn- html-close [an-html &opt parent functions]
  (attribute an-html :open? false)
  (def lines (children-of an-html))
  (def last-line (last lines))
  (def last-index (dec (length last-line)))
  (var i last-index)
  (while (> i 0)
    (def c (get last-line i))
    (unless (= 10 c)
      (break))
    (-- i))
  (unless (= last-index i)
    (array/pop lines)
    (array/push lines (string/slice last-line 0 (inc i)))))


(defn- html-continue [an-html block]
  (def lines (children-of block))
  (if (= :close (first lines))
    (do
      (unless (one? (length lines))
        (array/push (children-of an-html) (get lines 1)))
      (html-close an-html))
    (array/concat (children-of an-html) lines)))


(defn- html-needs-nl? [an-html]
  (= 7 (attribute an-html :kind)))


(defn- html-next-block [an-html line pos grammar functions]
  (defn close [&opt text]
    (if (nil? text)
      [:html {} [:close]]
      [:html {} [:close text]]))
  (defn code [text]
    [:html {:kind (attribute an-html :kind)} [text]])
  (def html-grammar
    ~{:main   (* (+ :close :code) ($))
      :eol    -1
      :space  (set " \t\n")
      :close  (/ ,(case (attribute an-html :kind)
                    1 ~(<- (* (thru (* "</" ,(words "script" "style" "pre") ">")) (thru :eol)))
                    2 ~(<- (* (thru "-->") (thru :eol)))
                    3 ~(<- (* (thru "?>") (thru :eol)))
                    4 ~(<- (* (thru ">") (thru :eol)))
                    5 ~(<- (* (thru "]]>") (thru :eol)))
                    6 ~(* (any :space) :eol)
                    7 ~(* (any :space) :eol)) ,close)
      :code   (/ '(thru :eol) ,code)})
  (peg/match html-grammar line pos (attribute an-html :kind)))


(defn- html-see-blank [an-html functions]
  true)


(defn- linkdef-close [a-def &opt parent functions]
  (defn register-link [ref-text dest &opt title]
    (def ref (normalise ref-text))
    (unless (get links ref)
      (put links ref {:url (uri-encode dest) :title title})))

  (def link-grammar
    ~{:main (* (/ (* :label ":" :gap :dest (+ (* (> 0 (+ :space :nl)) :gap :title (any :space) :eol) (* (any :space) :eol))) ,register-link) ($))

      :eol   (+ :nl -1)
      :nl    "\n"
      :space (set " \t")
      :blank (* :nl (any :space) :nl) # check whether parens balanced
      :gap   (* (any :space) (? :nl) (any :space))

      :escaped (+ (* "\\" '(set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")) '(* "\\" 1))
      :entity  (/ (+ (* (constant :ent) '(* "&" (some (+ :w :d)) ";"))
                     (* (constant :dec) '(* "&#" (between 1 7 :d) ";"))
                     (* (constant :hex) '(* "&#" (set "Xx") (between 1 6 :h) ";")))
                  ,entity-decode)
      :char    (+ :escaped :entity '1)

      :label (* "[" :gap '(some (+ (* "\\" 1) (if-not (+ (set "\\[]") :blank) 1))) :gap "]")
      :dest  (+ (* "<" (% (any (if-not (set "<>\n") :char))) ">")
                (* (not "<") (% (some (if-not (+ (range "\x00\x20") "\x7F") :char)))))
      :title (+ (* `"` (% (any (if-not (+ (set "\"") :blank) :char))) `"`)
                (* "'" (% (any (if-not (+ (set "'") :blank) :char))) "'")
                (* "(" (% (any (if-not (+ (set "()") :blank) :char))) ")"))})

  (when (attribute a-def :open?)
    (attribute a-def :open? false)
    (def all-text (-> (children-of a-def) (string/join "\n") string/trim))
    (var i 0)
    (while (< i (length all-text)) # a linkdef 'block' can contain multiple definitions as well as a follow-on paragraph
      (if (def res (peg/match link-grammar all-text i))
        (set i (get res 1))
        (break)))
    (array/pop (children-of parent))
    (when (< i (length all-text))
      (def content (string/slice all-text i))
      (array/push (children-of parent) [:paragraph @{:indent (attribute a-def :indent) :open? false :inlines? true} @[content]])))
  nil)


(defn- linkdef-lazy? [a-def]
  true)


(defn- linkdef-equal? [a-def block]
  (or (= :paragraph (type-of block))
      (= :linkdef (type-of block))))


(defn- linkdef-needs-nl? [a-def]
  true)


(defn- linkdef-next-block [a-def line pos grammar functions]
  (def result (peg/match grammar line pos))
  (def block (get result 0))
  (def needs-nl-fn (get-fn :needs-nl? block functions))
  (if (needs-nl-fn block)
    [(to-continuation :paragraph line pos) (length line)]
    result))


(defn- list-blank [a-list parent functions]
  (def item (next-container a-list))
  (when (and (not (nil? item))
             (attribute item :starts-blank?)
             (zero? (length (children-of item))))
    (attribute item :open? false))
  (attribute a-list :has-blank? true)
  (next-container a-list))


(defn- list-equal? [a-list block]
  (and (= :list (type-of block))
       (= (attribute a-list :kind) (attribute block :kind))
       (or (and (= :ordinal (attribute a-list :kind))
                (= (attribute a-list :delim) (attribute block :delim)))
           (and (= :bullet (attribute a-list :kind))
                (= (attribute a-list :marker) (attribute block :marker))))))


(defn- list-needs-nl? [a-list]
  (or (attribute (next-container a-list) :starts-blank?)
      (and (= :ordinal (attribute a-list :kind))
           (not= 1 (attribute a-list :start)))))


# The current open block is a list so we need to handle the case where the
# line is a continuation of the list. To check this we need to descend through
# the open list items in the current list that are indented at least as much as
# the current line.
(defn- list-next-block [a-list line pos grammar functions]
  (def next-pos (dedent line pos))
  (var next-b nil)
  (var parent-list nil)
  (var parent-item nil)
  (var curr-list a-list)
  (var curr-item (next-container a-list))
  (while curr-item
    # Set start col of list item
    (def curr-width (attribute curr-item :width))

    # Break if there's not enough padding
    (def remaining-width (- col-edge col-pos))
    (if (> curr-width remaining-width)
      (break)
      (+= col-pos curr-width))

    # The parent list is at least equal to the current list
    (when parent-list
      (attribute parent-list :has-blank? false))
    (set parent-list curr-list)

    # Create continuation
    (def new-item [:list-item-continue (get curr-item 1) @[]])
    (def new-list [:list (get curr-list 1) [new-item]])

    # Set next-b and parent
    (if (nil? next-b)
      (set next-b new-list)
      (array/push (children-of parent-item) new-list))
    (set parent-item new-item)

    # Break if no more lists
    (def child (next-container curr-item))
    (when (or (nil? child) (not= :list (type-of child)))
      (break))

    # Prepare for next round of loop
    (set curr-list child)
    (set curr-item (next-container curr-list)))

  # Parse line from current position
  (def result (peg/match grammar line next-pos))

  # Make parent list loose if a blank line has come before
  (cond
    (and (nil? parent-list)
         (attribute a-list :has-blank?)
         (list-equal? a-list (get result 0)))
    (attribute a-list :tight? false)

    (and parent-list
         (attribute parent-list :has-blank?))
    (attribute parent-list :tight? false))

  # Return result
  (if (nil? next-b)
    result
    [next-b next-pos]))


(defn- list-item-equal? [an-item block]
  (= :list-item-continue (type-of block)))


(defn- paragraph-append [a-paragraph continuation functions]
  (array/concat (children-of a-paragraph) (children-of continuation)))


(defn- paragraph-blank [a-paragraph parent functions]
  (attribute a-paragraph :open? false)
  nil)


(defn- paragraph-equal? [a-paragraph block]
  (or (= :paragraph (type-of block))
      (and (= :heading (type-of block))
           (= :setext (attribute block :kind)))))


(defn- paragraph-follower [a-paragraph block]
  (when (= :paragraph (type-of block))
    a-paragraph))


(defn- paragraph-lazy? [a-paragraph]
  true)


(defn- paragraph-next-block [a-paragraph line pos grammar functions]
  (def result (peg/match grammar line pos))
  (def block (get result 0))

  (defn heading? []
    (and (= :thematic-break (type-of block))
         (= 45 (attribute block :char))
         (peg/match ~(* (any " ") (some "-") (any " ") -1) (first (children-of block)))))

  (cond
    (heading?)
    [[:heading @{:level 2 :open? false :inlines? true :kind :setext} @["-"]] (length line)]

    ((get-fn :needs-nl? block functions) block)
    [(to-continuation :paragraph line pos) (length line)]

    result))


## Block rules

(add-to rules
  {:blocks
    {:blockquote {:see-blank   blockquote-see-blank}
     :codeblock  {:blank       codeblock-blank
                  :close       codeblock-close
                  :continue    codeblock-continue
                  :equal?      codeblock-equal?
                  :follower    codeblock-follower
                  :lazy?       codeblock-lazy?
                  :needs-nl?   codeblock-needs-nl?
                  :next-block  codeblock-next-block
                  :see-blank   codeblock-see-blank}
     :heading    {:equal?      heading-equal?
                  :follower    heading-follower
                  :lazy?       heading-lazy?
                  :replace     heading-replace}
     :html       {:blank       html-close
                  :close       html-close
                  :continue    html-continue
                  :needs-nl?   html-needs-nl?
                  :next-block  html-next-block
                  :see-blank   html-see-blank}
     :linkdef    {:blank       linkdef-close
                  :close       linkdef-close
                  :equal?      linkdef-equal?
                  :lazy?       linkdef-lazy?
                  :needs-nl?   linkdef-needs-nl?
                  :next-block  linkdef-next-block}
     :list       {:blank       list-blank
                  :equal?      list-equal?
                  :needs-nl?   list-needs-nl?
                  :next-block  list-next-block}
     :list-item  {:equal?      list-item-equal?}
     :paragraph  {:append      paragraph-append
                  :blank       paragraph-blank
                  :equal?      paragraph-equal?
                  :follower    paragraph-follower
                  :lazy?       paragraph-lazy?
                  :next-block  paragraph-next-block}
     'default    {:append      default-block-append
                  :blank       default-block-blank
                  :close       default-block-close
                  :continue    default-block-continue
                  :equal?      default-block-equal?
                  :follower    default-block-follower
                  :lazy?       default-block-lazy?
                  :needs-nl?   default-block-needs-nl?
                  :next-block  default-block-next-block
                  :see-blank   default-block-see-blank}}})


