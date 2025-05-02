(use ../globals)
(use ../utilities)

## Grammar

(defn- html [kind & args]
  (def indent (- col-edge col-pos))
  (def [close? text] (if (one? (length args)) [nil (first args)] args))
  (def line (string (when (> indent 0) (string/repeat " " indent))
                    (if close? (string/slice text 0 -2) text)))
  [:html @{:open? (if close? false true) :kind kind} @[line]])


(def grammar
  ~@{:html {:main   (/ (+ :type-1 :type-2 :type-3 :type-4 :type-5 :type-6 :type-7) ,html)
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
                     :name  (* (not ,(words "script" "style" "pre")) :w (any (+ :w :d "-")))}}})


## Functions

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


(add-to rules
  @{:blocks
    @{:html {:blank       html-close
             :close       html-close
             :continue    html-continue
             :needs-nl?   html-needs-nl?
             :next-block  html-next-block
             :see-blank   html-see-blank}}})
