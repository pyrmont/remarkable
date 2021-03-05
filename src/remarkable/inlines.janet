(use ./globals)
(use ./utilities)


(defn- codespan [delim content]
  (def buf @"")
  (var only-spaces? true)
  (each c content
    (case c
      10 (buffer/push buf " ")
      32 (buffer/push buf c)
         (do
           (buffer/push buf c)
           (set only-spaces? false))))
  (def [start end] (if (and (= 32 (first buf))
                            (= 32 (last buf))
                            (not only-spaces?))
                     [1 (dec (length buf))]
                     [0 nil]))
  [:codespan @{} (string/slice buf start end)])


(defn- autolink [scheme uri &opt email?]
  (def url (string scheme uri))
  [:link @{:url (uri-encode url)} @[(if email? uri url)]])


(defn- rawhtml [content]
  [:rawhtml @{} content])


(defn- hardbreak []
  [:hardbreak])


(defn- emphasis [start-pos run flank &opt pre-punc? post-punc?]
  (def delim (case (get run 0) 42 "*" 95 "_"))
  (def num (length run))
  (def left? (case flank :left true :left-and-right true))
  (def right? (case flank :right true :left-and-right true))
  (def end-pos (+ num start-pos))
  (array/push delimiters
    [:delims @{:kind :emphasis :delim delim :count num :left? left? :right? right? :pre-punc? pre-punc? :post-punc? post-punc? :start-pos start-pos :end-pos end-pos} @[]])
  (array/peek delimiters))


(defn- link-inline [url &opt title]
  {:url (uri-encode url) :title title})


(defn- link [start-pos delim flank &opt meta end-pos]
  (default end-pos (+ (length delim) start-pos))
  (def image? (when (= 2 (length delim)) true))
  (def left? (when (= :left flank) true))
  (def right? (when (= :right flank) true))
  (array/push delimiters
    [:delims @{:kind :link :delim delim :count 1 :left? left? :right? right? :image? image? :start-pos start-pos :end-pos end-pos :meta meta} @[]])
  (array/peek delimiters))


(defn- fragment [content]
  [:fragment content])


(def grammar
  ~{:main :elements

    :alpha (range "AZ" "az")
    :alnum (+ :alpha (range "09"))
    :ws    (set " \n\r\v\t")
    :nl    "\n"
    :blank (* "\n" (any :space) "\n")
    :space (set " \t")
    :punc  (set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")

    :elements (any (+ :inline :delim :text))

    :inline (+ :codespan :rawhtml :autolink :hardbreak)

    :codespan {:main  (+ (unref (/ (* :open '(to :close) :close) ,codespan))
                         (/ '(some "`") ,fragment))
               :open  (<- (some "`") :delim)
               :close (* (! (> -1 "`")) (backmatch :delim) (not "`"))}

    :autolink {:main   (/ (* "<" (+ :mail :other) ">") ,autolink)
               :mail   (* (constant "mailto:") :email (constant true))
               :email  '(* (some (+ :w :d (set ".!#$%&'*+/=?^_`{|}~-")))
                           "@"
                           (* :alnum (at-most 61 (+ :w :d "-"))
                           (any (* "." :alnum (at-most 61 (+ :w :d "-"))))))
               :other  (* :scheme :uri)
               :scheme '(* (+ ,(words "mailto")
                              (* :w (some (+ :w :d (set "+.-"))))) ":")
               :uri    '(any (if-not (+ (set "<>") (range "\x00\x20") "\x7F") 1))}

    :rawhtml {:main        (/ '(+ :opening-tag :closing-tag :comment :instruction :declaration :cdata) ,rawhtml)
              :name        (* :alpha (any (+ :alnum "-")))
              :attribute   {:main    (* :ws :name (? :spec))
                            :name    (* (+ :alpha (set "_:")) (any (+ :alnum (set "_.:-"))))
                            :spec    (* (? :ws) "=" (? :ws) (+ :unquote :single :double))
                            :unquote (some (if-not (+ :ws (set "\"'=<>`")) 1))
                            :single  (* "'" (to "'") "'")
                            :double  (* `"` (to `"`) `"`)}
              :opening-tag (* "<" :name (any :attribute) (any :ws) (? "/") ">")
              :closing-tag (* "</" :name (any :ws) ">")
              :comment     (* "<!--" (not (+ ">" "->")) (thru "--") ">")
              :instruction (* "<?" (thru "?>"))
              :declaration (* "<!" (some (range "AZ")) (some :ws) (some (if-not ">" 1)) ">")
              :cdata       (* "<![CDATA[" (thru "]]>"))}

    :hardbreak (/ (+ "\\\n" (* (at-least 2 :space) "\n")) ,hardbreak)

    :delim (+ :emphasis :link)

    :emphasis {:main   (+ (/ (* ($) :runs) ,emphasis) (/ :run ,fragment))
               :uws    (+ :ws # include other Unicode whitespace
                          "\u00A0" "\u1680" "\u2000" "\u2001" "\u2002" "\u2003"
                          "\u2004" "\u2005" "\u2006" "\u2007" "\u2008" "\u2009"
                          "\u200A" "\u202F" "\u205F" "\u3000")
               :runs   (+ (* (! :l-wsp) :run (! :t-wsp) (constant :left-and-right))
                          (* :l-punc    :run :t-punc    (constant :left-and-right) (constant true) (constant true))
                          (*            :run (! :t-wsp) (constant :left))
                          (* :l-wsp     :run :t-punc    (constant :left) (constant nil) (constant true))
                          (* (! :l-wsp) :run            (constant :right))
                          (* :l-punc    :run :t-wsp     (constant :right) (constant true)))
               :run    '(+ (some "*") (some "_"))
               :l-wsp  (+ :l-uws :l-punc)
               :t-wsp  (+ :t-uws :t-punc)
               :l-uws  (+ (> -1 :uws) (! (> -1 1)))
               :t-uws  (+ (> 0 :uws) (> 0 -1))
               :l-punc (> -1 :punc)
               :t-punc (> 0 :punc)}

    :link {:main  (cmt (+ :open :close) ,link)
           :open  (* ($) '(* (? "!") "[") (constant :left))
           :close {:main   (* ($) '(* "]" (? "[]")) (constant :right) (+ :inline (constant nil)) ($))
                   :gap    (* (any :space) (? :nl) (any :space))
                   :char   (+ :escaped :entity '1)
                   :parens (* '"(" (% (any (+ (if-not (set "()") '1) :parens))) '")")
                   :inline (/ (* "(" :gap (* :dest (? (* :gap :title))) :gap ")") ,link-inline)
                   :dest   (+ (* "<" (% (any (if-not (set "<>\n") :char))) ">")
                              (* (not "<") (% (any (if-not (+ (range "\x00\x20") "\x7F" ")") (+ :parens :char))))))
                   :title  (+ (* `"` (% (any (if-not (+ (set "\"") :blank) :char))) `"`)
                              (* "'" (% (any (if-not (+ (set "'") :blank) :char))) "'")
                              (* "(" (% (any (if-not (+ (set "()") :blank) :char))) ")"))}}

    :text (/ (+ (% (some (+ :escaped :entity :trail :char))) '1) ,fragment)

    :escaped (+ (* "\\" ':punc)
                '(* "\\" (if-not :nl 1)))
    :trail   (* :space ':nl)
    :char    (if-not (+ (set "&<>*_[]!`\\") (* (at-least 2 :space) "\n")) '1)
    :entity  (/ (+ (* (constant :ent) '(* "&" (some (+ :w :d)) ";"))
                   (* (constant :dec) '(* "&#" (between 1 7 :d) ";"))
                   (* (constant :hex) '(* "&#" (set "Xx") (between 1 6 :h) ";")))
                ,entity-decode)})


## Inline functions

(defn- default-inline-close [node delim ancestors]
  (array/pop ancestors))


(defn- emphasis-match? [open-i close-i delimiters]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (and (and (= (attribute opener :kind) (attribute closer :kind))
            (= (attribute opener :delim) (attribute closer :delim))
            (not (attribute opener :skip?))
            (not= 0 (attribute opener :count))
            (not= 0 (attribute closer :count)))
       (case (attribute closer :delim)
         "*"
         (do
           (and # rules 1 and 5
                (attribute opener :left?)
                # rules 3 and 7
                (attribute closer :right?)
                # rules 9 and 10
                (or (and (not (attribute opener :right?))
                         (not (attribute closer :left?)))
                    (or (not= 0 (% (+ (attribute opener :count) (attribute closer :count)) 3))
                        (and (= 0 (% (attribute opener :count) 3))
                             (= 0 (% (attribute closer :count) 3)))))))
         "_"
         (do
           (and # rules 2 and 6
                (and (attribute opener :left?)
                     (or (not (attribute opener :right?))
                         (attribute opener :pre-punc?)))
                # rules 4 and 8
                (and (attribute closer :right?)
                     (or (not (attribute closer :left?))
                         (attribute closer :post-punc?)))
                # rules 9 and 10
                (or (and (not (attribute opener :right?))
                         (not (attribute closer :left?)))
                    (or (not= 0 (% (+ (attribute opener :count) (attribute closer :count)) 3))
                        (and (= 0 (% (attribute opener :count) 3))
                             (= 0 (% (attribute closer :count) 3))))))))))


(defn- emphasis-match-up [open-i close-i delimiters text]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (def [kind len] (if (and (>= (attribute opener :count) 2)
                           (>= (attribute closer :count) 2))
                    [:strong 2] [:emphasis 1]))
  (array/insert (children-of opener) 0 [:open @{:kind kind} @[]])
  (attribute opener :count (- (attribute opener :count) len))
  (attribute opener :end-pos (- (attribute opener :end-pos) len))
  (array/push (children-of closer) [:close @{:kind kind} @[]])
  (attribute closer :count (- (attribute closer :count) len))
  (attribute closer :start-pos (+ (attribute closer :start-pos) len)))


(defn- image-close [node delim ancestors]
  (def meta (attribute delim :meta))
  (merge-into (get node 1) meta)
  (array/pop ancestors))


(defn- link-close [node delim ancestors]
  (if (attribute delim :drop?)
    (array/pop (children-of (array/peek ancestors)))
    (merge-into (get node 1) (attribute delim :meta)))
  (array/pop ancestors))


(defn- link-match? [open-i close-i delimiters]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (and (attribute opener :left?)
       (not (attribute opener :skip?))
       (not (zero? (attribute opener :count)))
       (not (zero? (attribute closer :count)))))


(defn- link-meta-from-label [open-i close-i delimiters text]
  (defn extract-ref [opener closer]
    (-> text
        (string/slice (attribute opener :end-pos) (attribute closer :start-pos))
        normalise))
  (var result nil)
  (def closer (get delimiters close-i))
  (def label-opener (get delimiters (+ 1 close-i)))
  (def label-closer (get delimiters (+ 2 close-i)))
  (if (and label-opener
           label-closer
           (= :link (attribute label-opener :kind) (attribute label-closer :kind))
           (= (attribute closer :end-pos) (attribute label-opener :start-pos)))
    (do
      (set result (get links (extract-ref label-opener label-closer)))
      (when result
        (array/push (children-of label-opener) [:open {:kind :link} []])
        (array/push (children-of label-closer) [:close {:drop? true} []])
        (attribute label-opener :count 0)
        (attribute label-closer :count 0)))
    (do
      (def opener (get delimiters open-i))
      (set result (get links (extract-ref opener closer)))))
  result)


(defn- link-match-up [open-i close-i delimiters text]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (unless (or (attribute opener :inactive?)
              (attribute closer :meta))
    (attribute closer :meta (link-meta-from-label open-i close-i delimiters text)))
  (if (or (attribute opener :inactive?)
          (nil? (attribute closer :meta)))
    (do
      (attribute opener :skip? true)
      (attribute closer :skip? true))
    (do
      (def kind (if (attribute opener :image?) :image :link))
      (array/push (children-of opener) [:open @{:kind kind} @[]])
      (attribute opener :count 0)
      (array/push (children-of closer) [:close @{:kind kind :meta (attribute closer :meta)} @[]])
      (attribute closer :count 0)
      (unless (attribute opener :image?)
        (var i open-i)
        (while (def prev-opener (get delimiters (-- i)))
          (when (and (attribute prev-opener :left?)
                     (= :link (attribute prev-opener :kind))
                     (not (attribute prev-opener :image?)))
            (attribute prev-opener :inactive? true)))))))


(add-to rules
  {:inlines
    {:emphasis {:match?   emphasis-match?
                :match-up emphasis-match-up}
     :image    {:close    image-close}
     :link     {:close    link-close
                :match?   link-match?
                :match-up link-match-up}
     'default  {:close    default-inline-close}}})


# TODO Simplify this
(add-to priorities
  {:levels [1 0]
   :kinds  {:emphasis 0
            :link     1}})
