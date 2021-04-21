(use ../globals)
(use ../utilities)


## Grammar

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


(def grammar
  ~{:link {:main  (cmt (+ :open :close) ,link)
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
                              (* "(" (% (any (if-not (+ (set "()") :blank) :char))) ")"))}}})


## Functions

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
    {:image    {:close    image-close}
     :link     {:close    link-close
                :match?   link-match?
                :match-up link-match-up}}})


# TODO Simplify this
(add-to priorities
  {:levels [1 0]
   :kinds  {:emphasis 0
            :link     1}})
