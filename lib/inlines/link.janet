(import ../state)
(import ../util)

## Grammar

(defn- link-inline [url &opt title]
  {:url (util/uri-encode url) :title title})

(defn- link [start-pos delim flank &opt meta end-pos]
  (default end-pos (+ (length delim) start-pos))
  (def image? (when (= 2 (length delim)) true))
  (def left? (when (= :left flank) true))
  (def right? (when (= :right flank) true))
  (array/push state/delimiters
    [:delims @{:kind :link :delim delim :count 1 :left? left? :right? right? :image? image? :start-pos start-pos :end-pos end-pos :meta meta} @[]])
  (array/peek state/delimiters))

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
  (def meta (util/attribute delim :meta))
  (merge-into (get node 1) meta)
  (array/pop ancestors))

(defn- link-close [node delim ancestors]
  (if (util/attribute delim :drop?)
    (array/pop (util/children-of (array/peek ancestors)))
    (merge-into (get node 1) (util/attribute delim :meta)))
  (array/pop ancestors))

(defn- link-match? [open-i close-i delimiters]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (and (util/attribute opener :left?)
       (not (util/attribute opener :skip?))
       (not (zero? (util/attribute opener :count)))
       (not (zero? (util/attribute closer :count)))))

(defn- link-meta-from-label [open-i close-i delimiters text]
  (defn extract-ref [opener closer]
    (-> text
        (string/slice (util/attribute opener :end-pos) (util/attribute closer :start-pos))
        util/normalise))
  (var result nil)
  (def closer (get delimiters close-i))
  (def label-opener (get delimiters (+ 1 close-i)))
  (def label-closer (get delimiters (+ 2 close-i)))
  (if (and label-opener
           label-closer
           (= :link (util/attribute label-opener :kind) (util/attribute label-closer :kind))
           (= (util/attribute closer :end-pos) (util/attribute label-opener :start-pos)))
    (do
      (set result (get state/links (extract-ref label-opener label-closer)))
      (when result
        (array/push (util/children-of label-opener) [:open {:kind :link} []])
        (array/push (util/children-of label-closer) [:close {:drop? true} []])
        (util/attribute label-opener :count 0)
        (util/attribute label-closer :count 0)))
    (do
      (def opener (get delimiters open-i))
      (set result (get state/links (extract-ref opener closer)))))
  result)

(defn- link-match-up [open-i close-i delimiters text]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (unless (or (util/attribute opener :inactive?)
              (util/attribute closer :meta))
    (util/attribute closer :meta (link-meta-from-label open-i close-i delimiters text)))
  (if (or (util/attribute opener :inactive?)
          (nil? (util/attribute closer :meta)))
    (do
      (util/attribute opener :skip? true)
      (util/attribute closer :skip? true))
    (do
      (def kind (if (util/attribute opener :image?) :image :link))
      (array/push (util/children-of opener) [:open @{:kind kind} @[]])
      (util/attribute opener :count 0)
      (array/push (util/children-of closer) [:close @{:kind kind :meta (util/attribute closer :meta)} @[]])
      (util/attribute closer :count 0)
      (unless (util/attribute opener :image?)
        (var i open-i)
        (while (def prev-opener (get delimiters (-- i)))
          (when (and (util/attribute prev-opener :left?)
                     (= :link (util/attribute prev-opener :kind))
                     (not (util/attribute prev-opener :image?)))
            (util/attribute prev-opener :inactive? true)))))))

(util/add-to state/rules
  {:inlines
    {:image    {:close    image-close}
     :link     {:close    link-close
                :match?   link-match?
                :match-up link-match-up}}})

# TODO Simplify this

(util/add-to state/priorities
  {:levels [1 0]
   :kinds  {:emphasis 0
            :link     1}})
