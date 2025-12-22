(import ../state)
(import ../util)
(import ../node)

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
  (def meta (node/attribute delim :meta))
  (merge-into (get node 1) meta)
  (array/pop ancestors))

(defn- link-close [node delim ancestors]
  (if (node/attribute delim :drop?)
    (array/pop (node/children-of (array/peek ancestors)))
    (merge-into (get node 1) (node/attribute delim :meta)))
  (array/pop ancestors))

(defn- link-match? [open-i close-i delimiters]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (and (node/attribute opener :left?)
       (not (node/attribute opener :skip?))
       (not (zero? (node/attribute opener :count)))
       (not (zero? (node/attribute closer :count)))))

(defn- link-meta-from-label [open-i close-i delimiters text]
  (defn extract-ref [opener closer]
    (-> text
        (string/slice (node/attribute opener :end-pos) (node/attribute closer :start-pos))
        util/normalise))
  (var result nil)
  (def closer (get delimiters close-i))
  (def label-opener (get delimiters (+ 1 close-i)))
  (def label-closer (get delimiters (+ 2 close-i)))
  (if (and label-opener
           label-closer
           (= :link (node/attribute label-opener :kind) (node/attribute label-closer :kind))
           (= (node/attribute closer :end-pos) (node/attribute label-opener :start-pos)))
    (do
      (set result (get state/links (extract-ref label-opener label-closer)))
      (when result
        (array/push (node/children-of label-opener) [:open {:kind :link} []])
        (array/push (node/children-of label-closer) [:close {:drop? true} []])
        (node/attribute label-opener :count 0)
        (node/attribute label-closer :count 0)))
    (do
      (def opener (get delimiters open-i))
      (set result (get state/links (extract-ref opener closer)))))
  result)

(defn- link-match-up [open-i close-i delimiters text]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (unless (or (node/attribute opener :inactive?)
              (node/attribute closer :meta))
    (node/attribute closer :meta (link-meta-from-label open-i close-i delimiters text)))
  (if (or (node/attribute opener :inactive?)
          (nil? (node/attribute closer :meta)))
    (do
      (node/attribute opener :skip? true)
      (node/attribute closer :skip? true))
    (do
      (def kind (if (node/attribute opener :image?) :image :link))
      (array/push (node/children-of opener) [:open @{:kind kind} @[]])
      (node/attribute opener :count 0)
      (array/push (node/children-of closer) [:close @{:kind kind :meta (node/attribute closer :meta)} @[]])
      (node/attribute closer :count 0)
      (unless (node/attribute opener :image?)
        (var i open-i)
        (while (def prev-opener (get delimiters (-- i)))
          (when (and (node/attribute prev-opener :left?)
                     (= :link (node/attribute prev-opener :kind))
                     (not (node/attribute prev-opener :image?)))
            (node/attribute prev-opener :inactive? true)))))))

(util/add-to state/protocols
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
