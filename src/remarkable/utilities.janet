(use ./globals)


(import ./entities)
(import ./folding)


(defn add-to [d1 d2]
  (each k (keys d2)
    (if (nil? (get d1 k))
      (put d1 k (get d2 k))
      (each name (keys (get d2 k))
        (put-in d1 [k name] (get-in d2 [k name])))))
  d1)


(defn attribute [node attr &opt value]
  (if (nil? value)
    (get (get node 1) attr)
    (put (get node 1) attr value)))


(defn type-of [node]
  (first node))


(defn children-of [node]
  (last node))


(defn get-fn [name node functions]
  (or (get (get functions (type-of node)) name)
      (get (get functions 'default) name)))


(defn next-child [node]
  (when (attribute node :container?)
    (last (children-of node))))


(defn next-container [node]
  (def child (next-child node))
  (when (and child (attribute child :open?))
    child))


(defn close-children [parent functions]
  (var prev parent)
  (var node (next-container prev))
  (while node
    (def close-fn (get-fn :close node functions))
    (close-fn node prev)
    (set prev node)
    (set node (next-container prev))))


(defn last-descendant [start-n]
  (var next-next-n (next-child start-n))
  (var next-n next-next-n)
  (while next-next-n
    (set next-n next-next-n)
    (set next-next-n (next-child next-next-n)))
  next-n)


(defn num-cols [c &opt extra-width]
  (default extra-width 0)
  (case c
     9 (- 4 (% (+ col-edge extra-width) 4))
    32 1))


(defn record-padding [padding]
  (each c padding
    (+= col-edge (num-cols c))))


(defn dedent [line start]
  (def len (length line))
  (var pos start)
  (var total-width 0)
  (while (< pos len)
    (def width (num-cols (get line pos) total-width))
    (if (nil? width)
      (break))
    (+= total-width width)
    (++ pos))
  (+= col-edge total-width)
  pos)


(defn to-fragment [content]
  [:fragment content])


(defn to-continuation [block-type line pos]
  (def content (-> (string/slice line pos) string/trim))
  [block-type @{} @[content]])


(defn uri-encode [uri]
  (def hex ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"])

  (defn encode-char [ch]
    (def c (get ch 0))
    (string "%" (in hex (math/floor (/ c 16))) (in hex (% c 16)))) #TODO Is there a more efficient way to do this?

  (def encode-grammar
    ~{:main      (* (% (any (+ :escaped :unescaped))) -1)
      :escaped   (+ '"%"
                    (/ '"&"  "&amp;")
                    (/ '(+ (set "<>\"|^`[]{}\\")
                           (range "\x00\x20")
                           (range "\x7F\xFF")) ,encode-char))
      :unescaped '1})
  (first (peg/match encode-grammar uri)))


(defn entity-decode [kind entity]
  (case kind
    :ent (or (entities/to-grapheme entity) entity)

    :dec (let [code (scan-number (string/slice entity 2 -2))]
           (if (zero? code)
             "\uFFFD"
             (parse (string/format `"\U%06x"` code))))

    :hex (let [code (string/slice entity 3 -2)
               len  (length code)]
           (if (string/check-set "0" code)
             "\uFFFD"
             (parse (string `"\U` (when (< len 6) (string/repeat "0" (- 6 len))) code `"`))))))


(defn normalise [s]
  (def buf @"")
  (var found-space? false)
  (each c s
    (if (or (= 9 c) (= 10 c) (= 32 c))
      (set found-space? true)
      (do
        (buffer/push buf (if found-space? " " "") c)
        (set found-space? false))))
  (folding/case-fold buf))


(defn update-col-pos [block]
  (when (def width (attribute block :width))
    (+= col-pos width))
  block)


(defn words [& xs]
  (defn word [s]
    (tuple
      ;(reduce (fn [arr c]
                 (->> ~(set ,(string/from-bytes c (- c 32)))
                      (array/push arr)))
               @['*]
               s)))
  ['+ ;(map word xs)])
