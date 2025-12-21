(import ./entities)
(import ./folding)
(import ./state)

# Private functions

(defn- num-cols [c &opt extra-width]
  (default extra-width 0)
  (case c
     9 (- 4 (% (+ state/col-edge extra-width) 4))
    32 1))

# State functions

(defn record-padding [padding]
  (each c padding
    (+= state/col-edge (num-cols c))))

# Selection functions

(defn type-of
  ```
  Gets the type of the node
  ```
  [node]
  (first node))

(defn attribute
  ```
  Gets or sets the attribute `attr` of `node`

  Whether this function gets or sets depends on whether `value` is provided.
  ```
  [node attr &opt value]
  (if (nil? value)
    (get (get node 1) attr)
    (put (get node 1) attr value)))

(defn get-fn
  ```
  Gets a function called `name` associated with the type of `node`

  If there is no function called `name` associated with the type, instead gets
  the function called `name` from the default group.
  ```
  [name node functions]
  (or (get (get functions (type-of node)) name)
      (get (get functions 'default) name)))

(defn update-col-pos
  ```
  Updates the column position in the global state

  The column position is updated based on the width of `block`. Returns `block`.
  ```
  [block]
  (when (def width (attribute block :width))
    (+= state/col-pos width))
  block)

# Relationship functions

(defn add-to
  ```
  Adds the values in `d2` to `d1` using the same keys
  ```
  [d1 d2]
  (each k (keys d2)
    (if (nil? (get d1 k))
      (put d1 k (get d2 k))
      # TOASK why is it like this?
      (each name (keys (get d2 k))
        (put-in d1 [k name] (get-in d2 [k name])))))
  d1)

(defn children-of
  ```
  Gets the children of `node`
  ```
  [node]
  (last node))

(defn next-child
  ```
  Gets the last child of `node` if it is a container

  Returns nil if `node` is not a container.
  ```
  [node]
  (when (attribute node :container?)
    (last (children-of node))))

(defn next-container
  ```
  Gets the next open child of `node`

  Returns nil if the next child is not open.
  ```
  [node]
  (def child (next-child node))
  (when (and child (attribute child :open?))
    child))

(defn close-children
  ```
  Closes the children of `parent`
  ```
  [parent functions]
  (var prev parent)
  (while (def node (next-container prev))
    (def close-fn (get-fn :close node functions))
    (close-fn node prev)
    (set prev node)))

(defn last-descendant
  ```
  Gets the last descendant of `node`
  ```
  [node]
  (var curr-n (next-child node))
  (while (def next-n (next-child curr-n))
    (set curr-n next-n))
  curr-n)

# String manipulation

(defn dedent
  ```
  Dedents the `line` based on `start`
  ```
  [line start]
  (def len (length line))
  (var pos start)
  (var total-width 0)
  (while (< pos len)
    (def width (num-cols (get line pos) total-width))
    (if (nil? width)
      (break))
    (+= total-width width)
    (++ pos))
  (+= state/col-edge total-width)
  pos)

(defn entity-decode
  ```
  Encodes `entity` of kind `kind`
  ```
  [kind entity]
  (case kind
    :ent
    (or (entities/to-grapheme entity) entity)
    :dec
    (let [code (scan-number (string/slice entity 2 -2))]
      (if (zero? code)
        "\uFFFD"
        (parse (string/format `"\U%06x"` code))))
    :hex
    (let [code (string/slice entity 3 -2)
          len  (length code)]
      (if (string/check-set "0" code)
        "\uFFFD"
        (parse (string `"\U` (when (< len 6) (string/repeat "0" (- 6 len))) code `"`))))
    # impossible
    (errorf "kind %j not recognised" kind)))

(defn normalise
  ```
  Normalises whitespace in `s`
  ```
  [s]
  (def buf @"")
  (var found-space? false)
  (each c s
    (if (or (= 9 c) (= 10 c) (= 32 c))
      (set found-space? true)
      (do
        (buffer/push buf (if found-space? " " "") c)
        (set found-space? false))))
  (folding/case-fold buf))

(defn uri-encode
  ```
  Converts `uri` to be URL-encoded
  ```
  [uri]
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

# Miscellaneous

(defn to-continuation
  ```
  Creates a block node of `block-type` with trimmed `line` as its content
  ```
  [block-type line pos]
  (def content (-> (string/slice line pos) string/trim))
  [block-type @{} @[content]])

(defn words
  ```
  Converts `xs` into a grammar rule for use in a PEG
  ```
  [& xs]
  (defn word [s]
    (tuple
      ;(reduce (fn [arr c]
                 (->> ~(set ,(string/from-bytes c (- c 32)))
                      (array/push arr)))
               @['*]
               s)))
  ['+ ;(map word xs)])
