(import ../state)
(import ../util)
(import ../container)

## Grammar

(defn- list-item [marker-width trailing-space &opt starts-blank?]
  (def start-pos state/col-pos)
  (+= state/col-edge marker-width)
  (set state/col-pos state/col-edge)
  (util/record-padding trailing-space)
  (if (empty? trailing-space)
    (++ state/col-pos)
    (+= state/col-pos (if (or starts-blank? (> (- state/col-edge state/col-pos) 4)) 1 (- state/col-edge state/col-pos))))
  [:list-item @{:container? true :open? true :width (- state/col-pos start-pos) :starts-blank? starts-blank?} @[]])

(defn- list [kind marker delim trailing-space &opt starts-blank?]
  (def start (when (= :ordinal kind) (scan-number marker)))
  (def marker-width (+ (length marker) (if (nil? delim) 0 1)))
  (def item (list-item marker-width trailing-space starts-blank?))
  [:list @{:kind kind :marker marker :delim delim :start start :tight? true :container? true :open? true} @[item]])

(def grammar
  ~{:list {:main    (cmt (* :marker :after) ,list)
           :marker  (+ :bullet :ordinal)
           :bullet  (* (constant :bullet) '(set "-+*") (constant nil))
           :ordinal (* (constant :ordinal) '(between 1 9 :d) '(set ".)"))
           :after   (+ (* '(any :space) :eol (constant true))
                       (* '(some :space)))}})

## Functions

(defn- list-blank [a-list parent protocols]
  (def item (util/next-container a-list))
  (when (and (not (nil? item))
             (util/attribute item :starts-blank?)
             (zero? (length (util/children-of item))))
    (util/attribute item :open? false))
  (util/attribute a-list :has-blank? true)
  (util/next-container a-list))

(defn- list-equal? [a-list block]
  (and (= :list (util/type-of block))
       (= (util/attribute a-list :kind) (util/attribute block :kind))
       (or (and (= :ordinal (util/attribute a-list :kind))
                (= (util/attribute a-list :delim) (util/attribute block :delim)))
           (and (= :bullet (util/attribute a-list :kind))
                (= (util/attribute a-list :marker) (util/attribute block :marker))))))

(defn- list-needs-nl? [a-list]
  (or (util/attribute (util/next-container a-list) :starts-blank?)
      (and (= :ordinal (util/attribute a-list :kind))
           (not= 1 (util/attribute a-list :start)))))

# The current open block is a list so we need to handle the case where the
# line is a continuation of the list. To check this we need to descend through
# the open list items in the current list that are indented at least as much as
# the current line.
(defn- list-next-block [a-list line pos grammar protocols]
  (def next-pos (util/dedent line pos))
  (var next-b nil)
  (var parent-list nil)
  (var parent-item nil)
  (var curr-list a-list)
  (var curr-item (util/next-container a-list))
  (while curr-item
    # set start col of list item
    (def curr-width (util/attribute curr-item :width))
    # break if there's not enough padding
    (def remaining-width (- state/col-edge state/col-pos))
    (if (> curr-width remaining-width)
      (break)
      (+= state/col-pos curr-width))
    # the parent list is at least equal to the current list
    (when parent-list
      (util/attribute parent-list :has-blank? false))
    (set parent-list curr-list)
    # create continuation
    (def new-item [:list-item-continue (get curr-item 1) @[]])
    (def new-list [:list (get curr-list 1) [new-item]])
    # set next-b and parent
    (if (nil? next-b)
      (set next-b new-list)
      (array/push (util/children-of parent-item) new-list))
    (set parent-item new-item)
    # break if no more lists
    (def child (util/next-container curr-item))
    (when (or (nil? child) (not= :list (util/type-of child)))
      (break))
    # prepare for next round of loop
    (set curr-list child)
    (set curr-item (util/next-container curr-list)))
  # parse line from current position
  (def result (peg/match grammar line next-pos))
  # make parent list loose if a blank line has come before
  (cond
    (and (nil? parent-list)
         (util/attribute a-list :has-blank?)
         (list-equal? a-list (get result 0)))
    (util/attribute a-list :tight? false)
    (and parent-list
         (util/attribute parent-list :has-blank?))
    (util/attribute parent-list :tight? false))
  # return result
  (if (nil? next-b)
    result
    [next-b next-pos]))

(defn- list-item-equal? [an-item block]
  (= :list-item-continue (util/type-of block)))

# Use container base with list-specific overrides
(util/add-to state/protocols
  {:blocks
    {:list (container/make-protocol
             {:equal?     list-equal?
              :blank      list-blank
              :next-block list-next-block
              :needs-nl?  list-needs-nl?})
     :list-item  {:equal? list-item-equal?}}})
