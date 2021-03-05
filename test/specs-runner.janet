(import json)
(import ../src/remarkable :as mark)


(def examples
  (json/decode (slurp "test/spec.json")))


(def html-grammar
  ~{:main (* (any (+ :value :broken-tag)) -1)

    :value (+ :tagged-value :raw-value)

    :raw-value (+ (* :s+ (> 0 (+ -1 "<")))
                  (/ '(some (if-not (set "<>") 1)) ,string/trim))

    :broken-tag '(* "<" (? "/") (thru (+ ">" "\n")))

    :name  (/ (* :s* ':w+ :s*) ,keyword)

    :attrs (/ (any :attr) ,struct)
    :attr  {:main  (* :key (+ (* "=" :value) (constant true)))
            :key   (/ (* :s* '(some (+ :w :d (set "_:"))) :s*) ,keyword)
            :value (* :s* (+ (* "\"" '(some (if-not "\"" (+ "\\\"" 1))) "\"")
                             (* "'" '(some (if-not "'" (+ "\\'" 1))) "'")
                             '(some (+ :w :d (set "_:")))):s*)}

    :tagged-value (/ (+ :self-closing :literal :enclosing :other) ,tuple)

    :other (* (constant :-other) (constant {})
              (+ :comment :instruction :declaration :cdata))

    :self-closing {:main (* "<" :name :attrs (? "/") ">")
                   :name (/ (* :s* '(+ "a" "br" "hr" "img" "style") :s*) ,keyword)}

    :literal {:main  (* :open :value :close)
              :open  (* "<" (/ '"code" ,keyword) :attrs ">")
              :close "</code>"
              :value '(any (if-not (set "<>") 1))}

    :enclosing {:main (* :open (any :value) :close)
                :open (* "<" :name :attrs ">")
                :close (* "</" (drop :name) ">")}

    :comment '(* "<!--" (thru "-->"))

    :instruction '(* "<?" :w* (thru "?>"))

    :declaration '(* "<!" :w+ (thru ">"))

    :cdata '(* "<![CDATA[" (thru "]]>"))})


(defn compare-html [a-html b-html]
  (def a-root (peg/match html-grammar a-html))
  (def b-root (peg/match html-grammar b-html))
  (var result true)

  (defn compare-nodes [a b]
    (if (and (string? a)
             (string? b)
             (= a b))
      result
      (when (and (= (length a) (length b))
                 (= (first a) (first b)))
        (var equal? true)
        (loop [k :keys (get a 1)
                 :while equal?]
          (set equal? (= (get (get a 1) k)
                         (get (get b 1) k))))
        (loop [i :range [2 (length a)]
                 :while equal?]
          (set equal? (compare-nodes (get a i) (get b i))))
        equal?)))

  (loop [i :range [0 (length a-root)]
           :while result]
    (set result (compare-nodes (get a-root i) (get b-root i))))
  result)


(defn run-spec [example]
  (print "Example #" (get example "example"))
  (def input (get example "markdown"))
  (def expect (get example "html"))
  (def actual (-> input mark/parse-md (mark/render-html {:start-nl? false})))
  (assert (compare-html expect actual)
          (string "-------" "\n"
                  "Example #" (get example "example") "\n"
                  "Source: " (describe input) "\n"
                  "Expect: " (describe expect) "\n"
                  "Actual: " (describe actual) "\n"
                  "-------")))


(def spec-num (-?> (dyn :args) (get 1) scan-number))
(if (not (nil? spec-num))
  (run-spec (find |(= spec-num (get $ "example")) examples))
  (each example examples
    (run-spec example)))
