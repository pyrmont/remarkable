(import ./util)

(def src "https://raw.githubusercontent.com/commonmark/cmark/refs/heads/master/src/utf8.c")
(def output "lib/punctuation.janet")
(def fname "cmark_utf8proc_is_punctuation_or_symbol")

(def grammar
  ~ {:main (* (to "&&") "&&" :s+ "(" :comps ")")
     :comps (some (* (? (* :s+ "||" :s+)) :comp))
     :comp (* "(" (+ (group :range) :equal) ")")
     :range (* "uc" :s+ ">=" :s+ (number :d+)
               :s+ "&&" :s+
               "uc" :s+ "<=" :s+ (number :d+))
     :equal (* "uc" :s+ "==" :s+ (number :d+))})

(def content (util/slurp-url src))
(def start (string/find fname content))
(def comps (peg/match grammar content (+ start (length fname))))

(def b @"")
(buffer/push b "(defn upunc? [x]\n  (or\n")

(each c comps
  (if (indexed? c)
    (buffer/push b "    (and (>= x " (string (c 0)) ") (<= x " (string (c 1)) "))\n")
    (buffer/push b "    (= x " (string c) ")\n")))

(buffer/popn b 1)
(buffer/push b "))")

(spit output b)
