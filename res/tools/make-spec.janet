(import ../helpers/util)

(defn- parse-spec [text]
  (var current-section "")
  (var example-num 0)
  (var line-num 0)
  (def examples @[])
  (def lines (string/split "\n" text))
  (var i 0)
  (while (< i (length lines))
    (def line (get lines i))
    (set line-num (+ i 1))
    # track section headers
    (when (string/has-prefix? "## " line)
      (set current-section (string/slice line 3)))
    # find example blocks
    (when (= line "```````````````````````````````` example")
      (def start-line line-num)
      (++ i)
      (def markdown-lines @[])
      (while (and (< i (length lines))
                  (not= (get lines i) "."))
        (array/push markdown-lines (get lines i))
        (++ i))
      (++ i)
      (def html-lines @[])
      # collect HTML until we hit the closing backticks
      (while (and (< i (length lines))
                  (not= (get lines i) "````````````````````````````````"))
        (array/push html-lines (get lines i))
        (++ i))
      (def end-line (+ i 1))
      (++ example-num)
      # convert arrows (→) to tabs
      (defn arrows-to-tabs [s]
        (string/replace-all "\u2192" "\t" s))
      # Add the example
      (array/push examples
        @{:markdown (arrows-to-tabs (string (string/join markdown-lines "\n") "\n"))
          :html (arrows-to-tabs (string (string/join html-lines "\n") "\n"))
          :example example-num
          :start_line start-line
          :end_line end-line
          :section current-section}))
    (++ i))
  examples)

(defn- generate-spec [src output]
  (print (string "Downloading " src "..."))
  (def content (util/slurp-url src))
  (def examples (parse-spec content))
  (spit output (string/format "%j" examples))
  (print "Extracted " (length examples) " examples to " output))

# Generate CommonMark spec
(generate-spec "https://spec.commonmark.org/0.31.2/spec.txt"
               "res/fixtures/cmark_spec.jdn")
