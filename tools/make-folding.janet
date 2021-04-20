(def input "CaseFolding.txt") # http://unicode.org/Public/UCD/latest/ucd/CaseFolding.txt
(def output "src/remarkable/folding.janet")
(def indent "   ")

(def case-fold-fn
  ```
  (defn case-fold [s]
    (def buf @"")
    (var i 0)
    (while (< i (length s))
      (def c (get s i))
      (def grapheme
        (cond
          # 1-byte variant (0xxxxxxx)
          (< c 0x80) c
          # 2-byte variant (110xxxxx 10xxxxxx)
          (< 0xBF c 0xE0) (bor (blshift (band c 0x1F) 6)
                               (band (get s (++ i)) 0x3F))
          # 3-byte variant (1110xxxx 10xxxxxx 10xxxxxx)
          (< c 0xF0) (bor (blshift (band c 0x0F) 12)
                          (blshift (band (get s (++ i)) 0x3F) 6)
                          (band (get s (++ i)) 0x3F))
          # 4-byte variant (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
          (< c 0xF8) (bor (blshift (band c 0x07) 18)
                          (blshift (band (get s (++ i)) 0x3F) 12)
                          (blshift (band (get s (++ i)) 0x3F) 6)
                          (band (get s (++ i)) 0x3F))))
      (def lowered (get lower grapheme))
      (if (nil? lowered)
        (buffer/push buf grapheme)
        (buffer/push buf ;lowered))
      (++ i))
    (string buf))
  ```)

(with [src (file/open input :r)]
  (with [dest (file/open output :w)]
    (file/write dest "(def lower\n  {\n")
    (loop [line :iterate (file/read src :line)]
      (unless (or (= 10 (get line 0))
                  (= 35 (get line 0)))
        (def [before kind after] (string/split "; " line))
        (when (or (= "C" kind)
                  (= "F" kind))
          (def afters (->> (string/split " " after) (map |(string "0x" $))))
          (file/write dest indent)
          (file/write dest (string "0x" before " [" (string/join afters " ") "]\n")))))
    (file/write dest "})\n")
    (file/write dest "\n\n")
    (file/write dest case-fold-fn)))
