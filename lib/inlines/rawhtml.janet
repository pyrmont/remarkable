(use ../globals)
(use ../utilities)


## Grammar

(defn- rawhtml [content]
  [:rawhtml @{} content])


(def grammar
  ~{:rawhtml {:main        (/ '(+ :opening-tag :closing-tag :comment :instruction :declaration :cdata) ,rawhtml)
              :name        (* :alpha (any (+ :alnum "-")))
              :attribute   {:main    (* :ws :name (? :spec))
                            :name    (* (+ :alpha (set "_:")) (any (+ :alnum (set "_.:-"))))
                            :spec    (* (? :ws) "=" (? :ws) (+ :unquote :single :double))
                            :unquote (some (if-not (+ :ws (set "\"'=<>`")) 1))
                            :single  (* "'" (to "'") "'")
                            :double  (* `"` (to `"`) `"`)}
              :opening-tag (* "<" :name (any :attribute) (any :ws) (? "/") ">")
              :closing-tag (* "</" :name (any :ws) ">")
              :bad-comment (* "<!--" (any "-") ">")
              :ok-comment  (* "<!--" (to "-->") "-->")
              :comment     (+ :bad-comment :ok-comment)
              :instruction (* "<?" (thru "?>"))
              :declaration (* "<!" (some (range "AZ")) (some :ws) (some (if-not ">" 1)) ">")
              :cdata       (* "<![CDATA[" (thru "]]>"))}})
