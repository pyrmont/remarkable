(import ../deps/argy-bargy/argy-bargy :as argy)

(import ./init :as remark)

(def config
  ```
  The configuration for remark
  ```
  {:rules [:input     {:proxy "path"
                       :help  "The path of the input."}
           "--output" {:kind  :single
                       :short "o"
                       :proxy "path"
                       :help  "The path to output the result."}]
   :info {:about "A tool for parsing Markdown into HTML."}})

(def file-env (curenv))

(defn run
  []
  (def parsed (argy/parse-args "remark" config))
  (def err (parsed :err))
  (def help (parsed :help))
  (def params (parsed :params))
  (def opts (parsed :opts))
  (cond
    (not (empty? help))
    (do
      (prin help)
      (os/exit (if (opts "help") 0 1)))
    (not (empty? err))
    (do
      (eprin err)
      (os/exit 1))
    # default
    (do
      (try
        (do
          (def input (params :input))
          (-> (slurp input) remark/parse-md remark/render-html prin))
        ([e f]
         (eprint "error: " e)
         (debug/stacktrace f)
         (os/exit 1))))))

(defn main [& args] (run))
