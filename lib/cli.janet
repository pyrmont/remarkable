(import ../deps/argy-bargy/argy-bargy :as argy)

(import ./init :as remark)

(def config
  ```
  The configuration for remark
  ```
  {:rules [:input     {:proxy "path"
                       :req?  true
                       :help  `The path of the input. To read from stdin, use
                              '-'.`}
           "--output" {:kind  :single
                       :short "o"
                       :proxy "path"
                       :help  `The path to output the result. If not set and
                              input is a file path, replace the file extension
                              with .html. To output to stdout, use '-'.`}]
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
          (def input-path (params :input))
          (def input-text
            (if (= "-" input-path)
              (file/read stdin :all)
              (slurp input-path)))
          (def output-path
            (or (opts "output")
                (if (= "-" input-path)
                  (error "cannot infer output path when input is stdin")
                  (do
                    (def begin (or (-?> (string/find-all "/" input-path) last inc) 0))
                    (def ends (string/find-all "." input-path begin))
                    (def end (if (empty? ends) (length input-path) (last ends)))
                    (string (string/slice input-path 0 end) ".html")))))
          (def html (-> input-text remark/parse-md remark/render-html))
          (if (= "-" output-path)
            (prin html)
            (spit output-path html)))
        ([e f]
         (eprint "error: " e)
         (debug/stacktrace f)
         (os/exit 1))))))

(defn main [& args] (run))
