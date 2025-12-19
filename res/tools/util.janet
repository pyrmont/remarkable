(defn slurp-url [url]
  (def proc (os/spawn ["curl" "-s" url] :p {:in :pipe :err :pipe :out :pipe}))
  (def [exit-code out err]
    (ev/gather
      (os/proc-wait proc)
      (ev/read (proc :out) :all)
      (ev/read (proc :err) :all)))
  (os/proc-close proc)
  (assert (zero? exit-code) (string "failed to read " url))
  out)
