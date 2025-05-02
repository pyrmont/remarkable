(declare-project
  :name "Remarkable"
  :description "A PEG-based Markdown parser"
  :author "Michael Camilleri"
  :license "MIT"
  :url "https://github.com/pyrmont/remarkable"
  :repo "git+https://github.com/pyrmont/remarkable"
  :vendored [{:url "https://github.com/pyrmont/medea"
              :prefix "medea"
              :include ["lib/decode.janet"]}])

# (declare-executable
#   :name "remark"
#   :entry "src/remarkable/init.janet")

(declare-source
  :source ["deps"
           "lib"]
  :prefix "remarkable")

(phony "spec" []
  (def spec-num (get (dyn :args) 3))
  (def result (os/execute [(dyn :executable "janet") "test/specs-runner.janet" (string spec-num)] :p)))

(task "vendor" []
  (def sep (if (= :windows (os/which)) "\\" "/"))
  (def deps-dir "deps")
  (def temp-dir "tmp")
  (defn mkdirp [path]
    (def pwd (os/cwd))
    (each dir (string/split sep path)
      (os/mkdir dir)
      (os/cd dir))
    (os/cd pwd))
  (defn is-tarball? [url]
    (or (string/has-suffix? ".gz" url)
        (string/has-suffix? ".tar" url)))
  (each {:url url
         :tag tag
         :prefix prefix
         :include includes
         :exclude excludes} (get (dyn :project) :vendored)
    (if-not url
      (error "Vended dependencies need a :url key")
      (do
        (default tag "HEAD")
        (def tarball (if (is-tarball? url) url (string url "/archive/" tag ".tar.gz")))
        (def dest-dir (if prefix (string/join [deps-dir prefix] sep) deps-dir))
        (def filename (-> (string/split "/" tarball) last))
        (print "Vendoring " tarball " to " dest-dir)
        (defer (rimraf temp-dir)
          (do
            (os/mkdir temp-dir)
            (def tar-file (string/join [temp-dir filename] sep))
            (curl "-sL" tarball "-o" tar-file)
            (tar "xf" tar-file "-C" temp-dir "--strip-components" "1")
            (rimraf tar-file)
            (when excludes
              (each exclude excludes
                (rimraf (string/join [temp-dir exclude] sep))))
            (def files (if includes includes (os/dir temp-dir)))
            (each file files
              (def from (string/join [temp-dir file] sep))
              (def to (string/join [dest-dir file] sep))
              (mkdirp (dirname to))
              (copy from to))))))))
