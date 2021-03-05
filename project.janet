(declare-project
  :name "Remarkable"
  :description "A PEG-based Markdown parser"
  :author "Michael Camilleri"
  :license "MIT"
  :url "https://github.com/pyrmont/remarkable"
  :repo "git+https://github.com/pyrmont/remarkable"
  :dependencies ["https://github.com/janet-lang/json.git"])

(phony "spec" []
  (def spec-num (get (dyn :args) 3))
  (def result (os/execute [(dyn :executable "janet") "test/specs-runner.janet" (string spec-num)] :p)))

