(use ../deps/testament)

(import ../lib/remarkable)

(deftest none
  (def expect
     [:document
       {:container? true :open? true}
       [[:paragraph
         {:inlines? true :open? false}
         ["hello world"]]]])
  (is (== expect (remarkable/parse-md "hello world"))))

(deftest basic
  (def expect
     [:document {:container? true :open? true}
      [[:paragraph {:inlines? true :open? false}
        ["hello "
         [:strong {}
          ["world"]]]]]])
  (is (== expect (remarkable/parse-md "hello **world**"))))

(deftest trailing-ws-1
  (def expect
     [:document {:container? true :open? true} []])
  (is (== expect (remarkable/parse-md " "))))

(deftest trailing-ws-2
  (def expect
     [:document {:container? true :open? true} []])
  (is (== expect (remarkable/parse-md "\t"))))

(deftest trailing-ws-3
  (def expect
     [:document {:container? true :open? true}
      [[:heading {:inlines? true :kind :atx :level 1 :open? false}
        ["Hello World"]]]])
  (is (== expect (remarkable/parse-md "# Hello World\n "))))

(deftest trailing-ws-4
  (def expect
     [:document {:container? true :open? true}
      [[:blockquote {:container? true :open? false}
        [[:paragraph {:inlines? true :open? false}
          ["Hello world"]]]]]])
  (is (== expect (remarkable/parse-md "> Hello world\n "))))

(deftest trailing-ws-5
  (def expect
     [:document {:container? true :open? true}
      [[:codeblock {:kind :indented :open? false}
        ["(def a 1)\n"]]]])
  (is (== expect (remarkable/parse-md "    (def a 1)\n "))))

(run-tests!)
