(ns organum.core-test
  (:use clojure.test
        clojure.walk
        organum.core)
  (:require [clojure.java.io :as io]))

(deftest test-headline
  (testing "Parsing headline"
    (is (= (#'organum.core/parse-headline "** TODO Some text :atag:btag:")
           (#'organum.core/section 2 "Some text" ["atag" "btag"] "TODO")))))

(deftest test-block
  (testing "Parsing block heeader"
    (is (= (#'organum.core/parse-block "  #+BEGIN_WIBBLE minor")
           (#'organum.core/block "WIBBLE" "minor")))))

(deftest test-testfile
  (testing "Parsing test.org"
    (let [sections (parse-file (io/resource "test.org"))]
      (is (vector? sections))
      (testing "Is only :section after leading :root"
        (is (every? #(= (:type %) :section) (rest sections))))
      (testing "Has top level headings"
        (is (some #(= (:level %) 1) sections)))
      (testing "Has second level headings"
        (is (some #(= (:level %) 2) sections)))
      (testing "Has headings with keywords"
        (is (some :kw sections)))
      (testing "Contains a property drawer"
        (is (some #(= (:type %) :drawer) (mapcat :content sections))))
      (testing "Contains table lines"
        (is (some #(= (:line-type %) :table-row) (mapcat :content sections))))
      (testing "Contains unordered list lines"
        (is (some #(= (:line-type %) :unordered-list) (mapcat :content sections)))))))

(def org-keywords ["NEXT" "WAITING" "DELEGATED" "CANCELED" "DONE"])

(deftest test2
  (testing "Parsing test2.org"
    (let [sections (parse-file (io/resource "test2.org") :encoding "ISO-8859-1" :org-todo-keywords org-keywords)]
      (is (= (:kw (nth sections 2)) "NEXT"))
      (is (= (:kw (nth sections 5)) "WAITING"))
      (is (= (:kw (nth sections 6)) "DELEGATED"))
      (is (= (:kw (nth sections 7)) "CANCELED"))
      (is (= (:kw (nth sections 8)) "DONE")))))
