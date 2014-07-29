(ns organum.core
  (:require [clojure.java.io :refer [reader]]
            [clojure.string :refer [blank? split triml replace-first join lower-case]]))

;; node constructors

(def #^{:private true :dynamic true} *org-todo-keywords* ["TODO" "DONE"])
(defn- node [type] {:type type :content []})
(defn- root [] (node :root))
(defn- section [level name tags kw] (merge (node :section) {:level level :name name :tags tags :kw kw}))
(defn- block [type qualifier] (merge (node :block) {:block-type type :qualifier qualifier}))
(defn- drawer [] (node :drawer))
(defn- line [type text] {:line-type type :text text})

(defn- classify-line
  "Classify a line for dispatch to handle-line multimethod."
  [ln]
  (let [headline-re #"^(\*+)\s*(.*)$"
        pdrawer-re #"^\s*:(PROPERTIES|END):"
        pdrawer (fn [x] (second (re-matches pdrawer-re x)))
        pdrawer-item-re #"^\s*:([0-9A-Za-z_\-]+):\s*(.*)$"
        block-re #"^\s*#\+(BEGIN|END)_(\w*)\s*([0-9A-Za-z_\-]*)?.*"
        block (fn [x] (rest (re-matches block-re x)))
        def-list-re #"^\s*(-|\+|\s+[*])\s*(.*?)::.*"
        ordered-list-re #"^\s*\d+(\.|\))\s+.*"
        unordered-list-re #"^\s*(-|\+|\s+[*])\s+.*"
        metadata-re #"^\s*(CLOCK|DEADLINE|START|CLOSED|SCHEDULED):.*"
        table-sep-re #"^\s*\|[-\|\+]*\s*$"
        table-row-re #"^\\s*\\|.*"
        inline-example-re #"^\s*:\s.*"
        horiz-re #"^\s*-{5,}\s*$"]
    (cond
     (re-matches headline-re ln) :headline
     (blank? ln) :blank
     (re-matches def-list-re ln) :definition-list
     (re-matches ordered-list-re ln) :ordered-list
     (re-matches unordered-list-re ln) :unordered-list
     (= (pdrawer ln) "PROPERTIES") :property-drawer-begin-block
     (= (pdrawer ln) "END") :property-drawer-end-block
     (re-matches pdrawer-item-re ln) :property-drawer-item
     (re-matches metadata-re ln) :metadata
     (= (first (block ln)) "BEGIN") :begin-block
     (= (first (block ln)) "END") :end-block
     (= (second (block ln)) "COMMENT") :comment
     (= (first ln) \#) :comment
     (re-matches table-sep-re ln) :table-separator
     (re-matches table-row-re ln) :table-row
     (re-matches inline-example-re ln) :inline-example
     (re-matches horiz-re ln) :horizontal-rule
     :else :paragraph)))

(defn- strip-tags
  "Return the line with tags stripped out and list of tags"
  [ln]
  (if-let [[_ text tags] (re-matches #"(.*?)\s*(:[\w:]*:)\s*$" ln)]
    [text (remove blank? (split tags #":"))]
    [ln nil]))

(defn- strip-keyword
  "Return the line with keyword stripped out and list of keywords"
  [ln]
  (let [keywords-re (->> *org-todo-keywords* (join \|) (format "(%s)?") re-pattern)
        words (split ln #"\s+")]
    (if (re-matches keywords-re (words 0))
      [(triml (replace-first ln (words 0) "")) (words 0)]
      [ln nil])))

(defn- parse-headline [ln]
  (when-let [[_ prefix text] (re-matches  #"^(\*+)\s*(.*?)$" ln)]
    (let [[text tags] (strip-tags text)
          [text kw] (strip-keyword text)]
      (section (count prefix) text tags kw))))

(defn parse-block [ln]
  (let [block-re #"^\s*#\+(BEGIN|END)_(\w*)\s*([0-9A-Za-z_\-]*)?"
        [_ _ type qualifier] (re-matches block-re ln)]
    (block type qualifier)))

;; State helpers

(defn- subsume
  "Updates the current node (header, block, drawer) to contain the specified
   item."
  [state item]
  (let [top (last state)
        new (update-in top [:content] conj item)]
    (conj (pop state) new)))

(defn- subsume-top
  "Closes off the top node by subsuming it into its parent's content"
  [state]
  (let [top (last state)
        state (pop state)]
    (subsume state top)))

(defmulti #^{:private true}
  handle-line
  "Parse line and return updated state."
  (fn [state ln]
    (classify-line ln)))

(defmethod #^{:private true}
  handle-line :headline [state ln]
  (conj state (parse-headline ln)))

(defmethod #^{:private true}
  handle-line :begin-block [state ln]
  (conj state (parse-block ln)))

(defmethod #^{:private true}
  handle-line :end-block [state ln]
  (subsume-top state))

(defmethod #^{:private true}
  handle-line :property-drawer-begin-block [state ln]
  (conj state (drawer)))

(defmethod #^{:private true}
  handle-line :property-drawer-end-block [state ln]
  (subsume-top state))

(defmethod #^{:private true}
  handle-line :default [state ln]
  (subsume state (line (classify-line ln) ln)))

(defn parse-file
  "Parse file (name / url / File) into (flat) sequence of sections. First section may be type :root,
   subsequent are type :section. Other parsed representations may be contained within the sections"
  [f & {:keys [org-todo-keywords encoding]
        :or {org-todo-keywords *org-todo-keywords*
             encoding "UTF-8"}}]
  (binding [*org-todo-keywords* org-todo-keywords]
    (with-open [rdr (reader f :encoding encoding)]
      (reduce handle-line [(root)] (line-seq rdr)))))

(defn agenda-tasks
  "Takes parsed org file and returns agenda tasks"
  [org]
  (->> org
       (filter :kw)
       (map (fn [m] {:desc (:name m) :status (keyword (lower-case (:kw m)))}))))
