;;; By cesarbp - testing stuff

(ns clabango.play
  "Playing around"
  (:require [clabango.parser :as p]
            [clojure.string :as s]
            [clabango.tags :refer [valid-tags]])
  (:import [java.io PushbackReader CharArrayReader]))

;;; TODO - finish tag buffers
;;; TODO - turn tokens into fns

(defn ^PushbackReader make-reader
  [^String s]
  (PushbackReader. (CharArrayReader. (.toCharArray s))))

(def ^Character tag-opener \{)
(def ^Character filter-second \{)
(def ^Character tag-second \%)

(declare buffer-string buffer-filter buffer-tag)

(defn lexer*
  [^PushbackReader r state ast]
  (loop [state state ast ast c (.read r)]
    (if (== -1 c)
      ast
      (let [c (char c)]
        (case state
          :read-string
          (recur (if (= filter-second c) :read-filter :read-tag)
                 (conj ast (buffer-string r))
                 (.read r))
          :read-filter
          (recur :read-string
                 (conj ast {:type :filter
                            :body (buffer-filter r)})
                 (.read r))
          :read-tag
          (recur :read-string
                 (conj ast (buffer-tag r))
                 (.read r)))))))

(defn buffer-string
  [^PushbackReader r]
  (let [sb (StringBuilder.)]
    (loop [cn (.read r)]
      (if (== cn -1)
        ;; Finished reading
        (str sb)
        (let [c (char cn)]
          (if (= tag-opener c)
            (let [cn2 (.read r)]
              (if (== cn2 -1)
                ;; Reached end of reader
                (do (.append sb c)
                    (str sb))
                (let [c2 (char cn2)]
                  (if (or (= filter-second c2)
                          (= tag-second c2))
                    ;; Finished reading because we found a token start
                    ;; Unread last so lexer* can know whether its tag or filter
                    (do (.unread r (int c2))
                        (str sb))
                    (do (.append sb c)
                        (.append sb c2)
                        (recur (.read r)))))))
            (do (.append sb c)
                (recur (.read r)))))))))

(defn buffer-filter
  [^PushbackReader r]
  (let [sb (StringBuilder.)]
    (loop [cn (.read r)]
      (if (== -1 cn)
        (throw (IllegalStateException. "Filter with no closing '}}'"))
        (let [c (char cn)]
          (case c
            ;; Possibly found the end of the filter
            \}
            (let [cn2 (.read r)]
              (if (== -1 cn2)
                (throw (IllegalStateException. "Filter with no closing '}}'"))
                (let [c2 (char cn2)]
                  (if (= \} c2)
                    ;; Found the end of the filter
                    (str sb)
                    (do (.append sb c)
                        (.append sb c2)
                        (str sb))))))
            ;; Possibly found a nested token - probably illegal
            \{
            (let [cn2 (.read r)]
              (if (== -1 cn2)
                (throw (IllegalStateException. "Filter with no closing '}}'"))
                (let [c2 (char cn2)]
                  (case c2
                    ;; Nested filter
                    \{ {:type :filter
                        :body (buffer-filter r)}
                    ;; Nested tag
                    \% {:type :tag
                        :body (buffer-tag r)}
                    ;; Didnt find a nested token
                    (do (.append sb c)
                        (.append sb c2)
                        (recur (.read r)))))))
            (do (.append sb c)
                (recur (.read r)))))))))

(declare buffer-tag-args buffer-tag-body buffer-tag-closing parse-tag-args)

(defn buffer-tag
  [^PushbackReader r]
  (let [{:keys [name args closing]} (buffer-tag-args r)
        ]
    (when-not (= :inline closing)
      (buffer-tag-closing r closing))
    (if (= :inline closing)
      {:type :tag-inline
       :args args
       :name name}
      (let [body (body (lexer* r :read-string []))]
        {:type :tag
         :args args
         :name name
         :body body}))))

(defn buffer-tag-args
  [^PushbackReader r]
  (let [sb (StringBuilder.)]
    (loop [cn (.read r)]
      (if (== -1 cn)
        (throw (IllegalStateException. "Tag with no closing '%}'"))
        (let [c (char cn)]
          (if (= \% c)
            ;; Possibly found the end of the tag args
            (let [cn2 (.read r)]
              (if (== -1 cn2)
                (throw (IllegalStateException. "Tag with no closing '%}'"))
                (let [c2 (char cn2)]
                  (if (= \} c2)
                    ;; Found the end of the tag args
                    (str sb)
                    (do (.append sb c)
                        (.append sb c2)
                        (recur (.read r)))))))
            (do (.append sb c)
                (recur (.read r)))))))))

(defn buffer-tag-closing
  [^PushbackReader r to-find]
  (let [sb (StringBuilder.)]
    (loop [cn (.read r)]
      (if (== -1 cn)
        (throw (IllegalStateException. (str "Didn't find '" to-find "' closing tag")))
        ()))))

(defn parse-tag-args
  [s]
  (let [[tag & args] (-> s
                         (s/trim)
                         (s/split #"\s+"))]
    (if-not (find @valid-tags tag)
      (throw (IllegalStateException. (str "Invalid tag:" tag)))
      {:name tag
       :args args
       :closing (@valid-tags tag)})))
