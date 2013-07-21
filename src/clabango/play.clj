;;; By cesarbp - testing stuff

(ns clabango.play
  "Playing around"
  (:require [clabango.parser :as p]
            [clojure.string :as s]
            [clabango.tags :refer [valid-tags]])
  (:import [java.io PushbackReader CharArrayReader]))

;;; TODO - implement filter/tag parsers

(defn ^PushbackReader make-reader
  [^String s]
  (PushbackReader. (CharArrayReader. (.toCharArray s))))

(def ^Character tag-opener \{)
(def ^Character filter-second \{)
(def ^Character tag-second \%)

(declare buffer-string buffer-filter buffer-tag parser*)

(defn parser
  [s]
  (let [rdr (make-reader s)]
    (parser* rdr :read-string [] nil)))

(defn parser*
  [^PushbackReader r state ast closing-to-find]
  (loop [state state ast ast to-find closing-to-find]
    (case state
      :read-string
      (let [[next-step buffered-str] (buffer-string r)]
        (case next-step
          ;; Found end of string
          ;; TODO - error if there's still a closing tag to find
          :end ast
          :read-filter (recur :read-filter
                              (conj ast buffered-str)
                              to-find)
          :read-tag (recur :read-tag
                           (conj ast buffered-str)
                           to-find)))
      :read-filter
      (recur :read-string
             (conj ast {:type :filter
                        :body (buffer-filter r)})
             to-find)
      :read-tag
      (let [{:keys [status name closing-tag args]} (buffer-tag r)]
        (case status
          :found-closing-tag
          (cond
           (and to-find closing-tag
                (= to-find closing-tag))
           ast
           (and to-find closing-tag)
           (throw (IllegalStateException. (str "Expected closing tag "
                                               to-find
                                               ". Got " closing-tag " instead.")))
           ;; Found a closing tag for no reason
           :else
           (do
             (println "Warning: Found a random closing tag:" closing-tag)
             (recur :read-string
                    ast
                    to-find)))
          :found-opening-tag
          ;; Keep parsing after the tag, looks kinda silly
          (do (recur :read-string
                     (conj ast
                           {:type :tag
                            :tag name
                            :args args
                            ;; Parse the body of the tag with a brand new tree
                            :body
                            (when-not (= :inline closing-tag)
                              (parser* r :read-string [] (when-not
                                                             (= :inline closing-tag)
                                                           closing-tag)))})
                     to-find ;; Assume we found the closing tag
                     )))))))

(defn buffer-string
  [^PushbackReader r]
  (let [sb (StringBuilder.)]
    (loop [cn (.read r)]
      (if (== cn -1)
        ;; Finished reading
        [:end
         (str sb)]
        (let [c (char cn)]
          (if (= tag-opener c)
            (let [cn2 (.read r)]
              (if (== cn2 -1)
                ;; Reached end of reader
                (do (.append sb c)
                    [:end
                     (str sb)])
                (let [c2 (char cn2)]
                  (if (or (= filter-second c2)
                          (= tag-second c2))
                    [(if (= filter-second c2)
                       :read-filter
                       :read-tag)
                     (str sb)]
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
            (do (.append sb c)
                (recur (.read r)))))))))

(declare parse-tag-args)

(defn buffer-tag
  [^PushbackReader r]
  (let [sb (StringBuilder.)]
    (loop [cn (.read r)]
      (if (== -1 cn)
        (throw (IllegalStateException. "Tag with no closing %} 1"))
        (let [c (char cn)]
          (case c
            ;; Possibly found end of tag '%}'
            \%
            (let [cn2 (.read r)]
              (if (== -1 cn2)
                (throw (IllegalStateException. "Tag with no closing %} 2"))
                (let [c2 (char cn2)]
                  (case c2
                    \}
                    ;; Return the map result of parsing the tag args
                    (parse-tag-args (str sb))
                    (do (.append sb c)
                        (.append sb c2)
                        (recur (.read r)))))))
            (do (.append sb c)
                (recur (.read r)))))))))


(defn type-of-tag
  [tag]
  (let [vtags @valid-tags
        opening (set (keys vtags))
        closing (set (vals vtags))]
    (cond
     (or (= :inline (closing tag))
         (opening tag))
     :opening
     (closing tag)
     :closing
     :else
     :invalid)))

(defn parse-tag-args
  [s]
  (let [[tag & args]
        ;; TODO - find out actual spec
        (-> s
            (s/trim)
            (s/split #"\s+"))
        tag-type (type-of-tag tag)]
    (if (= :invalid tag-type)
      (throw (IllegalStateException. (str "Invalid tag:" tag)))
      {:name tag
       :args args
       :status (case tag-type
                 :opening :found-opening-tag
                 :closing :found-closing-tag)
       :closing-tag (case tag-type
                      :opening (@valid-tags tag)
                      :closing tag)})))


;;; Testing stuff
(comment
  (def ts1
    "<body>
{{foo|upper}}

{% for x in lst %}
{{x}}
{% include foo.html %}
{%endfor%}

</body>")
 (def ts2 (apply str (repeat 2 ts1)))
 (def ts3
    "<body>
{{foo|upper}}

{% for x in lst %}
{{x}}
{% include foo.html %}
{% block bar%}
{{y}}
{%endblock%}

{%endfor%}

</body>")

 (def ts4 (apply str (repeat 2 ts3)))

 (parser ts1)
 (parser ts2)
 (parser ts3)

)
