(ns scl.core
  (:require [jsonista.core :as j] ; fastest yet simple json around
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.test :refer [is]])
  (:gen-class))

(defn parse-line 
  "Takes a header and a regex separator to build lines. If there is a header, outputs maps with the header as keys, else arrays."
  {:test #(do (is (= "[1,2.0,3]" ((parse-line false #"\,\s?") "1,2.0, 3")))
              (is (= "{\"a\":1,\"b\":2}" ((parse-line ["a","b"] #"\;") "1;2")))
              (is (= "{\"a\":\"b\",\"c\":\"d\"}" ((parse-line ["a","c"] #"\_") "b_d"))))}
  [header sep]
  (let [build-line (if header #(zipmap header %) identity)]
    #(->> (s/split % sep)
          (mapv edn/read-string)
          build-line
          j/write-value-as-string)))

(defn -main [file sep & args]
  (with-open [input (io/reader file)]
    (let [init (if (= \\ (first sep)) "\\" "") ; to accept regex we need to escape the '\'s
          sep* (re-pattern (str init (first sep) "\\s?")) ; build regex, with possible space after
          no-headers? ((into #{} args) "--no-headers") ; check if we want headers
          lines* (line-seq input)  ; lazy seq of lines
          header (if no-headers? false (s/split (first lines*) sep*))
          lines (if no-headers? lines* (rest lines*))
          build-line (parse-line header sep*)] ; function that takes a line and builds a json line
          (->> lines
              (pmap build-line)
              (mapv println))
          (System/exit 0))))
