(ns hive-mcp.chroma.helpers-trifecta-test
  "Chroma metadata codec characterization and contract generator checks."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.chroma.helpers :as helpers]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.generator :as mg]))

(deftrifecta tag-splitting
  hive-mcp.chroma.helpers/split-tags
  {:golden-path "test/golden/chroma/split-tags.edn"
   :cases {:empty "" :single "a" :multiple "a,b,c"}
   :gen (mg/generator [:string {:gen/max 100}])
   :pred #(or (nil? %) (vector? %))
   :num-tests 100
   :mutations [["always-empty" (constantly nil)]
               ["do-not-split" (fn [s] (when (seq s) [s]))]]
   :assert (fn [] (is (= ["a" "b"] (helpers/split-tags "a,b")))
              (is (nil? (helpers/split-tags ""))))})

(deftest metadata-fields-survive-conversion
  (let [entry (helpers/metadata->entry {:id "entry-1" :document "search text"
                                        :metadata {:type "note" :tags "a,b"
                                                   :content "hello"}})]
    (is (= "entry-1" (:id entry)))
    (is (= "search text" (:document entry)))
    (is (= ["a" "b"] (:tags entry)))))
