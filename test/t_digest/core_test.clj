(ns t-digest.core-test
  (:require [t-digest.core :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test :refer [is deftest testing]])
  (:import [com.tdunning.math.stats TDigest]))

(def test-opts {:num-tests 1000
                :par       4})

(defn histogram-rf
  ([] (create))
  ([acc x] (insert acc x))
  ([acc] acc))

(defn make-histogram
  [xs]
  (transduce identity histogram-rf xs))

(defn make-reference-histogram
  [compression xs]
  (reduce #(doto %1 (.add %2 1)) (TDigest/createDigest compression) xs))

(defn =ish [a b]
  (< (/ (abs (- a b)) (max a b)) 0.005))

(def numeric
  (gen/one-of [(gen/double* {:infinite? false :NaN? false})
               gen/int gen/large-integer]))

(def non-empty-vector
  (comp gen/not-empty (partial gen/vector)))

(deftest median-test
  (testing ""
    (let [histogram (make-histogram [])]
      (is (nil? (median histogram))))

    (let [histogram (make-histogram (shuffle (range 10)))]
      (is (=ish 4.5 (median histogram))))

    (let [histogram (make-histogram (shuffle (range 100)))]
      (is (=ish 49.5 (median histogram))))

    (let [histogram (make-histogram (shuffle (range 1000)))]
      (is (=ish 499 (median histogram))))

    (let [coll [3 1.0 3]
          h1 (make-histogram coll)
          h2 (make-reference-histogram 100 coll)]
      (is (= (quantile h1 0.51)
             (.quantile h2 0.51))))))

(defspec reference-version-median-equivalence
  test-opts
  (for-all [coll (non-empty-vector numeric)
            percentile (gen/choose 0 100)]
           (let [compression 100
                 q (/ percentile 100.0)
                 h1 (make-histogram coll)
                 h2 (make-reference-histogram compression coll)]
             (is (= (double (quantile h1 q))
                    (.quantile h2 q))))))
