(ns t-digest.core
  (:refer-clojure :rename {conj cconj})
  (:import [com.tdunning.math.stats TDigest]))

(defn abs [x]
  (if (neg? x) (- x) x))

(defn centroid-comparator
  "Compares centroids on their means."
  [a b]
  (compare (:mean a) (:mean b)))

(def empty-digest
  {:n 0
   :d 0.01
   :centroids (sorted-set-by centroid-comparator)})

(defn add-centroid [centroids {:keys [mean count] :as centroid}]
  (if (contains? centroids centroid)
    (let [existing (get centroids centroid)]
      (-> (disj centroids existing)
          (cconj (update existing :count + count))))
    (cconj centroids centroid)))

(defn compute-centroid-quantile
  [centroids n {:keys [count] :as centroid}]
  (let [sum (transduce (map :count) + (subseq centroids < centroid))]
    (/ (+ (/ count 2.0) sum) n)))

(defn update-centroid
  [centroids {:keys [mean count] :as centroid} x w]
  (let [new-count (+ count w)
        new-centroid (-> (assoc centroid :count new-count)
                         (update :mean + (/ (* w (- x mean)) new-count)))]
    (-> (disj centroids centroid)
        (add-centroid new-centroid))))

(defn threshold [n d q]
  (* 4 n d q (- 1 q)))

(defn closest-centroids [centroids x]
  (let [a (first (subseq centroids >= {:mean x}))
        b (first (rsubseq centroids <= {:mean x}))]
    (cond
      (empty? centroids) nil
      (nil? a) [b]
      (nil? b) [a]
      (= (:mean a) (:mean b)) [a]
      (= (abs (- (:mean a) x))
         (abs (- (:mean b) x))) [a b]
      (< (abs (- (:mean a) x))
         (abs (- (:mean b) x))) [a]
      :else [b])))

(defn candidate?
  [centroids {:keys [count] :as centroid} n d w]
  (let [threshold (threshold n d (compute-centroid-quantile centroids n centroid))]
    (<= (+ count w) threshold)))

(defn add-value [centroids n d x w]
  (if (empty? centroids)
    (add-centroid centroids {:mean x :count w})
    (if-let [candidate (first (filter #(candidate? centroids % n d w) (closest-centroids centroids x)))]
      (update-centroid centroids candidate x w)
      (add-centroid centroids {:mean x :count w}))))

(defn conj
  ([digest x]
   (conj digest x 1))
  ([{:keys [n d centroids] :as digest} x w]
   (let [new-n (+ n w)]
     (-> (assoc digest :n new-n)
         (update :centroids add-value new-n d x w)))))

(defn percentile
  [{:keys [n d centroids]} p]
  (let [p (min (max 0 p) 100)
        p (* 0.01 p n)]
    (if (< p (-> centroids first :count))
      (-> centroids first :mean)
      (let [stats (partition 3 1 (rest (reductions (fn [stats {:keys [count mean]}]
                                                     (-> (assoc stats :mean mean)
                                                         (assoc :count count)
                                                         (update :total + count)))
                                                   {:count 0 :total 0}
                                                   centroids)))]
        (or (some (fn [[w x y]]
                    (when (< p (:total x))
                      (let [delta (/ (- (:mean y) (:mean w)) 2.0)]
                        (+ (:mean x) (* (- (/ (- p (:total w)) (:count x)) 0.5) delta))))) stats)
            (-> centroids last :mean))))))
