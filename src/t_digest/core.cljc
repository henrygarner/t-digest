(ns t-digest.core)

(defn ^:no-doc abs [x]
  (if (neg? x) (- x) x))

(defn ^:no-doc ceil [x]
  #?(:clj  (Math/ceil x)
     :cljs (js/Math.ceil x)))

(defn ^:no-doc centroid-comparator
  "Compares centroids on their means."
  [a b]
  (compare (:mean a) (:mean b)))

(def ^:no-doc empty-digest
  {:d 0.01
   :n 0
   :centroids (sorted-set-by centroid-comparator)})

(defn ^:no-doc add-centroid [centroids {:keys [mean count] :as centroid}]
  (if (contains? centroids centroid)
    (let [existing (get centroids centroid)]
      (-> (disj centroids existing)
          (conj (update existing :count + count))))
    (conj centroids centroid)))

(defn ^:no-doc compute-centroid-quantile
  [centroids n {:keys [count] :as centroid}]
  (let [sum (transduce (map :count) + (subseq centroids < centroid))]
    (/ (+ (/ count 2.0) sum) n)))

(defn ^:no-doc update-centroid
  [centroids {:keys [mean count] :as centroid} x w]
  (let [new-count (+ count w)
        new-centroid (-> (assoc centroid :count new-count)
                         (update :mean + (/ (* w (- x mean)) new-count)))]
    (-> (disj centroids centroid)
        (add-centroid new-centroid))))

(defn ^:no-doc threshold [n d q]
  (* 4 n d q (- 1 q)))

(defn ^:no-doc closest-centroids [centroids x]
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

(defn ^:no-doc candidate?
  [centroids {:keys [count] :as centroid} n d w]
  (let [threshold (threshold n d (compute-centroid-quantile centroids n centroid))]
    (<= (+ count w) threshold)))

(defn ^:no-doc add-value [centroids n d x w]
  (if (empty? centroids)
    (add-centroid centroids {:mean x :count w})
    (if-let [candidate (first (filter #(candidate? centroids % n d w) (closest-centroids centroids x)))]
      (update-centroid centroids candidate x w)
      (add-centroid centroids {:mean x :count w}))))

(defn ^:no-doc weighted-mean
  [i pi ni pm nm]
  (let [d (- ni pi)
        pw (/ (- ni i) d)
        nw (/ (- i pi) d)]
    (+ (* pm pw)
       (* nm nw))))

;; Public API ;;

(defn create
  "Creates an empty t-digest"
  ([]
   (create {}))
  ([{:keys [compression] :or {compression 100} :as options}]
   (assoc empty-digest :compression compression)))

(defn insert
  "Inserts a value into the digest with an optional weight"
  ([digest x]
   (insert digest x 1))
  ([{:keys [n d centroids] :as digest} x w]
   (let [new-n (+ n w)]
     (-> (assoc digest :n new-n)
         (update :centroids add-value new-n d x w)))))

(defn quantile
  "Calculates the minimum value x such that we think that the proportion of samples
   <= x is q"
  [{:keys [n d centroids]} q]
  (let [q (min (max 0.0 q) 1.0)
        c (count centroids)
        index (* q (dec n))]
    (cond
      (zero? c) nil
      (= c 1) (-> centroids first :mean)
      :else
      (let [indices (->> (reductions (fn [{:keys [total]} {:keys [count] :as centroid}]
                                       (-> (assoc centroid :total (+ count total))
                                           (assoc :index total)))
                                     {:total 0} centroids)
                         (rest)
                         (partition 2 1))]
        (some (fn [[a b]]
                (cond
                  (<= (:index a) index (:index b))
                  (let [pi (dec (ceil index))
                        ni (ceil index)
                        pm (:mean a)
                        nm (if (= ni (float (:index b)))
                             (:mean b) (:mean a))]
                    (weighted-mean index pi ni pm nm))

                  (= (:total b) n)
                  (let [pi (dec (ceil index))
                        ni (ceil index)
                        pm (if (= (float (:index a)) pi)
                             (:mean a) (:mean b))
                        nm (:mean b)]
                    (weighted-mean index pi ni pm nm))))
              indices)))))

(defn median
  "Calculates the 0.5 quantile"
  [digest]
  (quantile digest 0.5))

(defn iqr
  "Calculates the interquartile range. It is a measure of statistical dispersion
  more robust against outliers than the standard deviation."
  [digest]
  (- (quantile digest 0.75)
     (quantile digest 0.25)))

