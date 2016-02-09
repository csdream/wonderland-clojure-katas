(ns alphabet-cipher.coder)

(def offset (int \a))

(def mod-num (count "abcdefghijklmnopqrstuvwxy"))

(defn- char->index [c]
  (- (int c) offset))

(defn- index->char [n]
  (char (+ n offset)))

(defn- string->indexes [s]
  (map char->index s))


(defn- indexes->string [ns]
  (apply str (map index->char ns)))

(defn- code-op-fn [f]
  (fn [k m] (f k m)))

(def encode-op-fn (fn [k m] (mod (+ k m) (inc mod-num))))
(def decode-op-fn (fn [k m] (mod (- m k) (inc mod-num))))
(def cipher-op-fn (fn [c m] (mod (- c m) (inc mod-num))))

(defn code-op [keyword message code-op-fn]
  (indexes->string
   (let [keyword-indexes (string->indexes keyword)
          message-indexes (string->indexes message)
          partitioned-message-indexes (partition-all (count keyword) message-indexes)]
      (flatten (map #(map code-op-fn keyword-indexes %) partitioned-message-indexes)))))

(defn- is-pattern-found? [ns]
  (= 1 (count (set ns))))

(defn- find-pattern
  ([coll] (find-pattern coll 1))
  ([coll n] (if (= n (count coll))
               coll
               (let [ns (partition n coll)]
                 (if (is-pattern-found? ns)
                   (first ns)
                   (recur coll (inc n)))))))

(defn encode [keyword message]
  (code-op keyword message encode-op-fn))

(defn decode [keyword message]
  (code-op keyword message decode-op-fn))

(defn decipher [cipher message]
  (apply str
         (find-pattern
          (code-op cipher message cipher-op-fn))))

(comment
  (do
   (def keywords "scones")
   (def message "meetmebythetree")
   (def cipher "egsgqwtahuiljgs")
   (def k-index (string->indexes keywords))
   (def m-index (string->indexes message)))

  )















