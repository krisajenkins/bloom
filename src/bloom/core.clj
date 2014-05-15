(ns bloom.core
  (:require [clojure.java.io :as io]))

(defn builtin-hash
  [element]
  (hash element))

(defn sum-hash
  [element]
  (reduce + (map int element)))

(defn djb2-hash
  [element]
  (reduce (fn [hash-address char]
            (+ (bit-shift-left hash-address 5)
               (int char)))
          5381
          element))

(defn adler32
  [element]
  (let [[s1-final s2-final]
        (reduce (fn [[s1 s2] c]
                  (let [s1' (mod (+ s1 (int c))
                                 65521)
                        s2' (mod (+ s1' s2)
                                 65521)]
                    [s1' s2']))
                [1 0]
                element)]
    (bit-or s1-final (bit-shift-left s2-final 16))))

(def hashing-functions [sum-hash
                        builtin-hash
                        djb2-hash
                        adler32])

(defn add-element
  [dictionary element]
  (let [size (count dictionary)]
    (reduce (fn [dictionary f]
              (assoc dictionary
                (mod (f element) size)
                true))
            dictionary
            hashing-functions)))

(defn make-dictionary
  [elements]
  (let [initial-dictionary (vec (take 1000000 (repeat false)))]
    (reduce add-element
            initial-dictionary
            elements)))

(defn member?
  [dictionary element]
  (let [size (count dictionary)]
    (every? (fn [f]
              (let [the-hash (mod (f element) size)]
                (get dictionary the-hash)))
            hashing-functions)))


(defn classify-words
  [words]
  (with-open [reader (io/reader "/usr/share/dict/words")]
    (let [dictionary (make-dictionary (line-seq reader))]

      [(frequencies dictionary)
       (apply merge
              (for [word words]
                {word (member? dictionary word)}))])))


(classify-words ["foo"
                 "cheese"
                 "bargnasdf"
                 "crown"
                 "asdf"
                 "anchor"
                 "bleat"
                 "bar"])

(->> "Now then now then now then!!!???!!! Uggggeeeeyaaaaaa! Uggggeeeeeyaaaaaaa! Wots been appenin' now then now then now then, come sit on me lap that one, the 13 year old!!! Now then now them now then, as it appen's, boyz n girlz?!!!??? Now then now then now then!!!???!!! Uggggeeeeyaaaaaa! Uggggeeeeeyaaaaaaa! I'm fooked, me!!!???!! Jimmy Savile accused of sexually assaulting girls on ITV1 documentary"
     (re-seq #"\w+")
     (map clojure.string/lower-case)
     classify-words
     (map reverse)
     (sort-by first))
