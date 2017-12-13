(ns clojure-noob.core
  (:gen-class))


; Chapter 1: Intro

(defn -main
  "Prints a message and exits. This is the main function executed when executing `lein run`"
  [& args]
  (println "I'm a little teapot!"))


; Chapter 3: Basics

(defn greet
  "I greet someone"
  ([name]
    (println (str "Hello, "  name)))
  ([]
    (greet "No-Name")))

(defn pets_vector
  "Returns a vector of pets"
  ([]
   ["Scramasax" "Grumpy" "Hiccup"]))

(defn pets_list
  "Returns a list of pets"
  ([]
   '("Scramasax" "Grumpy" "Hiccup")))

(defn pets_set
  "Returns a set of pets"
  ([]
   #{"Scramasax" "Grumpy" "Hiccup"}))

(defn pets_hash-map
  "Returns a hash-map of pets and their weight"
  ([]
   {:grumpy 300 :hiccup 200 :scramasax 3000}))

(defn pet_weight
  "Returns the weight of a pet"
  ([key]
   ((pets_hash-map) key))
  ([]
    (pet_weight
      (first (keys (pets_hash-map))))))

(defn add_100
  "Adds 100 to the input number"
  ([number]
    (+ number 100)))

(defn dec_maker
  "Creates a custom decrementor"
  ([dec-by]
    #(- % dec-by)))

; Decreases the value of a number by 9
(def dec9 (dec_maker 9))

(defn mapset
  "Takes in a vector and returns it as a set"
  ([f coll]
   (set (map f coll))))


;Generic Asymmetric Body Parts
(def asym-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))

(def base-alien-body-parts [{:name "head" :size 3}
                      {:name "eye-01" :size 1}
                      {:name "ear-01" :size 1}
                      {:name "mouth" :size 1}
                      {:name "nose" :size 1}
                      {:name "neck" :size 2}
                      {:name "shoulder-01" :size 3}
                      {:name "upper-arm-01" :size 3}
                      {:name "chest" :size 10}
                      {:name "back" :size 10}
                      {:name "forearm-01" :size 3}
                      {:name "abdomen" :size 6}
                      {:name "kidney-01" :size 1}
                      {:name "hand-01" :size 2}
                      {:name "knee-01" :size 2}
                      {:name "thigh-01" :size 4}
                      {:name "lower-leg-01" :size 3}
                      {:name "achilles-01" :size 1}
                      {:name "foot-01" :size 2}])

(defn alien-parts
  "Returns a set of additional alien limbs"
  [part]
  (let [parts [part]]
    (loop [iteration 1 new-parts parts]
      (if (> iteration 5)
        new-parts
        (do
          (let [new-parts (conj new-parts {:name (clojure.string/replace (:name part) #"-01" (str "-" (format "%02d" iteration) ))
                                           :size (:size part)})]
            (recur (inc iteration) new-parts))
          )))
    ))

(defn alien-body-parts
  "Expects a seq of maps that have a :name and :size. Creates 5 limb variations for each limb"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set (alien-parts part))))
          []
          asym-body-parts))

(defn generic-parts
  "Returns a number of additional limbs up to the limit specified by the `limit` param"
  [part limit]
  (let [parts [part]]
    (loop [iteration 1 new-parts parts]
      (if (> iteration limit)
        new-parts
        (do
          (let [new-parts (conj new-parts {:name (clojure.string/replace (:name part) #"-01" (str "-" (format "%02d" iteration)))
                                           :size (:size part)})]
            (recur (inc iteration) new-parts))
          )))
    ))

(defn generic-body-parts
  "Expects a seq of maps that have a :name and :size. Creates `limit` number of limbs"
  [asym-body-parts limb-limit]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set (generic-parts part limb-limit))))
          []
          asym-body-parts))


; Chapter 4: Abstractions

(def sum #(reduce + %))
(def avg #(/ (sum %) (count %)))
(defn stats
  [numbers]
  (map #(% numbers) [sum count avg]))