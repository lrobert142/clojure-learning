(ns clojure-noob.core
  (:gen-class))
(use '[clojure.string :only [index-of]])

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


; Chapter 9: Concurrency

(defn bing-search
  "Searches for a term on Bing and prints the HTML of the page"
  [search-term]
  (println (slurp (str "https://www.bing.com/search?q=" search-term))))
;Fun fact, Google requires a user-agent or it will throw a 400/403 error
;Supposedly to stop bots scraping their search pages :/

(defn bing-search-future
  "Searches for a term on Bing and prints the HTML of that page using `future`"
  [search-term]
  (let [result (future (slurp (str "https://www.bing.com/search?q=" search-term)))]
    (println "Fetching results, please wait...")
    (println @result)))
;Create a "future" (referenced by `result`) which will run the search on another thread
;Then print a message about fetching results so the users knows to wait
;Then print the found results when we finally have them

;Define a delay that will be `force`d later
(def bing-example-search-delay
  (delay (do
           (println "New results found!!!")
           (slurp (str "https://www.bing.com/search?q=example")))))

(defn bing-search-delay
  "Searches \"example\" on Bing and prints the HTML of that page using a predefined `delay`"
  []
  (let [search-results bing-example-search-delay]
    (println "Search **started**, please wait...")
    (println (force search-results))))
;let the search results = our custom delay
;Show a message to the user to let them know we are loading and...
;`force` the delay which will show a message (only the first time it is called)...
;then load and return the contents of a URL

(def search-promise (promise))

(defn bing-search-promise
  "Searches for a term on Bing and prints the HTML of that page using `promise`"
  [search-term]
  (do
    (println "Searching...")
    (deliver search-promise (slurp (str "https://www.bing.com/search?q=" search-term)))
    (println @search-promise)))
;Show a message to let the user know we are about to start searching
;`deliver` the promise to actual make the request
;`println` the result of the promise (what we retrieved from the URL)

(defn search-future
  "Searches for a term on the provided search engine and returns a future of the page's HTML"
  [search-engine search-term]
  (future (slurp (str "https://" search-engine "/search?q=" search-term))))

(defn search
  "Searches for a term using all the search engines provided and returns a vector of mapped results"
  [search-term search-engines]
  (reduce (fn [results search-engine]
            (if (.contains search-engine "google")
              (do
                (println "Cannot search via Google, it will 403")
                results)
              (into results (let [result (search-future search-engine search-term)]
                              (println (str "Searching Engine: " search-engine))
                              (list {:engine search-engine :html @result})))
              ))
          []
          search-engines))
;`reduce` each of search-engines into a new vector containing results which...
;if the URL contains "google", display an error and leave the results unaltered, otherwise it...
;lets a new `future`, prints a message to the user to let them know we are searching and...
;creates and returns a new list of maps where each map...
;has the name of the :engine (var) and the retrieved value of the :result (@result) from the URL
;;
;NOTE: As @result is a `future`, it will asynchronously make these URL calls and append when
;the data comes back

(defn search-links
  "Search for a term using the provided search engines and returns a vector of URLs from those pages"
  [search-term search-engines]
  (reduce (fn [urls result]
            (println (subs (:html result)
                           (+ (index-of (:html result) "<a href=\"http") (count "<a href=\"") )
                           (index-of (:html result) "\"" (+ (index-of (:html result) "<a href=\"http") (count "<a href=\"")))
                           )))
          []
          (search search-term search-engines)))
;This gets ONLY the first link. ;TODO Get all links?


; Chapter 10: Atoms & Refs

(def ch10-atom (atom {:number 0}))

(defn atom-plus-three
  "Increments an atom 3 times and returns then derefs it"
  [the-atom]
  (loop [index 0]
    (if (< index 3)
      (do
        (swap! the-atom update-in [:number] inc)
        (recur (inc index)))
      @the-atom)))

(defn quote-future
  "Creates and returns a `future` that will get the word count of a quote from http://www.braveclojure.com/random-quote"
  []
  (future (count (slurp "http://www.braveclojure.com/random-quote"))))

(defn quote-word-count
  "Creates and returns the total word count of quotes from http://www.braveclojure.com/random-quote"
  [num-of-quotes]
  (loop [index 0 the-atom (atom {:word-count 0})]
    (if (< index num-of-quotes)
      (do
        (swap! the-atom update-in [:word-count]
               #(let [quote-count (quote-future)]
                  (+ % @quote-count)))
        (recur (inc index) the-atom))
      @the-atom)))
;TODO