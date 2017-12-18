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

;In this basic example, inventory is just a hash-map of :item-name and quantity
(defn generate-player
  "Creates a player with basic details"
  [name current-health max-health inventory]
  {:name name
   :current-health current-health
   :max-health max-health
   :inventory inventory})

;First player, weakened with no inventory
(def player1 (ref (generate-player "Player One" 15 40 {})))
;Second player, full health with a single healing potion
(def player2 (ref (generate-player "Player Two" 40 40 {:healing-potion 1})))

(defn heal-player
  "Heals a player's hit points back to full, and returns the healed player"
  [healer being-healed]
  (dosync
    (alter being-healed update-in [:current-health] +
           (- (:max-health @being-healed) (:current-health @being-healed)))
    (alter healer update-in [:inventory :healing-potion] dec))
  @being-healed)

; CH 13

(defmulti full-moon-behavior (fn [were-creature] (:were-type were-creature)))
(defmethod full-moon-behavior :wolf
    [were-creature]
    (str (:name were-creature) " will howl and murder"))
(defmethod full-moon-behavior :simmons
    [were-creature]
    (str (:name were-creature) " will encourage people and sweat to the oldies"))
; :default will be used only if none of the above match
(defmethod full-moon-behavior :croc
  [were-creature]
  (str (:name were-creature) " will swim around and eat people"))
(defmethod full-moon-behavior :default
  [were-creature]
  (str (:name were-creature) " will stay up all night fantasy footballing"))

; To call:
; (full-moon-behavior {:were-type :wolf :name "Rachel from next door"})

(defprotocol Psychodynamics
  "Plumb the inner depths of your data types"
  (thoughts [x] "The data type's innermost thoughts")
  (feelings-about [x] [x y] "Feelings about self or other"))
;What we are defining, but not how it is implemented

;(extend-type java.lang.String
;  Psychodynamics
;  (thoughts [x] (str x " thinks, 'Truly, the character defines the data type'")
;    (feelings-about
;      ([x] (str x " is longing for a simpler way of life"))
;      ([x y] (str x " is envious of " y "'s simpler way of life")))))
; Protocols are similar to - but not the same as - interfaces, it seems; you define what should occur
; but not how until you extend another object (add more functionality like an interface does)

(extend-protocol Psychodynamics
  java.lang.String
  (thoughts [x] "Truly, the character defines the data type")
  (feelings-about
    ([x] "longing for a simpler way of life")
    ([x y] (str "envious of " y "'s simpler way of life")))

  java.lang.Object
  (thoughts [x] "Maybe the Internet is just a vector for toxoplasmosis")
  (feelings-about
    ([x] "meh")
    ([x y] (str "meh about " y))))
; Allows us to implement this protocol (interface) on multiple objects

(defprotocol WereCreature
  (full-moon-behavior [x]))

(defrecord WereSimmons [name title]
  WereCreature
  (full-moon-behavior [x]
    (str name " will encourage people and sweat to the oldies")))
; To call: (full-moon-behavior (->WereSimmons "Bob" "Lead Cleaner"))

(defprotocol Greetings
  "Allows objects to greet others"
  (hello [x] "Says a friendly hello")
  (hi [x] [x y] "A more informal greeting"))

(extend-type java.lang.String
  Greetings
  (hello [x] (str "Hello, " x)
    (hi
      ([x] (str "Hi there, " x)))))
; Although this is here for the exercise, it will be overwritten by the items below

(extend-protocol Greetings
  java.lang.String
  (hello [x] (str "Just wanted to say hello, " x))
  (hi
    ([x]  (str "Well hi, " x))
    ([x y] (str x " says hi to " y)))

  java.lang.Object
  (hello [x] "Generic Hello World!")
  (hi
    ([x] "Generic Hi!")
    ([x y] (str "Generic Hi to you, " y))))
