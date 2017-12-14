(ns the-divine-cheese-code.core
  (:require [clojure.java.browse :as browse]
            [the-divine-cheese-code.visualization.svg :refer [xml]])
  (:gen-class))
;Import browse library and our custom functions (referred to via XML)

(def heists [{:location "Cologne, Germany"
              :cheese-name "Archbishop Hildebold's Cheese Pretzel"
              :lat 50.95
              :lng 6.97}
             {:location "Zurich, Switzerland"
              :cheese-name "The Standard Emmental"
              :lat 47.37
              :lng 8.55}
             {:location "Marseille, France"
              :cheese-name "Le Fromage de Cosquer"
              :lat 43.30
              :lng 5.37}
             {:location "Zurich, Switzerland"
              :cheese-name "The Lesser Emmental"
              :lat 47.37
              :lng 8.55}
             {:location "Vatican City"
              :cheese-name "The Cheese of Turin"
              :lat 41.90
              :lng 12.45}])

(defn url
  "Generates the file URL of the output file to use in the web browser"
  [filename]
  (str "file:///"
       (System/getProperty "user.dir")
       "/"
       filename))

(defn template
  "Displays the template SVG created via out custom functions"
  [contents]
  (str "<style>polyline { fill:none; stroke:#5881d8; stroke-width:3}</style>"
       contents))

(defn -main
  "The main function to execute for this project"
  [& args]
  (let [filename "map.html"]
    (->> heists
         (xml 50 100)
         template
         (spit filename))
    (browse/browse-url (url filename))))
;`->>` tells Clojure to pass the value of the previous item to the next function
;and allows us to chain values without having to wrap everything in brackets over
;and over again (which can get confusing)
;; !!! MIND BLOWN !!!
;In this case, it means get the heists vector, **append** that vector to the
;**end** of (xml 50 100) --> which would call (xml 50 100 heists) --> then call
;template with this value and so on. This means fully expanded it would look like:
;(spit filename (template (xml 50 100 (heists))))