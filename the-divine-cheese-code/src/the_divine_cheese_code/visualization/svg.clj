(ns the-divine-cheese-code.visualization.svg
  (:require [clojure.string :as s])
  (:refer-clojure :exclude [min max]))
;min/max is excluded here because we are defining our functions below

(defn comparator-over-maps
  "Returns a function that applies the `comparison-fn` over the passed in maps' values"
    [comparison-fn ks]
    (fn [maps]
      (zipmap ks
              (map (fn [k] (apply comparison-fn (map k maps)))
                                       ks))))
;Return a function that...
;Creates a zipmap (joins two vectors into a map) of...
;The result of applying the `comparison-fn` TO EACH KEY...
;Which itself takes a map param that contains all key=>value pairs for ONLY THAT KEY

(def min (comparator-over-maps clojure.core/min [:lat :lng]))
(def max (comparator-over-maps clojure.core/max [:lat :lng]))

(defn translate-to-00
  "Translates a set of coordinates to values relative to (0,0) on a graph"
  [locations]
  (let [mincoords (min locations)]
    (map #(merge-with - % mincoords) locations)))
;`merge-with` merges two maps together by applying a fn to each value
;The first param is the map to merge into, the second is the map of values to
;(in this case) subtract from the values in the first map

(defn scale
  "Scales a set of coordinates to better represent the absolute max value of the graph"
  [width height locations]
  (let [maxcoords (max locations)
        ratio {:lat (/ height (:lat maxcoords))
               :lng (/ width (:lng maxcoords))}]
    (map #(merge-with * % ratio) locations)))
;In this case, `merge-with` takes all locations and the ratio for each location
;and multiplies them together (per value, of course)

(defn latlng->point
  "Convert lat/lng map to comma-separated string"
  [latlng]
  (str (:lat latlng) "," (:lng latlng)))

(defn points
  "Given a seq of lat/lng maps, return string of points joined by space"
  [locations]
  (s/join " " (map latlng->point locations)))

(defn line
  "Returns the polyline element used in an SVG"
  [points]
  (str "<polyline points=\"" points "\" />"))
;In this case, points only needs to be space-separated for the SVG to display

(defn transform
  "Translates the locations to be ready to print on a graph"
  [width height locations]
  (->> locations
       translate-to-00
       (scale width height)))

(defn xml
  "svg 'template', which also flips the coordinate system"
  [width height locations]
  (str "<svg height=\"" height "\" width=\"" width "\">"
       ;; These two <g> tags change the coordinate system so that
       ;; 0,0 is in the lower-left corner, instead of SVG's default
       ;; upper-left corner
       "<g transform=\"translate(0," height ")\">"
       "<g transform=\"rotate(-90)\">"
       (-> (transform width height locations)
           points
           line)
       "</g></g>"
       "</svg>"))