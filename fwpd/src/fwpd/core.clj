(ns fwpd.core)
; The file we will look in
(def filename "suspects.csv")
; The keys used in the file
(def vamp-keys [:name :glitter-index])

(defn str->int
  "Converts a String to an Integer"
    [str]
    (Integer. str))

; Conversion function for each vamp-key we are looking for
(def conversions {:name identity
                    :glitter-index str->int})

(defn convert
  "Takes a vamp-key and value and returns the converted value"
    [vamp-key value]
    ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  "Returns a seq of records that have a 'glitter index' greater than the minimum amount"
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn glitter-names
  "Returns a seq of names that have a glitter index higher than the specified amount"
  [minimum-glitter records]
  (reduce (fn [names mapped-record]
            (into names (vector (:name mapped-record))))
          []
          (glitter-filter minimum-glitter records)))
; Starting with an empty vector...
; And using all the records we find using #glitter-filter...
; Put the name (as a vector for full name, not individual letters)...
; From the individual record into the names vector...
; And repeat until we have no more records