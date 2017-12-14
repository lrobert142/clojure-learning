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

;(glitter-filter 2 (mapify (parse (slurp filename))))
(defn glitter-filter
  "Returns a seq of records that have a 'glitter index' greater than the minimum amount"
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

; CH04 Exercises

; Solution sourced from https://stackoverflow.com/questions/27914026/find-if-a-map-contains-multiple-keys
(defn validate
  "Validates that a record has the passed in keys"
  [validation-keys record]
  (every? record validation-keys))

(defn convertToCsv
  "Converts a map of records to a CSV string"
  [records]
  (reduce (fn [output record]
            (str (clojure.string/join ","  [(:name record) (:glitter-index record)]) "\n" output))
          ""
          records))

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

;(updateSuspectsFile (append (mapify (parse (slurp filename))) {:name "Bob" :glitter-index 0} ) )
(defn updateSuspectsFile
  "Updates the file of suspects, converting to the CSV format if needed"
  [suspects]
  (if (string? suspects)
    (spit filename suspects)
    (updateSuspectsFile (convertToCsv suspects))))

;(append (mapify (parse (slurp filename))) {:name "Bob" :glitter-index 0} )
(defn append
  "Appends a new suspect to a list of current suspects (in-memory only)"
  ([current-suspects new-suspect-name new-suspect-index]
   (append current-suspects {:name new-suspect-name
                             :glitter-index new-suspect-index}))
  ([current-suspects new-suspect]
   (if (validate vamp-keys new-suspect)
     (into current-suspects
           (vector {:name (:name new-suspect)
                    :glitter-index  (:glitter-index new-suspect)}))
     (println "Invalid Suspect Data"))))

;(removeSuspect (mapify (parse (slurp filename))) "Edward Cullen")
(defn removeSuspect
  "Removes a suspect with the given name from the list (in-memory only)"
  [current-suspects new-suspect-name]
  (remove (fn [suspect]
            (= (:name suspect) new-suspect-name))
          current-suspects))
