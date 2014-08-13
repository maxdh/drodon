(ns drodon.core)

(def objects '(whiskey-bottle bucket frog chain))

(def game-map (hash-map
   'living-room '((you are in the living room
                   of a wizards house - there is a wizard
                   snoring loudly on the couch -)
                  (west door garden)
                  (upstairs stairway attic))
   'garden '((you are in a beautiful garden -
              there is a well in front of you -)
             (east door living-room))
   'attic '((you are in the attic of the
             wizards house - there is a giant
             welding torch in the corner -)
            (downstairs stairway living-room))))

(def object-locations (hash-map
                       'whiskey-bottle 'living-room
                       'bucket 'living-room
                       'chain 'garden
                       'frog 'garden))

(def location 'living-room)

;;convert to string then take out of quotes - 'name' gets rid of namespace
(defn clean-print [list]
  (map (fn [x] (symbol (name x))) list))

(defn describe-location [location game-map]
  (first (location game-map)))

(defn describe-path [path]
  `(there is a ~(second path) going ~(first path) from here -))

(defn describe-paths [location game-map]
  (apply concat (map describe-path (rest (get game-map location)))))

;; lookup the location value of the object in the object-locations hashmap and see if it equals current location
(defn is-at? [obj loc obj-locations-map]
  (= (obj obj-locations-map) loc))


;; given a list of objects, filter them to only keep the ones in the current locatation.
;; then map each of these objects into a string and join them all together.
(defn describe-floor [loc objs obj-loc]
  (apply concat (map (fn [x]
                       `(you see a ~x on the floor -))
                     (filter (fn [x] 
                               (is-at? x loc obj-loc)) objs))))
;;describe the current room, paths and items
(defn look []
  (clean-print (concat (describe-location location game-map)
          (describe-paths location game-map)
          (describe-floor location objects object-locations))))

(defn walk-direction [direction]
  (let [next (first (filter (fn [x] (= direction (first x)))
                            (rest (location game-map))))]
    (cond next 
          (do (def location `~(nth next 2)) (look))
          :else '(you cannot go that way -))))



(defmacro def-action [& rest] 
  `(defmacro ~@rest))

;; makes walk into a function, and inserts a quote before 'direction' so you dont have to
(def-action walk [direction]
  `(walk-direction '~direction))

;; change the location of 'object' to be 'body'
(defn pickup-object [object]
  (cond (is-at? object location object-locations)
        (do
          (def object-locations (assoc object-locations object 'body))
          `(you are now carrying the ~object))
        :else '(you cannot get that!)))

(def-action pickup [object] 
  `(clean-print (pickup-object '~object)))

;; filter all objects, returning only the ones at 'body'
;; is x at body in object-locations?
(defn inventory []
  (filter (fn [x] (is-at? x 'body object-locations)) objects))

;;look at clojure docs example, returns true if object is in inventory
(defn have? [object]
  (some #(= object %) (inventory)))

(defn discard-object [object]
  (cond (have? object)
        (do
          (def object-locations (assoc object-locations object location))
          `(you dropped the ~object))
        :else '(you cannot get that!)))

(def-action discard [object]
  `(clean-print (discard-object '~object)))

(def chain-welded false)

(def bucket-filled false)

(def-action game-action [command subj obj place & args]
  `(def-action ~command [subject# object#]
     `(clean-print (cond (and (= location '~'~place)
                             (= '~subject# '~'~subj)
                             (= '~object# '~'~obj)
                             (have? '~'~subj))
                        ~@'~args
                        :else '(i cannot ~'~command like that -)))))

(game-action weld chain bucket attic
   (cond (and (have? 'bucket) (def chain-welded true))
              '(the chain is now securely welded to the bucket -)
         :else '(you do not have a bucket -)))

(game-action dunk bucket well garden
             (cond chain-welded 
                   (do (def bucket-filled true)
                       '(the bucket is now full of water))
                   :else '(the water level is too low to reach -)))

(game-action splash bucket wizard living-room
             (cond (not bucket-filled) '(the bucket has nothing in it -)
                   (have? 'frog) '(the wizard awakens and sees that you stole
                                       his frog -
                                       he is so upset he banishes you to the
                                       netherworlds - you lose! the end -)
                   :else '(the wizard awakens from his slumber and greets you
                               warmly -
                               he hands you the magic low-carb donut - you win!
                               the end -)))


;; TODO
;; Add max. inventory size?
;; Add help fn to list available actions
