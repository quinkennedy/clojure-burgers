(ns burgers.core
  (:gen-class)
  (:use clojure.set))

(def allIngredients {'patty 5 'bun 1 'cheese 1 'porkbelly 2 'potato 1 'oil 1})

(def allTools {'grill 100 'frier 300})

(def allRecipes {
  'burger '(patty bun grill 1)
  'fries '(potato frier oil 5)
  'bacon '(porkbelly grill 1)})

(def myItems {'grill 1 'frier 1 'patty 5 'bun 6 'potato 5 'oil 5})

(def myOrders ())

(def myMoney 0)

(def allMenu {'burger 10 'fries 3 'cheese-burger 13 'bacon-cheese-burger 17 'cheese-fries 5})

(def myMenu (set '(burger fries)))

;(defn serve [food orders items]
;  "Takes food to serve, a list of orders, and a list of items on-hand.
;  If the food specified is not in the list of items or in the orders,
;  then don't make any changes, otherwise remove the item, replace any tools,
;  and remove the order. returns [success orders items]"
;
;(defn toss [food items]
;  "removes the specified food from the items list, and replaces any
;  tools which were in use."
  

(defn cook [food recipes items]
  "creates the specified food iff the food is a key in the recipes
  and all the necessary items are available. returns the new items
  list [success items]"
  (let [recipe (get recipes food)
       ingredients (set (drop-last recipe))
       ingredientsOnHand (clojure.set/intersection ingredients (set (keys items)))
       foodCount (or (get items food) 0)
       itemsWithFood (conj (dissoc items food) [food (inc foodCount)])]
  (if 
    (or 
      (= recipe ()) 
      (not (= (count ingredients) (count ingredientsOnHand))))
    [false items]
    [true (into {} (for 
      [pair (filter 
        (fn [a] (< 0 (second a))) 
        (map 
          (fn [b] (if 
            (contains? ingredients (first b)) 
            [(first b) (dec (second b))] 
            b)) 
          itemsWithFood))] 
      pair))])))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println myItems)
  (println (cook 'burger allRecipes myItems)))

