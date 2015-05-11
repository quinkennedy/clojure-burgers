(ns burgers.core
  (:gen-class)
  (:require clojure.set
            clojure.string))

(def allIngredients {'patty 5 'bun 1 'cheese 1 'porkbelly 2 'potato 1 'oil 1})

(def allTools {'grill 100 'frier 300})

(def allRecipes {
  'burger '(patty bun grill 1)
  'fries '(potato frier oil 5)
  'bacon '(porkbelly grill 1)})

(def myItems {'grill 1 'frier 1 'patty 5 'bun 6 'potato 5 'oil 5})

(def myOrders ())

(def numCustomers 1)

(def myMoney 0)

(def allMenu {'burger 10 'fries 3 'cheese-burger 13 'bacon-cheese-burger 17 'cheese-fries 5})

(def myMenu (set '(burger fries)))
  
(defn execOn [f hm k]
  (let [n (or (k hm) 0)]
  (conj (dissoc hm k) [k (f n)])))

(defn getOrder [menu]
  (nth menu (int (* (count menu) (rand)))))

(defn removeEmpty [items]
  "Removes all items from the list that have <= 0
  amount. 'items' can be a hash-map '{a 1 b 2}
  or a list of duples '([a 1] [b 2]).
  returns a hash-map"
  (into {} (for
    [pair (filter
      #(< 0 (second %))
      items)]
    pair)))

(defn extract [food items recipes tools]
  "removes one of the specified food from items
  This is provided as shared functionality for SERVE and TOSS"
  (let [foodCount (food items)
        hasFood (not (or (= nil foodCount) (= 0 foodCount)))
        recipe (food recipes)
        ingredients (set (drop-last recipe))]
  [hasFood
  (if hasFood
    (removeEmpty (map #(if 
                         (and (contains? ingredients (first %)) 
                              (contains? tools (first %))) 
                         [(first %) (inc (second %))]
                         %) 
                      (merge 
                        (zipmap (keys tools) (repeat 0)) 
                        (execOn dec items food))))
    items)]))

(defn toss [food items recipes tools]
  (extract food items recipes tools))

(defn removeFirst [k l]
  (let [[n m] (split-with (partial not= k) l)] (concat n (rest m))))

(defn serve [food items orders recipes tools]
  (let [hasOrder (contains? (set orders) food)
        ordersNoFood (removeFirst food orders)
	itemsNoFood (extract food items recipes tools)]
  (if (and hasOrder (first itemsNoFood))
    [true (second itemsNoFood) ordersNoFood]
    [false items orders])))
      
(defn cook [food recipes items]
  "creates the specified food iff the food is a key in the recipes
  and all the necessary items are available. returns the new items
  list [success items]"
  (let [recipe (food recipes)
       ingredients (set (drop-last recipe))
       ingredientsOnHand (clojure.set/intersection ingredients (set (keys items)))
       foodCount (or (food items) 0)
       itemsWithFood (execOn inc items food)]
  (if 
    (or 
      (= recipe ()) 
      (not (= (count ingredients) (count ingredientsOnHand))))
    [false items]
    [true (removeEmpty
      (map 
        (fn [b] (if 
          (contains? ingredients (first b)) 
          [(first b) (dec (second b))] 
          b)) 
        itemsWithFood))])))

(def quit false)

(defn doQuit []
  (def quit true))

(defn doGetOrder []
  (getOrder myMenu))

(defn doToss [food]
  (toss food myItems allRecipes allTools))

(defn doCook [food]
  (cook food allRecipes myItems))

(defn doServe [food]
  (serve food myItems myOrders allRecipes allTools))

(defn doPrint []
  (println myItems)
  (println numCustomers)
  (println myOrders))


(def input {"quit" doQuit "exit" doQuit "getOrder" doGetOrder "serve" doServe "cook" doCook "make" doCook "toss" doToss "print" doPrint})

(defn -main [& args]
  (println "finished initialization procedures")
  (while (not quit) 
    (let [in (clojure.string/split (read-line) #" ")]
    ((get input (first in)))))
  (println "quitting"))
;
;  "I don't do a whole lot ... yet."
;  [& args]
;  (println myItems)
;  (println (cook 'burger allRecipes myItems))
;  (println (extract 'burger (second (cook 'burger allRecipes myItems)) allRecipes allTools))
;  (println (serve 'burger (second (cook 'burger allRecipes myItems)) myOrders allRecipes allTools))
;  (println (serve 'burger (second (cook 'burger allRecipes myItems)) '(burger burger) allRecipes allTools)))
;
