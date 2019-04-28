(ns ShipSearch.core
  (:gen-class)
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, Welcome to Assignment 2!"))

(def read-map-from-file
      "takes a map file and reads it in; returning a state with a zero length path"
      (memoize (fn [file]

      ;reads in the given file and then splits it into a vector by the end of line character
      ;the vector is then stored in a hashmap under the key :map and an empty vector representing the path is stored under the key :path
      {:map (vec (clojure.string/split (slurp file) #"[\n]"))
       :path []}
)))

(def start
      "takes a state and returns the coordinates of the starting port"
      (memoize (fn [state]

      ;i represents the row index (y coordinate) and m represents the map element of the given state
      (loop [i 0
             m (state :map)]
            ;if the current row contains the character 'S'...
            (if (.contains (nth m 0) "S")

                ;then return a vector containg the index of the charcter 'S' in the curent row, and the inedx of the current row (e.g [x y])
                [(.indexOf (nth m 0) "S") i]

                ;else perform the loop again with a row index incremented by 1 and a map with the first row removed
                (recur (inc i) (rest m))))
)))

(def goal
      "takes a state and returns the coordinates of the goal port"
      (memoize (fn [state]
      ;i represents the row index (y coordinate) and m represents the map element of the given state
      (loop [i 0
             map (state :map)]
            ;if the current row contains the character 'G'...
            (if (.contains (nth map 0) "G")

                ;then return a vector containg the index of the charcter 'G' in the curent row, and the inedx of the current row (e.g [x y])
                [(.indexOf (nth map 0) "G") i]

                ;else perform the loop again with a row index incremented by 1 and a map with the first row removed
                (recur (inc i) (rest map))))
)))

(def valid
      "takes a state and returns a list of all valid moves from the current position of the ship (e.g. empty space or the goal state)"
      (memoize (fn [state]

      ;for each row in the map of the given state..
      (for [[y row] (map-indexed vector (state :map))
            ;and for each character in the row..
      	    [x val] (map-indexed vector row)

            ;when the current charcter is..
	          :when (and
                    ;equal to a 'space' OR a 'G'
                    (or (= (compare \  val) 0) (= (compare \G val) 0))
                    ;AND the coordinates of the current character are not already in the path of the given state
                    (= (.contains (state :path) [x y]) false))]

           ;return the coordinates of the current character
	         [x y])
)))

(def position
      "takes a state and returns the current coordinates of the ship"
      (memoize (fn [state]

      ;if the path of the given state is empty...
      (if (empty? (state :path))

          ;return the position of the starting port of the given state
          (start state)

          ;else return the last coordinate in the path of the given state
          (last(state :path)))
)))

(def cost
      "takes a state and returns the cost of the state"
      (memoize (fn [state]

      ;return the length of the path of the given state
      (count (state :path))
)))

(def heuristic
      "takes a state and computes the heuristic value using the Euclidean distance metric"
      (memoize (fn [state]

      ;store the x and y coordinates of the ships current position and the goal port
      (let [xShip (first(position state))
      	    yShip (last(position state))
	          xGoal (first(goal state))
	          yGoal (last(goal state))]

      ;use the above coordinates to calculate the Euclideans distance metric
	    (Math/sqrt (+ (Math/pow (- xShip xGoal) 2) (Math/pow (- yShip yGoal) 2))))
)))

(def expand
      "takes a state and returns a list of new valid states obtained by extending the length
      of the given state's path by one in all valid directions"
      (memoize (fn [state]

      ;store the x and y coordinates of the ships current position
      (let [xCoor (first(position state))
      	    yCoor (last(position state))
            ;create a vector of coordinates that can potentially be expanded to from the given state
	          potentialStates [[(inc xCoor) yCoor] [(dec xCoor) yCoor] [xCoor (inc yCoor)] [xCoor (dec yCoor)]]
            ;filter out the invalid cordinates from the list of potential expansions to create a vector of valid coordinate expansions
            validExpansions (filter #(.contains (valid state) %) potentialStates)]

            ;for each valid coordinate expansion...
            (for [e validExpansions]
                 ;create a new state with a map the same as the given state, and a path as the path of the given state with the current expansion coordinate added to the end.
                 {:map (state :map) :path (conj (state :path) e)}))
)))

(defn print-state
      "takes a state and pretty prints it to the console"
      [state]

      ;a loop that executes the same number of times as the number of rows in the map of the given state (y represents the y coordinate)
      (dotimes [y (count (state :map))]
          ;a loop that executes the same number of times as the number of characters in the current row (x represents the x coordinate)
          (dotimes [x (count (nth (state :map) y))]
              ;if the current coordinates are in the path oh the given state but are not the coordiantes of the goal port..
              (if (and (.contains (state :path) [x y]) (not= [x y] (goal state)))

                  ;print a '.' to the console to represent the path of the ship
                  (print ".")

                  ;else print out the character on the map at the current coordinates
                  (print (nth (nth (state :map) y) x)))) (println))
)

(def verbose false)

(defn best-first
  "function that attempts to find the path of the least cost using the Best-first search algorithm"
  [file]

  ;store the intial state of the read in file
  (let [s (read-map-from-file file)
        ;create a frontier that stores a state and the heuristic value of the state; ordered by heuristic value (min to max)
        frontier (priority-map s (heuristic s))
        ;create a vector to store all the coordinates in the frontier and/or the coordinates that have been expanded and are no longer in the froniter
        beenSeen [(start s)]]

        ;f represents the current frontier; e represents the number of expansions that have been performed, and bs represents the vector of coordinates that have current 'been seen'
        (loop [f frontier e 0 bs beenSeen]
          ;if there are no elements in the frontier..
              (if (= (count f) 0)

                  ;no path has been found from the start port to the goal port
                  (println "No path found")

                  ;else get the state with the smallest heuristic value from the frontier
                  (let [smallest (key (peek f))]

                    (do
                       (when verbose (print-state smallest))

                       ;if the ship in the current state is in the same position as the goal port..
                       (if (= (position  smallest) (goal smallest))

                           ;pretty print the current state
                           (do (print-state smallest)
                               ;print out the cost of the current state and the number of expansions required to get to this state
                               (println "Cost: " (cost smallest) "Expansions: " e))

                           ;else create a list of expansions possible from the current state. This is done by calling the expand function on the current state and then removing coordinates that have already been expanded to.
                           (let [ex (filter #(= (.contains bs (position %)) false) (expand smallest))
                                 ;store each state expansion with their heuristic value
                                 expanded (for [item ex] [item (heuristic item)])
                                 ;remove the current state from the frontier and replace it with the new expanded states
                                 newfrontier (into (pop f) expanded)]

                                 ;perform the loop again on the editted frontier; with the number of expansions incremented by 1, and with the coordinates that were expanded to added to the 'been seen' vetcor
                                 (recur newfrontier (inc e) (concat bs (vec (map #(position (first %)) expanded)))))))))))
)

(defn a-star
  "function that attempts to find the path of the least cost using the A* search algorithm"
  [file]

  ;store the intial state of the read in file
  (let [s (read-map-from-file file)
        ;create a frontier that stores a state and the f-value (cost + heuristic) of the state; ordered by heuristic value (min to max)
        frontier (priority-map s (+ (cost s) (heuristic s)))
        ;create a vector to store all the coordinates in the frontier and/or the coordinates that have been expanded and are no longer in the froniter
        beenSeen [(start s)]]

        ;f represents the current frontier; e represents the number of expansions that have been performed, and bs represents the vector of coordinates that have current 'been seen'
        (loop [f frontier e 0 bs beenSeen]
              ;if there are no elements in the frontier..
              (if (= (count f) 0)

                  ;no path has been found from the start port to the goal port
                  (println "No path found")

                  ;else get the state with the smallest f-value from the frontier
                  (let [smallest (key (peek f))]

                    (do
                       (when verbose (print-state smallest))

                       ;if the ship in the current state is in the same position as the goal port..
                       (if (= (position  smallest) (goal smallest))

                           ;pretty print the current state
                           (do (print-state smallest)
                               ;print out the cost of the current state and the number of expansions required to get to this state
                               (println "Cost: " (cost smallest) "Expansions: " e))

                           ;else create a list of expansions possible from the current state. This is done by calling the expand function on the current state and then removing coordinates that have already been expanded to.
                           (let [ex (filter #(= (.contains bs (position %)) false) (expand smallest))
                                 ;store each state expansion with their f-value
                                 expanded (for [item ex] [item (+ (cost item) (heuristic item))])
                                 ;remove the current state from the frontier and replace it with the new expanded states
                                 newfrontier (into (pop f) expanded)]

                                 ;perform the loop again on the editted frontier; with the number of expansions incremented by 1, and with the coordinates that were expanded to added to the 'been seen' vetcor
                                 (recur newfrontier (inc e) (concat bs (vec (map #(position (first %)) expanded)))))))))))
)





































