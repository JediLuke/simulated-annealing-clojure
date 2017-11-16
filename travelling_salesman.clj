(print "\nInitiate: simulated annealing.\n\n")

;
; Initial setup


(def initialState 72)
(def initialTemp 10)

; returns a function which will calculate the y value for any x input value on the line y = mx + b
(defn lineFunction [m b]
   (fn [x] (+ (* m x) b)))

; The following two parameters must be defined as *functions*
(def phi (fn [x] (* x 2)))          ; "energy" or "cost function"
(def stateFn (lineFunction 2 3))    ; function which takes in a state and returns the next state


;
; These functions are used in the higher level algorithm
(defn calcEnergy [state, temp] (phi state))     ; Returns a function which calculates the energy of a state according to some energy function phi
(defn nextState [state] (stateFn state))        ; Returns a function which transforms states according to the initialization function provided

; Compute the probability of moving from one state to the next, given the Temperature of the system
(defn probMove [state, nextState, tempConstant]
   ; Exponent function
   (defn exp [x n]
      (reduce * (repeat n x)))
   ; This function is a Boultzmann distribution
   (exp [2.718 (/
      (- nextState state)
      (* tempConstant))]))


;
; Simulated annealing

; Start a SA search from a given start state, at a given start temp, cooling down until tempMin has been reached
(defn simAnneal [state temp tempMin]
   
   ; Base case - return state when system "cools down" to the point where search stops, i.e. temp <= tempMin
   (if (<= temp tempMin)
      (do
         (print "\n\n\nSystem cooldown achieved.\n")
         state)
      ; else -  Recursive case
      (let [curEnergy (calcEnergy state temp)]                          ; Compute current energy level for current state
         (print "Current state:" state "Current temp:" temp "\n")
         ;(print "CurEnergy:" curEnergy "\n")
         (let [nextState (nextState state)]                             ; Apply the state transform function to the current state
            ;(print "nextState:" nextState "\n")
            (let [nextStateEnergy (calcEnergy nextState temp)]          ; Compute energy for next state
               ;(print "nextStateEnergy:" nextStateEnergy "\n")
               (let [deltaE (- nextStateEnergy curEnergy)]              ; Calculate deltaE
                  (if (compare deltaE 0)                                ; if (deltaE > 0) ; If next state is more optimal
                     (simAnneal nextState (- temp 1) tempMin)                 ;     return nextState
                     ((if (probMove state nextState temp) > 0)          ; elsif boulztMan(params) > rand(0,1)
                        (simAnneal nextState (- temp 1) tempMin)              ;     Move to worse state with probability dependent on temperature
                        (simAnneal state     (- temp 1) tempMin))                 ; otherwise (i.e. prob of new state not high enough), return currentState
                  )
               )
            )
         )
      )
   )
)


; For tMax to tMin
(print "Final state: " (simAnneal initialState initialTemp 0))

   
;-----------------------------------------------------------------------

;  References
;  ----------

;     Simulated annealing

;     - https://www.gnu.org/software/gsl/manual/html_node/Simulated-Annealing-algorithm.html
;     - https://www.youtube.com/watch?v=eBmU1ONJ-os
;     - http://kaygun.tumblr.com/post/59316257966/simulated-annealing-in-lisp
;     - https://www.wikipedia.com/en/Simulated_annealing
;     - https://www.wikipedia.com/en/Metropolis%E2%80%93Hastings_algorithm

;     Clojure

;     - https://learnxinyminutes.com/docs/clojure/