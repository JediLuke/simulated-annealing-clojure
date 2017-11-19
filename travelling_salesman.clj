(print "\nInitiate: simulated annealing.\n\n")


;
; Initial setup


(def initialState 0)
(def initialTemp 10) ; num iterations


(defn lineFunction [m b] (fn [x] (+ (* m x) b))) ; returns a function which will calculate the y value for any x input value on the line y = mx + b
(defn firstTest [] (fn [x] (Math/sin (Math/exp (+ (* -1 x x) x 1)))))

; The following two parameters must be defined as functions
; "energy" or "cost function" (objective function)
(def phi
   (fn [x] (Math/sin (Math/exp (+ (* -1 x x) x 1))))) ; y = sin(exp(-x^2 + x + 1))
; function which takes in a state and returns the next state (neighbourhood function)
(def stateFn
   ;(lineFunction 2 3))
   (fn [x] (- (rand 7) 3))) ; Randomly choose value in range [-3,4] (where we know the answer lies)
   

;
; These functions are used in the higher level algorithm
(defn calcEnergy [state] (phi state))     ; Returns a function which calculates the energy of a state according to some energy function phi
(defn nextState [state] (stateFn state))        ; Returns a function which transforms states according to the initialization function provided

; Compute the probability of moving from one state to the next, given the Temperature of the system
(defn probMove [stateEnergy nextStateEnergy temperature]
   ; (defn exp [x n] (reduce * (repeat n x)))  ; Exponent function
   (if (<= nextStateEnergy stateEnergy)      ; If nextState has LESS energy than current state, i.e. we are MINIMIZING cost/energy here
      1                                      ;     Then we definitely move - thus probability of moving is 1
      (do
         ;(let [factor (/ (- (* -1 nextStateEnergy) stateEnergy) temperature)]
         (let [factor (/ (- stateEnergy nextStateEnergy) temperature)]
            (print "Factor: " factor " ")
            (let [prob (Math/pow 2.718 factor)] ; (exp 2.718 factor)]                         ;     Otherwise we use a Boultzmann distribution to determine probability of moving
               (print "prob calculated: " prob "\n")
               prob
               )
         )
      )
   )
)

(defn newTemp [curTemp]
   (/ curTemp 1.01)) 


;
; Simulated annealing

; Start a SA search from a given start state, at a given start temp, cooling down until tempMin has been reached
(defn simAnneal [state temp tempMin]
   (print "\nTEMP: " temp "\n\n")
   ; Base case - return state when system "cools down" to the point where search stops, i.e. temp <= tempMin
   (if (<= temp tempMin)
      (do
         (print "\n\n\nSystem cooldown achieved.\n")
         state ; return current state & complete search
      )
      ; else -  Recursive case
      (let [curEnergy (calcEnergy state)]                          ; Compute current energy level for current state
         (print "Current state:" state "Current energy: " curEnergy "Current temp:" temp "\n")
         (let [nextState (nextState state)]                             ; Apply the state transform function to the current state
            (let [nextStateEnergy (calcEnergy nextState)]          ; Compute energy for next state
               (print "CurEnergy: " curEnergy " nextEnergy: " nextStateEnergy "\n")
               (let [deltaE (- curEnergy nextStateEnergy)]              ; Calculate deltaE
                  (print "deltaE: " deltaE " ")
                  (if (> deltaE 0)                                   ; if (deltaE > 0) ; i.e. if currennt state has more energy, we jump (we are minimising a function here). If next state is more optimal
                     (do
                        (print "Jumping to next state!\n")
                        (simAnneal nextState (newTemp temp) tempMin)                 ;     return nextState
                     )
                     (let [pMove (probMove curEnergy nextStateEnergy temp)]
                        (print "Probbility of moving: " pMove)
                        (if (> pMove (Math/random))          ; elsif boulztMan(params) > rand(0,1)
                           (do
                              (print "\nChance says go to next state")
                              (simAnneal nextState (newTemp temp) tempMin)              ;     Move to worse state with probability dependent on temperature
                           )
                           (do
                              (print "\nNope staying put")
                              (simAnneal state (newTemp temp) tempMin)                 ; otherwise (i.e. prob of new state not high enough), return currentState
                           )
                        )
                     )
                  )
               )
            )
         )
      )
   )
)


; For tMax to tMin
(print "Final state: " (simAnneal initialState initialTemp 0.001))

   
;-----------------------------------------------------------------------

;  References
;  ----------

;     Simulated annealing

;     - https://www.gnu.org/software/gsl/manual/html_node/Simulated-Annealing-algorithm.html
;     - https://www.youtube.com/watch?v=eBmU1ONJ-os
;     - http://kaygun.tumblr.com/post/59316257966/simulated-annealing-in-lisp
;     - https://www.wikipedia.com/en/Simulated_annealing
;     - https://www.wikipedia.com/en/Metropolis%E2%80%93Hastings_algorithm
;     - http://www.theprojectspot.com/tutorial-post/simulated-annealing-algorithm-for-beginners/6
;     - http://homes.ieu.edu.tr/~agokce/Courses/Chapter%208%20Theory%20and%20Practice%20of%20simulated%20Annealing.pdf
;     - https://pdfs.semanticscholar.org/0f17/5f0f5ba0eae1783f03d6d39e52e6cd1f6726.pdf

;     Clojure

;     - https://learnxinyminutes.com/docs/clojure/