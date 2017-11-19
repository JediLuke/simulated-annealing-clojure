(print "\nInitiate: simulated annealing.\n\n")


;     Initial setup
; ---------------------


(def initialState 0)
(def initialTemp 10)


(def neighbourFn
   ; Neighbourhood function : Generate the next state for us to explore, usually influenced by current state
   (fn [x] (- (rand 7) 3))) ; Randomly choose value in range [-3,4] (we know answer lies in this domain)


(def phi
   ; "energy", "cost" or "objective" function : Takes in a state & returns the 'energy' of that state. We are looking for global min of this Fn.
   (fn [x] (Math/sin (Math/exp (+ (* -1 x x) x 1))))) ; y = sin(exp(-x^2 + x + 1)), global minima approx. (0.5, -0.3), see [3]


;     Helper functions
; ------------------------


(defn probStateChange [stateEnergy nextStateEnergy temperature]
   ; Compute the probability of moving from one state to the next, given the Temperature of the system
   (if (<= nextStateEnergy stateEnergy)      ; If nextState has LESS energy than current state (we are MINIMIZING phi)
      1                                      ;     Then we definitely move - thus probability of moving is 1
      (Math/pow 2.718 (/                     ;     Otherwise we use a Boultzmann distribution to determine probability of moving
         (- stateEnergy nextStateEnergy)
         temperature))))


(defn newTemp [curTemp]
   ; Cooldown function: Determinnes rate at which system 'cools down' and stops searching
   (/ curTemp 1.01)) 


;     Simulated annealing
; ---------------------------


(defn simAnneal [state temp tempMin]
   ; Base case - search is finished, return the result
   (if (<= temp tempMin) (do
      (print "\n\nSystem cooldown achieved.\n")
      state)
   ; Else -  recursive case
      (let [nextState (neighbourFn state)]                                    ; Find neighbouring state in search space
         (if (> (- (phi state) (phi nextState)) 0)                            ; IF nextState has less energy
            (simAnneal nextState (newTemp temp) tempMin)                      ;     Move to nextState as it's more optimal
            (let [pMove (probStateChange (phi state) (phi nextState) temp)]   ; ELSE move to next state according to some probability (Boultzmann distribution)
               (if (> pMove (Math/random))                                    
                  (simAnneal nextState (newTemp temp) tempMin)                ;     Move to next (& worse) state - probability of accepting worse states higher when system temperature is high
                  (simAnneal state (newTemp temp) tempMin))                   ;     otherwise stay in current state and look at different neighbour
                  )))))


; Start recursive algorithm
(print "Final state: " (simAnneal initialState initialTemp 0.001))

   
;-----------------------------------------------------------------------

;     References
;  -----------------

;     Simulated annealing

;     - [1] https://www.gnu.org/software/gsl/manual/html_node/Simulated-Annealing-algorithm.html
;     - [2] https://www.youtube.com/watch?v=eBmU1ONJ-os
;     - [3] http://kaygun.tumblr.com/post/59316257966/simulated-annealing-in-lisp
;     - [4] https://www.wikipedia.com/en/Simulated_annealing
;     - [5] https://www.wikipedia.com/en/Metropolis%E2%80%93Hastings_algorithm
;     - [6] http://www.theprojectspot.com/tutorial-post/simulated-annealing-algorithm-for-beginners/6
;     - [7] http://homes.ieu.edu.tr/~agokce/Courses/Chapter%208%20Theory%20and%20Practice%20of%20simulated%20Annealing.pdf
;     - [8] https://pdfs.semanticscholar.org/0f17/5f0f5ba0eae1783f03d6d39e52e6cd1f6726.pdf

;     Clojure

;     - [9] https://learnxinyminutes.com/docs/clojure/