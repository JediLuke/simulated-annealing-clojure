(print "Initiate: simulated annealing.")


; Function to compute the "energy" or "cost" of a current state
(defn calcEnergy [phi, state]
   (phi state))

; Given current state, go to the next one according to some transform function
(defn generateNextState [state, transform]
   (transform state))

; Exponent function
(defn exp [x n]
   (reduce * (repeat n x))) 

; Compute the probability of moving from one state to the next, given the Temperature of the system
(defn probMove [curState, nextState, tempConstant]
   ; This function is a Boultzmann distribution
   (exp [2.718 (/
      (- nextState curState)
      (* tempConstant))]))


; Initial configuration

; Initial temperature

; For tMax to tMin

   ; Compute current energy level for current state
   ; Compute next state
   ; Compute energy for next state
   ; Calculate deltaE
   ; if (deltaE > 0) ; If next state is more optimal
      ; return nextState
   ; elsif boulztMan(params) > rand(0,1) ; Move to worse state with propability dependent on temperature
      ; return currentState

   
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