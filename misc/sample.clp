(deftemplate rocky
  (slot name))

(deftemplate boris
  (slot name))

(deftemplate natasha
  (slot name)
  (slot nemesis))

;(defrule nemesis
;  (natasha (name "natasha") (nemesis "bullwinkle"))
;  =>
;  (printout t "nemesis fired!" crlf))

 (defrule schtum
   ?rocky <- (rocky (name "rocky"))
   (boris (name "boris"))
   ?natasha <- (natasha (name "natasha"))
   =>
   (retract ?rocky)
   (retract ?natasha)
   (printout t "schtum!" crlf))

; (defrule schtum-shared
;   (boris (name "boris"))
;   =>
;   (printout t "schtum-shared!" crlf))

; (defrule schtum-simple
;   ?f <- (rocky (name "rocky"))
;   =>
;   (retract ?f)
;   (printout t "schtum-simple!" crlf))

; (defrule no-patterns
;   =>
;   (printout t "no-patterns!" crlf))

;(reset)
;(assert (natasha (name "natasha") (nemesis "bullwinkle")))
;(retract (fact-id 1))
(reset)
(assert (rocky (name "rocky")))
(assert (boris (name "boris")))
(assert (natasha (name "natasha")))
(run)
;(assert (boris (name "boris")))
;(retract (fact-id 1))
