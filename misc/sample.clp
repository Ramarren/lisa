(deftemplate rocky
  (slot name))

(deftemplate boris
  (slot name))

(deftemplate natasha
  (slot name)
  (slot nemesis))

(defrule nemesis
  (natasha (name "natasha") (nemesis ?nemesis&"rocky"))
  (rocky (name ?nemesis))
  =>
  (printout t "nemesis fired!" crlf))

;  (defrule schtum
;    ?rocky <- (rocky (name "rocky"))
;    (boris (name "boris"))
;    ?natasha <- (natasha (name "natasha"))
;    =>
;    (retract ?rocky)
;    (retract ?natasha)
;    (printout t "schtum!" crlf))

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

(assert (rocky (name "rocky")))
(assert (natasha (name "natasha") (nemesis "rocky")))
(run)
