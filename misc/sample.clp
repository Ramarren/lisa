(deftemplate rocky
  (slot name))

(deftemplate boris
  (slot name))

(deftemplate natasha
  (slot name)
  (slot nemesis))

(defrule not-rocky
  (not (rocky (name "rocky")))
  =>
  (printout t "not-rocky fired." crlf))

; (defrule duplicate-vars
;   (natasha (name ?name) (nemesis ?name))
;   =>
;   (printout t "duplicate-vars fired!" crlf))

; (defrule nemesis
;   (natasha (name "natasha") (nemesis ?nemesis&"rocky"))
;   (rocky (name ?nemesis))
;   =>
;   (printout t "nemesis fired!" crlf))

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

(reset)
(assert (rocky (name "boris")))
(retract (fact-id 1))

;(assert (natasha (name "natasha") (nemesis "rocky")))
;(assert (natasha (name "natasha") (nemesis "natasha")))

;(run)
