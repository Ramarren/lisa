(deftemplate rocky
  (slot name))

(deftemplate boris
  (slot name))

(deftemplate natasha
  (slot name)
  (slot nemesis))

(defrule rocky-boris-natasha
  ?f-1 <- (rocky (name "bullwinkle"))
  (boris (name ?boris-name&"boris"))
  (natasha (name "natasha") (nemesis ?boris-name))
  =>
  (printout t "rocky-boris-natasha fired (?boris-name = " ?name ")" crlf)
  (retract ?f-1))

; (defrule not-rocky
;   (not (rocky (name "rocky")))
;   =>
;   (printout t "not-rocky fired." crlf))

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
;   (rocky (name ?name&:(stringp ?name)))
;   (boris (name ?name&:(stringp ?name)))
;   (natasha (name "natasha"))
;   =>
;   (printout t "schtum-simple!" crlf))

; (defrule no-patterns
;   =>
;   (printout t "no-patterns!" crlf))

;(reset)
;(assert (rocky (name "boris")))
;(assert (boris (name "boris")))
;(assert (natasha (name "natasha")))

;(assert (natasha (name "natasha") (nemesis "rocky")))
;(assert (natasha (name "natasha") (nemesis "natasha")))

;(run)
