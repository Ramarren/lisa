(deftemplate rocky
  (slot name))

(deftemplate boris
  (slot name))

; (defrule schtum
;   (rocky (name "rocky"))
;   (boris (name "boris"))
;   =>
;   (printout t "schtum!" crlf))

; (defrule schtum-shared
;   (boris (name "boris"))
;   =>
;   (printout t "schtum-shared!" crlf))

; (defrule schtum-simple
;   (rocky (name "rocky"))
;   =>
;   (printout t "schtum-simple!" crlf))

(defrule no-patterns
  =>
  (printout t "no-patterns!" crlf))

(reset)

(assert (rocky (name "rocky")))
(assert (boris (name "boris")))
