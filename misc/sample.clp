(deftemplate rocky
  (slot name))

(deftemplate boris
  (slot name))

(deftemplate natasha
  (slot name)
  (slot nemesis))

(defrule nemesis
  (natasha (name "natasha") (nemesis "bullwinkle"))
  =>
  (printout t "nemesis fired!" crlf))

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

;;;(defrule no-patterns
;;;  =>
;;;  (printout t "no-patterns!" crlf))

;;;(reset)
;;;(assert (rocky (name "rocky")))
;;;(assert (boris (name "boris")))
