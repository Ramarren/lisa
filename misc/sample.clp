(deftemplate rocky
  (slot name))

(deftemplate boris
  (slot name))

(defrule schtum
  (rocky (name "rocky"))
  (boris (name "boris"))
  =>
  (printout t "schtum!" crlf))

(defrule schtum-shared
  (boris (name "boris"))
  =>
  (printout t "schtum-shared!" crlf))

(reset)
(assert (rocky (name "rocky")))
(assert (boris (name "boris")))
