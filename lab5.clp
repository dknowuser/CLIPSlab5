; Block structure definition
(deftemplate block
	(slot color (type SYMBOL))
	(slot size (type INTEGER))
	(slot place (type SYMBOL)(default heap))
	(slot upper (type SYMBOL)(default none))
	(slot lower (type SYMBOL)(default none)))

; Current task
(deftemplate task
	(slot current-task (type SYMBOL)))

; Define blocks we have here
(deffacts start-cond
	(block (color BLUE) (size 10))
	(block (color GREEN) (size 8))
	(block (color RED) (size 18))
	(block (color YELLOW) (size 15))
	(task (current-task find)))

; Initial rule for adding initial fact
(defrule init
	(initial-fact)
=>
	(assert (start-cond))
	(printout t "Initial fact is added" crlf)
	(printout t "Block 1: color BLUE size 10" crlf)
	(printout t "Block 2: color GREEN size 8" crlf)
	(printout t "Block 3: color RED size 18" crlf)
	(printout t "Block 4: color YELLOW size 15" crlf)
	(printout t "The current task has been switched to find" crlf))

; The rule that gets the biggest block from the heap
; and makes find to be a current task
(defrule do-find
	?t<-(task (current-task find))
	?b<-(block (color ?clr1) (size ?sz1) (place heap))
	(not (exists (block (size ?sz2&:(> ?sz2 ?sz1)) (place heap))))
=>
	(modify ?b(place hand))
	(printout t ?clr1 " block was taken" crlf)
	(modify ?t(current-task build))
	(printout t "The current task has been switched to build" crlf))

; The rule that builds the tower base
(defrule do-base
	?t<-(task (current-task build))
	?b<-(block (color ?clr) (place hand))
	(not (exists (block (place tower))))
=>
	(modify ?b(place tower))
	(printout t ?clr " block was inserted in a base of the tower" crlf)
	(modify ?t(current-task update))
	(printout t "The current task has been switched to find" crlf))

; The rule that builds the tower
(defrule do-build
	?t<-(task (current-task build))
	?bh<-(block (color ?hclr) (place hand))
	?bt<-(block (color ?tclr) (place tower) (upper none))
=>
	(modify ?bh(place tower) (lower ?tclr))
	(modify ?bt(upper ?hclr))
	(printout t ?hclr " block was inserted in the tower" crlf)
	(modify ?t(current-task update))
	(printout t "The current task has been switched to find" crlf))

; The rule that deletes the current task
(defrule delete-task
	?t<-(task (current-task find))
	(not (exists (block (place heap))))
=>
	(retract ?t)
	(printout t "The current task has been deleted" crlf))

; The rule that updates tower top output
(defrule do-top-update
	?t<-(task (current-task update))
	?b<-(block (color ?clr) (size ?sz) (place tower) (upper none) (lower ?lb))
=>
	(printout t crlf "Tower:" crlf)
	(printout t ?clr " block size " ?sz crlf)
	(if (eq ?lb none) then
		(printout t crlf)
		(modify ?t(current-task find))
	else (assert (color ?lb))))

; The rule that updates tower output
(defrule do-update
	?t<-(task (current-task update))
	(exists (color ?clr))
	?c<-(color ?clr)
	?b<-(block (color ?clr) (size ?sz) (place tower) (lower ?lb))
=>
	(retract ?c)
	(printout t ?clr " block size " ?sz crlf)
	(if (eq ?lb none) then
		(printout t crlf)
		(modify ?t(current-task find))
	else (assert (color ?lb))))

