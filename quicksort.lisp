#!/usr/local/bin/sbcl --script


; Xinke Cao, CS 326 Section 1002, Assignment 2
; Description: This lisp program will generate a list of random numbers and sort them using Quicksort. 
;			   Both the random list and the sorted list will be printed.
; Input: users will be able to input the size of the list and the range of the intergers of that list will take
; Output: the program will output the random list and the sorted integer list


; everythingLessThan: this function will create a list the consist of all of the elements in the myList that is less than pivot 
; parameters: int pivot, list myList 
; return value: an list that has all of the elements in myList that is less than pivot
(defun everythingLessThan (pivot myList)
	(if (= (length myList) 1)
		(if (< (car myList) pivot)
			(list (nth 0 myList))
		)
		(if (< (car myList) pivot)
			(append (list (nth 0 myList)) (everythingLessThan pivot (cdr myList)))
			(append (everythingLessThan pivot (cdr myList))))))


; everythingEqualTo: this function will create a list the consist of all of the elements in the myList that is equal to pivot 
; parameters: int pivot, list myList 
; return value: an list that has all of the elements in myList that is equal to pivot
(defun everythingEqualTo (pivot myList)
	(if (eq (cdr myList) NIL)
		(if (= (car myList) pivot)
			(list (car myList))
		)
		(if (= (car myList) pivot)
			(append (list (car myList)) (everythingEqualTo pivot (cdr myList)))
			(append (everythingEqualTo pivot (cdr myList))))))


; everythingMoreThan: this function will create a list the consist of all of the elements in the myList that is more than pivot 
; parameters: int pivot, list myList 
; return value: an list that has all of the elements in myList that is more than pivot
(defun everythingMoreThan (pivot myList)
	(if (eq (cdr myList) NIL)
		(if (> (car myList) pivot)
			(list (car myList));(print (list (car myList)))
		)
		(if (> (car myList) pivot)
			(append (list (car myList)) (everythingMoreThan pivot (cdr myList)))
			(append (everythingMoreThan pivot (cdr myList))))))



; pivotValue: this function calculates the median of the first element, the last element, and the middle element of an list to aid the quicksort algorithm
; parameters: list myList 
; return value: the median of the first element, the last element, and the middle element of an list
(defun pivotValue (myList)
	(if (eq (cdr myList) NIL)
		(car myList)
		(if (< (length myList) 3)
				(car myList)
				(medianValue (nth 0 myList) (nth (+ 0 (multiple-value-bind (q r) (floor (- (- (length myList) 1) 0) 2) q )) myList) (- (length myList) 1)))))


; myQuicksort: this function sorts myList using the quicksort algorithm, which has the goal of putting the pivot in the right place then sort everything less than pivot and everything more than pivot
; parameters: list myList
; return value: the sorted list
(defun myQuicksort (myList)
	(if (> (length myList) 1)
		(append (myQuicksort (everythingLessThan (pivotValue myList) myList)) (everythingEqualTo (pivotValue myList) myList) (myQuicksort (everythingMoreThan (pivotValue myList) myList)))
		(if (= (length myList) 1)
			(list (nth 0 myList)))))


; randomListGenerator: this function generates a random list 
; parameters: list smallList, int size 
; return value: a random list
(defun randomListGenerator (smallList size)
	(if (>= size 1)	;can still generate list
		(randomListGenerator 
			(append 
				(list 
					(generateRandomNumbers 
						(parse-integer 
							(nth 2 sb-ext:*posix-argv*)))) smallList) (- size 1))   ;returns a list with one more element
		smallList ))



; generateRandomNumbers: this function generates a random number by passing in a value that is the maximum of the number generated
; parameters: int maximum
; return value: a random number
(defun generateRandomNumbers (maximum)
	(if (>= maximum 1)
		(+ 1 (random maximum)) 
		-1 ))


; medianValue: this function finds the median of 3 numbers 
; parameters: int a, int b, int c 
; return value: the median of a, b, c
(defun medianValue (a b c)
	(if (> (* (- a b) (- b c)) 0)
		b
		(if (> (* (- a b) (- a c)) 0)	
			c
			a)))


; randomList stores the random list generated 
(defvar randomList (randomListGenerator '() (parse-integer (nth 1 sb-ext:*posix-argv*))))

; prints out the random list 
(format t "~S~%" randomList)

; prints out the sorted list 
(format t "~S~%" (myQuicksort randomList))


