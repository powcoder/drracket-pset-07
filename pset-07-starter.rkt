;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pset-07-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DO NOT PUT ANY PERSONALLY IDENTIFYING BEYOND YOUR CS ID IN THIS FILE.
;; YOUR COMPUTER SCIENCE IDs WILL BE SUFFICIENT TO IDENTIFY YOU 
;; AND, IF YOU HAVE ONE, YOUR PARTNER
(require 2htdp/universe)
(require 2htdp/image)
(require spd/tags)

(@assignment pset-07);Do not edit or remove this tag
(@csid ??? ???)      ;Replace ??? with your csid,
;;                   ;second ??? is replaced with partner id if you have one




(@problem 1)
;; 
;; Design a function that consumes a list of strings and a list of naturals.
;; The function should produce the encrypted message by appending single
;; character strings from the words in the list of strings to each other.
;; The letter to be selected from each word depends on the values in the
;; list of naturals.
;;
;; If the list of naturals is not empty then the first number in the list
;; represents the position of the letter to choose from the first string.
;; 
;; For example:
;; (decryptor (list "The" "nose" "will" "hang" "most" "days")
;;            (list 2 0 1 3 0 1))
;; should produce "enigma"
;;
;; If the number is longer than the length of the string, then the word is
;; skipped
;; For example:
;; (decryptor (list "The" "nose" "will" "not" "hang" "on" "most" "days")
;;            (list 2 0 1 4 3 3 0 1))
;;   should produce "enigma"
;;
;; If the list of strings is not empty and the list of naturals is empty then
;; the decryptor should append the second letter from all the strings
;; capitalized
;; For example:
;;   (decryptor (list "The" "nose" "will" "hang" "most" "days")
;;              (list 2 0 1 3 0))
;;     should produce "enigmA"
;; 
;; If the list of strings is empty then the result should be the empty string.
;;
;; It is safe to assume that each word's length is >=2
;;
;; You must solve this as a 2-one-of problem and your solution must include
;; a table. You should show the correspondence between table cells and
;; cond QA pairs. If you can simplify the table you should.  Finally, it is
;; IMPERATIVE that your cond questions only include the kinds of questions
;; that the template rules produce, other conditional behaviour must go into
;; the cond answers.
;;


;(@htdf decryptor)






;; PART 2
;;
;; Read through the data definition for a patient, that you worked with for
;; problem set 6
;;

(@htdd Patient ListOfPatient ListOfString)
(define-struct patient (name age vac? symptoms transmits))
;; Patient is (make-patient String Natural Boolean ListOfString ListOfPatient)
;; interp. a patient with a name,
;;         their age restricted to [1, 120]
;;         vac? is true if patient has recieved the flu vaccine for 2019
;;         list of symptoms
;;         list of people they are known to have infected

;; ListOfPatient is one of:
;; - empty
;; - (cons Patient ListOfPatient)
;; interp. a list of patients

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings

(define LOS0 empty)
(define LOS1 (list "Fever"))
(define LOS2 (list "Fever" "Fatigue" "Diarrhea"))
(define LOS3 (list "Fatigue" "Diarrhea" "Vomiting"))
(define LOS4 (list "Diarrhea" "Stomach pain"))
(define LOS5 (list "Vomiting" "Stomach pain"))
(define LOS6 (list "Fever" "Fatigue" "Stomach pain" "Vomiting"))

(define P1  (make-patient "Hu"    12 false  LOS6 empty))
(define P2  (make-patient "Mandy"  6 true  LOS2 empty))
(define P3  (make-patient "Tom"   65 false LOS4 empty))
(define P4  (make-patient "Akemi" 44 true LOS5 empty))
(define P5  (make-patient "Chung"  5 true  LOS4 (list P1 P2 P3)))
(define P6  (make-patient "Neda"  26 false LOS2 empty))
(define P7  (make-patient "Jill"  57 true LOS3 empty))
(define P8  (make-patient "Paul"  11 true  LOS2 (list P6 P7)))
(define P9  (make-patient "Simran" 5 true  LOS1 (list P5 P4)))
(define P10 (make-patient "Kathy"  6 false  LOS0 (list P8 P9)))

(define LOP0 empty)
(define LOP1 (list P1 P2 P3))
(define LOP2 (list P9 P8))

(define (fn-for-patient p)
  (... (patient-name p)
       (patient-age p)
       (patient-vac? p)
       (fn-for-los (patient-symptoms p))
       (fn-for-lop (patient-transmits p))))
                 
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-patient (first lop))
              (fn-for-lop (rest lop)))]))


(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

(@problem 2)       
;;
;; Use local to refactor the three function templates above into  
;; a single encapsulated template that operates on a patient.
;;


(@problem 3)
;;
;; Refactor the following function designs into a single design for a single
;; function called count.
;;

(@htdf count--patient count--lop)
(@signature Patient -> Natural)
(@signature ListOfPatient -> Natural)
;; produce the number of patients who are >= 20 years and not vaccinated

(check-expect (count--lop empty) 0)
(check-expect (count--patient P3) 1)
(check-expect (count--patient P4) 0)
(check-expect (count--lop LOP1) 1)
(check-expect (count--lop (cons P4 LOP1)) 1)
(check-expect (count--patient P9) 1)
(check-expect (count--patient P10) 2)

(@template Patient)

(define (count--patient p)
  (if (and (not (patient-vac? p))
           (>= (patient-age p) 20))
      (+ 1 (count--lop (patient-transmits p)))
      (count--lop (patient-transmits p))))

(@template ListOfPatient)

(define (count--lop lop)
  (cond [(empty? lop) 0]
        [else
         (+ (count--patient (first lop))
            (count--lop (rest lop)))]))


(@problem 4)
;;
;; Refactor the following function designs into a single design for a single
;; function called all-kids.
;;

(@htdf all-kids--patient all-kids--lop  all-kids--los)
(@signature Patient String -> ListOfPatient)
(@signature ListOfPatient String -> ListOfPatient)
(@signature ListOfString String -> Boolean)
;; produce a list of patients < 13 yrs with symptom str

(check-expect (all-kids--los LOS0 "Fever") false)
(check-expect (all-kids--los LOS1 "Fever") true)
(check-expect (all-kids--los LOS2 "Vomiting") false)
(check-expect (all-kids--los LOS6 "Fever") true)
(check-expect (all-kids--los LOS6 "Stomach pain") true)
(check-expect (all-kids--los LOS6 "Vomiting") true)

(check-expect (all-kids--lop LOP0    "Vomiting") empty)
(check-expect (all-kids--patient P1  "Fever")    (list P1))
(check-expect (all-kids--patient P4  "Vomiting") empty)
(check-expect (all-kids--patient P2  "Vomiting") empty)
(check-expect (all-kids--lop LOP1    "Fever")    (list P1 P2))
(check-expect (all-kids--lop LOP2    "Fever")    (list P9 P1 P2 P8))
(check-expect (all-kids--patient P10 "Vomiting") (list P1))
(check-expect (all-kids--patient P10 "Fatigue")  (list  P8 P1 P2))
(check-expect (all-kids--patient P10 "Diarrhea") (list P8 P5 P2))
(check-expect (all-kids--patient P10 "Fever")    (list P8 P9 P1 P2))

(@template Patient)

(define (all-kids--patient p str)
  (if (and (< (patient-age p) 13)
           (all-kids--los (patient-symptoms p) str))
      (cons p
            (all-kids--lop (patient-transmits p) str))
      (all-kids--lop (patient-transmits p) str)))

(@template ListOfPatient)

(define (all-kids--lop lop str)
  (cond [(empty? lop) empty]
        [else
         (append (all-kids--patient (first lop) str)
                 (all-kids--lop (rest lop) str))]))

(@template ListOfString)

(define (all-kids--los los str)
  (cond [(empty? los) false]
        [else
         (or (string=? str (first los))
             (all-kids--los (rest los) str))]))



(@problem 5)
;;
;; Refactor the following function designs into a single design for a single
;; function called find.
;;
(@htdf find--patient find--lop)
(@signature Patient String -> Patient or false)
(@signature ListOfPatient String -> Patient or false)
;; produce the patient of the given name n, or false if not found
(check-expect (find--lop LOP0 "Hu") false)
(check-expect (find--lop LOP1 "Mandy") P2)
(check-expect (find--lop LOP2 "Tom") P3)
(check-expect (find--patient P10 "Hu") P1)
(check-expect (find--patient P10 "Flora") false)
(check-expect (find--patient P10 "Jill") P7)
(check-expect (find--patient P10 "Akemi") P4)


(@template Patient backtracking)

(define (find--patient p n)
  (if (string=? (patient-name p) n)
      p
      (find--lop (patient-transmits p) n)))

(@template ListOfPatient backtracking)

(define (find--lop lop n)
  (cond [(empty? lop) false]
        [else
         (if (not (false? (find--patient (first lop) n)))
             (find--patient (first lop) n)
             (find--lop (rest lop) n))]))


(@problem 6)
;;
;; Design a function that consumes a patient and produces the names of
;; all patients in a given patient's tree who have either vomitting or
;; diarrhea as a symptom and are under the age of 65, including the given
;; patient, if applicable.
;;
;; You should produce a single design for a single function called
;; names-criteria.
;;
