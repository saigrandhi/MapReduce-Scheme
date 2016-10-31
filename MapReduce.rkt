;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname MapReduce) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;PART B - SAI GRANDHI
;;Map Reduce function is being implemented below
(define (score page)
  ;;calling scoreHelper that will take a webpage with words and two lists for score tracking
  (let ((scoreOfPage (scoreHelper page '() '())))
    ;;scoreOfPage will be returned by sorting  all the keywords alphabetically
    (sort scoreOfPage (λ (temp1 temp2) (string<? (car temp1) (car temp2))))))

(define (scoreHelper page onePageScore repeatedScore)
  ;;checking if the page has words or not
  (if (null? page)
      onePageScore
      (if (null? repeatedScore)
          (letrec
              ;;since it is a new word from page, the score is set to one
              ((iThScore (cons (list (car page) 1) onePageScore))
               ;;using repeatedScore list for future findings of this word
               (repeatedScore iThScore))
            ;;the below line adds a pir to the repeatedScore list
            (scoreHelper (cdr page) iThScore repeatedScore))
          ;;in next recursive call, checks if there is a pair with same word
          (if (eqv? (car page) (caar repeatedScore))
              (letrec
                  ((iThScore (substitute (list (car page) (+ 1 (cadar repeatedScore))) onePageScore))
                   (jThScore iThScore))
                (scoreHelper (cdr page) iThScore jThScore))
              ;;call the next pair
              (scoreHelper page onePageScore (cdr repeatedScore))))))

;;helper method for scoreHelper
;;checks if the word has come before and if so
;;updates the list of repeatedScore in the scoreHelper
;;as required
(define (substitute words ls)
  (if (null? ls)
      '()
      (if (eqv? (car words) (caar ls))
          (cons words (substitute words (cdr ls)))
          (cons (car ls) (substitute words (cdr ls))))))
;;This is the reduce phase of the MapReduce algorithm
;;Start off with reduceScores function that takes a map of the score function
;;Then returns the pairs with their scores.
(define (reduceScores mapScoreList)
  ;;mapScoreList is currently a shared list with nested lists inside it
  ;;so we need to make it all into one list
  (let ((makeOneList (apply append mapScoreList)))
    ;;calling the helper function removeRepeated to remove duplicates from the mapScoreList
    (sort (removeRepeated makeOneList '()) (λ (count1 count2) (> (cadr count1) (cadr count2))))))

;;removeRepeated function is being implemented here
;;takes two variables, a list and another extra list that stores the new unique values
(define (removeRepeated scoreList uniqueList)
  (if (member? (caar scoreList) uniqueList)
      scoreList
      (let
          ;;score of word to 0 and calls the helper function
          ((changeScorels (remove-helper (caar scoreList) scoreList 0))
           (changeUniquels (cons (caar scoreList) uniqueList)))
        (removeRepeated changeScorels changeUniquels))))

;;remove-helper method for the removeRepeated function
;;removes repeated words in the list pairs
(define (remove-helper word scoreList wordScore)
  (if (null? scoreList)
      (cons (list word wordScore) '())
      ;;if word exists in one of the pair
      (if (eqv? word (caar scoreList))
          (remove-helper word (cdr scoreList) (+ wordScore (cadar scoreList)))
          (cons (car scoreList) (remove-helper word (cdr scoreList) wordScore)))))

;;firstThree function is being implemented below
;;it takes one input, reduceScoreResult and returns only the top 3 words
(define (firstThree reduceScoreResult)
  (list (car reduceScoreResult) (cadr reduceScoreResult) (caddr reduceScoreResult)))
              

