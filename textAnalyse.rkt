#|
    Joan Andoni Gonźalez Rioz
    A00569929
    Enrique Vadillo
    A01018714

    Program analyze the exported chats from whats app and facebook in format .txt and
    returns the percentage of used words that you could get tracked.
|#

; #lang racket

(define (welcome)
	(display "This program analyses an exported chat txt file\n Please enter the name fo the file between quotes\n like this:  \"file.txt\"\n ")
)

(define (countOfWords lst)
    (foldl 
        ( lambda 
            (key ht)
            (hash-update ht key add1 0)
        )
        #hash() lst
    )
)

(define (appendWordsInList2track lines finalWords)
    (append (string-split (car lines)) finalWords)
)

(define words2track (appendWordsInList2track (file->lines "words2track.txt") '()))

(define (contains diccionary word)
    (cond
        ((null? diccionary) #f)
        ((equal? (car diccionary) word) (list word))
        (else (contains (cdr diccionary) word))
    )
)

(define (comparisonOfWords listRepeated words2compare numberOfWords numberOfAppearances)
    (cond
        ((null? words2compare) (printResults numberOfAppearances numberOfWords (/ numberOfAppearances numberOfWords) listRepeated))
        ((contains words2track (car words2compare)) (comparisonOfWords (append listRepeated (contains words2track (car words2compare))) (cdr words2compare) (+ numberOfWords 1) (+ numberOfAppearances 1)))
        ((not (contains words2track (car words2compare))) (comparisonOfWords listRepeated (cdr words2compare) (+ numberOfWords 1) numberOfAppearances))
    )
)

(define (appendWords lines finalWords)
    (cond
        ((null? lines) (comparisonOfWords '() finalWords 0 0))
        (else (appendWords (cdr lines) (append (string-split (car lines)) finalWords)))
    )
)

(define (printResults numberOfApperances numberOfWords ratio listRepeated)
  (printf "There were ~a words in the chat ~n" numberOfWords)
  (printf "Your selected words  appeared ~a times ~n" numberOfApperances)
  (printf "That makes ~a% of all the words ~n" (exact->inexact (* ratio 100)))
  (printf "List of words to search is : ~a ~n" (appendWordsInList2track (file->lines "words2track.txt") '()))
  (printf "The most frequent words which apppeared is: ")
  (countOfWords listRepeated)
)

(welcome)
(appendWords (file->lines (read)) '())
