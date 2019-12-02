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

(define (most-frequent-element xs)
  (define ht (make-hash))
  (for ([x xs]) (hash-update! ht x add1 0))
  (for/fold ([max-x #f] [max-count 0]) ([(x c) ht])
    (if (> c max-count)
        (values x c)
        (values max-x max-count)))
)

(define words2track (file->list "words2track.txt" ))

(define (contains diccionary word)
    (cond
        ((null? diccionary) 0)
        ((equal? (car diccionary) word) 1)
        (else (contains (cdr diccionary) word))
    )
)

(define (comparisonOfWords words2compare numberOfWords numberOfAppearances)
    (cond
        ((null? words2compare) (printResults numberOfAppearances numberOfWords (/ numberOfAppearances numberOfWords)))
        (else (comparisonOfWords (cdr words2compare) (+ numberOfWords 1) (+ numberOfAppearances (contains words2track (car words2compare)))))
    )
)

(define (appendWords lines finalWords)
    (cond
        ((null? lines) (comparisonOfWords finalWords 0 0))
        (else (appendWords (cdr lines) (append (string-split (car lines)) finalWords)))
    )
)

(define (printResults numberOfApperances numberOfWords ratio)
  (printf "There were ~a words in the chat ~n" numberOfWords)
  (printf "Your selected words  appeared ~a times ~n" numberOfApperances)
  (printf "That makes ~a% of all the words ~n" (exact->inexact (* ratio 100)))
  (printf "List of words to search is : ~a ~n" (file->list "words2track.txt"))
  (printf "The most frequent words which apppeared is: ")
  (most-frequent-element (file->list "WhatsAppChat.txt"))
)

(welcome)
(appendWords (file->lines (read)) '())
