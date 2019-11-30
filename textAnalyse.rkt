#|
    Joan Andoni Gonźalez Rioz
    A00569929
    Enrique Badillo
    A0

    Program analyze the exported chats from whats app and facebook in format .txt and
    returns the percentage of used words that you could get tracked.
|#

(define words2track (list "This" "only" "y"))

(define (contains diccionary word)
    (cond
        ((null? diccionary) 0)
        ((equal? (car diccionary) word) 1)
        (else (contains (cdr diccionary) word))
    )
)

(define (comparisonOfWords words2compare numberOfWords numberOfAppearances)
    (cond
        ((null? words2compare) (list numberOfAppearances numberOfWords (/ numberOfAppearances numberOfWords)))
        (else (comparisonOfWords (cdr words2compare) (+ numberOfWords 1) (+ numberOfAppearances (contains words2track (car words2compare)))))
    )
)

(define (appendWords lines finalWords)
    (cond
        ((null? lines) (comparisonOfWords finalWords 0 0))
        (else (appendWords (cdr lines) (append (string-split (car lines)) finalWords)))
    )
)

(appendWords (file->lines "WhatsAppChat.txt") '())