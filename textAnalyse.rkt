

(define (appendWords lines)
    (if (null? (cdr lines))                                             ; IF the next value in the list is empty
        (append (string-split (car lines)))                             ; TRUE -> Regresar todas las palabras de los array en una lista
        (append (string-split (car lines)) (appendWords (cdr lines)))   ; FALSE -> Append de los elementos del primer array, con los elementos de los array que siguen  
    )
)

(with-input-from-file "test.txt"                ; Make the read from the following file
    (lambda ()
        (let loop (
            (lines '())                         ; Create the array where the lines are going to be saved
            (next-line (read-line))             ; Move to the next line
            )
            ( if (eof-object? next-line)        ; IF is the end of the file then...
                (appendWords (reverse lines))   ; Return all the lines
                (loop (cons next-line lines)    ; ELSE keep reading to the next line
                    (read-line)
                )
            )
        )
    )
)