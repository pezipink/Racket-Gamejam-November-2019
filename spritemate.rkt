#lang asi64

(require threading racket/function)

(struct sprite-data
  (frame-count   
   start-address  ; raw data location
   start-offset   ; sprite pointer offset 
   end-offset     ; final frame
   colours        ; multicolours   
   data           ; actual data
   )  #:transparent)

(define (extract-sprite filename start-address start-offset)
  (let* (;read file as a sequence of lines
         [lines (file->lines filename)]
         ;count the amount of frames by looking at lines that end with :
         [frames (length (filter (λ (s) (string-suffix? s ":")) lines))]
         [colours (~>>
                   lines
                   (filter (λ (s) (string-prefix? s "// sprite")))
                   (map (λ (s)
                          (let* ([sl (string-length s)]
                                 [start (- sl 2)])
                            (substring s start))))
                   (map (λ (s) (string->number s 16))))]
         

         ; extract the raw data as one big lump
         [data (~>>
                lines
                ; filter to .byte rows 
                (filter (λ (s) (string-prefix? s ".byte")))
                ; clean up text leaving raw hex values
                (map (λ (s) (string-replace s ".byte " "")))
                (map (λ (s) (string-replace s "$" "")))
                (map (λ (s) (string-split s ",")))
                ; flatten into one big list of numbers
                (flatten)
                ; parse hex 
                (map (λ (s) (string->number s 16))))])

    (sprite-data frames
                 start-address
                 start-offset
                 (+ start-offset (- frames 1))
                 colours
                 data)))

(define sprites
  (let ([files
         (~>>
          (directory-list "sprites" #:build? #t)
          (map path->string)
          (filter (λ (s) (string-suffix? s ".txt"))))])
    (match-let-values
        ([(_ _ sprites)
          (for/fold ([address $2000]
                     [offset $80]
                     [sprites (list)])
                    ([filename files])
            (let
                ([short-name  ; extract file name (probably a nicer way to do this)
                  (~>
                   filename
                   (string-split "\\")
                   reverse
                   first
                   (string-replace ".txt" ""))]
                 [sprite (extract-sprite filename address offset)])
              ; build sprite data folding though the addresses and offsets for later use
              (values (+ address (* (sprite-data-frame-count sprite) $40))
                      (+ offset (sprite-data-frame-count sprite))
                      (cons (cons short-name sprite) sprites))))])
      ; hash by sprite name
      (make-hash sprites))))


(define (get-sprite-start name)
  (~>
   sprites
   (hash-ref name)
   sprite-data-start-offset))

(define (get-sprite-end name)
  (~>
   sprites
   (hash-ref name)
   sprite-data-end-offset))

(define (get-last-address)
  (~>>
   sprites
   (hash-values)
   (map
    (match-lambda
      [(sprite-data
        cnt start
        _ _ _ _ )  (+ start (* cnt $40))]))        
   (argmax identity)))
(define (get-last-offset)
  (~>>
   sprites
   (hash-values)
   (map
    (match-lambda
      [(sprite-data
        _ _ _ end  _ _ )  end]))        
   (argmax identity)))
(provide (all-defined-out))
