#lang asi64
; PSID

(require threading)

(struct psid-header
  (magic-id
   ver
   data-offset
   load-address ; big endian pair
   init-address ; 
   play-address ; 
   songs
   start-song
   speed
   title
   author
   released       
   flags
   zp
   ) #:transparent )

(define (load-psid filename)
  (printf "loading psid ~a\n" filename)
  (let* ([raw-bytes (~> filename file->bytes bytes->list)]
         [header-bytes (take raw-bytes (+ 2 #x7c))]
         [code-bytes (drop raw-bytes (+ 2 #x7c))])
    (define/match (pair->16-bit pair)
      [((or (list a b) (cons a b))) (bitwise-ior (arithmetic-shift a 8) b)])
    (define (aux code acc)
      (if (eq? '() code)
          acc
          (match-let ([(list-rest a tail) code])
            (if (hash-has-key? opcode-raw-metadata a)
                (match-let
                    ([(metadata oc am v s _ _ _ tt)                 
                      (hash-ref opcode-raw-metadata a #f)])
                  (match (cons oc am)
                    [(cons (or 'sta 'stx 'sty) 'zp)                        
                     (aux (drop tail (- s 1)) (cons (car tail) acc))]
                    [_
                     (if (>  (length tail) (- s 1))
                         (aux (drop tail (- s 1)) acc)
                         acc)]))
                (aux tail acc)))))
    (match-let*
        ([(list-rest m1 m2 m3 m4
                     _ v
                     _ off
                     la1 la2
                     ia1 ia2
                     pa1 pa2
                     _ songs
                     _ ssong
                     sp1 sp2 sp3 sp4
                     tail) header-bytes]
         [title
          (~> tail (take #x20) (filter (λ (x) (not (eq? 0 x))) _)
              (map integer->char _) list->string)]
         [author
          (~> tail (drop #x20) (take #x20) (filter (λ (x) (not (eq? 0 x))) _)
              (map integer->char _) list->string)]
         [released
          (~> tail (drop #x40) (take #x20) (filter (λ (x) (not (eq? 0 x))) _)
              (map integer->char _) list->string)]
         [magic (list->string (map integer->char (list m1 m2 m3 m4)))]
         [zp (let ([addresses (remove-duplicates (aux code-bytes '()))])
               (printf "\tmusic directly accesses the following zp addresses ~a\n"
                       (map (λ (x) (format "$~x" x)) addresses))
               addresses)]
               
         [header
          (psid-header
           magic v off
           ;currently not dealing with explict load addresses.
           ;take the load address from first two data bytes
           ;(these have been included at the end of the header)
           (pair->16-bit (take (reverse header-bytes) 2))
           (pair->16-bit (cons ia1 ia2)) (pair->16-bit (cons pa1 pa2))
           songs ssong (list sp1 sp2 sp3 sp4)
           title author released
           (if (> v 1) 0 0) ; todo: loads flags from + $76 16 bit
           zp
           )])

      (when (not (and (eq? la1 0) (eq? la2 0)))
        (printf "ERROR! PSID uses specfic load address and not the normal binary loading address.  Have not handled this case yet!"))        

      (printf "\tpsid data: ~a, ~a, ~a
\t\tdata address $~x
\t\tinit address $~x
\t\tplay address $~x
\t\tsongs ~x\n"
              (psid-header-title header)
              (psid-header-author header)
              (psid-header-released header)
              (psid-header-load-address header)
              (psid-header-init-address header)
              (psid-header-play-address header)
              (psid-header-songs header))
      (set-location (psid-header-load-address header))
      (write-values code-bytes)
      header)))

(define (init-psid header) {jsr (psid-header-init-address header)})
(define (play-psid header) {jsr (psid-header-play-address header)})


(provide (all-defined-out))
