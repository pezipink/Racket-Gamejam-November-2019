#lang asi64
(require threading)
(require syntax/parse/define)
(require (file "psid.rkt"))
(require (file "spritemate.rkt"))
; setup emulator
(set-emulator-program! emu "c64.prg")
(set-emulator-execute?! emu #t)
(if (eq? (system-type 'os) 'windows)
    (set-emulator-path! emu "C:\\Program Files\\WinVICE-3.0-x64\\x64.exe")
    (set-emulator-path! emu "/snap/bin/vice-jz.x64"))

(define (ascii->petscii text)
  (~>>
   (string->list text)
   (map (λ (x)
          (cond
              [(char-alphabetic? x)
               (- (char->integer (char-upcase x)) 64)]
              [(let ([n (char->integer x)])
                 (and (>= n 32) (<= n 63)))
               (char->integer x)]              
              [else
               (case x
                 [(#\@) 0]
                 [(#\_) 82]
                 [else 63])])))))
;non-kitten-items
(define nkis
  (~>>
   (append
    (file->lines "nkis\\original.nki")
    (file->lines "nkis\\fortunes.nki")
    (file->lines "nkis\\vanilla.nki"))
   (filter (λ (x) (not (or (string-prefix? x "#") (> (string-length x) 80)))))
   (shuffle)
   (take _ 256)))

(define (zp-generator exceptions)
  (let* ([next-addr $ff]
         ;blacklist of addresses to not use
         [kernal (list $a0 $a1 $a2)]
         [exceptions (remove-duplicates (append exceptions kernal))])
    (λ ()
      (define (aux)
        (let ([addr next-addr])
          (set! next-addr (- next-addr 1))
          (if (set-member? exceptions addr)
              (aux)
              addr)))
      (aux))))

{
 breath_fire = (load-psid "music\\Breath_of_fire.sid")

  joy-up    = %0000_0001
  joy-down  = %0000_0010
  joy-left  = %0000_0100         
  joy-right = %0000_1000
  joy-fire  = %0001_0000

  state_start_screen = 0
  state_game = 1
  state_found_kitty = 2

  ;kitty found states
  state_kitty_moving = 0
  state_heart_flashing = 1
  
 (define-syntax-parser zp
   [(_ name ...)
    #'{ (begin         
          (define name (new-zp))
          (printf "~a = $~x\n" 'name name)
          (set-jump-source (format "~a" 'name) name)
          ) ...}])

   new-zp = (zp-generator (psid-header-zp breath_fire))

   ; ZP addresses
  ; ---------------------------
 (zp ; assigned from $ff downwards
   ;pointers are little-endian but we are assigning backwards here.
  lfsr
  lfsr2
  temp_high 
  temp_low
  object_high
  object_low
  lookup1_high
  lookup1_low 
  lookup2_high
  lookup2_low 
  string_size 
  screen_pointer_high 
  screen_pointer_low
  kitty_high
  kitty_low
  robot_high
  robot_low
  input_delay
  input_delay2
  quote_index
  game_state ; 0 
  inner_state 
   )
}


    ; main program
(C64 {
   *= $0801
   ;autostart 163844 ($4000)
   (data $0b $08 $01 $00 $9E $31 $36 $33 $38 $34 $00 $00 $00 $00)          

   (hash-for-each sprites
    (λ (_ sprite)
      (begin
        (set-location (sprite-data-start-address sprite))
        (data (sprite-data-data sprite)))))   

   
   *= $4000

   lda @0
   sta game_state
   sta inner_state
   sta $d021   
   sta $d020
   lda @(random 1 255)
   sta lfsr
   lda @(random 1 255)
   sta lfsr2

   lda @0
   sta $d01b  ; sprites in front of chars

   ; turn on multicolour mode for all sprites
   lda @$FF
   sta $d01c 
   lda @$04 ; sprite multicolor 1
   sta $D025
   lda @$06 ; sprite multicolor 2
   sta $D026

   ; set background colour to black
   lda @0
   sta $d021
   sei
   lda @$7f
   sta $dc0d ;switch off cia interrupts
   sta $dd0d ; cia2
   lda @1
   sta $d01a ; raster interrupts on
   lda @$1b
   ldx @$08
   ldy @$14
   sta $d011    ; Clear high bit of $d012, set text mode
   stx $d016    ; single-colour
   lda @<interrupt+
   sta $0314
   lda @>interrupt+ ;high
   sta $0315
   lda @$80  ; last line for interrupt
   sta $d012

   ;ack interrupts
   lda $dc0d
   lda $dd0d
   asl $d019
   cli

   (init-psid breath_fire)
   jsr init_title:
   jmp (here)

:init_title   
   jsr clear-screen:
   jsr white-colours:
   lda @$04
   sta screen_pointer_high
   lda @$00
   sta screen_pointer_low
   lda @<title-data:
   sta lookup2_low
   lda @>title-data:
   sta lookup2_high

   lda @0
   ldy @0
   ; move to the first char
:write
   lda £ lookup2_low y
   sta £ screen_pointer_low y
   iny
   bne write-
   ;next page
   lda lookup2_low
   clc
   adc @$ff
   sta lookup2_low
   bcc skip+
   inc lookup2_high
   :skip
   lda screen_pointer_low
   clc
   adc @$ff
   sta screen_pointer_low
   bcc skip+
   inc screen_pointer_high
   :skip   
:write
   lda £ lookup2_low y
   sta £ screen_pointer_low y
   iny
   bne write-
   ;next page
   lda lookup2_low
   clc
   adc @$ff
   sta lookup2_low
   bcc skip+
   inc lookup2_high
   :skip
   lda screen_pointer_low
   clc
   adc @$ff
   sta screen_pointer_low
   bcc skip+
   inc screen_pointer_high
   :skip   
:write
   lda £ lookup2_low y
   sta £ screen_pointer_low y
   iny
   bne write-
 ; next page
   lda lookup2_low
   clc
   adc @$ff
   sta lookup2_low
   bcc skip+
   inc lookup2_high
   :skip
   lda screen_pointer_low
   clc
   adc @$ff
   sta screen_pointer_low
   bcc skip+
   inc screen_pointer_high
   :skip   
:write
   lda £ lookup2_low y
   sta £ screen_pointer_low y
   iny
   bne write-
   rts
   
:init_game
   jsr clear-screen:
   jsr random-colours:
   jsr draw-border:

   lda @2
   sta input_delay
   ;no sprites
   lda @0
   sta $d015

   ; generate random location for the robot. to generate random screen locations
   ; we'll rng 256 * 3 then + 128. it not quite 920 but its close enough
;   break
   jsr gen-screen-loc-clear:
   lda temp_low
   sta robot_low
   lda temp_high
   sta robot_high

   lda @35  ;#
   ldy @0
   sta £ robot_low y

   ; now we generate some locations for things to inspect.
   ; the first one will be the kitty.
   jsr gen-screen-loc-clear:
   lda temp_low
   sta kitty_low
   lda temp_high
   sta kitty_high
   ; pick a random char for kitty
   jsr random-char:
   ldy @0
   sta £ kitty_low y
   sty temp_low
   sty temp_high
   ; now generate a bunch more stuff
   ldx @15
:loop
   jsr gen-screen-loc-clear:
   jsr random-char:
   ldy @0
   sta £ temp_low y
   dex
   bne loop-
   rts

:play_game
   jsr handle_input:
   ;if the robot hit something it will still be in temp
   ;so we can check here if its the kitty.
   lda object_low
   cmp kitty_low
   bne done+
   lda object_high
   cmp kitty_high
   bne done+
   inc $d020
   
   jsr clear-screen:
   lda @state_found_kitty
   sta game_state
   lda @(get-sprite-start "kitty")      
   sta $07f8
   lda @(get-sprite-start "heart")      
   sta $07f9
   lda @(get-sprite-start "robot")      
   sta $07fa
   lda @$30
   sta temp_low ; used as animation counter
   lda @100 ; x kitty
   sta $d000   
   lda @142 ;y kitty
   sta $d001

   ;heart 
   lda @175
   sta $d002
   lda @142
   sta $d003
   
   lda @250 ; x robot
   sta $d004
   lda @142 ;y robot
   sta $d005
   
   lda @%0000_0101
   sta $d015
   jsr white-colours:
   :done
   rts
   
   
:handle_input
   ldx input_delay
   beq next+
   dex
   stx input_delay
   rts
:next
   ldx @2
   stx input_delay
   lda robot_low
   sta temp_low
   lda robot_high
   sta temp_high
   lda $dc00
   tax
   and @joy-left
   bne next-joy+
     ; JOY LEFT
     lda temp_low
     sec
     sbc @1
     sta temp_low
     bcs skip+
     dec temp_high
    :skip
     jsr collision:  ; equal if a blank space
     bne skip+
     ; move left
     lda @$20
     sta £ robot_low y
     lda temp_low
     sta robot_low
     lda temp_high
     sta robot_high
     lda @35
     sta £ robot_low y
     rts
   :skip
     ; if this wans't a wall, show quote
     jsr collision-wall:
     bne quote+
     rts
     :quote
     ;preserve object collided
     sta quote_index
     lda temp_low
     sta object_low
     lda temp_high
     sta object_high
     jsr show_quote:
     rts
:next-joy
   txa
   and @joy-right
   bne next-joy+
     ; JOY LEFT
     lda temp_low
     clc
     adc @1
     sta temp_low
     bcc skip+
     inc temp_high
   :skip
     jsr collision:  ; 0 if no collision
     bne skip+
     ; move right
     lda @$20
     sta £ robot_low y
     lda temp_low
     sta robot_low
     lda temp_high
     sta robot_high
     lda @35
     sta £ robot_low y
     rts
   :skip
     ; if this wans't a wall, show quote
     jsr collision-wall:
     bne quote+
     rts
     :quote
     ;preserve object collided
     sta quote_index
     lda temp_low
     sta object_low
     lda temp_high
     sta object_high
     jsr show_quote:
     rts
:next-joy
   txa
   and @joy-up
   bne next-joy+
     ; JOY UP
     lda temp_low
     sec
     sbc @40
     sta temp_low
     bcs skip+
     dec temp_high
   :skip
     jsr collision:  ; 0 if no collision
     bne skip+
     ; move up
     lda @$20
     sta £ robot_low y
     lda temp_low
     sta robot_low
     lda temp_high
     sta robot_high
     lda @35
     sta £ robot_low y
     rts
   :skip
     ; if this wans't a wall, show quote
     jsr collision-wall:
     bne quote+
     rts
     :quote
     ;preserve object collided
     sta quote_index
     lda temp_low
     sta object_low
     lda temp_high
     sta object_high
     jsr show_quote:
     rts

:next-joy
   txa
   and @joy-down
   bne next-joy+
     ; JOY down
     lda temp_low
     clc
     adc @40
     sta temp_low
     bcc skip+
     inc temp_high
   :skip
     jsr collision:  ; 0 if no collision
     bne skip+
     ; move up
     lda @$20
     sta £ robot_low y
     lda temp_low
     sta robot_low
     lda temp_high
     sta robot_high
     lda @35
     sta £ robot_low y
     rts
   :skip
     ; if this wans't a wall, show quote
     jsr collision-wall:
     bne quote+
     rts
     :quote
     ;preserve object collided
     sta quote_index
     lda temp_low
     sta object_low
     lda temp_high
     sta object_high
     jsr show_quote:
     rts
:next-joy     
   :skip

   rts

    
:kitty_found
   lda inner_state
   bne heart+
   ldx temp_low
   beq done+
   dex
   stx temp_low
   inc $d000  ;x kitty
   ;; inc $d002   ; x hear
   dec $d004  ; x ro
   rts
:done
   lda @1
   sta inner_state
   ; write text. 
   lda @$90
   sta screen_pointer_low
   lda @$05
   sta screen_pointer_high
   
   lda @<kitty-found-data-1:
   sta temp_low
   lda @>kitty-found-data-1:
   sta temp_high
   ;copy 40 chars
   ldx @40
   ldy @0
:loop
   lda £ temp_low y
   sta £ screen_pointer_low y
   iny
   dex
   bne loop-
   lda @$58
   sta screen_pointer_low
   lda @$06
   sta screen_pointer_high
   
   lda @<kitty-found-data-2:
   sta temp_low
   lda @>kitty-found-data-2:
   sta temp_high
   ;copy 40 chars
   ldx @40
   ldy @0
:loop
   lda £ temp_low y
   sta £ screen_pointer_low y
   iny
   dex
   bne loop-
   lda @15 ; used as heart timer
   sta temp_low
   rts
:heart
   ldx temp_low
   beq flash+
   dex
   stx temp_low
   rts
   ;flash heart on and off
:flash
   lda $d015 ;@%0000_0101
   eor @%0000_0010
   sta $d015
   lda @15
   sta temp_low
   lda $dc00
   tax
   and @joy-fire
   bne done+

   lda @state_game
   sta game_state
   lda @0
   sta inner_state
   jsr init_game:
   
:done   
   rts

:show_title
  lda $dc00
  and @joy-fire
  bne skip+
  jsr init_game:
  lda @state_game
  sta game_state
:skip
  rts


:interrupt
(play-psid breath_fire)
   lda game_state
   beq title+
   cmp @state_game
   beq main+
   jmp kitty+
  :title
   jsr show_title:
   jmp done+
  :main
   jsr play_game:
   jmp done+
  :kitty
   jsr kitty_found:
  
:done
   asl $d019 ;ack
   pla
   tay
   pla
   tax
   pla
;   dec $d020
   rti
 
   
:show_quote
   jsr clear-quote-area:

   ;right, first we load the base address of the string lookup table
   lda @<string-lookup:
   sta lookup1_low
   lda @>string-lookup:   
   sta lookup1_high
   lda @0
   tay
   sta temp_high   
   ; generate a random number to 256 (this will be read from the screen)
   ; our lookup table is 16 bits so shift once to get the actual index   
   ;jsr rng:
   lda quote_index
   clc
   sta temp_low
   rol temp_low
   rol temp_high
   ;now add it to the pointer
   clc
   lda temp_low
   adc lookup1_low
   sta lookup1_low
   lda temp_high
   adc lookup1_high
   sta lookup1_high

   ldy @0
   ;read pointer
   lda £ lookup1_low y
   sta lookup2_low
   iny
   lda £ lookup1_low y
   sta lookup2_high

   ;finally we are pointing at the string. the first byte is its size
   ldy @0
   lda £ lookup2_low y
   sta string_size

   ;setup a pointer to the top of the screen - 1
   lda @$03
   sta screen_pointer_high
   lda @$ff
   sta screen_pointer_low
   
   ; move to the first char
:write
   iny
   lda £ lookup2_low y
   sta £ screen_pointer_low y
   
   ;are we done?
   cpy string_size
   bne write-
   rts

:collision   
  ; just check the temp pointer for anytihng not a space
  ldy @0
  lda £ temp_low y
  cmp @$20
  rts

:collision-wall
  ldy @0
  lda £ temp_low y
  cmp @68
  beq done+
  cmp @70
  beq done+
  cmp @71
  beq done+
  cmp @72
:done
  rts

  
:rng ;terrible lfsr rng
    lda lfsr2
    asl
    asl
    eor lfsr2
    asl
    eor lfsr2
    asl
    asl
    eor lfsr2
    asl
    rol lfsr
    rol lfsr2
    lda lfsr
    rts

:gen-screen-loc-clear ;checks the location isn't used
    ldy @0
:loop
    jsr gen-screen-loc:    
    lda £ temp_low y
    cmp @$20
    bne loop-
    rts

:gen-screen-loc
    lda @$78   ; usable screen starts at $0478 after 3 rows ($78 chars) for the quote area and border
    sta temp_low
    lda @$04
    sta temp_high
    jsr rng:     ; 256     
    clc
    adc temp_low
    sta temp_low
    bcc skip+
    inc temp_high
:skip    jsr rng:    ; + 256
    clc
    adc temp_low
    sta temp_low
    bcc skip+
    inc temp_high
:skip jsr rng:   ; + 256
    clc
    adc temp_low
    sta temp_low
    bcc skip+
    inc temp_high
:skip
    jsr rng:
    and @%0111_1111 ; + 128
    adc temp_low
    sta temp_low
    bcc skip+
    inc temp_high
:skip
    rts
    
:clear-quote-area
    ldx @79
    ldy @1 ; white
  :clrloop
    lda @$20 ; space
    sta $0400 x
    lda @1
    sta $D800 x 
    dex
    bne clrloop-
    sta $D800 x 
    rts

:random-char
    jsr rng:
    cmp @$20
    beq random-char:
    cmp @35
    beq random-char:
    cmp @85
    beq random-char:
    cmp @71
    beq random-char:
    cmp @73
    beq random-char:
    cmp @74
    beq random-char:
    cmp @75
    beq random-char:
    rts

:random-colour ;not black
    jsr rng:
    and @$f
    beq random-colour-
    rts
    
:random-colours
   ;set usable area to random colours
    ldx @$00    
    :clrloop
    jsr random-colour:    
    sta $d800 x
    jsr random-colour:
    sta $d900 x
    jsr random-colour:
    sta $da00 x
    jsr random-colour:
    sta $db00 x
    dex         
    bne clrloop-
    rts

:white-colours
   ;set usable area to random colours
    ldx @$00
    lda @1
    :clrloop
    sta $d800 x
    sta $d900 x
    sta $da00 x
    sta $db00 x
    dex         
    bne clrloop-
    rts

:clear-screen
    lda @$00    
    sta $d020   
    sta $d021   
    tax         
    lda @$20    
  :clrloop
    sta $0400 x 
    sta $0500 x   
    sta $0600 x   
    sta $0700 x
    dex         
    bne clrloop-
    rts
    ; we have two lookup tables. one maps the screen code values
    ; to the location of the actual string in a 2 byte format.
    ;  strings themselves prefixed with their length
    ; (aways less than $ff)

:draw-border
    ; top border is the 3rd row all across the top.
    ; starts at 0x878 for 40 columns
    ; char 68 is the top border 
    ldx @40
 :loop
    lda @68    
    sta $0450 x
    dex
    bne loop-
    sta $0450 x
    lda @70 ;82 bottom border
    ldx @40
  :loop
    sta $07C0 x
    dex
    bne loop-
    sta $07C0 x

    ; left and right are 20 chars starting at 0x478 and 0x49f
    lda @71
    (for/list ([i (in-range 0 21)])
      { sta (+ $478 (* i 40)) })
    lda @72
    (for/list ([i (in-range 0 21)])
      { sta (+ $49f (* i 40))  })
    ;85 top left corner
    lda @85
    sta $0450
    ;73 top right
    lda @73
    sta $0477
    ;74 bottom left
    lda @74
    sta $07c0
    ;75 bottom right
    lda @75
    sta $07E7
    rts

:string-lookup
   ; this lookup points at the actual string data
    (let ([start (+ (label-loc string-lookup:) (* (length nkis) 2))])
      (let-values
          ([(data loc)               
            (for/fold ([location (list)]
                       [last-location start])
                      ([line nkis])                 
              (values
               (cons (arithmetic-shift last-location -8)
                (cons (bitwise-and last-location #xFF) location))
               (+ last-location (+ (string-length line) 1))))])
        (write-values (reverse data))))

    :string-data ;and this is the actual string data
    (write-values
     (for/list ([line nkis])
       (list (bitwise-and (string-length line) $ff)
             (ascii->petscii line))))
    
:kitty-found-data-1
(write-values (ascii->petscii "            robot found kitty!          "))
:kitty-found-data-2
(write-values (ascii->petscii "        press fire to start again.      "))

:title-data
(~>>
 (list
  "        www.pinksquirrellabs.com        "
  "                                        "
  "                presents                "
  "                                        "
  "            robotfindskitten            "
  "           a \"zen simulation\"           "
  "                                        "
  "          originally written by         "
  "           Leonard Richardson           "
  "                                        "
  "        Code&Design :   @pezi_pink      "
  "        Sprite GFX  : @roundcrisis      "
  "              Music :     Agemixer      "
  "                                        "
  "   Racket gamejam entry november 2019   "
  "                                        "
  "        Written with #lang asi64        "
  "                                        "
  " you are the robot. can you find kitty? "
  "                                        "
  "          press fire to start           "
  "                                        "
  "             www.crummy.com             "
  "                                        "
  "      ! BEWARE THE PINK SQUIRREL !      "
  )
 (map ascii->petscii)
 (write-values))



})
