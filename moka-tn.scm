(require "moka.scm")
(require "tokyo-night-colors.scm")

;; moka.hx configuration (Tokyo Night)

;; (bubble-set) options: #t, #f, 'angled

(define mode-match #t)
(define mode-colors
  (hash 'normal
        (hash ':bg tn_blue #:fg tn_bg)
        'insert
        (hash 'bg tn_green 'fg tn_bg)
        'select
        (hash 'bg tn_purple 'fg tn_bg)))

(define (moka-tn)
  (define bubble-set #f)
  (moka-configure!
   #:transparent? #f
   #:mode-normal "NORMAL"
   #:mode-insert "INSERT"
   #:mode-select "SELECT"
   #:sections
   (list
    ;; mode
    (moka-section (list (moka-segment 'mode #:bubble? bubble-set #:gap 0)
                        (moka-segment 'hardcoded-garbage #:bubble? #f #:gap 0))
                  #:align 'left
                  #:gap 0)
    ;; git branch and diagnostics
    (moka-section
     (list (moka-segment 'git-branch #:bg tn_terminal_black #:bubble? #f #:gap 0)
           ;; TODO: figure out how to have a hidden module such that a separator only
           ;; shows up if both sides of a separator are there
           (moka-segment 'cond-right-sep #:bg tn_terminal_black #:bubble? #f #:gap 0)
           (moka-segment 'diagnostics #:bg tn_terminal_black #:bubble? #f #:colored-icons? #t #:gap 0)
           (moka-segment 'hardcoded-right-arrow
                         #:bg tn_bg_dark
                         #:fg tn_terminal_black
                         #:bubble? #f
                         #:gap 0))
     #:align 'left
     #:gap 0)
    ;; file and spinner
    (moka-section (list (moka-segment 'file
                                      #:bg tn_bg_dark
                                      #:fg tn_fg
                                      #:bubble? bubble-set
                                      #:colored-icons? #t
                                      #:gap 0))
                  ;; TODO: uncomment when moka (ie helix steel fork) adds support for this?
                  ; (moka-segment 'spinner #:bg tn_bg_dark #:fg tn_fg #:bubble? bubble-set #:gap 0)
                  #:align 'left
                  #:gap 0)
    ;; TODO: uncomment and move into list when moka (ie helix steel fork) adds support for this
    ; (moka-segment 'file-encoding #:bg tn_bg_dark #:fg tn_fg #:bubble? #f #:gap 0)
    ; (moka-segment 'file-line-ending #:bg tn_bg_dark #:fg tn_fg #:bubble? #f #:gap 0)
    ;; file info (ie encoding, line-ending, type)
    (moka-section (list (moka-segment 'file-type
                                      #:bg tn_bg_dark
                                      #:fg tn_fg
                                      #:bubble? bubble-set
                                      #:colored-icons? #t
                                      #:gap 0))
                  #:align 'right
                  #:gap 0)
    (moka-section
     (list (moka-segment 'selections #:bg tn_terminal_black #:bubble? 'left-angled #:pad #f #:gap 0)
           (moka-segment 'left-sep #:bg tn_terminal_black #:bubble? #f #:gap 0)
           (moka-segment 'position-percentage #:bg tn_terminal_black #:bubble? #f #:pad #f #:gap 0))
     #:align 'right
     #:gap 0)
    (moka-section (list (moka-segment 'full-left-sep #:bg tn_terminal_black #:bubble? #f #:gap 0)
                        (moka-segment 'position #:fg tn_bg #:bubble? bubble-set #:gap 0))
                  #:align 'right
                  #:gap 0))
   #:mode-colors mode-colors
   #:mode-match? mode-match))

(define (moka-tn-round)
  (define bubble-set #t)
  (moka-configure!
   #:transparent? #t
   #:sections
   (list
    ;; mode
    (moka-section (list (moka-segment 'mode #:bubble? bubble-set #:gap 1)) #:align 'left #:gap 1)
    ;; git branch and diagnostics
    (moka-section
     (list (moka-segment 'git-branch #:bg tn_terminal_black #:bubble? 'cond-left-pilled #:gap 0)
           ;; TODO: figure out how to have a hidden module such that a separator only
           ;; shows up if both sides of a separator are there
           (moka-segment 'cond-right-sep #:bg tn_terminal_black #:bubble? #f #:gap 0)
           (moka-segment 'diagnostics
                         #:bg tn_terminal_black
                         #:bubble? 'cond-right-pilled
                         #:colored-icons? #t
                         #:gap 0))
     #:align 'left
     #:gap 1)
    ;; file and spinner
    (moka-section (list (moka-segment 'file
                                      #:bg tn_bg_dark
                                      #:fg tn_fg
                                      #:bubble? bubble-set
                                      #:colored-icons? #t
                                      #:gap 0))
                  ;; TODO: uncomment when moka (ie helix steel fork) adds support for this?
                  ; (moka-segment 'spinner #:bg tn_bg_dark #:fg tn_fg #:bubble? bubble-set #:gap 0)
                  #:align 'left
                  #:gap 1)
    ;; TODO: uncomment and move into list when moka (ie helix steel fork) adds support for this
    ; (moka-segment 'file-encoding #:bg tn_bg_dark #:fg tn_fg #:bubble? #f #:gap 0)
    ; (moka-segment 'file-line-ending #:bg tn_bg_dark #:fg tn_fg #:bubble? #f #:gap 0)
    ;; file info (ie encoding, line-ending, type)
    (moka-section (list (moka-segment 'file-type
                                      #:bg tn_bg_dark
                                      #:fg tn_fg
                                      #:bubble? bubble-set
                                      #:colored-icons? #t
                                      #:gap 0))
                  #:align 'right
                  #:gap 1)
    (moka-section
     (list (moka-segment 'selections #:bg tn_terminal_black #:bubble? 'left-pilled #:pad #f #:gap 0)
           (moka-segment 'left-sep #:bg tn_terminal_black #:bubble? #f #:gap 0)
           (moka-segment 'position-percentage
                         #:bg tn_terminal_black
                         #:bubble? 'right-pilled
                         #:pad #f
                         #:gap 0))
     #:align 'right
     #:gap 1)
    (moka-section (list (moka-segment 'position #:fg tn_bg #:bubble? bubble-set #:gap 0))
                  #:align 'right
                  #:gap 1))
   #:mode-colors mode-colors
   #:mode-match? mode-match))

(define (moka-tn-angled)
  (define bubble-set 'angled)
  (moka-configure!
   #:transparent? #t
   #:sections
   (list
    ;; mode
    (moka-section (list (moka-segment 'mode #:bubble? bubble-set #:gap 1)) #:align 'left #:gap 1)
    ;; git branch and diagnostics
    (moka-section
     (list (moka-segment 'git-branch #:bg tn_terminal_black #:bubble? 'cond-left-angled #:gap 0)
           ;; TODO: figure out how to have a hidden module such that a separator only
           ;; shows up if both sides of a separator are there
           (moka-segment 'cond-right-sep #:bg tn_terminal_black #:bubble? #f #:gap 0)
           (moka-segment 'diagnostics
                         #:bg tn_terminal_black
                         #:bubble? 'cond-right-angled
                         #:colored-icons? #t
                         #:gap 0))
     #:align 'left
     #:gap 1)
    ;; file and spinner
    (moka-section (list (moka-segment 'file
                                      #:bg tn_bg_dark
                                      #:fg tn_fg
                                      #:bubble? bubble-set
                                      #:colored-icons? #t
                                      #:gap 0))
                  ;; TODO: uncomment when moka (ie helix steel fork) adds support for this?
                  ; (moka-segment 'spinner #:bg tn_bg_dark #:fg tn_fg #:bubble? bubble-set #:gap 0)
                  #:align 'left
                  #:gap 1)
    ;; TODO: uncomment and move into list when moka (ie helix steel fork) adds support for this
    ; (moka-segment 'file-encoding #:bg tn_bg_dark #:fg tn_fg #:bubble? #f #:gap 0)
    ; (moka-segment 'file-line-ending #:bg tn_bg_dark #:fg tn_fg #:bubble? #f #:gap 0)
    ;; file info (ie encoding, line-ending, type)
    (moka-section (list (moka-segment 'file-type
                                      #:bg tn_bg_dark
                                      #:fg tn_fg
                                      #:bubble? bubble-set
                                      #:colored-icons #t
                                      #:gap 0))
                  #:align 'right
                  #:gap 1)
    (moka-section
     (list (moka-segment 'selections #:bg tn_terminal_black #:bubble? 'left-angled #:pad #f #:gap 0)
           (moka-segment 'left-sep #:bg tn_terminal_black #:bubble? #f #:gap 0)
           (moka-segment 'position-percentage
                         #:bg tn_terminal_black
                         #:bubble? 'right-angled
                         #:pad #f
                         #:gap 0))
     #:align 'right
     #:gap 1)
    (moka-section (list (moka-segment 'position #:fg tn_bg #:bubble? bubble-set #:gap 0))
                  #:align 'right
                  #:gap 1))
   #:mode-colors mode-colors
   #:mode-match? mode-match))

(define (moka-bl-tn bf-bubble-set icons)
  (moka-bufferline-configure! #:active
                              (moka-buffer-style #:bg tn_blue #:fg tn_bg #:bubble? bf-bubble-set)
                              #:inactive (moka-buffer-style #:bg (if bf-bubble-set tn_bg tn_bg_dark)
                                                            #:fg tn_comment
                                                            #:bubble? bf-bubble-set)
                              #:icons? icons
                              #:gap 0
                              #:colored-icons? icons))

(provide moka-tn
         moka-tn-round
         moka-tn-angled
         moka-bl-tn)
