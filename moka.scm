(require "helix/components.scm")
(require "helix/configuration.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require "helix/static.scm")
(require "helix/commands.scm")
(require "../deps/glyph.hx/glyph.scm")
(require-builtin helix/core/text as text.)

(provide moka-segment
         moka-section
         moka-configure!
         moka-enable!
         moka-disable!
         moka-refresh-git!
         moka-buffer-style
         moka-bufferline-configure!
         moka-bufferline-enable!
         moka-bufferline-disable!
         moka-reserved-top
         moka-reserved-bottom
         moka-register-segment!
         moka-current-path
         moka-current-dirty?
         moka-current-doc-id
         moka-bufferline-move-left!
         moka-bufferline-move-right!
         moka-set-forest-clip!)

(struct MokaSegment (content fg bg bubble? pad? gap colored-icons?))
(struct MokaSection (align segments gap))

(define (moka-segment content
                      #:fg [fg 'auto]
                      #:bg [bg 'auto]
                      #:bubble? [bubble? 'auto]
                      #:pad? [pad? 'auto]
                      #:gap [gap 2]
                      #:colored-icons? [colored-icons? 'auto])
  (MokaSegment content fg bg bubble? pad? gap colored-icons?))

(define (moka-section segments #:align [align 'left] #:gap [gap 2])
  (MokaSection align segments gap))

(define *moka-row-offset* 2)
(define *moka-transparent?* #t)
(define *moka-enabled?* #f)
(define *moka-hooks-registered?* #f)
(define *moka-mode-match?* #f)

(define *moka-git-branch* "")
(define *moka-git-status-map* (hash))

(define *moka-default-colors*
  (hash 'mode-fallback-bg
        "#585b70"
        'mode-fallback-fg
        "#1e1e2e"
        'git-branch-bg
        "#a6e3a1"
        'git-branch-fg
        "#1e1e2e"
        'lsp
        "#89b4fa"
        'position
        "#cba6f7"
        'dirty
        "#f9e2af"))

(define *moka-colors* *moka-default-colors*)

(define *moka-fill-spacer* "█")
(define *moka-pill-left* "")
(define *moka-pill-right* "")
(define *moka-angle-left* "")
(define *moka-angle-right* "")
;; symbols found from Lualine repo
(define *moka-angle-left-empty* "")
(define *moka-angle-right-empty* "")

(define *moka-mode-insert* (string->editor-mode "insert"))
(define *moka-mode-select* (string->editor-mode "select"))

(define *moka-mode-labels* (hash 'normal "NOR" 'insert "INS" 'select "SEL"))

(define *moka-mode-colors* (hash))

(define *moka-mode-scopes*
  (hash 'normal "ui.statusline.normal" 'insert "ui.statusline.insert" 'select "ui.statusline.select"))

(define (moka-mode-symbol)
  (cond
    [(equal? (editor-mode) *moka-mode-insert*) 'insert]
    [(equal? (editor-mode) *moka-mode-select*) 'select]
    [else 'normal]))

(define (moka-mode-label)
  (hash-try-get *moka-mode-labels* (moka-mode-symbol)))

(define (moka-mode-scope)
  (hash-try-get *moka-mode-scopes* (moka-mode-symbol)))

(define (moka-mode-color key)
  (define entry (hash-try-get *moka-mode-colors* (moka-mode-symbol)))
  (and entry (hash-try-get entry key)))

(define *moka-default-sections*
  (list (moka-section (list (moka-segment 'mode) (moka-segment 'file)) #:align 'left)
        (moka-section (list (moka-segment 'lsp) (moka-segment 'git-branch) (moka-segment 'position))
                      #:align 'right)))

(define *moka-sections* *moka-default-sections*)

(define (moka-configure! #:row-offset [row-offset 2]
                         #:transparent? [transparent? #t]
                         #:sections [sections *moka-default-sections*]
                         #:colors [colors *moka-default-colors*]
                         #:mode-normal [mode-normal "NOR"]
                         #:mode-insert [mode-insert "INS"]
                         #:mode-select [mode-select "SEL"]
                         #:mode-colors [mode-colors (hash)]
                         #:mode-match? [mode-match? #f])
  (set! *moka-row-offset* row-offset)
  (set! *moka-transparent?* transparent?)
  (set! *moka-mode-match?* mode-match?)
  (set! *moka-sections* sections)
  (set! *moka-colors* colors)
  (set! *moka-mode-labels* (hash 'normal mode-normal 'insert mode-insert 'select mode-select))
  (set! *moka-mode-colors* mode-colors))

(define (moka-base-style)
  (if *moka-transparent?*
      (theme-scope-ref "ui.background")
      (theme-scope-ref "ui.statusline")))

(define *moka-forest-side* #f)
(define *moka-forest-width* 0)

;; forest calls this when the snacks sidebar opens, moves, or closes
(define (moka-set-forest-clip! side width)
  (set! *moka-forest-side* side)
  (set! *moka-forest-width* (if (number? width) width 0)))

;; x0 and width of the text buffer, full row when forest snacks is closed
(define (moka-bar-clip width)
  (if (and *moka-forest-side* (number? *moka-forest-width*) (> *moka-forest-width* 0))
      (if (equal? *moka-forest-side* 'left)
          (list *moka-forest-width* (max 1 (- width *moka-forest-width*)))
          (if (equal? *moka-forest-side* 'right)
              (list 0 (max 1 (- width *moka-forest-width*)))
              (list 0 width)))
      (list 0 width)))

(define hardcoded-garbage-color "#414868")
(define (hardcoded-garbage-bg)
  (define git-branch-out (moka-git-branch-content))
  (define diagnostic-out (moka-diagnostics-content))
  (if (and (equal? git-branch-out "") (equal? diagnostic-out "")) #f hardcoded-garbage-color))

;; 'mode prefers #:mode-colors, then theme colors, then the fallback hash
(define (moka-default-bg content)
  (define git-branch-out (moka-git-branch-content))
  (cond
    [(equal? content 'mode)
     (or (moka-mode-color 'bg)
         (style->bg (theme-scope-ref (moka-mode-scope)))
         (hash-try-get *moka-colors* 'mode-fallback-bg))]
    ;; CUSTOM CHANGE TO EMULATE TOKYO NIGHT NVIM (LUALINE)
    [(equal? content 'hardcoded-garbage) (hardcoded-garbage-bg)]
    [(and (equal? content 'position) *moka-mode-match?*)
     (or (moka-mode-color 'bg)
         (style->bg (theme-scope-ref (moka-mode-scope)))
         (hash-try-get *moka-colors* 'mode-fallback-bg))]
    [(equal? content 'git-branch) (hash-try-get *moka-colors* 'git-branch-bg)]
    [else #f]))

(define (moka-default-fg content)
  (cond
    [(equal? content 'mode)
     (or (moka-mode-color 'fg)
         (style->fg (theme-scope-ref (moka-mode-scope)))
         (hash-try-get *moka-colors* 'mode-fallback-fg))]
    ;; if mode match is enabled, use that logic
    [(and (equal? content 'git-branch) *moka-mode-match?*)
     (or (moka-mode-color 'bg)
         (style->bg (theme-scope-ref (moka-mode-scope)))
         (hash-try-get *moka-colors* 'mode-fallback-bg))]
    ;; if mode match is disabled, use the default logic
    [(equal? content 'git-branch) (hash-try-get *moka-colors* 'git-branch-fg)]
    [(equal? content 'lsp) (hash-try-get *moka-colors* 'lsp)]
    ;; CUSTOM CHANGE TO EMULATE TOKYO NIGHT NVIM (includes hardcoded garbage)
    [(and (or (equal? content 'position-percentage)
              (equal? content 'diagnostics)
              (equal? content 'selections)
              (equal? content 'separator)
              (equal? content 'left-sep)
              (equal? content 'right-sep)
              (equal? content 'full-left-sep)
              (equal? content 'full-right-sep)
              (equal? content 'cond-sep)
              (equal? content 'cond-left-sep)
              (equal? content 'cond-right-sep)
              (equal? content 'cond-full-left-sep)
              (equal? content 'cond-full-right-sep)
              (equal? content 'hardcoded-garbage)))
     (or (moka-mode-color 'bg)
         (style->bg (theme-scope-ref (moka-mode-scope)))
         (hash-try-get *moka-colors* 'mode-fallback-bg))]
    [(equal? content 'position) (hash-try-get *moka-colors* 'position)]
    [else #f]))

(define (moka-resolve value default-thunk)
  (if (equal? value 'auto)
      (default-thunk)
      value))

;; hex string or already-built Color?
(define (moka-to-color value)
  (if (string? value)
      (glyph-hex->color value)
      value))

(define (cond-bubble bubble?)
  ;; TODO: figure out how to not shamelessly hard code this?
  (define git-branch-out (moka-git-branch-content))
  (define diagnostic-out (moka-diagnostics-content))
  (if (or (equal? git-branch-out "") (equal? diagnostic-out ""))
      (cond
        [(or (equal? bubble? 'cond-left-pilled) (equal? bubble? 'cond-right-pilled))
         (cons *moka-pill-left* *moka-pill-right*)]
        [(or (equal? bubble? 'cond-left-angled) (equal? bubble? 'cond-right-angled))
         (cons *moka-angle-left* *moka-angle-right*)])
      (cond
        [(equal? bubble? 'cond-left-pilled) (cons *moka-pill-left* "")]
        [(equal? bubble? 'cond-right-pilled) (cons "" *moka-pill-right*)]
        [(equal? bubble? 'cond-left-angled) (cons *moka-angle-left* "")]
        [(equal? bubble? 'cond-right-angled) (cons "" *moka-angle-right*)])))

(define (moka-caps-for bg bubble?)
  (cond
    [(not bg) #f]
    [(equal? bubble? #t) (cons *moka-pill-left* *moka-pill-right*)]
    [(equal? bubble? 'left-pilled) (cons *moka-pill-left* "")]
    [(equal? bubble? 'right-pilled) (cons "" *moka-pill-right*)]
    [(equal? bubble? 'angled) (cons *moka-angle-left* *moka-angle-right*)]
    [(equal? bubble? 'left-angled) (cons *moka-angle-left* "")]
    [(equal? bubble? 'right-angled) (cons "" *moka-angle-right*)]
    [(or (equal? bubble? 'cond-left-pilled)
         (equal? bubble? 'cond-right-pilled)
         (equal? bubble? 'cond-left-angled)
         (equal? bubble? 'cond-right-angled))
     (cond-bubble bubble?)]
    [else #f]))

(define (moka-runs-for fragments bg fallback-fg colored?)
  (define fg (or fallback-fg (and bg "#1e1e2e")))
  (if colored?
      (map (lambda (frag) (cons (car frag) (or (cdr frag) fg))) fragments)
      (map (lambda (frag) (cons (car frag) fg)) fragments)))

(define (moka-fragments-text fragments)
  (string-join (map car fragments) ""))

(define (moka-pad-fragments fragments bg pad?)
  (if (and bg pad?)
      (append (list (cons " " #f)) fragments (list (cons " " #f)))
      fragments))

(define (moka-styled-width fragments bg bubble? pad?)
  (define caps (moka-caps-for bg bubble?))
  (+ (string-length (moka-fragments-text (moka-pad-fragments fragments bg pad?)))
     (cond
       [(not caps) 0]
       [(or (equal? (car caps) "") (equal? (cdr caps) "")) 1]
       [else 2])))

(define (moka-draw-styled! frame x y fragments bg bubble? pad? fallback-fg colored? base-style)
  (define bg-style
    (if bg
        (style-bg base-style (moka-to-color bg))
        base-style))
  (define caps (moka-caps-for bg bubble?))
  (define cap-style
    (if bg
        (style-fg base-style (moka-to-color bg))
        base-style))
  (define start-x
    (if (and caps (not (equal? (car caps) "")))
        (begin
          (frame-set-string! frame x y (car caps) cap-style)
          (+ x 1))
        x))
  (define end-x
    (let loop ([runs (moka-runs-for (moka-pad-fragments fragments bg pad?) bg fallback-fg colored?)]
               [cx start-x])
      (if (null? runs)
          cx
          (let* ([run (car runs)]
                 [text (car run)]
                 [fg (cdr run)]
                 [run-style (if fg
                                (style-fg bg-style (moka-to-color fg))
                                bg-style)])
            (frame-set-string! frame cx y text run-style)
            (loop (cdr runs) (+ cx (string-length text)))))))
  (if (and caps (not (equal? (cdr caps) "")))
      (begin
        (frame-set-string! frame end-x y (cdr caps) cap-style)
        (+ end-x 1))
      end-x))

(define (moka-segment-bg segment)
  (moka-resolve (MokaSegment-bg segment) (lambda () (moka-default-bg (MokaSegment-content segment)))))

(define (moka-segment-fallback-fg segment)
  (moka-resolve (MokaSegment-fg segment) (lambda () (moka-default-fg (MokaSegment-content segment)))))

;; mode/git-branch default to round pills
(define (moka-default-bubble? content)
  (cond
    [(equal? content 'mode) #t]
    [(equal? content 'git-branch) #t]
    [else #f]))

;; no padding for separator (it's already padding)
(define (moka-default-pad? content)
  (cond
    [(equal? content 'spacing) #f]
    [(equal? content 'separator) #f]
    [(equal? content 'left-sep) #f]
    [(equal? content 'right-sep) #f]
    [(equal? content 'full-left-sep) #f]
    [(equal? content 'full-right-sep) #f]
    [(equal? content 'cond-sep) #f]
    [(equal? content 'cond-left-sep) #f]
    [(equal? content 'cond-right-sep) #f]
    [(equal? content 'cond-full-left-sep) #f]
    [(equal? content 'cond-full-right-sep) #f]
    [(equal? content 'hardcoded-garbage) #f]
    [(equal? content 'hardcoded-right-arrow) #f]
    [else #t]))

(define (moka-segment-bubble? segment)
  (moka-resolve (MokaSegment-bubble? segment)
                (lambda () (moka-default-bubble? (MokaSegment-content segment)))))

(define (moka-segment-pad? segment)
  (moka-resolve (MokaSegment-pad? segment)
                (lambda () (moka-default-pad? (MokaSegment-content segment)))))

;; auto makes icon colored only on transparent background else have fg color
(define (moka-segment-colored-icons? segment bg)
  (moka-resolve (MokaSegment-colored-icons? segment) (lambda () (not bg))))

;; read live, no caching
(define (moka-current-path)
  (with-handler (lambda (_) #f) (cx->current-file)))

(define (moka-current-dirty?)
  (with-handler (lambda (_) #f) (editor-document-dirty? (editor->doc-id (editor-focus)))))

(define (moka-current-doc-id)
  (with-handler (lambda (_) #f) (editor->doc-id (editor-focus))))

(define (moka-current-lsp-names)
  (with-handler (lambda (_) '()) (map lsp-client-name (get-active-lsp-clients))))

(define (moka-git-repo? dir)
  (with-handler (lambda (_) #f)
                (define proc
                  (~> (command "git" (list "-C" dir "rev-parse" "--is-inside-work-tree"))
                      with-stdout-piped
                      with-stderr-piped
                      spawn-process))
                (and (Ok? proc)
                     (equal? (trim (read-port-to-string (child-stdout (Ok->value proc)))) "true"))))

(define (moka-read-branch dir)
  (with-handler (lambda (_) "")
                (define proc
                  (~> (command "git" (list "-C" dir "rev-parse" "--abbrev-ref" "HEAD"))
                      with-stdout-piped
                      with-stderr-piped
                      spawn-process))
                (if (Ok? proc)
                    (trim (read-port-to-string (child-stdout (Ok->value proc))))
                    "")))

(define (moka-git-status-symbol code)
  (define x (string-ref code 0))
  (define y (string-ref code 1))
  (cond
    [(and (char=? x #\?) (char=? y #\?)) 'untracked]
    [(or (char=? x #\A) (char=? y #\A)) 'added]
    [(or (char=? x #\D) (char=? y #\D)) 'deleted]
    [(or (char=? x #\R) (char=? y #\R)) 'renamed]
    [(or (char=? x #\M) (char=? y #\M)) 'modified]
    [else #f]))

(define (moka-status-path rest)
  (define parts (split-many rest " -> "))
  (trim-end-matches (if (> (length parts) 1)
                        (list-ref parts (- (length parts) 1))
                        rest)
                    (path-separator)))

(define (moka-parse-status-lines lines)
  (let loop ([ls lines]
             [statuses (hash)])
    (if (null? ls)
        statuses
        (let ([line (car ls)])
          (if (< (string-length line) 3)
              (loop (cdr ls) statuses)
              (let* ([code (substring line 0 2)]
                     [path (moka-status-path (trim (substring line 3 (string-length line))))]
                     [sym (moka-git-status-symbol code)])
                (loop (cdr ls)
                      (if sym
                          (hash-insert statuses path sym)
                          statuses))))))))

(define (moka-scan-git-status root)
  (with-handler (lambda (_) (hash))
                (define proc
                  (~> (command "git" (list "-C" root "status" "--porcelain"))
                      with-stdout-piped
                      with-stderr-piped
                      spawn-process))
                (if (Ok? proc)
                    (let* ([output (read-port-to-string (child-stdout (Ok->value proc)))]
                           [lines (filter (lambda (l) (> (string-length l) 0))
                                          (split-many output "\n"))])
                      (moka-parse-status-lines lines))
                    (hash))))

;; shells out to git, so only on save/open/focus, not every keystroke
(define (moka-refresh-git!)
  (define root (with-handler (lambda (_) #f) (helix-find-workspace)))
  (if (and root (moka-git-repo? root))
      (begin
        (set! *moka-git-branch* (moka-read-branch root))
        (set! *moka-git-status-map* (moka-scan-git-status root)))
      (begin
        (set! *moka-git-branch* "")
        (set! *moka-git-status-map* (hash))))
  (redraw))

(define (moka-relpath path)
  (define prefix (string-append (helix-find-workspace) (path-separator)))
  (if (and (>= (string-length path) (string-length prefix))
           (equal? (substring path 0 (string-length prefix)) prefix))
      (substring path (string-length prefix) (string-length path))
      path))

(define (moka-current-git-status path)
  (and path (hash-try-get *moka-git-status-map* (moka-relpath path))))

(define (moka-mode-content)
  (moka-mode-label))

;; icon/status/name keep their own color
(define (moka-file-content)
  (define path (moka-current-path))
  (if (not path)
      ""
      (let* ([name (file-name path)]
             [status (moka-current-git-status path)]
             ; [base (list (cons (glyph-icon name) (glyph-color name)) (cons " " #f))]
             [base (list (cons "" #f))]
             [with-status (if status
                              (append base
                                      (list (cons (glyph-git-icon status) (glyph-git-color status))
                                            (cons " " #f)))
                              base)]
             [with-name (append with-status (list (cons name #f)))])
        (if (moka-current-dirty?)
            (append with-name (list (cons " " #f) (cons "*" (hash-try-get *moka-colors* 'dirty))))
            with-name))))

(define (moka-git-branch-content)
  *moka-git-branch*)

(define (moka-lsp-content)
  (string-join (moka-current-lsp-names) ", "))

(define (moka-position-content)
  (string-append (number->string (+ 1 (get-current-line-number)))
                 ":"
                 (number->string (+ 1 (get-current-column-number)))))

(define (moka-spacer-content)
  " ")
(define (moka-separator-content)
  "|")
(define (moka-arrowed-separator left)
  (cond
    [left *moka-angle-left-empty*]
    [else *moka-angle-right-empty*]))
(define (moka-full-arrowed-separator left)
  (cond
    [left *moka-angle-left*]
    [else *moka-angle-right*]))
(define (moka-left-sep-content)
  (moka-arrowed-separator #t))
(define (moka-right-sep-content)
  (moka-arrowed-separator #f))
(define (moka-full-left-sep-content)
  (moka-full-arrowed-separator #t))
(define (moka-full-right-sep-content)
  (moka-full-arrowed-separator #f))

(define (moka-conditional-separator handler)
  ;; TODO: figure out how to not hard code this?
  (define git-branch-out (moka-git-branch-content))
  (define diagnostic-out (moka-diagnostics-content))
  (if (or (equal? git-branch-out "") (equal? diagnostic-out ""))
      ""
      (handler)))

(define (moka-hardcoded-garbage-sep handler)
  (define git-branch-out (moka-git-branch-content))
  (define diagnostic-out (moka-diagnostics-content))
  (if (and (equal? git-branch-out "") (equal? diagnostic-out ""))
      ""
      (handler)))

(define (moka-cond-space-content)
  (moka-conditional-separator moka-spacer-content))

(define (moka-cond-sep-content)
  (moka-conditional-separator moka-separator-content))

(define (moka-cond-left-sep-content)
  (moka-conditional-separator moka-left-sep-content))

(define (moka-cond-right-sep-content)
  (moka-conditional-separator moka-right-sep-content))

(define (moka-cond-full-left-sep-content)
  (moka-conditional-separator moka-full-left-sep-content))

(define (moka-cond-full-right-sep-content)
  (moka-conditional-separator moka-full-right-sep-content))

(define (moka-hardcoded-right-arrow-garbage)
  (moka-hardcoded-garbage-sep moka-full-right-sep-content))

(define (moka-file-absolute-path-content)
  (define path (moka-current-path))
  (if path
      (string-append " " path " ")
      ""))

(define (moka-last-index-of s ch)
  (let loop ([i (- (string-length s) 1)])
    (cond
      [(< i 0) #f]
      [(char=? (string-ref s i) ch) i]
      [else (loop (- i 1))])))

;; strips the extension, keeping dotfiles like .gitignore whole
(define (moka-file-stem name)
  (define dot (moka-last-index-of name #\.))
  (if (and dot (> dot 0))
      (substring name 0 dot)
      name))

(define (moka-file-base-name-content)
  (define path (moka-current-path))
  (if path
      (string-append " " (moka-file-stem (file-name path)) " ")
      ""))

(define (moka-file-modification-indicator-content)
  (if (moka-current-dirty?) " [+] " ""))

(define (moka-total-line-numbers-content)
  (define doc (moka-current-doc-id))
  (if (not doc)
      ""
      (let ([rope (with-handler (lambda (_) #f) (editor->text doc))])
        (if rope
            (string-append " " (number->string (text.rope-len-lines rope)) " ")
            ""))))

(define (moka-file-type-content)
  (define path (moka-current-path))
  (define name
    (if path
        (file-name path)
        ""))
  (define doc (moka-current-doc-id))
  (define lang (and doc (with-handler (lambda (_) #f) (editor-document->language doc))))
  (if lang
      (let ([base (list (cons (glyph-icon name) (glyph-color name)) (cons " " #f))])
        (append base (list (cons lang #f) (cons "" #f))))
      " text"))

(define (moka-selections-content)
  (define sel (with-handler (lambda (_) #f) (current-selection-object)))
  (if (not sel)
      ""
      (let ([count (length (selection->ranges sel))])
        (if (= count 1)
            "1 sel"
            (string-append ""
                           (number->string (+ 1 (selection->primary-index sel)))
                           "/"
                           (number->string count)
                           " sels")))))

(define (moka-primary-selection-length-content)
  (define sel (with-handler (lambda (_) #f) (current-selection-object)))
  (if (not sel)
      ""
      (let* ([primary (selection->primary-range sel)]
             [len (- (range->to primary) (range->from primary))])
        (string-append " " (number->string len) (if (= len 1) " char " " chars ")))))

(define (moka-position-percentage-content)
  (define doc (moka-current-doc-id))
  (if (not doc)
      ""
      (let ([rope (with-handler (lambda (_) #f) (editor->text doc))])
        (if (not rope)
            ""
            (let ([total (text.rope-len-lines rope)])
              (if (<= total 0)
                  ""
                  (string-append (number->string (quotient (* (+ 1 (get-current-line-number)) 100)
                                                           total))
                                 "%")))))))

(define (moka-register-content)
  (define reg (with-handler (lambda (_) #f) (selected-register!)))
  (if reg
      (string-append " reg=" (string reg) " ")
      ""))

(define (moka-diagnostics-content)
  (define doc (moka-current-doc-id))
  (if (not doc)
      ""
      (let ([counts (with-handler (lambda (_) #f) (editor-document-diagnostic-counts doc))])
        (if (not counts)
            ""
            (let ([errors (list-ref counts 3)]
                  [warnings (list-ref counts 2)]
                  [info (list-ref counts 1)]
                  [hints (list-ref counts 0)])
              (let loop ([entries (list (list errors 'error)
                                        (list warnings 'alert)
                                        (list info 'information)
                                        (list hints 'check))]
                         [frags '()])
                (if (null? entries)
                    (if (null? frags) "" frags)
                    (let* ([entry (car entries)]
                           [count (list-ref entry 0)]
                           [icon-key (list-ref entry 1)])
                      (if (= count 0)
                          (loop (cdr entries) frags)
                          (let* ([color (glyph-ui-color icon-key)]
                                 [text
                                  (string-append (glyph-ui-icon icon-key) " " (number->string count))]
                                 [sep (if (null? frags)
                                          '()
                                          (list (cons " " #f)))]
                                 [new-frags (append frags sep (list (cons text color)))])
                            (loop (cdr entries) new-frags)))))))))))

(define *moka-content-registry*
  (hash 'mode
        moka-mode-content
        'file
        moka-file-content
        'git-branch
        moka-git-branch-content
        'lsp
        moka-lsp-content
        'position
        moka-position-content
        'spacer
        moka-spacer-content
        'separator
        moka-separator-content
        'left-sep
        moka-left-sep-content
        'right-sep
        moka-right-sep-content
        'full-left-sep
        moka-full-left-sep-content
        'full-right-sep
        moka-full-right-sep-content
        'cond-space
        moka-cond-space-content
        'cond-sep
        moka-cond-sep-content
        'cond-left-sep
        moka-cond-left-sep-content
        'cond-right-sep
        moka-cond-right-sep-content
        'cond-full-left-sep
        moka-cond-full-left-sep-content
        'cond-full-right-sep
        moka-cond-full-right-sep-content
        'hardcoded-garbage
        moka-full-right-sep-content
        'hardcoded-right-arrow
        moka-hardcoded-right-arrow-garbage
        'file-absolute-path
        moka-file-absolute-path-content
        'file-base-name
        moka-file-base-name-content
        'file-modification-indicator
        moka-file-modification-indicator-content
        'total-line-numbers
        moka-total-line-numbers-content
        'file-type
        moka-file-type-content
        'selections
        moka-selections-content
        'primary-selection-length
        moka-primary-selection-length-content
        'position-percentage
        moka-position-percentage-content
        'register
        moka-register-content
        'diagnostics
        moka-diagnostics-content))

;; lets other .scm files add named segments, same as the built-ins above
(define (moka-register-segment! key handler)
  (set! *moka-content-registry* (hash-insert *moka-content-registry* key handler)))

(define (moka-content-fragments segment)
  (define content (MokaSegment-content segment))
  (define raw
    (cond
      [(symbol? content)
       (define handler (hash-try-get *moka-content-registry* content))
       (if handler
           (handler)
           "")]
      [(procedure? content) (content)]
      [else ""]))
  (if (string? raw)
      (list (cons raw #f))
      raw))

(define (moka-segment-text segment)
  (moka-fragments-text (moka-content-fragments segment)))

(define (moka-segment-width segment)
  (moka-styled-width (moka-content-fragments segment)
                     (moka-segment-bg segment)
                     (moka-segment-bubble? segment)
                     (moka-segment-pad? segment)))

(define (moka-sections-for align)
  (filter (lambda (sec) (equal? (MokaSection-align sec) align)) *moka-sections*))

;; caps alone don't count as content, drop segments with no actual text
(define (moka-section-segments-nonempty section)
  (filter (lambda (seg) (not (equal? (moka-segment-text seg) ""))) (MokaSection-segments section)))

;; adds gap-of after every item except last section
(define (moka-sum-with-gaps items width-of gap-of)
  (if (null? items)
      0
      (let loop ([xs items]
                 [total 0])
        (if (null? (cdr xs))
            (+ total (width-of (car xs)))
            (loop (cdr xs) (+ total (width-of (car xs)) (gap-of (car xs))))))))

(define (moka-section-width section)
  (moka-sum-with-gaps (moka-section-segments-nonempty section) moka-segment-width MokaSegment-gap))

(define (moka-sections-nonempty align)
  (filter (lambda (sec) (> (moka-section-width sec) 0)) (moka-sections-for align)))

(define (moka-align-width align)
  (moka-sum-with-gaps (moka-sections-nonempty align) moka-section-width MokaSection-gap))

(define (moka-draw-segment! frame x y segment base-style)
  (define bg (moka-segment-bg segment))
  (moka-draw-styled! frame
                     x
                     y
                     (moka-content-fragments segment)
                     bg
                     (moka-segment-bubble? segment)
                     (moka-segment-pad? segment)
                     (moka-segment-fallback-fg segment)
                     (moka-segment-colored-icons? segment bg)
                     base-style))

(define (moka-draw-section! frame x y section base-style)
  (let loop ([segs (moka-section-segments-nonempty section)]
             [cx x])
    (if (null? segs)
        cx
        (let* ([seg (car segs)]
               [next-cx (moka-draw-segment! frame cx y seg base-style)])
          (if (null? (cdr segs))
              next-cx
              (loop (cdr segs) (+ next-cx (MokaSegment-gap seg))))))))

(define (moka-draw-align! frame x y align base-style)
  (let loop ([secs (moka-sections-nonempty align)]
             [cx x])
    (if (null? secs)
        cx
        (let* ([sec (car secs)]
               [next-cx (moka-draw-section! frame cx y sec base-style)])
          (if (null? (cdr secs))
              next-cx
              (loop (cdr secs) (+ next-cx (MokaSection-gap sec))))))))

(define (moka-render-bar state rect frame)
  (with-handler
   (lambda (_) void)
   (define width (area-width rect))
   (define height (area-height rect))
   (define clip (moka-bar-clip width))
   (define x0 (car clip))
   (define bw (cadr clip))
   ;; clamped so offset 0 can't land off-screen
   (define y (max 0 (min (- height 1) (- height *moka-row-offset*))))
   (define base-style (moka-base-style))
   ;; the blanked native statusline still paints its bg
   ;; remove the row when the statusline is displayed on another line
   (define native-y (- height 2))
   (when (and (>= native-y 0) (not (= y native-y)))
     (buffer/clear-with frame (area x0 native-y bw 1) base-style))
   (buffer/clear-with frame (area x0 y bw 1) base-style)
   (moka-draw-align! frame x0 y 'left base-style)
   (define center-width (moka-align-width 'center))
   (when (> center-width 0)
     (moka-draw-align! frame (+ x0 (quotient (- bw center-width) 2)) y 'center base-style))
   (define right-width (moka-align-width 'right))
   (when (> right-width 0)
     (moka-draw-align! frame (- (+ x0 bw) right-width) y 'right base-style))))

(define (moka-cursor-handler state rect)
  #f)

(define (moka-blank-native-statusline!)
  (statusline #:left '() #:center '() #:right '()))

(define (moka-restore-native-statusline!)
  (statusline))

;; add the statusline to the screen once. popups remove only themselves
;; when they close, so nothing here can remove the bar
(define (moka-ensure-component!)
  (set! *moka-enabled?* #t)
  (push-component! (new-component! "moka" #f moka-render-bar (hash "cursor" moka-cursor-handler))))

(define (moka-register-hooks!)
  (unless *moka-hooks-registered?*
    (register-hook 'document-saved (lambda (doc-id) (moka-refresh-git!)))
    (register-hook 'document-opened (lambda (doc-id) (moka-refresh-git!)))
    (register-hook 'terminal-focus-gained (lambda () (moka-refresh-git!)))
    (set! *moka-hooks-registered?* #t)))

(define (moka-enable!)
  (unless *moka-enabled?*
    (moka-blank-native-statusline!)
    (moka-register-hooks!)
    (moka-refresh-git!)
    (moka-ensure-component!)
    (set! *moka-enabled?* #t)))

(define (moka-disable!)
  (when *moka-enabled?*
    (pop-last-component-by-name! "moka")
    (moka-restore-native-statusline!)
    (set! *moka-enabled?* #f)))

(struct MokaBufferStyle (fg bg bubble?))

(define (moka-buffer-style #:fg [fg 'auto] #:bg [bg 'auto] #:bubble? [bubble? 'auto])
  (MokaBufferStyle fg bg bubble?))

(define *moka-bufferline-enabled?* #f)
(define *moka-bufferline-hooks-registered?* #f)
(define *moka-bufferline-mode* 'multiple)
(define *moka-bufferline-row-offset* 0)
(define *moka-bufferline-gap* 1)
(define *moka-bufferline-show-icons?* #t)
(define *moka-bufferline-colored-icons?* 'auto)
(define *moka-bufferline-show-dirty?* #t)
(define *moka-bufferline-dirty-color* "#f9e2af")
(define *moka-bufferline-clickable?* #t)
(define *moka-bufferline-active-style* (moka-buffer-style #:bg "#89b4fa" #:fg "#1e1e2e" #:bubble? #t))
(define *moka-bufferline-inactive-style* (moka-buffer-style))

(define *moka-bufferline-tabs* '())

;; #:mode mirrors helix's own buferline settings never/always/multiple
(define (moka-bufferline-configure!
         #:row-offset [row-offset 0]
         #:mode [mode 'multiple]
         #:active [active (moka-buffer-style #:bg "#89b4fa" #:fg "#1e1e2e" #:bubble? #t)]
         #:inactive [inactive (moka-buffer-style)]
         #:gap [gap 1]
         #:icons? [icons? #t]
         #:colored-icons? [colored-icons? 'auto]
         #:dirty? [dirty? #t]
         #:dirty-color [dirty-color "#f9e2af"]
         #:clickable? [clickable? #t])
  (set! *moka-bufferline-row-offset* row-offset)
  (set! *moka-bufferline-mode* mode)
  (set! *moka-bufferline-active-style* active)
  (set! *moka-bufferline-inactive-style* inactive)
  (set! *moka-bufferline-gap* gap)
  (set! *moka-bufferline-show-icons?* icons?)
  (set! *moka-bufferline-colored-icons?* colored-icons?)
  (set! *moka-bufferline-show-dirty?* dirty?)
  (set! *moka-bufferline-dirty-color* dirty-color)
  (set! *moka-bufferline-clickable?* clickable?))

(define (moka-bufferline-style-for active?)
  (if active? *moka-bufferline-active-style* *moka-bufferline-inactive-style*))

;; 'auto keeps icon colors only on tabs without a bg, same rule as the statusline
(define (moka-bufferline-tab-colored-icons? bg)
  (moka-resolve *moka-bufferline-colored-icons?* (lambda () (not bg))))

(define (moka-bufferline-tab-bg style)
  (moka-resolve (MokaBufferStyle-bg style) (lambda () #f)))
(define (moka-bufferline-tab-fg style)
  (moka-resolve (MokaBufferStyle-fg style) (lambda () #f)))
(define (moka-bufferline-tab-bubble? style)
  (moka-resolve (MokaBufferStyle-bubble? style) (lambda () #f)))

(define (moka-bufferline-doc-fragments doc-id)
  (define path (with-handler (lambda (_) #f) (editor-document->path doc-id)))
  (define name
    (if path
        (file-name path)
        "[scratch]"))
  (define icon-frag
    (if (and *moka-bufferline-show-icons?* path)
        (list (cons (glyph-icon name) (glyph-color name)) (cons " " #f))
        '()))
  (define dirty?
    (and *moka-bufferline-show-dirty?*
         (with-handler (lambda (_) #f) (editor-document-dirty? doc-id))))
  (define dirty-frag
    (if dirty?
        (list (cons " " #f) (cons "*" *moka-bufferline-dirty-color*))
        '()))
  (append icon-frag (list (cons name #f)) dirty-frag))

;; user-set tab order, synced against the live doc set on every render
(define *moka-bufferline-order* '())

(define (moka-bufferline-list-contains? lst x)
  (cond
    [(null? lst) #f]
    [(equal? (car lst) x) #t]
    [else (moka-bufferline-list-contains? (cdr lst) x)]))

(define (moka-bufferline-sync-order!)
  (define live (with-handler (lambda (_) '()) (editor-all-documents)))
  (define still-open
    (filter (lambda (d) (moka-bufferline-list-contains? live d)) *moka-bufferline-order*))
  (define new-docs (filter (lambda (d) (not (moka-bufferline-list-contains? still-open d))) live))
  (set! *moka-bufferline-order* (append still-open new-docs)))

(define (moka-bufferline-docs)
  (moka-bufferline-sync-order!)
  *moka-bufferline-order*)

(define (moka-bufferline-index-of lst target)
  (let loop ([xs lst]
             [i 0])
    (cond
      [(null? xs) #f]
      [(equal? (car xs) target) i]
      [else (loop (cdr xs) (+ i 1))])))

(define (moka-bufferline-swap-indices lst i j)
  (define a (list-ref lst i))
  (define b (list-ref lst j))
  (let loop ([xs lst]
             [k 0]
             [acc '()])
    (cond
      [(null? xs) (reverse acc)]
      [(= k i) (loop (cdr xs) (+ k 1) (cons b acc))]
      [(= k j) (loop (cdr xs) (+ k 1) (cons a acc))]
      [else (loop (cdr xs) (+ k 1) (cons (car xs) acc))])))

;; swaps the focused buffer's tab with its left (-1) or right (+1) neighbor
(define (moka-bufferline-move! delta)
  (define focused (moka-current-doc-id))
  (when focused
    (define order (moka-bufferline-docs))
    (define i (moka-bufferline-index-of order focused))
    (define j (and i (+ i delta)))
    (when (and i j (>= j 0) (< j (length order)))
      (set! *moka-bufferline-order* (moka-bufferline-swap-indices order i j))
      (redraw))))

(define (moka-bufferline-move-left!)
  (moka-bufferline-move! -1))
(define (moka-bufferline-move-right!)
  (moka-bufferline-move! 1))

(define (moka-bufferline-focused-doc-id)
  (with-handler (lambda (_) #f) (editor->doc-id (editor-focus))))

(define (moka-bufferline-visible?)
  (cond
    [(equal? *moka-bufferline-mode* 'never) #f]
    [(equal? *moka-bufferline-mode* 'always) #t]
    [else (> (length (moka-bufferline-docs)) 1)]))

;; syncs the reserved top row from hooks
(define (moka-bufferline-sync-clip!)
  (set-editor-clip-top! (if (moka-bufferline-visible?)
                            (+ *moka-bufferline-row-offset* 1)
                            0)))

(define (moka-render-bufferline state rect frame)
  (with-handler
   (lambda (_) void)
   (when (moka-bufferline-visible?)
     (define width (area-width rect))
     (define clip (moka-bar-clip width))
     (define x0 (car clip))
     (define bw (cadr clip))
     (define y *moka-bufferline-row-offset*)
     (define base-style (moka-base-style))
     (buffer/clear-with frame (area x0 y bw 1) base-style)
     (define focused (moka-bufferline-focused-doc-id))
     (let loop ([docs (moka-bufferline-docs)]
                [cx x0]
                [tabs '()])
       (if (or (null? docs) (>= cx (+ x0 bw)))
           (set! *moka-bufferline-tabs* (reverse tabs))
           (let* ([doc-id (car docs)]
                  [style (moka-bufferline-style-for (equal? doc-id focused))]
                  [fragments (moka-bufferline-doc-fragments doc-id)]
                  [tab-bg (moka-bufferline-tab-bg style)]
                  [tab-bubble? (moka-bufferline-tab-bubble? style)]
                  [tab-pad? #t] ;; TODO: make a function for this?
                  [tab-width (moka-styled-width fragments tab-bg tab-bubble? tab-pad?)]
                  [start-x cx])
             ;; tabs stop at the sidebar edge like the rest of the bar
             (if (> (+ cx tab-width) (+ x0 bw))
                 (set! *moka-bufferline-tabs* (reverse tabs))
                 (let ([end-x (moka-draw-styled! frame
                                                 cx
                                                 y
                                                 fragments
                                                 tab-bg
                                                 tab-bubble?
                                                 tab-pad?
                                                 (moka-bufferline-tab-fg style)
                                                 (moka-bufferline-tab-colored-icons? tab-bg)
                                                 base-style)])
                   (loop (cdr docs)
                         (+ end-x *moka-bufferline-gap*)
                         (cons (list doc-id start-x end-x) tabs))))))))))

(define (moka-bufferline-cursor-handler state rect)
  #f)

(define (moka-bufferline-tab-at col)
  (define hit
    (filter (lambda (tab) (and (>= col (cadr tab)) (< col (caddr tab)))) *moka-bufferline-tabs*))
  (if (null? hit)
      #f
      (car (car hit))))

(define (moka-bufferline-handle-event state event)
  (if (and *moka-bufferline-clickable?*
           (mouse-event? event)
           (equal? (event-mouse-kind event) 0)
           (equal? (event-mouse-row event) *moka-bufferline-row-offset*))
      (let ([doc-id (moka-bufferline-tab-at (event-mouse-col event))])
        (when doc-id
          (with-handler (lambda (_) #f) (editor-switch-action! doc-id Action/Replace)))
        event-result/consume)
      event-result/ignore))

;; add the bufferline to the screen once. popups remove only themselves
;; when they close, so nothing here can remove the bar
(define (moka-bufferline-ensure-component!)
  (set! *moka-bufferline-enabled?* #t)
  (push-component!
   (new-component!
    "moka-bufferline"
    #f
    moka-render-bufferline
    (hash "cursor" moka-bufferline-cursor-handler "handle_event" moka-bufferline-handle-event))))

(define (moka-bufferline-register-hooks!)
  (unless *moka-bufferline-hooks-registered?*
    (register-hook 'document-opened (lambda (doc-id) (moka-bufferline-sync-clip!)))
    (register-hook 'document-closed (lambda (closed-event) (moka-bufferline-sync-clip!)))
    (set! *moka-bufferline-hooks-registered?* #t)))

;; deactive helix default bufferline
(define (moka-bufferline-blank-native!)
  (with-handler (lambda (_) #f) (bufferline "never")))

(define (moka-bufferline-enable!)
  (unless *moka-bufferline-enabled?*
    (moka-bufferline-blank-native!)
    (moka-bufferline-register-hooks!)
    (moka-bufferline-sync-clip!)
    (moka-bufferline-ensure-component!)
    (set! *moka-bufferline-enabled?* #t)))

(define (moka-bufferline-disable!)
  (when *moka-bufferline-enabled?*
    (pop-last-component-by-name! "moka-bufferline")
    (set-editor-clip-top! 0)
    (set! *moka-bufferline-enabled?* #f)))

;;@doc
;; Number of rows the moka bufferline occupies at the top of the screen
(define (moka-reserved-top)
  (if (and *moka-bufferline-enabled?* (moka-bufferline-visible?))
      (+ *moka-bufferline-row-offset* 1)
      0))

;;@doc
;; Number of rows the moka statusline occupies at the bottom of the screen
(define (moka-reserved-bottom)
  (if *moka-enabled?*
      (max 1 *moka-row-offset*)
      0))
