(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))

(struct StrealState (paths mode branch) #:mutable)

(define keymap-help
  '("s"      "Add / remove current file"
    "1..9"   "Open file"
    "Esc, q" "Close popup"
    "C"      "Clear list"
    "h"      "Open in horizontal split"
    "v"      "Open in vertical split"
    "d"      "Delete mode"
    "e"      "Edit current Streal file"
    "?"      "Show keymap"))

(define chars-to-encode '(#\% #\space #\\ #\/ #\: #\* #\? #\" #\< #\> #\|))

(define (editor-focus-path)
  (~> (editor-focus) (editor->doc-id) (editor-document->path)))

(define (trim-current-directory path)
  (~> (or path "")
      (trim-start-matches (string-append (current-directory) (path-separator)))
      (trim-start-matches (string-append "." (path-separator)))))

(define (toggle-mode state mode)
  (set-StrealState-mode! state (if (eqv? (StrealState-mode state) mode) 'normal mode)))

(define (percent-encode str)
  (~>> str
       (string->list)
       (map (lambda (x)
              (if (list-contains x chars-to-encode)
                  (~>> x
                       (char->integer)
                       ((flip number->string) 16)
                       (string-upcase)
                       (string-append "%")
                       (string->list))
                  x)))
       (flatten)
       (apply string)))

(define (handle-error err)
  (set-error! (string-append "'streal':" (error-object-message err))))

(define (get-git-branch)
  (begin
    (define result
      (~> (command "git" '("branch" "--show-current"))
          with-stdout-piped
          with-stderr-piped
          spawn-process))
    (cond
      [(Ok? result)
       (let ([handle (Ok->value result)])
         (define stdout (read-port-to-string (child-stdout handle)))
         (define stderr (read-port-to-string (child-stderr handle)))
         (if (and (string=? stderr "") (not (string=? stdout "")))
             (trim stdout)
             #false))]
      [(Err? result) (error (Err->value result))])))

(define (get-streal-file-path branch)
  (let* ([slash (path-separator)]
         [branch-path (if branch
                          (string-append "branch" slash (percent-encode branch) slash)
                          "")])
    (string-append (canonicalize-path "~")
                   (path-separator)
                   ".streal"
                   (path-separator)
                   branch-path
                   (percent-encode (current-directory))
                   ".txt")))

(define (read-file-as-string name)
  (call-with-input-file name
                        (lambda (in)
                          (do ((x (read-char in) (read-char in)) (chars '() (cons x chars)))
                              ((eof-object? x) (list->string (reverse chars)))))))

(define (get-paths branch)
  (let ([path (get-streal-file-path branch)])
    (if (is-file? path)
        (~>> path
             (read-file-as-string)
             ((flip split-many) "\n")
             (map (lambda (x) (trim-current-directory (trim x))))
             (filter (lambda (x) (> (string-length x) 0))))
        '())))

(define (write-paths paths branch)
  (let* ([path (get-streal-file-path branch)]
         [contents (~> paths (string-join "\n") (string-append "\n"))]
         [directory (parent-name path)])
    (when (is-file? path)
      (delete-file! (get-streal-file-path branch)))
    (unless (path-exists? (parent-name path))
      (create-directory! (parent-name path)))
    (unless (empty? paths)
      (call-with-output-file path (lambda (in) (write-string contents in))))
    (delete-empty-directories)))

(define (directory-empty? path) (= (length (read-dir path)) 0))

(define (delete-empty-directories)
  (let* ([slash (path-separator)]
         [streal-path (string-append (canonicalize-path "~") slash ".streal")]
         [branch-path (string-append streal-path slash "branch")])
    (when (path-exists? branch-path)
      (begin
        (for-each delete-directory! (filter directory-empty? (read-dir branch-path)))
        (when (directory-empty? branch-path)
          (delete-directory! branch-path))))))

(define (remove-path paths path branch)
  (write-paths (filter (lambda (x) (not (string=? path x))) paths) branch))

(define (calculate-popup-area rect paths mode)
  (let* ([rect-width (area-width rect)]
         [rect-height (area-height rect)]
         [width (min (if (eqv? mode 'help)
                         (+ (apply max (map string-length keymap-help)) 11)
                         (+ (if (> (length paths) 0)
                                (max (apply max (map (lambda (x) (string-length x)) paths)) 8)
                                9)
                            6))
                     (- rect-width 4))]
         [height (min (if (eqv? mode 'help)
                          (+ (/ (length keymap-help) 2) 2)
                          (+ (max (length paths) 1) 2))
                      (- rect-height 4))]
         [x (ceiling (max 0 (- (ceiling (/ rect-width 2)) (floor (/ width 2)))))]
         [y (ceiling (max 0 (- (ceiling (/ rect-height 2)) (floor (/ height 2)))))])
    (area (- x 1) (- y 1) width height)))

(define (calculate-text-area popup-area)
  (let ([padding-x 2]
        [padding-y 1])
    (area (+ (area-x popup-area) padding-x)
          (+ (area-y popup-area) padding-y)
          (- (area-width popup-area) (* padding-x 2))
          (- (area-height popup-area) (* padding-y 2)))))

(define (shorten-paths paths)
  (let ([split-paths (map (lambda (x) (reverse (split-many x (path-separator)))) paths)])
    (map (lambda (split-path)
           (let ([result (mutable-vector)]
                 [i 0]
                 [len (length split-path)]
                 [split-paths-to-check split-paths])
             (while [and (< i len) (> (length split-paths-to-check) 0)]
                    (vector-push! result (list-ref split-path i))
                    (set! split-paths-to-check
                          (filter (lambda (x)
                                    (and (not (eq? x split-path))
                                         (< i (length x))
                                         (string=? (list-ref split-path i) (list-ref x i))))
                                  split-paths-to-check))
                    (set! i (+ i 1)))
             (string-join (reverse (vector->list result)) (path-separator))))
         split-paths)))

(define (switch-or-open path mode)
  (let* ([doc-ids (editor-all-documents)]
         [path-hash (apply hash
                           (flatten (map (lambda (x)
                                           (list (trim-current-directory (editor-document->path x))
                                                 x))
                                         doc-ids)))]
         [path-doc-id (hash-try-get path-hash path)])
    (when (eq? mode 'horizontal)
      (helix.hsplit))
    (when (eq? mode 'vertical)
      (helix.vsplit))
    (if path-doc-id
        (editor-switch-action! path-doc-id (Action/Replace))
        (helix.open path))))

(define (render-streal state area buf)
  (let* ([mode (StrealState-mode state)]
         [paths (StrealState-paths state)]
         [shortened-paths (shorten-paths paths)]
         [streal-area (calculate-popup-area area shortened-paths mode)]
         [text-area (calculate-text-area streal-area)]
         [popup-style (theme-scope "ui.popup")]
         [active-style (theme-scope "ui.text.focus")]
         [number-style (theme-scope "markup.list")]
         [delete-style (theme-scope "error")])
    (buffer/clear buf streal-area)
    (block/render buf streal-area (make-block popup-style (style) "all" "plain"))
    (when (not (eqv? mode 'normal))
      (frame-set-string! buf
                         (+ (area-x streal-area) 2)
                         (area-y streal-area)
                         (symbol->string mode)
                         active-style))
    (if (eqv? mode 'help)
        (for-each
         (lambda (i)
           (let ([key (list-ref keymap-help (* i 2))]
                 [description (list-ref keymap-help (+ (* i 2) 1))])
             (frame-set-string! buf (area-x text-area) (+ (area-y text-area) i) key number-style)
             (frame-set-string! buf
                                (+ (area-x text-area) 7)
                                (+ (area-y text-area) i)
                                description
                                popup-style)))
         (range (/ (length keymap-help) 2)))
        (begin
          (when (= (length paths) 0)
            (frame-set-string! buf (area-x text-area) (area-y text-area) "  (empty)" popup-style))
          (for-each (lambda (i)
                      (let* ([path (list-ref paths i)]
                             [shortened-path (list-ref shortened-paths i)]
                             [current-style (cond
                                              [(eqv? mode 'delete) delete-style]
                                              [(string=? (trim-current-directory (editor-focus-path))
                                                         (trim-current-directory path))
                                               active-style]
                                              [else popup-style])])
                        (frame-set-string! buf
                                           (area-x text-area)
                                           (+ (area-y text-area) i)
                                           (number->string (+ i 1))
                                           number-style)
                        (frame-set-string! buf
                                           (+ (area-x text-area) 2)
                                           (+ (area-y text-area) i)
                                           shortened-path
                                           current-style)))
                    (range (length paths)))))))

(define (handle-event state event)
  (let* ([mode (StrealState-mode state)]
         [paths (StrealState-paths state)]
         [branch (StrealState-branch state)]
         [char (key-event-char event)]
         [num (char->number (or char #\null))]
         [current-path (trim-current-directory (editor-focus-path))])
    (with-handler
     (lambda (err)
       (handle-error err)
       event-result/consume)
     (cond
       [(key-event-escape? event) event-result/close]
       [(eqv? char #\q) event-result/close]
       [(not (eqv? num #false))
        (if (and (>= num 1) (<= num (length paths)))
            (let ([selected-path (list-ref paths (- num 1))])
              (if (eqv? mode 'delete)
                  (begin
                    (remove-path paths selected-path branch)
                    (set-StrealState-paths! state (get-paths branch))
                    (set-status! (string-append "'" selected-path "' removed from Streal file."))
                    event-result/consume)
                  (begin
                    (switch-or-open selected-path mode)
                    event-result/close)))
            (error (string-append "No path in streal file on line " (number->string num) ".")))]
       [(eqv? char #\s)
        (if (string=? current-path "")
            (error "Can't add to Streal file with no path set.")
            (begin
              (if (member current-path paths)
                  (begin
                    (remove-path paths current-path branch)
                    (set-status! (string-append "'" current-path "' removed from Streal file.")))
                  (begin
                    (write-paths (append paths (list current-path)) branch)
                    (set-status! (string-append "'" current-path "' added to Streal file."))))
              event-result/close))]
       [(eqv? char #\e)
        (switch-or-open (get-streal-file-path branch) mode)
        event-result/close]
       [(eqv? char #\C)
        (write-paths '() branch)
        (set-status! "Streal file cleared.")
        event-result/close]
       [(eqv? char #\d)
        (toggle-mode state 'delete)
        event-result/consume]
       [(eqv? char #\h)
        (toggle-mode state 'horizontal)
        event-result/consume]
       [(eqv? char #\v)
        (toggle-mode state 'vertical)
        event-result/consume]
       [(eqv? char #\?)
        (toggle-mode state 'help)
        event-result/consume]
       [(eqv? char #\:) event-result/ignore]
       [else event-result/consume]))))

;;@doc
;; Open the Streal popup
;; Flags:
;;   --per-branch  Keep a separate Streal file per Git branch
(define (streal-open [flag ""])
  (with-handler handle-error
                (if (and (not (string=? flag "")) (not (string=? flag "--per-branch")))
                    (error (string-append "unknown flag '" flag "'"))
                    (let ([branch (if (string=? flag "--per-branch")
                                      (get-git-branch)
                                      #false)])
                      (push-component! (new-component! "streal"
                                                       (StrealState (get-paths branch) 'normal branch)
                                                       render-streal
                                                       (hash "handle_event" handle-event)))))))

(provide streal-open)
