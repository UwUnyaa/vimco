;;; vimco.el --- Convert Vim themes to Emacs

;; this code is based on code from
;; <https://github.com/zphr/vim-theme-converter.el>

(eval-when-compile
  (require 'cl))

(require 'pp)

(defconst vimco-repo-link
  "https://github.com/DoMiNeLa10/vimco"
  "Link to the repository that holds this file.")

(defvar vimco-face-name-alist
  '(("Normal"        default)
    ("Cursor"        cursor)
    ("CursorLine"    highline-face)
    ("Visual"        region)
    ("StatusLine"    mode-line mode-line-buffer-id) ; minibuffer-prompt
    ("StatusLineNC"  mode-line-inactive)
    ("LineNr"        linum fringe)
    ("MatchParen"    show-paren-match-face)
    ("Search"        isearch)
    ("IncSearch"     isearch-lazy-highlight-face)
    ("Comment"       font-lock-comment-face font-lock-doc-face)
    ("Statement"     font-lock-builtin-face)
    ("Function"      font-lock-function-name-face)
    ("Keyword"       font-lock-keyword-face)
    ("String"        font-lock-string-face)
    ("Type"          font-lock-type-face)
    ("Identifier"    font-lock-variable-name-face)
    ("Constant"      font-lock-constant-face)
    ("Error"         font-lock-warning-face)
    ("PreProc"       font-lock-preprocessor-face)
    ("Underlined"    underline)
    ("Directory"     dired-directory)
    ("Pmenu"         ac-candidate-face)
    ("PmenuSel"      ac-selection-face)
    ("SpellBad"      flyspell-incorrect)
    ("SpellRare"     flyspell-duplicate)))

(defvar vimco-attribute-alist
  '(("bold"       :weight bold)
    ;; ("standout"  . )       ; I have no idea what this one is supposed to do
    ("underline"  :underline t)
    ("undercurl"  :underline (:style wave))
    ("reverse"    :inverse-video t)
    ("inverse"    :inverse-video t)
    ("italic"     :slant italic)))

(defun vimco-separate-newline (&rest lines)
  (mapconcat #'identity lines "\n"))

(defun vimco-get-lines ()
  (let (prev-line lines)
    (goto-char (point-min))
    (setq prev-line (point))
    (while (/= (forward-line) 1)
      (push (cons prev-line (- (point) 1)) lines)
      (setq prev-line (point)))
    (nreverse lines)))

(defun vimco-parse-line (region)
  "Turns substring from buffer REGION, which should look
like (beg .end) into an alist. If a word looks like \"a=b\", it's
stored as (\"a\" . \"b\"), else cdr is set to nil."
  (let* ((beg (car region))
         (end (cdr region)))
    (mapcar (lambda (word)
              (if (string-match "=" word)
                  (let ((data (split-string word "=" t)))
                    (cons (car data) (cadr data)))
                (cons word nil)))
            (split-string (buffer-substring-no-properties beg end) " " t))))

(defun vimco-line-to-faces (line)
  "Transforms a LINE into a list of face definitions. LINE should
be an alist returned by `vimco-parse-line'."
  (cl-flet ((get-prop (prop) (cdr (assoc prop line))))
    (let ((face-names (cdr (assoc (caar line) vimco-face-name-alist))))
      (when face-names
        (let
            ((foreground (get-prop "guifg"))
             (background (get-prop "guibg"))
             (attributes (or (get-prop "gui")
                             (get-prop "term")
                             (get-prop "cterm")))
             faces face-attributes)
          ;; replace "NONE" with nil
          (mapc
           (lambda (prop)
             (let ((value (symbol-value prop)))
               (when (and value
                          (string-equal value "NONE"))
                 (set prop nil))))
           '(foreground background attributes))
          ;; turn attributes into a list
          (when attributes
            (setq attributes
                  (let (new-attributes)
                    (mapc
                     (lambda (attribute)
                       (setq attribute (cdr (assoc attribute vimco-attribute-alist)))
                       (when attribute
                         (setq new-attributes (append attribute new-attributes))))
                     (split-string attributes "," t))
                    new-attributes)))
          ;; turn foreground and background into proper lists
          (mapc
           (lambda (symbol)
             (let ((value (symbol-value symbol)))
               (when value
                 (set symbol
                      `(,(intern (format ":%s" symbol)) ,value)))))
           '(foreground background))
          ;; put everything into a single plist
          (mapc
           (lambda (attribute)
             (when attribute
               (setq face-attributes (append face-attributes attribute))))
           (list attributes background foreground))
          ;; make face definitions
          (mapc
           (lambda (face)
             (push
              `'(,face ((((class color) (min-colors 89))
                         (,@face-attributes))))
              faces))
           face-names)
          faces)))))

(defun vimco-convert-theme (file)
  (let* ((theme-name (intern
                      (file-name-sans-extension
                       (file-name-nondirectory file))))
         (theme-file (format "%s-theme.el" theme-name)))
    (find-file theme-file)
    ;; clear the buffer
    (delete-region (point-min) (point-max))
    (insert
     (vimco-separate-newline
      ;; comments at the beginning of a file
      (format ";;; %s --- Custom face theme for Emacs\n" theme-file)
      ";; This theme was generated with vimco.el"
      ";; You can get it from:"
      (format ";; <%s>\n" vimco-repo-link)
      ";;; Code:\n"
      ;; code
      (pp-to-string `(deftheme ,theme-name))
      (with-temp-buffer
        (insert-file-contents-literally file)
        (pp-to-string
         `(custom-theme-set-faces
           ',theme-name
           ,@(let (faces)
               (mapc
                (lambda (line)
                  (let ((face-specifications
                         (vimco-line-to-faces (vimco-parse-line line))))
                    (when face-specifications
                      (setq faces (append face-specifications faces)))))
                (vimco-get-lines))
               faces))))
      (pp-to-string `(provide-theme ',theme-name))
      ;; set file variables so the theme won't get compiled
      (concat ";; Local" " Variables:")
      ";; no-byte-compile: t"
      ";; End:\n"))))

(defun vimco-convert-vim-theme (theme-name)
  (interactive
   (list (completing-read
          "Theme name: "
          (mapcar
           #'file-name-sans-extension
           (directory-files "~/.vim/colors" nil "\\.vim$")))))
  (let ((theme-file (concat temporary-file-directory theme-name)))
    (call-process (executable-find "vim") nil nil nil
                  "-u" "NONE"
                  "-c"
                  (vimco-separate-newline
                   ":set columns=3000"
                   (format ":colorscheme %s" theme-name)
                   (format ":redir > %s" theme-file)
                   ":highlight"
                   ":redir END"
                   ":q"))
    (vimco-convert-theme theme-file))
  (message "Write this file somewhere"))