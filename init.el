;;; init.el -- It begins.
;;
;; John Evans
;; Initially created 05/15/2010
;; Last updated 05/15/2010
;;
;; Targetting emacs 23.1
;;
;; This is the first thing to get loaded for user customization.


;; Let's time how long it takes to startup emacs.
(defvar *emacs-load-start* (current-time))


;; Disable the menu bar and tool bar
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Have emacs store customizations in a separate file instead of modifying
;; this one.
(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file 'noerror)


;; Set up the color theme library
(add-to-list 'load-path "~/.emacs.d/lib/color-theme")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

;; Load and activate the railscast theme
(load-file "~/.emacs.d/themes/color-theme-railscasts.el")
(color-theme-railscasts)


;; This tells emacs not to convert multiple spaces to tabs when doing
;; things like ident-region
(setq-default indent-tabs-mode nil)


;; Enable upcase/downcasing a region with C-x C-u and C-x C-l
;; respectively.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; The emacs-textmate package makes emacs mimic Textmate's behavior
;; with regards to matching pairs of e.g. quotes, parens, etc.

;; For example, if you type a quote it will add the closing quote
;; automatically and then position the cursor inside the two quotes...
;; but then if you hit another quote it will replace the second one
;; (even if you already typed something)

;; Also you can highlight a region and hit a paired character like
;; quote and it will insert an open quote at the beginning of the
;; region and a close quote at the end.

;; NOTE: I renamed the minor mode ("textmate-mode") defined in this
;; file to "textmate-pairs-mode" to avoid conflicts with textmate.el
;; which also defines a textmate-minor mode which provides different
;; functionality.

(load-file "~/.emacs.d/lib/emacs-textmate/emacs-textmate.el")
(textmate-pairs-mode)

;; This is the other textmate mode I was referring to above:
(load-file "~/.emacs.d/lib/textmate.el/textmate.el")
(textmate-mode)


;; This makes emacs behave more like Textmate with regard to pasting
;; idented code.  I don't think it's quite right, in that let's say
;; you cut something that is indented four spaces and then move to
;; a point where it's indented 12 spaces and yank, the yanked code
;; will be indented 16 spaces... but it's closer to the behavior that
;; I want.
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode
                                                     scheme-mode
                                                     haskell-mode
                                                     ruby-mode
                                                     rspec-mode
                                                     python-mode
                                                     c-mode
                                                     c++-mode
                                                     objc-mode
                                                     latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))


;; This makes enter/return auto-indent in c-like modes.
(add-hook 'c-mode-common-hook
          '(lambda ()
             (local-set-key (kbd "RET") 'newline-and-indent)))
;; And in python... TODO: DRY these up
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "RET") 'newline-and-indent)))


;; We want to know when we're leaving behind trailing whitespace.
(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)


;; Save a list of open files in ~/.emacs.desktop
;; Execute "M-x desktop-save" once and it will update whenever auto-save
;; occurs (and on every exit?)
(require 'desktop)
(desktop-save-mode 1)
(add-hook 'auto-save-hook
          (lambda () (desktop-save-in-desktop-dir)))

;; Save a bunch of variables to the desktop file for lists specify the
;; len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;; Enable shift-arrow keys to move between buffers directionally.
(windmove-default-keybindings)


;; Let's have some snippets, shall we?
(add-to-list 'load-path "~/.emacs.d/lib/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")


;; Syntax highlighting for YAML please.
(add-to-list 'load-path "~/.emacs.d/lib/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))


;; Causes emacs to highlight the matching paren any time the point is
;; resting on one.
(show-paren-mode 1)


;; Set up bindings for fill-paragraph to auto-wrap paragraphs of text, etc.
(global-set-key (kbd "C-x C-p") 'fill-paragraph)
(global-set-key (kbd "C-x p") 'fill-paragraph)

;; Load rect-mark and setup key bindings for it.
;; TODO: Write my own version of this that behaves like TextMate.
;; Or maybe try CuaMode
(load-file "~/.emacs.d/lib/rect-mark/rect-mark.el")
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)


;; Add a minimap
(add-to-list 'load-path "~/.emacs.d/lib/minimap")
(require 'minimap)

(setq minimap-active 0)
(defun toggle-minimap ()
  "Toggle minimap on/off"
  (interactive)
  (if (= minimap-active 1)
      (progn
        (minimap-kill)
        (setq minimap-active 0))
    (progn
      (minimap-create)
      (setq minimap-active 1))))

(global-set-key (kbd "C-S-m") 'toggle-minimap)

;; Start the server by default
(server-start)


;; Gotta have that hippe expand
(global-set-key (kbd "M-/") 'hippie-expand)


;; Column numbers, please.
(column-number-mode 1)

;; Load sr-speedbar and bind it to "M-s M-s"
(load-file "~/.emacs.d/lib/sr-speedbar.el")
(global-set-key (kbd "M-s M-s") 'sr-speedbar-toggle)
;; TODO: That key binding should also switch to the speedbar.


;; Report the time it took to initialize emacs in the *Messages* buffer.
(message "My .emacs loaded in %ds"
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) (second
                                                       *emacs-load-start*)))))


;; Add js2-mode for editing javascript.
;; js2-mode MUST be byte compiler or it will refuse to run.  I use something
;; like this command to byte compile everything:
;; find . -name \*.el -exec emacs -batch -f batch-byte-compile {} \;
(add-to-list 'load-path "~/.emacs.d/lib/js2-mode")
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))


;; Set up slime + clojure
(add-to-list 'load-path "~/.emacs.d/lib/clojure-mode")
(require 'clojure-mode)

;; swank-clojure
(add-to-list 'load-path "~/.emacs.d/lib/swank-clojure/")

(setq swank-clojure-jar-path "~/.clojure/clojure.jar"
      swank-clojure-extra-classpaths (list
				      "~/opt/swank-clojure/src/main/clojure"
				      "~/.clojure/clojure-contrib.jar"))

(require 'swank-clojure)

;; slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))

(add-to-list 'load-path "~/.emacs.d/lib/slime")
(require 'slime)
(slime-setup)

;; flymake->lintrunner
;; TODO: Only do all this if the lintrunner script exists, or pull
;; lintrunner into my .emacs.d
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/pluto/pycloud/apps/emacs/bin/lintrunner.py" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(add-hook 'python-mode-hook (lambda() (flymake-mode t)))


;; Allow us to invoke occur from within isearch:
;; Alex Schroeder [http://www.emacswiki.org/cgi-bin/wiki/OccurBuffer]
(defun isearch-occur ()
  "*Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;;; init.el -- The End.
