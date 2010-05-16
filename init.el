;;; init.el -- It begins.
;;
;; John Evans
;; Initially created 05/15/2010
;; Last updated 05/15/2010
;;
;; Targetting emacs 23.1
;;
;; This is the first thing to get loaded for user customization.


;; Disable the menu bar and tool bar
(menu-bar-mode 0)
(tool-bar-mode 0)


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


;; Enable upcase/downcasing a region with C-x C-u and C-x C-l respectively.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; The emacs-textmate package makes emacs mimic Textmate's behavior with
;; regards to matching pairs of e.g. quotes, parens, etc.
;; For example, if you type a quote it will add the closing quote
;; automatically and then position the cursor inside the two quotes...
;; but then if you hit another quote it will replace the second one (even if
;; you already typed something)

(load-file "~/.emacs.d/lib/emacs-textmate.el")
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


;;; init.el -- The End.
