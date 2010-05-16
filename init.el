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

;;; init.el -- The End.
