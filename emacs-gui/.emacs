;;; -*- lexical-binding: t -*-

(use-package emacs
  :custom
  (auto-fill-function 'do-auto-fill t)
  (calendar-week-start-day 1)
  (completion-ignore-case t)
  (custom-enabled-themes '(modus-vivendi) nil nil)
  (display-battery-mode nil)
  (display-time-mode nil)
  (electric-pair-mode t)
  (enable-local-variables nil)
  (global-visual-line-mode t)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (mode-line-compact 'long)
  (touch-screen-enable-hscroll nil)
  (tool-bar-button-margin 12)
  (menu-bar-mode t)
  (face-font-family-alternatives
   '(("JuliaMono" "Noto Sans SC") ("Libertinus Serif" "Noto Serif SC")))
  (modus-themes-bold-constructs nil)
  (modus-themes-inhibit-reload nil)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (network-security-level 'paranoid)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (require-final-newline t)
  (tab-always-indent 'complete)
  (tool-bar-mode t)
  (tool-bar-position 'bottom)
  (visible-bell t))

(use-package visual-fill
  :ensure t
  :hook ((text-mode . visual-fill-mode)))

(use-package adaptive-wrap
  :ensure t)

(use-package text-mode
  :hook
  ((text-mode . variable-pitch-mode))
  :bind
  (("C-c d" . dictionary-search)))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :ensure t
  :custom
  (vertico-mode t))

(use-package shr
  :custom
  (shr-cookie-policy nil)
  (shr-inhibit-images t)
  (shr-use-colors nil))

(custom-set-faces
 '(default ((t (:height 184 :family "JuliaMono"))))
 '(menu ((t (:family "Libertinus Serif"))))
 '(variable-pitch ((t (:family "Libertinus Serif" :height 230)))))

(defun android-font-list ()
  "list all available fonts"
  (dolist (f (font-family-list))
    (princ f)
    (princ "\n")))

(use-package dictionary
  :hook
  ((dictionary-mode . variable-pitch-mode)
   (text-mode . text-mode-tool-bar)
   (dictionary-mode . text-mode-tool-bar))

  :config
  (defun text-mode-tool-bar (&rest _ignored)
    "Set up tool bar for text mode"
    (interactive)
    (define-key menu-bar-goto-menu [scroll-up]
                '(menu-item "Scroll up" scroll-up-command :help "Scroll up a full screen"))
    (let ((map (make-sparse-keymap)))
      (tool-bar-local-item-from-menu 'dictionary-lookup-definition "index" map dictionary-mode-map  :label "Look up word at point")
      (tool-bar-local-item-from-menu 'scroll-up-command "save" map global-map  :label "Scroll up")
      (setq-local secondary-tool-bar-map map))))
