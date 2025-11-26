;; -*- lexical-binding: t; -*-

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
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (mode-line-compact 'long)
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
  (visible-bell t)
  (xterm-mouse-mode nil))

(use-package visual-fill
  :ensure t)

(use-package adaptive-wrap
  :ensure t)

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :ensure t
  :custom
  (vertico-mode t))

(use-package magit
  :ensure t)

(use-package org
  :bind
  (("C-c a" . org-agenda)
   ("C-c l" . org-store-link))
  :custom
  (org-agenda-inhibit-startup t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-start-with-log-mode nil)
  (org-clock-mode-line-total 'current)
  (org-agenda-prefix-format
   '((agenda . " %i %?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
  (org-agenda-files
   "~/Projects/org/agenda-file-list.txt")
  (org-directory "~/Projects/org")
  (org-agenda-span 'day)
  (org-display-custom-times nil)
  (org-time-stamp-custom-formats '("%m-%d" . "%H:%M"))
  (org-export-initial-scope 'buffer)
  (org-export-backends '(ascii beamer html icalendar latex md odt))
  (org-modules
   '(ol-bbdb ol-bibtex ol-doi ol-eww ol-info ol-irc ol-mhe ol-rmail org-tempo)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
