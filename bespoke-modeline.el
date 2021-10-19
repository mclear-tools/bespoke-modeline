;;; bespoke-modeline.el -- custom mode line for bespoke theme  ;; -*- lexical-binding: t -*-
;; Copyright (C) 2020 Colin McLear
;; -------------------------------------------------------------------
;; Authors: Colin McLear, Nicholas Rougier
;; -------------------------------------------------------------------
;; URL: https://github.com/mclear-tools/bespoke-modeline
;; -------------------------------------------------------------------
;; Created: 2021-10-20
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; -------------------------------------------------------------------
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
;;; Commentary:
;; A custom mode line for bespoke-theme
;; This mode line originated as a fork of bespoke-emacs modeline.
;; See https://github.com/rougier/nano-emacs
;; https://github.com/rougier/nano-modeline
;; -------------------------------------------------------------------
;;
;; Bespoke mode line format:
;;
;; [ status | name (primary)               secondary | item1 | item2 ]
;;
;; -------------------------------------------------------------------
;;
;; It can be displayed at the bottom (mode-line) or at the top (header-line)
;; depending on bespoke-modeline-position custom setting.
;;
;; There are two sets of faces (for active and inactive modelines) that
;; can be customized (M-x: customize-group + bespoke-modeline)
;;
;; - bespoke-modeline-active-name      / bespoke-modeline-inactive-name
;; - bespoke-modeline-active-primary   / bespoke-modeline-inactive-primary
;; - bespoke-modeline-active-secondary / bespoke-modeline-inactive-secondary
;; - bespoke-modeline-active-status-RO / bespoke-modeline-inactive-status-RO
;; - bespoke-modeline-active-status-RW / bespoke-modeline-inactive-status-RW
;; - bespoke-modeline-active-status-** / bespoke-modeline-inactive-status-**
;;
;; Usage example:
;;
;; M-x: bespoke-modeline-mode
;;

;;; Code:
(defgroup bespoke nil
  "Bespoke group"
  :group 'convenience)

(defgroup bespoke-modeline nil
  "Bespoke group modeline"
  :group 'bespoke)

(defgroup bespoke-modeline-active nil
  "Active modeline faces.

Modeline is composed as:
[ status | name (primary)                        secondary ]"
  :group 'bespoke-modeline)

(defgroup bespoke-modeline-inactive nil
  "Inactive modeline faces

Modeline is composed as:
[ status | name (primary)                        secondary ]"
  :group 'bespoke-modeline)


(defcustom bespoke-modeline-position 'top
  "Default position (top or bottom)"
  :type '(choice (const :tag "Top"    top)
                 (const :tag "Bottom" bottom))
  :group 'bespoke-modeline)

(defface bespoke-modeline-active
  '((t (:inherit mode-line)))
  "Modeline face for active modeline"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-active-name
  '((t (:inherit (mode-line bold))))
  "Modeline face for active name element"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-active-primary
  '((t (:inherit mode-line)))
  "Modeline face for active primary element"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-active-secondary
  '((t (:inherit mode-line)))
  "Modeline face for active secondary element"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-active-status-RO
  '((t (:inherit mode-line)))
  "Modeline face for active READ-ONLY element"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-active-status-RW
  '((t (:inherit mode-line)))
  "Modeline face for active READ-WRITE element"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-active-status-**
  '((t (:inherit mode-line)))
  "Modeline face for active MODIFIED element"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-inactive
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive window"
  :group 'bespoke-modeline-inactive)

(defface bespoke-modeline-inactive-name
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive name element"
  :group 'bespoke-modeline-inactive)

(defface bespoke-modeline-inactive-primary
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive primary element"
  :group 'bespoke-modeline-inactive)

(defface bespoke-modeline-inactive-secondary
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive primary element"
  :group 'bespoke-modeline-inactive)

(defface bespoke-modeline-inactive-status-RO
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive READ-ONLY element"
  :group 'bespoke-modeline-inactive)

(defface bespoke-modeline-inactive-status-RW
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive READ-WRITE element"
  :group 'bespoke-modeline-inactive)

(defface bespoke-modeline-inactive-status-**
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive MODIFIED element"
  :group 'bespoke-modeline-inactive)

(defcustom bespoke-modeline-user-mode nil
  "User supplied mode to be evaluated for modeline."
  :type '(choice (const nil) function)
  :group 'bespoke-modeline)

(defun bespoke-modeline-user-mode-p ()
  "Should the user supplied mode be called for modeline?"
  bespoke-modeline-user-mode)

(defun bespoke-modeline-truncate (str size &optional ellipsis)
  "If STR is longer than SIZE, truncate it and add ELLIPSIS."

  (let ((ellipsis (or ellipsis "…")))
    (if (> (length str) size)
        (format "%s%s" (substring str 0 (- size (length ellipsis))) ellipsis)
      str)))

(defun bespoke-modeline-vc-branch ()
  "Return current VC branch if any."
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun bespoke-modeline-mode-name ()
  "Return current major mode name"
  (format-mode-line mode-name))


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(defun bespoke-modeline-shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))


(defun bespoke-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (active        (eq window bespoke-modeline--selected-window))
         (space-up       +0.20)
         (space-down     -0.25)
         (prefix (cond ((string= status "RO")
                        (propertize (if (window-dedicated-p)"•RO " " RO ")
                                    'face (if active
                                              'bespoke-modeline-active-status-RO
                                            'bespoke-modeline-inactive-status-RO)))
                       ((string= status "**")
                        (propertize (if (window-dedicated-p)"•** " " ** ")
                                    'face (if active
                                              'bespoke-modeline-active-status-**
                                            'bespoke-modeline-inactive-status-**)))
                       ((string= status "RW")
                        (propertize (if (window-dedicated-p) "•RW " " RW ")
                                    'face (if active 'bespoke-modeline-active-status-RW
                                            'bespoke-modeline-inactive-status-RW)))
                       (t (propertize status
                                      'face (if active 'bespoke-modeline-active-status-**
                                              'bespoke-modeline-inactive-status-**)))))
         (left (concat
                (propertize " "  'face (if active 'bespoke-modeline-active
                                         'bespoke-modeline-inactive)
                            'display `(raise ,space-up))
                (propertize name 'face (if active 'bespoke-modeline-active-name
                                         'bespoke-modeline-inactive-name))
                (propertize " "  'face (if active 'bespoke-modeline-active
                                         'bespoke-modeline-inactive)
                            'display `(raise ,space-down))
                (propertize primary 'face (if active 'bespoke-modeline-active-primary
                                            'bespoke-modeline-inactive-primary))))
         (right (concat secondary " "))
         
         (available-width (- (window-total-width) 
                             (length prefix) (length left) (length right)
                             (/ (window-right-divider-width) char-width)))
     (available-width (max 1 available-width)))
    (concat prefix
            left
            (propertize (make-string available-width ?\ )
                        'face (if active 'bespoke-modeline-active
                                'bespoke-modeline-inactive))
            (propertize right 'face (if active 'bespoke-modeline-active-secondary
                                      'bespoke-modeline-inactive-secondary)))))


;; ---------------------------------------------------------------------
(defun bespoke-modeline-ein-notebook-mode ()
  (let ((buffer-name (format-mode-line "%b")))
    (bespoke-modeline-compose (if (ein:notebook-modified-p) "**" "RW")
                           buffer-name
                           ""
                           (ein:header-line))))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun bespoke-modeline-elfeed-search-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                         "Elfeed"
                         (concat "(" (elfeed-search--header)  ")")
                         ""))

(defun bespoke-modeline-elfeed-setup-header ()
  (setq header-line-format (default-value 'header-line-format)))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-elfeed-show-mode-p ()
  (derived-mode-p 'elfeed-show-mode))

(defun bespoke-modeline-elfeed-show-mode ()
  (let* ((title        (elfeed-entry-title elfeed-show-entry))
         (tags         (elfeed-entry-tags elfeed-show-entry))
         (tags-str     (mapconcat #'symbol-name tags ", "))
         (date         (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed         (elfeed-entry-feed elfeed-show-entry))
         (feed-title   (plist-get (elfeed-feed-meta feed) :title))
         (entry-author (elfeed-meta elfeed-show-entry :author)))
    (bespoke-modeline-compose (bespoke-modeline-status)
                           (bespoke-modeline-truncate title 40)
                           (concat "(" tags-str ")")
                           feed-title)))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun bespoke-modeline-calendar-mode () "")

;; Calendar (no header, only overline)
(defun bespoke-modeline-calendar-setup-header ()
  (setq header-line-format "")
  (face-remap-add-relative
   'header-line `(:overline ,(face-foreground 'default)
                            :height 0.5
                            :background ,(face-background 'default))))

;; From https://emacs.stackexchange.com/questions/45650
;; (add-to-list 'display-buffer-alist
;;              `(,(rx string-start "*Calendar*" string-end)
;;               (display-buffer-below-selected)))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun bespoke-modeline-org-capture-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                         "Capture"
                         "(org)"
                         ""))

(defun bespoke-modeline-org-capture-turn-off-header-line ()
  (setq-local header-line-format (default-value 'header-line-format))
  (message nil))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
    (node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
    line)
    (while  (> depth 0)
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))
    (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
                     crumbs (cons nil crumbs))))
    (forward-line 1)
    (dolist (node crumbs)
      (let ((text
         (if (not (equal node "Top")) node
           (format "%s"
               (if (stringp Info-current-file)
               (file-name-sans-extension
                (file-name-nondirectory Info-current-file))
             Info-current-file)))))
    (setq line (concat line (if (null line) "" " > ")
                                (if (null node) "..." text)))))
    (if (and cnode (not (equal cnode "Top")))
        (setq line (concat line (if (null line) "" " > ") cnode)))
    line))

(defun bespoke-modeline-info-mode-p ()
  (derived-mode-p 'Info-mode))

(defun bespoke-modeline-info-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                         "Info"
                         (concat "("
                                 (bespoke-modeline-info-breadcrumbs)
                                 ")")
                         ""))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun bespoke-modeline-org-agenda-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                         "Agenda"
                         ""
                         (format-time-string "%A %-e %B %Y")))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

(defun bespoke-modeline-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun bespoke-modeline-term-mode ()
  (bespoke-modeline-compose " >_ "
                         "Terminal"
                         (concat "(" shell-file-name ")")
                         (bespoke-modeline-shorten-directory default-directory 32)))


;; ---------------------------------------------------------------------
(defun bespoke-modeline-mu4e-last-query ()
  "Get the most recent mu4e query or nil if there is none."
  (if (fboundp 'mu4e-last-query)
      (mu4e-last-query)
    mu4e~headers-last-query))

(defun bespoke-modeline-mu4e-context ()
  "Return the current mu4e context as a non propertized string."

  (if (> (length (mu4e-context-label)) 0)
      (concat "(" (substring-no-properties (mu4e-context-label) 1 -1) ")")
    "(none)"))

(defun bespoke-modeline-mu4e-server-props ()
  "Encapsulates the call to the variable mu4e-/~server-props
depending on the version of mu4e."
  (if (string> mu4e-mu-version "1.6.5")
      mu4e--server-props
    mu4e~server-props))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun bespoke-modeline-mu4e-dashboard-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                         "Mail"
                         (bespoke-modeline-mu4e-context)
                         (format "%d messages" (plist-get (bespoke-modeline-mu4e-server-props) :doccount))))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-mu4e-loading-mode-p ()
  (derived-mode-p 'mu4e-loading-mode))

(defun bespoke-modeline-mu4e-loading-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                         "Mail"
                         (bespoke-modeline-mu4e-context)
                         (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun bespoke-modeline-mu4e-main-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                         "Mail"
                         (bespoke-modeline-mu4e-context)
                         (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-mu4e-quote (str)
  (if (string> mu4e-mu-version "1.6.5")
      (mu4e~quote-for-modeline str)
    (mu4e-quote-for-modeline str)))

(defun bespoke-modeline-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun bespoke-modeline-mu4e-headers-mode ()
  (let ((mu4e-modeline-max-width 80))
    (bespoke-modeline-compose (bespoke-modeline-status)
                           (bespoke-modeline-mu4e-quote (bespoke-modeline-mu4e-last-query))
                           ""
                           (format "[%s]"
                                   (bespoke-modeline-mu4e-quote
                                    (mu4e-context-name (mu4e-context-current)))))))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun bespoke-modeline-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date     (mu4e-message-field msg :date)))
    (bespoke-modeline-compose (bespoke-modeline-status)
                           (bespoke-modeline-truncate subject 60)
                           ""
                           from)))

(defun bespoke-modeline-mu4e-view-hook ()
  (setq header-line-format "%-")
  (face-remap-add-relative 'header-line
                           '(:background "#ffffff"
                                         :underline nil
                                         :box nil
                                         :height 1.0)))
;; (add-hook 'mu4e-view-mode-hook #'bespoke-modeline-mu4e-view-hook)


;; ---------------------------------------------------------------------
(defun bespoke-modeline-bespoke-help-mode-p ()
  (derived-mode-p 'bespoke-help-mode))

(defun bespoke-modeline-bespoke-help-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                         "GNU Emacs / N Λ N O"
                         "(help)"
                         ""))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun bespoke-modeline-message-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                         "Message" "(draft)" ""))

;; ---------------------------------------------------------------------
;; (defvar org-mode-line-string nil)
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook #'bespoke-modeline-org-clock-out))

(defun bespoke-modeline-org-clock-out ()
  (setq org-mode-line-string nil)
  (force-mode-line-update))

(defun bespoke-modeline-org-clock-mode-p ()
  (and (boundp 'org-mode-line-string)
       (not org-mode-line-string)))

(defun bespoke-modeline-org-clock-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (bespoke-modeline-mode-name))
          (branch      (bespoke-modeline-vc-branch))
          (position    (format-mode-line "%l:%c")))
      (bespoke-modeline-compose (bespoke-modeline-status)
                             buffer-name 
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                             (propertize branch 'face 'italic)))
                                     ")" )
                             org-mode-line-string)))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-docview-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun bespoke-modeline-docview-mode ()
  (let ((buffer-name (format-mode-line "%b"))
    (mode-name   (bespoke-modeline-mode-name))
    (branch      (bespoke-modeline-vc-branch))
    (page-number (concat
              (number-to-string (doc-view-current-page)) "/"
              (or (ignore-errors
                (number-to-string (doc-view-last-page-number)))
              "???"))))
    (bespoke-modeline-compose
     (bespoke-modeline-status)
     buffer-name
     (concat "(" mode-name
         (if branch (concat ", "
                (propertize branch 'face 'italic)))
         ")" )
     page-number)))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(defun bespoke-modeline-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
    (mode-name   (bespoke-modeline-mode-name))
    (branch      (bespoke-modeline-vc-branch))
    (page-number (concat
              (number-to-string (pdf-view-current-page)) "/"
              (or (ignore-errors
                (number-to-string (pdf-cache-number-of-pages)))
              "???"))))
    (bespoke-modeline-compose
     "RW"
     buffer-name
     (concat "(" mode-name
         (if branch (concat ", "
                (propertize branch 'face 'italic)))
         ")" )
     page-number)))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-buffer-menu-mode-p ()
  (derived-mode-p 'buffer-menu-mode))

(defun bespoke-modeline-buffer-menu-mode ()
    (let ((buffer-name "Buffer list")
          (mode-name   (bespoke-modeline-mode-name))
          (position    (format-mode-line "%l:%c")))

      (bespoke-modeline-compose (bespoke-modeline-status)
                             buffer-name "" position)))
;;(defun buffer-menu-mode-header-line ()
;;  (face-remap-add-relative
;;   'header-line `(:background ,(face-background 'bespoke-subtle))))
;;(add-hook 'Buffer-menu-mode-hook
;;          #'buffer-menu-mode-header-line)

;; ---------------------------------------------------------------------
(defun bespoke-modeline-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun bespoke-modeline-completion-list-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (bespoke-modeline-mode-name))
          (position    (format-mode-line "%l:%c")))

      (bespoke-modeline-compose (bespoke-modeline-status)
                             buffer-name "" position)))

;; ---------------------------------------------------------------------
(with-eval-after-load 'deft
  (defun bespoke-modeline-deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun bespoke-modeline-deft-mode-p ()
  (derived-mode-p 'deft-mode))

(defun bespoke-modeline-deft-mode ()
  (let ((prefix (bespoke-modeline-status))
        (primary "Notes")
        (filter  (if deft-filter-regexp
                     (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (bespoke-modeline-compose prefix primary filter matches)))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun bespoke-modeline-text-mode-p ()
  (derived-mode-p 'text-mode))

(defun bespoke-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (bespoke-modeline-mode-name))
          (branch      (bespoke-modeline-vc-branch))
          (position    (format-mode-line "%l:%c")))
      (bespoke-modeline-compose (bespoke-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                            (propertize branch 'face 'italic)))
                                     ")" )
                             position)))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"
  
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))
  

;; ---------------------------------------------------------------------
(defun bespoke-modeline-face-clear (face)
  "Clear FACE"
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    'unspecified))


;; ---------------------------------------------------------------------
(defvar bespoke-modeline--saved-mode-line-format nil)
(defvar bespoke-modeline--saved-header-line-format nil)
(defvar bespoke-modeline--selected-window nil)

(defun bespoke-modeline--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq bespoke-modeline--selected-window (selected-window)))



(defun bespoke-modeline ()
  "Build and set the modeline."
  
  (let* ((format
          '((:eval
             (cond
              ((bespoke-modeline-user-mode-p)            (funcall ,bespoke-modeline-user-mode))
              ((bespoke-modeline-prog-mode-p)            (bespoke-modeline-default-mode))
              ((bespoke-modeline-message-mode-p)         (bespoke-modeline-message-mode))
              ((bespoke-modeline-elfeed-search-mode-p)   (bespoke-modeline-elfeed-search-mode))
              ((bespoke-modeline-elfeed-show-mode-p)     (bespoke-modeline-elfeed-show-mode))
              ((bespoke-modeline-deft-mode-p)            (bespoke-modeline-deft-mode))
              ((bespoke-modeline-info-mode-p)            (bespoke-modeline-info-mode))
              ((bespoke-modeline-calendar-mode-p)        (bespoke-modeline-calendar-mode))
              ((bespoke-modeline-org-capture-mode-p)     (bespoke-modeline-org-capture-mode))
              ((bespoke-modeline-org-agenda-mode-p)      (bespoke-modeline-org-agenda-mode))
              ((bespoke-modeline-org-clock-mode-p)       (bespoke-modeline-org-clock-mode))
              ((bespoke-modeline-term-mode-p)            (bespoke-modeline-term-mode))
              ((bespoke-modeline-vterm-mode-p)           (bespoke-modeline-term-mode))
              ((bespoke-modeline-mu4e-dashboard-mode-p)  (bespoke-modeline-mu4e-dashboard-mode))
              ((bespoke-modeline-mu4e-main-mode-p)       (bespoke-modeline-mu4e-main-mode))
              ((bespoke-modeline-mu4e-loading-mode-p)    (bespoke-modeline-mu4e-loading-mode))
              ((bespoke-modeline-mu4e-headers-mode-p)    (bespoke-modeline-mu4e-headers-mode))
              ((bespoke-modeline-mu4e-view-mode-p)       (bespoke-modeline-mu4e-view-mode))
              ((bespoke-modeline-text-mode-p)            (bespoke-modeline-default-mode))
              ((bespoke-modeline-pdf-view-mode-p)        (bespoke-modeline-pdf-view-mode))
              ((bespoke-modeline-docview-mode-p)         (bespoke-modeline-docview-mode))
              ;; ((bespoke-modeline-buffer-menu-mode-p)     (bespoke-modeline-buffer-menu-mode))
              ((bespoke-modeline-completion-list-mode-p) (bespoke-modeline-completion-list-mode))
              ((bespoke-modeline-bespoke-help-mode-p)       (bespoke-modeline-bespoke-help-mode))
              (t                                      (bespoke-modeline-default-mode)))))))
    
    (if (eq bespoke-modeline-position 'top)
        (progn
          (setq header-line-format format)
          (setq-default header-line-format format))
      (progn
        (setq mode-line-format format)
        (setq-default mode-line-format format)))))


(defun bespoke-modeline-mode--activate ()
  "Activate nano modeline"

  ;; Save current mode-line and header-line
  (unless bespoke-modeline--saved-mode-line-format
    (setq bespoke-modeline--saved-mode-line-format mode-line-format)
    (setq bespoke-modeline--saved-header-line-format header-line-format))

  ;; since the EIN library itself is constantly re-rendering the notebook, and thus
  ;; re-setting the header-line-format, we cannot use the bespoke-modeline function to set
  ;; the header format in a notebook buffer. Fortunately, EIN exposes the
  ;; ein:header-line-format variable for just this purpose.
  (with-eval-after-load 'ein
    (if (eq bespoke-modeline-position 'top)
        (setq ein:header-line-format '((:eval (bespoke-modeline-ein-notebook-mode))))))

  ;; Elfeed uses header-line, we need to tell it to use our own format
  (with-eval-after-load 'elfeed
    (if (eq bespoke-modeline-position 'top)
        (setq elfeed-search-header-function #'bespoke-modeline-elfeed-setup-header)))
  
  (with-eval-after-load 'calendar
    (add-hook 'calendar-initial-window-hook
              #'bespoke-modeline-calendar-setup-header))

  (with-eval-after-load 'org-clock
    (add-hook 'org-clock-out-hook #'bespoke-modeline-org-clock-out))
  
  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook
              #'bespoke-modeline-org-capture-turn-off-header-line))

  (with-eval-after-load 'esh-mode
    (setq eshell-status-in-mode-line nil))

  (with-eval-after-load 'mu4e
    (advice-add 'mu4e~header-line-format :override #'bespoke-modeline))

  (if (eq bespoke-modeline-position 'top)
      (setq Info-use-header-line nil))

  (if (eq bespoke-modeline-position 'top)
      (setq Buffer-menu-use-header-line nil))

  ;; Update selected window
  (bespoke-modeline--update-selected-window)
  ;; (setq bespoke-modeline--selected-window (selected-window))

  (setq         mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq         header-line-format nil)
  (setq-default header-line-format nil)
      
  (bespoke-modeline)
  
  ;; This hooks is necessary to register selected window because when
  ;;  a modeline is evaluated, the corresponding window is always selected.
  (add-hook 'post-command-hook #'bespoke-modeline--update-selected-window)

  (force-mode-line-update t))


(defun bespoke-modeline-mode--inactivate ()
  "Inactivate nano mode line and restored default mode-line"
  
  (custom-reevaluate-setting 'Info-use-header-line)
  (custom-reevaluate-setting 'Buffer-menu-use-header-line)
  (custom-reevaluate-setting 'eshell-status-in-mode-line)

  (if (boundp 'ein:header-line-format)
      (setq ein:header-line-format '(:eval (ein:header-line))))
  (if (boundp 'elfeed-search-header-function)
      (setq elfeed-search-header-function #'elfeed-search--header))
  
  (remove-hook 'calendar-initial-window-hook
               #'bespoke-modeline-calendar-setup-header)
  (remove-hook 'org-capture-mode-hook
               #'bespoke-modeline-org-capture-turn-off-header-line)
  (remove-hook 'org-clock-out-hook
               #'bespoke-modeline-org-clock-out)
  (remove-hook 'post-command-hook
               #'bespoke-modeline--update-selected-window)
  (advice-remove 'mu4e~header-line-format #'bespoke-modeline)

  (setq         mode-line-format bespoke-modeline--saved-mode-line-format)
  (setq-default mode-line-format bespoke-modeline--saved-mode-line-format)
  (setq         header-line-format bespoke-modeline--saved-header-line-format)
  (setq-default header-line-format bespoke-modeline--saved-header-line-format))


;;;###autoload
(define-minor-mode bespoke-modeline-mode
  "Toggle bespoke-modeline minor mode"
  :group 'nano
  :global t
  :init-value nil

  (if bespoke-modeline-mode
      (bespoke-modeline-mode--activate)
    (bespoke-modeline-mode--inactivate))

  ;; Run any registered hooks
  (run-hooks 'bespoke-modeline-mode-hook))


(provide 'bespoke-modeline)
;;; bespoke-modeline.el ends here
