;;; bespoke-modeline.el -- custom mode line for bespoke theme  ;; -*- lexical-binding: t -*-
;; Copyright (C) 2020 Colin McLear
;; -------------------------------------------------------------------
;; Authors: Colin McLear
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
;; A bespoke modeline.
;; This mode line originated as a fork of the nano-emacs modeline.
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
;; You may also toggle the modeline to the top or bottom using M-x: bespoke-modeline-toggle

;;; Code:

;;;; Group
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


;;;; Faces

(defface bespoke-modeline-active
  '((t (:inherit (mode-line))))
  "Modeline face for active modeline"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-active-name
  '((t (:inherit (mode-line))))
  "Modeline face for active name element"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-active-primary
  '((t (:weight light :inherit (mode-line))))
  "Modeline face for active primary element"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-active-secondary
  '((t (:inherit mode-line)))
  "Modeline face for active secondary element"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-active-status-RO
  '((t (:inherit default :background "yellow" :box (:line-width 2 :color "yellow" :style nil))))
  "Modeline face for active READ-ONLY element"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-active-status-RW
  '((t (:inherit default :background "green" (:line-width 2 :color "green" :style nil))))
  "Modeline face for active READ-WRITE element"
  :group 'bespoke-modeline-active)

(defface bespoke-modeline-active-status-**
  '((t (:inherit default :background "red" (:line-width 2 :color "red" :style nil))))
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
  '((t (:foreground "light gray" :background "dark gray" (:line-width 2 :color "dark gray" :style nil))))
  "Modeline face for inactive READ-ONLY element"
  :group 'bespoke-modeline-inactive)

(defface bespoke-modeline-inactive-status-RW
  '((t (:foreground "light gray" :background "dark gray" (:line-width 2 :color "dark gray" :style nil))))
  "Modeline face for inactive READ-WRITE element"
  :group 'bespoke-modeline-inactive)

(defface bespoke-modeline-inactive-status-**
  '((t (:foreground "light gray" :background "dark gray" (:line-width 2 :color "dark gray" :style nil))))
  "Modeline face for inactive MODIFIED element"
  :group 'bespoke-modeline-inactive)



;;; Modeline Options
(defcustom bespoke-modeline-position 'top
  "Default modeline position (top or bottom)"
  :type '(choice
          (const :tag "Nil" nil)
          (const :tag "Top"    top)
          (const :tag "Bottom" bottom))
  :group 'bespoke-modeline)

(defcustom bespoke-modeline-user-mode nil
  "User supplied mode to be evaluated for modeline."
  :type '(choice (const nil) function)
  :group 'bespoke-modeline)

(defcustom bespoke-modeline-size 3
  "Set the size of the modeline as an integer
Initial value is 3."
  :group 'bespoke-modeline
  :type 'integer)

(defcustom bespoke-modeline-cleaner nil
  "If t then show abbreviated mode symbol in modeline. Default is
nil. To change the values of the major-mode symbols see the value
of bespoke-modeline-cleaner-alist"
  :group 'bespoke-modeline
  :type 'boolean)

(defcustom bespoke-modeline-git-diff-mode-line t
  "If t then show diff lines in modeline."
  :group 'bespoke-modeline
  :type 'boolean)

(defcustom bespoke-modeline-vc-symbol ""
  "Symbol to use in buffers visiting files under version control"
  :group 'bespoke-modeline
  :type 'string)

;; Visual Bell
(defcustom bespoke-modeline-visual-bell t
  "If t then use bespoke-modeline-visual-bell."
  :group 'bespoke-themes
  :type 'boolean)

;; Mode line symbols
(defcustom bespoke-modeline-gui-ro-symbol " ⨂ "
  "Modeline gui read-only symbol."
  :group 'bespoke-modeline
  :type 'string)

(defcustom bespoke-modeline-gui-mod-symbol " ⨀ "
  "Modeline gui modified symbol."
  :group 'bespoke-modeline
  :type 'string)

(defcustom bespoke-modeline-gui-rw-symbol " ◯ "
  "Modeline gui read-write symbol."
  :group 'bespoke-modeline
  :type 'string)

(defcustom bespoke-modeline-tty-ro-symbol " RO "
  "Modeline tty read-only symbol."
  :group 'bespoke-modeline
  :type 'string)

(defcustom bespoke-modeline-tty-mod-symbol " ** "
  "Modeline tty modified symbol."
  :group 'bespoke-modeline
  :type 'string)

(defcustom bespoke-modeline-tty-rw-symbol " RW "
  "Modeline tty read-write symbol."
  :group 'bespoke-modeline
  :type 'string)

(defcustom bespoke-modeline-truncate-value 30
  "Value of modeline truncate-length function."
  :group 'bespoke-modeline
  :type 'integer)

(defcustom bespoke-modeline-spacer ""
  "Space adjustment for right end of modeline."
  :type 'string
  :group 'bespoke-modeline)

(defcustom bespoke-modeline-space-top +0.20
  "Space adjustment for top of modeline
 Possitive is upwards"
  :type 'float
  :group 'bespoke-modeline)

(defcustom bespoke-modeline-space-bottom -0.25
  "Space adjustment for bottom of modeline
 Negative is downwards."
  :type 'float
  :group 'bespoke-modeline)

;;; Optional Functions

;;;; Visual bell for mode line

;; See https://github.com/hlissner/emacs-doom-themes for the idea

(require 'face-remap)

(defface bespoke-modeline-visual-bell '((t (:underline "red3")))
  "Face to use for the mode-line when `bespoke-modeline-visual-bell-config' is used."
  :group 'bespoke-modeline)

;;;###autoload
(defun bespoke-modeline-visual-bell-fn ()
  "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
  (let ((bespoke-modeline--bell-cookie (face-remap-add-relative 'mode-line 'bespoke-modeline-visual-bell)))
    (force-mode-line-update)
    (run-with-timer 0.15 nil
                    (lambda (cookie buf)
                      (with-current-buffer buf
                        (face-remap-remove-relative cookie)
                        (force-mode-line-update)))
                    bespoke-modeline--bell-cookie
                    (current-buffer))))

;;;###autoload
(defun bespoke-modeline-visual-bell-config ()
  "Enable flashing the mode-line on error."
  (setq ring-bell-function #'bespoke-modeline-visual-bell-fn
        visible-bell t))

(when bespoke-modeline-visual-bell
  (bespoke-modeline-visual-bell-config))


;;;; Clean mode line
;; Source: https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
(require 'cl-lib)

(defvar bespoke-modeline-cleaner-alist
  `((dired-mode . "Dir")
    (emacs-lisp-mode . "EL")
    (fundamental-mode . "F")
    (helpful-mode . "")
    (help-mode . "")
    (lisp-interaction-mode . "λ")
    (markdown-mode . "MD")
    (magit-mode . "MG")
    (nxhtml-mode . "NX")
    (prog-mode . "PR")
    (python-mode . "PY")
    (text-mode . "TX")
    )
  "Alist for `bespoke-modeline/clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *as substitute for* the original.")

(defun bespoke-modeline/clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in bespoke-modeline-cleaner-alist
           do (let* ((mode (car cleaner))
                     (mode-str (cdr cleaner))
                     (old-mode-str (cdr (assq mode minor-mode-alist))))
                (when old-mode-str
                  (setcar old-mode-str mode-str))
                ;; major mode
                (when (eq mode major-mode)
                  (setq mode-name mode-str)))))

(when bespoke-modeline-cleaner
  (add-hook 'after-change-major-mode-hook #'bespoke-modeline/clean-mode-line))



;;; Modeline Functions
;;;; Base Functions
(defun bespoke-modeline-user-mode-p ()
  "Should the user supplied mode be called for modeline?"
  bespoke-modeline-user-mode)

(defun bespoke-modeline-truncate (str size &optional ellipsis)
  "If STR is longer than SIZE, truncate it and add ELLIPSIS."

  (let ((ellipsis (or ellipsis "…")))
    (if (> (length str) size)
        (format "%s%s" (substring str 0 (- size (length ellipsis))) ellipsis)
      str)))

(defun bespoke-modeline-mode-name ()
  "Return current major mode name"
  (format-mode-line mode-name))

;;;; Branch display
;; -------------------------------------------------------------------
(defun bespoke-project-name ()
  "return name of project without path"
  (file-name-nondirectory (directory-file-name (if (vc-root-dir) (vc-root-dir) "-"))))

(defun bespoke-modeline-vc-project-branch ()
  "If buffer is visiting a file under version control, show project and branch name for file. Otherwise show '-'"
  (let ((backend (vc-backend buffer-file-name)))
    (concat
     (if buffer-file-name
         (if vc-mode
             (let ((project-name (bespoke-project-name)))
               ;; Project name
               (unless (string= "-" project-name)
                 (concat
                  ;; Divider
                  (propertize " •" 'face `(:inherit fringe))
                  (format " %s" project-name)
                  )))))
     ;; Show branch
     (if vc-mode
         (concat
          bespoke-modeline-vc-symbol (substring-no-properties vc-mode ;    
                                                              (+ (if (eq backend 'Hg) 2 3) 2)))  nil))))
;;;; Git diff in modeline
;; https://cocktailmake.github.io/posts/emacs-modeline-enhancement-for-git-diff/
(when bespoke-modeline-git-diff-mode-line
  (defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
    "Show the information of git diff on modeline."
    (setq ad-return-value
	      (concat ad-return-value
		          (let ((plus-minus (vc-git--run-command-string
				                     file "diff" "--numstat" "--")))
		            (if (and plus-minus
		                     (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
		                (concat
                         " "
			             (format "+%s" (match-string 1 plus-minus))
			             (format "-%s" (match-string 2 plus-minus)))
		              (propertize "" 'face '(:weight bold))))))))

;;;; Dir display


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

;;;; Mode line status
;; ---------------------------------------------------------------------
(defun bespoke-modeline-status ()
  "Return buffer status: default symbols are read-only (⨂)/(RO),
modified (⨀)/(**), or read-write (◯)/(RW)"
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    ;; Use status letters for TTY display
    (cond
     (modified
      (if (display-graphic-p)
          bespoke-modeline-gui-mod-symbol
        bespoke-modeline-tty-mod-symbol))
     (read-only
      (if (display-graphic-p)
          bespoke-modeline-gui-ro-symbol
        bespoke-modeline-tty-ro-symbol))
     (t (if (display-graphic-p)
            bespoke-modeline-gui-rw-symbol
          bespoke-modeline-tty-rw-symbol)))))

;;;; Compose mode line
;; -------------------------------------------------------------------

(defun bespoke-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (active        (eq window bespoke-modeline--selected-window))
         (prefix (if (display-graphic-p)
                     (cond ((string= status bespoke-modeline-gui-ro-symbol)
                            (propertize (if (window-dedicated-p)" –– " bespoke-modeline-gui-ro-symbol)
                                        'face (if active
                                                  'bespoke-modeline-active-status-RO
                                                'bespoke-modeline-inactive-status-RO)))
                           ((string= status bespoke-modeline-gui-mod-symbol)
                            (propertize (if (window-dedicated-p)" –– " bespoke-modeline-gui-mod-symbol)
                                        'face (if active
                                                  'bespoke-modeline-active-status-**
                                                'bespoke-modeline-inactive-status-**)))
                           ((string= status bespoke-modeline-gui-rw-symbol)
                            (propertize (if (window-dedicated-p) " –– " bespoke-modeline-gui-rw-symbol)
                                        'face (if active 'bespoke-modeline-active-status-RW
                                                'bespoke-modeline-inactive-status-RW)))
                           (t (propertize status
                                          'face (if active 'bespoke-modeline-active-status-**
                                                  'bespoke-modeline-inactive-status-**))))
                   ;; TTY displays
                   (cond ((string= status bespoke-modeline-tty-ro-symbol)
                          (propertize
                           (if (window-dedicated-p) " -- " bespoke-modeline-tty-ro-symbol)
                           'face (if active
                                     'bespoke-modeline-active-status-RO
                                   'bespoke-modeline-inactive-status-RO)))
                         ((string= status bespoke-modeline-tty-mod-symbol)
                          (propertize
                           (if (window-dedicated-p) " -- " bespoke-modeline-tty-mod-symbol)
                           'face (if active
                                     'bespoke-modeline-active-status-**
                                   'bespoke-modeline-inactive-status-**)))
                         ((string= status bespoke-modeline-tty-rw-symbol)
                          (propertize
                           (if (window-dedicated-p) " -- " bespoke-modeline-tty-rw-symbol)
                           'face (if active 'bespoke-modeline-active-status-RW
                                   'bespoke-modeline-inactive-status-RW)))
                         (t (propertize status
                                        'face (if active 'bespoke-modeline-active-status-**
                                                'bespoke-modeline-inactive-status-**))))))

         (left (concat
                (propertize " "  'face (if active 'bespoke-modeline-active
                                         'bespoke-modeline-inactive)
                            'display `(raise ,bespoke-modeline-space-top))
                (propertize name 'face (if active 'bespoke-modeline-active-name
                                         'bespoke-modeline-inactive-name))
                (propertize " "  'face (if active 'bespoke-modeline-active
                                         'bespoke-modeline-inactive)
                            'display `(raise ,bespoke-modeline-space-bottom))
                (propertize primary 'face (if active 'bespoke-modeline-active-primary
                                            'bespoke-modeline-inactive-primary))))
         (right (concat secondary " " bespoke-modeline-spacer))

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

;;;; Default display

(defun bespoke-modeline-default-mode ()
  (let ((buffer-name (format-mode-line (if buffer-file-name (file-name-nondirectory (buffer-file-name)) "%b")))
        (mode-name   (bespoke-modeline-mode-name))
        (branch      (bespoke-modeline-vc-project-branch))
        (position    (format-mode-line "%l:%c ")))
    (bespoke-modeline-compose (bespoke-modeline-status)
                              (bespoke-modeline-truncate buffer-name bespoke-modeline-truncate-value)
                              (concat "(" mode-name
                                      (when branch
                                        branch)
                                      ")")
                              (concat
                               ;; Narrowed buffer
                               (when (buffer-narrowed-p)
                                 (propertize "⇥ "  'face `(:inherit fringe)))
                               position))))

;;;; Prog & Text Modes
;; ---------------------------------------------------------------------
(defun bespoke-modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun bespoke-modeline-elisp-mode-p ()
  (derived-mode-p 'lisp-data-mode))

(defun bespoke-modeline-text-mode-p ()
  (derived-mode-p 'text-mode))

;;;; Info Display
;; ---------------------------------------------------------------------
(setq Info-use-header-line nil)
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

;;;; Term & Vterm
;; ---------------------------------------------------------------------
;; term
(defun bespoke-modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

;; vterm
(defun bespoke-modeline-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun bespoke-modeline-term-mode ()
  (bespoke-modeline-compose " >_ "
                            "Terminal"
                            (concat "(" (file-name-nondirectory shell-file-name) ")")
                            (bespoke-modeline-shorten-directory default-directory 32)))

;; ---------------------------------------------------------------------

(defun bespoke-modeline-get-ssh-host (_str)
  (let ((split-defdir (split-string default-directory)))
    (if (equal (length split-defdir) 1)
        (car (split-string (shell-command-to-string "hostname") "\n"))
      (cadr split-defdir))))

(defun bespoke-modeline-ssh-mode ()
  (bespoke-modeline-compose " >_ "
                            "Terminal"
                            (concat "(" (bespoke-modeline-get-ssh-host default-directory) ")")
                            (bespoke-modelibe-shorten-directory (car (last (split-string default-directory ":"))) 32)))

;;;; Message Mode
;; ---------------------------------------------------------------------
(defun bespoke-modeline-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun bespoke-modeline-message-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                            "Message" "(draft)" ""))

;;;; Docview Mode
;;---------------------------------------------------------------------
(defun bespoke-modeline-docview-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun bespoke-modeline-docview-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	    (mode-name   (bespoke-modeline-mode-name))
	    (branch      (bespoke-modeline-vc-project-branch))
	    (page-number (concat
		              (number-to-string (doc-view-current-page)) "/"
		              (or (ignore-errors
			                (number-to-string (doc-view-last-page-number)))
			              "???"))))
    (bespoke-modeline-compose
     (bespoke-modeline-status)
     buffer-name
     (concat "(" mode-name
             branch
	         ")" )
     page-number)))

;;;; PDF View Mode
;; ---------------------------------------------------------------------
(defun bespoke-modeline-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(with-eval-after-load 'pdf-tools
  (require 'pdf-view))

(defun bespoke-modeline-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	    (mode-name   (bespoke-modeline-mode-name))
	    (branch      (bespoke-modeline-vc-project-branch))
	    (page-number (concat
		              (number-to-string (eval `(pdf-view-current-page))) "/"
		              (or (ignore-errors
			                (number-to-string (pdf-cache-number-of-pages)))
			              "???"))))
    (bespoke-modeline-compose
     (bespoke-modeline-status)
     buffer-name
     (concat "(" mode-name
             branch
	         ")" )
     (concat page-number " "))))

;;;; MenuMode

(defun bespoke-modeline-buffer-menu-mode-p ()
  (derived-mode-p 'buffer-menu-mode))

(defun bespoke-modeline-buffer-menu-mode ()
  (let ((buffer-name "Buffer list")
        (mode-name   (bespoke-modeline-mode-name))
        (position    (format-mode-line "%l:%c ")))

    (bespoke-modeline-compose (bespoke-modeline-status)
                              buffer-name "" position)))


;;;; Completion
;; ---------------------------------------------------------------------
(defun bespoke-modeline-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun bespoke-modeline-completion-list-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (bespoke-modeline-mode-name))
        (position    (format-mode-line "%l:%c ")))

    (bespoke-modeline-compose (bespoke-modeline-status)
                              buffer-name "" position)))

;;;; Deft Mode

(with-eval-after-load 'deft
  (defun deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun bespoke-modeline-deft-mode-p ()
  (derived-mode-p 'deft-mode))

(defun bespoke-modeline-deft-mode ()
  (let ((prefix " DEFT ")
        (primary "Search:")
        (filter  (if deft-filter-regexp
                     (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (bespoke-modeline-compose prefix primary filter matches)))

;;;; Calendar Mode
;; ---------------------------------------------------------------------
(defun bespoke-modeline-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun bespoke-modeline-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun bespoke-modeline-calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default)))))
(add-hook 'calendar-initial-window-hook #'bespoke-modeline-calendar-setup-header)

;;;; Org Capture
;; ---------------------------------------------------------------------
(defun bespoke-modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun bespoke-modeline-org-capture-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                            "Capture"
                            "(Org)"
                            ""))

(with-eval-after-load 'org-capture
  (defun bespoke-modeline--org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value nil))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'bespoke-modeline--org-capture-turn-off-header-line))

;;;; Org Agenda
;; ---------------------------------------------------------------------
(defun bespoke-modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun bespoke-modeline-org-agenda-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                            "Agenda"
                            ""
                            (concat (propertize "◴"
                                                'face 'default
                                                'display '(raise 0.06))
                                    (format-time-string "%H:%M "))))

;;;; Org Clock
;; ---------------------------------------------------------------------
(defun bespoke-modeline-org-clock-mode-p ()
  (and (boundp 'org-mode-line-string)
       (stringp org-mode-line-string))
  (bespoke-modeline-org-clock-deactivate))

(defun bespoke-modeline-org-clock-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (bespoke-modeline-mode-name))
        (branch      (bespoke-modeline-vc-project-branch))
        (position    (format-mode-line "%l:%c")))
    (bespoke-modeline-compose (bespoke-modeline-status)
                              buffer-name
                              (concat "(" mode-name
                                      (when branch
                                        branch)
                                      ")" )
                              (concat
                               ;; Narrowed buffer
                               (when (buffer-narrowed-p)
                                 (propertize "⇥ "  'face `(:inherit fringe)))
                               org-mode-line-string
                               " "
                               position
                               " "))))

(defun bespoke-modeline-org-clock-out ()
  (setq org-mode-line-string nil)
  (force-mode-line-update))

(defun bespoke-modeline-org-clock-deactivate ()
  (with-eval-after-load 'org-clock
    (add-hook 'org-clock-out-hook #'bespoke-modeline-org-clock-out)))

;;;; Elfeed
(defun bespoke-modeline-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun bespoke-modeline-elfeed-search-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                            "Elfeed"
                            (concat "(" (elfeed-search--header)  ")")
                            ""))

(defun bespoke-modeline-elfeed-setup-header ()
  (setq header-line-format (default-value 'header-line-format)))

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

;;;; Mu4e

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

(defun bespoke-modeline-mu4e-activate ()
  (with-eval-after-load 'mu4e
    (advice-add 'mu4e~header-line-format :override #'bespoke-modeline)))

(defun bespoke-modeline-mu4e-deactivate ()
  (advice-remove #'mu4e~header-line-format #'bespoke-modeline))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun bespoke-modeline-mu4e-dashboard-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                            (format "%d messages"
                                    (plist-get (bespoke-modeline-mu4e-server-props) :doccount))
                            ""
                            " "))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-mu4e-loading-mode-p ()
  (derived-mode-p 'mu4e-loading-mode))


(defun bespoke-modeline-mu4e-loading-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                            "Loading..."
                            (bespoke-modeline-mu4e-context)
                            (format-time-string "%A %d %B %Y, %H:%M ")))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun bespoke-modeline-mu4e-main-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                            (bespoke-modeline-mu4e-context)
                            ""
                            (format-time-string "%A %d %B %Y, %H:%M ")))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-mu4e-compose-mode-p ()
  (derived-mode-p 'mu4e-compose-mode))

(defun bespoke-modeline-mu4e-compose-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                            (format-mode-line "%b")
                            ""
                            (format "[%s] "
                                    (bespoke-modeline-mu4e-quote
                                     (mu4e-context-name (mu4e-context-current))))))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-mu4e-quote (str)
  (if (version< "1.6.5" mu4e-mu-version)
      (mu4e~quote-for-modeline str)
    (mu4e-quote-for-modeline str)))

(defun bespoke-modeline-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun bespoke-modeline-mu4e-headers-mode ()
  (let ((mu4e-modeline-max-width 80))
    (bespoke-modeline-compose (bespoke-modeline-status)
                              "Search:"
                              (or (bespoke-modeline-mu4e-quote
                                   (bespoke-modeline-mu4e-last-query)) "")
                              (format "[%s] "
                                      (bespoke-modeline-mu4e-quote
                                       (mu4e-context-name (mu4e-context-current)))))))

;; ---------------------------------------------------------------------
(defun bespoke-modeline-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun bespoke-modeline-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date    (mu4e-message-field msg :date)))
    (bespoke-modeline-compose (bespoke-modeline-status)
                              (or from "")
                              (concat "(" (bespoke-modeline-truncate (or subject "") 50 "…") ")")
                              (concat (or (format-time-string mu4e-headers-date-format date) "") " "))))

;;;; Help

(defun bespoke-modeline-bespoke-help-mode-p ()
  (derived-mode-p 'bespoke-help-mode))

(defun bespoke-modeline-bespoke-help-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                            "GNU Emacs"
                            "(help)"
                            ""))

;;;; Ein

(defun bespoke-modeline-ein-notebook-mode ()
  (let ((buffer-name (format-mode-line "%b")))
    (bespoke-modeline-compose (if (ein:notebook-modified-p) "**" "RW")
                              buffer-name
                              ""
                              (ein:header-line))))

;;; Set Mode line

;;;; Clear Modeline Faces

(defun bespoke-modeline-face-clear (face)
  "Clear FACE"
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    'unspecified))


;;;; Define Selected Window

(defvar bespoke-modeline--saved-mode-line-format nil)
(defvar bespoke-modeline--saved-header-line-format nil)
(defvar bespoke-modeline--selected-window nil)

(defun bespoke-modeline--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq bespoke-modeline--selected-window (selected-window)))

;;;; Set content for mode/header line

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
              ((bespoke-modeline-buffer-menu-mode-p)     (bespoke-modeline-buffer-menu-mode))
              ((bespoke-modeline-completion-list-mode-p) (bespoke-modeline-completion-list-mode))
              ((bespoke-modeline-bespoke-help-mode-p)    (bespoke-modeline-bespoke-help-mode))
              (t                                         (bespoke-modeline-default-mode)))))))
    
    (if (eq bespoke-modeline-position 'top)
        (progn
          (setq-default mode-line-format (list (propertize "%_" 'face `(:inherit fringe))))
          (setq header-line-format format)
          (setq-default header-line-format format))
      (progn
        (setq mode-line-format format)
        (setq-default mode-line-format format)))))


(defun bespoke-modeline-mode--activate ()
  "Activate bespoke modeline"

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

  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook
              #'bespoke-modeline--org-capture-turn-off-header-line))

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

  (setq         mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq         header-line-format nil)
  (setq-default header-line-format nil)

  (bespoke-modeline)
  
  ;; This hook is necessary to register selected window because when
  ;;  a modeline is evaluated, the corresponding window is always selected.
  (add-hook 'post-command-hook #'bespoke-modeline--update-selected-window)
  (force-mode-line-update t))


(defun bespoke-modeline-mode--deactivate ()
  "Deactivate bespoke mode-line and restore default mode-line"
  
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
               #'bespoke-modeline--org-capture-turn-off-header-line)
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
  :group 'bespoke-modeline
  :global t
  :init-value nil

  (if bespoke-modeline-mode
      (progn
        ;; (bespoke-modeline--set-line-faces)
        (bespoke-modeline-mode--activate))
    (bespoke-modeline-mode--deactivate))

  ;; Run any registered hooks
  (run-hooks 'bespoke-modeline-mode-hook))

;; Toggle functions
(defun bespoke-modeline-get-current-theme ()
  "Determines the currently enabled theme."
  (or (car custom-enabled-themes) '*default*))

;;;###autoload
(defun bespoke-modeline-reload-current-theme ()
  "Reloads the currently activated theme."
  (bespoke-modeline-enable-theme (bespoke-modeline-get-current-theme)))

;;;###autoload
(defun bespoke-modeline-enable-theme (theme)
  "Enables the specified color-theme.
Pass `*default*' to select Emacs defaults."
  (cl-flet* ((disable-all-themes ()
                                 (mapcar 'disable-theme
	                                     custom-enabled-themes)))
    (disable-all-themes)
    (condition-case nil
        (when (not (eq theme '*default*))
          (load-theme theme t))
      (error nil))))

(defun bespoke-modeline--set-line-faces ()
  (cond ((and (fboundp 'bespoke-modeline)
              (eq bespoke-modeline-position 'top))
         (custom-set-faces
          ;; '(mode-line-active ((t (:inherit fixed-pitch))))
          ;; '(header-line ((t (:height 2))))
          '(mode-line ((t (:height 0.1 :underline "light gray" :overline nil :box nil))))
          '(mode-line-inactive ((t (:height 0.1 :underline "light gray" :overline nil :box nil))))))
        ;; Modeline at footer
        ((and (fboundp 'bespoke-modeline)
              (eq bespoke-modeline-position 'bottom))
         (custom-set-faces
          '(mode-line ((t (:height 1))))
          '(mode-line-inactive ((t (:height 1))))))))

(defun bespoke-modeline-toggle ()
  "Toggle between a modeline in header or one at footer. Note that
this function reloads the theme to properly set colors and that
you may need to revert buffers to see the modeline properly."
  (interactive)
  (if (eq bespoke-modeline-position 'top)
      (progn
        (bespoke-modeline-face-clear 'mode-line)
        (setq bespoke-modeline-position 'bottom)
        (bespoke-modeline-mode--deactivate)
        ;; (bespoke-modeline--set-line-faces)
        (bespoke-modeline-mode--activate)
        (bespoke-modeline-reload-current-theme)
        (run-hooks 'bespoke-modeline-mode-hook))
    (progn
      (setq bespoke-modeline-position 'top)
      (bespoke-modeline-mode--deactivate)
      ;; (bespoke-modeline--set-line-faces)
      (setq-default mode-line-format (list (propertize "%_" 'face `(:inherit fringe))))
      (bespoke-modeline-mode--activate)
      (bespoke-modeline-reload-current-theme)
      (run-hooks 'bespoke-modeline-mode-hook))))

(provide 'bespoke-modeline)
;;; bespoke-modeline.el ends here
