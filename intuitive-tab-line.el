;;; intuitive-tab-line.el --- Browser-like tabs for tab-line  -*- lexical-binding: t; -*-

;; Author: Eddie Drury <eddie.drury@gmail.com>
;; URL: https://github.com/thread314/intuitive-tab-line-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tabs

;;; Commentary:

;; Makes Emacs tabs behave the way tabs behave in a browser: every buffer
;; the user opens gets its own tab, and the order of those tabs persists
;; until it is changed by hand.
;;
;; The tabs are simply an ordered list of buffers,
;; `intuitive-tab-line--current-tab-list', which is installed as
;; `tab-line-tabs-function'.  The commands below manipulate that list.
;;
;; Suggested setup:
;;
;;   (setq tab-line-tabs-function 'intuitive-tab-line-buffers-list)
;;   (global-tab-line-mode 1)

;;; Code:

(require 'tab-line)
(require 'seq)
(require 'cl-lib)

(defgroup intuitive-tab-line nil
  "Browser-like tabs for `tab-line-mode'."
  :group 'convenience
  :prefix "intuitive-tab-line-")

(defcustom intuitive-tab-line-exclude-regexps
  '("magit"                             ; magit buffers
    "COMMIT_EDITMSG"                    ; commit message buffers
    "CAPTURE-"                          ; org capture buffers
    "\\*org-roam\\*"
    "\\*NeoTree\\*")
  "Buffers whose names match any of these regexps never get a tab.
Checked before `intuitive-tab-line-include-functions'."
  :type '(repeat regexp))

(defcustom intuitive-tab-line-include-functions
  (list #'buffer-file-name              ; all file buffers
        (lambda (buffer)                ; dired buffers
          (buffer-local-value 'list-buffers-directory buffer))
        (lambda (buffer)                ; help buffers
          (string-match-p "\\*help\\*" (buffer-name buffer)))
        #'buffer-base-buffer)           ; indirect buffers
  "Predicates deciding which buffers get their own tab.
Each is called with one argument, the buffer.  A buffer is given a
tab as soon as any of them returns non-nil."
  :type '(repeat function))

(defvar intuitive-tab-line--current-tab-list nil
  "Ordered list of the buffers currently displayed as tabs.")

(defvar intuitive-tab-line--default-tab nil
  "Buffer set by `intuitive-tab-line-set-default-tab', or nil.")

(defvar intuitive-tab-line--stored-tab-list nil
  "Tab list saved by `intuitive-tab-line-store-tabs'.")

(defun intuitive-tab-line--prune ()
  "Drop killed buffers from the tab list and return it."
  (setq intuitive-tab-line--current-tab-list
        (seq-filter #'buffer-live-p intuitive-tab-line--current-tab-list)))

(defun intuitive-tab-line-buffers-list ()
  "Return the list of buffers to display as tabs.
This is meant to be used as `tab-line-tabs-function'."
  (intuitive-tab-line--prune))

(defun intuitive-tab-line--forget-buffer ()
  "Remove the buffer being killed from the tab list.
Added to `kill-buffer-hook' so that no dead buffer can reach `tab-line'."
  (setq intuitive-tab-line--current-tab-list
        (delq (current-buffer) intuitive-tab-line--current-tab-list))
  (when (eq intuitive-tab-line--default-tab (current-buffer))
    (setq intuitive-tab-line--default-tab nil)))

(defun intuitive-tab-line--add-buffer (buffer)
  "Append BUFFER to the tab list unless it is already there."
  (when (and (buffer-live-p buffer)
             (not (memq buffer intuitive-tab-line--current-tab-list)))
    (setq intuitive-tab-line--current-tab-list
          (append (intuitive-tab-line--prune) (list buffer)))))

(defun intuitive-tab-line--should-add-p (buffer)
  "Return non-nil if BUFFER ought to be given a tab of its own."
  (let ((name (buffer-name buffer)))
    (and name
         (not (seq-some (lambda (regexp) (string-match-p regexp name))
                        intuitive-tab-line-exclude-regexps))
         (seq-some (lambda (predicate) (funcall predicate buffer))
                   intuitive-tab-line-include-functions))))

(defun intuitive-tab-line-manually-add-current-buffer-to-tab ()
  "Create a tab for the current buffer, whatever kind of buffer it is."
  (interactive)
  (intuitive-tab-line--add-buffer (current-buffer))
  (force-mode-line-update))

(defun intuitive-tab-line-add-current-buffer-to-tab (&rest _args)
  "Create a tab for the current buffer if it qualifies for one.
Which buffers qualify is decided by `intuitive-tab-line-exclude-regexps'
and `intuitive-tab-line-include-functions'."
  (interactive)
  (when (intuitive-tab-line--should-add-p (current-buffer))
    (intuitive-tab-line--add-buffer (current-buffer)))
  (force-mode-line-update))

(defun intuitive-tab-line--switch-before-drop ()
  "Switch away from the current buffer before its tab is dropped.
Switch to the tab on the left, or to the one on the right if the
current tab is leftmost.  Return non-nil if the switch happened, and
nil if the current tab cannot be dropped.

Switching first prevents a previously dropped buffer from
unexpectedly returning to the tab list."
  (let ((n (seq-position intuitive-tab-line--current-tab-list (current-buffer))))
    (cond
     ((null n)
      (message "Current buffer has no tab to drop")
      nil)
     ((= (length intuitive-tab-line--current-tab-list) 1)
      (message "Only one tab open, cannot drop")
      nil)
     ((= n 0)
      (switch-to-buffer (nth 1 intuitive-tab-line--current-tab-list))
      t)
     (t
      (switch-to-buffer (nth (1- n) intuitive-tab-line--current-tab-list))
      t))))

(defun intuitive-tab-line-drop-tab (&optional kill)
  "Remove the tab for the current buffer.
Indirect, Dired and help buffers are killed, as are all buffers when
KILL is non-nil.  Every other buffer is left open, just without a tab."
  (interactive)
  (let ((buffer-to-drop (current-buffer)))
    (when (intuitive-tab-line--switch-before-drop)
      ;; `buffer-file-name' is nil for Dired and help buffers, so those
      ;; get killed rather than left around without a tab.
      (if (or kill
              (buffer-base-buffer buffer-to-drop)
              (not (buffer-file-name buffer-to-drop)))
          (kill-buffer buffer-to-drop)
        (setq intuitive-tab-line--current-tab-list
              (delq buffer-to-drop intuitive-tab-line--current-tab-list)))
      (force-mode-line-update))))

(defun intuitive-tab-line--swap-tabs (i j)
  "Swap the tabs at positions I and J in the tab list."
  (let ((tabs intuitive-tab-line--current-tab-list))
    (cl-rotatef (nth i tabs) (nth j tabs))))

(defun intuitive-tab-line-shift-tab-left ()
  "Shift the current tab one spot to the left."
  (interactive)
  (let ((n (seq-position intuitive-tab-line--current-tab-list (current-buffer))))
    (when (and n (> n 0))
      (intuitive-tab-line--swap-tabs n (1- n))))
  (force-mode-line-update))

(defun intuitive-tab-line-shift-tab-right ()
  "Shift the current tab one spot to the right."
  (interactive)
  (let ((n (seq-position intuitive-tab-line--current-tab-list (current-buffer))))
    (when (and n (< n (1- (length intuitive-tab-line--current-tab-list))))
      (intuitive-tab-line--swap-tabs n (1+ n))))
  (force-mode-line-update))

(defun intuitive-tab-line-set-default-tab ()
  "Set the current buffer as `intuitive-tab-line--default-tab'."
  (interactive)
  (setq intuitive-tab-line--default-tab (current-buffer)))

(defun intuitive-tab-line-goto-default-tab ()
  "Go to `intuitive-tab-line--default-tab'."
  (interactive)
  (if (buffer-live-p intuitive-tab-line--default-tab)
      (switch-to-buffer intuitive-tab-line--default-tab)
    (message "No default tab has been set")))

(defun intuitive-tab-line-close-all-right-tabs ()
  "Close all tabs to the right of the current tab.
The buffers themselves are left open."
  (interactive)
  (let ((n (seq-position intuitive-tab-line--current-tab-list (current-buffer))))
    (if (null n)
        (message "Current buffer has no tab")
      (setq intuitive-tab-line--current-tab-list
            (seq-take intuitive-tab-line--current-tab-list (1+ n)))
      (force-mode-line-update))))

(defun intuitive-tab-line-load-initial-buffer-only ()
  "Load `initial-buffer-choice' as the only tab."
  (interactive)
  (if initial-buffer-choice
      (progn
        (cond ((stringp initial-buffer-choice)
               (find-file initial-buffer-choice))
              ((functionp initial-buffer-choice)
               (funcall initial-buffer-choice)))
        (setq intuitive-tab-line--current-tab-list (list (current-buffer)))
        (force-mode-line-update))
    (message "initial-buffer-choice is not set.")))

(defun intuitive-tab-line-store-tabs (&rest _args)
  "Store the current tab list in a temporary variable.
If there are functions that create new tabs where you would rather they
did not, add this as before-advice to those functions, and add
`intuitive-tab-line-restore-tabs' as after-advice."
  (interactive)
  (setq intuitive-tab-line--stored-tab-list intuitive-tab-line--current-tab-list))

(defun intuitive-tab-line-restore-tabs (&rest _args)
  "Restore the tab list saved by `intuitive-tab-line-store-tabs'."
  (interactive)
  (setq intuitive-tab-line--current-tab-list intuitive-tab-line--stored-tab-list)
  (intuitive-tab-line--prune)
  (force-mode-line-update))

(add-hook 'buffer-list-update-hook #'intuitive-tab-line-add-current-buffer-to-tab)
(add-hook 'kill-buffer-hook #'intuitive-tab-line--forget-buffer)

(provide 'intuitive-tab-line)

;;; intuitive-tab-line.el ends here
