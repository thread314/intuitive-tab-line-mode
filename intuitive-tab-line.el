;;; intuitive-tab-line-mode --- a better tab-line behavior

;;; Commentary:

;;; Code:

(require 'tab-line)

(defun intuitive-tab-line-buffers-list ()
  "A simple list containing all user-created buffers to display as tabs."
  intuitive-tab-line--current-tab-list)

(defun intuitive-tab-line-manually-add-current-buffer-to-tab ()
  "Create a tab for the current buffer."
  (interactive)
  (setq intuitive-tab-line--current-tab-list (append intuitive-tab-line--current-tab-list (list (current-buffer))))
  (setq intuitive-tab-line--current-tab-list (seq-remove (lambda (elt) (not (buffer-name elt))) intuitive-tab-line--current-tab-list))
  (force-mode-line-update))

(defun intuitive-tab-line-add-current-buffer-to-tab (&rest _args)
  "Create a tab for the current buffer."
  (interactive)
  (if
      (and
       (not (seq-contains-p intuitive-tab-line--current-tab-list (current-buffer))) ;;Exclude if already has a tab
       (not (string-match (rx "magit") (buffer-name (current-buffer)))) ;;Exclude magit
       (not (string-match (rx "COMMIT_EDITMSG") (buffer-name (current-buffer)))) ;;Exclude magit and edit msg
       (not (string-match (rx "CAPTURE-") (buffer-name (current-buffer)))) ;;Exclude capture buffer
       (not (string-match (rx "*org-roam*") (buffer-name (current-buffer)))) ;;Exclude org-roam buffer
       (not (string-match (rx "*NeoTree*") (buffer-name (current-buffer)))) ;;Exclude org-roam buffer
       (or
        ;;Define any buffers you would always like to be given their own tab here.
        (buffer-file-name (current-buffer)) ;;Include all file buffers
        (buffer-local-value 'list-buffers-directory (current-buffer)) ;;Include Dired Buffers
        (string-match (rx "*help") (buffer-name (current-buffer))) ;;Include Help Buffers
        (buffer-base-buffer (current-buffer)) ;;Include Indirect Buffers
	    ))
      (setq intuitive-tab-line--current-tab-list (append intuitive-tab-line--current-tab-list (list (current-buffer)))))
  (setq intuitive-tab-line--current-tab-list (seq-remove (lambda (elt) (not (buffer-name elt))) intuitive-tab-line--current-tab-list))
  (force-mode-line-update))

(setq intuitive-tab-line--current-tab-list (list (current-buffer)))

(defun intuitive-tab-line-switch-before-drop-kill ()
  "Switch to another tab, before dropping/killing current buffer (to prevent backgrounded buffers unexpectedly returning to intuitive-tab-line--current-tab-list)."
  (let ((n (seq-position intuitive-tab-line--current-tab-list (current-buffer))))
    (cond
     ((= (length intuitive-tab-line--current-tab-list) 1)
      ;;If only one tab, return error
      (message "Only one tab open, cannot drop"))
     ;;If left most tab, switch right
     ((= n 0)
      (switch-to-buffer (nth 1 intuitive-tab-line--current-tab-list)))
     ;;otherwise switch left
     (t
      (switch-to-buffer (nth (- n 1) intuitive-tab-line--current-tab-list))))))

(defun intuitive-tab-line-drop-tab (&optional kill)
  "Remove the tab for the current buffer. Will KILL indirect buffers, but leave all others open."
  (interactive)
  (let ((buffer-to-drop (current-buffer)))
    (intuitive-tab-line-switch-before-drop-kill)
    ;;if buffer is indirect, dired, help or kill is non-nil, kill-this-buffer, otherwise remove from tab-list (keeping buffer open)
    (if (or kill
            (buffer-base-buffer buffer-to-drop)
            ;;buffer-file-name is blank for dired and help descriptions, so kill those buffers
            (not (buffer-file-name buffer-to-drop)))
        (kill-buffer buffer-to-drop)
      (setq intuitive-tab-line--current-tab-list (delete buffer-to-drop intuitive-tab-line--current-tab-list))))
  (force-mode-line-update))

(defun intuitive-tab-line-shift-tab-left ()
  "Shift the current tab one spot to the left."
  (interactive)
  (let ((n (seq-position intuitive-tab-line--current-tab-list (current-buffer))))
    (when
        (> n 0)
      (progn
        (setq intuitive-tab-line--current-tab-list
              (append
               (seq-take intuitive-tab-line--current-tab-list (- n 1))
               (list (elt intuitive-tab-line--current-tab-list n))
               (list (elt intuitive-tab-line--current-tab-list (- n 1)))
               (seq-drop intuitive-tab-line--current-tab-list (+ n 1)))))))
  (force-mode-line-update))

(defun intuitive-tab-line-shift-tab-right ()
  "Shift the current tab one spot to the right."
  (interactive)
  (let ((n (seq-position intuitive-tab-line--current-tab-list (current-buffer))))
    (when
        (< n (- (length intuitive-tab-line--current-tab-list) 1))
      (progn
        (setq intuitive-tab-line--current-tab-list
              (append
               (seq-take intuitive-tab-line--current-tab-list n)
               (list (elt intuitive-tab-line--current-tab-list (+ n 1)))
               (list (elt intuitive-tab-line--current-tab-list n))
               (seq-drop intuitive-tab-line--current-tab-list (+ n 2)))))))
  (force-mode-line-update))

(defun intuitive-tab-line-set-default-tab ()
  "Set current tab as `intuitive-tab-line--default-tab'."
  (interactive)
  (setq intuitive-tab-line--default-tab (current-buffer)))

(defun intuitive-tab-line-goto-default-tab ()
  "Go to `intuitive-tab-line--default-tab'."
  (interactive)
  (if intuitive-tab-line--default-tab
      (switch-to-buffer intuitive-tab-line--default-tab)
    (message "No default tab has been set")))

(defun intuitive-tab-line-close-all-right-tabs ()
  "Close all tabs to the right of the current tab."
  (interactive)
  (let
      ((position
        (- (safe-length intuitive-tab-line--current-tab-list) (cl-position (current-buffer) intuitive-tab-line--current-tab-list) 1)))
    (setq intuitive-tab-line--current-tab-list (butlast intuitive-tab-line--current-tab-list position))
    (force-mode-line-update)))

(defun intuitive-tab-line-load-initial-buffer-only ()
  "Load `initial-buffer-choice' as the only tab."
  (interactive)
  (if initial-buffer-choice
      (progn
	    ;;(dolist (buf (buffer-list))
	    ;;(intuitive-tab-line-close-if-indirect))
	    (cond ((stringp initial-buffer-choice)
               (find-file initial-buffer-choice))
              ((functionp initial-buffer-choice)
               (funcall initial-buffer-choice)))
        (setq intuitive-tab-line--current-tab-list (list (current-buffer)))
        (force-mode-line-update))
    (message "initial-buffer-choice is not set.")))

(setq intuitive-tab-line--stored-tab-list nil)

(defun intuitive-tab-line-store-tabs (&rest scratch)
  "Store the current tab list in a temporary variable. If there are certain functions that create new tabs (where you'd rather this didn't happen), add this as before-advice to that function. Then add intuitive-tab-line-restore-tabs as after-advice to the function."
  (interactive)
  (setq intuitive-tab-line--stored-tab-list intuitive-tab-line--current-tab-list))

(defun intuitive-tab-line-restore-tabs (&rest scratch)
  (interactive)
  (setq intuitive-tab-line--current-tab-list intuitive-tab-line--stored-tab-list))

(add-hook 'buffer-list-update-hook #'intuitive-tab-line-add-current-buffer-to-tab)

(provide 'intuitive-tab-line)

;;; intuitive-tab-line.el ends here

