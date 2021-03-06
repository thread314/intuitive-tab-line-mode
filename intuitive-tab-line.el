
(global-tab-line-mode)

(setq tab-line-tabs-function 'tab-line-tabs-mode-buffers) 

(defun tab-line-tabs-mode-buffers ()
  "A simple list containing all user-created buffers to display as tabs"
  my/current-tab-list)

(defun my/manually-add-current-buffer-to-tab ()
  "Create a tab for the current buffer"
  (interactive)
  (setq my/current-tab-list (append my/current-tab-list (list (current-buffer))))
  (setq my/current-tab-list (seq-remove (lambda (elt) (not (buffer-name elt))) my/current-tab-list))
  (force-mode-line-update))

(defun my/add-current-buffer-to-tab (&rest _args)
  "Create a tab for the current buffer"
  (interactive)
  (if
      (and 
       (not (seq-contains-p my/current-tab-list (current-buffer))) ;;Exclude if already has a tab 
       (not (string-match (rx "magit") (buffer-name (current-buffer)))) ;;Exclude magit
       (not (string-match (rx "COMMIT_EDITMSG") (buffer-name (current-buffer)))) ;;Exclude magit and edit msg
       (not (string-match (rx "CAPTURE-") (buffer-name (current-buffer)))) ;;Exclude capture buffer
       (not (string-match (rx "*org-roam*") (buffer-name (current-buffer)))) ;;Exclude org-roam buffer
       (or
        ;;Define any buffers you would always like to be given their own tab here.
        (buffer-file-name (current-buffer)) ;;Include all file buffers 
        (buffer-local-value 'list-buffers-directory (current-buffer)) ;;Include Dired Buffers
        (string-match (rx "*help") (buffer-name (current-buffer))) ;;Include Help Buffers
        (buffer-base-buffer (current-buffer)) ;;Include Indirect Buffers
	))
      (setq my/current-tab-list (append my/current-tab-list (list (current-buffer)))))
  (setq my/current-tab-list (seq-remove (lambda (elt) (not (buffer-name elt))) my/current-tab-list))
  (force-mode-line-update))

(setq my/current-tab-list (list (current-buffer)))

(defun my/switch-before-drop-kill ()
  "Switch to another tab, before dropping/killing current buffer (to prevent backgrounded buffers unexpectedly returning to my/current-tab-list)."
  (let ((n (seq-position my/current-tab-list (current-buffer))))
    (cond
     ((= (length my/current-tab-list) 1)
      ;;If only one tab, return error
      (message "Only one tab open, cannot drop")) 
     ;;If left most tab, switch right
     ((= n 0)
      (switch-to-buffer (nth 1 my/current-tab-list))) 
     ;;otherwise switch left
     (t
      (switch-to-buffer (nth (- n 1) my/current-tab-list))))))

(defun my/drop-tab (&optional kill)
  "Remove the tab for the current buffer. Will kill indirect buffers, but leave all others open"
  (interactive)
  (let ((buffer-to-drop (current-buffer)))  
    (my/switch-before-drop-kill)
    ;;if buffer is indirect, dired, help or kill is non-nil, kill-this-buffer, otherwise remove from tab-list (keeping buffer open)
    (if (or kill
            (buffer-base-buffer buffer-to-drop)
            ;;buffer-file-name is blank for dired and help descriptions, so kill those buffers
            (not (buffer-file-name buffer-to-drop)))
        (kill-buffer buffer-to-drop)
      (setq my/current-tab-list (delete buffer-to-drop my/current-tab-list))))
  (force-mode-line-update))

(defun my/shift-tab-left ()
  "Shift the current tab one spot to the left"
  (interactive)
  (let ((n (seq-position my/current-tab-list (current-buffer))))
    (when
        (> n 0)
      (progn 
        (setq my/current-tab-list
              (append
               (seq-take my/current-tab-list (- n 1))
               (list (elt my/current-tab-list n))
               (list (elt my/current-tab-list (- n 1)))
               (seq-drop my/current-tab-list (+ n 1)))))))
  (force-mode-line-update))

(defun my/shift-tab-right ()
  "Shift the current tab one spot to the right"
  (interactive)
  (let ((n (seq-position my/current-tab-list (current-buffer))))
    (when
        (< n (- (length my/current-tab-list) 1))
      (progn 
        (setq my/current-tab-list
              (append
               (seq-take my/current-tab-list n)
               (list (elt my/current-tab-list (+ n 1)))
               (list (elt my/current-tab-list n))
               (seq-drop my/current-tab-list (+ n 2)))))))
  (force-mode-line-update))

(defun my/set-default-tab ()
  "Set current tab as my/default-tab"
  (interactive)
  (setq my/default-tab (current-buffer)))

(defun my/goto-default-tab ()
  "Go to my/default-tab"
  (interactive)
  (if my/default-tab
      (switch-to-buffer my/default-tab)
    (message "No default tab has been set")))

(defun my/load-initial-buffer-only ()
  "Load initial-buffer-choice as the only tab."
  (interactive)
  (if initial-buffer-choice
      (progn
	;;(dolist (buf (buffer-list))
	  ;;(my/close-if-indirect))
	(cond ((stringp initial-buffer-choice)
               (find-file initial-buffer-choice))
              ((functionp initial-buffer-choice)
               (funcall initial-buffer-choice)))
  (setq my/current-tab-list (list (current-buffer)))
  (force-mode-line-update))
    (message "initial-buffer-choice is not set."))) 

  ;; (advice-add 'quit-window :before #'my/drop-tab)

(add-hook 'buffer-list-update-hook #'my/add-current-buffer-to-tab)

