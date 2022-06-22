
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
       (not (seq-contains-p my/current-tab-list (current-buffer)))
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

(defun my/drop-tab ()
  "Remove the tab for the current buffer. Will kill indirect buffers, but leave all others open"
  (interactive)
  (let ((n (seq-position my/current-tab-list (current-buffer)))
        (buffer-to-drop (current-buffer)))  
    (my/close-if-indirect)
    (if (= n 0)
        (switch-to-buffer (nth 1 my/current-tab-list))
      (switch-to-buffer (nth (- n 1) my/current-tab-list)))
    (setq my/current-tab-list (delete buffer-to-drop my/current-tab-list)))
  (force-mode-line-update))

(defun my/close-if-indirect ()
  "Kill the current buffer if it is indirect"
  (if (or (buffer-base-buffer)
          ;;buffer-file-name is blank for dired and help descriptions, so kill those buffers
          (not (buffer-file-name (current-buffer))))
      (kill-buffer)))

(add-hook 'buffer-list-update-hook #'my/add-current-buffer-to-tab)

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
	(dolist (buf (buffer-list))
	  (my/close-if-indirect))
	(cond ((stringp initial-buffer-choice)
               (find-file initial-buffer-choice))
              ((functionp initial-buffer-choice)
               (funcall initial-buffer-choice)))
  (setq my/current-tab-list (list (current-buffer)))
  (force-mode-line-update))
    (message "initial-buffer-choice is not set."))) 

