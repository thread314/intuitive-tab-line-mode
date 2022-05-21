  (global-tab-line-mode)

  (setq tab-line-tabs-function 'tab-line-tabs-mode-buffers) 

  (defun tab-line-tabs-mode-buffers ()
    "A simple list containing all user-created buffers to display as tabs"
    my/current-tab-list)

  (defun my/add-current-buffer-to-tab (&rest _args)
    "Create a tab for the current buffer"
    (interactive)
    (if (not (seq-contains-p my/current-tab-list (current-buffer)))
        (setq my/current-tab-list (append my/current-tab-list (list (current-buffer)))))
    (setq my/current-tab-list (seq-remove (lambda (elt) (not (buffer-name elt))) my/current-tab-list))
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

  (defun my/find-file (file)
    "Call find-file and create a tab for the new buffer"
    (interactive)
    (find-file file)
    (my/add-current-buffer-to-tab))

  (defun my/find-file-no-args ()
    "Call find-file with no arguments and create a tab for the new buffer"
    (interactive)
    (if buffer-file-name
        (ido-find-file-in-dir (file-name-directory buffer-file-name))
      (ido-find-file-in-dir "~/"))
    (my/add-current-buffer-to-tab))

  (defun my/drop-tab ()
    "Remove the tab for the current buffer. Will kill indirect buffers, but leave all others open"
    (interactive)
    (let ((n (seq-position my/current-tab-list (current-buffer))))  
      (setq my/current-tab-list (delete (current-buffer) my/current-tab-list))
      (my/close-if-indirect)
      (switch-to-buffer (nth (- n 1) my/current-tab-list)))
    (force-mode-line-update))

  (defun my/kill-buffer ()
    "Remove the tab for the current buffer and kill this buffer"
    (interactive)
    (setq my/current-tab-list (delete (current-buffer) my/current-tab-list))
    (kill-buffer)
    (force-mode-line-update))

  (defun my/close-if-indirect ()
    "Kill the current buffer if it is indirect"
    (if (or (buffer-base-buffer)
            ;;buffer-file-name is blank for dired and help descriptions, so kill those buffers
            (not (buffer-file-name (current-buffer))))
        (kill-buffer)))

  (defun my/load-initial-buffer-only ()
    "Load initial-buffer-choice as the only tab."
    (interactive)
    (dolist (buf  (buffer-list))
      (my/close-if-indirect))
    (find-file initial-buffer-choice)
    (setq my/current-tab-list (list (current-buffer)))
    (force-mode-line-update)) 

  (defun my/set-default-tab ()
    "Set current tab as my/default-tab"
    (interactive)
    (setq my/default-tab (current-buffer)))

  (defun my/goto-default-tab ()
    "Go to my/default-tab"
    (interactive)
    (switch-to-buffer my/default-tab))

  (advice-add 'dired-find-file :after #'my/add-current-buffer-to-tab)
  (advice-add 'dired-find-alternate-file :after #'my/add-current-buffer-to-tab)
  (advice-add 'helpful-update :after #'my/add-current-buffer-to-tab)
  (advice-add 'find-file-read :after #'my/add-current-buffer-to-tab)
  (add-hook 'dired-mode-hook 'my/add-current-buffer-to-tab)
  (advice-add 'org-roam-node-find :after #'my/add-current-buffer-to-tab)

  (setq my/current-tab-list (list (current-buffer)))
  (my/load-initial-buffer-only)

  ;;Set org-open-at-point to open files in same window
  (setcdr (assoc 'file org-link-frame-setup) 'find-file)
  (advice-add 'org-open-at-point :after #'my/add-current-buffer-to-tab)
