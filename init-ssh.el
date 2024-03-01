;;; init-ssh.el --- SSH Machines Managment  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar ssh-machines-list
  '()
  "List of SSH machines. Format: (NAME ADDRESS NOTES).")

(defun add-ssh-machine (name address notes)
  "Add a new SSH machine to the list in the format of NAME, ADDRESS, NOTES."
  (interactive "sName: \nsAddress: \nsNotes: ")
  (add-to-list 'ssh-machines-list (list name address notes))
  (with-temp-file "~/.emacs.d/ssh-machines.el"
    (insert "(setq ssh-machines-list '")
    (prin1 ssh-machines-list (current-buffer))
    (insert ")"))
  (message "Added %s to SSH machines list" name))

(defun remove-ssh-machine ()
  "Remove an new SSH machine from the list."
  (interactive)
  (let ((machine-name (completing-read "Remove machine: " (mapcar 'car ssh-machines-list))))
    (setq ssh-machines-list (remove-if (lambda (machine) (string= (car machine) machine-name)) ssh-machines-list))
    (with-temp-file "~/.emacs.d/ssh-machines.el"
      (insert "(setq ssh-machines-list '")
      (prin1 ssh-machine-list (current-buffer))
      (insert ")"))
    (message "Removed %s from SSH machines list" machine-name)))

(defun ssh-connect ()
  "Connect to a machine via SSH."
  (interactive)
  (let* ((machine-names (mapcar #'car ssh-machines-list))
	 (selected-name (completing-read "Select machine: " machine-names))
	 (machine-info (assoc selected-name ssh-machines-list)))
    (when machine-info
      (let ((address (nth 1 machine-info)))
	(ansi-term (concat "ssh " address))))))

(defun list-ssh-machines ()
  "List all SSH machines in a new buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "SSH Machines*")
    (erase-buffer)
    (dolist (machine ssh-machines-list)
      (insert (format "%s - %s: %s\n" (car machine) (nth 2 machine) (nth 1 machine))))
    (switch-to-buffer (current-buffer))))

(when (file-exists-p "~/.emacs.d/ssh-machines.el")
  (load "~/.emacs.d/ssh-machines.el"))

(provide 'init-ssh)

;;; init-ssh.el ends here.
