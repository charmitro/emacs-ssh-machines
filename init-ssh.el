;;; init-ssh.el --- SSH Machines Managment  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-seq)

(defcustom ssh-copy-method 'scp
  "Method to use for copying files to remote machines. Can be either 'scp or 'rsync."
  :type '(choice (const scp) (const rsync))
  :group 'ssh-machines)

(define-multisession-variable ssh-machines-list
  '()
  "List of SSH machines. Format: (NAME ADDRESS NOTES).")

(defun add-ssh-machine (name address notes)
  "Add a new SSH machine to the list in the format of (NAME, ADDRESS, NOTES)."
  (interactive "sName: \nsAddress: \nsNotes: ")
  (setf (multisession-value ssh-machines-list)
	(append (multisession-value ssh-machines-list) (list (list name address notes))))
  (message "Added %s to SSH machines list" name))

(defun remove-ssh-machine ()
  "Remove an SSH machine from the list by selecting from a prompted list."
  (interactive)
  (let* ((machine-names (mapcar (lambda (machine) (car machine)) (multisession-value ssh-machines-list)))
	 (selected-name (completing-read "Select SSH machine to remove: " machine-names)))
    (when selected-name
      (let* ((current-list (multisession-value ssh-machines-list))
	     (filtered-list (cl-remove-if (lambda (machine)
					    (string= selected-name (first machine)))
					  current-list)))
	(setf (multisession-value ssh-machines-list) filtered-list)
	(message "Removed %s from SSH machines list" selected-name)))))

(defun ssh-connect ()
  "Connect to a machine via SSH."
  (interactive)
  (let* ((machine-names (mapcar #'car (multisession-value ssh-machines-list)))
	 (selected-name (completing-read "Select machine: " machine-names))
	 (machine-info (assoc selected-name (multisession-value ssh-machines-list))))
    (when machine-info
      (pcase-let ((`(,_ ,address ,_) machine-info))
	(ansi-term (concat "ssh " address))))))

(defun list-ssh-machines ()
  "List all SSH machines in a new buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "SSH Machines*")
    (erase-buffer)
    (dolist (machine (multisession-value ssh-machines-list))
      (pcase-let ((`(,name ,address ,desc) machine))
	(insert (format "%s - %s: %s\n" name desc address))))
    (switch-to-buffer (current-buffer))))

(defun export-ssh-machines (file-path)
  "Export the list of SSH Machines to a specified FILE-PATH."
  (interactive "FExport to file: ")
  (with-temp-file file-path
    (prin1 `(setq ssh-machines-list ',(multisession-value ssh-machines-list)) (current-buffer))
    (message "Exported SSH machines to %s" file-path)))

(defun import-ssh-machines (file-path)
  "Import a list of SSH machines from a specified FILE-PATH."
  (interactive "fImport from file: ")
  (load-file file-path)
  (message "Imported SSH machines from %s" file-path))

(defun copy-file-to-ssh-machine (file-path)
  "Copy a file from the host machine to a selected remote SSH machine."
  (interactive "fFile to copy: ")
  (let* ((machine-names (mapcar #'car (multisession-value ssh-machines-list)))
	 (selected-name (completing-read "Select target machine: " machine-names))
	 (machine-info (assoc selected-name (multisession-value ssh-machines-list))))
    (when machine-info
      (pcase-let ((`(,_,address ,_) machine-info))
	(let ((remote-path (read-string "Remote destination path: ")))
	  (cl-case ssh-copy-method
	    (scp
	     (shell-command (format "scp %s %s:%s via scp" file-path address remote-path))
	     (message "File %s copied to %s:%s" file-path address remote-path))
	    (rsync
	     (shell-command (format "rsync %s %s:%s" file-path address remote-path))
	     (message "File %s copied to %s:%s via rsync" file-path address remote-path))))))))

(provide 'init-ssh)

;;; init-ssh.el ends here.
