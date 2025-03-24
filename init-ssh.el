;;; init-ssh.el --- SSH Machines Managment  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-seq)

(defvar ssh-machines-list
  (multisession-make '())
  "A list of SSH machines to connect to.
Each element is a list (NAME ADDRESS DESCRIPTION [KEY-FILE]).
NAME is the name of the machine.
ADDRESS is the SSH address of the machine.
DESCRIPTION is a short description of the machine.
Optional KEY-FILE is the filename of an SSH key to use.")

(defcustom ssh-keys-directory "~/.ssh/"
  "Directory where SSH keys are stored."
  :type 'directory
  :group 'ssh-machines)

(defcustom ssh-copy-method 'scp
  "Method to use for copying files to remote machines.
Possible values are `scp' or `rsync'."
  :type '(choice (const scp) (const rsync))
  :group 'ssh-machines)

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
  "Connect to a machine via SSH, using associated key if available."
  (interactive)
  (let* ((machine-names (mapcar #'car (multisession-value ssh-machines-list)))
	 (selected-name (completing-read "Select machine: " machine-names))
	 (machine-info (assoc selected-name (multisession-value ssh-machines-list))))
    (when machine-info
      (pcase-let ((`(,_ ,address ,_ . ,rest) machine-info))
	(let ((key-option (if (car rest)
			      (format " -i %s" (expand-file-name (car rest) ssh-keys-directory))
			    "")))
	  (ansi-term (concat "ssh" key-option " " address)))))))

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
  "Copy a file (FILE-PATH) from the host machine to a selected remote SSH machine.
The user will be prompted to select the target machine and specify the remote
destination path."
  (interactive "fFile to copy: ")
  (let* ((machine-names (mapcar #'car (multisession-value ssh-machines-list)))
	 (selected-name (completing-read "Select target machine: " machine-names))
	 (machine-info (assoc selected-name (multisession-value ssh-machines-list))))
    (when machine-info
      (pcase-let ((`(,_,address ,_) machine-info))
	(let ((remote-path (read-string "Remote destination path: ")))
	  (cl-case ssh-copy-method
	    (scp
	     (shell-command (format "scp %s %s:%s" file-path address remote-path))
	     (message "File %s copied to %s:%s" file-path address remote-path))
	    (rsync
	     (shell-command (format "rsync %s %s:%s" file-path address remote-path))
	     (message "File %s copied to %s:%s via rsync" file-path address remote-path))))))))

(defun ssh-list-keys ()
  "List all SSH keys in the SSH keys directory."
  (interactive)
  (let ((keys-buffer (get-buffer-create "*SSH Keys*"))
	(key-files (directory-files ssh-keys-directory nil "^id_.*$")))
    (with-current-buffer keys-buffer
      (erase-buffer)
      (if key-files
	  (progn
	    (insert "Available SSH Keys:\n\n")
	    (dolist (key-file key-files)
	      (unless (string-match-p "\\.pub$" key-file)
		(let ((pub-file (concat key-file ".pub")))
		  (insert (format "• %s" key-file))
		  (when (file-exists-p (expand-file-name pub-file ssh-keys-directory))
		    (insert " (with public key)")
		    (let ((key-comment (ssh-get-key-comment pub-file)))
		      (when key-comment
			(insert (format " - %s" key-comment)))))
		  (insert "\n")))))
	(insert "No SSH keys found in " ssh-keys-directory "\n"))
      (insert "\nUse M-x ssh-generate-key to create a new key pair.\n")
      (insert "Use M-x ssh-copy-key to copy a key to a remote server.\n")
      (special-mode)
      (switch-to-buffer (current-buffer)))))

(defun ssh-get-key-comment (pub-key-file)
  "Extract comment from SSH public key file PUB-KEY-FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name pub-key-file ssh-keys-directory))
    (when (re-search-forward "\\([^ ]+\\)$" nil t)
      (match-string 1))))

(defun ssh-generate-key (key-type key-name key-comment)
  "Generate a new SSH key pair.
KEY-TYPE is the type of key (e.g., 'rsa', 'ed25519').
KEY-NAME is the filename for the key.
KEY-COMMENT is typically your email address for identification."
  (interactive
   (list (completing-read "Key type: " '("rsa" "ed25519" "ecdsa" "dsa") nil t "ed25519")
	 (read-string "Key name (e.g., id_github): " "id_")
	 (read-string "Key comment (typically email): ")))
  (let ((key-path (expand-file-name key-name ssh-keys-directory))
	(bits (when (string= key-type "rsa")
		(read-string "Key bits (2048, 4096): " "4096"))))
    (make-directory ssh-keys-directory t)
    (if (file-exists-p key-path)
	(user-error "Key with name %s already exists" key-name)
      (let ((command (format "ssh-keygen -t %s%s -f %s -C \"%s\" -N \"\""
			     key-type
			     (if (string= key-type "rsa") (format " -b %s" bits) "")
			     key-path
			     key-comment)))
	(message "Generating key with command: %s" command)
	(async-shell-command command "*SSH Key Generation*")))))

(defun ssh-copy-key (key-file)
  "Copy an SSH public key (KEY-FILE) to a remote machine.
Use \='ssh-copy-id\=' internally."
  (interactive
   (list (completing-read "Select key to copy: "
			  (cl-remove-if
			   (lambda (file) (string-match-p "\\.pub$" file))
			   (directory-files ssh-keys-directory nil "^id_.*$"))
			  nil t)))
  (let* ((machine-names (mapcar #'car (multisession-value ssh-machines-list)))
	 (selected-name (completing-read "Select target machine: " machine-names))
	 (machine-info (assoc selected-name (multisession-value ssh-machines-list))))
    (when machine-info
      (pcase-let ((`(,_ ,address ,_) machine-info))
	(let ((key-path (expand-file-name (concat key-file ".pub") ssh-keys-directory)))
	  (if (file-exists-p key-path)
	      (async-shell-command
	       (format "ssh-copy-id -i %s %s" key-path address)
	       "*SSH Copy ID*")
	    (user-error "Public key file %s does not exist" key-path)))))))

(defun ssh-associate-key-with-machine ()
  "Associate an SSH key with a specific machine in the list."
  (interactive)
  (let* ((machine-names (mapcar #'car (multisession-value ssh-machines-list)))
	 (selected-name (completing-read "Select machine: " machine-names))
	 (machine-info (assoc selected-name (multisession-value ssh-machines-list)))
	 (key-files (cl-remove-if
		     (lambda (file) (string-match-p "\\.pub$" file))
		     (directory-files ssh-keys-directory nil "^id_.*$")))
	 (selected-key (completing-read "Select key to associate: " key-files)))
    (when (and machine-info selected-key)
      (let* ((current-list (multisession-value ssh-machines-list))
	     (updated-machine (list (car machine-info)
				    (cadr machine-info)
				    (caddr machine-info)
				    selected-key))
	     (updated-list (mapcar (lambda (m)
				     (if (string= (car m) selected-name)
					 updated-machine
				       m))
				   current-list)))
	(setf (multisession-value ssh-machines-list) updated-list)
	(message "Associated %s with machine %s" selected-key selected-name)))))

(provide 'init-ssh)

;;; init-ssh.el ends here.
