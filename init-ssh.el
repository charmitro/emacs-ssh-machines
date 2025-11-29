;;; init-ssh.el --- SSH Machines Management  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(defgroup ssh-machines nil
  "SSH machines management."
  :group 'tools
  :prefix "ssh-")

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
  (unless (multisession-value ssh-machines-list)
    (user-error "No SSH machines configured"))
  (let* ((machine-names (mapcar (lambda (machine) (car machine)) (multisession-value ssh-machines-list)))
	 (selected-name (completing-read "Select SSH machine to remove: " machine-names)))
    (when selected-name
      (let* ((current-list (multisession-value ssh-machines-list))
	     (filtered-list (cl-remove-if (lambda (machine)
					    (string= selected-name (car machine)))
					  current-list)))
	(setf (multisession-value ssh-machines-list) filtered-list)
	(message "Removed %s from SSH machines list" selected-name)))))

(defun ssh-connect ()
  "Connect to a machine via SSH, using associated key if available."
  (interactive)
  (unless (multisession-value ssh-machines-list)
    (user-error "No SSH machines configured"))
  (let* ((machine-names (mapcar #'car (multisession-value ssh-machines-list)))
	 (selected-name (completing-read "Select machine: " machine-names))
	 (machine-info (assoc selected-name (multisession-value ssh-machines-list))))
    (when machine-info
      (pcase-let ((`(,_ ,address ,_ . ,rest) machine-info))
	(let ((key-option (if (car rest)
			      (format " -i %s" (shell-quote-argument
						(expand-file-name (car rest) ssh-keys-directory)))
			    "")))
	  (ansi-term (concat "ssh" key-option " " address)))))))

(defvar ssh-machines-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'ssh-machines-connect-at-point)
    (define-key map (kbd "d") #'ssh-machines-delete-at-point)
    (define-key map (kbd "e") #'ssh-machines-edit-at-point)
    (define-key map (kbd "g") #'list-ssh-machines)
    map)
  "Keymap for `ssh-machines-mode'.")

;; Ensure keybindings are updated when reloading
(define-key ssh-machines-mode-map (kbd "e") #'ssh-machines-edit-at-point)

(define-derived-mode ssh-machines-mode tabulated-list-mode "SSH-Machines"
  "Major mode for listing and managing SSH machines.
\\{ssh-machines-mode-map}"
  :keymap ssh-machines-mode-map
  (setq tabulated-list-format [("Name" 15 t)
                               ("Address" 30 t)
                               ("Description" 25 t)
                               ("Key" 15 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun ssh-machines--get-entries ()
  "Return entries for `tabulated-list-entries'."
  (mapcar (lambda (machine)
            (pcase-let ((`(,name ,address ,desc . ,rest) machine))
              (list name (vector name
                                 address
                                 (or desc "")
                                 (or (car rest) "")))))
          (multisession-value ssh-machines-list)))

(defun ssh-machines-connect-at-point ()
  "Connect to the SSH machine at point."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (when name
      (let ((machine-info (assoc name (multisession-value ssh-machines-list))))
        (when machine-info
          (pcase-let ((`(,_ ,address ,_ . ,rest) machine-info))
            (let ((key-option (if (car rest)
                                  (format " -i %s" (shell-quote-argument
                                                    (expand-file-name (car rest) ssh-keys-directory)))
                                "")))
              (ansi-term (concat "ssh" key-option " " address)))))))))

(defun ssh-machines-delete-at-point ()
  "Delete the SSH machine at point."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (when (and name (yes-or-no-p (format "Delete machine '%s'? " name)))
      (let ((filtered-list (cl-remove-if (lambda (machine)
                                           (string= name (car machine)))
                                         (multisession-value ssh-machines-list))))
        (setf (multisession-value ssh-machines-list) filtered-list)
        (message "Deleted %s" name)
        (list-ssh-machines)))))

(defun ssh-machines-edit-at-point ()
  "Edit the SSH machine at point."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (when name
      (let ((machine-info (assoc name (multisession-value ssh-machines-list))))
        (when machine-info
          (pcase-let ((`(,old-name ,old-address ,old-desc . ,rest) machine-info))
            (let* ((new-name (read-string "Name: " old-name))
                   (new-address (read-string "Address: " old-address))
                   (new-desc (read-string "Description: " old-desc))
                   (key-file (car rest))
                   (updated-machine (if key-file
                                        (list new-name new-address new-desc key-file)
                                      (list new-name new-address new-desc)))
                   (updated-list (mapcar (lambda (m)
                                           (if (string= (car m) old-name)
                                               updated-machine
                                             m))
                                         (multisession-value ssh-machines-list))))
              (setf (multisession-value ssh-machines-list) updated-list)
              (message "Updated %s" new-name)
              (list-ssh-machines))))))))

(defun edit-ssh-machine ()
  "Edit an SSH machine by selecting from the list."
  (interactive)
  (unless (multisession-value ssh-machines-list)
    (user-error "No SSH machines configured"))
  (let* ((machine-names (mapcar #'car (multisession-value ssh-machines-list)))
         (selected-name (completing-read "Select machine to edit: " machine-names nil t))
         (machine-info (assoc selected-name (multisession-value ssh-machines-list))))
    (when machine-info
      (pcase-let ((`(,old-name ,old-address ,old-desc . ,rest) machine-info))
        (let* ((new-name (read-string "Name: " old-name))
               (new-address (read-string "Address: " old-address))
               (new-desc (read-string "Description: " old-desc))
               (key-file (car rest))
               (updated-machine (if key-file
                                    (list new-name new-address new-desc key-file)
                                  (list new-name new-address new-desc)))
               (updated-list (mapcar (lambda (m)
                                       (if (string= (car m) old-name)
                                           updated-machine
                                         m))
                                     (multisession-value ssh-machines-list))))
          (setf (multisession-value ssh-machines-list) updated-list)
          (message "Updated %s" new-name))))))

(defun list-ssh-machines ()
  "List all SSH machines in an interactive buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*SSH Machines*")))
    (with-current-buffer buffer
      (ssh-machines-mode)
      (setq tabulated-list-entries #'ssh-machines--get-entries)
      (tabulated-list-print t))
    (switch-to-buffer buffer)))

(defun export-ssh-machines (file-path)
  "Export the list of SSH Machines to a specified FILE-PATH."
  (interactive "FExport to file: ")
  (with-temp-file file-path
    (prin1 (multisession-value ssh-machines-list) (current-buffer)))
  (message "Exported SSH machines to %s" file-path))

(defun import-ssh-machines (file-path)
  "Import a list of SSH machines from a specified FILE-PATH."
  (interactive "fImport from file: ")
  (with-temp-buffer
    (insert-file-contents file-path)
    (setf (multisession-value ssh-machines-list) (read (current-buffer))))
  (message "Imported SSH machines from %s" file-path))

(defun ssh-parse-config-file (config-file)
  "Parse CONFIG-FILE and return list of (name address desc key) entries.
Skips wildcard patterns like Host * or Host *.example.com."
  (let ((hosts '())
        (current-host nil)
        (current-hostname nil)
        (current-user nil)
        (current-key nil))
    (with-temp-buffer
      (insert-file-contents (expand-file-name config-file))
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ;; Match Host directive (start of new stanza)
         ((looking-at "^[[:space:]]*Host[[:space:]]+\\([^#\n]+\\)")
          ;; Save previous host if valid
          (when (and current-host (not (string-match-p "[*?]" current-host)))
            (let* ((addr (or current-hostname current-host))
                   (address (if current-user (format "%s@%s" current-user addr) addr))
                   (key-name (when current-key
                               (file-name-nondirectory current-key))))
              (push (list current-host address "Imported from SSH config" key-name) hosts)))
          ;; Start new host
          (setq current-host (string-trim (match-string 1))
                current-hostname nil
                current-user nil
                current-key nil))
         ;; Match HostName
         ((looking-at "^[[:space:]]+HostName[[:space:]]+\\([^#\n]+\\)")
          (setq current-hostname (string-trim (match-string 1))))
         ;; Match User
         ((looking-at "^[[:space:]]+User[[:space:]]+\\([^#\n]+\\)")
          (setq current-user (string-trim (match-string 1))))
         ;; Match IdentityFile (use first one only)
         ((and (not current-key)
               (looking-at "^[[:space:]]+IdentityFile[[:space:]]+\\([^#\n]+\\)"))
          (setq current-key (string-trim (match-string 1)))))
        (forward-line 1))
      ;; Don't forget the last host
      (when (and current-host (not (string-match-p "[*?]" current-host)))
        (let* ((addr (or current-hostname current-host))
               (address (if current-user (format "%s@%s" current-user addr) addr))
               (key-name (when current-key
                           (file-name-nondirectory current-key))))
          (push (list current-host address "Imported from SSH config" key-name) hosts))))
    (nreverse hosts)))

(defun import-from-ssh-config ()
  "Import SSH hosts from ~/.ssh/config into the machines list.
Skips wildcard patterns and duplicates (hosts already in the list)."
  (interactive)
  (let* ((config-file "~/.ssh/config")
         (parsed-hosts (ssh-parse-config-file config-file))
         (existing-names (mapcar #'car (multisession-value ssh-machines-list)))
         (imported 0)
         (duplicates 0))
    (dolist (host parsed-hosts)
      (if (member (car host) existing-names)
          (cl-incf duplicates)
        (setf (multisession-value ssh-machines-list)
              (append (multisession-value ssh-machines-list) (list host)))
        (cl-incf imported)))
    (message "Imported %d host%s from %s%s"
             imported
             (if (= imported 1) "" "s")
             config-file
             (if (> duplicates 0)
                 (format " (skipped %d duplicate%s)"
                         duplicates
                         (if (= duplicates 1) "" "s"))
               ""))))

(defun copy-file-to-ssh-machine (file-path)
  "Copy a file (FILE-PATH) from the host machine to a selected remote SSH machine.
The user will be prompted to select the target machine and specify the remote
destination path."
  (interactive "fFile to copy: ")
  (unless (multisession-value ssh-machines-list)
    (user-error "No SSH machines configured"))
  (let* ((machine-names (mapcar #'car (multisession-value ssh-machines-list)))
	 (selected-name (completing-read "Select target machine: " machine-names))
	 (machine-info (assoc selected-name (multisession-value ssh-machines-list))))
    (when machine-info
      (pcase-let ((`(,_ ,address ,_) machine-info))
	(let ((remote-path (read-string "Remote destination path: ")))
	  (cl-case ssh-copy-method
	    (scp
	     (shell-command (format "scp %s %s:%s"
				    (shell-quote-argument file-path)
				    address
				    (shell-quote-argument remote-path)))
	     (message "File %s copied to %s:%s" file-path address remote-path))
	    (rsync
	     (shell-command (format "rsync %s %s:%s"
				    (shell-quote-argument file-path)
				    address
				    (shell-quote-argument remote-path)))
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
      (let ((command (format "ssh-keygen -t %s%s -f %s -C %s -N \"\""
			     key-type
			     (if (string= key-type "rsa") (format " -b %s" bits) "")
			     (shell-quote-argument key-path)
			     (shell-quote-argument key-comment))))
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
  (unless (multisession-value ssh-machines-list)
    (user-error "No SSH machines configured"))
  (let* ((machine-names (mapcar #'car (multisession-value ssh-machines-list)))
	 (selected-name (completing-read "Select target machine: " machine-names))
	 (machine-info (assoc selected-name (multisession-value ssh-machines-list))))
    (when machine-info
      (pcase-let ((`(,_ ,address ,_) machine-info))
	(let ((key-path (expand-file-name (concat key-file ".pub") ssh-keys-directory)))
	  (if (file-exists-p key-path)
	      (async-shell-command
	       (format "ssh-copy-id -i %s %s" (shell-quote-argument key-path) address)
	       "*SSH Copy ID*")
	    (user-error "Public key file %s does not exist" key-path)))))))

(defun ssh-associate-key-with-machine ()
  "Associate an SSH key with a specific machine in the list."
  (interactive)
  (unless (multisession-value ssh-machines-list)
    (user-error "No SSH machines configured"))
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
