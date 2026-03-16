;;; init-ssh.el --- SSH Machines Management  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'term)
(require 'multisession)

(defgroup ssh-machines nil
  "SSH machines management."
  :group 'tools
  :prefix "ssh-")

(cl-defstruct ssh-machine
  "A remote machine reachable via SSH.
  NAME is a display label.
  HOST is the hostname or IP.
  USER is an optional login username.
  DESCRIPTION is a free-form note.
  KEY is an optional private key filename relative to `ssh-keys-directory'."
  name
  host
  user
  description
  key)

(define-multisession-variable ssh-machines-list '()
  "A list of SSH machines to connect to.
Each element is an `ssh-machine` struct.
NAME is a display label.
HOST is the hostname or IP.
USER is an optional login username.
DESCRIPTION is a free-form note.
KEY is an optional private key filename relative to `ssh-keys-directory'.")

(defcustom ssh-keys-directory "~/.ssh/"
  "Directory where SSH keys are stored."
  :type 'directory
  :group 'ssh-machines)

(defcustom ssh-copy-method 'scp
  "Method to use for copying files to remote machines.
Possible values are `scp' or `rsync'."
  :type '(choice (const scp) (const rsync))
  :group 'ssh-machines)

(defun add-ssh-machine ()
  "Add a new SSH machine, ensuring unique name and host, and normalize optional fields."
  (interactive)
  (let* ((machines (multisession-value ssh-machines-list))
         (name (string-trim (read-string "Name: "))))
    (when (string-empty-p name)
      (user-error "Machine name cannot be empty"))
    (when (cl-find name machines :key #'ssh-machine-name :test #'string=)
      (user-error "A machine with name '%s' already exists" name))

    (let ((host (string-trim (read-string "Host: "))))
      (when (string-empty-p host)
        (user-error "Host cannot be empty"))
      (when (cl-find host machines :key #'ssh-machine-host :test #'string=)
        (user-error "A machine with host '%s' already exists" host))

      (let* ((user-input (string-trim (read-string "User (optional): ")))
             (user (unless (string-empty-p user-input) user-input))
             (desc (string-trim (read-string "Description: ")))
             (key (ssh-select-or-create-key))
             (machine (make-ssh-machine
                       :name name
                       :host host
                       :user user        ;; normalized: nil if empty
                       :description desc
                       :key key)))       ;; nil if not selected
	(push machine (multisession-value ssh-machines-list))
	(ssh-machines--refresh-buffer)
	(message "Added %s to SSH machines list" name)))))

(defun remove-ssh-machine ()
  "Remove a selected machine"
  (interactive)
  (unless (multisession-value ssh-machines-list)
    (user-error "No SSH machines configured"))
  (let* ((machines (multisession-value ssh-machines-list))
         (selected
          (completing-read
           "Select SSH machine to remove: "
           (mapcar #'ssh-machine-name machines) nil t)))
    (setf (multisession-value ssh-machines-list)
          (cl-remove-if
           (lambda (m)
             (string= selected (ssh-machine-name m)))
           machines))
    (ssh-machines--refresh-buffer)
    (message "Removed %s from SSH machines list" selected)))

(defun ssh-machine-full-address (machine)
  "Return full SSH address for MACHINE."
  (let ((user (ssh-machine-user machine))
        (host (ssh-machine-host machine)))
    (if (and user (not (string-empty-p user)))
        (format "%s@%s" user host)
      host)))

(defun ssh--split-address (address)
  "Split ADDRESS into (USER . HOST) on the first @ symbol.
Returns a cons cell (USER . HOST).
If ADDRESS contains no @, USER is nil and HOST is the full ADDRESS.
If ADDRESS is \"user@host@weird\", USER is \"user\" and HOST is \"host@weird\"."
  (if (string-match "^\\([^@]+\\)@\\(.*\\)$" address)
      (cons (match-string 1 address)
            (match-string 2 address))
    (cons nil address)))

(defun ssh-connect-machine (machine)
  "Connect to a machine"
  (let* ((address (ssh-machine-full-address machine))
         (key (ssh-machine-key machine))
         (key-option
          (when key
            (format " -i %s"
                    (shell-quote-argument
                     (expand-file-name key ssh-keys-directory))))))
    (ansi-term (concat "ssh" (or key-option "") " " address) (concat "ssh-term-" (ssh-machine-name machine)))))

(defun ssh-connect ()
  "Connect to a machine via SSH."
  (interactive)
  (unless (multisession-value ssh-machines-list)
    (user-error "No SSH machines configured"))

  (let* ((machines (multisession-value ssh-machines-list))
         (names (mapcar #'ssh-machine-name machines))
         (selected (completing-read "Select machine: " names))
         (machine (cl-find selected machines
                           :key #'ssh-machine-name
                           :test #'string=)))

    (when machine
      (ssh-connect-machine machine))))

(defun ssh-machines-connect-at-point ()
  "Connect to the SSH machine at point."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (if (not name)
	(user-error "No machine at point")
      (let ((machine (ssh-find-machine name)))
	(if (not machine)
            (user-error "Machine '%s' not found — try refreshing with 'g'" name)
	  (ssh-connect-machine machine))))))

(defvar ssh-machines-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'ssh-machines-connect-at-point)
    (define-key map (kbd "a") #'add-ssh-machine)
    (define-key map (kbd "d") #'ssh-machines-remove-at-point)
    (define-key map (kbd "e") #'ssh-machines-edit-at-point)
    (define-key map (kbd "g") #'list-ssh-machines)
    map)
  "Keymap for `ssh-machines-mode'.")

(define-derived-mode ssh-machines-mode tabulated-list-mode "SSH-Machines"
  "Major mode for listing and managing SSH machines.
\\{ssh-machines-mode-map}"
  :keymap ssh-machines-mode-map
  (setq tabulated-list-format
        [("Name"        15 t)
         ("Address"     30 t)
         ("Description" 25 t)
         ("Key"         15 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun ssh-machines--get-entries ()
  "Return entries for `tabulated-list-entries'."
  (let ((machines (multisession-value ssh-machines-list)))
    (mapcar (lambda (machine)
	      (list (ssh-machine-name machine)
		    (vector (ssh-machine-name machine)
			    (ssh-machine-full-address machine)
			    (or (ssh-machine-description machine) "")
			    (or (ssh-machine-key machine) ""))))
	    machines)))

(defun ssh-machines-remove-at-point ()
  "Delete the SSH machine at point."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (if (not name)
        (user-error "No machine at point")
      (let ((machine (ssh-find-machine name)))
        (if (not machine)
            (user-error "Machine '%s' not found — try refreshing with 'g'" name)
          (when (yes-or-no-p (format "Delete machine '%s'? " name))
            (setf (multisession-value ssh-machines-list)
                  (cl-remove-if (lambda (m)
                                  (string= name (ssh-machine-name m)))
                                (multisession-value ssh-machines-list)))
            (message "Deleted %s" name)
	    (ssh-machines--refresh-buffer)))))))

(defun ssh-machines-edit-at-point ()
  "Edit the SSH machine at point using `ssh-machine` struct."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (if (not name)
        (user-error "No machine at point")
      (let ((machine (ssh-find-machine name)))
        (if (not machine)
            (user-error "Machine '%s' not found" name)
	  
          (let* ((new-name (read-string "Name: " (ssh-machine-name machine)))
                 (new-host (read-string "Host: " (ssh-machine-host machine)))
                 (new-user (read-string "User (optional): " (or (ssh-machine-user machine) "")))
                 (new-desc (read-string "Description: " (or (ssh-machine-description machine) "")))
                 (new-key (ssh-select-or-create-key (ssh-machine-key machine))))

            (setf (ssh-machine-name machine) new-name
                  (ssh-machine-host machine) new-host
                  (ssh-machine-user machine) (unless (string-empty-p new-user) new-user)
                  (ssh-machine-description machine) new-desc
                  (ssh-machine-key machine) new-key)

	    ;; Force persistence to disk
	    (setf (multisession-value ssh-machines-list)
		  (multisession-value ssh-machines-list))

            (message "Updated machine '%s'" new-name)
	    (ssh-machines--refresh-buffer)))))))

(defun edit-ssh-machine ()
  "Edit an SSH machine by selecting from the list."
  (interactive)
  (unless (multisession-value ssh-machines-list)
    (user-error "No SSH machines configured"))

  (let* ((machines (multisession-value ssh-machines-list))
         (machine-names (mapcar #'ssh-machine-name machines))
         (selected-name (completing-read "Select machine to edit: " machine-names nil t))
         (machine (ssh-find-machine selected-name)))

    (when machine
      (let* ((new-name (read-string "Name: " (ssh-machine-name machine)))
             (new-host (read-string "Host: " (ssh-machine-host machine)))
             (new-user (read-string "User (optional): " (or (ssh-machine-user machine) "")))
             (new-desc (read-string "Description: " (or (ssh-machine-description machine) "")))
             (new-key (ssh-select-or-create-key (ssh-machine-key machine))))

        (setf (ssh-machine-name machine) new-name
	      (ssh-machine-host machine) new-host
	      (ssh-machine-user machine) (unless (string-empty-p new-user) new-user)
	      (ssh-machine-description machine) new-desc
	      (ssh-machine-key machine) new-key)

	;; Force persistence to disk
	(setf (multisession-value ssh-machines-list)
	      (multisession-value ssh-machines-list))

	(ssh-machines--refresh-buffer)
        (message "Updated %s" new-name)))))

(defun ssh-find-machine (name)
  "Return the ssh-machine struct with NAME, or nil if not found."  
  (cl-find name
           (multisession-value ssh-machines-list)
           :key #'ssh-machine-name
           :test #'string=))

(defun ssh-machines--refresh-buffer ()
  "Revert the `*SSH Machines*' buffer if it is currently displayed."
  (when-let ((buf (get-buffer "*SSH Machines*")))
    (with-current-buffer buf
      (when (derived-mode-p 'ssh-machines-mode)
        (tabulated-list-revert)))))

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
  "Export the list of SSH Machines to a specified FILE-PATH.
This version generate a file incompatible with the old version of `import-ssh-machines'"
  (interactive "FExport to file: ")
  (with-temp-file file-path
    (prin1 (multisession-value ssh-machines-list) (current-buffer)))
  (message "Exported SSH machines to %s" file-path))

(defun import-ssh-machines (file-path)
  "Import a list of SSH machines from a specified FILE-PATH.
This version is incompatible with files exported in the old version of `export-ssh-machines'
to import old machines use `ssh-machines-migrate-from-file'"
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
  "Import SSH hosts from ~/.ssh/config into `ssh-machines-list`.
Skips wildcard patterns and duplicates. Converts entries to `ssh-machine` structs."
  (interactive)
  (let* ((config-file "~/.ssh/config")
         (parsed-hosts (ssh-parse-config-file config-file))
         (existing-names (mapcar #'ssh-machine-name (multisession-value ssh-machines-list)))
         (imported 0)
         (duplicates 0))
    (dolist (host parsed-hosts)
      (let* ((name (nth 0 host))
             (address (nth 1 host))
             (desc (nth 2 host))
             (key (nth 3 host)))
	(cond
	 ((member name existing-names)
	  (cl-incf duplicates))
	 (t
	  (let* ((parsed (ssh--split-address address))
		 (user (car parsed))
		 (host-name (cdr parsed)) 
                 (machine (make-ssh-machine
                           :name name
                           :host host-name
                           :user user
                           :description desc
                           :key key)))
	    (push machine (multisession-value ssh-machines-list)))
	  (cl-incf imported)))))
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
  "Copy FILE-PATH to a selected SSH machine interactively.
Uses the method defined by `ssh-copy-method' (scp or rsync).
Opens a terminal buffer to allow interactive password entry."
  (interactive "fFile to copy: ")
  (unless (multisession-value ssh-machines-list)
    (user-error "No SSH machines configured"))

  (let* ((machines (multisession-value ssh-machines-list))
         (selected-name (completing-read "Select target machine: "
                                         (mapcar #'ssh-machine-name machines) nil t))
         (machine (cl-find selected-name machines
                           :key #'ssh-machine-name :test #'string=)))
    (when machine
      (let* ((remote-path  (read-string "Remote destination path: "))
             (local-file   (expand-file-name file-path))
             (address      (ssh-machine-full-address machine))
             (key          (ssh-machine-key machine))
             (key-option   (when key (format "-i %s" (shell-quote-argument (expand-file-name key ssh-keys-directory)))))
             (cmd
              (cl-case ssh-copy-method
                (scp (format "scp %s %s %s"
                             (or key-option "")
                             (shell-quote-argument local-file)
                             (concat address ":"
                                     (shell-quote-argument remote-path))))
                (rsync (format "rsync -avz %s %s %s"
                               (if key-option
				   (format "-e \"ssh %s\"" key-option)
				 "")
                               (shell-quote-argument local-file)
                               (concat address ":"
                                       (shell-quote-argument remote-path))))
                (otherwise
                 (user-error "Unknown ssh-copy-method: %s" ssh-copy-method))))
             (full-cmd (concat cmd
                               "; echo \"\nTransfer complete — press Enter to close.\""
                               "; read")))
        (let ((buf (generate-new-buffer
                    (format "*%s Transfer*"
                            (upcase (symbol-name ssh-copy-method))))))
          (with-current-buffer buf
            (term-mode)
            (term-exec buf (format "%s-transfer" ssh-copy-method) "/bin/sh" nil (list "-c" full-cmd)))
          (switch-to-buffer buf))))))

(defun ssh-list-private-keys ()
  "Return list of SSH private keys in `ssh-keys-directory`."
  (cl-remove-if
   (lambda (file) (string-match-p "\\.pub$" file))
   (directory-files ssh-keys-directory nil "^id_.*$")))

(defun ssh-generate-key (key-type key-name key-comment)
  "Generate a new SSH key pair synchronously.
Returns KEY-NAME on success, nil on failure."
  (let* ((key-path (expand-file-name key-name ssh-keys-directory))
         (bits (when (string= key-type "rsa")
                 (read-string "Key bits (2048, 4096): " "4096")))
         (command (format "ssh-keygen -t %s%s -f %s -C %s -N \"\""
                          key-type
                          (if (string= key-type "rsa")
                              (format " -b %s" bits)
                            "")
                          (shell-quote-argument key-path)
                          (shell-quote-argument key-comment))))
    (make-directory ssh-keys-directory t)
    (if (file-exists-p key-path)
        (user-error "Key with name '%s' already exists" key-name)
      (message "Generating key...")
      (let ((exit-code (shell-command command)))
        (if (= exit-code 0)
            (progn
              (message "Key '%s' generated successfully." key-name)
              key-name)
          (user-error "ssh-keygen failed with exit code %d" exit-code))))))

(defun ssh-select-or-create-key (&optional current-key)
  "Select an existing SSH key or create a new one.
If CURRENT-KEY is provided, it is pre-selected as the default.
Returns the key filename relative to `ssh-keys-directory', or nil."
  (let* ((keys     (sort (ssh-list-private-keys) #'string<))
         (choices  (append '("[None]" "[Generate new key]") keys))
         (default  (if (and current-key (member current-key keys))
                       current-key
                     "[None]"))
         (prompt   (if current-key
                       (format "SSH key (current: %s): " current-key)
                     "SSH key: "))
         (selection (completing-read prompt choices nil t nil nil default)))
    (cond
     ((string= selection "[None]")
      nil)
     ((string= selection "[Generate new key]")
      (let* ((key-type    (completing-read "Key type: "
                                           '("rsa" "ed25519" "ecdsa" "dsa")
                                           nil t nil nil "ed25519"))
             (key-name    (read-string "Key name (e.g., id_github): " "id_"))
             (key-comment (read-string "Key comment (typically email): "))
             (generated   (ssh-generate-key key-type key-name key-comment)))
        generated))
     (t
      selection))))

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

(defun ssh-copy-key (key-file)
  "Copy an SSH public key (KEY-FILE) to a selected SSH machine using `ssh-copy-id`.
Prompts for both key and machine, uses `ssh-machine` structs."
  (interactive
   (list (completing-read "Select key to copy: "
                          (ssh-list-private-keys) nil t)))
  (unless (multisession-value ssh-machines-list)
    (user-error "No SSH machines configured"))

  (let* ((machines (multisession-value ssh-machines-list))
         (selected-name (completing-read "Select target machine: "
                                         (mapcar #'ssh-machine-name machines) nil t))
         (machine (ssh-find-machine selected-name)))
    (unless machine
      (user-error "Machine '%s' not found" selected-name))

    (let ((pub-key-path (expand-file-name (concat key-file ".pub") ssh-keys-directory)))
      (unless (file-exists-p pub-key-path)
        (user-error "Public key file %s does not exist" pub-key-path))

      (let* ((cmd (format "ssh-copy-id -i %s %s; echo \"\nDone — press Enter to close.\"; read"
                          (shell-quote-argument pub-key-path)
                          (ssh-machine-full-address machine)))
             (buf (generate-new-buffer "*SSH Copy ID*")))
        (with-current-buffer buf
          (term-mode)
          (term-exec buf "ssh-copy-id" "/bin/sh" nil (list "-c" cmd)))
        (switch-to-buffer buf)))))

(defun ssh-associate-key-with-machine ()
  "Associate an SSH key with a specific machine in `ssh-machines-list` using the `ssh-machine` struct."
  (interactive)
  (unless (multisession-value ssh-machines-list)
    (user-error "No SSH machines configured"))
  (let* ((machines (multisession-value ssh-machines-list))
         (selected-name (completing-read "Select machine: "
                                         (mapcar #'ssh-machine-name machines) nil t))
         (machine (ssh-find-machine selected-name)))
    (unless machine
      (user-error "Machine '%s' not found" selected-name))
    (let ((selected-key (ssh-select-or-create-key (ssh-machine-key machine))))
    ;;(let ((selected-key (ssh-select-or-create-key)))
      (setf (ssh-machine-key machine) selected-key)

      ;; Force persistence to disk
      (setf (multisession-value ssh-machines-list)
	    (multisession-value ssh-machines-list))
      
      (ssh-machines--refresh-buffer)
      (message "Associated key '%s' with machine '%s'" (or selected-key "[None]") selected-name))))

(defun ssh-machines-migrate-from-file (file-path)
  "Migrate old-format SSH machines from FILE-PATH to the current struct format.
The old format is a list of (NAME ADDRESS DESCRIPTION) where ADDRESS is
user@host.
Existing entries in `ssh-machines-list' whose name already appears are skipped."
  (interactive "fOld machines file: ")
  (unless (file-exists-p file-path)
    (user-error "File not found: %s" file-path))

  (let* ((old-data (with-temp-buffer
                     (insert-file-contents file-path)
                     (read (current-buffer))))
	 
         (existing-names (mapcar #'ssh-machine-name
                                 (multisession-value ssh-machines-list)))
         (imported   0)
         (duplicates 0))

    (dolist (entry old-data)
      (pcase-let ((`(,name ,address ,desc . ,_rest) entry))
        (if (member name existing-names)
            (cl-incf duplicates)
	  (let* ((parsed (ssh--split-address address))
                 (user   (car parsed))
                 (host   (cdr parsed))
                 (machine (make-ssh-machine
                           :name        name
                           :host        host
                           :user        (and user (not (string-empty-p user)) user)
                           :description (or desc "")
                           :key         nil)))
            (setf (multisession-value ssh-machines-list)
                  (append (multisession-value ssh-machines-list) (list machine)))
            (push name existing-names)
            (cl-incf imported)))))

    (ssh-machines--refresh-buffer)
    (message "Migration complete: %d machine(s) imported from %s%s"
             imported
             file-path
             (if (> duplicates 0)
                 (format " (%d duplicate(s) skipped)" duplicates)
               ""))))

(provide 'init-ssh)

;;; init-ssh.el ends here.
