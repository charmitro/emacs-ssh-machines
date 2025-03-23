# Emacs SSH Machines Management Tool

## Overview

The Emacs SSH Machines Management Tool is designed to simplify the management of SSH connections directly within Emacs. This Emacs package allows users to efficiently manage a list of SSH machines, including adding, removing, listing, and connecting to them.

## Features

- **Add SSH Machines:** Users can easily add new SSH machines, specifying their name, address, and optional notes for each entry.
- **Remove SSH Machines:** This feature allows for the removal of SSH machines from the list, keeping the configuration clean and up-to-date.
- **List SSH Machines:** Displays a neatly formatted list of all configured SSH machines in an Emacs buffer, providing a quick overview of available connections.
- **Connect to SSH Machines:** Enables users to connect to a selected SSH machine using the SSH protocol directly from Emacs, facilitating seamless remote work.
- **Copy Files to remote:** Users can easily copy files from their local machine to the remote machine.
- **SSH Key Management:** Manage your SSH keys directly from Emacs, including viewing, generating, and associating keys with specific servers.

## Installation

To install the Emacs SSH Machines Management Tool, follow these steps:

1. **Clone the Repository:**
    Clone the tool's repository to a local directory.
    ```shell
    git clone https://github.com/yourusername/emacs-ssh-machines.git
    ```

2. **Copy the `init-ssh.el` File:**
    Copy the `init-ssh.el` file into your `.emacs.d` directory or another directory within your Emacs load path.

3. **Load the Tool:**
    Include the following line in your Emacs configuration file (`.emacs` or `init.el`):
    ```elisp
    (require 'init-ssh)
    ```

## Usage

### Adding a New SSH Machine

Invoke `M-x add-ssh-machine` and input the machine's name, address, and notes as prompted to add a new SSH machine to your list.

### Removing an SSH Machine

Execute `M-x remove-ssh-machine` and select the machine you wish to remove from the presented list.

### Listing SSH Machines

To view all configured SSH machines, use `M-x list-ssh-machines`. This will display the machines in a new Emacs buffer.

### Connecting to an SSH Machine

Use `M-x ssh-connect` and select the desired SSH machine to establish an SSH connection.

### Copying a file to an SSH Machine

To copy a file from your local machine to the remote one, use `M-x copy-file-to-ssh-machine`. You will be prompted to select the file to copy, the target machin, and the remote destination path.

### Managing SSH Keys

The SSH Machines Management Tool now includes comprehensive SSH key management features:

#### Viewing SSH Keys

Use `M-x ssh-list-keys` to display all your SSH keys in a buffer. This will show:
- The names of your key files
- Whether each key has a corresponding public key
- Associated comments (typically email addresses)

#### Generating SSH Keys

Execute `M-x ssh-generate-key` to create a new SSH key pair. You will be prompted for:
- Key type (RSA, Ed25519, ECDSA, or DSA)
- Key name (filename)
- Key comment (typically your email address)
- For RSA keys, you can specify the bit length (2048 or 4096)

#### Copying SSH Keys to Remote Servers

Use `M-x ssh-copy-key` to copy a public key to a remote server. This simplifies the process of setting up key-based authentication.

#### Associating Keys with Specific Servers

Execute `M-x ssh-associate-key-with-machine` to link a specific SSH key with a server in your machines list. When you connect to this server using `M-x ssh-connect`, the associated key will be used automatically.

## Configuration

The list of SSH machines is stored in the `ssh-machines-list` variable. This list is automatically updated as you add or remove machines through the Emacs commands provided by this tool.

## Contributing

If you'd like to improve th9s tool, please fork the repository, create a feature branch for your changes, and submit a pull request.

## License

This project is licensed under the GPL-3.0 License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

Special thanks,

- to the Emacs community for their invaluable resources and support, making tools like this possible.
- to the Reddit community that helped me refine and make this better.
