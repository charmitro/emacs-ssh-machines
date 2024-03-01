# Emacs SSH Machines Management Tool

## Overview
The Emacs SSH Machines Management Tool provides a simple yet powerful interface for managing and connecting to SSH machines directly within Emacs. It allows users to add, remove, list, and connect to SSH machines with ease.

## Features
- **Add SSH Machines:** Easily add new SSH machines with their name, address, and notes.
- **Remove SSH Machines:** Remove machines from the list when they are no longer needed.
- **List SSH Machines:** View a list of all configured SSH machines in a dedicated Emacs buffer.
- **Connect to SSH Machines:** Connect to any listed machine via SSH with a simple command.

## Installation

1. **Clone the Repository:**
    ```shell
    git clone https://github.com/charmitro/emacs-ssh-machines.git
    ```

2. **Copy the `init-ssh.el` File:**
    Copy the `init-ssh.el` file into your `.emacs.d` directory or any other directory in your Emacs load path.

3. **Load the Tool:**
    Add the following line to your Emacs configuration file (`.emacs` or `init.el`):
    ```elisp
    (require 'init-ssh)
    ```

4. **Restart Emacs:**
    Restart Emacs to load the new configuration.

## Usage

- **Adding a New SSH Machine:**
    Use the command `M-x add-ssh-machine` and follow the prompts to enter the machine's name, address, and notes.

- **Removing an SSH Machine:**
    Use the command `M-x remove-ssh-machine` and select the machine you wish to remove.

- **Listing SSH Machines:**
    Use the command `M-x list-ssh-machines` to view all configured machines.

- **Connecting to an SSH Machine:**
    Use the command `M-x ssh-connect` and select the machine you wish to connect to.

## Configuration

The tool stores the list of SSH machines in `~/.emacs.d/ssh-machines.el`. This file is automatically created and updated as you add or remove machines. You can manually edit this file if necessary, but it is recommended to use the provided Emacs commands to ensure the correct format.

## Contributing

Contributions are welcome! If you'd like to contribute, please fork the repository and use a feature branch. Pull requests are warmly welcome.

## License

This project is licensed under the  GPL-3.0 License - see the LICENSE.md file for details.

## Acknowledgments

- Thanks to the Emacs community for providing invaluable resources and support.
