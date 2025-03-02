#!/bin/bash
# =============================================================================
# MSYS2 Environment Backup and Restore Script (No Tarball)
# =============================================================================
# This script automates backing up and restoring your MSYS2 environment.
#
# It backs up the following items from your home directory (~):
#
#   - A list of explicitly installed packages (using pacman -Qe)
#   - Configuration files: .bashrc, .vimrc, .tcshrc, .gitconfig, and the entire .config/ directory.
#
# The backup is stored in a folder named "msys2_env" created in the current working directory.
#
# Backup Structure:
#   msys2_env/
#   ├── msys2_packages.txt   # List of explicitly installed packages (with versions)
#   └── config_backup/       # Saved configuration files (copied from your home directory)
#       ├── .bashrc
#       ├── .vimrc
#       ├── .tcshrc
#       ├── .gitconfig
#       └── .config/        # Additional configuration files
#
# During restore, the script will update MSYS2 and then copy the configuration files 
# back into your home directory ($HOME), regardless of where you run the restore command.
#
# In addition, the restore step will:
#   - Check for diff-so-fancy and install it if missing.
#   - Check for vim-plug and install/update Vim plugins.
#
# Git Usage:
#   To ensure that all files (including dotfiles) are added to Git, run:
#       git add -f msys2_env/
#   Or modify your .gitignore to exempt the msys2_env folder:
#       !msys2_env/
#
# Migration Steps:
#   1. On your current MSYS2 system, run:
#         ./msys2_env_manager.sh backup
#      This creates the "msys2_env/" folder (in the current directory) with the package list
#      and configuration files copied from your home directory.
#
#   2. Add the msys2_env/ folder to Git:
#         git add -f msys2_env/
#   3. Commit and push to your GitHub repository.
#   4. On a new system, clone your repository and run:
#         ./msys2_env_manager.sh restore
#
# Help Text:
#   The backup folder will contain:
#       msys2_packages.txt   - a list of installed packages with versions.
#       config_backup/       - configuration files copied from your home directory,
#                              including dotfiles.
#
# To add all files to Git, use:
#       git add -f msys2_env/
#
# Author: Your Name
# =============================================================================

# Define backup directory and file names (created in the current working directory)
BACKUP_DIR="msys2_env"
PKG_LIST="$BACKUP_DIR/msys2_packages.txt"
CONFIG_BACKUP="$BACKUP_DIR/config_backup"

# Define home directory
HOME_DIR="$HOME"

# Ensure the backup directory exists
mkdir -p "$BACKUP_DIR"
mkdir -p "$CONFIG_BACKUP"

# Function to display help
show_help() {
    echo "MSYS2 Environment Backup and Restore Script (No Tarball)"
    echo "==========================================================="
    echo ""
    echo "This script automates backing up and restoring your MSYS2 environment."
    echo ""
    echo "Backup Structure (created in the current directory):"
    echo ""
    echo "msys2_env/"
    echo "├── msys2_packages.txt   # List of explicitly installed packages (with versions)"
    echo "└── config_backup/       # Saved configuration files (copied from your home directory)"
    echo "    ├── .bashrc"
    echo "    ├── .vimrc"
    echo "    ├── .tcshrc"
    echo "    ├── .gitconfig"
    echo "    └── .config/        # Additional configuration files"
    echo ""
    echo "During restore, configuration files will be copied into your home directory ($HOME_DIR)."
    echo ""
    echo "Git Usage:"
    echo "  To add the backup to Git (ensuring dotfiles are included), run:"
    echo "      git add -f msys2_env/"
    echo "  Alternatively, modify your .gitignore to exempt the msys2_env folder:"
    echo "      !msys2_env/"
    echo ""
    echo "Usage:"
    echo "  $0 backup    - Backs up the MSYS2 environment from your home directory into msys2_env/ in the current folder."
    echo "  $0 restore   - Restores the MSYS2 environment from msys2_env/ to your home directory."
    echo "  $0 help      - Displays this help message."
    echo ""
    echo "Migration Steps:"
    echo "  1. Run '$0 backup' on your current MSYS2 system."
    echo "  2. Add the backup folder to Git:"
    echo "         git add -f msys2_env/"
    echo "  3. Commit and push to GitHub."
    echo "  4. On a new system, clone your repository and run '$0 restore'."
    exit 0
}

# Function to backup MSYS2 environment
backup_env() {
    echo "🔄 Backing up MSYS2 environment from home directory ($HOME_DIR)..."
    
    # Backup installed packages
    echo "📦 Saving package list to $PKG_LIST..."
    pacman -Qe > "$PKG_LIST"

    # Backup important configuration files from home directory
    echo "⚙️  Copying configuration files from $HOME_DIR to $CONFIG_BACKUP..."
    cp "$HOME_DIR"/.bashrc "$HOME_DIR"/.bash_profile "$HOME_DIR"/.vimrc "$HOME_DIR"/.inputrc "$HOME_DIR"/.gitconfig "$CONFIG_BACKUP" 2>/dev/null
    cp "$HOME_DIR"/.tcshrc "$CONFIG_BACKUP" 2>/dev/null
    if [ -d "$HOME_DIR/.config" ]; then
        cp -r "$HOME_DIR/.config" "$CONFIG_BACKUP"
    fi

    echo "✅ Backup completed! Check the '$BACKUP_DIR' directory."
}

# Function to restore MSYS2 environment
restore_env() {
    echo "🔄 Restoring MSYS2 environment to home directory ($HOME_DIR)..."

    # Update MSYS2
    echo "📦 Updating MSYS2..."
    pacman -Syu --noconfirm

    # Restore installed packages by extracting only package names from msys2_packages.txt
    if [ -f "$PKG_LIST" ]; then
        echo "📦 Installing packages from $PKG_LIST..."
        awk '{print $1}' "$PKG_LIST" | xargs -r pacman -S --needed --noconfirm
    else
        echo "⚠️ Package list not found in $PKG_LIST! Skipping package restore."
    fi

    # Restore configuration files to home directory
    if [ -d "$CONFIG_BACKUP" ]; then
        echo "⚙️  Restoring configuration files to $HOME_DIR..."
        cp "$CONFIG_BACKUP"/.* "$HOME_DIR"/ 2>/dev/null
        cp -r "$CONFIG_BACKUP"/.config "$HOME_DIR"/ 2>/dev/null
    else
        echo "⚠️ Configuration backup not found in $CONFIG_BACKUP! Skipping configuration restore."
    fi

    # Install diff-so-fancy if not present
    if ! command -v diff-so-fancy >/dev/null 2>&1; then
        echo "📦 diff-so-fancy not found. Installing diff-so-fancy..."
        if [ ! -d "$HOME/.diff-so-fancy" ]; then
            git clone https://github.com/so-fancy/diff-so-fancy.git "$HOME/.diff-so-fancy"
        fi
        if ! grep -q 'export PATH="$HOME/.diff-so-fancy:$PATH"' "$HOME/.bashrc"; then
            echo 'export PATH="$HOME/.diff-so-fancy:$PATH"' >> "$HOME/.bashrc"
            echo "✅ diff-so-fancy added to PATH in .bashrc. Please restart your terminal if needed."
        fi
    else
        echo "✅ diff-so-fancy is already installed."
    fi

    # Ensure vim-plug is installed
    if [ ! -f "$HOME/.vim/autoload/plug.vim" ]; then
        echo "📦 vim-plug not found. Installing vim-plug..."
        curl -fLo "$HOME/.vim/autoload/plug.vim" --create-dirs \
            https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    else
        echo "✅ vim-plug is already installed."
    fi

    # Update Vim plugins using vim-plug
    echo "📦 Installing/updating Vim plugins using vim-plug..."
    vim +PlugInstall +qall

    echo "✅ Restore completed!"
}

# Main logic: Check for argument
case "$1" in
    backup)
        backup_env
        ;;
    restore)
        restore_env
        ;;
    help)
        show_help
        ;;
    *)
        echo "❌ Invalid argument! Use '$0 help' for usage information."
        exit 1
        ;;
esac
