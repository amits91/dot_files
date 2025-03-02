
#!/bin/bash
# =============================================================================
# MSYS2 Environment Backup and Restore Script (No Tarball)
# =============================================================================
# This script automates backing up and restoring your MSYS2 environment.
#
# It saves the following information into the backup directory (msys2_env/):
#
# msys2_env/
# ├── msys2_packages.txt   # List of explicitly installed packages
# └── config_backup/        # Saved configuration files
#     ├── .bashrc
#     ├── .vimrc
#     ├── .tcshrc
#     ├── .gitconfig
#     └── .config/         # Additional configuration files
#
# Usage:
#   ./msys2_env_manager.sh backup      - Backs up the MSYS2 environment to msys2_env/
#   ./msys2_env_manager.sh restore     - Restores the environment from msys2_env/
#   ./msys2_env_manager.sh help        - Displays this help message
#
# Migration Steps:
# 1. On your current MSYS2 system, run:
#      ./msys2_env_manager.sh backup
#    This creates the "msys2_env/" folder with the package list and configuration files.
#
# 2. To ensure that all files (including dot files) are added to Git, use:
#      git add -f msys2_env/
#
#    Alternatively, modify your .gitignore to exempt the msys2_env folder:
#      !msys2_env/
#
# 3. Commit and push the backup to your GitHub repository.
#
# 4. On the new system, clone your repository and run:
#      ./msys2_env_manager.sh restore
#
# Author: Your Name
# =============================================================================

# Define backup directories and file names
BACKUP_DIR="msys2_env"
PKG_LIST="$BACKUP_DIR/msys2_packages.txt"
CONFIG_BACKUP="$BACKUP_DIR/config_backup"

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
    echo "It will save the following structure in the backup directory (msys2_env/):"
    echo ""
    echo "msys2_env/"
    echo "├── msys2_packages.txt   # List of explicitly installed packages"
    echo "└── config_backup/        # Saved configuration files"
    echo "    ├── .bashrc"
    echo "    ├── .vimrc"
    echo "    ├── .tcshrc"
    echo "    ├── .gitconfig"
    echo "    └── .config/         # Additional configuration files"
    echo ""
    echo "To add all files (including dot files) to Git, run the following commands:"
    echo "  git add -f msys2_env/"
    echo ""
    echo "Alternatively, modify your .gitignore to exempt the msys2_env folder:"
    echo "  !msys2_env/"
    echo ""
    echo "Usage:"
    echo "  $0 backup    - Backs up the MSYS2 environment to msys2_env/"
    echo "  $0 restore   - Restores the MSYS2 environment from msys2_env/"
    echo "  $0 help      - Displays this help message"
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
    echo "🔄 Backing up MSYS2 environment..."
    
    # Backup installed packages
    echo "📦 Saving package list to $PKG_LIST..."
    pacman -Qe > "$PKG_LIST"

    # Backup important configuration files
    echo "⚙️  Saving configuration files to $CONFIG_BACKUP..."
    cp ~/.bashrc ~/.bash_profile ~/.vimrc ~/.inputrc ~/.gitconfig "$CONFIG_BACKUP" 2>/dev/null
    cp ~/.tcshrc "$CONFIG_BACKUP" 2>/dev/null
    if [ -d ~/.config ]; then
        cp -r ~/.config "$CONFIG_BACKUP"
    fi

    echo "✅ Backup completed! Check the '$BACKUP_DIR' directory."
}

# Function to restore MSYS2 environment
restore_env() {
    echo "🔄 Restoring MSYS2 environment..."

    # Update MSYS2
    echo "📦 Updating MSYS2..."
    pacman -Syu --noconfirm

    # Restore installed packages
    if [ -f "$PKG_LIST" ]; then
        echo "📦 Installing packages from $PKG_LIST..."
        pacman -S --needed - < "$PKG_LIST"
    else
        echo "⚠️ Package list not found in $PKG_LIST! Skipping package restore."
    fi

    # Restore configuration files
    if [ -d "$CONFIG_BACKUP" ]; then
        echo "⚙️  Restoring configuration files from $CONFIG_BACKUP..."
        cp "$CONFIG_BACKUP"/.* ~/
        cp -r "$CONFIG_BACKUP"/.config ~/
    else
        echo "⚠️ Configuration backup not found in $CONFIG_BACKUP! Skipping configuration restore."
    fi

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
