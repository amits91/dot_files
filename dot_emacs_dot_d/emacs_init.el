;; Emacs config file

;; Start
(require 'package)
; list the packages you want
(setq package-list '(company powerline))

; list the repositories containing them
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Basic Emacs setup
(column-number-mode)
(show-paren-mode)

;; Enable IDO mode and configuration
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

;; put backups in one location
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; Powerline
(powerline-center-theme)
(setq powerline-default-separator 'wave)


