;; Emacs config file

;; Start
(require 'package)
; list the packages you want
(setq package-list '(company powerline helm-swoop nlinum multiple-cursors))

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
(scroll-bar-mode -1)
;;(tool-bar-mode -1)

;; Enable IDO mode and configuration
;;(setq ido-enable-flex-matching t)
;;(setq ido-everywhere t)
;;(ido-mode 1)
;;(setq ido-create-new-buffer 'always)
;;(setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

;; put backups in one location
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; Powerline
(powerline-center-theme)
(setq powerline-default-separator 'wave)

;; Disable bell
(setq visible-bell 1)

;; company
(require 'company)

;; keybindings for company mode
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)


;; helm from https://github.com/emacs-helm/helm
(require 'helm)

;; Locate the helm-swoop folder to your path
(require 'helm-swoop)

(global-set-key (kbd "M-i") 'helm-swoop)
;;(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;;(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;;(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persisten-action)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; line numbers
(nlinum-mode)

;; Multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

;; Org indent mode
(setq org-startup-indented t)

;; Org capture key binding
(global-set-key (kbd "C-c c") 'org-capture)
;; Org capture template
(setq org-capture-templates
      (quote (
	      ("j" "Journal Entry"
	       entry (file+datetree "c:/Users/asrivas/Desktop/AMIT_ALL_FILES/git/org/journal.org")
	       "* Event: %?\n\n  %i\n\n  From: %a"
	       :empty-lines 1)
	      ("l" "Log Time" 
	       entry (file+datetree "c:/Users/asrivas/Desktop/AMIT_ALL_FILES/git/org/timelog.org")

	       "** %U - %^{Activity}  :TIME:")
("n" "Notes" 
entry (file+datetree "c:/Users/asrivas/Desktop/AMIT_ALL_FILES/git/org/taskdiary.org")

"* %^{Description}  %^g
%?
Added: %U")
("t" "Task Diary" 
entry (file+datetree "c:/Users/asrivas/Desktop/AMIT_ALL_FILES/git/org/taskdiary.org")

"* TODO %^{Description}  %^g
%?
Added: %U")
	      )
	     )
      )

;; Restore file in last session
(desktop-save-mode 1)

;; line wrap in org mode
;;(define-key org-mode-map "\M-q" 'toggle-truncate-lines)

;; linewrap enabled
;;(setq org-startup-truncated nil)

;; Org timestamp for DONE
(setq org-log-done 'time)
;; Org agenda
(setq org-agenda-files (quote ("c:/Users/asrivas/Desktop/AMIT_ALL_FILES/git/org")
			      )
      )
(global-set-key "\C-ca" 'org-agenda)
			     
;; Org refile
(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
