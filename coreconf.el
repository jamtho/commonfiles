;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aesthetic (GUI only)

;; (when (display-graphic-p)
;;   (setq ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682"
;;                                  "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]
;;         custom-enabled-themes '(deeper-blue)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General behaviour

(setq electric-indent-mode nil
      
      ido-enable-flex-matching t
      ido-mode 'both

      compilation-scroll-output 'first-error
      
      package-selected-packages '(meson-mode ttl-mode haxe-imports haxe-mode
                                  markdown-mode lua-mode d-mode
                                  modern-cpp-font-lock magit))

(ido-mode t)
(winner-mode t)

;; (load-theme 'deeper-blue)

(global-linum-mode t)

(tool-bar-mode -1)
(menu-bar-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax formatting

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq tab-width 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-M-o") 'prev-window)

(global-set-key (kbd "C-v") 'scroll-up-onethird-window)
(global-set-key (kbd "M-v") 'scroll-down-onethird-window)

(global-set-key (kbd "C-c g") 'magit-status)

(global-set-key (kbd "C-c m") 'recompile)

(global-set-key (kbd "C-w") 'kill-region-or-word)

(windmove-default-keybindings)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra functions

;; From https://www.emacswiki.org/emacs/RevertBuffer#toc2
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

(defun prev-window ()
  (interactive)
  (other-window -1))

(defun scroll-up-onethird-window ()
  (interactive)
  (scroll-up-command (floor (/ (window-height) 3))))

(defun scroll-down-onethird-window ()
  (interactive)
  (scroll-down-command (floor (/ (window-height) 3))))

(defun kill-region-or-word ()
  "If a region is selected, kill it. Otherwise backwards kill a word"
  (interactive)
  (if (region-active-p)
    (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
