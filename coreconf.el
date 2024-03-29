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
;; Server

(server-start)


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
      ido-everywhere t
      ido-mode 'both

      compilation-scroll-output 'first-error
      
      package-selected-packages '(meson-mode ttl-mode haxe-imports haxe-mode
                                  markdown-mode lua-mode d-mode
                                  modern-cpp-font-lock magit))

(ido-mode t)
(winner-mode t)
(fido-mode t)

;; (load-theme 'deeper-blue)

(global-linum-mode t)

(tool-bar-mode -1)
(menu-bar-mode -1)

(which-key-mode)
(setq which-key-idle-delay 0.5)

(treemacs-project-follow-mode t)

(setq-default isearch-lazy-count t)
(setq-default isearch-allow-scroll t)
(setq-default lazy-highlight-initial-delay 0)

;; Always word wrap markdown files
(add-hook 'find-file-hook
          (lambda () (when (string= (file-name-extension buffer-file-name) "md")
                       (visual-line-mode t))))

(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_GB")

;; Default location; feel free to override in local .emacs
(setq jamtho/persistent-scratch-file "~/PERSISTENT_SCRATCH.md")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax formatting

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq tab-width 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-M-o") 'jamtho/prev-window)

(global-set-key (kbd "C-v") 'jamtho/scroll-up-onethird-window)
(global-set-key (kbd "M-v") 'jamtho/scroll-down-onethird-window)

(global-set-key (kbd "C-c g") 'magit-status)

(global-set-key (kbd "C-c m") 'recompile)

(global-set-key (kbd "C-c s") 'jamtho/switch-to-text-scratch)
(global-set-key (kbd "C-c S") 'jamtho/find-file-persistent-scratch)

(global-set-key (kbd "C-c i") 'jamtho/ispell-region-or-buffer)

(global-set-key (kbd "C-c t") 'treemacs)

(global-set-key (kbd "C-w") 'jamtho/kill-region-or-word)

(global-set-key (kbd "<f5>")   'kmacro-end-and-call-macro)
(global-set-key (kbd "S-<f5>") 'kmacro-start-macro)
(global-set-key (kbd "C-<f5>") 'apply-macro-to-region-lines)

(windmove-default-keybindings)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra functions

;; From https://www.emacswiki.org/emacs/RevertBuffer#toc2
(defun jamtho/revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

(defun jamtho/prev-window ()
  (interactive)
  (other-window -1))

(defun jamtho/scroll-up-onethird-window ()
  (interactive)
  (scroll-up-command (floor (/ (window-height) 3))))

(defun jamtho/scroll-down-onethird-window ()
  (interactive)
  (scroll-down-command (floor (/ (window-height) 3))))

(defun jamtho/kill-region-or-word ()
  "If a region is selected, kill it. Otherwise backwards kill a word"
  (interactive)
  (if (region-active-p)
    (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun jamtho/xclip-copy-region ()
  "Pipe the selected region to xclip, to copy to x clipboard"
  (interactive)
  (let* ((process-connection-type nil)
         (proc (start-process "xclip" nil "xclip"
                              "-selection" "clipboard"))
         (str (buffer-substring (region-beginning) (region-end))))
    (process-send-string proc str)
    (process-send-eof proc)
    str))

(defun jamtho/xclip-paste ()
  "Fetch the clipboard from xclip and insert at the point"
  (interactive)
  (call-process "xclip" nil standard-output nil
                "-o" "-selection" "clipboard"))

(defun jamtho/xclip-paste-primary ()
  "As xclip-paste but uses the PRIMARY clipboard"
  (interactive)
  (call-process "xclip" nil standard-output nil
                "-o" "-selection" "primary"))

(defun jamtho/switch-to-text-scratch ()
  (interactive)
  (switch-to-buffer "*text-scratch*")
  (visual-line-mode t))

(defun jamtho/ispell-region-or-buffer ()
  "If a region is selected, spell check it. Otherwise spell check the whole buffer"
  (interactive)
  (if (region-active-p)
    (ispell-region (region-beginning) (region-end))
    (ispell-buffer)))

(defun jamtho/find-file-persistent-scratch ()
  (interactive)
  (find-file jamtho/persistent-scratch-file))
