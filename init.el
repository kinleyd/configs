;; syntax for clashing buffer names:
;; bridge2.clj, build.boot<ps>

;; activate package manager

(require 'package)
;;(add-to-list 'package-archives
;;             '("gnu" . "https://elpa.gnu.org/packages/") t) ;; Aug 2016 - recommended by EmacsWiki -  Kept giving tls termination errors and update problems as well. However, a gnu repo shows up nevertheless in when listing packages - perhaps it's there by default?

(add-to-list 'package-archives
    '("marmalade" . "https://marmalade-repo.org/packages/")) ;; problematic in 2014; but restored in Aug 2016 to get clojurescript-mode and latex-preview-pane, with the https link

;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.milkbox.net/packages/")) ;; deprecated by technomancy; reinstated in Aug 2016 in light of all the problems, still recommended by Bozhar - it is still the one with cutting edge (and unstable) releases.
;; Stay AWAY from this repo unless you want to go through the process of removing and reinstalling all packages!!!

(add-to-list 'package-archives
    '("melpa-stable" . "https://stable.melpa.org/packages/") t) ;; damn, the stable melpa address had changed and that's why I wasn't getting updates! Inserted this repo Aug 2016 - for list of packages available see: http://stable.melpa.org/#/

(package-initialize)

;; exwm 
(require 'exwm)
(require 'exwm-config)
(exwm-enable)
(exwm-config-ido)

(setq exwm-workspace-show-all-buffers t) ;; share exwm buffers in all workspaces, not just the workspace in which it was created in (the default behaviour)
(setq exwm-layout-show-all-buffers t)

;; You may want Emacs to show you the time
(setq display-time-default-load-average nil)
(setq display-time-day-and-date t)
(display-time-mode t)
;;(display-time) ;; not sure if I need this, with the above already there - nope, not required

;; customize one of these to have it just right
;;(display-time-24hr-format)
;;(display-time-format)

;; setup workspaces
(setq exwm-workspace-number 6) ;; 0 - 5, zero based

;; multi-monitor setup
(require 'exwm-randr)
;; you can associate multiple workspaces with a monitor output
(setq exwm-randr-workspace-output-plist
      '(0 "DVI-I-1" 1 "DVI-D-0" 2 "HDMI-0" 3 "DVI-I-1" 4 "DVI-D-0" 5 "HDMI-0"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DVI-I-1 --output DVI-D-0 --output HDMI-0 --auto")))
(exwm-randr-enable)

;; note by default:
;; s + down-mouse-1 to move an X window; s + down-mouse-3 to resize an X window.

;; exwm simulation keys, global to all application.
;; Prefix with C-c C-q to access X windows shortkeys if not covered by simulation keys 
(defvar kdd/default-simulation-keys
  '(
    ;; move
    ([?\C-b] . left)
    ([?\M-b] . C-left)
    ([?\C-f] . right)
    ([?\M-f] . C-right)
    ([?\C-p] . up)
    ([?\C-n] . down)
    ([?\C-a] . home) 
    ([?\C-e] . end) ;; in spreadsheets: C-e C-p - go to top;  C-e C-n - go to bottom
    ([?\M-v] . prior)
    ([?\C-v] . next)
    ;; delete
    ([?\C-d] . delete)
    ([?\C-k] . (S-end delete))
    ([?\M-d] . (C-S-right delete))
    ;; cut/copy/paste.
    ([?\C-w] . ?\C-x)
    ([?\M-w] . ?\C-c)
    ([?\C-y] . ?\C-v)
    ;; search
    ([?\C-s] . ?\C-f)
    ))

(exwm-input-set-simulation-keys kdd/default-simulation-keys)

;; exwm shortkeys

;; + We always need a way to go back to line-mode from char-mode
(exwm-input-set-key (kbd "s-r") #'exwm-reset)

;; + Bind a key to switch workspace interactively
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

;; + Bind "s-0" to "s-9" to switch to or create the corresponding workspace
;; However, it does not place the workspace on the active monitor - it just activates it on the monitor it's already on
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))

;; launch an app interactively
(exwm-input-set-key (kbd "s-&")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))

;; specify app shortkeys
;; s - super key

;; amule
(exwm-input-set-key (kbd "s-a")
                    (lambda ()
                      (interactive)
                      (exwm-workspace-switch 5)
                      (start-process-shell-command "amule" nil "amule")))

;; firefox
(exwm-input-set-key (kbd "s-f")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "firefox" nil "firefox")))

;; fbreader
(exwm-input-set-key (kbd "s-F")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "fbreader" nil "FBReader")))

;; gmrun
(exwm-input-set-key (kbd "s-g")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "gmrun" nil "gmrun")))

;; gnumeric
(exwm-input-set-key (kbd "s-G")
                    (lambda ()
                      (interactive)
                      (exwm-workspace-switch 4)
                      (start-process-shell-command "gnumeric" nil "gnumeric")))

;; gimp
(exwm-input-set-key (kbd "s-h")
                    (lambda ()
                      (interactive)
                      (exwm-workspace-switch 5)
                      (start-process-shell-command "gimp" nil "gimp")))

;; load workspaces

;; set global var to track if workspaces have been loaded
(defvar are-workspaces-loaded 0
  "Are workspaces loaded? Start out with value of 0.")
;;(symbol-value 'are-workspaces-loaded)

(exwm-input-set-key (kbd "s-l")
                    (lambda ()
                      (interactive)
                      ;; load only if not already loaded
                      (if (= are-workspaces-loaded 0)
                          (progn
                            (exwm-workspace-switch 3)
                            (kdd-setup-zen-window)
                        
                            (exwm-workspace-switch 0)
                            (start-process-shell-command "firefox" nil "firefox")
                            (sleep-for 2)

                            (exwm-workspace-switch 1)
                            (kdd-setup-cider-windows-exwm)
                            
                            (exwm-workspace-switch 2)
                            (kdd-setup-todo-windows)
                            (setq are-workspaces-loaded 1)))))


;; libreoffice
(exwm-input-set-key (kbd "s-L")
                    (lambda ()
                      (interactive)
                      ;;(exwm-run-shell-command "libreoffice") ;; this doesn't work
                      (exwm-workspace-switch 4) ;; select center output
                      (start-process-shell-command "libreoffice" nil "libreoffice")))

;; puddletag
(exwm-input-set-key (kbd "s-p")
                    (lambda ()
                      (interactive)
                      (exwm-workspace-switch 5)
                      (start-process-shell-command "puddletag" nil "puddletag")))


;; urxvt
(exwm-input-set-key (kbd "s-u")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "urxvt" nil "urxvt")))

;; start WinXP (with qemu and spicy); setup workspaces and windows manually
;; Note M-x only works on screens that are not in full-screen mode.

(exwm-input-set-key (kbd "s-w")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "vm-start" nil "vm-start")
                      (sleep-for 15) ;; need at least 15 seconds for the 2 exwm buffers to be created 
                      (start-process-shell-command "spicy" nil "spicy -h 127.0.0.1 -p 3001") ;; better without '--full-screen'; C-c C-f manually or toggle to fullscreen programmatically after load
                      (sleep-for 3) ;; and another 3 seconds here as well
                      (exwm-workspace-switch 5)
                      (exwm-workspace-switch-to-buffer "Spicy<3>") ;; second window
                      (exwm-layout-toggle-fullscreen)
                      (exwm-workspace-switch 4)
                      (exwm-workspace-switch-to-buffer "Spicy<2>") ;; main window -- using switch-to-buffer causes problems wehn combining start of spicy with workspace rearrangement
                      (exwm-layout-toggle-fullscreen)
                      ))

;; xfe
(exwm-input-set-key (kbd "s-x")
                    (lambda ()
                      (interactive)
                      (exwm-workspace-switch 4)
                      (start-process-shell-command "xfe" nil "xfe")))


;; setup 'to do' windows
;; open multiple windows in one frame
(defun kdd-setup-todo-windows ()
  (interactive)
  (progn
    (switch-to-buffer "tech-notes.org")
    (split-window-horizontally)
    (other-window 1)
    (switch-to-buffer "todo.org")))
;;(global-set-key (kbd "<f4>") 'kdd-setup-todo-windows) ; works, but I don't really need it

;; setup cider windows
;; open multiple windows in one frame

;; herbstluftwm version
(defun kdd-setup-cider-windows ()
(interactive)
    (progn
    (switch-to-buffer "wow.cljs")
    (split-window-horizontally) ;; emacs places you by default to window 0 after a split; just like hlwm
    (other-window 1)
    (switch-to-buffer "bridge1.clj"))) ;; syntax for clashing names: bridge2.clj, build.boot<ps>

;; exwm version
(defun kdd-setup-cider-windows-exwm ()
  (interactive)
  (progn
    (switch-to-buffer "wow.cljs") 
    (split-window-below) ;; emacs places you by default to window 0 after a split; just like hlwm
    (split-window-right)
    (other-window 1)
    (switch-to-buffer "bridge1.clj")
    (other-window 1)
    (split-window-right)
    (cd "~/")
    (start-process-shell-command "tmux" nil "urxvt -e tmux new-session -A -s main")
    (sleep-for 1)
    (other-window 1) ;; it does indeed shift down to the next split and not back 
    (switch-to-buffer "*Messages*")
    (split-window-right -56)
    (other-window 1)
    (start-process-shell-command "conky" nil "urxvt -e conky")
    (sleep-for 1)
    (other-window -1)
    (split-window-below)
    (start-process-shell-command "ncmpcpp" nil "urxvt -e ncmpcpp")
    (sleep-for 1)
    ;; if you want to set up an Arch logo here with neofetch
    ;; (split-window-right) 
    ;; (other-window 1)
    ;; (start-process-shell-command "urxvt" nil "urxvt")
    ;; (sleep-for 2)
    ;; "urxvt -e neofetch -L --ascii_bold off" -> doesn't work if scripted in exwm. Start it manually
    ;; neofetch has many more options than screenfetch (now removed) - see man neofetch
    ;;(other-window 1)
    ))

;; (split-window-right) splits the selected window into two side-by-side windows. The left window is the selected one; the right window displays the same portion of the same buffer, and has the same value of point. A positive numeric argument specifies how many columns to give the left window; a negative numeric argument specifies how many columns to give the right window. 


;; 80 characters requires only two saccades to read a line
(defun kdd-zen-mode ()
    "Switch to text-mode and set margins leaving a composition space of only 80 characters."
    (interactive) ; required if you want the function accessible in the mini buffer
    (let* ((current-extension (file-name-extension (or (buffer-file-name) "foo.unknown")))
    (max-text-width 80)
    (margin (max 0 (/ (- (window-width (selected-window)) max-text-width) 2))))
    (if (not (string= current-extension "zen"))
    ;; Do nothing if this isn't a .zen file, my custom zen mode file extension..
    ()
    (set-window-margins (selected-window) margin margin)
    (text-mode))))
;; custom keybinding...
(global-set-key (kbd "C-x Z") 'kdd-zen-mode)

;; setup zen window
(defun kdd-setup-zen-window ()
(interactive)
    (progn
    (switch-to-buffer "current.zen")
    (message "The name of this buffer is: %s." (buffer-name)) ;; this line makes the buffer active and ready for kdd-zen-mode
    (kdd-zen-mode)))

;; setup supporting cider window
(defun kdd-setup-messages-window ()
(interactive)
    (progn
    (switch-to-buffer "*Messages*")))

;; assign spice workspaces dynamically
;; this doesn't seem to be working atm
;; order of windows isn't as I expected - and changes if I change the order below!
;; best is not to set the placement - just move it to workspace 4 and 5 after loading the WinXP vm.

;; (require 'subr-x)  ;; because of when-let

;; (defvar exwm-workspace-window-assignments
;;   '(;;("Spicy" . 4) ;; root buffer
;;     ("Spicy<2>" . 4) ;; main screen buffer
;;     ("Spicy<3>" . 5)) ;; secondary screen buffer
;;   "An alist of window classes and which workspace to put them on.")

;; (add-hook 'exwm-manage-finish-hook
;;           (lambda ()
;;             (when-let ((target (cdr (assoc exwm-class-name exwm-workspace-window-assignments))))
;;               (exwm-workspace-move-window target))))




;; In the following example, we use class names for all windows except for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))


;; exwm system tray -- requires a system tray applet to be running
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)

;; eshell
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setenv "TERM" "emacs") ; enable colors
   ))


;; basic setting preferences
(cua-mode -1) ; disable cua mode, stick to emacs defaults 
(tool-bar-mode -1)
(menu-bar-mode -1) 
(column-number-mode t)
(show-paren-mode t) ; highlight matching paren
(setq inhibit-startup-message t) 
(setq comint-process-echoes t) ; suppress echo from zsh shell
(setq default-directory "~/" ) ; default directory, except for shells that may set their own
(setq-default major-mode 'lisp-mode) ; need this for elisp code, like .emacs
(setq-default indent-tabs-mode nil) ; don't allow tabs
(setq make-backup-files nil) ; stop making the ~name-of-file files
(setq mouse-autoselect-window t) ; focus follows mouse
(setq save-place '(saveplace)) ; file opens where you left cursor
(setq visible-bell t) ; don't let emacs hurt your ears, flash the title and mode line instead
(setq inhibit-startup-echo-area-message "kdd") ; ---> not sure what this does <---
(setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "xdg-open") ;; send links to default desktop browser

;; after emacs initialization
;; for example theme packages won't be found until after initialization
(add-hook 'after-init-hook 'kdd-after-init-hook)
(defun kdd-after-init-hook ()
    ;; set theme and fonts
    ;; there are different ways to refer to a font:
    ;; use 'fc-list | grep -i partial-name' to get the font name to use
    ;; use M-x describe-font to get the emacs long form, which is an easier way to pass font size in the name
    ;; always keep at least one here in case fonts get borked completely and you can't load or read emacs files:
    ;; "-xos4-terminus-medium-r-normal--12-140-*-*-*-*-*-*"
    ;; "-UW-Ttyp0-normal-normal-normal-*-12-*-*-*-c-80-iso10646-1"
    ;; "-unknown-Dina-normal-normal-normal-*-13-*-*-*-c-*-iso10646-1"
    (progn 
    (load-theme 'yak-shave t) ; yak-shave (based on fogus) odersky, graham, hickey, fogus, dorsey, deeper-blue, wombat  
    (setq default-frame-alist
    '((font . "-xos4-terminus-medium-r-normal--12-140-*-*-*-*-*-*")
    ;; should I use "name-9", "name 9" or just "name" when using short reference format?
    ;; They all work, but don't pass the size value at all. The long format does.
    ;; (set-face-attribute 'default nil :height 90) ;; this doesn't work here, but doesn't kill emacs boot either; get equiv. of (font . "FONT")
    ;; The value is in 1/10pt, so 100 will give you 10pt, etc.
    (cursor-type . hbar))))) ; change cursor type at frame rather than buffer level

;; set emacs path, differs from shell path
;; needed to get nrepl-jack-in and cider-jack-in to find the lein file
(setenv "PATH" (concat (getenv "PATH") ":~/LTF:~/.emacs.d"))
(setq exec-path (append exec-path '("~/LTF")))

;; on emacsclient load
;; disable scroll bars - works, finally
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))

(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; enable ledger-mode before ledger files are opened, otherwise it doesn't kick in
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
;(add-to-list 'load-path
;; (expand-file-name "/path/to/ledger/source/lisp/")) ;; this doesn't exist on my system; only /usr/bin/ledger binary exists
(setq ledger-use-iso-dates t) ;; use yyyy-mm-dd format

;; set file extensions that ledger-mode should recognize - you can choose any you prefer, though .dat is common
(add-to-list 'auto-mode-alist '("\\.dat$" . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode)) ;; this might not be necessary, according to ledger-mode at github

;; customize ledger-mode colorscheme
;; ledger-mode fonts don't integrate with Emacs themes, so going this route
;; can't use this approach as the font face isn't defined at load time:
;; (set-face-background 'ledger-font-xact-highlight-face "gray15")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(ledger-font-comment-face ((t (:foreground "olive drab"))))
 '(ledger-font-payee-uncleared-face ((t (:foreground "SlateGray3"))))
 '(ledger-font-posting-account-face ((t (:foreground "DarkOlivegreen3"))))
 '(ledger-font-posting-amount-face ((t (:foreground "golden rod"))))
 '(ledger-font-posting-date-face ((t (:foreground "dark golden rod"))))
 '(ledger-font-xact-highlight-face ((t (:background "gray15")))))

;; enable flycheck-ledger
(eval-after-load 'flycheck
  '(require 'flycheck-ledger))

;; enable company-mode completions
;;(add-hook 'after-init-hook 'global-company-mode)
;; disabled: more of a pain than gain

;; open freqently used files and set default buffer focus

;; configuration files
(find-file "~/.emacs.d/init.el")
;;(find-file "~/.config/herbstluftwm/loadapps.sh")
;;(find-file "~/.config/herbstluftwm/autostart")
;; (find-file "~/.conkyrc")
;; (find-file "~/.tmux.conf")
;; (find-file "~/.zshrc")
;; (find-file "~/.zshenv")
(find-file "~/.xinitrc")
(find-file "~/.Xresources")
;;(find-file "/etc/X11/xorg.conf")

;; to do and notes
(find-file "~/Documents/todo.org")
(find-file "~/Documents/current.zen")
(find-file "~/Documents/tech-notes.org")

;; The Bridge 1: Bread and Butter
(find-file "~/build.boot")
(find-file "~/src/bridge1.clj")
(find-file "~/src/payroll/genecs.clj")
(find-file "~/src/payroll/analysecs.clj")
(find-file "~/src/payroll/unittests.clj")

;; exploration: lumo
(find-file "~/LTF/Code-Complete/lumo/src/test/wow.cljs")
(find-file "~/LTF/Code-Complete/lumo/build.cljs")
(find-file "~/LTF/Code-Complete/lumo/src/test/core.cljs")

;; exploration rum, tenzing
;; (find-file "~/LTF/Code-Complete/rum-example/build.boot")
;; (find-file "~/LTF/Code-Complete/rum-example/src/clj/rum_example/db.clj")
;; (find-file "~/LTF/Code-Complete/rum-example/src/cljs/rum_example/app.cljs")

;; hoplon
;; (find-file "~/LTF/Code-Complete/hoplon/payroll/src/index.cljs.hl")
;; (find-file "~/LTF/Code-Complete/hoplon/payroll/build.boot")


;; multi-term beats just term, shell, etc.
;; but is still quite buggy. Better with a standalone terminal
;; (require 'multi-term)
;; (setq multi-term-program "/bin/zsh")
;; (global-set-key (kbd "C-c t") 'multi-term-next)
;; (global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

;; (let ((default-directory "~/LTF/Code-Complete/"))
;;  (multi-term-next))
;; (let ((default-directory "~/LTF/Code-Complete/"))
;;  (multi-term)) ; create a new multi-term shell, but only if one doesn't exist already


; cider configuration
(require 'cider-mode) ; necessary?
(setq cider-repl-history-file "~/LTF/cider-history")
(setq nrepl-buffer-name-show-port t) ; show port address on the mode line
(global-set-key (kbd "C-<return>") 'cider-eval-defun-at-point)
; was cider-eval-expression-at-point
;(global-set-key (kbd "C-<return>") 'cider-eval-expression-at-point-in-repl) ; echoes command in repl

;; nrepl configuration
(add-hook 'nrepl-interaction-mode-hook
    'nrepl-turn-on-eldoc-mode) ; enable eldoc in clojure buffers:
(setq nrepl-popup-stacktraces nil) ; override error buffer from popping up while working in the REPL buffer
(add-to-list 'same-window-buffer-names "*nrepl*") ; make C-c C-z switch to the *nrepl* buffer in the current window
(add-hook 'nrepl-mode-hook 'paredit-mode) ; enable paredit in the nREPL buffer as well

;; editing with root privileges
(defun djcb-find-file-as-root ()
    "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
    (interactive)
    (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
    (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))
;; with a custom keybinding...
(global-set-key (kbd "C-x F") 'djcb-find-file-as-root)

;; individual emacs package configuration

;; load and enable latex-preview-pane from local file; must have latex-preview-pane package from the repo to get the newer script to load
(require 'latex-preview-pane)
(load-file "~/.emacs.d/latex-preview-pane.el")
(latex-preview-pane-enable)

;; wrap paragraph based on indentation of first line
(global-visual-line-mode 1)

;; org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-hide-leading-stars t) ;; reduce to one visible but indented * for all org-mode levels
(setq org-startup-indented t) ;; indent paragraphs but needs global-visual-line-mode to handle the word wrapping

;; uniqify - better buffer names
(require 'uniquify)

;; ido-mode
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1) 

(require 'ido-at-point); with this M-Tab invokes completions in code
(ido-at-point-mode)

(require 'ido-vertical-mode) ; ido displays options vertically in mini-buffer
(ido-vertical-mode 1)

;; ido M-x mode, overrides default M-x minibuffer
(global-set-key
    "\M-x"
    (lambda ()
    (interactive)
    (call-interactively
    (intern
    (ido-completing-read
    "M-x "
    (all-completions "" obarray 'commandp))))))

;; recentf-mode
(require 'recentf)
    (recentf-mode 1)
    (setq recentf-max-menu-items 25)
    (global-set-key "\C-x\ \C-r" 'recentf-open-files)

; spell check, comes into effect in Tex mode, not automatic: M-x ispell-buffer RET
(setq-default ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
(setq-default ispell-dictionary "british") ; this can be set to any language

;; emacs-fu count words
(defun djcb-count-words (&optional begin end)
    "count words between BEGIN and END (region); if no region defined, count words in buffer"
    (interactive "r")
    (let ((b (if mark-active begin (point-min)))
    (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

;; helm completions
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)

;; Aug 2017 - helm bindings aren't working any more - not sure why
;; the helm commands being bound to are working though

;; bind helm commands to emacs defaults
;; I prefer ido over the color settings that come with helm (particularly the helm-find-files directory names).
;; so I'll keep helm, but comment out the helm-find-file rebinding - I can alway invoke it from the mini buffer if I want to.
(global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files) 
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; enable projectile
(projectile-mode)

;; enable evaluation of clojure and calc code in org-mode
(require 'ob-clojure)
(require 'ob-calc)

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; treemacs
(require 'treemacs)
(require 'treemacs-projectile)

;; treemacs set up

(global-set-key [f7] 'treemacs-projectile) ;; works if current buffer is a projectile buffer; shows contents of parent folders as well
(global-set-key [f8] 'treemacs) ;; shows files and folders in current folder only
;;(global-set-key [fx] 'treemacs-projectile-toggle) ;; not using this - let treemacs-toggle do the job
(global-set-key [f9] 'treemacs-toggle) 


(treemacs-follow-mode t)
(treemacs-filewatch-mode t)

(setq treemacs-follow-after-init          t
      treemacs-width                      35
      treemacs-indentation                2
      treemacs-git-integration            t
      treemacs-collapse-dirs              3
      treemacs-silent-refresh             nil
      treemacs-change-root-without-asking nil
      treemacs-sorting                    'alphabetic-desc
      treemacs-show-hidden-files          nil
      treemacs-never-persist              nil
      treemacs-is-never-other-window      nil
      treemacs-goto-tag-strategy          'refetch-index
      treemacs-no-png-images              t)

   ;;(describe-variable 'treemacs-show-hidden-files)

;; temporary fix for missing icon issue in treemacs
(setq treemacs--not-selected-icon-background "gray8"
      treemacs--selected-icon-background     "darkolivegreen")
(--each treemacs--icons
  (progn
    (treemacs--set-img-property
     (get-text-property 0 'img-selected it)
     :background treemacs--selected-icon-background)
    (treemacs--set-img-property
     (get-text-property 0 'img-unselected it)
     :background treemacs--not-selected-icon-background)))

;; do without images
;; track treemacs issue #53 - this isn't ready yet
;; (setq treemacs-no-images t)

;; multi-term - emacs key chords work with multiterm
(require 'multi-term)
(setq multi-term-program "/bin/zsh")


;; eterm-256color
(add-hook 'term-mode-hook #'eterm-256color-mode)
;; basically start a terminal that supports the minibuffer (multi-term for ex) - then M-x eterm-256color-mode


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-safe-themes
   (quote
    ("8f50e5dab3cba7fbd661790b215dc4cce39bc994e0855531d3a1fb0fddd025f0" "4dc3f07d1d8406c631bdaedafe67c9c14dc3d0d704ab87551213811c687f9a72" "345908d64c3fbfa9c894913e2a59f9a399cac23ebbe422446514aa3ad658b429" "ee0cc4e721500058af9b615592e537235450681ec5573177c2f367c96dfa7c6b" "7f3ea0c2d068ea6637341179b84dbc8c4ab1f8bf035ef06cd41686768a86e7a5" "8d234d08fb45010d5854f5a02d6987fd15ec267ec31112a0e3af4f1b8e920fdb" "fac746a78af78fcf716ae088160ee8b0648c4b851d0e9ad379a41cbccf3b8631" "36417043bebdff55324588473d7a06d8779963252f8c49640588e92754c753b7" "a28fc19036e89dc3c8d5a621c019abc21cd53ebd6723dca3bfc60e01cb6c497a" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "c492ddfd206aff95a75c8903cacd870dede984a33484b0405e9df7437a47fddf" "edc57cee709b2ca9c4d6bb2d057a110e393212c87b85d4493091761fd40e7c91" "970f589165ab9d73466f00deba08ad4adf7ffde0076d628b9e45d452ac4e33a8" "31b0607b7e19a6661840aa443adaa6dc4d439cae8e49a48fcc0cbd583d2d63e6" "643d506774ab8c87696ac0d8e6b345ad6ba1ab2aec04f0640d4dd6d6295b6fd7" "d5a21039359f4b4be4fa5a853fee70208bd8c819982d95a0e6fef881b035aca1" "e620d6129b317db0196466218f2bd5be90c66d4d932ff7ed2654d706dfc9cdcf" "7bbf2041da1b83425d82e71ae8b9e7924ed6d42b4992ebe6214017ec986317ab" "d135b8f3a4bcb1935c8d796ccb109a05f845309a69006d1e3bafda77a37c2196" "2a82f4aa20595b6925eefc74c8664c050d6b36cb040eae93eb8b78ba83889d88" "c63e6f40f11cbce4551ebb83821be4b289a03ce501011c845dc15d960159e525" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "971825a49b035983aea283bd06e6252df1bab0550c1c188af6b9fda5bff8d1cd" "72a3bb5d218ef1444af0d00390ff862ed49c37ce3be70ee1d91a6ed302212658" "c59e4e5d9eafac96ff9312201ed32964942c5f0419ba05650133dc8aea52f204" "0a29ccdf9eab3fb64aaf89994fedddd549164074da37eb057279ebb2707eb428" "c11d747895c51b2718b7d42ea287661dd7fe13f5867fa88c321eb293ad119553" "a5a99771823b78d92f4c61c745761ce2c794fba30e16b7490a7a2b144a68110f" "2306f5974504dc73b80267a63b82fdc599e141d0053c6e5d10d0394d28bdb71c" "033c7d957936f3688752295fd2542135d1e675eff90e9ab9a8d9cd2ec81526dd" "a4930a52e86e2515e15ea861a161e80948ad9bdc307886d8a4b88150f755e489" "9561163963d05a5ff4ed413312119b7af88d67da327ac2c2a2aa1b9d9a9cf061" "e9b4d7d46f9739ac0f2a121a123091cabc3b0652cac24b1173d98542a024696e" "159664742111ec0c106c1dc069c6a15588fc6101f5b571a06c2d6e972dd0f177" "b25293a4b0d05662c4448f76164a499827cd2d25ee45bccb648f1843ed843d22" "826de98a1076162d157a20425c228abd8352eef4d46f906d0a570fe3e022db57" "7f01e790e661336bcdec8d8fc572246487fe75ea9ed6f364d7101f29815c41c9" "05abf6dbc240f7d806c3b66aaa2f3a5befc20cc785ef5ec33f49200927458be8" "abddb2b0da2afd657437909faf6a6f8e8a519d44884d5ae3294e70a2f6c0dcf9" "dc31c62c3b2ae4557ed6b138622b6896cff15f7fc184ba43117486e0d226c114" "b1aff71035f1576f8519e8e10ee4e5fef11e6c304ced24062ae5806c327e15b5" "307c57ad13539550f6ce21e5732b36c9f50ad184bfeae226f127ebffaeb668e7" "1542b1b15157adc7ee442e71815e0c123778014ebcd71eb4e1cf8ffaf722684e" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" default)))
 '(debug-on-error t)
 '(ledger-reports
   (quote
    (("accounts" "ledger assets liabilities")
     ("equity" "ledger ")
     ("bal" "ledger -f %(ledger-file) bal")
     ("reg" "ledger -f %(ledger-file) reg")
     ("payee" "ledger -f %(ledger-file) reg @%(payee)")
     ("account" "ledger -f %(ledger-file) reg %(account)"))))
 '(ledger-use-iso-dates t)
 '(package-selected-packages
   (quote
    (transmission eterm-256color multi-term exwm use-package treemacs treemacs-projectile auto-indent-mode aggressive-indent flycheck-ledger projectile inf-clojure s paredit-everywhere org ledger-mode latex-preview-pane ido-yes-or-no ido-vertical-mode ido-at-point flylisp dash company clojurescript-mode clojure-cheatsheet ac-cider))))

