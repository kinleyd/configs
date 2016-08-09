;; activate package manager

(require 'package)
(add-to-list 'package-archives
             ;'("marmalade" . "http://marmalade-repo.org/packages/")) ;; problematic in 2014
             ;'("melpa" . "http://melpa.milkbox.net/packages/")) ;; deprecated by technomancy
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t) ;; May 2014

(package-initialize)

;; basic setting preferences
(global-visual-line-mode) ; wrap text
(cua-mode 0) ; disable cua mode, stick to emacs defaults 
(tool-bar-mode 0)
(menu-bar-mode 0) 
(column-number-mode t)
(show-paren-mode t) ; highlight matching paren
(setq inhibit-startup-message t) 
(setq comint-process-echoes t) ; suppress echo from zsh shell
(setq default-directory "/home/kdd/" ) ; default directory, except for shells that may set their own
(setq-default major-mode 'lisp-mode) ; need this for elisp code, like .emacs
(setq-default indent-tabs-mode nil) ; don't allow tabs
(setq make-backup-files nil) ; stop making the ~name-of-file files
(setq mouse-autoselect-window t) ; focus follows mouse
(setq save-place '(saveplace)) ; file opens where you left cursor
(setq visible-bell t) ; don't let emacs hurt your ears, flash the title and mode line instead
(setq inhibit-startup-echo-area-message "kdd") ; ---> not sure what this does <---

;; set emacs path, differs from shell path
;; needed to get nrepl-jack-in and cider-jack-in to find the lein file
(setenv "PATH" (concat (getenv "PATH") ":/home/kdd/LTF"))
(setq exec-path (append exec-path '("/home/kdd/LTF")))

;; open freqently used files and set default buffer focus
(find-file "~/Documents/todo.org")
(find-file "~/Documents/current.zen")
(find-file "~/Documents/tech-notes.org")
;(find-file "~/LTF/Code-Complete/ps/src/ps/workspace.clj")
;(setq initial-buffer-choice "*scratch*")

;; multi-term beats just term, shell, etc., but is still quite buggy. Better with a standalone terminal
;; (require 'multi-term)
;; (setq multi-term-program "/bin/zsh")
;; (global-set-key (kbd "C-c t") 'multi-term-next)
;; (global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

;; (let ((default-directory "/home/kdd/LTF/Code-Complete/"))
;;  (multi-term-next))
;; (let ((default-directory "/home/kdd/LTF/Code-Complete/"))
;;  (multi-term)) ; create a new multi-term shell, but only if one doesn't exist already

;; after emacs initialization
;; for example theme packages won't be found until after initialization
(add-hook 'after-init-hook 'kdd-after-init-hook)
(defun kdd-after-init-hook ()
  ;; set theme and fonts
  ;; different fonts and syntax for reference
  ;; "-*-Courier-normal-r-*-*-14-*-*-*-c-*-iso8859-1" only 14 looks right, but too large
  ;; "DejaVu Serif-10.5" works
  ;; "DejaVu Sans Mono-11" works
  ;; "DejaVu Sans-10.5" works well, capitalized part of font name gets a space
  ;; "-*-terminus-medium-r-*-*-*-120-75-75-*-*-iso8859-15" works well, current choice
  ;; "-xos4-terminus-medium-r-normal--14-140-*-*-*-*-*-*" 12 too small, 14 too big
  ;; "-bitstream-bitstream vera sans mono-medium-r-*-*-*-120-*-*-*-*-*-*" 
  (progn 
    (load-theme 'yak-shave t) ; yak-shave (based on fogus) odersky, graham, hickey, fogus, dorsey, deeper-blue, wombat  
    (setq default-frame-alist
          '((font . "-*-terminus-medium-r-*-*-*-120-75-75-*-*-iso8859-15")
          (cursor-type . hbar))))) ; change cursor type at frame rather than buffer level

;; on emacsclient load
;; this works, but only if a file is loaded explicitly with the *first* emacsclient you open 
(add-hook 'server-visit-hook 'kdd-emacsclient-hook)
(defun kdd-emacsclient-hook ()
  (progn
    (scroll-bar-mode 0))) 

;; setup To Do windows
;; open multiple windows in one frame
(defun kdd-setup-todo-windows ()
(interactive)
  (progn
    (switch-to-buffer "tech-notes.org")
    (split-window-horizontally)
    (other-window 1)
    (switch-to-buffer "todo.org")))
;;(global-set-key (kbd "<f4>") 'kdd-setup-todo-windows) ; works, but I don't really need it


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

;; setup zen window, called from hlwm's loadapps.sh
(defun kdd-setup-zen-window ()
(interactive)
  (progn
    (switch-to-buffer "current.zen")
    (message "The name of this buffer is: %s." (buffer-name)) ;; this line makes the buffer active and ready for kdd-zen-mode
    (kdd-zen-mode)))

; cider configuration
(require 'cider-mode) ; necessary?
(setq cider-repl-history-file "/home/kdd/LTF/cider-history")
(setq nrepl-buffer-name-show-port t) ; show port address on the mode line
(global-set-key (kbd "C-<return>") 'cider-eval-defun-at-point) ; make the same as Light Table (equivalent of C-c C-e)
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

;; latex-preview-pane
(require 'latex-preview-pane)
(define-key latex-preview-pane-mode-map (kbd "M-p") 'latex-preview-pane-update)
(define-key latex-preview-pane-mode-map (kbd "s-p") 'latex-preview-pane-update)
(define-key latex-preview-pane-mode-map (kbd "M-P") 'latex-preview-update)
(define-key latex-preview-pane-mode-map (kbd "s-P") 'latex-preview-update)

;; org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

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

;; ledger-mode
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
;(add-to-list 'load-path
;             (expand-file-name "/path/to/ledger/source/lisp/")) ;; this doesn't exist on my system; only /usr/bin/ledger binary exists
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

;; emacs-fu count words
(defun djcb-count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
        (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ee0cc4e721500058af9b615592e537235450681ec5573177c2f367c96dfa7c6b" "7f3ea0c2d068ea6637341179b84dbc8c4ab1f8bf035ef06cd41686768a86e7a5" "8d234d08fb45010d5854f5a02d6987fd15ec267ec31112a0e3af4f1b8e920fdb" "fac746a78af78fcf716ae088160ee8b0648c4b851d0e9ad379a41cbccf3b8631" "36417043bebdff55324588473d7a06d8779963252f8c49640588e92754c753b7" "a28fc19036e89dc3c8d5a621c019abc21cd53ebd6723dca3bfc60e01cb6c497a" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "c492ddfd206aff95a75c8903cacd870dede984a33484b0405e9df7437a47fddf" "edc57cee709b2ca9c4d6bb2d057a110e393212c87b85d4493091761fd40e7c91" "970f589165ab9d73466f00deba08ad4adf7ffde0076d628b9e45d452ac4e33a8" "31b0607b7e19a6661840aa443adaa6dc4d439cae8e49a48fcc0cbd583d2d63e6" "643d506774ab8c87696ac0d8e6b345ad6ba1ab2aec04f0640d4dd6d6295b6fd7" "d5a21039359f4b4be4fa5a853fee70208bd8c819982d95a0e6fef881b035aca1" "e620d6129b317db0196466218f2bd5be90c66d4d932ff7ed2654d706dfc9cdcf" "7bbf2041da1b83425d82e71ae8b9e7924ed6d42b4992ebe6214017ec986317ab" "d135b8f3a4bcb1935c8d796ccb109a05f845309a69006d1e3bafda77a37c2196" "2a82f4aa20595b6925eefc74c8664c050d6b36cb040eae93eb8b78ba83889d88" "c63e6f40f11cbce4551ebb83821be4b289a03ce501011c845dc15d960159e525" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "971825a49b035983aea283bd06e6252df1bab0550c1c188af6b9fda5bff8d1cd" "72a3bb5d218ef1444af0d00390ff862ed49c37ce3be70ee1d91a6ed302212658" "c59e4e5d9eafac96ff9312201ed32964942c5f0419ba05650133dc8aea52f204" "0a29ccdf9eab3fb64aaf89994fedddd549164074da37eb057279ebb2707eb428" "c11d747895c51b2718b7d42ea287661dd7fe13f5867fa88c321eb293ad119553" "a5a99771823b78d92f4c61c745761ce2c794fba30e16b7490a7a2b144a68110f" "2306f5974504dc73b80267a63b82fdc599e141d0053c6e5d10d0394d28bdb71c" "033c7d957936f3688752295fd2542135d1e675eff90e9ab9a8d9cd2ec81526dd" "a4930a52e86e2515e15ea861a161e80948ad9bdc307886d8a4b88150f755e489" "9561163963d05a5ff4ed413312119b7af88d67da327ac2c2a2aa1b9d9a9cf061" "e9b4d7d46f9739ac0f2a121a123091cabc3b0652cac24b1173d98542a024696e" "159664742111ec0c106c1dc069c6a15588fc6101f5b571a06c2d6e972dd0f177" "b25293a4b0d05662c4448f76164a499827cd2d25ee45bccb648f1843ed843d22" "826de98a1076162d157a20425c228abd8352eef4d46f906d0a570fe3e022db57" "7f01e790e661336bcdec8d8fc572246487fe75ea9ed6f364d7101f29815c41c9" "05abf6dbc240f7d806c3b66aaa2f3a5befc20cc785ef5ec33f49200927458be8" "abddb2b0da2afd657437909faf6a6f8e8a519d44884d5ae3294e70a2f6c0dcf9" "dc31c62c3b2ae4557ed6b138622b6896cff15f7fc184ba43117486e0d226c114" "b1aff71035f1576f8519e8e10ee4e5fef11e6c304ced24062ae5806c327e15b5" "307c57ad13539550f6ce21e5732b36c9f50ad184bfeae226f127ebffaeb668e7" "1542b1b15157adc7ee442e71815e0c123778014ebcd71eb4e1cf8ffaf722684e" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" default)))
 '(ledger-reports
   (quote
    (("bs" "ledger -f ps.ledger bal ^Assets ^Liabilities")
     ("is" "ledger -f ps.ledger bal ^Income ^Expenses")
     ("lksajfdlksdf" "ledger -f ps.ledger equity")
     ("accounts" "ledger assets liabilities")
     ("equity" "ledger ")
     ("bal" "ledger -f %(ledger-file) bal")
     ("reg" "ledger -f %(ledger-file) reg")
     ("payee" "ledger -f %(ledger-file) reg @%(payee)")
     ("account" "ledger -f %(ledger-file) reg %(account)")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
