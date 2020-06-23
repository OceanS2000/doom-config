;;; completion/helm-ocean/config.el -*- lexical-binding: t; -*-

;;; Packages

(use-package! helm-mode
  :hook (doom-first-input . helm-mode)
  :init
  (map! [remap apropos]                   #'helm-apropos
        [remap find-library]              #'helm-locate-library
        [remap bookmark-jump]             #'helm-bookmarks
        [remap execute-extended-command]  #'helm-M-x
        [remap find-file]                 #'helm-find-files
        [remap locate]                    #'helm-locate
        [remap imenu]                     #'helm-semantic-or-imenu
        [remap noop-show-kill-ring]       #'helm-show-kill-ring
        [remap persp-switch-to-buffer]    #'+helm/workspace-mini
        [remap switch-to-buffer]          #'helm-buffers-list
        [remap projectile-find-file]      #'+helm/projectile-find-file
        [remap projectile-recentf]        #'helm-projectile-recentf
        [remap projectile-switch-project] #'helm-projectile-switch-project
        [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer
        [remap recentf-open-files]        #'helm-recentf
        [remap yank-pop]                  #'helm-show-kill-ring
        [remap unicode-chars-list-chars]  #'+helm/unicode)
  :config
  ;; helm is too heavy for `find-file-at-point'
  (add-to-list 'helm-completing-read-handlers-alist (cons #'find-file-at-point nil)))


(use-package! helm
  :after helm-mode
  :preface
  (setq helm-candidate-number-limit 50
        helm-ff-auto-update-initial-value t
        helm-display-buffer-width 96
        helm-display-buffer-default-height 17
        helm-split-window-inside-p t
        ;; When calling `helm-semantic-or-imenu', don't immediately jump to
        ;; symbol at point
        helm-imenu-execute-action-at-once-if-one nil)
  (when (featurep! :editor evil +everywhere)
    (setq helm-default-prompt-display-function #'+helm--set-prompt-display))

  :init
  (let ((fuzzy (featurep! +fuzzy)))
    (setq helm-apropos-fuzzy-match fuzzy
          helm-bookmark-show-location fuzzy
          helm-buffers-fuzzy-matching fuzzy
          helm-ff-fuzzy-matching fuzzy
          helm-file-cache-fuzzy-match fuzzy
          helm-flx-for-helm-locate fuzzy
          helm-imenu-fuzzy-match fuzzy
          helm-lisp-fuzzy-completion fuzzy
          helm-locate-fuzzy-match fuzzy
          helm-projectile-fuzzy-match fuzzy
          helm-recentf-fuzzy-match fuzzy
          helm-semantic-fuzzy-match fuzzy)
    ;; Make sure that we have helm-multi-matching or fuzzy matching,
    ;; (as prescribed by the fuzzy flag) also in the following cases:
    ;; - helmized commands that use `completion-at-point' and similar functions
    ;; - native commands that fall back to `completion-styles' like `helm-M-x'
    (push (if EMACS27+
              (if fuzzy 'flex 'helm)
            (if fuzzy 'helm-flex 'helm))
          completion-styles))

  :config
  ;; Hide the modeline in helm windows as it serves little purpose.
  ;; (defun +helm--hide-mode-line (&rest _)
  ;;   (with-current-buffer (helm-buffer-get)
  ;;     (unless helm-mode-line-string
  ;;       (hide-mode-line-mode +1))))
  ;; (add-hook 'helm-after-initialize-hook #'+helm--hide-mode-line)
  ;; (advice-add #'helm-display-mode-line :override #'+helm--hide-mode-line)
  ;; (advice-add #'helm-ag-show-status-default-mode-line :override #'ignore)

  ;; Hide minibuffer if `helm-echo-input-in-header-line'
  (add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

  (when (featurep! +childframe)
    (setq helm-display-function #'+helm--display-buffer-in-posframe-fn)
    (pushnew! helm--frame-default-attributes 'parent-frame 'font))

  ;; Use helpful instead of describe-* to display documentation
  (setq helm-describe-function-function #'helpful-function
        helm-describe-variable-function #'helpful-variable)
  (set-popup-rule! "^\\*helm" :ignore t))


(use-package! helm-flx
  :when (featurep! +fuzzy)
  :hook (helm-mode . helm-flx-mode)
  :config (helm-flx-mode +1))


;;;###package helm-bookmark
(setq helm-bookmark-show-location t)


(after! helm-files
  (setq helm-boring-file-regexp-list
        (append (list "\\.projects$" "\\.DS_Store$")
                helm-boring-file-regexp-list)))


(defvar helm-generic-files-map (make-sparse-keymap))
(after! helm-locate
  (when (and IS-MAC
             (null helm-locate-command)
             (executable-find "mdfind"))
    (setq helm-locate-command "mdfind %s -name %s"))
  (set-keymap-parent helm-generic-files-map helm-map))


(use-package! helm-org
  :when (featurep! :lang org)
  :defer t
  :init
  (after! helm-mode
    (pushnew! helm-completing-read-handlers-alist
              '(org-capture . helm-org-completing-read-tags)
              '(org-set-tags . helm-org-completing-read-tags)))
  :config
  (advice-remove 'org-insert-link '+popup--helm-hide-org-links-popup-a))


(use-package! helm-projectile
  :commands (helm-projectile-find-file
             helm-projectile-recentf
             helm-projectile-switch-project
             helm-projectile-switch-to-buffer)
  :init
  (setq projectile-completion-system 'helm)
  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  :config
  (set-keymap-parent helm-projectile-find-file-map helm-map))


(use-package! helm-swoop
  :bind (([remap swiper] . #'helm-swoop)
         ([remap swiper-isearch] . #'helm-multi-swoop))
  :config
  (map! :map isearch-mode-map
        "M-i" 'helm-swoop-from-isearch
        :map helm-swoop-map
        "M-i" 'helm-multi-swoop-all-from-helm-swoop)
  (setq helm-swoop-speed-or-color t))


(use-package! helm-descbinds
  :hook (helm-mode . helm-descbinds-mode)
  :config
  (setq helm-descbinds-window-style 'split-window))


(use-package! helm-ag
  :commands (helm-ag helm-do-ag)
  :config
  (setq helm-ag-edit-save nil))
