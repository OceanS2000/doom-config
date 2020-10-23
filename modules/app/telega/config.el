;;; app/telega/config.el -*- lexical-binding: t; -*-

(use-package! telega
  :commands (telega)
  :custom
  (telega-chat-reply-prompt "<<< ")
  (telega-chat-edit-prompt "+++ ")
  (telega-chat-use-markdown-version 2)
  (telega-emoji-use-images nil)
  (telega-proxies
   (list '(:server "127.0.0.1" :port 6153 :enable t
           :type (:@type "proxyTypeSocks5")))) (
   telega-completing-read-function
   (cond ((featurep! :completion helm)
          ;; `helm-mode' ensures this using helm
          'completing-read)
         ((featurep! :completion ivy)
          'ivy-completing-read)
         (t 'ido-completing-read)))

  :init
  (unless (display-graphic-p) (setq telega-use-images nil))
  (setq telega-directory (expand-file-name "telega" doom-etc-dir)
        telega-cache-dir (expand-file-name "telega/cache" doom-cache-dir)
        telega-temp-dir (expand-file-name "telega/temp" doom-cache-dir))

  :hook
  ('telega-chat-mode . #'yas-minor-mode)
  ('telega-chat-mode . (defun +telega-chat-enable-company-h ()
                         (set-company-backend! 'telega-chat-mode
                           '(telega-company-emoji
                             telega-company-username
                             telega-company-hashtag
                             telega-company-botcmd))
                         (company-mode +1)))
  ('telega-chat-pre-message . #'telega-msg-ignore-blocked-sender)
  :config
  (set-evil-initial-state! '(telega-root-mode telega-chat-mode) 'emacs)

  (set-popup-rule! "^\\*Telega Root"
    :side 'right :size 83 :quit 'current :ttl nil :modeline t)
  (set-popup-rule! "^◀\\(\\[\\|<\\|{\\).*\\(\\]\\|>\\|}\\)"
    :ignore t)

  (defadvice! +telega-mode-line-use-text-icon-a ()
    :override 'telega-mode-line-icon
    (propertize
     (if doom-modeline-icon
         (all-the-icons-fileicon "telegram" :height 1.0 :v-adjust -0.1)
       "Tg")
     ;; 'face 'all-the-icons-green
     'local-map (eval-when-compile
                  (make-mode-line-mouse-map 'mouse-1 'telega))
     'mouse-face 'mode-line-highlight
     'help-echo "Click to show telega root buffer"))
  (telega-mode-line-mode 1)

  (after! all-the-icons
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(telega-root-mode all-the-icons-fileicon "telegram"
                                    :heigt 1.0
                                    :v-adjust -0.2
                                    :face all-the-icons-yellow))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(telega-chat-mode all-the-icons-fileicon "telegram"
                                    :heigt 1.0
                                    :v-adjust -0.2
                                    :face all-the-icons-blue)))

  (after! pyim
    (add-to-list 'pyim-english-input-switch-functions #'+pyim-probe-telega-msg)))

(use-package! helm-telega
  :if (featurep! :completion helm)
  :commands (helm-telega-stickerset-choose
             helm-telega-sticker-mini
             helm-telega-sticker-favourite-or-recent)
  :config
  (setf (alist-get "sticker" telega-chat-attach-commands nil nil #'string=)
        '(nil helm-telega-sticker-mini)))
