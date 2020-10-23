;;; input/chinese/config.el -*- lexical-binding: t; -*-

(use-package! pyim
  :demand t
  :config
  ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))

  (load! "+daniu")

  (setq default-input-method "pyim"
        pyim-dcache-directory (expand-file-name "pyim/dcache" doom-cache-dir)
        pyim-default-scheme 'daniu-shuangpin)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template
                  pyim-probe-evil-normal-mode))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  (pyim-isearch-mode 1)

  (if (featurep! +childframe)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'minibuffer))

  (setq pyim-page-length 5)

  :bind
  (("M-j" . pyim-convert-string-at-point)))
