;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (package! minions) ;; minor mode menu

;; Youtube is almost useless and it hangs Emacs while operation.
(package! org-yt :disable t)

;; The pinned version of `user-package' won't work
(unpin! use-package)

(package! osx-trash)

;; pyim setup
(package! pyim)
(package! posframe)

(package! telega :recipe (:no-byte-compile t))
(if (featurep! :complection helm)
    (package! helm-telega
      :recipe (:type git :host github :repo "telega-user/helm-telega")))

;; Through the threating documentation, we live in the bleeding edge of EMACS 28
;; anyway
(unpin! t)

;; emacsmirror provides too old project.el!
(package! project :built-in 'prefer)
(package! seq :built-in 'prefer)
(package! let-alist :built-in 'prefer)
