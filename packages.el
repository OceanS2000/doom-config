;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (package! minions) ;; minor mode menu

;; Youtube is almost useless and it hangs Emacs while operation.
(package! org-yt :disable t)
(package! swiper :disable t)

;; The pinned version of `user-package' won't work
(unpin! use-package)
(unpin! (:ui doom))

;; Through the threating documentation, we live in the bleeding edge of EMACS 28
;; anyway
(unpin! t)

;; We use metals instead of ENSIME because sadly ENSIME is dead
