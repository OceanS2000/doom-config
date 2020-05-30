;; -*- no-byte-compile: t; -*-
;;; completion/helm-ocean/packages.el

(package! helm)
;; (package! helm-rg :pin "785a80fe5cc87e27c5ea3d00a70049028d9e2847")
(package! helm-c-yasnippet)
(package! helm-company)
(package! helm-describe-modes
  :recipe (:host github :repo "emacs-helm/helm-describe-modes"))
(package! helm-projectile)
(when (featurep! +fuzzy)
  (package! helm-flx))
(when (featurep! :lang org)
  (package! helm-org))
(package! helm-descbinds)
(package! helm-swoop)
(package! helm-ag)
