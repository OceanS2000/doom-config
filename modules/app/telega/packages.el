;; -*- no-byte-compile: t; -*-
;;; app/telega/packages.el

(package! telega :recipe (:no-byte-compile t))
(when (featurep! :completion helm)
  (package! helm-telega
    :recipe (:type git :host github :repo "telega-user/helm-telega")))
