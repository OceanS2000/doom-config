;; -*- no-byte-compile: t; -*-
;;; input/chinese/packages.el

(unless (featurep! +rime)
  (package! pyim))
