;;; app/telega/autoloads.el -*- lexical-binding: t; -*-

;; Pyim probe
;;;###autoload
(defun +pyim-probe-telega-msg ()
  "Return if current point is at a telega button."
  (s-contains? "telega" (symbol-name (get-text-property (point) 'category))))
