;;; completion/helm-ocean/autoload/helm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +helm/projectile-find-file ()
  "Call `helm-find-files' if called from HOME, otherwise
`helm-projectile-find-file'."
  (interactive)
  (call-interactively
   (if (or (file-equal-p default-directory "~")
           (if-let* ((proot (doom-project-root)))
               (file-equal-p proot "~")
             t))
       #'helm-find-files
     #'helm-projectile-find-file)))

;;;###autoload
(defun +helm/workspace-buffer-list ()
  "A version of `helm-buffers-list' with its buffer list restricted to the
current workspace."
  (interactive)
  (unless (featurep! :ui workspaces)
    (user-error "This command requires the :ui workspaces module"))
  (with-no-warnings
    (with-persp-buffer-list nil (helm-buffers-list))))

;;;###autoload
(defun +helm/workspace-mini ()
  "A version of `helm-mini' with its buffer list restricted to the current
workspace."
  (interactive)
  (unless (featurep! :ui workspaces)
    (user-error "This command requires the :ui workspaces module"))
  (with-no-warnings
    (with-persp-buffer-list nil (helm-mini))))


;;
;;; Project search

;;;###autoload
(cl-defun +helm-file-search (&key query in all-files (recursive t) _prompt args)
  "Conduct a file search using helm-ag.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current
  project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "ag")
    (user-error "Couldn't find ag in your PATH"))
  (let ((this-command 'helm-do-ag)
        (target-dir (or in (doom-project-root) default-directory))
        (helm-ag-command-option
         (delq nil (append (list (when all-files "-a -z --hidden")
                                 (unless recursive "--depth 1"))
                           args))))
    (setq deactivate-mark t)
    (helm-do-ag target-dir
                nil
                (or query
                    (when (use-region-p)
                      (let ((beg (or (bound-and-true-p evil-visual-beginning) (region-beginning)))
                            (end (or (bound-and-true-p evil-visual-end) (region-end))))
                        (when (> (abs (- end beg)) 1)
                          (buffer-substring-no-properties beg end))))
                    nil))))

;;;###autoload
(defun +helm/project-search (&optional arg initial-query directory)
  "Performs a project search from the project root with ripgrep.

ARG (universal argument), include all files, even hidden or compressed ones, in
the search."
  (interactive "P")
  (+helm-file-search
   :query initial-query
   :in directory
   :all-files (and (not (null arg))
                   (listp arg))))

;;;###autoload
(defun +helm/project-search-from-cwd (&optional arg initial-query)
  "Performs a project search recursively from the current directory.

If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+helm-file-search
   :query initial-query
   :in default-directory
   :all-files (and (not (null arg))
                   (listp arg))))

;;;###autoload
(defun +helm--display-buffer-in-posframe-fn (buffer &optional resume)
  "Display helm buffer BUFFER in a separate frame.

Function suitable for `helm-display-function',
`helm-completion-in-region-display-function'
and/or `helm-show-completion-default-display-function'.

See `helm-display-buffer-height' and `helm-display-buffer-width' to
configure frame size.

Note that this feature is available only with emacs-25+."
  (cl-assert (and (fboundp 'window-absolute-pixel-edges)
                  (fboundp 'frame-geometry))
             nil "Helm buffer in own frame is only available starting at emacs-25+")
  (if (not (display-graphic-p))
      ;; Fallback to default when frames are not usable.
      (helm-default-display-buffer buffer)
    (setq helm--buffer-in-new-frame-p t)
    (let* ((relative-pos (pos-visible-in-window-p (window-point) nil t))
           (window-edge (window-body-pixel-edges))
           (pos-x (+ (nth 0 relative-pos) (nth 0 window-edge)))
           (pos-y (+ (nth 1 relative-pos) (nth 1 window-edge)))
           (half-screen-size (/ (display-pixel-height x-display-name) 2))
           (frame-info (frame-geometry))
           (prmt-size (length helm--prompt))
           (line-height (frame-char-height))
           (right-bound (cadr (assq 'outer-size frame-info)))
           tab-bar-mode
           (default-frame-alist
             (if resume
                 (buffer-local-value 'helm--last-frame-parameters
                                     (get-buffer buffer))
               `((font . ,(font-xlfd-name doom-font))
                 (width . ,helm-display-buffer-width)
                 (height . ,helm-display-buffer-height)
                 (tool-bar-lines . 0)
                 (left . ,(max (if (< (+ pos-x (* (frame-char-width) helm-display-buffer-width))
                                      right-bound)
                                   (- pos-x (* (frame-char-width) prmt-size))
                                 (- right-bound (* (frame-char-width)
                                                   (+ 2 helm-display-buffer-width))))
                               0))
                 ;; Try to put frame at the best possible place.
                 ;; Frame should be below point if enough
                 ;; place, otherwise above point and
                 ;; current line should not be hidden
                 ;; by helm frame.
                 (top . ,(if (> pos-y half-screen-size)
                             ;; Above point
                             (- pos-y
                                ;; add 1 lines to make sure there is always a gap
                                (* (+ helm-display-buffer-height 1) line-height))
                           ;; Below point
                           (+ pos-y line-height)))
                 (title . "Helm")
                 (undecorated . ,helm-use-undecorated-frame-option)
                 (background-color . ,(or helm-frame-background-color
                                          (face-attribute 'default :background)))
                 (foreground-color . ,(or helm-frame-foreground-color
                                          (face-attribute 'default :foreground)))
                 (alpha . ,(or helm-frame-alpha 100))
                 (vertical-scroll-bars . nil)
                 (menu-bar-lines . 0)
                 (fullscreen . nil)
                 (visibility . ,(null helm-display-buffer-reuse-frame))
                 (minibuffer . t)
                 (parent-frame . ,(window-frame)))))
           display-buffer-alist)
      ;; Display minibuffer above or below only in initial session,
      ;; not on a session triggered by action, this way if user have
      ;; toggled minibuffer and header-line manually she keeps this
      ;; setting in next action.
      (unless (or helm--executing-helm-action resume)
        ;; Add the hook inconditionally, if
        ;; helm-echo-input-in-header-line is nil helm-hide-minibuffer-maybe
        ;; will have anyway no effect so no need to remove the hook.
        (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
        (with-helm-buffer
          (setq-local helm-echo-input-in-header-line
                      (not (> pos-y half-screen-size)))))
      (helm-display-buffer-popup-frame buffer default-frame-alist)
      ;; When frame size have been modified manually by user restore
      ;; it to default value unless resuming or not using
      ;; `helm-display-buffer-reuse-frame'.
      ;; This have to be done AFTER raising the frame otherwise
      ;; minibuffer visibility is lost until next session.
      (unless (or resume (not helm-display-buffer-reuse-frame))
        (set-frame-size helm-popup-frame
                        helm-display-buffer-width
                        helm-display-buffer-height)))
    (helm-log-run-hook 'helm-window-configuration-hook)))

;;; Unicode

(defun +helm--unicode-names ()
  "Return sorted (DISPLAY . ACTUAL) of function `ucs-names'.
The result of function `ucs-names' is mostly, but not completely,
sorted, so this function ensures lexicographic order."
  (let* (cands
         (table (ucs-names))            ; Either hash map or alist
         (fmt (lambda (name code)       ; Common format function
                (let ((cand (format "%06X %-58s %c" code name code)))
                  (push `(,cand . ,code) cands))))
         (sortp (lambda (a b)
                  (string-lessp (car a) (car b)))))
    (if (not (hash-table-p table))
        ;; Support `ucs-names' returning an alist in Emacs < 26.
        ;; The result of `ucs-names' comes pre-reversed so no need to repeat.
        (dolist (entry table)
          (funcall fmt (car entry) (cdr entry)))
      (maphash fmt table)
      ;; Reverse to speed up sorting
      (setq cands (nreverse cands)))
    (sort cands sortp)))

(defvar +helm--unicode-source
  (helm-build-sync-source "Unicode Characters"
    :candidates (+helm--unicode-names)
    :action (helm-make-actions
             "Insert" (lambda (char)
                        (let ((copy (cond
                                     ((listp helm-current-prefix-arg)
                                      (car helm-current-prefix-arg))
                                     (helm-current-prefix-arg))))
                          (insert-char char copy)))
             "Insert code" (lambda (code)
                             (insert (format "%X" code))))
    :keymap helm-M-x-map)
  "Source for `+helm/unicode'")

;;;###autoload
(defun +helm/unicode ()
  "Insert unicode char with helm"
  (interactive)
  (let ((helm--mode-line-display-prefarg t))
    (helm :sources '(+helm--unicode-source)
          :prompt "Character name: "
          :buffer "*helm unicode characters*")
    (setq helm--mode-line-display-prefarg nil)))
