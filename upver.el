;;; upver.el --- Update your dependencies interactively  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; TODO: Upgrade all to wanted
;; TODO: Upgrade all to latest
;; TODO: Generalize upgrade to latest and wanted functions
;; FIXME: After using upgrade-to functions, nodes gets
;; outdated. Instead of putting node as overlay property, put node's
;; range (this might be bad because range might change after the
;; upgrade) OR simply re-run function from cache
;; TODO: define minor mode with keybindings
;; TODO: Cache values per buffer

(require 's)
(require 'treesit)
(require 'dash)

(defun upver--yarn-outdated (cb)
  "Run \"yarn outdated --json\" and call CB with it's parsed output."
  (let ((process-environment (cons "NODE_NO_WARNINGS=1" process-environment))
        (output-buf (generate-new-buffer "*upver-yarn-outdated*")))
    (set-process-sentinel
     (start-process "*upver-yarn*" output-buf "yarn" "outdated" "--json")
     (lambda (_proc _event)
       (funcall
        cb
        (with-current-buffer output-buf
          (json-parse-string
           (nth 1 (s-split-up-to "\n" (buffer-substring-no-properties (point-min) (point-max)) 1))
           :object-type 'alist :array-type 'list)))
       (kill-buffer output-buf)))))

(defun upver--find-value-node-with-key (root key)
  "Find JSON value node with given KEY, under ROOT."
  (treesit-node-child-by-field-name
   (--find
    (equal (format "\"%s\"" key) (treesit-node-text (treesit-node-child-by-field-name it "key")))
    (treesit-node-children root "pair"))
   "value"))

(defun upver--find-package-node (where package-name)
  "Find PACKAGE-NAMEs node.
WHERE is either \"devDependencies\" or \"dependencies\"."
  (upver--find-value-node-with-key
   (upver--find-value-node-with-key
    (treesit-node-child (treesit-buffer-root-node) 0)
    where)
   package-name))

(defun upver--overlay-at (position)
  "Return `upver' overlay at POSITION, or nil if none to be found."
  (let ((overlays (overlays-at position))
        ov found)
    (while (and (not found) (setq ov (car overlays)))
      (setq found (and (overlay-get ov 'upver) ov)
            overlays (cdr overlays)))
    found))


;; Public

(defun upver-init ()
  (interactive)
  (unless (equal (file-name-nondirectory (buffer-file-name)) "package.json")
    (user-error "Not in a package.json buffer!"))
  (message "upver: Getting updates...")
  (upver--yarn-outdated
   (lambda (updates)
     ;; TODO: remove only our overlays, see upver--overlay-at
     (remove-overlays)
     (let ((data (let-alist updates
                   (--map (list
                           :package (nth 0 it)
                           :path (nth 4 it)
                           :wanted (nth 2 it)
                           :latest (nth 3 it))
                          .data.body))))
       (--each data
         (when-let (node
                    (treesit-node-parent
                     (upver--find-package-node
                      (plist-get it :path)
                      (plist-get it :package))))
           (let* ((ov (make-overlay (treesit-node-start node)
                                    (treesit-node-end node)))
                  (wanted-latest? (equal (plist-get it :wanted)
                                         (plist-get it :latest))))
             (overlay-put ov 'upver t)
             (overlay-put ov 'upver-node node)
             (overlay-put ov 'upver-data it)
             (overlay-put
              ov 'after-string
              (format
               " %s → %s%s"
               (propertize "⇒ :wanted" 'face
                           `(:foreground "grey"))
               (propertize (plist-get it :wanted) 'face
                           `(:foreground ,(if wanted-latest? "green" "yellow")))
               (if (not wanted-latest?)
                   (format " | %s %s"
                           (propertize ":latest" 'face '(:foreground "grey"))
                           (propertize (plist-get it :latest) 'face '(:foreground "green")))
                 ""))))))
       (message "upver: Getting updates...Done.")))))


(defun upver-upgrade-to-wanted ()
  (interactive)
  (when-let* ((ov (upver--overlay-at (point)))
              (data (overlay-get ov 'upver-data))
              (node (overlay-get ov 'upver-node))
              (value-node (treesit-node-child-by-field-name node "value")))
    (save-excursion
      (goto-char (treesit-node-start value-node))
      (delete-region (treesit-node-start value-node) (treesit-node-end value-node))
      (insert "\"" (plist-get data :wanted) "\""))
    (delete-overlay ov)))


(provide 'upver)
;;; upver.el ends here
