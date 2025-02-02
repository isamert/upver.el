;;; upver.el --- Update your dependencies interactively  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 0.1.2
;; Homepage: https://github.com/isamert/upver.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1") (s "1.13.0") (dash "2.19.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Easily upgrade your dependencies inside Emacs for your
;; npm/Yarn/Cargo projects.  Basic workflow is as follows:
;;
;; - Do `upver'.  You'll get possible updates right next/below to your
;;   dependencies.
;; - Use \\[upver-wanted] or \\[upver-latest] to update current
;;   dependency, or use one of \\[upver-all-wanted]
;;   \\[upver-all-latest] to upgrade all of the dependencies.
;; - Use \\[upver-next] and \\[upver-prev] to navigate between
;;   upgradable dependencies.
;; - Do `upver-finish' to finalize the process.  Just closes the upver
;;   mode, does nothing special.

;;; Code:

(require 's)
(require 'dash)
(require 'map)
(require 'treesit)

;;;; Customization

(defgroup upver nil
  "Easy way to upgrade dependencies inside Emacs."
  :group 'utils)

(defcustom upver-prefix nil
  "String to show before a version info.
If it's nil, then it is decided automatically based on
`upver-placement' value.  Otherwise it needs to be set to a
string."
  :type '(choice (const :tag "Automatically select" nil)
                 (string :tag "A fixed string to display"))
  :group 'upver)

(defcustom upver-placement 'below
  "Place to display upver versions.
It can be either \\='below or \\='right.  Also see `upver-prefix'."
  :type '(choice (const :value below)
                 (const :value right))
  :group 'upver)

(defcustom upver-auto-next t
  "Whether to jump to next upgradable dependency after acting on current one.
This effects the behavior of \\[upver-wanted] or \\[upver-latest]."
  :type 'boolean
  :group 'upver)

;;;; Variables

(defvar upver--defs (make-hash-table :test 'equal))

(defvar-local upver--def nil)

(defvar-local upver-updates '()
  "Updates of the current buffer.")

(defvar-local upver--pos '()
  "Position info for updates.")

(cl-defun upver--register (name &rest rest)
  "Register a new backend.

NAME is an arbitrary name of the backend.  REST is following:

PRED is a function that checks if this backend is applicable for the
current buffer or not.  Called within the target buffer, with no
parameters.

FETCHER is a function that fetches the updates.  It takes one
parameter, a callback function.  It should call this callback function
when it finishes fetching the updates.  It should call the callback
function with something like:

    ((:package \"<package-name>\"
      :current \"<current-version>\"
      :wanted \"<wanted-version>\"
      :latest \"<latest-version>\")

      ...)

LOCATOR is a function that takes one parameter, package name, and
returns the corresponding treesit node.  Preferably it should return
the node where the package version is shown.  upver places the
overlays on the parent nodes of this returned node.


Here is an example:

    (upver--register
     \"cargo\"
     :pred (lambda ()
             (and (equal (buffer-name) \"Cargo.toml\")
                  (derived-mode-p \\='toml-ts-mode)))
     :fetcher #\\='upver--cargo-outdated
     :locator #\\='upver--cargo-find-package-node)"
  (map-put! upver--defs name rest))

;;;; NPM

(upver--register
 "npm"
 :pred (lambda ()
         (and (equal (buffer-name) "package.json")
              (derived-mode-p 'json-ts-mode)))
 :fetcher #'upver--npm-outdated
 :locator #'upver--npm-find-package-node)

(defun upver--npm-outdated (cb)
  "Run \"npm outdated\" and call CB with it's parsed output."
  (let ((process-environment (cons "NODE_NO_WARNINGS=1" process-environment))
        (output-buf (generate-new-buffer "*upver-npm-outdated*")))
    (set-process-sentinel
     (start-process "*upver-npm-outdated*" output-buf "npm" "outdated" "--json")
     (lambda (_proc _event)
       (funcall
        cb
        (with-current-buffer output-buf
          (--map
           (let-alist (cdr it)
             (list
              :package (car it)
              :current .current
              :wanted .wanted
              :latest .latest))
           (progn
             (goto-char (point-min))
             (json-parse-buffer :object-type 'alist :array-type 'list)))))
       (kill-buffer output-buf)))))

;; TODO: Rewrite this using `treesit-query-capture'
(defun upver--npm-find-package-node (package-name)
  "Find PACKAGE-NAMEs node.
Tries \"dependencies\" first and then \"devDependencies\"."
  (or
   (upver--npm-find-value-node-with-key
    (upver--npm-find-dependencies-node "dependencies")
    package-name)
   (upver--npm-find-value-node-with-key
    (upver--npm-find-dependencies-node "devDependencies")
    package-name)))

(defun upver--npm-find-value-node-with-key (root key)
  "Find JSON value node with given KEY, under ROOT."
  (treesit-node-child-by-field-name
   (--find
    (equal (format "\"%s\"" key) (treesit-node-text (treesit-node-child-by-field-name it "key")))
    (treesit-node-children root "pair"))
   "value"))

(defun upver--npm-find-dependencies-node (type)
  "Find dependencies node of TYPE.
TYPE is either \"dependencies\" or \"devDependencies\"."
  (upver--npm-find-value-node-with-key
   (treesit-node-child (treesit-buffer-root-node) 0)
   type))

;;;; Cargo

(upver--register
 "cargo"
 :pred (lambda ()
         (and (equal (buffer-name) "Cargo.toml")
              (derived-mode-p 'toml-ts-mode)))
 :fetcher #'upver--cargo-outdated
 :locator #'upver--cargo-find-package-node)

(defun upver--cargo-outdated (cb)
  "Run \"cargo outdated\" and call CB with it's parsed output."
  (let ((output-buf (generate-new-buffer "*upver-cargo-outdated*")))
    (set-process-sentinel
     (start-process "*upver-cargo-outdated*" output-buf "cargo" "outdated" "--depth=1" "--format=json")
     (lambda (_proc _event)
       (funcall
        cb
        (with-current-buffer output-buf
          (--map
           (let-alist it
             (list
              :package .name
              :current .project
              :wanted (if (equal "---" .compat)
                          .project
                        .compat)
              ;; TODO: Handle "Removed" packages. See the README of cargo outdated
              :latest .latest))
           (alist-get
            'dependencies
            (progn
              (goto-char (point-min))
              (json-parse-buffer :object-type 'alist :array-type 'list))))))
       (kill-buffer output-buf)))))

(defun upver--cargo-find-package-node (package-name)
  "Find PACKAGE-NAMEs node."
  ;; FIXME: Can't find packages with extended definition like:
  ;; textwrap = { version = "0.16.0", default-features = false }
  (alist-get
   'value
   (car
    (-partition
     2
     (treesit-query-capture
      (alist-get 'table (treesit-query-capture
                         (treesit-buffer-root-node)
                         '(((table (bare_key) @key) @table
                            (:match "\\(dev\\)?dependencies" @key)))))
      `(((pair (bare_key) @key (string) @value)
         (:equal @key ,package-name))))))))

;;;; Overlay utils

(defun upver--overlay-at (position)
  "Return `upver' overlay at POSITION, or nil if none to be found."
  (let ((overlays (overlays-at position))
        ov found)
    (while (and (not found) (setq ov (car overlays)))
      (setq found (and (overlay-get ov 'upver) ov)
            overlays (cdr overlays)))
    found))

(defun upver--clear-overlays ()
  (remove-overlays (point-min) (point-max) 'upver t))

;;;; Utils

(defun upver--find-closest-number (num list)
  (car (sort (copy-sequence list)
             (lambda (a b)
               (< (abs (- num a))
                  (abs (- num b)))))))

(defun upver--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;;;; Main

(defvar-keymap upver-dependency-map
  "#" #'upver-wanted
  "^" #'upver-latest
  "p" #'upver-prev
  "n" #'upver-next)

(defun upver--which-key (map fn)
  (key-description (where-is-internal fn map t)))

(defun upver--draw-updates (updates)
  (setq upver-updates updates)
  (upver--clear-overlays)
  (setq upver--pos '())
  (--each updates
    (when-let* ((node (funcall (plist-get upver--def :locator)
                               (plist-get it :package)))
                (parent (treesit-node-parent node)))
      (let* ((start (treesit-node-start parent))
             (end (treesit-node-end parent))
             (ov (make-overlay start end))
             (wanted-current? (equal (plist-get it :wanted)
                                     (plist-get it :current)))
             (wanted-latest? (equal (plist-get it :wanted)
                                    (plist-get it :latest))))
        (push (line-number-at-pos start) upver--pos)
        (overlay-put ov 'upver t)
        (overlay-put ov 'upver-node node)
        (overlay-put ov 'upver-data it)
        ;; TODO: Make faces customizable.
        (overlay-put ov 'keymap upver-dependency-map)
        (overlay-put
         ov 'help-echo
         (lambda (_window _obj _pos)
           (substitute-command-keys
            (format
             (concat
              "\\[upver-wanted] → %s\t\t\\[upver-latest] → %s\n"
              "\\[upver-next] → next\t\t\\[upver-prev] → previous\n"
              "\\[upver-finish] → Done\t\t\\[upver-cancel] → Cancel")
             (plist-get it :wanted)
             (plist-get it :latest)))))
        (overlay-put
         ov 'after-string
         (concat
          (if (eql upver-placement 'right)
              ""
            (concat "\n" (make-string (save-excursion (goto-char start) (current-indentation)) ? )))
          " "
          (or upver-prefix (if (eql upver-placement 'right) "⇒ " "⮑ "))
          (if wanted-current?
              ""
            (propertize ":wanted → " 'face
                        `(:foreground "grey")))
          (if wanted-current?
              ""
            (propertize (plist-get it :wanted) 'face
                        `(:foreground ,(if wanted-latest? "green" "yellow"))))
          (if (not wanted-latest?)
              (concat
               (if wanted-current? "" " | ")
               (propertize ":latest → " 'face '(:foreground "grey"))
               (propertize (plist-get it :latest) 'face '(:foreground "green")))
            ""))))))
  (sort upver--pos #'<))

;;;; Internal Misc.

(defun upver--upgrade-to (type &optional interactive?)
  (let ((inhibit-read-only t))
    (when-let* ((ov (upver--overlay-at (point)))
                (data (overlay-get ov 'upver-data))
                (node (overlay-get ov 'upver-node)))
      (save-excursion
        (goto-char (treesit-node-start node))
        (let* ((beg (treesit-node-start node))
               (end (treesit-node-end node))
               (current (prog1 (buffer-substring-no-properties (1+ beg) (1- end))
                          (delete-region beg end))))
          (insert
           "\""
           (if (s-prefix? "^" current) "^" "")
           (plist-get data type)
           "\"")))
      (upver--draw-updates (--remove (equal it data) upver-updates))
      (when (and upver-auto-next interactive?)
        (ignore-errors
          (upver-next))))))

(defun upver--upgrade-all-to (type)
  (--each upver--pos
    (save-excursion
      (upver--goto-line it)
      (back-to-indentation)
      (upver--upgrade-to type))))

;;;; Upver Mode

(define-minor-mode upver-mode
  "UpVer Mode."
  :lighter " UpVer"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c #") #'upver-wanted)
            (define-key map (kbd "C-c ^") #'upver-latest)
            (define-key map (kbd "C-c C-#") #'upver-all-wanted)
            (define-key map (kbd "C-c C-^") #'upver-all-latest)
            (define-key map (kbd "C-c C-n") #'upver-next)
            (define-key map (kbd "C-c C-p") #'upver-prev)
            (define-key map (kbd "C-c C-c") #'upver-finish)
            (define-key map (kbd "C-c C-k") #'upver-cancel)
            map))

;;;###autoload
(defun upver ()
  "Start an upver session."
  (interactive)
  (let ((def (seq-find
              (lambda (it) (funcall (plist-get it :pred)))
              (map-values upver--defs)))
        (buffer (current-buffer)))
    (unless def
      (unless (bound-and-true-p treesit-primary-parser)
        (warn "`treesit' is not enabled for this buffer.  upver requires a `treesit' based mode to work"))
      (user-error "upver :: Can't recognize \"%s\" buffer with mode `%s'"
                  (buffer-name) major-mode))
    (when (and buffer-file-name (buffer-modified-p))
      (if (y-or-n-p (format "Buffer %s is modified, save it? " (buffer-name)))
          (save-buffer)
        (user-error "upver :: Need to save the buffer first to use upver")))
    (when upver-mode
      (upver-finish))
    (message "upver: Getting updates...")
    (upver-mode +1)
    (read-only-mode)
    (setq-local upver--def def)
    (funcall
     (plist-get def :fetcher)
     (lambda (updates)
       (with-current-buffer buffer
         (upver--draw-updates updates)
         (upver--help-at-point)
         (message "upver: Getting updates...Done")
         (goto-char (point-min))
         (upver-next)
         (ignore-errors
           (recenter)))))))

(defun upver-finish ()
  "Finish the upver session."
  (interactive nil upver-mode)
  (when upver-mode
    (upver--clear-overlays)
    (setq upver-updates '())
    (setq upver--pos '())
    (upver-mode -1)
    (upver--help-at-point-cancel)
    (read-only-mode -1)
    (when (and buffer-file-name (buffer-modified-p))
      (when (y-or-n-p (format "Buffer %s is modified, save it? " (buffer-name)))
        (save-buffer)))
    ;; TODO: Add a finish hook that installs packages?
    (message "upver: Done. You may want to run your package manager to install updated versions")
    (setq-local upver--def nil)))

(defun upver-cancel ()
  "Cancel all edits made by upver and end the upver session."
  (interactive nil upver-mode)
  (when upver-mode
    (upver-finish)
    (message "upver: Cancelled")
    (when (and buffer-file-name (buffer-modified-p))
      (revert-buffer-quick))))

(defun upver-next ()
  "Go to next upgradable dependency."
  (interactive nil upver-mode)
  (let* ((line (line-number-at-pos))
         (closest (upver--find-closest-number line upver--pos)))
    (cond
     ((< line closest) (upver--goto-line closest))
     ((>= line closest)
      (let ((idx (1+ (-elem-index closest upver--pos))))
        (when (< idx (length upver--pos))
          (upver--goto-line (nth idx upver--pos)))))))
  (back-to-indentation))

(defun upver-prev ()
  "Go to previous upgradable dependency."
  (interactive nil upver-mode)
  (let* ((line (line-number-at-pos))
         (closest (upver--find-closest-number line upver--pos)))
    (cond
     ((<= line closest) (upver--goto-line (nth (1- (-elem-index closest upver--pos)) upver--pos)))
     ((> line closest) (upver--goto-line closest))))
  (back-to-indentation))

(defun upver-wanted ()
  "Upgrade current dependency to the wanted value."
  (interactive nil upver-mode)
  (upver--upgrade-to :wanted (called-interactively-p 'interactive)))

(defun upver-latest ()
  "Upgrade current dependency to the latest value."
  (interactive nil upver-mode)
  (upver--upgrade-to :latest (called-interactively-p 'interactive)))

(defun upver-all-wanted ()
  "Upgrade all dependencies to their wanted value."
  (interactive nil upver-mode)
  (upver--upgrade-all-to :wanted))

(defun upver-all-latest ()
  "Upgrade all dependencies to their latest value."
  (interactive nil upver-mode)
  (upver--upgrade-all-to :latest))

;;;; Help at point mode

(defvar upver--help-at-point-timer nil)

(defun upver--show-help ()
  (display-local-help t))

(defun upver--help-at-point ()
  (unless upver--help-at-point-timer
    (setq
     upver--help-at-point-timer
     (run-with-idle-timer
      0 t #'upver--show-help))))

(defun upver--help-at-point-cancel ()
  (when upver--help-at-point-timer
    (cancel-timer upver--help-at-point-timer)
    (setq
     upver--help-at-point-timer
     nil)))

(provide 'upver)
;;; upver.el ends here
