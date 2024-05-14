;;; upver.el --- Update your dependencies interactively  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 0.0.1
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

;; Easily upgrade your dependencies inside Emacs for your NPM/YARN
;; projects (other package managers are planned).  Basic workflow is
;; as follows:
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
;;
;; Manually editing the buffer during an upver session might create
;; some visual inconsistencies.  I hope to fix this but it's not a
;; priority.  If you stick with the functions above, everything will
;; be fine.
;;
;; TODO: Support other package managers other than npm, like (in order
;; of importance to me): cargo, maven, gradle.

;;; Code:

(require 's)
(require 'dash)
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

;;;; Version info providers

(defun upver--npm-outdated (cb)
  "Run \"npm outdated --json\" and call CB with it's parsed output."
  (unless (derived-mode-p 'json-ts-mode)
    (user-error "This only works with treesit.  Try enabling `json-ts-mode' first"))
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

;;;; Treesit utils

(defun upver--find-value-node-with-key (root key)
  "Find JSON value node with given KEY, under ROOT."
  (treesit-node-child-by-field-name
   (--find
    (equal (format "\"%s\"" key) (treesit-node-text (treesit-node-child-by-field-name it "key")))
    (treesit-node-children root "pair"))
   "value"))

(defun upver--find-dependencies-node (type)
  "Find dependencies node of TYPE.
TYPE is either \"dependencies\" or \"devDependencies\"."
  (upver--find-value-node-with-key
   (treesit-node-child (treesit-buffer-root-node) 0)
   type))

(defun upver--find-package-node (package-name)
  "Find PACKAGE-NAMEs node.
Tries \"dependencies\" first and then \"devDependencies\"."
  (or
   (upver--find-value-node-with-key
    (upver--find-dependencies-node "dependencies")
    package-name)
   (upver--find-value-node-with-key
    (upver--find-dependencies-node "devDependencies")
    package-name)))

;;;; Overlay utils

(defun upver--overlay-at (position)
  "Return `upver' overlay at POSITION, or nil if none to be found."
  (let ((overlays (overlays-at position))
        ov found)
    (while (and (not found) (setq ov (car overlays)))
      (setq found (and (overlay-get ov 'upver) ov)
            overlays (cdr overlays)))
    found))

;; TODO: remove only our overlays, see upver--overlay-at
(defun upver--clear-overlays ()
  (remove-overlays))

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

(defvar-local upver-updates '()
  "Updates of the current buffer.")

(defvar-local upver--pos '()
  "Position info for updates.")

(defun upver--draw-updates (updates)
  (setq upver-updates updates)
  (upver--clear-overlays)
  (setq upver--pos '())
  (--each updates
    (when-let (node
               (treesit-node-parent
                (upver--find-package-node
                 (plist-get it :package))))
      (let* ((start (treesit-node-start node))
             (end (treesit-node-end node))
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
        (overlay-put
         ov 'after-string
         (concat
          (if (eql upver-placement 'right)
              ""
            (concat "\n" (make-string (current-indentation) ? )))
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
  (sort upver--pos #'<)
  (message "upver: Getting updates...Done."))

;;;; Internal Misc.

(defun upver--upgrade-to (type)
  (when-let* ((ov (upver--overlay-at (point)))
              (data (overlay-get ov 'upver-data))
              (node (overlay-get ov 'upver-node))
              (value-node (treesit-node-child-by-field-name node "value")))
    (save-excursion
      (goto-char (treesit-node-start value-node))
      (let* ((beg (treesit-node-start value-node))
             (end (treesit-node-end value-node))
             (current (prog1 (buffer-substring-no-properties (1+ beg) (1- end))
                        (delete-region beg end))))
        (insert
         "\""
         (if (s-prefix? "^" current) "^" "")
         (plist-get data type)
         "\"")))
    (upver--draw-updates (--remove (equal it data) upver-updates))
    (when (and upver-auto-next interactive?)
      (upver-next))))

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
            map))

(defun upver ()
  "Start an upver session."
  (interactive)
  (unless (equal (file-name-nondirectory (buffer-file-name)) "package.json")
    (user-error "Not in a package.json buffer!"))
  (when upver-mode
    (upver-finish))
  (message "upver: Getting updates...")
  (upver-mode +1)
  (let ((buffer (current-buffer)))
    (upver--npm-outdated (lambda (updates)
                           (with-current-buffer buffer
                             (upver--draw-updates updates))))))

(defun upver-finish ()
  "Finish the upver session."
  (interactive nil upver-mode)
  (when upver-mode
    (upver--clear-overlays)
    (setq upver-updates '())
    (setq upver--pos '())
    (upver-mode -1)
    ;; TODO: Add a finish hook that installs packages?
    (message "upver: Done. You may want to run your package manager to install updated versions.")))

(defun upver-next ()
  "Go to next upgradable dependency."
  (interactive nil upver-mode)
  (let* ((line (line-number-at-pos))
         (closest (upver--find-closest-number line upver--pos)))
    (cond
     ((< line closest) (upver--goto-line closest))
     ((>= line closest) (upver--goto-line (nth (1+ (-elem-index closest upver--pos)) upver--pos)))))
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

(provide 'upver)
;;; upver.el ends here
