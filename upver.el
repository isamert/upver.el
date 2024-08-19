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
;;
;; TODO: Set header-line-format to display shortcuts?

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

(defcustom upver-auto-next t
  "Whether to jump to next upgradable dependency after acting on current one.
This effects the behavior of \\[upver-wanted] or \\[upver-latest]."
  :type 'boolean
  :group 'upver)

(defcustom upver-maven-display-dependency-updates-default-arguments
  '("-DprocessDependencyManagement=false"
    "-DprocessPluginDependenciesInPluginManagement=false")
  "Default arguments passed to `versions:display-dependency-updates' mvn subcommand."
  :type 'list
  :group 'upver-mvn)

(defcustom upver-maven-display-dependency-updates-latest-arguments
  nil
  "Arguments passed to `versions:display-dependency-updates' mvn subcommand for obtaining latest versions."
  :type 'list
  :group 'upver-mvn)

(defcustom upver-maven-display-dependency-updates-wanted-arguments
  '("-DallowMajorUpdates=false")
  "Arguments passed to `versions:display-dependency-updates' mvn subcommand for obtaining wanted versions."
  :type 'list
  :group 'upver-mvn)


;;;; Version info providers

;;;;; npm

(defun upver--npm-outdated (cb)
  "Run \"npm outdated --json\" and call CB with it's parsed output."
  (unless (derived-mode-p 'json-ts-mode)
    (user-error "This only works with treesit.  Try enabling `json-ts-mode' first"))
  (let ((process-environment (cons "NODE_NO_WARNINGS=1" process-environment))
        (output-buf (generate-new-buffer "*upver-npm-outdated*")))
    (set-process-sentinel
     (start-process " *upver-npm-outdated*" output-buf "npm" "outdated" "--json")
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

;;;;; maven

(cl-defun upver--run-mvn (&key args update-snapshots offline callback)
  (let ((output-buf (generate-new-buffer "*upver-mvn-outdated*")))
    (set-process-sentinel
     (apply #'start-process " *upver-mvn-outdated*" output-buf
            `("mvn" "-Dstyle.color=never"
              ,@(when offline '("--offline"))
              ,@(when update-snapshots '("--update-snapshots"))
              "versions:display-dependency-updates"
              "-Dversions.outputLineWidth=180"
              ,@upver-maven-display-dependency-updates-default-arguments
              ,@args))
     (lambda (_proc _event)
       (funcall
        callback
        (prog1 (with-current-buffer output-buf
                 (buffer-substring-no-properties (point-min) (point-max)))
          (kill-buffer output-buf)))))))


(defun upver--parse-mvn (version-variable output)
  (->>
   output
   (s-lines)
   (--filter (s-contains? " -> " it))
   (--map (s-split " " it t))
   (--map (list
           :package (nth 1 it)
           :current (nth 3 it)
           version-variable (nth 5 it)))))


(defun upver--mvn-outdated (cb)
  (upver--run-mvn
   :update-snapshots t
   :args upver-maven-display-dependency-updates-latest-arguments
   :callback
   (lambda (output)
     (let ((latests (upver--parse-mvn :latest output)))
       (upver--run-mvn
        :args upver-maven-display-dependency-updates-wanted-arguments
        :offline t ;; Second run can be offline, to speed things up
        :callback
        (lambda (output)
          (let ((wanteds (upver--parse-mvn :wanted output))
                result)
            (dolist (latest latests)
              (push
               (map-merge
                'plist
                (seq-find (lambda (wanted)
                            (equal (plist-get wanted :package)
                                   (plist-get latest :package)))
                          wanteds)
                latest)
               result))
            (funcall cb result))))))))

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
        (overlay-put ov 'keymap upver-dependency-map)
        (overlay-put
         ov 'help-echo
         (lambda (_window _obj _pos)
           (substitute-command-keys
	    (format
             (concat
              "\\[upver-wanted] → %s\t\t\\[upver-latest] → %s\n"
              "\\[upver-next] → next\t\t\\[upver-prev] → previous\n"
              "\\[upver-finish] → Done")
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
  (read-only-mode)
  (let ((buffer (current-buffer)))
    (upver--npm-outdated
     (lambda (updates)
       (with-current-buffer buffer
         (upver--draw-updates updates)
         (upver--help-at-point)
         (message "upver: Getting updates...Done.")
         (goto-char (point-min))
         (upver-next)
         (recenter))))))

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
    ;; TODO: Add a finish hook that installs packages?
    (message "upver: Done. You may want to run your package manager to install updated versions.")))

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

;;; help at point mode

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
