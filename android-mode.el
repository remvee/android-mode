;;; android-mode.el --- Minor mode for Android application development

;; Copyright (C) 2009-2012 R.W van 't Veer

;; Author: R.W. van 't Veer
;; Created: 20 Feb 2009
;; Keywords: tools processes
;; Version: 0.2.1
;; URL: https://github.com/remvee/android-mode

;; Contributors:
;;   Bert Hartmann
;;   Cristian Esquivias
;;   Donghyun Cho
;;   Jürgen Hötzel

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides support for running Android SDK subprocesses like the
;; emulator, logcat, ddms and ant.  When loaded `dired-mode' and
;; `find-file' hooks are added to automatically enable `android-mode'
;; when opening a file or directory in an android project.


;;; Code:

(eval-when-compile (require 'cl))

(defgroup android-mode nil
  "A minor mode for Android application development"
  :prefix "android-mode-"
  :group 'applications)

(defcustom android-mode-sdk-dir "~/Android/sdk"
  "Set to the directory containing the Android SDK."
  :type 'string
  :group 'android-mode)

(defcustom android-mode-sdk-tool-subdirs '("tools" "platform-tools")
  "List of subdirectors in the SDK containing commandline tools."
  :type '(repeat string)
  :group 'android-mode)

(defcustom android-mode-sdk-tool-extensions '("" ".bat" ".exe")
  "List of possible extensions for commandline tools."
  :type '(repeat string)
  :group 'android-mode)

(defcustom android-mode-key-prefix "\C-c \C-c"
  "Minor mode keys prefix."
  :type 'string
  :group 'android-mode)

(defcustom android-mode-avd ""
  "Default AVD to use."
  :type 'string
  :group 'android-mode)

(defface android-mode-verbose-face '((t (:foreground "DodgerBlue")))
  "Font Lock face used to highlight VERBOSE log records."
  :group 'android-mode)

(defface android-mode-debug-face '((t (:foreground "ForestGreen")))
  "Font Lock face used to highlight DEBUG log records."
  :group 'android-mode)

(defface android-mode-info-face '((t (:foreground "Gray45")))
  "Font Lock face used to highlight INFO log records."
  :group 'android-mode)

(defface android-mode-warning-face '((t (:foreground "Red")))
  "Font Lock face used to highlight WARN log records."
  :group 'android-mode)

(defface android-mode-error-face '((t (:foreground "Red" :bold t)))
  "Font Lock face used to highlight ERROR log records."
  :group 'android-mode)

(defvar android-mode-log-face-alist
  '(("V" . android-mode-verbose-face)
    ("D" . android-mode-debug-face)
    ("I" . android-mode-info-face)
    ("W" . android-mode-warning-face)
    ("E" . android-mode-error-face)))

(defun android-root ()
  "Look for AndroidManifest.xml file to find project root of android application."
  (locate-dominating-file default-directory "AndroidManifest.xml"))

(defmacro android-in-root (body)
  "Execute BODY form with project root directory as
``default-directory''.  The form is not executed when no project
root directory can be found."
  `(let ((android-root-dir (android-root)))
     (when android-root-dir
       (let ((default-directory android-root-dir))
         ,body))))

(defun android-local-sdk-dir ()
  "Try to find android sdk directory through the local.properties
file in the android project base directory.  If local.properties
doesn't exist, it does not contain the sdk-dir property or the
referred directory does not exist, return `android-mode-sdk-dir'
variable."
  (or
   (android-in-root
    (let ((local-properties "local.properties"))
      (and (file-exists-p local-properties)
           (with-temp-buffer
             (insert-file-contents local-properties)
             (goto-char (point-min))
             (and (re-search-forward "^sdk\.dir=\\(.*\\)" nil t)
                  (let ((sdk-dir (match-string 1)))
                    (and (file-exists-p sdk-dir) sdk-dir)))))))
   android-mode-sdk-dir))

(defun android-tool-path (name)
  "Find path to SDK tool. Calls `android-local-sdk-dir' to try to find locally
defined sdk directory. Defaults to `android-mode-sdk-dir'."
  (or (find-if #'file-exists-p
               (apply #'append
                      (mapcar (lambda (path)
                                (mapcar (lambda (ext)
                                          (mapconcat 'identity
                                                     `(,(android-local-sdk-dir)
                                                       ,path ,(concat name ext))
                                                     "/"))
                                        android-mode-sdk-tool-extensions))
                              android-mode-sdk-tool-subdirs)))
      (error "can't find SDK tool: %s" name)))

(defvar android-exclusive-processes ())
(defun android-start-exclusive-command (name command &rest args)
  (and (not (find (intern name) android-exclusive-processes))
       (set-process-sentinel (apply 'start-process-shell-command name name command args)
                             (lambda (proc msg)
                               (when (memq (process-status proc) '(exit signal))
                                 (setq android-exclusive-processes
                                       (delete (intern (process-name proc))
                                               android-exclusive-processes)))))
       (setq android-exclusive-processes (cons (intern name)
                                               android-exclusive-processes))))

(defun android-create-project (path package activity)
  "Create new Android project with SDK app"
  (interactive "FPath: \nMPackage: \nMActivity: ")
  (let* ((target (completing-read "Target: " (android-list-targets)))
         (expanded-path (expand-file-name path))
         (command (format "%s create project --path %S --package %s --activity %s --target %S"
                          (android-tool-path "android")
                          expanded-path package activity target))
         (output (shell-command-to-string command)))
    (if (string-equal "Error" (substring output 0 5))
        (error output)
      (find-file expanded-path))))

(defun android-list-targets ()
  "List Android SDKs installed on local machine."
  (let* ((command (concat (android-tool-path "android") " list target"))
         (output (shell-command-to-string command))
         (result nil)
         (offset 0))
    (while (string-match "id: [[:digit:]]+ or \"\\(.*\\)\"" output offset)
      (setq result (cons (match-string 1 output) result))
      (setq offset (match-end 0)))
    (if result
        (reverse result)
      (error "no Android Targets found"))))

(defun android-list-avd ()
  "List of Android Virtual Devices installed on local machine."
  (let* ((command (concat (android-tool-path "android") " list avd"))
         (output (shell-command-to-string command))
         (result nil)
         (offset 0))
    (while (string-match "Name: \\(.*\\)" output offset)
      (setq result (cons (match-string 1 output) result))
      (setq offset (match-end 0)))
    (if result
        (reverse result)
      (error "no Android Virtual Devices found"))))

(defun android-start-emulator ()
  "Launch Android emulator."
  (interactive)
  (let ((avd (or (and (not (string= android-mode-avd "")) android-mode-avd)
                 (completing-read "Android Virtual Device: " (android-list-avd)))))
    (unless (android-start-exclusive-command (concat "*android-emulator-" avd "*")
                                             (concat (android-tool-path "emulator") " -avd " avd))
      (message (concat "emulator " avd " already running")))))

(defun android-start-ddms ()
  "Launch Dalvik Debug Monitor Service tool."
  (interactive)
  (unless (android-start-exclusive-command "*android-ddms*" (android-tool-path "ddms"))
    (message "ddms already running")))

(defcustom android-logcat-buffer "*android-logcat*"
  "Name for the buffer where logcat output goes."
  :type 'string
  :group 'android-mode)

(defun android-logcat-find-file ()
  (interactive)
  (let ((filename (get-text-property (point) 'filename))
        (linenr (get-text-property (point) 'linenr)))
    (when filename
      (find-file (concat (android-root) "/src/" filename))
      (goto-char (point-min))
      (forward-line (1- linenr)))))

(defun android-logcat-find-file-mouse (event)
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (android-logcat-find-file))))

(defvar android-logcat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'android-logcat-find-file)
    (define-key map [mouse-2] 'android-logcat-find-file-mouse)
    map))

(defun android-logcat-prepare-msg (msg)
  (if (string-match "\\bat \\(.+\\)\\.\\([^.]+\\)\\.\\([^.]+\\)(\\(.+\\):\\([0-9]+\\))" msg)
      (let* ((package (match-string 1 msg))
             (class (match-string 2 msg))
             (method (match-string 3 msg))
             (filename (concat (replace-regexp-in-string "\\." "/" package) "/" (match-string 4 msg)))
             (linenr (match-string 5 msg)))
        (if (file-exists-p (concat (android-root) "/src/" filename))
            (propertize msg
                        'face 'underline
                        'mouse-face 'highlight
                        'filename filename
                        'linenr (string-to-number linenr)
                        'follow-link t)
          msg))
    msg))

(defvar android-logcat-pending-output "")

(defun android-logcat-process-filter (process output)
  "Process filter for displaying logcat output."
  (with-current-buffer android-logcat-buffer
    (let ((following (= (point-max) (point)))
          (buffer-read-only nil)
          (pos 0)
          (output (concat android-logcat-pending-output
                          (replace-regexp-in-string "" "" output))))
      (save-excursion
        (while (string-match "\n" output pos)
          (let ((line (substring output pos (match-beginning 0))))
            (setq pos (match-end 0))
            (goto-char (point-max))
            (if (string-match "^\\(.\\)/\\(.*\\)( *\\([0-9]+\\)): \\(.*\\)$" line)
                (let* ((level (match-string 1 line))
                       (level-face (cdr (or (assoc level android-mode-log-face-alist)
                                            (assoc "I" android-mode-log-face-alist))))
                       (tag (replace-regexp-in-string " *$" "" (match-string 2 line)))
                       (pid (match-string 3 line))
                       (msg (match-string 4 line)))
                  (insert (propertize level
                                      'font-lock-face level-face))
                  (tab-to-tab-stop)
                  (insert (propertize tag
                                      'font-lock-face 'font-lock-function-name-face))
                  (insert (propertize (concat "("  pid ")")
                                      'font-lock-face 'font-lock-constant-face))
                  (tab-to-tab-stop)
                  (insert (android-logcat-prepare-msg (propertize msg 'font-lock-face level-face))))
              (insert (propertize line
                                  'font-lock-face 'font-lock-warning-face)))
            (insert "\n")))
        (setq android-logcat-pending-output (substring output pos)))
      (when following (goto-char (point-max))))))

(defun android-logcat ()
  "Switch to ADB logcat buffer, create it when it doesn't exists yet."
  (interactive)
  (when (android-start-exclusive-command android-logcat-buffer
                                         (android-tool-path "adb")
                                         "logcat")
    (set-process-filter (get-buffer-process android-logcat-buffer)
                        #'android-logcat-process-filter)
    (with-current-buffer android-logcat-buffer
      (setq buffer-read-only t)
      (set (make-local-variable 'tab-stop-list) '(2 30))
      (use-local-map android-logcat-map)
      (font-lock-mode t)
      (android-mode t)))
  (switch-to-buffer android-logcat-buffer)
  (goto-char (point-max)))

(defun android-current-buffer-class-name ()
  "Try to determine the full qualified class name defined in the
current buffer."
  (save-excursion
    (cond ((string-match "\\.java$" buffer-file-name)
           (goto-char (point-min))
           (let* ((case-fold-search nil)
                  (package (and (search-forward-regexp "^[ \t]*package[ \t]+\\([a-z0-9_.]+\\)" nil t)
                                     (match-string-no-properties 1)))
                  (class (and (search-forward-regexp "\\bpublic[ \t]+class[ \t]+\\([A-Za-z0-9]+\\)" nil t)
                                   (match-string-no-properties 1))))
             (cond ((and package class) (concat package "." class))
                   (class class)))))))

(defun android-project-package ()
  "Return the package of the Android project"
  (android-in-root
   (let ((root (car (xml-parse-file "AndroidManifest.xml"))))
     (xml-get-attribute root 'package))))

(defun android-project-main-activities (&optional category)
  "Return list of main activity class names as found in the
manifest.  The names returned are fully qualified class names.
Names starting with a period or a capital letter are prepended by
the project package name.

Filter on CATEGORY intent when supplied."
  (android-in-root
   (flet ((first-xml-child (parent name)
                           (car (xml-get-children parent name)))
          (action-main-p (activity)
                         (let ((el (first-xml-child (first-xml-child activity
                                                                     'intent-filter)
                                                'action)))
                           (equal "android.intent.action.MAIN"
                                  (xml-get-attribute el 'android:name))))
          (category-p (activity)
                      (let ((el (first-xml-child (first-xml-child activity
                                                                  'intent-filter)
                                                 'category)))
                        (equal (concat "android.intent.category." category)
                               (xml-get-attribute el 'android:name)))))
     (let* ((root (car (xml-parse-file "AndroidManifest.xml")))
            (package (xml-get-attribute root 'package))
            (application (first-xml-child root 'application)))
       (mapcar (lambda (activity)
                 (let ((name (xml-get-attribute activity 'android:name)))
                   (cond ((string-match "^\\." name)   (concat package name))
                         ((string-match "^[A-Z]" name) (concat package "." name))
                         (t name))))
               (member-if (lambda (activity)
                            (and (action-main-p activity)
                                 (or (not category) (category-p activity))))
                          (xml-get-children application 'activity)))))))

(defun android-start-app ()
  "Start activity in the running emulator.  When the current
buffer holds an activity class specified in the manifest as a
main action intent is will be run.  Otherwise start the first
activity in the 'launcher' category."
  (interactive)
  (let* ((package (android-project-package))
         (current (android-current-buffer-class-name))
         (activity (if (member current
                               (android-project-main-activities))
                       current
                     (car (android-project-main-activities "LAUNCHER"))))
         (command (concat (android-tool-path "adb")
                          " shell am start -n "
                          (concat package "/" activity))))
    (unless activity (error "no main activity found in manifest"))
    (message "Starting activity: %s" activity)
    (let ((output (shell-command-to-string command)))
      (when (string-match "^Error: " output)
        (error (concat command "\n" output))))))

(defun android-ant (task)
  "Run ant TASK in the project root directory."
  (interactive "sTask: ")
  (android-in-root
   (compile (concat "ant -e " task))))

(defmacro android-defun-ant-task (task)
  `(defun ,(intern (concat "android-ant-"
                           (replace-regexp-in-string "[[:space:]]" "-" task)))
     ()
     ,(concat "Run 'ant " task "' in the project root directory.")
     (interactive)
     (android-ant ,task)))

(android-defun-ant-task "clean")
(android-defun-ant-task "debug")
(android-defun-ant-task "installd")
(android-defun-ant-task "uninstall")

(defconst android-mode-keys
  '(("d" . android-start-ddms)
    ("e" . android-start-emulator)
    ("l" . android-logcat)
    ("C" . android-ant-clean)
    ("c" . android-ant-debug)
    ("i" . android-ant-installd)
    ("r" . android-ant-reinstall)
    ("u" . android-ant-uninstall)
    ("a" . android-start-app)))

(defvar android-mode-map (make-sparse-keymap))
(add-hook 'android-mode-hook
          (lambda ()
            (dolist (spec android-mode-keys)
              (define-key
                android-mode-map
                (read-kbd-macro (concat android-mode-key-prefix " " (car spec)))
                (cdr spec)))))

;;;###autoload
(define-minor-mode android-mode
  "Android application development minor mode."
  nil
  " Android"
  android-mode-map)

(add-hook 'dired-mode-hook (lambda () (when (android-root) (android-mode t))))
(add-hook 'find-file-hook (lambda () (when (android-root) (android-mode t))))

(provide 'android-mode)

;;; android-mode.el ends here
