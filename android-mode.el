;;; android-mode.el -- Minor mode for Android application development

;; Copyright (C) 2009 R.W van 't Veer
;;
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

(defgroup android-mode nil
  "A minor mode for Android application development"
  :prefix "android-mode-"
  :group 'applications)

(defcustom android-mode-sdk-dir "~/Android/sdk"
  "Set to the directory containing the Android SDK."
  :type 'string
  :group 'android-mode)

(defvar android-exclusive-processes ())
(defmacro android-exclusive-sentinel (key proc)
  `(when (not (find ,key android-exclusive-processes))
     (set-process-sentinel ,proc
                           (lambda (proc msg)
                             (when (memq (process-status proc) '(exit signal))
                               (setq android-exclusive-processes (delete ,key android-exclusive-processes)))))
     (setq android-exclusive-processes (cons ,key android-exclusive-processes))))

(defun android-start-emulator ()
  "Start emulator."
  (interactive)
  (android-exclusive-sentinel 'emulator (start-process-shell-command "*android-emulator*"
                                                                     "*android-emulator*"
                                                                     (concat android-mode-sdk-dir "/tools/emulator"))))

(defun android-start-logcat ()
  "Start logcat in a separate buffer."
  (interactive)
  (android-exclusive-sentinel 'logcat (start-process-shell-command "*android-logcat*"
                                                                   "*android-logcat*"
                                                                   (concat android-mode-sdk-dir "/tools/adb") "logcat")))

(defun android-root ()
  "Look for AndroidManifest.xml file to find project root of android application."
  (let ((cwd default-directory)
        (found nil)
        (max 10))
    (while (and (not found) (> max 0))
      (if (file-exists-p (concat cwd "AndroidManifest.xml"))
        (setq found cwd)
        (setq cwd (concat cwd "../") max (- max 1))))
    (and found (expand-file-name found))))

(defmacro android-in-root (body)
  `(let ((default-directory (android-root)))
     ,body))

(defun android-compile-reinstall ()
  "Run 'ant reinstall' in the project root directory."
  (interactive)
  (android-in-root
   (compile "ant reinstall")))

(define-minor-mode android-mode
  "Android application development minor mode."
  nil
  " Android"
  '(("\C-c\C-e" . android-start-emulator)
    ("\C-c\C-l" . android-start-logcat)
    ("\C-c\C-x" . android-compile-reinstall)))

(add-hook 'dired-mode-hook (lambda () (when (android-root) (android-mode t))))
(add-hook 'find-file-hooks (lambda () (when (android-root) (android-mode t))))

(provide 'android-mode)
