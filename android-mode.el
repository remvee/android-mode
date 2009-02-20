(defconst *android-sdk-dir* "~/Development/android/sdk")

(defvar android-emulator-running nil)
(defun android-start-emulator ()
  "Start emulator."
  (interactive)
  (when (not android-emulator-running)
    (set-process-sentinel (start-process-shell-command "*android-emulator*"
                                                       "*android-emulator*"
                                                       (concat *android-sdk-dir* "/tools/emulator"))
                          (lambda (proc msg) (when (memq (process-status proc) '(exit signal))
                                               (setq android-emulator-running nil))))
    (setq android-emulator-running t)))

(defvar android-logcat-running nil)
(defun android-start-logcat ()
  "Start logcat in a separate buffer."
  (interactive)
  (when (not android-logcat-running)
    (set-process-sentinel (start-process-shell-command "*android-logcat*"
                                                       "*android-logcat*"
                                                       (concat *android-sdk-dir* "/tools/adb") "logcat")
                          (lambda (proc msg) (when (memq (process-status proc) '(exit signal))
                                               (setq android-logcat-running nil))))
    (setq android-logcat-running t)))

(defun android-root ()
  "Look for build.xml file to find project root of android application."
  (let ((cwd default-directory)
        (found nil)
        (max 10))
    (while (and (not found) (> max 0))
      (if (file-exists-p (concat cwd "build.xml"))
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

(defvar android-mode nil)
(make-variable-buffer-local 'android-mode)

(defun android-mode (&optional arg)
  "Android minor mode."
  (interactive "P")
  (setq android-mode
        (if (null arg)
          (not android-mode)
          (> (prefix-numeric-value arg) 0)))
  (if android-mode
    (global-set-key [f12] 'android-compile-reinstall)
    (global-unset-key [f12])))

(if (not (assq 'android-mode minor-mode-alist)) 
  (setq minor-mode-alist (cons '(android-mode " Android") minor-mode-alist))) 
