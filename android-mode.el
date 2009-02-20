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
