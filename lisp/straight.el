;; -*- lexical-binding: t -*-

;;; Load vendored dependencies from vendor/
;;; No package manager. No network. Just load-path.

(defvar fate-vendor-dir (expand-file-name "vendor/" fate-emacs-dir)
  "Directory containing vendored dependencies.")

(dolist (dir (directory-files fate-vendor-dir t "^[^.]"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    ;; Add subdirs that contain .el files (e.g. corfu/extensions, evil-collection/modes/*)
    (dolist (subname '("extensions" "modes"))
      (let ((sub (expand-file-name subname dir)))
        (when (file-directory-p sub)
          (add-to-list 'load-path sub)
          (dolist (subdir (directory-files sub t "^[^.]"))
            (when (file-directory-p subdir)
              (add-to-list 'load-path subdir))))))))

;; Emacs 31 warns whenever it loads a .el without a `lexical-binding' cookie.
;; evil-easymotion, evil-snipe and yasnippet still ship without one upstream,
;; and vendor/ is never hand-edited -- scripts/vendor.sh would overwrite the
;; fix on the next bump. Every file in lisp/ and lang/ carries the cookie, so
;; inhibiting the warning hides nothing of ours.
(require 'warnings)
(add-to-list 'warning-inhibit-types '(files missing-lexbind-cookie))

;; use-package is built-in since Emacs 29. No :straight, no :ensure needed
;; for vendored packages -- they're already on load-path.
(require 'use-package)

;; CRITICAL: Never try to install anything. No network, no package.el.
(setq use-package-always-ensure nil)
(setq package-enable-at-startup nil)
(setq package-archives nil)
