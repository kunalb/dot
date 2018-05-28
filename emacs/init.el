;;; init.el
; Load org config file from both symlinked parent folder and actual parent folder

;; Handling GC better
(setq gc-cons-threshold (* 50 1000 1000))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq init-file-name (or (buffer-file-name) load-file-name))
(setq dotfiles-dirs `(,(file-name-directory (file-truename init-file-name))
                      ,(file-name-directory init-file-name)))
(mapc #'org-babel-load-file
      (apply #'append
             (mapcar
              (lambda (dotfiles-dir)
                (directory-files dotfiles-dir t "^[^#]*\\.org$"))
              dotfiles-dirs)))

(setq gc-cons-threshold (* 5 1000 1000))
