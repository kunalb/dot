;;; init.el
; Load org config file from both symlinked parent folder and actual parent folder
(setq init-file-name (or (buffer-file-name) load-file-name))
(setq dotfiles-dirs `(,(file-name-directory (file-truename init-file-name))
                      ,(file-name-directory init-file-name)))
(mapc #'org-babel-load-file
      (apply #'append
             (mapcar
              (lambda (dotfiles-dir)
                (directory-files dotfiles-dir t "^[^#]*\\.org$"))
              dotfiles-dirs)))
