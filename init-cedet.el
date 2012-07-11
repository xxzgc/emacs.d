(load-file "~/.emacs.d/cedet-1.1beta2/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 

(global-srecode-minor-mode 1)            ; Enable template insertion menu

(require 'semantic-ia)
(require 'semantic-gcc)
(semantic-add-system-include "/usr/local/include" 'c-mode)
(semantic-add-system-include "/usr/include" 'c-mode)

(setq semantic-idle-scheduler-mode t)
(setq semantic-idle-scheduler-idle-time 5)


(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(require 'semanticdb)
(global-semanticdb-minor-mode 1)

;; если вы хотите включить поддержку gnu global
(when (cedet-gnu-global-version-check t)
  (require 'semanticdb-global)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

;; включить поддержку ctags для основных языков:
;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;;(when (cedet-ectag-version-check)
;;  (semantic-load-enable-primary-exuberent-ctags-support))

(setq qt4-base-dir "/usr/include/qt4")
(semantic-add-system-include qt4-base-dir 'c++-mode)
(add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))

(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
(add-hook 'c-mode-common-hook 'my-cedet-hook)

(defun my-c-mode-cedet-hook ()
 (linum-mode))
 ;; (local-set-key "." 'semantic-complete-self-insert)
 ;; (local-set-key ">" 'semantic-complete-self-insert))
 ;; (local-set-key "." 'semantic-ia-complete-symbol-menu)
 ;; (local-set-key ">" 'semantic-ia-complete-symbol-menu))


(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; Reflect FS
(ede-cpp-root-project "ReflectFS"
                :name "ReflectFS"
                :file "~/Development/c/reflectfs/CMakeLists.txt"
                :include-path '("/"
                                "/libs"
                                "/src"
                               )
                ;;:system-include-path '(".")
                :spp-table '(("isUnix" . "")
                             ("BOOST_TEST_DYN_LINK" . "")))

;; Reflect FS
(ede-cpp-root-project "Private Budget"
                :name "Private Budget"
                :file "~/Development/qt4/PrivateBudget/main.cpp"
                :include-path '("/"
                               )
                :system-include-path '("/usr/include/qt4")
                :spp-table '(("isUnix" . "")
                             ("BOOST_TEST_DYN_LINK" . "")))

(provide 'init-cedet)
