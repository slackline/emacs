;; PYTHON CONFIGURATION
;;; --------------------------------------

;; Hide the modeline for inferior python processes
(use-package inferior-python-mode
  :ensure nil
  :hook (inferior-python-mode . hide-mode-line-mode))

;; Required to easily switch virtual envs
;; via the menu bar or with `pyvenv-workon`
;; Setting the `WORKON_HOME` environment variable points
;; at where the envs are located.
;;
;; https://github.com/jorgenschaefer/pyvenv
(use-package pyvenv
  :ensure t
  :defer 1
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/.virtualenvs/"))
  (pyvenv-mode 1)
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t
	venv-byhost
	'(("kimura" . "~/.virtualenvs/")
	  ("fisher" . "~/.virtualenvs/")
	  ("haldane" . "~/.virtualenvs/")
	  ("ovh" . "~/.virtualenvs/")
	  ("alarmpi" . "~/.virtualenvs/")
	  ("alarmpi-4b" . "~/.virtualenvs/"))
	venv-location (cdr
		       (assoc system-name venv-byhost))
	default-venv-byhost
	'(("kimura" . "~/.virtualenvs/default")
	  ("fisher" . "~/.virtualenvs/python3_9")
	  ("haldane" . "~/.virtualenvs/default")
	  ("ovh" . "~/.virtualenvs/default")
	  ("alarmpi" . "~/.virtualenvs/default")
	  ("alarmpi-4b" . "~/.virtualenvs/default"))
	default-venv (cdr
		      (assoc system-name default-venv-byhost))
	python-environment-directory venv-location)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
					  (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode)
  (after-init-hook . (pyvenv-workon default-env))
  )

;; https://github.com/z80dev/uv-mode
;; (use-package uv-mode
;;   :hook (python-mode . uv-mode-auto-activate-hook))

;; https://github.com/robert-zaremba/auto-virtualenvwrapper.el
;; (use-package auto-virtualenvwrapper
;;   :hook (python-mode . auto-virtualenvwrapper-activate))

;;  https://github.com/nryotaro/pyvenv-auto
(use-package pyvenv-auto
  :ensure t
  :hook ((python-mode . pyvenv-auto-run)))

;; Built-in Python utilities
(use-package python
  :after (pyvenv)
  :ensure t
  :defer 1
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil
	python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"
	python-environment-directory venv-location)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")
  ;; Define a skeleton for printing https://emacs.stackexchange.com/questions/80320/defining-custom-python-skeletons
  (python-skeleton-define print
			  "Insert a print statement that will show the value of the argument."
			  "Enter the variable/object name: "
			  "print(f'\\n{" str "=}\\n')")
  (python-skeleton-define parameterized-test
			  "Insert a test with @pytest.mark.parametrize() fixture."
			  "Enter the parameters: "
			  "@pytest.mark.parametrize(\n    (" str "),\n    [\n        pytest.param(,id=\"\"),\n    ]\n)\ndef test_() -> None:")
  :bind (:map python-mode-map
	      ("C-c p t" . python-pytest-dispatch)
	      ("C-c p l" . pylint)
	      ("C-c p y" . pylint-insert-ignore-comment)
	      ("C-c p n" . numpydoc-generate)
	      ("C-c p b" . blacken-buffer)
	      ("C-c p r" . ruff-format-buffer)
	      ("C-c p v" . pyvenv-workon)
	      ("C-c p T c" . python-skeleton-class)
	      ("C-c p T d" . python-skeleton-def)
	      ("C-c p T f" . python-skeleton-for)
	      ("C-c p T i" . python-skeleton-if)
	      ("C-c p T m" . python-skeleton-import)
	      ("C-c P" . python-skeleton-print)
	      ("C-c p T t" . python-skeleton-try)
	      ("C-c p T T" . python-skeleton-parameterized-test)
	      ("C-c p T w" . python-skeleton-while)))

;; https://github.com/wbolster/emacs-python-pytest
(use-package python-pytest
  :after (pyvenv)
  :ensure t
  :config
  (transient-append-suffix
    'python-pytest-dispatch
    '(0)
    ["Regression Tests"
     ("-r" "Reset regression tests" "--regtest-reset")
     ("--tee" "Print results" "--regtest-tee")
     ("--nodiff" "Suppress output" "--regtest-nodiff")
     ("--endings" "Do not strip whitespaces at end of recorded lines" "--regtest-consider-line-endings")])
  )

;; TODO - Add these additional transient options
;; (transient-append-suffix
;;   'python-pytest-dispatch
;;   '(0)
;;   ["Testmon"
;;     ("--testmon" "Select tests affected by changes" "--testmon")
;;     ("--testmon-noselect" "Reorder & prioritise tests most likely to fail first" "--testmon-noselect")
;;     ("--testmon-nocollect" "Deactivate collection and writing of testmon data" "--testmon-nocollect")
;;     ("--testmon-forceselect" "Select only tests affected bv changes and satisfying pytest selectors"
;;       "--testmon-forceselect")
;;     ("--no-testmon" "Turn of testmon" "--no-testmon")
;;     ])
;; (transient-append-suffix
;;   'python-pytest-dispatch
;;   '(0)
;;   ["Hypothesis"
;;     ("--explain" "Enable the explain phase for failing Hypothesis tests" "--hypothesis-explain")
;;     ])
;; (transient-append-suffix
;;   'python-pytest-dispatch
;;   '(0)
;;   ["Syrupy"
;;     ("--snapshot-update" "Update snapshots" "--snapshot-update")
;;     ("--snapshot-warn-unused" "Do not fail on unused snapshots" "--snapshot-warn-unused")
;;     ("--snapshot-details" "Include details of unused snapshots in final report" "--snapshot-details")
;;     ])
;; (transient-append-suffix
;;   'python-pytest-dispatch
;;   '(0)
;;   ["Matplotlib"
;;     ("--mpl-summary" "Generate HTML summary on failure" "--mpl-generate-summary html")
;;     ("--mpl-baseline-relative" "Interpret the baseline directory as relative to the test location"
;;       "--mpl-baseline-relative")
;;     ("--mpl-deterministic" "Whether to make the image file metadata deterministic" "--mpl-deterministic")
;;     ("--mpl-no-deterministic" "Whether to make the image file metadata deterministic" "--mpl-no-deterministic")
;;     ])


;; https://github.com/ionrock/pytest-el
;; (use-package pytest
;;   :after (pyvenv)
;;   :ensure t)


;; https://github.com/emacsorphanage/pylint
;; (use-package pylint
;;   :hook (python-mode . pylint-add-menu)
;;   (python-mode . add-key-bindings))

;;; Linting - Lots of options, currently going with blacken
;; https://github.com/pythonic-emacs/blacken
(use-package blacken
  :ensure t
  :custom
  (blacken-line-length 120)
  :hook (python-mode . blacken-mode))

;; https://github.com/scop/emacs-ruff-format
(use-package ruff-format
  :ensure t
  :hook (python-mode . ruff-format-on-save-mode))

;; https://github.com/christophermadsen/emacs-lazy-ruff
(use-package lazy-ruff
  :ensure t
  :bind (("C-c p l" . lazy-ruff-lint-format-dwim)) ;; keybinding
  :config
  (lazy-ruff-mode-global-toggle t)) ;; Enable the lazy-ruff minor mode globally

;; https://github.com/erickgnavar/flymake-ruff
(use-package flymake-ruff
  :ensure t
  ;; :hook (python-mode . flymake-ruff-load))
  :hook (lsp-managed-mode . flymake-ruff-load))

;; https://github.com/douglasdavis/numpydoc.el
(use-package numpydoc
  :ensure t
  :after lsp
  :config
  (setq numpydoc-prompt-for-input t)
  (setq numpydoc-insert-examples-block 0)
  (setq numpydoc-insert-raises-block 0))


;; https://github.com/millejoh/emacs-ipython-notebook
;; (use-package ein
;;   :ensure t
;;   :config
;;   (setq ein:output-area-inlined-images t
;; 	ein:polymode t
;; 	ein:query-timeout 100000)
;;   :bind (:map ein:notebook-mode
;; 	      ("C-c e r" . ein:notebook-rename-command)
;; 	      ("C-c e x" . ein:notebook-restart-session-command)
;; 	      ("C-c e a" . ein:worksheet-execute-all-cells)
;; 	      ("C-c e <down>" . ein:worksheet-move-cell-down-km)
;; 	      ("C-c e <up>" . ein:worksheet-move-cell-up-km)
;; 	      ("C-c e d" . ein:worksheet-delete-cell)
;; 	      ("C-c e u" . ein:worksheet-execute-all-cells-above)
;; 	      ("C-c e b" . ein:worksheet-execute-all-cells-below)))

;; https://github.com/paetzke/py-autopep8.el
;; (use-package py-autopep8
;;   :hook (python-mode . py-autopep8-enable-on-save))

;; (use-package yapfify
;;   :ensure t
;; ;;   :hook (python-mode . yapf-mode))

;; https://github.com/cybniv/poetry.el
;; (use-package poetry
;; 	     :ensure t
;; 	   ;; 	     :after lsp)


;; python.el end here
