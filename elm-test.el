;;; elm-test.el --- Enhance elm-mode for elm-test

;; Copyright (C) 2017  Juan Edi

;; Author: Juan Edi
;; Keywords: elm elm-tesst
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; At least for the moment, this package just sets up a few fuctions to run elm
;; tests from inside emacs when using elm-mode.
;;
;; Most of it is just a copy of the fantastic rspec-mode
;; (https://github.com/pezra/rspec-mode)

;;; Code:

(require 'compile)
(require 'ansi-color)

(defgroup elm-test nil
  "elm-test integration"
  :group 'languages)

(defcustom elm-test-command "elm-test"
  "The command for elm-test."
  :type 'string
  :group 'elm-test)

(defcustom elm-test-command-options nil
  "Default options used with elm-test-command."
  :type 'string
  :group 'elm-test)

(defvar elm-test-find-project-root 'elm-test--project-root)

(define-compilation-mode elm-test-compilation-mode "Elm Test Compilation"
  "Compilation mode for elm-test output."
  ;; (add-hook 'compilation-filter-hook 'elm-test--colorize-compilation-buffer nil t)
  )

(defun elm-test-run ()
  "Run elm-test on the current buffer's file."
  (interactive)
  (elm-test--run-single-file
   (buffer-file-name)
   elm-test-command-options))

(defun elm-test-watch ()
  "Run elm-test on the current buffer's file in watch mode."
  (interactive)
  (elm-test--run-single-file
   (buffer-file-name)
   (concat elm-test-command-options " --watch")))

(defun elm-test--run-single-file (test-file &rest opts)
  "Run elm-test on SPEC_FILE with the specified options OPTS."
  (elm-test--compile (shell-quote-argument test-file) opts))

(defun elm-test--compile (target &optional opts)
  "Run a compile for TARGET with the specified options OPTS."

  (setq elm-test-last-directory default-directory
        elm-test-last-arguments (list target opts))

  (let ((default-directory (or (apply elm-test-find-project-root ())
                               (elm-test--project-root)
                               default-directory)))
    (compile
     (elm-test--compile-command target opts)
     'elm-test-compilation-mode)))

(defun elm-test-rerun ()
  "Re-run the last elm-test invocation."
  (interactive)
  (if (not elm-test-last-directory)
      (error "No previous verification")
    (let ((default-directory elm-test-last-directory))
      (apply #'elm-test--compile elm-test-last-arguments))))

(defun elm-test--project-root ()
  "Looks for the root of an elm project, assuming a directory structure in which
the root contains a 'test' directory for elm-test stuff.
It works by going up the directory hierarchy (starting at the current file)
until we reach the first elm-package.json file (which would correspond to tests)
and going up one more time to reach the root."
  (let
      ((elm-test-directory (locate-dominating-file (buffer-file-name) "elm-package.json")))
    (file-name-directory (directory-file-name elm-test-directory))))

(defun elm-test--compile-command (target &optional opts)
  "Composes elm-test command line for the compile function"
  (mapconcat 'identity `(,elm-test-command
                         ,(elm-test--runner-options opts)
                         ,target) " "))

(defun elm-test--runner-options (&optional opts)
  "Return string of options from OPTS for command line."
  (let ((opts (if (listp opts)
                  opts
                (list opts))))
    (mapconcat 'identity opts " ")))

(provide 'elm-test)
;;; elm-test.el ends here
