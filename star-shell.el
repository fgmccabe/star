;;; -*- lexical-binding: t; -*-
;;; Star shell
;;; Copyright (C) 2019 and beyond F.G. McCabe

(require 'comint)

;; Customization point -- where is the executable for star
(defcustom star-path "/usr/local/bin/star"
  "Path name for star executable"
  :type 'file
  :group 'star)

(defvar star-flags `("-dg" "-r" ,star-build-repo)
  "standard flags to pass to star executable")

(defvar star-prompt-regexp "\\[\\([0-9]+\\)\\]>>"
  "Regexp that matches prompt from star executable")

(defvar star-debug-run-mode-map
  (let ((map (make-sparse-keymap)))
    (progn
      (set-keymap-parent map comint-mode-map)
      (define-key map "\t" 'completion-at-point)
      map)))
      
