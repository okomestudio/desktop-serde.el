;;; desktop-serde.el --- Desktop serialization layer  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025-2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/desktop-serde
;; Version: 0.1.1
;; Keywords: convenience, desktop, save
;; Package-Requires: ((emacs "31.1"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The extension adds a serialization/deserialization layer to the
;; `desktop' package in Emacs.
;;
;; For a global variable stored in `desktop-globals-to-save',
;; serializer/deserializer functions can be registered in
;; `ok-desktop-global-var-serdes-funs'. The layer runs on each desktop
;; save or read.
;;
;;; Code:

(require 'desktop)

(defgroup desktop-serde nil
  "Custom serialization layer for Emacs desktop session persistence."
  :prefix "desktop-serde-"
  :group 'desktop)

(defcustom desktop-serde-before-read-hook nil
  "Hook executed immediately before `desktop-read' restores state."
  :type 'hook
  :group 'desktop-serde)

(defcustom desktop-serde-global-var-funs nil
  "List of variable transformations applied during desktop save/restore.
Each element is a list of the form (VARIABLE . FUNCTION):

VARIABLE: Symbol of a global variable present in `desktop-globals-to-save'.

FUNCTION: Function taking (ACTION VALUE) as arguments. VALUE is the
runtime value of VARIABLE. When ACTION is `serialize', it returns its
printable/serializable form. When ACTION is `deserialize', it returns
its reconstructed runtime value. See `desktop-serde-hash-table' for example."
  :type '(repeat (cons (symbol :tag "Variable")
                       (function :tag "De/serializer")))
  :group 'desktop-serde)

(defun desktop-serde-outvar--ad (fun varspec)
  "Advice around `desktop-outvar' (FUN) to serialize VARSPEC before saving.
If VARSPEC (a symbol) has a registered serializer in `desktop-serde-global-var-funs',
its value is temporarily transformed using the serializer function
before being written by `desktop-outvar'. The variable's original
runtime value is restored immediately after."
  (if-let* ((serfun (and (symbolp varspec)
                         (alist-get varspec desktop-serde-global-var-funs))))
      (let ((old (symbol-value varspec)))
        (set varspec (funcall serfun 'serialize old))
        (unwind-protect
            (funcall fun varspec)
          (set varspec old)))
    (funcall fun varspec)))

(advice-add #'desktop-outvar :around #'desktop-serde-outvar--ad)

(defun desktop-serde-read--ad (fun &rest args)
  "Advice around `desktop-read' (FUN) to deserialize restored variables.
Executes `desktop-serde-before-read-hook', invokes `desktop-read' with ARGS, and then
runs registered deserializers for all configured variables in
`desktop-serde-global-var-funs'."
  (run-hooks 'desktop-serde-before-read-hook)
  (prog1 (apply fun args)
    (pcase-dolist (`(,var . ,deserfun) desktop-serde-global-var-funs)
      (set var (funcall deserfun 'deserialize (symbol-value var))))))

(advice-add #'desktop-read :around #'desktop-serde-read--ad)

(defun desktop-serde-hash-table (action value)
  "Perform hash table de/serialization on VALUE.
ACTION is a symbol, either `serialize' or `deserialize'."
  (pcase action
    ('serialize (prin1-to-string value))
    ('deserialize
     (if-let* ((desered (and (stringp value) (read value)))
               (_ (hash-table-p desered)))
         desered
       (make-hash-table :test 'equal)))))

(provide 'desktop-serde)
;;; desktop-serde.el ends here
