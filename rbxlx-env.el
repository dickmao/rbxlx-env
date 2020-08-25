;;; rbxlx-env.el --- Roblox development with a reasonable editor -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020 The Authors of rbxlx-env.el

;; Authors: dickmao <github id: dickmao>
;; Version: 0.1.0
;; Keywords: games tools
;; URL: https://github.com/dickmao/rbxlx

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with rbxlx-env.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extract and reintegrate Lua_ code from Roblox Place (.rbxlx) files.

;; Rojo_ does something similar, with considerably more fanfare.

;;; Code:

(require 'subr-x)
(require 'dash)
(require 'dash-functional)

(defvar rbxlx-insertion-points nil
  "An alist of (FOLDER-PATH . (:scriptguid :place)) where to stick new cdata strings.")

(defvar rbxlx-roblox nil "Modify in-place.")

(defun rbxlx--write-source (file source)
  (make-directory (file-name-directory file) t)
  (write-region source nil file))

(defun rbxlx--tailrec (item path dir)
  "Recurse on XML"
  (when-let ((is-list (listp item))
             (props (nthcdr 2 (alist-get 'Properties (nthcdr 2 item)))))
    (let (scriptguid
          (class (alist-get 'class (cl-second item))))
      (mapc
       (lambda (x)
         (when (string= "Name" (alist-get 'name (cl-second x)))
           (push (cl-third x) path))
         (when (string= "ScriptGuid" (alist-get 'name (cl-second x)))
           (setq scriptguid (cl-third x))))
       props)
      (when (and (stringp scriptguid)
                 (string-match-p "^\\(LocalScript\\|Script\\|ModuleScript\\)$" class))
        (let* ((descriptor (if (string= class (car path))
                               scriptguid
                             (car path)))
               (script-path (cons class path)))
          (mapc
           (lambda (x)
             (when (string= "Source" (alist-get 'name (cl-second x)))
               (unless (cl-third x)
                 (setf (nthcdr 2 x) (list "")))
               (setf (alist-get script-path rbxlx-insertion-points nil nil #'equal)
                     `(:scriptguid ,scriptguid :place ,(gv-ref (cl-third x))))
               (rbxlx--write-source
                (expand-file-name (format "%s.lua" descriptor)
                                  (mapconcat #'identity
                                             (cons (directory-file-name dir) (reverse script-path)) "/"))
                (cl-third x))))
           props)))))

  (mapcar (-rpartial #'rbxlx--tailrec path dir)
          (cl-remove-if-not
           (lambda (node) (stringp (alist-get 'class (cl-second node))))
           (nthcdr 2 item))))

;;;###autoload
(defun rbxlx-unfurl (rbxlx)
  (setq rbxlx-insertion-points nil)
  (setq rbxlx-roblox nil)
  (let* ((truename (abbreviate-file-name (file-truename rbxlx)))
         (number (nthcdr 10 (file-attributes truename)))
         (buffer (find-file-noselect-1 (create-file-buffer rbxlx)
                                      rbxlx t nil truename number)))
    (with-current-buffer buffer
      (unwind-protect
          (let* ((xml (libxml-parse-xml-region (point-min) (point-max)))
                 (_ (-tree-map-nodes
                     (lambda (x) (and (listp x) (eq (car x) 'roblox)))
                     (lambda (x) (setq rbxlx-roblox (nthcdr 2 x)))
                     xml))
                 (top (cl-remove-if-not
                       (lambda (node) (stringp (alist-get 'class (cl-second node))))
                       rbxlx-roblox))
                 (dir (make-temp-file "rbxlx-unfurl-" t)))
            (mapc (-rpartial 'rbxlx--tailrec nil dir) top)
            dir)
        (basic-save-buffer)
        (let (kill-buffer-query-functions)
          (kill-buffer))))))

;; (print-out (caddr rbxlx-roblox))

;;;###autoload
(defun rbxlx-furl (rbxlx path)
  "Walk PATH and replace corresponding Source nodes in `rbxlx-insertion-points'."
  (let* ((skip (length (split-string path "/")))
         (truename (abbreviate-file-name (file-truename rbxlx)))
         (number (nthcdr 10 (file-attributes truename)))
         (buffer (find-file-noselect-1 (create-file-buffer rbxlx)
                                       rbxlx t nil truename number)))
    (unwind-protect
        (progn
          (dolist (plst (mapcar #'cdr rbxlx-insertion-points))
            (when (plist-get plst :place)
              (setf (gv-deref (plist-get plst :place)) ""))
            (with-current-buffer buffer
              (save-excursion
                (re-search-forward (plist-get plst :scriptguid))
                (when (re-search-forward (regexp-quote "<![CDATA[") (+ (point) 100) t)
                  (let ((pos (match-end 0)))
                    (unless (search-forward (regexp-quote "]]>") nil t)
                      (error "Problem"))
                    (delete-region pos (match-beginning 0)))))))
          (mapcar
           (lambda (lua)
             (-when-let* ((key (reverse (cl-subseq
                                         (split-string (directory-file-name (file-name-directory lua)) "/")
                                         skip)))
                          (plst (alist-get key rbxlx-insertion-points nil nil #'equal))
                          (new-cdata (with-temp-buffer
                                       (insert-file-contents lua)
                                       (buffer-string))))
               (with-current-buffer buffer
                 (save-excursion
                   (re-search-forward (plist-get plst :scriptguid))
                   (if (re-search-forward (regexp-quote "<![CDATA[") (+ (point) 100) t)
                       (let ((pos (match-end 0)))
                         (unless (search-forward (regexp-quote "]]>") nil t)
                           (error "Problem"))
                         (delete-region pos (match-beginning 0))
                         (goto-char pos))
                     (re-search-forward (regexp-quote "</ProtectedString>")
                                        (+ (point) 100))
                     (goto-char (match-beginning 0))
                     (save-excursion (insert "<![CDATA[]]>"))
                     (re-search-forward (regexp-quote "<![CDATA[")))
                   (insert new-cdata)))
               (when (plist-get plst :place)
                 (setf (gv-deref (plist-get plst :place)) new-cdata))))
           (let ((the-files (directory-files-recursively path "\\.lua$")))
             (message "%s" (mapconcat #'identity the-files "\n"))
             the-files
             )))
      (with-current-buffer buffer
        (basic-save-buffer)
        (let (kill-buffer-query-functions)
          (kill-buffer))))))

(provide 'rbxlx-env)
