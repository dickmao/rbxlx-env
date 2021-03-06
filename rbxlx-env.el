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

(defun rbxlx-alist-get (key alist &optional default remove testfn)
  "Replicated library function for emacs-25.

Same argument meanings for KEY ALIST DEFAULT REMOVE and TESTFN."
  (ignore remove)
  (let ((x (if (not testfn)
               (assq key alist)
             (assoc key alist))))
    (if x (cdr x) default)))

(gv-define-expander rbxlx-alist-get
  (lambda (do key alist &optional default remove testfn)
    (macroexp-let2 macroexp-copyable-p k key
      (gv-letplace (getter setter) alist
        (macroexp-let2 nil p `(if (and ,testfn (not (eq ,testfn 'eq)))
                                  (assoc ,k ,getter)
                                (assq ,k ,getter))
          (funcall do (if (null default) `(cdr ,p)
                        `(if ,p (cdr ,p) ,default))
                   (lambda (v)
                     (macroexp-let2 nil v v
                       (let ((set-exp
                              `(if ,p (setcdr ,p ,v)
                                 ,(funcall setter
                                           `(cons (setq ,p (cons ,k ,v))
                                                  ,getter)))))
                         `(progn
                            ,(cond
                             ((null remove) set-exp)
                             ((or (eql v default)
                                  (and (eq (car-safe v) 'quote)
                                       (eq (car-safe default) 'quote)
                                       (eql (cadr v) (cadr default))))
                              `(if ,p ,(funcall setter `(delq ,p ,getter))))
                             (t
                              `(cond
                                ((not (eql ,default ,v)) ,set-exp)
                                (,p ,(funcall setter
                                              `(delq ,p ,getter))))))
                            ,v))))))))))

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
               (setf (rbxlx-alist-get script-path rbxlx-insertion-points nil nil #'equal)
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

;;;###autoload
(defun rbxlx-furl (rbxlx path)
  "Walk PATH and replace corresponding Source nodes in `rbxlx-insertion-points'."
  (let* ((path (file-relative-name path))
         (skip (length (split-string path "/")))
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
             (-when-let* ((lua (file-relative-name lua))
                          (key (reverse (cl-subseq
                                         (split-string (directory-file-name (file-name-directory lua)) "/")
                                         skip)))
                          (plst (rbxlx-alist-get key rbxlx-insertion-points nil nil #'equal))
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
                     (re-search-forward "</.*String>" (+ (point) 100))
                     (goto-char (match-beginning 0))
                     (save-excursion (insert "<![CDATA[]]>"))
                     (re-search-forward (regexp-quote "<![CDATA[")))
                   (insert new-cdata)))
               (when (plist-get plst :place)
                 (setf (gv-deref (plist-get plst :place)) new-cdata))))
           (directory-files-recursively path "\\.lua$")))
      (with-current-buffer buffer
        (basic-save-buffer)
        (let (kill-buffer-query-functions)
          (kill-buffer))))))

(provide 'rbxlx-env)
