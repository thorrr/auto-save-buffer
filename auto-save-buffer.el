 ;; Copyright (C) 2012, Jason Bell

;; Author: Jason Bell <jbellthor AT gmail dot com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/IdleHighlight
;; Version: 1.0
;; Created: 2012-12-15
;; Keywords: auto save, autosave

;; This file is NOT part of GNU Emacs.

;;; License:

 ;; This program is free software: you can redistribute it and/or modify
 ;; it under the terms of the GNU General Public License as published by
 ;; the Free Software Foundation, either version 3 of the License, or
 ;; (at your option) any later version.

 ;; This program is distributed in the hope that it will be useful,
 ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ;; GNU General Public License for more details.

 ;; You should have received a copy of the GNU General Public License
 ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; auto-save-buffer - silent autosave for emacs

;;; Commentary:

;; Derived from http://www.litchie.net/programs/real-auto-save.el

;; Put this file in a folder where Emacs can find it.
;;
;; Add following lines to your .emacs initialization file:
;;
;;     (require 'auto-save-buffer)
;;     (add-hook 'find-file-hooks 'turn-on-auto-save-buffer) ;; real-auto-save every single file
;;     or
;;   
;;     (add-hook 'text-mode-hook 'turn-on-auto-save-buffer) ;; file mode specific auto-saving
;;
;; Customizations:
;;    
;;    Idle timer:
;;       (setq auto-save-buffer-interval 5) ;; in seconds
;;    
;;    Global auto-save toggle:
;;         (setq auto-save-buffer-global-p nil)
;;         (setq auto-save-buffer-global-p 't) ;;default
;;
;;    Prevent auto-save of buffers until you've manually saved once:
;;         (setq auto-save-buffer-only-after-regular-save 't)
;;
;;    Toggle the "Wrote ..." message:
;;       (setq auto-save--buffer-messaging nil)
;;       (setq auto-save--buffer-messaging 't) ;;default
;;
;; Commands:
;;
;;    Buffer auto-save toggle:
;;         M-x turn-on-auto-save-buffer
;;         M-x turn-off-auto-save-buffer
;;


(defcustom auto-save-buffer-alist nil
  "List of buffers that will be auto saved")
 
(defcustom auto-save-buffer-interval 3
  "Time interval of real auto save")
 
(defcustom auto-save-buffer-global-p t
  "Globally toggle real auto save")
 
(defcustom auto-save-buffer-messaging t
  "Toggle \"Wrote ... \" message")
 
(defcustom auto-save-buffer-only-after-regular-save nil
  "Wait until user C-x C-s to start autosaving")
 
(defadvice save-buffer (before save-buffer-real-autosave-mark activate)
   (set (make-local-variable 'auto-save-buffer/manually-saved) 't))
 
(defvar message-original nil
  "A pointer to the original definition of message")
(fset 'message-original (symbol-function 'message))
 
(defvar write-region-original nil
  "A pointer to the original definition of write-region")
(fset 'write-region-original (symbol-function 'write-region))
 
(defun no-message (fs &rest args)
  't)
 
(defun write-region-silent (start end filename &optional append visit lockname mustbenew)
  (set-buffer-modified-p nil)
  (write-region-original start end filename append 1 lockname mustbenew))

(defun auto-save-buffer ()
  "Save every buffer in auto-save-buffer-alist"
  (interactive)
  (if auto-save-buffer-global-p
      (save-excursion
        (dolist (elem auto-save-buffer-alist)
          (if (ignore-errors (set-buffer elem) t)
              (if (and (buffer-file-name) (buffer-modified-p)
                       (or (not auto-save-buffer-only-after-regular-save)
                           (and auto-save-buffer-only-after-regular-save auto-save-buffer/manually-saved)))
                  (progn
                    ;;we have to do this because advising 'message and 'write-region don't work
                    (if (not auto-save-buffer-messaging) (progn
                        (fset 'message (symbol-function 'no-message))
                        (fset 'write-region (symbol-function 'write-region-silent))))
                    (write-file (buffer-file-name))
                    (if (not auto-save-buffer-messaging) (progn
                        (fset 'message (symbol-function 'message-original))
                        (fset 'write-region (symbol-function 'write-region-original))))
                    )))))))

(defun turn-on-auto-save-buffer ()
  (interactive)
  (if (buffer-file-name)
        (add-to-list 'auto-save-buffer-alist (buffer-name))))

(defun turn-off-auto-save-buffer ()
  (interactive)
  (when (buffer-file-name)
    (setq auto-save-buffer-alist (remove (buffer-name) auto-save-buffer-alist))))

(run-with-idle-timer auto-save-buffer-interval t 'auto-save-buffer)

(provide 'auto-save-buffer)
