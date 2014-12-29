;;; cliist.el --- Cliist calls in Emacs
     
;; Copyright (C) 2014 Žiga Stopinšek
     
;; Author: Žiga Stopinšek <sigi.kajzer@gmail.com>
;; Version: 0.1
;; Keywords: productiviry, todo
;; URL: http://github.com/ddksr/cliist
     
;;; Commentary:
     
;; This package provides some basic Todoist querying via commandline tool cliist.
;; You are required to install cliist ( http://github.com/ddksr/cliist )
     
;;;###autoload

(setq cliist/query-exe "cliist %s --format org")

(defun cliist/open-buffer-and-run-command (title cliist-command)
  (switch-to-buffer (get-buffer-create (format "*Cliist: %s*" title)))
  (erase-buffer)
  (org-mode)
  (insert (shell-command-to-string (format cliist/query-exe cliist-command)))
  (goto-line 0) (move-to-column 0)
  (org-cycle) (org-cycle))

(defun cliist/today-and-overdue ()
  (interactive)
  (cliist/open-buffer-and-run-command "today and overdue" ""))

(defun cliist/query (query)
  (interactive "sQuery: \n")
  (cliist/open-buffer-and-run-command query (format "-q %s" query)))
  
(defun cliist/project (name)
  (interactive "sProject name: \n")
  (cliist/open-buffer-and-run-command (format "#%s" name) (format "-p %s" name)))

(defun cliist/view-all ()
  (interactive)
  (cliist/open-buffer-and-run-command "All" "-A"))


(provide 'cliist)
