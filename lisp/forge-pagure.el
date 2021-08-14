;;; forge-pagure.el --- Pagure support            -*- lexical-binding: t -*-

;; Copyright (C) 2021  Jakub Kadlčík

;; Author: Jonas Bernoulli <frostyx@email.cz>
;; Maintainer: Jonas Bernoulli <frostyx@email.cz>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Forge is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Forge is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Forge.  If not, see http://www.gnu.org/licenses.

;;; Code:

; TODO :%s/glab/pagure/s
(require 'glab)
(require 'pagure)

(require 'forge)
(require 'forge-issue)
(require 'forge-pullreq)

;;; Class

(defclass forge-pagure-repository (forge-repository)
  ((issues-url-format         :initform "https://%h/%o/%n/issues")
   (issue-url-format          :initform "https://%h/%o/%n/issue/%i")
   ;; (issue-post-url-format     :initform "https://%h/%o/%n/issues/%i#note_%I")
   ;; (pullreqs-url-format       :initform "https://%h/%o/%n/merge_requests")
   ;; (pullreq-url-format        :initform "https://%h/%o/%n/merge_requests/%i")
   ;; (pullreq-post-url-format   :initform "https://%h/%o/%n/merge_requests/%i#note_%I")
   ;; (commit-url-format         :initform "https://%h/%o/%n/commit/%r")
   ;; (branch-url-format         :initform "https://%h/%o/%n/commits/%r")
   (remote-url-format         :initform "https://%h/%o/%n")
   ;; (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   ;; (create-pullreq-url-format :initform "https://%h/%o/%n/merge_requests/new")
   ;; (pullreq-refspec :initform "+refs/merge-requests/*/head:refs/pullreqs/*")))
   ))


;;; Factories

(defun forge-issue-from-alist (repo data)
  (let-alist data
    (forge-issue
     :id (forge--object-id 'forge-issue repo .id)
     :repository (oref repo id)
     :number .id
     :state (pcase-exhaustive .status
              ("Closed" 'closed) ; TODO Make sure it is really Closed
              ("Open" 'open))
     :author .user.name
     :title .title
     :created .date_created
     :updated .last_updated
     :closed .closed_at
     :locked-p nil
     :milestone nil ; TODO
     :body (forge--sanitize-string .content))))


;;; Pull
;;; Repository

(cl-defmethod forge--pull ((repo forge-pagure-repository) until)


  (let* ((buf (current-buffer))
        (dir default-directory)
        (cb (lambda (data) nil))
        (data (forge--fetch-issues repo cb until)))

        (let-alist data
        (print "AAA")
        ;; (emacsql-with-transaction (forge-db)
          (print "A11")
          (dolist (v .issues)   (forge--update-issue-2 repo v))
          ;; (dolist (v .issues) (print v))
          ;; (forge--update-issue-2 repo (car .issues))
          (print "A22")

          )
        )
    )



(cl-defmethod forge--fetch-repository ((repo forge-pagure-repository) callback)
  (forge--glab-get repo "/:project" nil
    :callback (lambda (value _headers _status _req)
                (when (magit-get-boolean "forge.omitexpensive")
                  (setq value (append '((assignees) (forks) (labels)) value)))
                (funcall callback callback value))))


;;; issues

(cl-defmethod forge--fetch-issues ((repo forge-pagure-repository) callback until)
  (let ((cb (let (val cur cnt pos)
              (lambda (cb &optional v)
                (cond
                 ((not pos)
                  (if (setq cur (setq val v))
                      (progn
                        (setq pos 1)
                        (setq cnt (length val))
                        (forge--msg nil nil nil "pulling issue %s/%s" pos cnt)
                        ;; (forge--fetch-issue-posts repo cur cb)
                        )
                    (forge--msg repo t t "pulling repo issues")
                    (funcall callback callback (cons 'issues val))))
                 (t
                  (if (setq cur (cdr cur))
                      (progn
                        (cl-incf pos)
                        (forge--msg nil nil nil "pulling issue %s/%s" pos cnt)
                        ;; (forge--fetch-issue-posts repo cur cb)
                        )
                    (forge--msg repo t t "pulling repo issues")
                    (funcall callback callback (cons 'issues val)))))))))
    (forge--msg repo t nil "pulling repo issues")

    (print "BBB")

    ;; TODO We need to go through all pages
    (forge--pagure-get repo "/:project/issues")

    )
  )


(cl-defmethod forge--update-issue-2 ((repo forge-pagure-repository) data)
  (print "DDD")
  (emacsql-with-transaction (forge-db)
    (let-alist data
      (let ((issue (forge-issue-from-alist repo data)))
        (print issue)
        (closql-insert (forge-db) issue t)
        (print "EEE")
        ;; TODO Update also issue comments
        ))

))

;;; pullreqs


;;; other

;; the extend of the documentation for "get /projects/:id/users" is
;; "get the users list of a project."  i don't know what that means,
;; but it stands to reason that this must at least overlap with the
;; set of users that can be assigned to topics.


;;;; notifications

;; the closest to notifications that pagure provides are "events" as
;; described at https://docs.pagure.com/ee/api/events.html.  this
;; allows us to see the last events that took place, but that is not
;; good enough because we are mostly interested in events we haven't
;; looked at yet.  pagure doesn't make a distinction between unread
;; and read events, so this is rather useless and we don't use it for
;; the time being.

;;; mutations

;;; utilities

(cl-defun forge--pagure-get (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host callback errorback)
  (declare (indent defun))
  (pagure-get (if obj (forge--format-resource obj resource) resource)
            params
            :host (or host (oref (forge-get-repository obj) apihost))
            :auth 'forge
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback
            :errorback (or errorback (and callback t))))


;;; _
(provide 'forge-pagure)
;;; forge-pagure.el ends here
