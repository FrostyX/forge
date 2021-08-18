;;; forge-pagure.el --- Pagure support            -*- lexical-binding: t -*-

;; Copyright (C) 2021  Jakub Kadlčík

;; Author: Jakub Kadlčík <frostyx@email.cz>
;; Maintainer: Jakub Kadlčík <frostyx@email.cz>
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


;; Example endpoints
;; https://pagure.io/api/0/copr/copr
;; https://pagure.io/api/0/copr/copr/issues?per_page=20&page=1


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


;;; Pull

(cl-defmethod forge--pull ((repo forge-pagure-repository) until)
  (let* ((buf (current-buffer))
        (dir default-directory)
        (cb (lambda (data) nil))
        (repository (forge--fetch-repository repo cb))
        (issues (forge--fetch-issues repo cb until))
        (pullreqs (forge--fetch-pullreqs repo cb until)))

    (forge--update-repository repo repository)
    (forge--update-issues repo issues t)
    (forge--update-pullreqs repo pullreqs t)
    (print "Everything updated")))


;;; Factories / Decoders
;;; APIs evolve, grow, and change. Let's do the Elm approach here and define a
;;; minimal functions (decoders) just for parsing the fetched JSON.

(defun forge-repository-from-alist (data)
  "Convert project JSON response to an alist with the keys corresponding to our
database column names.
See the 'Project information' section for the JSON schema
https://pagure.io/api/0/#projects-tab
"
  (let-alist data
    (list
     (cons 'created (timestamp-to-iso .date_created))
     (cons 'updated (timestamp-to-iso .date_modified))
     (cons 'pushed nil)
     (cons 'parent nil) ; TODO
     (cons 'description .description)
     (cons 'homepage nil)
     (cons 'default-branch nil) ; TODO Needs additional request
     (cons 'archived-p nil) ; TODO Make sure there is no archiving in pagure
     (cons 'fork-p nil) ; TODO
     (cons 'locked-p nil) ; TODO
     (cons 'mirror-p nil)
     (cons 'private-p nil) ;; TODO Probably needs additional request
     (cons 'issues-p t) ;; TODO Probably needs additional request
     (cons 'wiki-p nil) ; TODO
     (cons 'stars 0) ; TODO Is there even API for this?
     (cons 'watchers 0) ; TODO Needs additional request
     )))


(defun forge-issue-from-alist (repo data)
  "Convert a single issue from an issues JSON response into an object.
See the 'List project\'s issues' section for the JSON schema
https://pagure.io/api/0/#issues-tab
"
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
     :created (timestamp-to-iso .date_created)
     :updated (timestamp-to-iso .last_updated)
     :closed .closed_at
     :locked-p nil
     :milestone nil ; TODO
     :body (forge--sanitize-string .content))))


(defun forge-pullreq-from-alist (repo data)
  "Convert a single PR from an PRs JSON response into an object.
See the 'List project\'s Pull-Requests' section for the JSON schema
https://pagure.io/api/0/#pull_requests-tab
"
  (let-alist data
    (forge-pullreq
     :id           (forge--object-id 'forge-pullreq repo .id)
     :repository   (oref repo id)
     :number       .id
     :state        (pcase-exhaustive .status
                     ("Open" 'open)) ; TODO 'merged and 'closed
     :author       .user.name
     :title        .title
     :created      (timestamp-to-iso .date_created)
     :updated      (timestamp-to-iso .updated_on)
     :closed       (and .closed_at (timestamp-to-iso .closed_at))
     :merged       nil ; TODO there is no field for it, check closed status
     :locked-p     nil
     :editable-p   nil ; TODO
     :cross-repo-p nil ; TODO how are these defined anyway?
     :base-ref     .branch
     :base-repo    .project.fullname
     :head-repo    .project.repo_from.user.name
     :head-ref     .branch_from
     :head-repo    .project.repo_from.user.name
     :head-repo    .project.repo_from.fullname
     :milestone    nil ; TODO
     ;; There is no project description per se.
     ;; Pagure treats it as a first comment.
     :body         (alist-get 'comment (car .comments)))))


(defun forge-issue-post-from-alist (repo issue-id data)
  "Convert a single issue comment from an issues JSON response into an object.
See the 'Comment of an issue' section for the JSON schema
https://pagure.io/api/0/#issues-tab
"
  (let-alist data
    (forge-issue-post
     :id (forge--object-id issue-id .id)
     :issue issue-id
     :number .id
     :author .user.name
     :created (timestamp-to-iso .date_created)
     :updated nil ;; TODO Maybe it is present in JSON only for updated comments
     :body .comment)))


(defun forge-pullreq-post-from-alist (repo pullreq-id data)
  "Convert a single PR comment from an PRs JSON response into an object.
See the 'Comment of an issue' section for the JSON schema
https://pagure.io/api/0/#issues-tab
(There is no pull-requests-specific documentation section)
"
  (let-alist data
    (forge-pullreq-post
     :id (forge--object-id pullreq-id .id)
     :pullreq pullreq-id
     :number .id
     :author .user.name
     :created (timestamp-to-iso .date_created)
     :updated nil ;; TODO Maybe it is present in JSON only for updated comments
     :body .comment)))


;;; GET API endpoints
;;; TODO API methods need to return decoded objects, not raw responses.
;;;      Therefore database methods could take them instead of having to
;;;      create them within themselves

(cl-defmethod forge--fetch-repository ((repo forge-pagure-repository) callback)
  "Request Pagure API for information about a project/repository"
  (let ((response (forge--pagure-get repo "/:project")))
    (forge-repository-from-alist response)))


(cl-defmethod forge--fetch-issues ((repo forge-pagure-repository) callback until)
  "Request Pagure API for information about a project issues"
  ;; TODO We need to go through all pages
  (let-alist (forge--pagure-get repo "/:project/issues")
      .issues))


(cl-defmethod forge--fetch-pullreqs ((repo forge-pagure-repository) callback until)
  "Request Pagure API for information about a project pull requests"
  ;; TODO We need to go through all pages
  (let-alist (forge--pagure-get repo "/:project/pull-requests")
      .requests))


;;; Database

(cl-defmethod forge--update-repository ((repo forge-pagure-repository) project)
  (emacsql-with-transaction (forge-db)
    (loop for pair in project
          do (setf (slot-value repo (car pair)) (cdr pair)))))


(cl-defmethod forge--update-issues ((repo forge-pagure-repository) issues bump)
  (emacsql-with-transaction (forge-db)
    (dolist (v issues) (forge--update-issue repo v bump))))


(cl-defmethod forge--update-pullreqs ((repo forge-pagure-repository) pullreqs bump)
  (emacsql-with-transaction (forge-db)
    (dolist (v pullreqs) (forge--update-pullreq repo v bump))))


(cl-defmethod forge--update-issue ((repo forge-pagure-repository) data bump)
  (emacsql-with-transaction (forge-db)
    (let-alist data
      (let ((issue (forge-issue-from-alist repo data)))
        (closql-insert (forge-db) issue t)
        ;; FIXME We depend on knowledge of the JSON structure. That should be
        ;;       dealt with in fetch function
        (forge--update-issue-posts repo (slot-value issue :id) .comments)))))


(cl-defmethod forge--update-pullreq ((repo forge-pagure-repository) data bump)
  (emacsql-with-transaction (forge-db)
    (let-alist data
      (let ((pullreq (forge-pullreq-from-alist repo data)))
        (closql-insert (forge-db) pullreq t)
        ;; FIXME We depend on knowledge of the JSON structure. That should be
        ;;       dealt with in fetch function
        (forge--update-pullreq-posts
         repo
         (slot-value pullreq :id)

         ;; We already used the first comment as the PR description
         (cdr .comments))))))


(cl-defmethod forge--update-issue-posts ((repo forge-pagure-repository)
                                         issue-id
                                         comments)
  (emacsql-with-transaction (forge-db)
    (dolist (v comments) (forge--update-issue-post repo issue-id v))))


(cl-defmethod forge--update-pullreq-posts ((repo forge-pagure-repository)
                                         pullreq-id
                                         comments)
  (emacsql-with-transaction (forge-db)
    (dolist (v comments) (forge--update-pullreq-post repo pullreq-id v))))


(cl-defmethod forge--update-issue-post ((repo forge-pagure-repository)
                                        issue-id
                                        comment)
  (emacsql-with-transaction (forge-db)
    (closql-insert
     (forge-db)
     (forge-issue-post-from-alist repo issue-id comment))))


(cl-defmethod forge--update-pullreq-post ((repo forge-pagure-repository)
                                        pullreq-id
                                        comment)
  (emacsql-with-transaction (forge-db)
    (closql-insert
     (forge-db)
     (forge-pullreq-post-from-alist repo pullreq-id comment))))









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

(defun timestamp-to-iso (timestamp)
  "Pagure API returns dates in form of Unix timestamps. One other peculiarity is
that it returns the numeric value as a string, so we need to convert it to int,
and then transform it into ISO 8601 Format.

See http://ergoemacs.org/emacs/elisp_datetime.html
"
  (let ((number (string-to-number timestamp)))
    (concat
     (format-time-string "%Y-%m-%dT%T" number)
     ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
      (format-time-string "%z" number)))))

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
