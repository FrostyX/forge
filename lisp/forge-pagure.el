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

;;; Pull
;;;; Repository

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


  ;; (let ((cb (let ((buf (and (derived-mode-p 'magit-mode)
  ;;                           (current-buffer)))
  ;;                 (dir default-directory)
  ;;                 (val nil))
  ;;             (lambda (cb &optional v)
  ;;               (when v (if val (push v val) (setq val v)))
  ;;               (let-alist val
  ;;                 (cond
  ;;                  (t (forge--fetch-issues repo cb until))
  ;;                  (t
  ;;                   (forge--msg repo t t   "Pulling REPO")
  ;;                   (forge--msg repo t nil "Storing REPO")
  ;;                   (emacsql-with-transaction (forge-db)
  ;;                     (dolist (v .issues)   (forge--update-issue repo v))
  ;;                     (oset repo sparse-p nil))
  ;;                   (forge--msg repo t t "Storing REPO")
  ;;                   (forge--git-fetch buf dir repo))))))))
  ;;   (funcall cb cb)))

;; (cl-defmethod forge--pull ((repo forge-pagure-repository) until)
;;   (let ((cb (let ((buf (and (derived-mode-p 'magit-mode)
;;                             (current-buffer)))
;;                   (dir default-directory)
;;                   (val nil))
;;               (lambda (cb &optional v)
;;                 (when v (if val (push v val) (setq val v)))
;;                 (let-alist val
;;                   (cond
;;                    ;; ((not val)
;;                    ;;  (forge--fetch-repository repo cb))
;;                    ;; ((not (assq 'assignees val))
;;                    ;;  (forge--fetch-assignees repo cb))
;;                    ;; ((not (assq 'forks val))
;;                    ;;  (forge--fetch-forks repo cb))
;;                    ;; ((not (assq 'labels val))
;;                    ;;  (forge--fetch-labels repo cb))
;;                    ;; ((and .issues_enabled
;;                    ;;       (not (assq 'issues val)))
;;                    ;;  (forge--fetch-issues repo cb until))
;;                    (t (forge--fetch-issues repo cb until))
;;                    ;; ((and .merge_requests_enabled
;;                    ;;       (not (assq 'pullreqs val)))
;;                    ;;  (forge--fetch-pullreqs repo cb until))
;;                    (t
;;                     (forge--msg repo t t   "Pulling REPO")
;;                     (forge--msg repo t nil "Storing REPO")
;;                     (emacsql-with-transaction (forge-db)
;;                       ;; (forge--update-repository repo val)
;;                       ;; (forge--update-assignees  repo .assignees)
;;                       ;; (forge--update-labels     repo .labels)
;;                       (dolist (v .issues)   (forge--update-issue repo v))
;;                       ;; (dolist (v .pullreqs) (forge--update-pullreq repo v))
;;                       (oset repo sparse-p nil))
;;                     (forge--msg repo t t "Storing REPO")
;;                     (forge--git-fetch buf dir repo))))))))
;;     (funcall cb cb)))

(cl-defmethod forge--fetch-repository ((repo forge-pagure-repository) callback)
  (forge--glab-get repo "/:project" nil
    :callback (lambda (value _headers _status _req)
                (when (magit-get-boolean "forge.omitexpensive")
                  (setq value (append '((assignees) (forks) (labels)) value)))
                (funcall callback callback value))))

;; (cl-defmethod forge--update-repository ((repo forge-pagure-repository) data)
;;   (let-alist data
;;     (oset repo created        .created_at)
;;     (oset repo updated        .last_activity_at)
;;     (oset repo pushed         nil)
;;     (oset repo parent         .forked_from_project.path_with_namespace)
;;     (oset repo description    .description)
;;     (oset repo homepage       nil)
;;     (oset repo default-branch .default_branch)
;;     (oset repo archived-p     .archived)
;;     (oset repo fork-p         (and .forked_from_project.path_with_namespace t))
;;     (oset repo locked-p       nil)
;;     (oset repo mirror-p       .mirror)
;;     (oset repo private-p      (equal .visibility "private"))
;;     (oset repo issues-p       .issues_enabled)
;;     (oset repo wiki-p         .wiki_enabled)
;;     (oset repo stars          .star_count)
;;     (oset repo watchers       .star_count)))

;; (cl-defmethod forge--split-url-path
;;   ((_class (subclass forge-pagure-repository)) path)
;;   (and (string-match "\\`\\(?:~?\\(.+\\)/\\)?\\([^/]+?\\)\\'" path)
;;        (list (match-string 1 path)
;;              (match-string 2 path))))

;;;; issues

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
    ;; (print (forge--glab-get repo "/:project/issues"))

    ;; (print (forge--glab-get repo "/:project/issues"
    ;;   :callback (lambda (value _headers _status _req)
    ;;               (funcall cb cb value))))

    ;; (forge--glab-get repo "/:project/issues"

    ;;   :callback (lambda (value _headers _status _req)
    ;;               (funcall cb cb value))))

    ;; )

    ;; TODO We need to go through all pages
    (forge--pagure-get repo "/:project/issues")


    )


    ;; (forge--glab-get repo "/:project/issues"
    ;;   `((per_page . 100)
    ;;     (order_by . "updated_at")
    ;;     (updated_after . ,(forge--topics-until repo until 'issue)))
    ;;   :unpaginate t
    ;;   :callback (lambda (value _headers _status _req)
    ;;               (funcall cb cb value))))
  )
    ;; (forge--glab-get repo "/:project/issues"
    ;;   `((per_page . 100)
    ;;     (order_by . "updated_at")
    ;;     (updated_after . ,(forge--topics-until repo until 'issue)))
    ;;   :unpaginate t
    ;;   :callback (lambda (value _headers _status _req)
    ;;               (funcall cb cb value)))))

;; (cl-defmethod forge--fetch-issue-posts ((repo forge-pagure-repository) cur cb)
;;   (let-alist (car cur)
;;     (forge--glab-get repo
;;       (format "/projects/%s/issues/%s/notes" .project_id .iid)
;;       '((per_page . 100))
;;       :unpaginate t
;;       :callback (lambda (value _headers _status _req)
;;                   (setf (alist-get 'notes (car cur)) value)
;;                   (funcall cb cb)))))

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

    ;; (let-alist data
    ;;   (let* ((issue-id (forge--object-id 'forge-issue repo .iid))
    ;;          (issue
    ;;           (forge-issue
    ;;            :id           issue-id
    ;;            :repository   (oref repo id)
    ;;            :number       .iid
    ;;            :state        (pcase-exhaustive .state
    ;;                            ("closed" 'closed)
    ;;                            ("opened" 'open))
    ;;            :author       .author.username
    ;;            :title        .title
    ;;            :created      .created_at
    ;;            :updated      .updated_at
    ;;            ;; `.closed_at' may be nil even though the issues is
    ;;            ;; closed.  in such cases use 1, so that this slots
    ;;            ;; at least can serve as a boolean.
    ;;            :closed       (or .closed_at (and (equal .state "closed") 1))
    ;;            :locked-p     .discussion_locked
    ;;            :milestone    .milestone.iid
    ;;            :body         (forge--sanitize-string .description))))
    ;;     (closql-insert (forge-db) issue t)
    ;;     (unless (magit-get-boolean "forge.omitexpensive")
    ;;       (forge--set-id-slot repo issue 'assignees .assignees)
    ;;       (forge--set-id-slot repo issue 'labels .labels))
    ;;     .body .id ; silence emacs 25 byte-compiler.
    ;;     (dolist (c .notes)
    ;;       (let-alist c
    ;;         (let ((post
    ;;                (forge-issue-post
    ;;                 :id      (forge--object-id issue-id .id)
    ;;                 :issue   issue-id
    ;;                 :number  .id
    ;;                 :author  .author.username
    ;;                 :created .created_at
    ;;                 :updated .updated_at
    ;;                 :body    (forge--sanitize-string .body))))
    ;;           (closql-insert (forge-db) post t))))))

))

;;;; pullreqs

;; (cl-defmethod forge--fetch-pullreqs ((repo forge-pagure-repository) callback until)
;;   (let ((cb (let (val cur cnt pos)
;;               (lambda (cb &optional v)
;;                 (cond
;;                  ((not pos)
;;                   (if (setq cur (setq val v))
;;                       (progn
;;                         (setq pos 1)
;;                         (setq cnt (length val))
;;                         (forge--msg nil nil nil "pulling pullreq %s/%s" pos cnt)
;;                         (forge--fetch-pullreq-posts repo cur cb))
;;                     (forge--msg repo t t "pulling repo pullreqs")
;;                     (funcall callback callback (cons 'pullreqs val))))
;;                  ((not (assq 'source_project (car cur)))
;;                   (forge--fetch-pullreq-source-repo repo cur cb))
;;                  ((not (assq 'target_project (car cur)))
;;                   (forge--fetch-pullreq-target-repo repo cur cb))
;;                  (t
;;                   (if (setq cur (cdr cur))
;;                       (progn
;;                         (cl-incf pos)
;;                         (forge--msg nil nil nil "pulling pullreq %s/%s" pos cnt)
;;                         (forge--fetch-pullreq-posts repo cur cb))
;;                     (forge--msg repo t t "pulling repo pullreqs")
;;                     (funcall callback callback (cons 'pullreqs val)))))))))
;;     (forge--msg repo t nil "pulling repo pullreqs")
;;     (forge--glab-get repo "/projects/:project/merge_requests"
;;       `((per_page . 100)
;;         (order_by . "updated_at")
;;         (updated_after . ,(forge--topics-until repo until 'pullreq)))
;;       :unpaginate t
;;       :callback (lambda (value _headers _status _req)
;;                   (funcall cb cb value)))))

;; (cl-defmethod forge--fetch-pullreq-posts
;;   ((repo forge-pagure-repository) cur cb)
;;   (let-alist (car cur)
;;     (forge--glab-get repo
;;       (format "/projects/%s/merge_requests/%s/notes" .target_project_id .iid)
;;       '((per_page . 100))
;;       :unpaginate t
;;       :callback (lambda (value _headers _status _req)
;;                   (setf (alist-get 'notes (car cur)) value)
;;                   (funcall cb cb)))))

;; (cl-defmethod forge--fetch-pullreq-source-repo
;;   ((repo forge-pagure-repository) cur cb)
;;   ;; if the fork no longer exists, then `.source_project_id' is nil.
;;   ;; this will lead to difficulties later on but there is nothing we
;;   ;; can do about it.
;;   (let-alist (car cur)
;;     (if .source_project_id
;;         (forge--glab-get repo (format "/projects/%s" .source_project_id) nil
;;           :errorback (lambda (_err _headers _status _req)
;;                        (setf (alist-get 'source_project (car cur)) nil)
;;                        (funcall cb cb))
;;           :callback (lambda (value _headers _status _req)
;;                       (setf (alist-get 'source_project (car cur)) value)
;;                       (funcall cb cb)))
;;       (setf (alist-get 'source_project (car cur)) nil)
;;       (funcall cb cb))))

;; (cl-defmethod forge--fetch-pullreq-target-repo
;;   ((repo forge-pagure-repository) cur cb)
;;   (let-alist (car cur)
;;     (forge--glab-get repo (format "/projects/%s" .target_project_id) nil
;;       :errorback (lambda (_err _headers _status _req)
;;                    (setf (alist-get 'source_project (car cur)) nil)
;;                    (funcall cb cb))
;;       :callback (lambda (value _headers _status _req)
;;                   (setf (alist-get 'target_project (car cur)) value)
;;                   (funcall cb cb)))))

;; (cl-defmethod forge--update-pullreq ((repo forge-pagure-repository) data)
;;   (emacsql-with-transaction (forge-db)
;;     (let-alist data
;;       (let* ((pullreq-id (forge--object-id 'forge-pullreq repo .iid))
;;              (pullreq
;;               (forge-pullreq
;;                :id           pullreq-id
;;                :repository   (oref repo id)
;;                :number       .iid
;;                :state        (pcase-exhaustive .state
;;                                ("merged" 'merged)
;;                                ("closed" 'closed)
;;                                ("opened" 'open))
;;                :author       .author.username
;;                :title        .title
;;                :created      .created_at
;;                :updated      .updated_at
;;                ;; `.merged_at' and `.closed_at' may both be nil even
;;                ;; though the pullreq is merged or otherwise closed.
;;                ;; in such cases use 1, so that these slots at least
;;                ;; can serve as booleans.
;;                :closed       (or .closed_at
;;                                  (and (member .state '("closed" "merged")) 1))
;;                :merged       (or .merged_at
;;                                  (and (equal .state "merged") 1))
;;                :locked-p     .discussion_locked
;;                :editable-p   .allow_maintainer_to_push
;;                :cross-repo-p (not (equal .source_project_id
;;                                          .target_project_id))
;;                :base-ref     .target_branch
;;                :base-repo    .target_project.path_with_namespace
;;                :head-ref     .source_branch
;;                :head-user    .source_project.owner.username
;;                :head-repo    .source_project.path_with_namespace
;;                :milestone    .milestone.iid
;;                :body         (forge--sanitize-string .description))))
;;         (closql-insert (forge-db) pullreq t)
;;         (unless (magit-get-boolean "forge.omitexpensive")
;;           (forge--set-id-slot repo pullreq 'assignees (list .assignee))
;;           (forge--set-id-slot repo pullreq 'labels .labels))
;;         .body .id ; silence emacs 25 byte-compiler.
;;         (dolist (c .notes)
;;           (let-alist c
;;             (let ((post
;;                    (forge-pullreq-post
;;                     :id      (forge--object-id pullreq-id .id)
;;                     :pullreq pullreq-id
;;                     :number  .id
;;                     :author  .author.username
;;                     :created .created_at
;;                     :updated .updated_at
;;                     :body    (forge--sanitize-string .body))))
;;               (closql-insert (forge-db) post t))))))))

;; ;;;; other

;; ;; the extend of the documentation for "get /projects/:id/users" is
;; ;; "get the users list of a project."  i don't know what that means,
;; ;; but it stands to reason that this must at least overlap with the
;; ;; set of users that can be assigned to topics.

;; (cl-defmethod forge--fetch-assignees ((repo forge-pagure-repository) callback)
;;   (forge--glab-get repo "/projects/:project/users"
;;     '((per_page . 100))
;;     :unpaginate t
;;     :callback (lambda (value _headers _status _req)
;;                 (funcall callback callback (cons 'assignees value)))))

;; (cl-defmethod forge--update-assignees ((repo forge-pagure-repository) data)
;;   (oset repo assignees
;;         (with-slots (id) repo
;;           (mapcar (lambda (row)
;;                     (let-alist row
;;                       ;; for other forges we don't need to store `id'
;;                       ;; but here we do because that's what has to be
;;                       ;; used when assigning issues.
;;                       (list (forge--object-id id .id)
;;                             .username
;;                             .name
;;                             .id)))
;;                   data))))

;; (cl-defmethod forge--fetch-forks ((repo forge-pagure-repository) callback)
;;   (forge--glab-get repo "/projects/:project/forks"
;;     '((per_page . 100)
;;       (simple . "true"))
;;     :unpaginate t
;;     :callback (lambda (value _headers _status _req)
;;                 (funcall callback callback (cons 'forks value)))))

;; (cl-defmethod forge--update-forks ((repo forge-pagure-repository) data)
;;   (oset repo forks
;;         (with-slots (id) repo
;;           (mapcar (lambda (row)
;;                     (let-alist row
;;                       (nconc (forge--repository-ids
;;                               (eieio-object-class repo)
;;                               (oref repo githost)
;;                               .namespace.path
;;                               .path)
;;                              (list .namespace.path
;;                                    .path))))
;;                   data))))

;; (cl-defmethod forge--fetch-labels ((repo forge-pagure-repository) callback)
;;   (forge--glab-get repo "/projects/:project/labels"
;;     '((per_page . 100))
;;     :unpaginate t
;;     :callback (lambda (value _headers _status _req)
;;                 (funcall callback callback (cons 'labels value)))))

;; (cl-defmethod forge--update-labels ((repo forge-pagure-repository) data)
;;   (oset repo labels
;;         (with-slots (id) repo
;;           (mapcar (lambda (row)
;;                     (let-alist row
;;                       ;; we should use the label's `id' instead of its
;;                       ;; `name' but a topic's `labels' field is a list
;;                       ;; of names instead of a list of ids or an alist.
;;                       ;; as a result of this we cannot recognize when
;;                       ;; a label is renamed and a topic continues to be
;;                       ;; tagged with the old label name until it itself
;;                       ;; is modified somehow.  additionally it leads to
;;                       ;; name conflicts between group and project
;;                       ;; labels.  see #160.
;;                       (list (forge--object-id id .name)
;;                             .name
;;                             (downcase .color)
;;                             .description)))
;;                   ;; for now simply remove one of the duplicates.
;;                   (cl-delete-duplicates data
;;                                         :key (apply-partially #'alist-get 'name)
;;                                         :test #'equal)))))

;; ;;;; notifications

;; ;; the closest to notifications that pagure provides are "events" as
;; ;; described at https://docs.pagure.com/ee/api/events.html.  this
;; ;; allows us to see the last events that took place, but that is not
;; ;; good enough because we are mostly interested in events we haven't
;; ;; looked at yet.  pagure doesn't make a distinction between unread
;; ;; and read events, so this is rather useless and we don't use it for
;; ;; the time being.

;; ;;; mutations

;; (cl-defmethod forge--submit-create-pullreq ((_ forge-pagure-repository) base-repo)
;;   (let-alist (forge--topic-parse-buffer)
;;     (pcase-let* ((`(,base-remote . ,base-branch)
;;                   (magit-split-branch-name forge--buffer-base-branch))
;;                  (`(,head-remote . ,head-branch)
;;                   (magit-split-branch-name forge--buffer-head-branch))
;;                  (head-repo (forge-get-repository 'stub head-remote)))
;;       (forge--glab-post head-repo "/projects/:project/merge_requests"
;;         `(,@(and (not (equal head-remote base-remote))
;;                  `((target_project_id . ,(oref base-repo forge-id))))
;;           (target_branch . ,base-branch)
;;           (source_branch . ,head-branch)
;;           (title         . , .title)
;;           (description   . , .body)
;;           (allow_collaboration . t))
;;         :callback  (forge--post-submit-callback)
;;         :errorback (forge--post-submit-errorback)))))

;; (cl-defmethod forge--submit-create-issue ((_ forge-pagure-repository) repo)
;;   (let-alist (forge--topic-parse-buffer)
;;     (forge--glab-post repo "/projects/:project/issues"
;;       `((title       . , .title)
;;         (description . , .body))
;;       :callback  (forge--post-submit-callback)
;;       :errorback (forge--post-submit-errorback))))

;; (cl-defmethod forge--submit-create-post ((_ forge-pagure-repository) topic)
;;   (forge--glab-post topic
;;     (if (forge-issue-p topic)
;;         "/projects/:project/issues/:number/notes"
;;       "/projects/:project/merge_requests/:number/notes")
;;     `((body . ,(string-trim (buffer-string))))
;;     :callback  (forge--post-submit-callback)
;;     :errorback (forge--post-submit-errorback)))

;; (cl-defmethod forge--submit-edit-post ((_ forge-pagure-repository) post)
;;   (forge--glab-put post
;;     (cl-etypecase post
;;       (forge-pullreq "/projects/:project/merge_requests/:number")
;;       (forge-issue   "/projects/:project/issues/:number")
;;       (forge-issue-post "/projects/:project/issues/:topic/notes/:number")
;;       (forge-pullreq-post "/projects/:project/merge_requests/:topic/notes/:number"))
;;     (if (cl-typep post 'forge-topic)
;;         (let-alist (forge--topic-parse-buffer)
;;           ;; keep pagure from claiming that the user
;;           ;; changed the description when that isn't
;;           ;; true.  the same isn't necessary for the
;;           ;; title; in that case pagure performs the
;;           ;; necessary check itself.
;;           `((title . , .title)
;;             ,@(and (not (equal .body (oref post body)))
;;                    `((description . , .body)))))
;;       `((body . ,(string-trim (buffer-string)))))
;;     :callback  (forge--post-submit-callback)
;;     :errorback (forge--post-submit-errorback)))

;; (cl-defmethod forge--set-topic-field
;;   ((_repo forge-pagure-repository) topic field value)
;;   (forge--glab-put topic
;;     (cl-typecase topic
;;       (forge-pullreq "/projects/:project/merge_requests/:number")
;;       (forge-issue   "/projects/:project/issues/:number"))
;;     `((,field . ,value))
;;     :callback (forge--set-field-callback)))

;; (cl-defmethod forge--set-topic-title
;;   ((repo forge-pagure-repository) topic title)
;;   (forge--set-topic-field repo topic 'title title))

;; (cl-defmethod forge--set-topic-state
;;   ((repo forge-pagure-repository) topic)
;;   (forge--set-topic-field repo topic 'state_event
;;                           (cl-ecase (oref topic state)
;;                             (closed "reopen")
;;                             (open   "close"))))

;; (cl-defmethod forge--set-topic-labels
;;   ((repo forge-pagure-repository) topic labels)
;;   (forge--set-topic-field repo topic 'labels
;;                           (mapconcat #'identity labels ",")))

;; (cl-defmethod forge--set-topic-assignees
;;   ((repo forge-pagure-repository) topic assignees)
;;   (let ((users (mapcar #'cdr (oref repo assignees))))
;;     (cl-typecase topic
;;       (forge-pullreq ; can only be assigned to a single user.
;;        (forge--set-topic-field repo topic 'assignee_id
;;                                (caddr (assoc (car assignees) users))))
;;       (forge-issue
;;        (forge--set-topic-field repo topic 'assignee_ids
;;                                (--map (caddr (assoc it users)) assignees))))))

;; (cl-defmethod forge--delete-comment
;;   ((_repo forge-pagure-repository) post)
;;   (forge--glab-delete post
;;     (cl-etypecase post
;;       (forge-pullreq-post
;;        "/projects/:project/merge_requests/:topic/notes/:number")
;;       (forge-issue-post
;;        "/projects/:project/issues/:topic/notes/:number")))
;;   (closql-delete post)
;;   (magit-refresh))

;; (cl-defmethod forge--topic-templates ((repo forge-pagure-repository)
;;                                       (_ (subclass forge-issue)))
;;   (--filter (string-match-p "\\`\\.pagure/issue_templates/.+\\.md\\'" it)
;;             (magit-revision-files (oref repo default-branch))))

;; (cl-defmethod forge--topic-templates ((repo forge-pagure-repository)
;;                                       (_ (subclass forge-pullreq)))
;;   (--filter (string-match-p "\\`\\.pagure/merge_request_templates/.+\\.md\\'" it)
;;             (magit-revision-files (oref repo default-branch))))

;; (cl-defmethod forge--fork-repository ((repo forge-pagure-repository) fork)
;;   (with-slots (owner name) repo
;;     (forge--glab-post repo (format "/projects/%s%%2f%s/fork" owner name)
;;       (and (not (equal fork (ghub--username (ghub--host nil))))
;;            `((namespace . ,fork)))
;;       :noerror t)
;;     (ghub-wait (format "/projects/%s%%2f%s" fork name)
;;                nil :auth 'forge :forge 'pagure)))

;; (cl-defmethod forge--merge-pullreq ((_repo forge-pagure-repository)
;;                                     topic hash method)
;;   (forge--glab-put topic
;;     "/projects/:project/merge_requests/:number/merge"
;;     `((squash . ,(if (eq method 'squash) "true" "false"))
;;       ,@(and hash `((sha . ,hash))))))

;; ;;; utilities

;; (cl-defmethod forge--topic-type-prefix ((_repo forge-pagure-repository) type)
;;   (if (eq type 'pullreq) "!" "#"))

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

;; (cl-defun forge--glab-put (obj resource
;;                                &optional params
;;                                &key query payload headers
;;                                silent unpaginate noerror reader
;;                                host callback errorback)
;;   (declare (indent defun))
;;   (glab-put (if obj (forge--format-resource obj resource) resource)
;;             params
;;             :host (or host (oref (forge-get-repository obj) apihost))
;;             :auth 'forge
;;             :query query :payload payload :headers headers
;;             :silent silent :unpaginate unpaginate
;;             :noerror noerror :reader reader
;;             :callback callback
;;             :errorback (or errorback (and callback t))))

;; (cl-defun forge--glab-post (obj resource
;;                                 &optional params
;;                                 &key query payload headers
;;                                 silent unpaginate noerror reader
;;                                 host callback errorback)
;;   (declare (indent defun))
;;   (glab-post (forge--format-resource obj resource)
;;              params
;;              :host (or host (oref (forge-get-repository obj) apihost))
;;              :auth 'forge
;;              :query query :payload payload :headers headers
;;              :silent silent :unpaginate unpaginate
;;              :noerror noerror :reader reader
;;              :callback callback
;;              :errorback (or errorback (and callback t))))

;; (cl-defun forge--glab-delete (obj resource
;;                                   &optional params
;;                                   &key query payload headers
;;                                   silent unpaginate noerror reader
;;                                   host callback errorback)
;;   (declare (indent defun))
;;   (glab-delete (forge--format-resource obj resource)
;;                params
;;                :host (or host (oref (forge-get-repository obj) apihost))
;;                :auth 'forge
;;                :query query :payload payload :headers headers
;;                :silent silent :unpaginate unpaginate
;;                :noerror noerror :reader reader
;;                :callback callback
;;                :errorback (or errorback (and callback t))))

;;; _
(provide 'forge-pagure)
;;; forge-pagure.el ends here
