;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :libwebextensions)

(sb-alien:define-alien-callable on-init sb-alien:void ((extension (sb-alien:* t)))
  (format t "Initializing WebExtensions library for ~a~%" extension))

(sb-alien:define-alien-callable on-page-created sb-alien:void ((extension (sb-alien:* t))
                                                               (page (sb-alien:* t))
                                                               (user-data (sb-alien:* t)))
  (declare (ignorable user-data))
  (format t "Page ~a created for ~a~%" page extension))

(sb-alien:define-alien-callable on-message sb-alien:void ((page (sb-alien:* t))
                                                          (message (sb-alien:* t))
                                                          (user-data (sb-alien:* t)))
  (declare (ignorable user-data))
  (format t "Message ~a received for page ~a~%" message page))
