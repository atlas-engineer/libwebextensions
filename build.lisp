;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :cl-user)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(asdf:load-system :libwebextensions)

(defun dump-core ()
  (sb-ext:save-lisp-and-die
   "libwebextensions.core"
   :callable-exports '(libwebextensions::on-init
                       libwebextensions::on-page-created
                       libwebextensions::on-message)))
