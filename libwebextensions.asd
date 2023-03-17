;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause
(in-package :asdf)

(defsystem "libwebextensions"
  :description "Describe libwebextensions here"
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/libwebextensions"
  :license  "BSD-3 Clause"
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "libwebextensions"))
  :in-order-to ((test-op (test-op "libwebextensions/tests")
                         (test-op "libwebextensions/tests/compilation"))))
