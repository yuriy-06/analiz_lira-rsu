;;; -*- mode: lisp; syntax: common-lisp; package: grunt; encoding: utf-8 -*-
;;; $Id$
;;;
;;; Copyright (c) 2014 Vdovik Yuriy.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.


(in-package :asdf)

(defsystem :analiz
  :name "analiz"
  :author "Vdovik Yuriy <babah.yuriy06@gmail.com>"
  :version "1"
  :maintainer "Vdovik Yuriy <babah.yuriy06@gmail.com>"
  :licence "MIT License"
  :description "Some function for engineering work"
  :depends-on (:cl-ppcre :parse-float :cl-svg :bordeaux-threads :climp)
  :components ((:file "package")
	       (:file "short-utils-3")
		   (:file "print-expand")
	       (:file "test-util")
	       (:file "analiz-for-one-8-3")
	       (:file "filter")
	       (:file "file-3")
	       (:file "geom-9")
	       (:file "rsu5")
	       (:file "kb3")
	       (:file "analiz-m")
	       (:file "analiz-ini20")
	       (:file "parallel-4-3")
	       (:file "circle"))
  :serial t)
  
