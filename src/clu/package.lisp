;;;clution.lib - project development tools for CL
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(defpackage #:clution.lib.clu
  (:use
   #:%clution.lib
   #:alexandria
   #:cl
   #:cl-arrows
   #:clution.lib.cl-sexp
   #:enumerable
   #:parse-float)
  (:export
   ;;;clu
   #:read-clu-file
   #:write-clu-file

   #:clu-file-add-dir
   #:clu-file-remove-dir
   #:clu-file-add-system
   #:clu-file-remove-system
   #:clu-file-plist))
