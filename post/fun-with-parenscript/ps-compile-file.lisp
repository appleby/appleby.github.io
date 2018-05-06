;;; Load this file in slime-repl and then call
;;; fun-with-ps:ps-compile-file-to-file. Alternatively, you can eval the
;;; following forms in your Parenscript buffers to setup a keybinding to
;;; compile the current buffer or have emacs auto-compile the buffer on save.
#|
(defun fun-with-ps/compile-file ()
  (interactive)
  (slime-eval-async `(fun-with-ps:ps-compile-file-to-file :input ,(buffer-file-name))))

;; The following forms should be evaluated in an Parenscript
;; buffer. Otherwise, you need to modify the above fun-with-ps/compile-file
;; defun with a guard to ensure it only runs in Parenscript buffers.

;; Change the keybinding to whatever suits you.
(local-set-key (kbd "C-c m k") 'fun-with-ps/compile-file)

;; Or, if you use bind-key:
(bind-key "C-c m k" 'fun-with-ps/compile-file (current-local-map))

;; Optionally, add an after-save-hook to automatically compile Parenscript
;; buffers on save.
(add-hook 'after-save-hook #'fun-with-ps/compile-file nil t)
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :parenscript))

(defpackage :fun-with-ps
  (:use :common-lisp :parenscript)
  (:export #:ps-compile-file-to-file))

(in-package :fun-with-ps)

(defun ps-compile-file-to-file (&key (input #P"parenscript.lisp") output)
  (let ((outpath (or output (uiop:make-pathname* :defaults input :type "js"))))
    (uiop:with-enough-pathname (enough-outpath :pathname outpath)
      (with-open-file (*parenscript-stream* enough-outpath :direction :output :if-exists :supersede)
        (ps-compile-file input)))))
