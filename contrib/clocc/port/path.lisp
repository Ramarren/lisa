;;; Pathnames and Filesystems
;;;
;;; Copyright (C) 1999-2001 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: path.lisp,v 1.1 2002/04/08 20:13:42 youngde Exp $
;;; $Source: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/clocc/port/Attic/path.lisp,v $

(eval-when (compile load eval)
  (require :port-ext (translate-logical-pathname "clocc:src;port;ext")))

(in-package :port)

(export
 '(pathname-ensure-name probe-directory default-directory chdir mkdir rmdir
   *logical-hosts-definitions* load-logical-host))

;;;
;;; utilities
;;;

(defun pathname-ensure-name (path)
  "Make sure that the pathname has a name slot.
Call `pathname' on it argument and, if there is no NAME slot,
but there is a TYPE slot, move TYPE into NAME."
  (let ((path (pathname path)))
    (if (or (pathname-name path) (null (pathname-type path))) path
        ;; if you use CLISP, you will need 2000-03-09 or newer
        (make-pathname :name (concatenate 'string "." (pathname-type path))
                       :type nil :defaults path))))

;;;
;;; filesystem access
;;;

(defun probe-directory (filename)
  "Check whether the file name names an existing directory."
  ;; based on
  ;; From: Bill Schelter <wfs@fireant.ma.utexas.edu>
  ;; Date: Wed, 5 May 1999 11:51:19 -0500
  ;; fold the name.type into directory
  (let* ((path (pathname filename))
         (name (pathname-name path))
         (type (pathname-type path))
         (new-dir
          (cond ((and name type) (list (concatenate 'string name "." type)))
                (name (list name))
                (type (list type))
                (t nil))))
    (when new-dir
      (setq path (make-pathname
                  :directory (append (pathname-directory path) new-dir)
                  :name nil :type nil :defaults path)))
    #+allegro (excl::probe-directory path)
    #+clisp (values
             (ignore-errors
               (#+lisp=cl ext:probe-directory #-lisp=cl lisp:probe-directory
                          path)))
    #+cmu (eq :directory (unix:unix-file-kind (namestring path)))
    #+lispworks (lw:file-directory-p path)
    #+sbcl (eq :directory (sb-unix:unix-file-kind (namestring path)))
    #-(or allegro clisp cmu lispworks sbcl)
    (probe-file (make-pathname :directory dir))))

(defun default-directory ()
  "The default directory."
  #+allegro (excl:current-directory)
  #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
  #+cmu (ext:default-directory)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (lcl:working-directory)
  #-(or allegro clisp cmu cormanlisp lispworks lucid) (truename "."))

(defun chdir (dir)
  #+allegro (excl:chdir dir)
  #+clisp (#+lisp=cl ext:cd #-lisp=cl lisp:cd dir)
  #+cmu (setf (ext:default-directory) dir)
  #+cormanlisp (ccl:set-current-directory dir)
  #+gcl (si:chdir dir)
  #+lispworks (hcl:change-directory dir)
  #+lucid (lcl:working-directory dir)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid)
  (error 'not-implemented :proc (list 'chdir dir)))

(defsetf default-directory chdir "Change the current directory.")

(defun mkdir (dir)
  #+allegro (excl:make-directory dir)
  #+clisp (#+lisp=cl ext:make-dir #-lisp=cl lisp:make-dir dir)
  #+cmu (unix:unix-mkdir (directory-namestring dir) #o777)
  #+lispworks (system:make-directory dir)
  #+sbcl (sb-unix:unix-mkdir (directory-namestring dir) #o777)
  #-(or allegro clisp cmu lispworks sbcl)
  (error 'not-implemented :proc (list 'mkdir dir)))

(defun rmdir (dir)
  #+allegro (excl:delete-directory dir)
  #+clisp (#+lisp=cl ext:delete-dir #-lisp=cl lisp:delete-dir dir)
  #+cmu (unix:unix-rmdir dir)
  #+lispworks
  ;; `lw:delete-directory' is present in LWW 4.1.20 but not on LWL 4.1.0
  (if (fboundp 'lw::delete-directory)
      (lw::delete-directory dir)
      (delete-file dir))
  #-(or allegro clisp cmu lispworks) (delete-file dir))

;;;
;;; logical pathnames
;;;

(defun load-logical-host-def (host file &key style (verbose *load-verbose*))
  "Load the logical HOST definition from FILE.
STYLE can be either :CMU or :ALLEGRO."
  (ecase style
    (:allegro
     (with-open-file (fi file :if-does-not-exist nil)
       (unless fi (return-from load-logical-host-def nil))
       (when verbose (format t ";; Loading logical hosts from ~s~%" file))
       (do ((done nil) (ho (read fi nil +eof+) (read fi nil +eof+)))
           ((eq ho +eof+)
            (when verbose (format t ";; Done with ~s~%" file))
            done)
         (unless done (setq done (string-equal ho host)))
         (when verbose (format t ";;   host ~s~%" ho))
         (setf (logical-pathname-translations ho) (eval (read fi))))))
    (:cmucl
     (with-open-file (fi file :if-does-not-exist nil)
       (unless fi (return-from load-logical-host-def nil))
       (when verbose
         (format t ";; Loading host ~s from ~s..." host file)
         (force-output))
       (prog1 (setf (logical-pathname-translations host) (read fi))
         (when verbose (format t "done~%")))))))

(defcustom *logical-hosts-definitions* list nil
  "*The list of files or directories to load logical host definitions from.")

(defun load-logical-host (host &key (verbose *load-verbose*))
  "Load the definition of the logical HOST from `*logical-hosts-definitions*'."
  (handler-case (load-logical-pathname-translations host)
    (error (err)
      (dolist (path *logical-hosts-definitions* (error err))
        (setq path (pathname path))
        (if (pathname-name path)
            (when (load-logical-host-def host path :style :allegro
                                         :verbose verbose)
              (return-from load-logical-host t))
            (when (load-logical-host-def
                   host (merge-pathnames (string-downcase host) path)
                   :style :cmucl :verbose verbose)
              (return-from load-logical-host t)))))))

(provide :port-path)
;;; file path.lisp ends here
