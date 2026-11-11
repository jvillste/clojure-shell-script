;;; cider-script-jack-in.el --- Jack in to a Clojure shell script  -*- lexical-binding: t; -*-

;; A script that runs through `clojure-shell-script' carries its tools.deps map
;; on the second line:
;;
;;   #!/path/to/clojure-shell-script
;;   '{:deps {babashka/fs {:mvn/version "0.5.34"}}}
;;
;;   (ns example (:require [babashka.fs :as fs]))
;;
;; `cider-jack-in' knows nothing about that line, so this file teaches it: when
;; a buffer's shebang names `clojure-shell-script', the map on the second line
;; is merged into the deps CIDER injects for nREPL and the result is put in
;; `cider-jack-in-cmd'.
;;
;; The merge is necessary because `clojure' only honours the *last* `-Sdeps'
;; flag, so appending a second one would silently drop either the script's
;; libraries or CIDER's nREPL middleware.
;;
;; Enable it once in your init file:
;;
;;   (require 'cider-script-jack-in)
;;   (add-hook 'clojure-mode-hook #'cider-script-jack-in-setup)

;;; Code:

(require 'cider)
(require 'parseedn)
(require 'subr-x)

(defconst cider-script-jack-in--alias :cider/nrepl
  "The alias CIDER always activates, used to extend the classpath.")

(defun cider-script-jack-in--shebang (file)
  "Return the shebang line of FILE, or nil."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 512)
      (goto-char (point-min))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (when (string-prefix-p "#!" line)
          line)))))

(defun cider-script-jack-in--script-p (file)
  "Return non-nil when FILE's shebang names `clojure-shell-script'.
The name is matched at the end of the interpreter path, so both
`#!/workspace/clojure-shell-script' and `#!./clojure-shell-script' count."
  (when-let* ((shebang (cider-script-jack-in--shebang file)))
    (string-match-p "clojure-shell-script\\'" (string-trim (substring shebang 2)))))

(defun cider-script-jack-in--deps-line (file)
  "Return the tools.deps map text on the second line of FILE, or nil.
The leading quote that keeps the shell script from being read as Clojure code
is removed."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (forward-line 1)
      (let ((line (string-trim (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))))
        (when (string-prefix-p "{" (setq line (string-remove-prefix "'" line)))
          line)))))

(defun cider-script-jack-in--deps-map (file)
  "Return the tools.deps map on the second line of FILE, or nil."
  (when-let* ((line (cider-script-jack-in--deps-line file)))
    (condition-case nil
        (let ((parsed (parseedn-read-str line)))
          (and (hash-table-p parsed) parsed))
      (error nil))))

(defun cider-script-jack-in--merge (base override)
  "Return OVERRIDE merged into BASE.
Nested maps are merged recursively; any other value in OVERRIDE wins.  This
keeps CIDER's injected dependencies while letting the script override them."
  (if (and (hash-table-p base) (hash-table-p override))
      (let ((result (copy-hash-table base)))
        (maphash (lambda (key value)
                   (let ((existing (gethash key result ::missing)))
                     (puthash key
                              (if (eq existing ::missing)
                                  value
                                (cider-script-jack-in--merge existing value))
                              result)))
                 override)
        result)
    override))

(defun cider-script-jack-in--relative (directory project-directory)
  "Return DIRECTORY as a path relative to PROJECT-DIRECTORY."
  (directory-file-name (file-relative-name directory project-directory)))

(defun cider-script-jack-in--classpath-entries (file project-directory deps)
  "Return classpath entries for FILE, relative to PROJECT-DIRECTORY.
The script's own directory is always included so that CIDER can load its
namespace.  Paths declared by the script are resolved against the script's
directory, which is how `clojure-shell-script' would see them.  DEPS is the
script's deps map."
  (let* ((script-directory (file-name-directory (expand-file-name file)))
         (declared (or (and deps (gethash :paths deps)) []))
         (paths (cons (cider-script-jack-in--relative script-directory
                                                      project-directory)
                      (mapcar (lambda (path)
                                (cider-script-jack-in--relative
                                 (expand-file-name path script-directory)
                                 project-directory))
                              declared))))
    (vconcat (delete-dups paths))))

(defun cider-script-jack-in--without-paths (deps)
  "Return DEPS with :paths removed.
The paths cannot be merged into the top-level map: whatever `-Sdeps' says
about :paths replaces the project's own paths, so they are moved to an
alias instead."
  (let ((result (copy-hash-table deps)))
    (remhash :paths result)
    result))

(defun cider-script-jack-in--add-extra-paths (deps extra-paths)
  "Return DEPS with EXTRA-PATHS added to the alias CIDER activates."
  (let* ((result (copy-hash-table deps))
         (aliases (or (gethash :aliases result) (make-hash-table :test #'equal)))
         (alias (or (gethash cider-script-jack-in--alias aliases)
                    (make-hash-table :test #'equal)))
         (existing (or (gethash :extra-paths alias) [])))
    (setq aliases (copy-hash-table aliases)
          alias (copy-hash-table alias))
    (puthash :extra-paths (vconcat existing extra-paths) alias)
    (puthash cider-script-jack-in--alias alias aliases)
    (puthash :aliases aliases result)
    result))

(defun cider-script-jack-in--injected (parameters)
  "Return the command CIDER injects for PARAMETERS, split into parts.
The value is a list of the deps map and the trailing `-M' argument, so that
the map can be rewritten while everything else is passed through as is."
  (let ((injected (cider-inject-jack-in-dependencies parameters 'clojure-cli)))
    (unless (string-match "\\`-Sdeps \\(.+?\\) \\(-M.*\\)\\'" injected)
      (error "Unrecognised jack-in command: %s" injected))
    (list (parseedn-read-str
           (replace-regexp-in-string "\\\\\\(.\\)" "\\1" (match-string 1 injected)))
          (string-trim (match-string 2 injected)))))

(defun cider-script-jack-in--command (file project-directory)
  "Return a jack-in command for FILE with its deps map merged in, or nil.
PROJECT-DIRECTORY is where `clojure' runs, so classpath entries are made
relative to it."
  (when-let* ((script-deps (cider-script-jack-in--deps-map file))
              (command (cider-jack-in-resolve-command 'clojure-cli))
              (parameters (or (cider-jack-in-params 'clojure-cli) ""))
              (injected (cider-script-jack-in--injected parameters))
              (merged (cider-script-jack-in--merge
                       (car injected)
                       (cider-script-jack-in--without-paths script-deps)))
              (merged (cider-script-jack-in--add-extra-paths
                       merged
                       (cider-script-jack-in--classpath-entries
                        file project-directory script-deps))))
    (format "%s -Sdeps %s %s"
            command
            (shell-quote-argument (parseedn-print-str merged))
            (cadr injected))))

;;;###autoload
(defun cider-script-jack-in-setup ()
  "Jack in with the deps map on the second line of a `clojure-shell-script'.
Meant for `clojure-mode-hook'.  Does nothing unless the buffer's shebang
names `clojure-shell-script' and its second line is a tools.deps map, so
ordinary Clojure buffers keep the normal jack-in."
  (when-let* ((file (buffer-file-name))
              ((cider-script-jack-in--script-p file))
              (project-directory (or (cider-project-dir)
                                     (file-name-directory file)))
              (command (cider-script-jack-in--command file project-directory)))
    (setq-local cider-jack-in-cmd command)))

(provide 'cider-script-jack-in)
;;; cider-script-jack-in.el ends here
