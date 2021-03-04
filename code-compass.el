;;; code-compass.el --- Make Emacs your compass in a sea of software complexity.

;; Copyright (C) 2020 Andrea Giugliano

;; Author: Andrea Giugliano <agiugliano@live.it>
;; Version: 0.1.0
;; Package-Version: 20210224
;; Keywords: emacs, sofware, analysis

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Make Emacs your compass in a sea of software complexity
;;
;; This tool puts the power and knowledge of your repository history in your hands.
;; The current analyses supported are:
;;   - c/show-hotspots:
;;     show hotspots in code repository as a circle diagram.
;;     Circles are packages or modules.
;;     The redder the circle, the more it has been modified lately. The bigger the more code it contains.
;;
;; See documentation on https://github.com/ag91/code-compass

;;; Code:
(require 'f)
(require 's)
(require 'simple-httpd)
(require 'async)
(require 'url)


(defgroup code-compass nil
  "Options specific to code-compass."
  :tag "code-compass"
  :group 'code-compass)

(defcustom c/default-periods
  '("beginning" "1d" "2d" "3d" "6d" "12d" "18d" "24d" "1m" "2m" "6m")
  "A list of choices for starting date for reducing the Git log for analysis. 'beginning' is a keyword to say to not reduce.'Nd' means to start after N days, where N is a positive number. 'Nm' means to start after N months, where N is a positive number."
  :group 'code-compass)

(defcustom c/snapshot-periods
  '("1d" "3m" "6m" "9m" "12m" "15m")
  "A list of snapshots periods to show evolution of analyses over time."
  :group 'code-compass)

(defconst c/path-to-code-compass (file-name-directory load-file-name) "Path to code compass.")

(defun c/expand-file-name (file-name)
  "Expand FILE-NAME with `c/path-to-code-compass'."
  (expand-file-name file-name c/path-to-code-compass))

(defcustom c/code-maat-command
  (format "java -jar %s/code-maat-1.0.1-standalone.jar" (c/expand-file-name "dependencies"))
  "Command to run Code-maat (https://github.com/adamtornhill/code-maat). Currently defaults to use docker because easier to setup."
  :group 'code-compass
  :options `(,(format "java -jar %s/code-maat-1.0.1-standalone.jar" (c/expand-file-name "dependencies")) "docker run -v /tmp/:/data code-maat-app"))

(defcustom c/preferred-browser
  "chromium"
  "Browser to use to open graphs served by webserver."
  :group 'code-compass)

(defun c/subtract-to-now (n month|day &optional time)
  "Subtract N * MONTH|DAY to current time. Optionally give TIME from which to start."
  (time-subtract
   (or time (current-time))
   (seconds-to-time (* 60 60 month|day n))))

(defun c/request-date (days|months &optional time)
  "Request date in days or months by asking how many DAYS|MONTHS ago. Optionally give TIME from which to start."
  (interactive
   (list (completing-read "From how long ago?" c/default-periods)))
  (when (not (string= days|months "beginning"))
    (format-time-string
     "%Y-%m-%d"
     (apply
      'c/subtract-to-now
      (-concat
       (if (s-contains-p "m" days|months)
           (list (string-to-number (s-replace "m" "" days|months)) (* 24 31))
         (list (string-to-number (s-replace "d" "" days|months)) 24))
       (list time))))))

(defun c/first (l)
  (car l))

(defun c/second (l)
  (nth 1 l))

(defun c/third (l)
  (nth 2 l))

(defun c/temp-dir (repository)
  (format "/tmp/code-compass-%s/" (f-filename repository)))

(defmacro c/in-directory (directory &rest body)
  "Executes BODY in DIRECTORY by temporarily changing current buffer's default directory to DIRECTORY."
  `(let ((current-dir default-directory))
     (unwind-protect
         (progn
           (cd ,directory)
           ,@body)
       (cd current-dir))))

(defmacro c/in-temp-directory (repository &rest body)
  "Executes BODY in temporary directory created for analysed REPOSITORY."
  `(progn
     (mkdir (c/temp-dir repository) t)
     (c/in-directory
      (c/temp-dir repository)
      ,@body)))

(defun c/produce-git-report (repository date &optional before-date)
  "Create git report for REPOSITORY with a Git log starting at DATE. Define optionally a BEFORE-DATE."
  (interactive
   (list (call-interactively 'c/request-date)))
  (message "Producing git report...")
  (shell-command
   (s-concat
    (format "git -C %s" repository)
    " log --all --numstat --date=short --pretty=format:'--%h--%ad--%aN' --no-renames "
    (when date
      (format "--after=%s " date))
    (when before-date
      (format "--before=%s " before-date))
    "> gitreport.log"))
  repository)

(defun c/run-code-maat (command repository)
  "Run code-maat's COMMAND on REPOSITORY."
  (message "Producing code-maat %s report for %s..." command repository)
  (let ((source-file "./dependencies/code-maat-1.0.1-standalone.jar")
        (maat-jar-p (s-contains-p "jar" c/code-maat-command)))
    (unless (or (not maat-jar-p) (file-exists-p (c/expand-file-name source-file)))
      (mkdir (c/expand-file-name "dependencies") t)
      (url-copy-file "https://github.com/smontanari/code-forensics/raw/v3.0.0/lib/analysers/code_maat/code-maat-1.0.1-standalone.jar" (c/expand-file-name source-file) t))
    (shell-command
     (format
      "%1$s -l %4$s/code-compass-%2$s/gitreport.log -c git2 -a %3$s > %3$s.csv"
      c/code-maat-command
      (f-filename repository)
      command
      (if maat-jar-p "/tmp" "/data")))))

(defun c/produce-code-maat-revisions-report (repository)
  "Create code-maat revisions report for REPOSITORY."
  (c/run-code-maat "revisions" repository)
  repository)

(defun c/produce-cloc-report (repository)
  "Create cloc report for REPOSITORY."
  (message "Producing cloc report...")
  (shell-command
   (format "(cd %s; cloc ./ --by-file --csv --quiet) > cloc.csv" repository))
  repository)

(defun c/copy-file (file-name directory)
  (copy-file (c/expand-file-name file-name) directory t))

(defun c/generate-merger-script (repository)
  "Generate a Python script to give weights to the circle diagram of REPOSITORY."
  (c/copy-file "./scripts/csv_as_enclosure_json.py" (c/temp-dir repository))
  repository)

(defun c/generate-d3-v3-lib (repository)
  "Make available the D3 library for REPOSITORY. This is just to not depend on a network connection."
  (mkdir "d3" t)
  (let ((source-file "./dependencies/d3.v3.min.js"))
    (unless (file-exists-p (c/expand-file-name source-file))
      (mkdir (c/expand-file-name "dependencies") t)
      (url-copy-file "http://d3js.org/d3.v3.min.js" (c/expand-file-name source-file) t))
    (c/copy-file source-file "d3/"))
  repository)

(defun c/generate-d3-v4-lib (repository)
  "Make available the D3 v4 library for REPOSITORY. This is just to not depend on a network connection."
  (mkdir "d3" t)
  (let ((source-file "./dependencies/d3.v4.min.js"))
    (unless (file-exists-p (c/expand-file-name source-file))
      (mkdir (c/expand-file-name "dependencies") t)
      (url-copy-file "http://d3js.org/d3.v4.min.js" (c/expand-file-name source-file) t))
    (c/copy-file source-file "d3/"))
  repository)

(defun c/produce-json (repository)
  "Produce json for REPOSITORY."
  (message "Produce json...")
  (shell-command
   "python3 csv_as_enclosure_json.py --structure cloc.csv --weights revisions.csv > hotspot_proto.json")
  repository)

(defun c/generate-host-enclosure-diagram-html (repository)
  "Generate host html from REPOSITORY."
  (c/copy-file "./pages/enclosure-diagram/style.css" (c/temp-dir repository))
  (c/copy-file "./pages/enclosure-diagram/script.js" (c/temp-dir repository))
  (c/copy-file "./pages/enclosure-diagram/zoomable.html" (c/temp-dir repository))
  repository)

(defun c/navigate-to-localhost (repository &optional port)
  "Navigate to served directory for REPOSITORY, optionally at specified PORT."
  (let ((port (or port 8888)))
    (browse-url (format "http://localhost:%s/zoomable.html" port)))
  (sleep-for 1)
  repository)

(defun c/run-server (repository &optional port)
  "Serve directory for REPOSITORY, optionally at PORT."
  (let ((httpd-host 'local)
        (httpd-port (or port 8888)))
    (httpd-stop)
    (ignore-errors (httpd-serve-directory (c/temp-dir repository))))
  repository)

(defun c/run-server-and-navigate (repository &optional port)
  "Serve and navigate to REPOSITORY, optionally at PORT."
  (when port
    (c/run-server repository port)
    (c/navigate-to-localhost repository port)))

(defun c/async-run (command repository date &optional port do-not-serve)
  "Run asynchronously COMMAND taking a REPOSITORY and a DATE, optionally at PORT."
  (async-start
   `(lambda ()
      (setq load-path ',load-path)
      (load-file ,(symbol-file command))
      (let ((browse-url-browser-function 'browse-url-generic)
            (browse-url-generic-program ,c/preferred-browser))
        (funcall ',command ,repository ,date)))
   `(lambda (result)
      (let ((browse-url-browser-function 'browse-url-generic)
            (browse-url-generic-program ,c/preferred-browser))
        (when (not ,do-not-serve) (c/run-server-and-navigate (if (eq ',command 'c/show-hotspot-cluster-sync) "system" ,repository) (or ,port 8888)))))))

(defun c/show-hotspots-sync (repository date &optional port)
  "Show REPOSITORY enclosure diagram for hotspots starting at DATE, optionally served at PORT."
  (interactive
   (list
    (read-directory-name "Choose git repository directory:" (vc-root-dir))
    (call-interactively 'c/request-date)))
  (c/in-temp-directory
   repository
   (--> repository
        (c/produce-git-report it date)
        c/produce-code-maat-revisions-report
        c/produce-cloc-report
        c/generate-merger-script
        c/generate-d3-v3-lib
        c/produce-json
        c/generate-host-enclosure-diagram-html
        (c/run-server-and-navigate it port))))

(defun c/show-hotspots (repository date &optional port)
  "Show REPOSITORY enclosure diagram for hotspots. Starting DATE reduces scope of Git log and PORT defines where the html is served."
  (interactive
   (list
    (read-directory-name "Choose git repository directory:" (vc-root-dir))
    (call-interactively 'c/request-date)))
  (c/async-run 'c/show-hotspots-sync repository date port))

(defun c/show-hotspot-snapshot-sync (repository)
  "Snapshot COMMAND over REPOSITORY over the last year every three months."
  (interactive
   (list
    (read-directory-name "Choose git repository directory:" (vc-root-dir))))
  (--each c/snapshot-periods (c/show-hotspots-sync repository (c/request-date it) 8888)))

;; BEGIN indentation

(defun c/split-on-newlines (code)
  "Split CODE over newlines."
  (s-split "\n" code))

(defun c/remove-empty-lines (lines)
  "Remove empty LINES."
  (--remove (eq (length (s-trim it)) 0) lines))

(defun c/remove-text-after-indentation (lines)
  "Remove text in LINES."
  (--map
   (apply 'string (--take-while (or (eq ?\s  it) (eq ?\t it)) (string-to-list it)))
   lines))

(defun c/find-indentation (lines-without-text)
  "Infer indentation level in LINES-WITHOUT-TEXT. If no indentation present in file, defaults to 2."
  (or (--> lines-without-text
        (--map (list (s-count-matches "\s" it) (s-count-matches "\t" it)) it)
        (let ((spaces-ind (-sort '< (--remove (eq 0 it) (-map 'c/first it))))
              (tabs-ind (-sort '< (--remove (eq 0 it) (-map 'c/second it)))))
          (if (> (length spaces-ind) (length tabs-ind))
              (c/first spaces-ind)
            (c/first tabs-ind))))
      2))

(defun c/convert-tabs-to-spaces (line-without-text n)
  "Replace tabs in LINE-WITHOUT-TEXT with N spaces."
  (s-replace "\t" (make-string n ?\s) line-without-text))

(defun c/calculate-complexity (line-without-text indentation)
  "Calculate indentation complexity by dividing length of LINE-WITHOUT-TEXT by INDENTATION."
  (/ (+ 0.0 (length line-without-text)) indentation))

(defun c/as-logical-indents (lines &optional opts)
  "Calculate logical indentations of LINES. Try to infer how many space is an indent unless OPTS provides it."
  (let ((indentation (or opts (c/find-indentation lines))))
    (list
     (--map
      (--> it
        (c/convert-tabs-to-spaces it indentation)
        (c/calculate-complexity it indentation))
      lines)
     indentation)))

(defun c/stats-from (complexities-indentation)
  "Return stats from COMPLEXITIES-INDENTATION."
  (let* ((complexities (c/first complexities-indentation))
         (mean (/ (-sum complexities) (length complexities)))
         (sd (sqrt (/ (-sum (--map (expt (- it mean) 2) complexities)) (length complexities)))))
    `((total . ,(-sum complexities))
      (n-lines . ,(length complexities))
      (max . ,(-max complexities))
      (mean . ,mean)
      (standard-deviation . ,sd)
      (used-indentation . ,(c/second complexities-indentation)))))

(defun c/calculate-complexity-stats (code &optional opts)
  "Return complexity of CODE based on indentation. If OPTS is provided, use these settings to define what is the indentation."
  (--> code
    ;; TODO maybe add line numbers, so that I can also open the most troublesome (max-c) line automatically?
    c/split-on-newlines
    c/remove-empty-lines
    c/remove-text-after-indentation
    (c/as-logical-indents it opts)
    c/stats-from))

(defun c/calculate-complexity-current-buffer (&optional indentation)
  "Calculate complexity of the current buffer contents.
Optionally you can provide the INDENTATION level of the file. The
code can infer it automatically."
  (interactive)
  (message "%s"
           (c/calculate-complexity-stats
            (buffer-substring-no-properties (point-min) (point-max)) indentation)))

;; END indentation

;; BEGIN complexity over commits

(defun c/retrieve-commits-up-to-date-touching-file (file &optional date)
  "Retrieve list of commits touching FILE from DATE."
  (s-split
   "\n"
   (shell-command-to-string
    (s-concat
     "git log --format=format:%H --reverse "
     (if date
         (s-concat "--after=" date " ")
       "")
     file))))

(defun c/retrieve-file-at-commit-with-git (file commit)
  "Retrieve FILE contents at COMMIT."
  (let ((git-file
         (s-join
          "/"
          (cdr
           (--drop-while
            (not
             (string= it (c/third (reverse (s-split "/" (magit-git-dir))))))
            (s-split "/" file))))))
    (shell-command-to-string (format "git show %s:\"%s\"" commit git-file))))

(defun c/git-hash-to-date (commit)
  "Return the date of the COMMIT. Note this is the date of merging in, not of the code change."
  (s-replace "\n" "" (shell-command-to-string (s-concat "git show --no-patch --no-notes --pretty='%cd' --date=short " commit))))

(defun c/calculate-complexity-over-commits (file &optional opts)
  (--> (call-interactively 'c/request-date)
    (c/retrieve-commits-up-to-date-touching-file file it)
    (--map
     (--> it
       (list it (c/retrieve-file-at-commit-with-git file it))
       (list (c/first it) (c/calculate-complexity-stats (c/second it) opts)))
     it)))

(defun c/plot-csv-file-with-graph-cli (file)
  "Plot CSV FILE with graph-cli."
  (shell-command
   (format "graph %s" file)))

(defun c/plot-lines-with-graph-cli (data)
  "Plot DATA from lists as a graph."
  (let ((tmp-file "/tmp/data-file-graph-cli.csv"))
    (with-temp-file tmp-file
      (insert "commit-date,total-complexity,loc\n")
      (insert (s-join "\n" (--map (s-replace-all '((" " . ",") ("(" . "") (")" . "")) (format "%s" it)) data))))
    (c/plot-csv-file-with-graph-cli tmp-file)))

(defun c/show-complexity-over-commits (file &optional opts)
  "Make a graph plotting complexity out of a FILE. Optionally give file indentation in OPTS."
  (interactive (list (read-file-name "Select file:" nil nil nil (buffer-file-name))))
  (c/plot-lines-with-graph-cli
   (--map
    (list (c/git-hash-to-date (c/first it)) (alist-get 'total (c/second it)) (alist-get 'n-lines (c/second it)))
    (c/calculate-complexity-over-commits file opts))))

;; END complexity over commits

;; BEGIN code churn
(defun c/produce-code-maat-abs-churn-report (repository)
  "Create code-maat abs-churn report for REPOSITORY."
  (c/run-code-maat "abs-churn" repository)
  repository)

(defun c/show-code-churn-sync (repository date)
  "Show how much code was added and removed from REPOSITORY from a DATE."
  (interactive (list
                (read-directory-name "Choose git repository directory:" (vc-root-dir))
                (call-interactively 'c/request-date)))
  (c/in-temp-directory
   repository
   (--> repository
        (c/produce-git-report it date)
        c/produce-code-maat-abs-churn-report
        "abs-churn.csv"
        c/plot-csv-file-with-graph-cli)))

(defun c/show-code-churn (repository date)
  "Show how much code was added and removed from REPOSITORY from a DATE."
  (interactive (list
                (read-directory-name "Choose git repository directory:" (vc-root-dir))
                (call-interactively 'c/request-date)))
  (c/async-run 'c/show-code-churn-sync repository date nil 't))
;; END complexity over commits

;; BEGIN change coupling
(defun c/produce-code-maat-coupling-report (repository)
  "Create code-maat coupling report for REPOSITORY."
  (c/run-code-maat "coupling" repository)
  repository)

(defun c/generate-coupling-json-script (repository)
  "Generate script to produce a weighted graph for REPOSITORY."
  (c/copy-file "./scripts/coupling_csv_as_edge_bundling.py" (c/temp-dir repository))
  repository)

(defun c/produce-coupling-json (repository)
  "Produce coupling json needed by d3 for REPOSITORY."
  (message "Produce coupling json...")
  (shell-command
   "python3 coupling_csv_as_edge_bundling.py --coupling coupling.csv > edgebundling.json")
  repository)

(defun c/generate-host-edge-bundling-html (repository)
  "Generate host html from REPOSITORY."
  (c/copy-file "./pages/edge-bundling/script.js" (c/temp-dir repository))
  (c/copy-file "./pages/edge-bundling/style.css" (c/temp-dir repository))
  (c/copy-file "./pages/edge-bundling/zoomable.html" (c/temp-dir repository))
  repository)

(defun c/show-coupling-graph-sync (repository date &optional port)
  "Show REPOSITORY edge bundling synchronously for code coupling up to DATE. Serve graph on PORT."
  (interactive (list
                (read-directory-name "Choose git repository directory:" (vc-root-dir))
                (call-interactively 'c/request-date)))
  (c/in-temp-directory
   repository
   (--> repository
        (c/produce-git-report it nil date)
        c/produce-code-maat-coupling-report
        c/generate-coupling-json-script
        c/generate-d3-v4-lib
        c/produce-coupling-json
        c/generate-host-edge-bundling-html
        (c/run-server-and-navigate it port))))

(defun c/show-coupling-graph (repository date &optional port)
  "Show REPOSITORY edge bundling for code coupling up to DATE. Serve graph on PORT."
  (interactive (list
                (read-directory-name "Choose git repository directory:" (vc-root-dir))
                (call-interactively 'c/request-date)))
  (c/async-run 'c/show-coupling-graph-sync repository date port))
;; END change coupling

;; BEGIN find coupled files
(defun c/add-filename-to-analysis-columns (repository analysis)
  "Add filepath from REPOSITORY to ANALYSIS columns."
  (--> analysis
    (s-split "\n" it)
    (--remove (s-blank? (s-trim it)) it)
    (-concat
     (list (car it))
     (--map
      (--> (s-split "," it)
        (-concat
         (list (s-concat repository "/" (c/first it)))
         (list
          (if (or (null (c/second it)) (not (s-contains-p "/" (c/second it))))
              (c/second it)
            (s-concat repository "/" (c/second it))))
         (cdr (cdr it)))
        (s-join "," it))
      (cdr it)))))

(defun c/get-coupling-alist-sync (repository)
  "Get list of coupled files in REPOSITORY async."
  (c/in-temp-directory
   repository
   (--> repository
     (c/produce-git-report it nil)
     c/produce-code-maat-coupling-report
     (c/get-analysis-as-string-from-csv it "coupling")
     (c/add-filename-to-analysis-columns repository it)
     (--map (s-split "," it) (cdr it)))))

(defun c/get-coupling-alist (repository fun)
  "FUN takes a list of coupled files in REPOSITORY."
  (async-start
   `(lambda ()
      (setq load-path ',load-path)
      (load-file ,(symbol-file 'c/get-coupling-alist))
      (c/get-coupling-alist-sync ,repository))
   fun))

(defcustom c/calculate-coupling-project-key-fn
  'identity
  "Function taking a REPOSITORY path and returning a string.")

(defvar c/coupling-project-map
  (make-hash-table :test 'equal)
  "Hash table to contain coupling files list.")

(defun c/get-coupled-files-alist (repository fun)
  "Run FUN on the coupled files for REPOSITORY."
  (let* ((key (funcall c/calculate-coupling-project-key-fn repository))
         (c/files (gethash key c/coupling-project-map)))
    (if c/files
        (funcall fun c/files)
      (progn
        (message "Building coupling cache asynchronously...")
        (c/get-coupling-alist
         repository
         `(lambda (result-files)
            (puthash ,key result-files c/coupling-project-map)
            (funcall ,fun result-files)))))))

(defun c/clear-coupling-project-map ()
  "Clear `c/coupling-project-hash'."
  (interactive)
  (clrhash c/coupling-project-map))

(defun c/get-coupled-files-alist-hook-fn ()
  "Calculate coupled files asynchronously."
  (let ((git-root (ignore-errors (vc-root-dir))))
    (when git-root
      (c/get-coupled-files-alist
       git-root
       `(lambda (x)
          (message
           "Finished to update coupled files for %s and found %s coupled files."
           ,git-root
           (length x)))))))

(defun c/find-coupled-files ()
  "Allow user to choose files coupled according to previous changes."
  (interactive)
  (let ((choose-file
         (lambda (files)
           (--> files
             (--sort (> (string-to-number (nth 3 it)) (string-to-number (nth 3 other))) files) ;; sort by number of commits
             (--sort (> (string-to-number (nth 2 it)) (string-to-number (nth 2 other))) files) ;; sort then by how often this file has changed
             (-map (lambda (file)
                     (when (or (string= (file-truename (buffer-file-name)) (file-truename (car file)))
                               (string= (file-truename (buffer-file-name)) (file-truename (nth 1 file))))
                       (car
                        (--remove (string= (file-truename (buffer-file-name)) (file-truename it)) (-take 2 file)))))
                   it)
             (-remove 'null it)
             (if (null it)
                 (message "No coupled file found!")
               (completing-read "Find coupled file: " it nil 't))))))
    (--> (vc-root-dir)
      (c/get-coupled-files-alist it choose-file)
      (when (f-file-p it) (find-file it)))))
;; END find coupled files

;; BEGIN code communication
(defun c/produce-code-maat-communication-report (repository)
  "Create code-maat communication report for REPOSITORY."
  (c/run-code-maat "communication" repository)
  repository)

(defun c/generate-communication-json-script (repository)
  "Generate script to produce a weighted graph for REPOSITORY."
  (c/copy-file "./scripts/communication_csv_as_edge_bundling.py" (c/temp-dir repository))
  repository)

(defun c/produce-communication-json (repository)
  "Generate REPOSITORY age json."
  (message "Produce age json...")
  (shell-command
   "python3 communication_csv_as_edge_bundling.py --communication communication.csv > edgebundling.json")
  repository)

(defun c/show-code-communication-sync (repository date &optional port)
  "Show REPOSITORY edge bundling for code communication from DATE. Green edges is incoming (dependant) and red outgoing (dependencies).Optionally set the PORT on which to serve the graph."
  (interactive
   (list
    (read-directory-name "Choose git repository directory:" (vc-root-dir))
    (call-interactively 'c/request-date)))
  (c/in-temp-directory
   repository
   (--> repository
        (c/produce-git-report it date)
        c/produce-code-maat-communication-report
        c/generate-communication-json-script
        c/generate-d3-v4-lib
        c/produce-communication-json
        c/generate-host-edge-bundling-html
        (c/run-server-and-navigate it port))))

(defun c/show-code-communication (repository date &optional port)
  "Show REPOSITORY edge bundling for code communication from DATE. Optionally define PORT on which to serve graph."
  (interactive
   (list
    (read-directory-name "Choose git repository directory:" (vc-root-dir))
    (call-interactively 'c/request-date)))
  (c/async-run 'c/show-code-communication-sync repository date port))
;; END code communication

;; BEGIN code knowledge
(defun c/produce-code-maat-main-dev-report (repository)
  "Create code-maat main-dev report for REPOSITORY."
  (c/run-code-maat "main-dev" repository)
  repository)

(defun c/generate-knowledge-json-script (repository)
  "Generate python script."
  (c/copy-file "./scripts/knowledge_csv_as_enclosure_diagram.py" (c/temp-dir repository))
  repository)

(defun c/produce-knowledge-json (repository)
  "Generate REPOSITORY age json."
  (message "Produce knowledge json...")
  (shell-command
   "python3 knowledge_csv_as_enclosure_diagram.py --structure cloc.csv --owners main-dev.csv --authors authors.csv > knowledge.json")
  repository)

(defun c/insert-authors-colors-in-file (authors-colors repository)
  (with-temp-file "authors.csv"
    (insert "author,color\n")
    (apply 'insert (--map (s-concat (car it) "," (cdr it) "\n") authors-colors))))

(defcustom c/authors-colors (list
                             "red"
                             "blue"
                             "orange"
                             "gray"
                             "green"
                             "violet"
                             "pink"
                             "brown"
                             "aquamarine"
                             "blueviolet"
                             "burlywood"
                             "cadetblue"
                             "chartreuse"
                             "chocolate"
                             "coral"
                             "cornflowerblue"
                             "cyan"
                             "darkblue"
                             "darkcyan"
                             "darkgoldenrod"
                             "darkgray"
                             "darkgreen"
                             "darkkhaki"
                             "darkmagenta"
                             "darkolivegreen"
                             "darkorange"
                             "darkorchid"
                             "darkred"
                             "darksalmon"
                             "darkseagreen"
                             "darkslateblue"
                             "darkslategray"
                             "darkturquoise"
                             "darkviolet"
                             "deeppink"
                             "deepskyblue"
                             "dimgray"
                             "dodgerblue"
                             "firebrick"
                             "forestgreen"
                             "fuchsia"
                             "gold"
                             "goldenrod"
                             "greenyellow"
                             "hotpink"
                             "indianred"
                             "indigo"
                             "lawngreen"
                             "lightcoral"
                             "lightgray"
                             "lightgreen"
                             "lightpink"
                             "lightsalmon"
                             "lightseagreen"
                             "lightskyblue"
                             "lightslategray"
                             "lightsteelblue"
                             "lime"
                             "limegreen"
                             "linen"
                             "magenta"
                             "maroon"
                             "mediumaquamarine"
                             "mediumblue"
                             "mediumorchid"
                             "mediumpurple"
                             "mediumseagreen"
                             "mediumslateblue"
                             "mediumspringgreen"
                             "mediumturquoise"
                             "mediumvioletred"
                             "midnightblue"
                             "mintcream"
                             "mistyrose"
                             "moccasin"
                             "navajowhite"
                             "navy"
                             "oldlace"
                             "olive"
                             "olivedrab"
                             "orangered"
                             "orchid"
                             "palegoldenrod"
                             "palegreen"
                             "paleturquoise"
                             "palevioletred"
                             "papayawhip"
                             "peachpuff"
                             "peru"
                             "plum"
                             "powderblue"
                             "purple"
                             "rosybrown"
                             "royalblue"
                             "saddlebrown"
                             "salmon"
                             "sandybrown"
                             "seagreen"
                             "seashell"
                             "sienna"
                             "silver"
                             "skyblue"
                             "slateblue"
                             "slategray"
                             "snow"
                             "springgreen"
                             "steelblue"
                             "tan"
                             "teal"
                             "thistle"
                             "tomato"
                             "turquoise"
                             "wheat"
                             "whitesmoke"
                             "yellow"
                             "yellowgreen")
  "Colors to use for authors.")

(defun c/generate-list-authors-colors (repository)
  "Generate list of authors of REPOSITORY."
  (--> (s-concat "cd " repository "; git shortlog HEAD -s -n | uniq | cut -f 2")
    shell-command-to-string
    (s-split "\n" it)
    (--remove (s-blank? (s-trim it)) it)
    (-zip it c/authors-colors)
    (c/insert-authors-colors-in-file it repository))
  repository)

(defun c/generate-host-knowledge-enclosure-diagram-html (repository)
  "Generate host html from REPOSITORY."
  (c/copy-file "./pages/knowledge-enclosure-diagram/script.js" (c/temp-dir repository))
  (c/copy-file "./pages/knowledge-enclosure-diagram/style.css" (c/temp-dir repository))
  (c/copy-file "./pages/knowledge-enclosure-diagram/zoomable.html" (c/temp-dir repository))
  repository)

(defun c/show-knowledge-graph-sync (repository date &optional port)
  "Show REPOSITORY enclosure diagram for code knowledge up to DATE. Optionally define PORT on which to serve graph."
  (interactive (list
                (read-directory-name "Choose git repository directory:" (vc-root-dir))
                (call-interactively 'c/request-date)))
  (c/in-temp-directory
   repository
   (--> repository
        (c/produce-git-report it date)
        c/produce-code-maat-main-dev-report
        c/produce-cloc-report
        c/generate-knowledge-json-script
        c/generate-d3-v3-lib
        c/generate-list-authors-colors
        c/produce-knowledge-json
        c/generate-host-knowledge-enclosure-diagram-html
        (c/run-server-and-navigate it port))))

(defun c/show-knowledge-graph (repository date &optional port)
  "Show REPOSITORY enclosure diagram for code knowledge up to DATE. Optionally define PORT on which to serve graph."
  (interactive (list
                (read-directory-name "Choose git repository directory:" (vc-root-dir))
                (call-interactively 'c/request-date)))
  (c/async-run 'c/show-knowledge-graph-sync repository date port))
;; END code knowledge

;; BEGIN code stability
(defun c/produce-code-maat-age-report (repository)
  "Create code-maat age report for REPOSITORY."
  (c/run-code-maat "age" repository)
  repository)

(defun c/generate-age-json-script (repository)
  "Generate python script for REPOSITORY."
  (c/copy-file "./scripts/code_age_csv_as_enclosure_json.py" (c/temp-dir repository))
  repository)

(defun c/produce-age-json (repository)
  "Generate REPOSITORY age json."
  (message "Produce age json...")
  (shell-command
   "python3 code_age_csv_as_enclosure_json.py --structure cloc.csv --weights age.csv > age.json")
  repository)

(defun c/generate-host-age-enclosure-diagram-html (repository)
  "Generate host html from REPOSITORY."
  (c/copy-file "./pages/age-enclosure-diagram/script.js" (c/temp-dir repository))
  (c/copy-file "./pages/age-enclosure-diagram/style.css" (c/temp-dir repository))
  (c/copy-file "./pages/age-enclosure-diagram/zoomable.html" (c/temp-dir repository))
  repository)

(defun c/show-code-age-sync (repository date &optional port)
  "Show REPOSITORY enclosure diagram for code stability/age up to DATE. Optionally define PORT on which to serve graph."
  (interactive (list
                (read-directory-name "Choose git repository directory:" (vc-root-dir))
                (call-interactively 'c/request-date)))
  (c/in-temp-directory
   repository
   (--> repository
     (c/produce-git-report it date)
     c/produce-code-maat-age-report
     c/produce-cloc-report
     c/generate-age-json-script
     c/generate-d3-v3-lib
     c/produce-age-json
     c/generate-host-age-enclosure-diagram-html
     (c/run-server-and-navigate it port))))

(defun c/show-stability-graph (repository date &optional port)
  "Show REPOSITORY enclosure diagram for code stability up to DATE. Optionally define PORT on which to serve graph."
  (interactive (list
                (read-directory-name "Choose git repository directory:" (vc-root-dir))
                (call-interactively 'c/request-date)))
  (c/async-run 'c/show-code-age-sync repository date port))
;; END code stability

;; BEGIN code fragmentation
(defun c/produce-code-maat-entity-ownership-report (repository)
  "Create code-maat entity-ownership report for REPOSITORY."
  (c/run-code-maat "entity-ownership" repository)
  repository)

(defun c/get-analysis-as-string-from-csv (repository analysis)
  "Get ANALYSIS in csv of REPOSITORY as text."
  (with-temp-buffer
    (insert-file-contents-literally (s-concat analysis ".csv"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun c/generate-pie-script (repository)
  "Generate python script for REPOSITORY."
  (c/copy-file "./scripts/csv-to-pie-graph.py" (c/temp-dir repository))
  repository)

(defcustom c/pie-or-bar-chart-command "python3 csv-to-pie-graph.py %s" "Command to visualize chart" :type 'string :options '("python3 csv-to-pie-graph.py %s" "graph %s --bar --width 0.4 --offset='-0.2,0.2'"))

(defun c/show-pie-chart-command (file)
  "Show pie chart of CSV FILE."
  (format c/pie-or-bar-chart-command file))

(defun c/sum-by-first-column (rows)
  (let (result)
    (dolist (row rows)
      (let* ((key (car row))
             (value (cdr row))
             (mapping (assoc key result)))
        (if mapping
            (setcdr mapping (+ (cdr mapping) value))
          (push (cons key value) result))))
    result))

(defun c/show-fragmentation-sync (path &optional date)
  "Show knowledge fragmentation for PATH."
  (interactive "fShow fragmentation for:")
  (let* ((path (file-truename path))
         (repository (s-trim (shell-command-to-string (format "cd %s; git rev-parse --show-toplevel" (file-name-directory path))))))
    (c/in-temp-directory
     repository
     (--> repository
       (c/produce-git-report it date)
       c/produce-code-maat-entity-ownership-report
       c/generate-pie-script
       (c/get-analysis-as-string-from-csv repository "entity-ownership")
       (c/add-filename-to-analysis-columns repository it)
       (print it)
       (--filter (s-starts-with-p path it) it)
       (--map
        (--> (s-split "," it)
          (cons (nth 1 it) (+ (string-to-number (nth 2 it)) (string-to-number (nth 3 it)))))
        it)
       (c/sum-by-first-column it)
       (--map
        (--> it (format "%s,%s\n" (car it) (cdr it)))
        it)
       (cons "author,+&-lines\n" it)
       (with-temp-file "fragmentation.csv"
         (apply 'insert it)))
     (shell-command (c/show-pie-chart-command "fragmentation.csv")))))

(defun c/show-fragmentation (path)
  "Show knowledge fragmentation for PATH."
  (interactive "fShow fragmentation for:")
  (c/async-run 'c/show-fragmentation-sync path nil nil 't))
;; END code fragmentation

;; BEGIN word analysis
;; taken from: https://emacs.stackexchange.com/questions/13514/how-to-obtain-the-statistic-of-the-the-frequency-of-words-in-a-buffer
(defvar c/punctuation-marks '(","
                              "."
                              "'"
                              "&"
                              "\"")
  "List of Punctuation Marks that you want to count.")

(defun c/count-raw-word-list (raw-word-list)
  "Produce a dictionary of RAW-WORD-LIST with the number of occurrences for each word."
  (--> raw-word-list
    (--reduce-from
     (progn
       (incf (cdr (or (assoc it acc)
                      (c/first (push (cons it 0) acc)))))
       acc)
     nil
     it)
    (sort it (lambda (a b) (string< (car a) (car b))))))

(defun c/word-stats (string)
  "Return word (as a token between spaces) frequency in STRING."
  (let* ((words (split-string
                 (downcase string)
                 (format "[ %s\f\t\n\r\v]+"
                         (mapconcat #'identity c/punctuation-marks ""))
                 t))
         (punctuation-marks (--filter
                             (member it c/punctuation-marks)
                             (split-string string "" t)))
         (raw-word-list (append punctuation-marks words))
         (word-list (c/count-raw-word-list raw-word-list)))
    (sort word-list (lambda (a b) (> (cdr a) (cdr b))))))

(defun c/word-stats-to-csv-string (string &optional order-fn)
  "Produce occurrences csv table for words in STRING, optionally sorting the table according to ORDER-FN."
  (--> string
    c/word-stats
    (--filter (> (length (car it)) 2) it)
    (sort it (or order-fn (lambda (a b) (> (cdr a) (cdr b)))))
    (--map (format "'%s',%d\n" (car it) (cdr it)) it)
    (apply 's-concat it)
    (s-concat "word,occurences\n\n" it)))

(defun c/word-statistics (string &optional order-fn)
  "Produce a buffer with word statistics from STRING, optionally define ORDER-FN (for example to see the ones appearing less first)."
  (interactive
   (list (buffer-substring-no-properties (point-min) (point-max))))
  (with-current-buffer (get-buffer-create "*word-statistics*")
    (erase-buffer)
    (insert (c/word-stats-to-csv-string string order-fn)))
  (pop-to-buffer "*word-statistics*")
  (goto-char (point-min)))

(defun c/word-semantics (string)
  "Produce a buffer with the words least used, which typically contain the most semantics from STRING."
  (interactive
   (list (buffer-substring-no-properties (point-min) (point-max))))
  (c/word-statistics string (lambda (a b) (< (cdr a) (cdr b)))))

(defun c/word-analysis-commits (arg)
  "Show the frequency of words used in commits messages. When ARG is set show only history for given file."
  (interactive "P")
  (--> (shell-command-to-string (s-concat
                                 "git log --pretty=format:\"%s\""
                                 (when arg (format " %s" (read-file-name "Analyze history of:")))))
    c/word-statistics))

(defun c/word-analysis-region ()
  "Show the frequency of words in a region."
  (interactive)
  (when (region-active-p)
    (--> (buffer-substring-no-properties (region-beginning) (region-end))
      c/word-statistics)))

(defun c/word-analysis-region-graph ()
  "Show the frequency graph for words in region."
  (interactive)
  (when (region-active-p)
    (--> (buffer-substring-no-properties (region-beginning) (region-end))
      c/word-stats-to-csv-string
      (with-temp-file "/tmp/word-stats.csv"
        (insert it))
      (shell-command "graph --bar --xtick-angle 90 /tmp/word-stats.csv"))))


;; END word analysis

;; BEGIN churn icon
(defcustom c/icon-trends
  '(:period "3"
            :always-additions (propertize "⬆" 'face `(:background "DarkOrange"))
            :always-deletions (propertize "⬇" 'face `(:background "SpringGreen"))
            :more-additions   (propertize "↗" 'face `(:background "Gold"))
            :more-deletions   (propertize "↘" 'face `(:background "GreenYellow")))
  "Icon and period of evaluation for trend.")

(defun c/calculate-added-delete-lines (file n-weeks-ago)
  "Calculate added and deleted lines for FILE from N-WEEKS-AGO."
  (--> "git log --since \"%s weeks ago\" --numstat --oneline %s "
    (format it n-weeks-ago file)
    (shell-command-to-string it)
    (s-split "\n" it)
    (--map (s-split "\t" it) it)
    (--filter (> (length it) 2) it)
    (--reduce-from
     (list
      :additions (+ (string-to-number (nth 0 it)) (plist-get acc :additions))
      :deletions (+ (string-to-number (nth 1 it)) (plist-get acc :deletions)))
     (list :additions 0 :deletions 0)
     it)))

(defcustom c/display-icon 't "Display an icon in modeline showing growth trend of code. A pointing up icon means the code has been growing, a pointing down arrow has been decreasing.")

(defun c/display-icon ()
  "Display icon for buffer according to the previous history."
  (when (and c/display-icon (vc-root-dir))
    (let* ((additions-deletions
            (c/calculate-added-delete-lines (buffer-file-name) (plist-get c/icon-trends :period)))

           (additions (plist-get additions-deletions :additions))
           (deletions (plist-get additions-deletions :deletions))
           (icon-key (cond
                      ((eq deletions 0) :always-additions)
                      ((eq additions 0) :always-deletions)
                      ((> additions deletions) :more-additions)
                      ('otherwise :more-deletions)))
           (icon (plist-get c/icon-trends icon-key)))
      (c/remove-icon)
      (setq-local mode-line-format (cons  `(:eval ,(plist-get c/icon-trends icon-key)) mode-line-format)))))

(defun c/remove-icon ()
  "Remove icon."
  (setq-local mode-line-format (--remove (-contains-p (--remove (symbolp it) c/icon-trends) (plist-get it :eval)) mode-line-format)))

(defun c/display-icon-delayed ()
  "Display icon function for `prog-mode-hook'."
  (run-with-timer 0.1 nil 'c/display-icon))

(add-hook 'prog-mode-hook 'c/display-icon-delayed)
;; END churn icon
;; BEGIN wrapper gource
(defcustom c/gource-command
  "gource"
  "Command to the gource utility. See https://gource.io/ for more information on how to install."
  :type 'string)

(defun c/show-gource (repository date)
  "Open gource for REPOSITORY from DATE."
  (interactive
   (list
    (read-directory-name "Choose git repository directory:" (vc-root-dir))
    (call-interactively 'c/request-date)))
  (if (executable-find c/gource-command)
      (async-shell-command (format "cd %s; %s --start-date %s -seconds-per-day 0.5" repository c/gource-command date))
    (message (format "Sorry, cannot find executable (%s). Try change the value of `c/gource-command'" c/gource-command))))
;; END wrapper gource
(provide 'code-compass)
;;; code-compass ends here



;; Local Variables:
;; time-stamp-pattern: "10/Version:\\?[ \t]+1.%02y%02m%02d\\?\n"
;; End:
