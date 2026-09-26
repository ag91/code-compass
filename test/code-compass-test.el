;;; code-compass.el --- Tests for code-compass.

;; Copyright (C) 2020 Andrea Giugliano

;; Author: Andrea Giugliano <agiugliano@live.it>

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

;; Tests for code-compass
;;

;;; Code:


(ert-deftest c/expand-file-name_expanded-file-name ()
  (let ((default-directory "/tmp")
        (code-compass-path-to-code-compass "somePath"))
    (should
     (string= (code-compass--expand-file-name "someFile") "/tmp/somePath/someFile"))))

(defun utils/parse-string-as-time (date-string)
  "Parse DATE-STRING as time."
  (let ((l (parse-time-string date-string)))
    (encode-time 0 0 0 (nth 3 l) (nth 4 l) (nth 5 l)))
  )

(ert-deftest c/subtract-to-now_return-the-day-before ()
  (should
   (string=
    (format-time-string
     "%Y-%m-%d"
     (code-compass--subtract-to-now
      1
      24
      (utils/parse-string-as-time "2021-01-02")))
    "2021-01-01")))

(ert-deftest c/request-date_git-date ()
  (should
   (string=
    (code-compass-request-date "1d" (utils/parse-string-as-time "2021-01-02"))
    "2021-01-01")))

(ert-deftest c/temp-dir_give-temp-dir-for-analyses ()
  (should
   (string= (code-compass--temp-dir "someRepo") "/tmp/code-compass-someRepo/")))

(ert-deftest c/in-temp-directory_run-in-directory ()
  (should
   (string= (code-compass--in-temp-directory "someDir" default-directory) "/tmp/code-compass-someDir/")))

(ert-deftest c/calculate-complexity-stats_empty-stats ()
  (should-not (code-compass-calculate-complexity-stats "")))

(ert-deftest c/calculate-complexity-stats_return-stats ()
  (should
   (equal
    (code-compass-calculate-complexity-stats
     "
(let ((x 1))
  (let ((y 2))
    (let ((z 3)))
      (+ x y z)))")
    '((total . 6.0)
      (n-lines . 4)
      (max . 3.0)
      (mean . 1.5)
      (standard-deviation . 1.118033988749895)
      (used-indentation . 2)))))

(ert-deftest c/add-filename-to-analysis-columns_prefix-lines ()
  (should
   (equal
    (code-compass--add-filename-to-analysis-columns
     "someRepo"
     "some,analysis\nsomeEntry,someData\n\n")
    '("some,analysis" "someRepo/someEntry,someData"))))

(ert-deftest c/sum-by-first-column_sums-rows-first-column ()
  (should
   (equal (code-compass--sum-by-first-column
           '(("a" . 1)
             ("b" . 1)
             ("a" . 1)
             ("c" . 1)
             ("c" . 1)
             ("c" . 1)))
          '(("c" . 3) ("b" . 1) ("a" . 2)))))

(ert-deftest c/word-stats_distribution-of-words ()
  (should
   (equal
    (code-compass--word-stats "hi, hi, hi, hello, hello\nbla")
    '(("," . 4) ("hi" . 3) ("hello" . 2) ("bla" . 1)))))

(ert-deftest c/parse-git-log-metrics_per-file-facts ()
  (let ((m (code-compass--parse-git-log-metrics
            "--1a2b3c--2026-01-02--andi\nsrc/a.el\n--2b3c4d--2026-01-04--andi\nsrc/a.el\n--4d5e6f--2026-01-03--bob\nsrc/a.el\nsrc/b.el\n--5e6f7a--2026-01-05--renovate[bot]\nsrc/b.el\n\n--6f7a8b--2026-01-06--andi\nsrc/b.el")))
    (should (equal (hash-table-count m) 2))
    (should (equal (plist-get (gethash "src/a.el" m) :revisions) 3))
    (should (equal (plist-get (gethash "src/a.el" m) :authors)
                   '(("andi" . 2) ("bob" . 1))))
    (should (equal (plist-get (gethash "src/a.el" m) :main-dev) "andi"))
    (should (equal (plist-get (gethash "src/a.el" m) :last-touch) "2026-01-04"))
    (should (equal (plist-get (gethash "src/b.el" m) :revisions) 3))))

(ert-deftest c/parse-git-log-metrics_ignore-authors ()
  (let ((m (code-compass--parse-git-log-metrics
            "--1a2b3c--2026-01-02--andi\nsrc/a.el\n--4d5e6f--2026-01-03--renovate[bot]\nsrc/b.el\nsrc/c.el"
            "renovate")))
    (should (equal (hash-table-count m) 1))
    (should (equal (plist-get (gethash "src/a.el" m) :revisions) 1))
    (should-not (gethash "src/b.el" m))
    (should-not (gethash "src/c.el" m))))

(ert-deftest c/parse-git-log-metrics_empty-log ()
  (should (equal (hash-table-count
                  (code-compass--parse-git-log-metrics ""))
                 0)))

(ert-deftest c/parse-git-log-metrics_merge-commits-count-nothing ()
  (let ((m (code-compass--parse-git-log-metrics
            "--1a2b3c--2026-01-02--andi\nsrc/a.el\n--4d5e6f--2026-01-03--bob")))
    (should (equal (hash-table-count m) 1))
    (should (equal (plist-get (gethash "src/a.el" m) :revisions) 1))))

(ert-deftest c/file-complexity_reads-a-file ()
  (should
   (equal (let ((f (make-temp-file "cc-metrics-" nil ".el"
                    "(let ((x 1))\n  (let ((y 2))\n    (let ((z 3)))\n      (+ x y z)))\n")))
            (prog1 (cdr (assq 'max (code-compass-file-complexity f)))
              (delete-file f)))
          3.0)))

(ert-deftest c/file-complexity_missing-file_nil ()
  (should-not (code-compass-file-complexity "/nonexistent/no-such-file.el")))

;; Just evaluate buffer to run tests.
(ert--stats-passed-expected (ert-run-tests 't (lambda (&rest args))))

;;; code-compass ends here
