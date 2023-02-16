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
  (should
   (string= (code-compass--calculate-complexity-stats "") nil)))

(ert-deftest c/calculate-complexity-stats_return-stats ()
  (should
   (equal
    (code-compass--calculate-complexity-stats
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

;; Just evaluate buffer to run tests.
(ert--stats-passed-expected (ert-run-tests 't (lambda (&rest args))))

;;; code-compass ends here
