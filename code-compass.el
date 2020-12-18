;;; code-compass.el --- Make Emacs your compass in a sea of software complexity.

;; Copyright (C) 2020 Andrea Giugliano

;; Author: Andrea Giugliano <agiugliano@live.it>
;; Version: 0.0.1
;; Package-Version: 20201215
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


(defgroup code-compass nil
  "Options specific to code-compass."
  :tag "code-compass"
  :group 'code-compass)

(defcustom c/default-periods
  '("beginning" "1d" "2d" "3d" "6d" "12d" "18d" "24d" "1m" "2m" "6m")
  "A list of choices for starting date for reducing the Git log for analysis. 'beginning' is a keyword to say to not reduce.'Nd' means to start after N days, where N is a positive number. 'Nm' means to start after N months, where N is a positive number."
  :group 'code-compass)

(defcustom c/code-maat-command
  "docker run -v /tmp/:/data code-maat-app"
  "Command to run Code-maat (https://github.com/adamtornhill/code-maat). Currently defaults to use docker because easier to setup."
  :group 'code-compass)

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

(defun c/produce-git-report (repository date)
  "Create git report for REPOSITORY with a Git log starting at DATE."
  (interactive
   (list (call-interactively 'c/request-date)))
  (message "Producing git report...")
  (shell-command
   (s-concat
    (format "cd %s;" repository)
    "git log --all --numstat --date=short --pretty=format:'--%h--%ad--%aN' --no-renames "
    (if date
        (format
         "--after=%s "
         date)
      "")
    (format
     "> /tmp/%s.log"
     (f-filename repository))))
  repository)

(defun c/run-code-maat (command repository)
  "Run code-maat's COMMAND on REPOSITORY."
  (message "Producing code-maat %s report for %s..." command repository)
  (shell-command
   (format
    "%s -l /data/%s.log -c git2 -a %s > /tmp/%s-%s.csv"
    c/code-maat-command
    (f-filename repository)
    command
    (f-filename repository)
    command)))

(defun c/produce-code-maat-revisions-report (repository)
  "Create code-maat revisions report for REPOSITORY."
  (c/run-code-maat "revisions" repository)
  repository)

(defun c/produce-cloc-report (repository)
  "Create cloc report for REPOSITORY."
  (message "Producing cloc report...")
  (shell-command
   (format "cd %s; cloc ./ --by-file --csv --quiet --report-file=/tmp/cloc-%s.csv" repository (f-filename repository)))
  repository)

(defun c/generate-merger-script (repository)
  "Generate a Python script to give weights to the circle diagram of REPOSITORY."
  (with-temp-file "/tmp/csv_as_enclosure_json.py"
    (insert
     "
#!/bin/env python

#######################################################################
## This program generates a JSON document suitable for a D3.js
## enclosure diagram visualization.
## The input data is read from two CSV files:
##  1) The complete system structure, including size metrics.
##  2) A hotspot analysis result used to assign weights to the modules.
#######################################################################

import argparse
import csv
import json
import sys

class MergeError(Exception):
        def __init__(self, message):
                Exception.__init__(self, message)

class Merged(object):
        def __init__(self):
                self._all_modules_with_complexity = {}
                self._merged = {}

        def sorted_result(self):
                # Sort on descending order:
                ordered = sorted(self._merged.items(), key=lambda item: item[1][0], reverse=True)
                return ordered

        def extend_with(self, name, freqs):
                if name in self._all_modules_with_complexity:
                        complexity = self._all_modules_with_complexity[name]
                        self._merged[name] = freqs, complexity

        def record_detected(self, name, complexity):
                self._all_modules_with_complexity[name] = complexity

def write_csv(stats):
        print 'module,revisions,code'
        for s in stats:
                name, (f,c) = s
                print name + ',' + f + ',' + c

def parse_complexity(merged, row):
        name = row[1][2:]
        complexity = row[4]
        merged.record_detected(name, complexity)

def parse_freqs(merged, row):
        name = row[0]
        freqs = row[1]
        merged.extend_with(name, freqs)

def merge(revs_file, comp_file):
        merged = Merged()
        parse_csv(merged, comp_file, parse_complexity, expected_format='language,filename,blank,comment,code')
        parse_csv(merged, revs_file, parse_freqs, expected_format='entity,n-revs')
        write_csv(merged.sorted_result())

######################################################################
## Parse input
######################################################################

def validate_content_by(heading, expected):
        if not expected:
                return # no validation
        comparison = expected.split(',')
        stripped = heading[0:len(comparison)] # allow extra fields
        if stripped != comparison:
                raise MergeError('Erroneous content. Expected = ' + expected + ', got = ' + ','.join(heading))

def parse_csv(filename, parse_action, expected_format=None):
        def read_heading_from(r):
                p = r.next()
                while p == []:
                        p = r.next()
                return p
        with open(filename, 'rb') as csvfile:
                r = csv.reader(csvfile, delimiter=',')
                heading = read_heading_from(r)
                validate_content_by(heading, expected_format)
                return [parse_action(row) for row in r]

class StructuralElement(object):
        def __init__(self, name, complexity):
                self.name = name
                self.complexity = complexity
        def parts(self):
                return self.name.split('/')

def parse_structural_element(csv_row):
        name = csv_row[1][2:]
        complexity = csv_row[4]
        return StructuralElement(name, complexity)

def make_element_weight_parser(weight_column):
        \"\"\" Parameterize with the column - this allows us
                to generate data from different analysis result types.
        \"\"\"
        def parse_element_weight(csv_row):
                name = csv_row[0]
                weight = float(csv_row[weight_column]) # Assert not zero?
                return name, weight
        return parse_element_weight

######################################################################
## Calculating weights from the given CSV analysis file
######################################################################

def module_weight_calculator_from(analysis_results):
        max_raw_weight = max(analysis_results, key=lambda e: e[1])
        max_value = max_raw_weight[1]
        normalized_weights = dict([(name, (1.0 / max_value) * n) for name,n in analysis_results])
        def normalized_weight_for(module_name):
                if module_name in normalized_weights:
                        return normalized_weights[module_name]
                return 0.0
        return normalized_weight_for

######################################################################
## Building the structure of the system
######################################################################

def _matching_part_in(hierarchy, part):
        return next((x for x in hierarchy if x['name']==part), None)

def _ensure_branch_exists(hierarchy, branch):
        existing = _matching_part_in(hierarchy, branch)
        if not existing:
                new_branch = {'name':branch, 'children':[]}
                hierarchy.append(new_branch)
                existing = new_branch
        return existing

def _add_leaf(hierarchy, module, weight_calculator, name):
        # TODO: augment with weight here!
        new_leaf = {'name':name, 'children':[],
                    'size':module.complexity,
                    'weight':weight_calculator(module.name)}
        hierarchy.append(new_leaf)
        return hierarchy

def _insert_parts_into(hierarchy, module, weight_calculator, parts):
        \"\"\" Recursively traverse the hierarchy and insert the individual parts
                of the module, one by one.
                The parts specify branches. If any branch is missing, it's
                created during the traversal.
                The final part specifies a module name (sans its path, of course).
                This is where we add size and weight to the leaf.
        \"\"\"
        if len(parts) == 1:
                return _add_leaf(hierarchy, module, weight_calculator, name=parts[0])
        next_branch = parts[0]
        existing_branch = _ensure_branch_exists(hierarchy, next_branch)
        return _insert_parts_into(existing_branch['children'],
                                                          module,
                                                          weight_calculator,
                                                          parts=parts[1:])

def generate_structure_from(modules, weight_calculator):
        hierarchy = []
        for module in modules:
                parts = module.parts()
                _insert_parts_into(hierarchy, module, weight_calculator, parts)

        structure = {'name':'root', 'children':hierarchy}
        return structure

######################################################################
## Output
######################################################################

def write_json(result):
        print json.dumps(result)

######################################################################
## Main
######################################################################

# TODO: turn it around: parse the weights first and add them to individual elements
# as the raw structure list is built!

def run(args):
        raw_weights = parse_csv(args.weights, parse_action=make_element_weight_parser(args.weightcolumn))
        weight_calculator = module_weight_calculator_from(raw_weights)

        structure_input = parse_csv(args.structure,
                                                                expected_format='language,filename,blank,comment,code',
                                                                parse_action=parse_structural_element)
        weighted_system_structure = generate_structure_from(structure_input, weight_calculator)
        write_json(weighted_system_structure)

if __name__ == \"__main__\":
        parser = argparse.ArgumentParser(description='Generates a JSON document suitable for enclosure diagrams.')
        parser.add_argument('--structure', required=True, help='A CSV file generated by cloc')
        parser.add_argument('--weights', required=True, help='A CSV file with hotspot results from Code Maat')
        parser.add_argument('--weightcolumn', type=int, default=1, help=\"The index specifying the columnt to use in the weight table\")
        # TODO: add arguments to specify which CSV columns to use!

        args = parser.parse_args()
        run(args)

"
     ))
  repository)


(defun c/generate-d3-lib (repository)
  "Make available the D3 library for REPOSITORY. This is just to not depend on a network connection."
  (mkdir (format "/tmp/%s/d3/" (f-filename repository)) t)
  (with-temp-file (format "/tmp/%s/d3/d3.min.js" (f-filename repository))
    (insert (base64-decode-string
             "IWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbihuLHQpe3JldHVybiB0Pm4/LTE6bj50PzE6bj49dD8wOjAv
MH1mdW5jdGlvbiB0KG4pe3JldHVybiBudWxsIT1uJiYhaXNOYU4obil9ZnVuY3Rpb24gZShuKXty
ZXR1cm57bGVmdDpmdW5jdGlvbih0LGUscix1KXtmb3IoYXJndW1lbnRzLmxlbmd0aDwzJiYocj0w
KSxhcmd1bWVudHMubGVuZ3RoPDQmJih1PXQubGVuZ3RoKTt1PnI7KXt2YXIgaT1yK3U+Pj4xO24o
dFtpXSxlKTwwP3I9aSsxOnU9aX1yZXR1cm4gcn0scmlnaHQ6ZnVuY3Rpb24odCxlLHIsdSl7Zm9y
KGFyZ3VtZW50cy5sZW5ndGg8MyYmKHI9MCksYXJndW1lbnRzLmxlbmd0aDw0JiYodT10Lmxlbmd0
aCk7dT5yOyl7dmFyIGk9cit1Pj4+MTtuKHRbaV0sZSk+MD91PWk6cj1pKzF9cmV0dXJuIHJ9fX1m
dW5jdGlvbiByKG4pe3JldHVybiBuLmxlbmd0aH1mdW5jdGlvbiB1KG4pe2Zvcih2YXIgdD0xO24q
dCUxOyl0Kj0xMDtyZXR1cm4gdH1mdW5jdGlvbiBpKG4sdCl7dHJ5e2Zvcih2YXIgZSBpbiB0KU9i
amVjdC5kZWZpbmVQcm9wZXJ0eShuLnByb3RvdHlwZSxlLHt2YWx1ZTp0W2VdLGVudW1lcmFibGU6
ITF9KX1jYXRjaChyKXtuLnByb3RvdHlwZT10fX1mdW5jdGlvbiBvKCl7fWZ1bmN0aW9uIGEobil7
cmV0dXJuIGhhK24gaW4gdGhpc31mdW5jdGlvbiBjKG4pe3JldHVybiBuPWhhK24sbiBpbiB0aGlz
JiZkZWxldGUgdGhpc1tuXX1mdW5jdGlvbiBzKCl7dmFyIG49W107cmV0dXJuIHRoaXMuZm9yRWFj
aChmdW5jdGlvbih0KXtuLnB1c2godCl9KSxufWZ1bmN0aW9uIGwoKXt2YXIgbj0wO2Zvcih2YXIg
dCBpbiB0aGlzKXQuY2hhckNvZGVBdCgwKT09PWdhJiYrK247cmV0dXJuIG59ZnVuY3Rpb24gZigp
e2Zvcih2YXIgbiBpbiB0aGlzKWlmKG4uY2hhckNvZGVBdCgwKT09PWdhKXJldHVybiExO3JldHVy
biEwfWZ1bmN0aW9uIGgoKXt9ZnVuY3Rpb24gZyhuLHQsZSl7cmV0dXJuIGZ1bmN0aW9uKCl7dmFy
IHI9ZS5hcHBseSh0LGFyZ3VtZW50cyk7cmV0dXJuIHI9PT10P246cn19ZnVuY3Rpb24gcChuLHQp
e2lmKHQgaW4gbilyZXR1cm4gdDt0PXQuY2hhckF0KDApLnRvVXBwZXJDYXNlKCkrdC5zdWJzdHJp
bmcoMSk7Zm9yKHZhciBlPTAscj1wYS5sZW5ndGg7cj5lOysrZSl7dmFyIHU9cGFbZV0rdDtpZih1
IGluIG4pcmV0dXJuIHV9fWZ1bmN0aW9uIHYoKXt9ZnVuY3Rpb24gZCgpe31mdW5jdGlvbiBtKG4p
e2Z1bmN0aW9uIHQoKXtmb3IodmFyIHQscj1lLHU9LTEsaT1yLmxlbmd0aDsrK3U8aTspKHQ9clt1
XS5vbikmJnQuYXBwbHkodGhpcyxhcmd1bWVudHMpO3JldHVybiBufXZhciBlPVtdLHI9bmV3IG87
cmV0dXJuIHQub249ZnVuY3Rpb24odCx1KXt2YXIgaSxvPXIuZ2V0KHQpO3JldHVybiBhcmd1bWVu
dHMubGVuZ3RoPDI/byYmby5vbjoobyYmKG8ub249bnVsbCxlPWUuc2xpY2UoMCxpPWUuaW5kZXhP
ZihvKSkuY29uY2F0KGUuc2xpY2UoaSsxKSksci5yZW1vdmUodCkpLHUmJmUucHVzaChyLnNldCh0
LHtvbjp1fSkpLG4pfSx0fWZ1bmN0aW9uIHkoKXtHby5ldmVudC5wcmV2ZW50RGVmYXVsdCgpfWZ1
bmN0aW9uIHgoKXtmb3IodmFyIG4sdD1Hby5ldmVudDtuPXQuc291cmNlRXZlbnQ7KXQ9bjtyZXR1
cm4gdH1mdW5jdGlvbiBNKG4pe2Zvcih2YXIgdD1uZXcgZCxlPTAscj1hcmd1bWVudHMubGVuZ3Ro
OysrZTxyOyl0W2FyZ3VtZW50c1tlXV09bSh0KTtyZXR1cm4gdC5vZj1mdW5jdGlvbihlLHIpe3Jl
dHVybiBmdW5jdGlvbih1KXt0cnl7dmFyIGk9dS5zb3VyY2VFdmVudD1Hby5ldmVudDt1LnRhcmdl
dD1uLEdvLmV2ZW50PXUsdFt1LnR5cGVdLmFwcGx5KGUscil9ZmluYWxseXtHby5ldmVudD1pfX19
LHR9ZnVuY3Rpb24gXyhuKXtyZXR1cm4gZGEobixfYSksbn1mdW5jdGlvbiBiKG4pe3JldHVybiJm
dW5jdGlvbiI9PXR5cGVvZiBuP246ZnVuY3Rpb24oKXtyZXR1cm4gbWEobix0aGlzKX19ZnVuY3Rp
b24gdyhuKXtyZXR1cm4iZnVuY3Rpb24iPT10eXBlb2Ygbj9uOmZ1bmN0aW9uKCl7cmV0dXJuIHlh
KG4sdGhpcyl9fWZ1bmN0aW9uIFMobix0KXtmdW5jdGlvbiBlKCl7dGhpcy5yZW1vdmVBdHRyaWJ1
dGUobil9ZnVuY3Rpb24gcigpe3RoaXMucmVtb3ZlQXR0cmlidXRlTlMobi5zcGFjZSxuLmxvY2Fs
KX1mdW5jdGlvbiB1KCl7dGhpcy5zZXRBdHRyaWJ1dGUobix0KX1mdW5jdGlvbiBpKCl7dGhpcy5z
ZXRBdHRyaWJ1dGVOUyhuLnNwYWNlLG4ubG9jYWwsdCl9ZnVuY3Rpb24gbygpe3ZhciBlPXQuYXBw
bHkodGhpcyxhcmd1bWVudHMpO251bGw9PWU/dGhpcy5yZW1vdmVBdHRyaWJ1dGUobik6dGhpcy5z
ZXRBdHRyaWJ1dGUobixlKX1mdW5jdGlvbiBhKCl7dmFyIGU9dC5hcHBseSh0aGlzLGFyZ3VtZW50
cyk7bnVsbD09ZT90aGlzLnJlbW92ZUF0dHJpYnV0ZU5TKG4uc3BhY2Usbi5sb2NhbCk6dGhpcy5z
ZXRBdHRyaWJ1dGVOUyhuLnNwYWNlLG4ubG9jYWwsZSl9cmV0dXJuIG49R28ubnMucXVhbGlmeShu
KSxudWxsPT10P24ubG9jYWw/cjplOiJmdW5jdGlvbiI9PXR5cGVvZiB0P24ubG9jYWw/YTpvOm4u
bG9jYWw/aTp1fWZ1bmN0aW9uIGsobil7cmV0dXJuIG4udHJpbSgpLnJlcGxhY2UoL1xzKy9nLCIg
Iil9ZnVuY3Rpb24gRShuKXtyZXR1cm4gbmV3IFJlZ0V4cCgiKD86XnxcXHMrKSIrR28ucmVxdW90
ZShuKSsiKD86XFxzK3wkKSIsImciKX1mdW5jdGlvbiBBKG4pe3JldHVybiBuLnRyaW0oKS5zcGxp
dCgvXnxccysvKX1mdW5jdGlvbiBDKG4sdCl7ZnVuY3Rpb24gZSgpe2Zvcih2YXIgZT0tMTsrK2U8
dTspbltlXSh0aGlzLHQpfWZ1bmN0aW9uIHIoKXtmb3IodmFyIGU9LTEscj10LmFwcGx5KHRoaXMs
YXJndW1lbnRzKTsrK2U8dTspbltlXSh0aGlzLHIpfW49QShuKS5tYXAoTik7dmFyIHU9bi5sZW5n
dGg7cmV0dXJuImZ1bmN0aW9uIj09dHlwZW9mIHQ/cjplfWZ1bmN0aW9uIE4obil7dmFyIHQ9RShu
KTtyZXR1cm4gZnVuY3Rpb24oZSxyKXtpZih1PWUuY2xhc3NMaXN0KXJldHVybiByP3UuYWRkKG4p
OnUucmVtb3ZlKG4pO3ZhciB1PWUuZ2V0QXR0cmlidXRlKCJjbGFzcyIpfHwiIjtyPyh0Lmxhc3RJ
bmRleD0wLHQudGVzdCh1KXx8ZS5zZXRBdHRyaWJ1dGUoImNsYXNzIixrKHUrIiAiK24pKSk6ZS5z
ZXRBdHRyaWJ1dGUoImNsYXNzIixrKHUucmVwbGFjZSh0LCIgIikpKX19ZnVuY3Rpb24gTChuLHQs
ZSl7ZnVuY3Rpb24gcigpe3RoaXMuc3R5bGUucmVtb3ZlUHJvcGVydHkobil9ZnVuY3Rpb24gdSgp
e3RoaXMuc3R5bGUuc2V0UHJvcGVydHkobix0LGUpfWZ1bmN0aW9uIGkoKXt2YXIgcj10LmFwcGx5
KHRoaXMsYXJndW1lbnRzKTtudWxsPT1yP3RoaXMuc3R5bGUucmVtb3ZlUHJvcGVydHkobik6dGhp
cy5zdHlsZS5zZXRQcm9wZXJ0eShuLHIsZSl9cmV0dXJuIG51bGw9PXQ/cjoiZnVuY3Rpb24iPT10
eXBlb2YgdD9pOnV9ZnVuY3Rpb24gVChuLHQpe2Z1bmN0aW9uIGUoKXtkZWxldGUgdGhpc1tuXX1m
dW5jdGlvbiByKCl7dGhpc1tuXT10fWZ1bmN0aW9uIHUoKXt2YXIgZT10LmFwcGx5KHRoaXMsYXJn
dW1lbnRzKTtudWxsPT1lP2RlbGV0ZSB0aGlzW25dOnRoaXNbbl09ZX1yZXR1cm4gbnVsbD09dD9l
OiJmdW5jdGlvbiI9PXR5cGVvZiB0P3U6cn1mdW5jdGlvbiBxKG4pe3JldHVybiJmdW5jdGlvbiI9
PXR5cGVvZiBuP246KG49R28ubnMucXVhbGlmeShuKSkubG9jYWw/ZnVuY3Rpb24oKXtyZXR1cm4g
dGhpcy5vd25lckRvY3VtZW50LmNyZWF0ZUVsZW1lbnROUyhuLnNwYWNlLG4ubG9jYWwpfTpmdW5j
dGlvbigpe3JldHVybiB0aGlzLm93bmVyRG9jdW1lbnQuY3JlYXRlRWxlbWVudE5TKHRoaXMubmFt
ZXNwYWNlVVJJLG4pfX1mdW5jdGlvbiB6KG4pe3JldHVybntfX2RhdGFfXzpufX1mdW5jdGlvbiBS
KG4pe3JldHVybiBmdW5jdGlvbigpe3JldHVybiBNYSh0aGlzLG4pfX1mdW5jdGlvbiBEKHQpe3Jl
dHVybiBhcmd1bWVudHMubGVuZ3RofHwodD1uKSxmdW5jdGlvbihuLGUpe3JldHVybiBuJiZlP3Qo
bi5fX2RhdGFfXyxlLl9fZGF0YV9fKTohbi0hZX19ZnVuY3Rpb24gUChuLHQpe2Zvcih2YXIgZT0w
LHI9bi5sZW5ndGg7cj5lO2UrKylmb3IodmFyIHUsaT1uW2VdLG89MCxhPWkubGVuZ3RoO2E+bztv
KyspKHU9aVtvXSkmJnQodSxvLGUpO3JldHVybiBufWZ1bmN0aW9uIFUobil7cmV0dXJuIGRhKG4s
d2EpLG59ZnVuY3Rpb24gaihuKXt2YXIgdCxlO3JldHVybiBmdW5jdGlvbihyLHUsaSl7dmFyIG8s
YT1uW2ldLnVwZGF0ZSxjPWEubGVuZ3RoO2ZvcihpIT1lJiYoZT1pLHQ9MCksdT49dCYmKHQ9dSsx
KTshKG89YVt0XSkmJisrdDxjOyk7cmV0dXJuIG99fWZ1bmN0aW9uIEgoKXt2YXIgbj10aGlzLl9f
dHJhbnNpdGlvbl9fO24mJisrbi5hY3RpdmV9ZnVuY3Rpb24gRihuLHQsZSl7ZnVuY3Rpb24gcigp
e3ZhciB0PXRoaXNbb107dCYmKHRoaXMucmVtb3ZlRXZlbnRMaXN0ZW5lcihuLHQsdC4kKSxkZWxl
dGUgdGhpc1tvXSl9ZnVuY3Rpb24gdSgpe3ZhciB1PWModCxRbyhhcmd1bWVudHMpKTtyLmNhbGwo
dGhpcyksdGhpcy5hZGRFdmVudExpc3RlbmVyKG4sdGhpc1tvXT11LHUuJD1lKSx1Ll89dH1mdW5j
dGlvbiBpKCl7dmFyIHQsZT1uZXcgUmVnRXhwKCJeX19vbihbXi5dKykiK0dvLnJlcXVvdGUobikr
IiQiKTtmb3IodmFyIHIgaW4gdGhpcylpZih0PXIubWF0Y2goZSkpe3ZhciB1PXRoaXNbcl07dGhp
cy5yZW1vdmVFdmVudExpc3RlbmVyKHRbMV0sdSx1LiQpLGRlbGV0ZSB0aGlzW3JdfX12YXIgbz0i
X19vbiIrbixhPW4uaW5kZXhPZigiLiIpLGM9TzthPjAmJihuPW4uc3Vic3RyaW5nKDAsYSkpO3Zh
ciBzPWthLmdldChuKTtyZXR1cm4gcyYmKG49cyxjPUkpLGE/dD91OnI6dD92Oml9ZnVuY3Rpb24g
TyhuLHQpe3JldHVybiBmdW5jdGlvbihlKXt2YXIgcj1Hby5ldmVudDtHby5ldmVudD1lLHRbMF09
dGhpcy5fX2RhdGFfXzt0cnl7bi5hcHBseSh0aGlzLHQpfWZpbmFsbHl7R28uZXZlbnQ9cn19fWZ1
bmN0aW9uIEkobix0KXt2YXIgZT1PKG4sdCk7cmV0dXJuIGZ1bmN0aW9uKG4pe3ZhciB0PXRoaXMs
cj1uLnJlbGF0ZWRUYXJnZXQ7ciYmKHI9PT10fHw4JnIuY29tcGFyZURvY3VtZW50UG9zaXRpb24o
dCkpfHxlLmNhbGwodCxuKX19ZnVuY3Rpb24gWSgpe3ZhciBuPSIuZHJhZ3N1cHByZXNzLSIrICsr
QWEsdD0iY2xpY2siK24sZT1Hby5zZWxlY3QoZWEpLm9uKCJ0b3VjaG1vdmUiK24seSkub24oImRy
YWdzdGFydCIrbix5KS5vbigic2VsZWN0c3RhcnQiK24seSk7aWYoRWEpe3ZhciByPXRhLnN0eWxl
LHU9cltFYV07cltFYV09Im5vbmUifXJldHVybiBmdW5jdGlvbihpKXtmdW5jdGlvbiBvKCl7ZS5v
bih0LG51bGwpfWUub24obixudWxsKSxFYSYmKHJbRWFdPXUpLGkmJihlLm9uKHQsZnVuY3Rpb24o
KXt5KCksbygpfSwhMCksc2V0VGltZW91dChvLDApKX19ZnVuY3Rpb24gWihuLHQpe3QuY2hhbmdl
ZFRvdWNoZXMmJih0PXQuY2hhbmdlZFRvdWNoZXNbMF0pO3ZhciBlPW4ub3duZXJTVkdFbGVtZW50
fHxuO2lmKGUuY3JlYXRlU1ZHUG9pbnQpe3ZhciByPWUuY3JlYXRlU1ZHUG9pbnQoKTtyZXR1cm4g
ci54PXQuY2xpZW50WCxyLnk9dC5jbGllbnRZLHI9ci5tYXRyaXhUcmFuc2Zvcm0obi5nZXRTY3Jl
ZW5DVE0oKS5pbnZlcnNlKCkpLFtyLngsci55XX12YXIgdT1uLmdldEJvdW5kaW5nQ2xpZW50UmVj
dCgpO3JldHVyblt0LmNsaWVudFgtdS5sZWZ0LW4uY2xpZW50TGVmdCx0LmNsaWVudFktdS50b3At
bi5jbGllbnRUb3BdfWZ1bmN0aW9uIFYoKXtyZXR1cm4gR28uZXZlbnQuY2hhbmdlZFRvdWNoZXNb
MF0uaWRlbnRpZmllcn1mdW5jdGlvbiAkKCl7cmV0dXJuIEdvLmV2ZW50LnRhcmdldH1mdW5jdGlv
biBYKCl7cmV0dXJuIGVhfWZ1bmN0aW9uIEIobil7cmV0dXJuIG4+MD8xOjA+bj8tMTowfWZ1bmN0
aW9uIEoobix0LGUpe3JldHVybih0WzBdLW5bMF0pKihlWzFdLW5bMV0pLSh0WzFdLW5bMV0pKihl
WzBdLW5bMF0pfWZ1bmN0aW9uIFcobil7cmV0dXJuIG4+MT8wOi0xPm4/Q2E6TWF0aC5hY29zKG4p
fWZ1bmN0aW9uIEcobil7cmV0dXJuIG4+MT9MYTotMT5uPy1MYTpNYXRoLmFzaW4obil9ZnVuY3Rp
b24gSyhuKXtyZXR1cm4oKG49TWF0aC5leHAobikpLTEvbikvMn1mdW5jdGlvbiBRKG4pe3JldHVy
bigobj1NYXRoLmV4cChuKSkrMS9uKS8yfWZ1bmN0aW9uIG50KG4pe3JldHVybigobj1NYXRoLmV4
cCgyKm4pKS0xKS8obisxKX1mdW5jdGlvbiB0dChuKXtyZXR1cm4obj1NYXRoLnNpbihuLzIpKSpu
fWZ1bmN0aW9uIGV0KCl7fWZ1bmN0aW9uIHJ0KG4sdCxlKXtyZXR1cm4gbmV3IHV0KG4sdCxlKX1m
dW5jdGlvbiB1dChuLHQsZSl7dGhpcy5oPW4sdGhpcy5zPXQsdGhpcy5sPWV9ZnVuY3Rpb24gaXQo
bix0LGUpe2Z1bmN0aW9uIHIobil7cmV0dXJuIG4+MzYwP24tPTM2MDowPm4mJihuKz0zNjApLDYw
Pm4/aSsoby1pKSpuLzYwOjE4MD5uP286MjQwPm4/aSsoby1pKSooMjQwLW4pLzYwOml9ZnVuY3Rp
b24gdShuKXtyZXR1cm4gTWF0aC5yb3VuZCgyNTUqcihuKSl9dmFyIGksbztyZXR1cm4gbj1pc05h
TihuKT8wOihuJT0zNjApPDA/biszNjA6bix0PWlzTmFOKHQpPzA6MD50PzA6dD4xPzE6dCxlPTA+
ZT8wOmU+MT8xOmUsbz0uNT49ZT9lKigxK3QpOmUrdC1lKnQsaT0yKmUtbyx5dCh1KG4rMTIwKSx1
KG4pLHUobi0xMjApKX1mdW5jdGlvbiBvdChuLHQsZSl7cmV0dXJuIG5ldyBhdChuLHQsZSl9ZnVu
Y3Rpb24gYXQobix0LGUpe3RoaXMuaD1uLHRoaXMuYz10LHRoaXMubD1lfWZ1bmN0aW9uIGN0KG4s
dCxlKXtyZXR1cm4gaXNOYU4obikmJihuPTApLGlzTmFOKHQpJiYodD0wKSxzdChlLE1hdGguY29z
KG4qPXphKSp0LE1hdGguc2luKG4pKnQpfWZ1bmN0aW9uIHN0KG4sdCxlKXtyZXR1cm4gbmV3IGx0
KG4sdCxlKX1mdW5jdGlvbiBsdChuLHQsZSl7dGhpcy5sPW4sdGhpcy5hPXQsdGhpcy5iPWV9ZnVu
Y3Rpb24gZnQobix0LGUpe3ZhciByPShuKzE2KS8xMTYsdT1yK3QvNTAwLGk9ci1lLzIwMDtyZXR1
cm4gdT1ndCh1KSpaYSxyPWd0KHIpKlZhLGk9Z3QoaSkqJGEseXQodnQoMy4yNDA0NTQyKnUtMS41
MzcxMzg1KnItLjQ5ODUzMTQqaSksdnQoLS45NjkyNjYqdSsxLjg3NjAxMDgqcisuMDQxNTU2Kmkp
LHZ0KC4wNTU2NDM0KnUtLjIwNDAyNTkqcisxLjA1NzIyNTIqaSkpfWZ1bmN0aW9uIGh0KG4sdCxl
KXtyZXR1cm4gbj4wP290KE1hdGguYXRhbjIoZSx0KSpSYSxNYXRoLnNxcnQodCp0K2UqZSksbik6
b3QoMC8wLDAvMCxuKX1mdW5jdGlvbiBndChuKXtyZXR1cm4gbj4uMjA2ODkzMDM0P24qbipuOihu
LTQvMjkpLzcuNzg3MDM3fWZ1bmN0aW9uIHB0KG4pe3JldHVybiBuPi4wMDg4NTY/TWF0aC5wb3co
biwxLzMpOjcuNzg3MDM3Km4rNC8yOX1mdW5jdGlvbiB2dChuKXtyZXR1cm4gTWF0aC5yb3VuZCgy
NTUqKC4wMDMwND49bj8xMi45MipuOjEuMDU1Kk1hdGgucG93KG4sMS8yLjQpLS4wNTUpKX1mdW5j
dGlvbiBkdChuKXtyZXR1cm4geXQobj4+MTYsMjU1Jm4+PjgsMjU1Jm4pfWZ1bmN0aW9uIG10KG4p
e3JldHVybiBkdChuKSsiIn1mdW5jdGlvbiB5dChuLHQsZSl7cmV0dXJuIG5ldyB4dChuLHQsZSl9
ZnVuY3Rpb24geHQobix0LGUpe3RoaXMucj1uLHRoaXMuZz10LHRoaXMuYj1lfWZ1bmN0aW9uIE10
KG4pe3JldHVybiAxNj5uPyIwIitNYXRoLm1heCgwLG4pLnRvU3RyaW5nKDE2KTpNYXRoLm1pbigy
NTUsbikudG9TdHJpbmcoMTYpfWZ1bmN0aW9uIF90KG4sdCxlKXt2YXIgcix1LGksbz0wLGE9MCxj
PTA7aWYocj0vKFthLXpdKylcKCguKilcKS9pLmV4ZWMobikpc3dpdGNoKHU9clsyXS5zcGxpdCgi
LCIpLHJbMV0pe2Nhc2UiaHNsIjpyZXR1cm4gZShwYXJzZUZsb2F0KHVbMF0pLHBhcnNlRmxvYXQo
dVsxXSkvMTAwLHBhcnNlRmxvYXQodVsyXSkvMTAwKTtjYXNlInJnYiI6cmV0dXJuIHQoa3QodVsw
XSksa3QodVsxXSksa3QodVsyXSkpfXJldHVybihpPUphLmdldChuKSk/dChpLnIsaS5nLGkuYik6
KG51bGw9PW58fCIjIiE9PW4uY2hhckF0KDApfHxpc05hTihpPXBhcnNlSW50KG4uc3Vic3RyaW5n
KDEpLDE2KSl8fCg0PT09bi5sZW5ndGg/KG89KDM4NDAmaSk+PjQsbz1vPj40fG8sYT0yNDAmaSxh
PWE+PjR8YSxjPTE1JmksYz1jPDw0fGMpOjc9PT1uLmxlbmd0aCYmKG89KDE2NzExNjgwJmkpPj4x
NixhPSg2NTI4MCZpKT4+OCxjPTI1NSZpKSksdChvLGEsYykpfWZ1bmN0aW9uIGJ0KG4sdCxlKXt2
YXIgcix1LGk9TWF0aC5taW4obi89MjU1LHQvPTI1NSxlLz0yNTUpLG89TWF0aC5tYXgobix0LGUp
LGE9by1pLGM9KG8raSkvMjtyZXR1cm4gYT8odT0uNT5jP2EvKG8raSk6YS8oMi1vLWkpLHI9bj09
bz8odC1lKS9hKyhlPnQ/NjowKTp0PT1vPyhlLW4pL2ErMjoobi10KS9hKzQscio9NjApOihyPTAv
MCx1PWM+MCYmMT5jPzA6cikscnQocix1LGMpfWZ1bmN0aW9uIHd0KG4sdCxlKXtuPVN0KG4pLHQ9
U3QodCksZT1TdChlKTt2YXIgcj1wdCgoLjQxMjQ1NjQqbisuMzU3NTc2MSp0Ky4xODA0Mzc1KmUp
L1phKSx1PXB0KCguMjEyNjcyOSpuKy43MTUxNTIyKnQrLjA3MjE3NSplKS9WYSksaT1wdCgoLjAx
OTMzMzkqbisuMTE5MTkyKnQrLjk1MDMwNDEqZSkvJGEpO3JldHVybiBzdCgxMTYqdS0xNiw1MDAq
KHItdSksMjAwKih1LWkpKX1mdW5jdGlvbiBTdChuKXtyZXR1cm4obi89MjU1KTw9LjA0MDQ1P24v
MTIuOTI6TWF0aC5wb3coKG4rLjA1NSkvMS4wNTUsMi40KX1mdW5jdGlvbiBrdChuKXt2YXIgdD1w
YXJzZUZsb2F0KG4pO3JldHVybiIlIj09PW4uY2hhckF0KG4ubGVuZ3RoLTEpP01hdGgucm91bmQo
Mi41NSp0KTp0fWZ1bmN0aW9uIEV0KG4pe3JldHVybiJmdW5jdGlvbiI9PXR5cGVvZiBuP246ZnVu
Y3Rpb24oKXtyZXR1cm4gbn19ZnVuY3Rpb24gQXQobil7cmV0dXJuIG59ZnVuY3Rpb24gQ3Qobil7
cmV0dXJuIGZ1bmN0aW9uKHQsZSxyKXtyZXR1cm4gMj09PWFyZ3VtZW50cy5sZW5ndGgmJiJmdW5j
dGlvbiI9PXR5cGVvZiBlJiYocj1lLGU9bnVsbCksTnQodCxlLG4scil9fWZ1bmN0aW9uIE50KG4s
dCxlLHIpe2Z1bmN0aW9uIHUoKXt2YXIgbix0PWMuc3RhdHVzO2lmKCF0JiZjLnJlc3BvbnNlVGV4
dHx8dD49MjAwJiYzMDA+dHx8MzA0PT09dCl7dHJ5e249ZS5jYWxsKGksYyl9Y2F0Y2gocil7cmV0
dXJuIG8uZXJyb3IuY2FsbChpLHIpLHZvaWQgMH1vLmxvYWQuY2FsbChpLG4pfWVsc2Ugby5lcnJv
ci5jYWxsKGksYyl9dmFyIGk9e30sbz1Hby5kaXNwYXRjaCgiYmVmb3Jlc2VuZCIsInByb2dyZXNz
IiwibG9hZCIsImVycm9yIiksYT17fSxjPW5ldyBYTUxIdHRwUmVxdWVzdCxzPW51bGw7cmV0dXJu
IWVhLlhEb21haW5SZXF1ZXN0fHwid2l0aENyZWRlbnRpYWxzImluIGN8fCEvXihodHRwKHMpPzop
P1wvXC8vLnRlc3Qobil8fChjPW5ldyBYRG9tYWluUmVxdWVzdCksIm9ubG9hZCJpbiBjP2Mub25s
b2FkPWMub25lcnJvcj11OmMub25yZWFkeXN0YXRlY2hhbmdlPWZ1bmN0aW9uKCl7Yy5yZWFkeVN0
YXRlPjMmJnUoKX0sYy5vbnByb2dyZXNzPWZ1bmN0aW9uKG4pe3ZhciB0PUdvLmV2ZW50O0dvLmV2
ZW50PW47dHJ5e28ucHJvZ3Jlc3MuY2FsbChpLGMpfWZpbmFsbHl7R28uZXZlbnQ9dH19LGkuaGVh
ZGVyPWZ1bmN0aW9uKG4sdCl7cmV0dXJuIG49KG4rIiIpLnRvTG93ZXJDYXNlKCksYXJndW1lbnRz
Lmxlbmd0aDwyP2Fbbl06KG51bGw9PXQ/ZGVsZXRlIGFbbl06YVtuXT10KyIiLGkpfSxpLm1pbWVU
eXBlPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh0PW51bGw9PW4/bnVsbDpu
KyIiLGkpOnR9LGkucmVzcG9uc2VUeXBlPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVu
Z3RoPyhzPW4saSk6c30saS5yZXNwb25zZT1mdW5jdGlvbihuKXtyZXR1cm4gZT1uLGl9LFsiZ2V0
IiwicG9zdCJdLmZvckVhY2goZnVuY3Rpb24obil7aVtuXT1mdW5jdGlvbigpe3JldHVybiBpLnNl
bmQuYXBwbHkoaSxbbl0uY29uY2F0KFFvKGFyZ3VtZW50cykpKX19KSxpLnNlbmQ9ZnVuY3Rpb24o
ZSxyLHUpe2lmKDI9PT1hcmd1bWVudHMubGVuZ3RoJiYiZnVuY3Rpb24iPT10eXBlb2YgciYmKHU9
cixyPW51bGwpLGMub3BlbihlLG4sITApLG51bGw9PXR8fCJhY2NlcHQiaW4gYXx8KGEuYWNjZXB0
PXQrIiwqLyoiKSxjLnNldFJlcXVlc3RIZWFkZXIpZm9yKHZhciBsIGluIGEpYy5zZXRSZXF1ZXN0
SGVhZGVyKGwsYVtsXSk7cmV0dXJuIG51bGwhPXQmJmMub3ZlcnJpZGVNaW1lVHlwZSYmYy5vdmVy
cmlkZU1pbWVUeXBlKHQpLG51bGwhPXMmJihjLnJlc3BvbnNlVHlwZT1zKSxudWxsIT11JiZpLm9u
KCJlcnJvciIsdSkub24oImxvYWQiLGZ1bmN0aW9uKG4pe3UobnVsbCxuKX0pLG8uYmVmb3Jlc2Vu
ZC5jYWxsKGksYyksYy5zZW5kKG51bGw9PXI/bnVsbDpyKSxpfSxpLmFib3J0PWZ1bmN0aW9uKCl7
cmV0dXJuIGMuYWJvcnQoKSxpfSxHby5yZWJpbmQoaSxvLCJvbiIpLG51bGw9PXI/aTppLmdldChM
dChyKSl9ZnVuY3Rpb24gTHQobil7cmV0dXJuIDE9PT1uLmxlbmd0aD9mdW5jdGlvbih0LGUpe24o
bnVsbD09dD9lOm51bGwpfTpufWZ1bmN0aW9uIFR0KCl7dmFyIG49cXQoKSx0PXp0KCktbjt0PjI0
Pyhpc0Zpbml0ZSh0KSYmKGNsZWFyVGltZW91dChRYSksUWE9c2V0VGltZW91dChUdCx0KSksS2E9
MCk6KEthPTEsdGMoVHQpKX1mdW5jdGlvbiBxdCgpe3ZhciBuPURhdGUubm93KCk7Zm9yKG5jPVdh
O25jOyluPj1uYy50JiYobmMuZj1uYy5jKG4tbmMudCkpLG5jPW5jLm47cmV0dXJuIG59ZnVuY3Rp
b24genQoKXtmb3IodmFyIG4sdD1XYSxlPTEvMDt0Oyl0LmY/dD1uP24ubj10Lm46V2E9dC5uOih0
LnQ8ZSYmKGU9dC50KSx0PShuPXQpLm4pO3JldHVybiBHYT1uLGV9ZnVuY3Rpb24gUnQobix0KXty
ZXR1cm4gdC0obj9NYXRoLmNlaWwoTWF0aC5sb2cobikvTWF0aC5MTjEwKToxKX1mdW5jdGlvbiBE
dChuLHQpe3ZhciBlPU1hdGgucG93KDEwLDMqZmEoOC10KSk7cmV0dXJue3NjYWxlOnQ+OD9mdW5j
dGlvbihuKXtyZXR1cm4gbi9lfTpmdW5jdGlvbihuKXtyZXR1cm4gbiplfSxzeW1ib2w6bn19ZnVu
Y3Rpb24gUHQobil7dmFyIHQ9bi5kZWNpbWFsLGU9bi50aG91c2FuZHMscj1uLmdyb3VwaW5nLHU9
bi5jdXJyZW5jeSxpPXI/ZnVuY3Rpb24obil7Zm9yKHZhciB0PW4ubGVuZ3RoLHU9W10saT0wLG89
clswXTt0PjAmJm8+MDspdS5wdXNoKG4uc3Vic3RyaW5nKHQtPW8sdCtvKSksbz1yW2k9KGkrMSkl
ci5sZW5ndGhdO3JldHVybiB1LnJldmVyc2UoKS5qb2luKGUpfTpBdDtyZXR1cm4gZnVuY3Rpb24o
bil7dmFyIGU9cmMuZXhlYyhuKSxyPWVbMV18fCIgIixvPWVbMl18fCI+IixhPWVbM118fCIiLGM9
ZVs0XXx8IiIscz1lWzVdLGw9K2VbNl0sZj1lWzddLGg9ZVs4XSxnPWVbOV0scD0xLHY9IiIsZD0i
IixtPSExO3N3aXRjaChoJiYoaD0raC5zdWJzdHJpbmcoMSkpLChzfHwiMCI9PT1yJiYiPSI9PT1v
KSYmKHM9cj0iMCIsbz0iPSIsZiYmKGwtPU1hdGguZmxvb3IoKGwtMSkvNCkpKSxnKXtjYXNlIm4i
OmY9ITAsZz0iZyI7YnJlYWs7Y2FzZSIlIjpwPTEwMCxkPSIlIixnPSJmIjticmVhaztjYXNlInAi
OnA9MTAwLGQ9IiUiLGc9InIiO2JyZWFrO2Nhc2UiYiI6Y2FzZSJvIjpjYXNlIngiOmNhc2UiWCI6
IiMiPT09YyYmKHY9IjAiK2cudG9Mb3dlckNhc2UoKSk7Y2FzZSJjIjpjYXNlImQiOm09ITAsaD0w
O2JyZWFrO2Nhc2UicyI6cD0tMSxnPSJyIn0iJCI9PT1jJiYodj11WzBdLGQ9dVsxXSksInIiIT1n
fHxofHwoZz0iZyIpLG51bGwhPWgmJigiZyI9PWc/aD1NYXRoLm1heCgxLE1hdGgubWluKDIxLGgp
KTooImUiPT1nfHwiZiI9PWcpJiYoaD1NYXRoLm1heCgwLE1hdGgubWluKDIwLGgpKSkpLGc9dWMu
Z2V0KGcpfHxVdDt2YXIgeT1zJiZmO3JldHVybiBmdW5jdGlvbihuKXt2YXIgZT1kO2lmKG0mJm4l
MSlyZXR1cm4iIjt2YXIgdT0wPm58fDA9PT1uJiYwPjEvbj8obj0tbiwiLSIpOmE7aWYoMD5wKXt2
YXIgYz1Hby5mb3JtYXRQcmVmaXgobixoKTtuPWMuc2NhbGUobiksZT1jLnN5bWJvbCtkfWVsc2Ug
bio9cDtuPWcobixoKTt2YXIgeD1uLmxhc3RJbmRleE9mKCIuIiksTT0wPng/bjpuLnN1YnN0cmlu
ZygwLHgpLF89MD54PyIiOnQrbi5zdWJzdHJpbmcoeCsxKTshcyYmZiYmKE09aShNKSk7dmFyIGI9
di5sZW5ndGgrTS5sZW5ndGgrXy5sZW5ndGgrKHk/MDp1Lmxlbmd0aCksdz1sPmI/bmV3IEFycmF5
KGI9bC1iKzEpLmpvaW4ocik6IiI7cmV0dXJuIHkmJihNPWkodytNKSksdSs9dixuPU0rXywoIjwi
PT09bz91K24rdzoiPiI9PT1vP3crdStuOiJeIj09PW8/dy5zdWJzdHJpbmcoMCxiPj49MSkrdStu
K3cuc3Vic3RyaW5nKGIpOnUrKHk/bjp3K24pKStlfX19ZnVuY3Rpb24gVXQobil7cmV0dXJuIG4r
IiJ9ZnVuY3Rpb24ganQoKXt0aGlzLl89bmV3IERhdGUoYXJndW1lbnRzLmxlbmd0aD4xP0RhdGUu
VVRDLmFwcGx5KHRoaXMsYXJndW1lbnRzKTphcmd1bWVudHNbMF0pfWZ1bmN0aW9uIEh0KG4sdCxl
KXtmdW5jdGlvbiByKHQpe3ZhciBlPW4odCkscj1pKGUsMSk7cmV0dXJuIHItdD50LWU/ZTpyfWZ1
bmN0aW9uIHUoZSl7cmV0dXJuIHQoZT1uKG5ldyBvYyhlLTEpKSwxKSxlfWZ1bmN0aW9uIGkobixl
KXtyZXR1cm4gdChuPW5ldyBvYygrbiksZSksbn1mdW5jdGlvbiBvKG4scixpKXt2YXIgbz11KG4p
LGE9W107aWYoaT4xKWZvcig7cj5vOyllKG8pJWl8fGEucHVzaChuZXcgRGF0ZSgrbykpLHQobywx
KTtlbHNlIGZvcig7cj5vOylhLnB1c2gobmV3IERhdGUoK28pKSx0KG8sMSk7cmV0dXJuIGF9ZnVu
Y3Rpb24gYShuLHQsZSl7dHJ5e29jPWp0O3ZhciByPW5ldyBqdDtyZXR1cm4gci5fPW4sbyhyLHQs
ZSl9ZmluYWxseXtvYz1EYXRlfX1uLmZsb29yPW4sbi5yb3VuZD1yLG4uY2VpbD11LG4ub2Zmc2V0
PWksbi5yYW5nZT1vO3ZhciBjPW4udXRjPUZ0KG4pO3JldHVybiBjLmZsb29yPWMsYy5yb3VuZD1G
dChyKSxjLmNlaWw9RnQodSksYy5vZmZzZXQ9RnQoaSksYy5yYW5nZT1hLG59ZnVuY3Rpb24gRnQo
bil7cmV0dXJuIGZ1bmN0aW9uKHQsZSl7dHJ5e29jPWp0O3ZhciByPW5ldyBqdDtyZXR1cm4gci5f
PXQsbihyLGUpLl99ZmluYWxseXtvYz1EYXRlfX19ZnVuY3Rpb24gT3Qobil7ZnVuY3Rpb24gdChu
KXtmdW5jdGlvbiB0KHQpe2Zvcih2YXIgZSx1LGksbz1bXSxhPS0xLGM9MDsrK2E8cjspMzc9PT1u
LmNoYXJDb2RlQXQoYSkmJihvLnB1c2gobi5zdWJzdHJpbmcoYyxhKSksbnVsbCE9KHU9Y2NbZT1u
LmNoYXJBdCgrK2EpXSkmJihlPW4uY2hhckF0KCsrYSkpLChpPUNbZV0pJiYoZT1pKHQsbnVsbD09
dT8iZSI9PT1lPyIgIjoiMCI6dSkpLG8ucHVzaChlKSxjPWErMSk7cmV0dXJuIG8ucHVzaChuLnN1
YnN0cmluZyhjLGEpKSxvLmpvaW4oIiIpfXZhciByPW4ubGVuZ3RoO3JldHVybiB0LnBhcnNlPWZ1
bmN0aW9uKHQpe3ZhciByPXt5OjE5MDAsbTowLGQ6MSxIOjAsTTowLFM6MCxMOjAsWjpudWxsfSx1
PWUocixuLHQsMCk7aWYodSE9dC5sZW5ndGgpcmV0dXJuIG51bGw7InAiaW4gciYmKHIuSD1yLkgl
MTIrMTIqci5wKTt2YXIgaT1udWxsIT1yLlomJm9jIT09anQsbz1uZXcoaT9qdDpvYyk7cmV0dXJu
ImoiaW4gcj9vLnNldEZ1bGxZZWFyKHIueSwwLHIuaik6InciaW4gciYmKCJXImluIHJ8fCJVImlu
IHIpPyhvLnNldEZ1bGxZZWFyKHIueSwwLDEpLG8uc2V0RnVsbFllYXIoci55LDAsIlciaW4gcj8o
ci53KzYpJTcrNypyLlctKG8uZ2V0RGF5KCkrNSklNzpyLncrNypyLlUtKG8uZ2V0RGF5KCkrNikl
NykpOm8uc2V0RnVsbFllYXIoci55LHIubSxyLmQpLG8uc2V0SG91cnMoci5IK01hdGguZmxvb3Io
ci5aLzEwMCksci5NK3IuWiUxMDAsci5TLHIuTCksaT9vLl86b30sdC50b1N0cmluZz1mdW5jdGlv
bigpe3JldHVybiBufSx0fWZ1bmN0aW9uIGUobix0LGUscil7Zm9yKHZhciB1LGksbyxhPTAsYz10
Lmxlbmd0aCxzPWUubGVuZ3RoO2M+YTspe2lmKHI+PXMpcmV0dXJuLTE7aWYodT10LmNoYXJDb2Rl
QXQoYSsrKSwzNz09PXUpe2lmKG89dC5jaGFyQXQoYSsrKSxpPU5bbyBpbiBjYz90LmNoYXJBdChh
KyspOm9dLCFpfHwocj1pKG4sZSxyKSk8MClyZXR1cm4tMX1lbHNlIGlmKHUhPWUuY2hhckNvZGVB
dChyKyspKXJldHVybi0xfXJldHVybiByfWZ1bmN0aW9uIHIobix0LGUpe2IubGFzdEluZGV4PTA7
dmFyIHI9Yi5leGVjKHQuc3Vic3RyaW5nKGUpKTtyZXR1cm4gcj8obi53PXcuZ2V0KHJbMF0udG9M
b3dlckNhc2UoKSksZStyWzBdLmxlbmd0aCk6LTF9ZnVuY3Rpb24gdShuLHQsZSl7TS5sYXN0SW5k
ZXg9MDt2YXIgcj1NLmV4ZWModC5zdWJzdHJpbmcoZSkpO3JldHVybiByPyhuLnc9Xy5nZXQoclsw
XS50b0xvd2VyQ2FzZSgpKSxlK3JbMF0ubGVuZ3RoKTotMX1mdW5jdGlvbiBpKG4sdCxlKXtFLmxh
c3RJbmRleD0wO3ZhciByPUUuZXhlYyh0LnN1YnN0cmluZyhlKSk7cmV0dXJuIHI/KG4ubT1BLmdl
dChyWzBdLnRvTG93ZXJDYXNlKCkpLGUrclswXS5sZW5ndGgpOi0xfWZ1bmN0aW9uIG8obix0LGUp
e1MubGFzdEluZGV4PTA7dmFyIHI9Uy5leGVjKHQuc3Vic3RyaW5nKGUpKTtyZXR1cm4gcj8obi5t
PWsuZ2V0KHJbMF0udG9Mb3dlckNhc2UoKSksZStyWzBdLmxlbmd0aCk6LTF9ZnVuY3Rpb24gYShu
LHQscil7cmV0dXJuIGUobixDLmMudG9TdHJpbmcoKSx0LHIpfWZ1bmN0aW9uIGMobix0LHIpe3Jl
dHVybiBlKG4sQy54LnRvU3RyaW5nKCksdCxyKX1mdW5jdGlvbiBzKG4sdCxyKXtyZXR1cm4gZShu
LEMuWC50b1N0cmluZygpLHQscil9ZnVuY3Rpb24gbChuLHQsZSl7dmFyIHI9eC5nZXQodC5zdWJz
dHJpbmcoZSxlKz0yKS50b0xvd2VyQ2FzZSgpKTtyZXR1cm4gbnVsbD09cj8tMToobi5wPXIsZSl9
dmFyIGY9bi5kYXRlVGltZSxoPW4uZGF0ZSxnPW4udGltZSxwPW4ucGVyaW9kcyx2PW4uZGF5cyxk
PW4uc2hvcnREYXlzLG09bi5tb250aHMseT1uLnNob3J0TW9udGhzO3QudXRjPWZ1bmN0aW9uKG4p
e2Z1bmN0aW9uIGUobil7dHJ5e29jPWp0O3ZhciB0PW5ldyBvYztyZXR1cm4gdC5fPW4scih0KX1m
aW5hbGx5e29jPURhdGV9fXZhciByPXQobik7cmV0dXJuIGUucGFyc2U9ZnVuY3Rpb24obil7dHJ5
e29jPWp0O3ZhciB0PXIucGFyc2Uobik7cmV0dXJuIHQmJnQuX31maW5hbGx5e29jPURhdGV9fSxl
LnRvU3RyaW5nPXIudG9TdHJpbmcsZX0sdC5tdWx0aT10LnV0Yy5tdWx0aT1hZTt2YXIgeD1Hby5t
YXAoKSxNPVl0KHYpLF89WnQodiksYj1ZdChkKSx3PVp0KGQpLFM9WXQobSksaz1adChtKSxFPVl0
KHkpLEE9WnQoeSk7cC5mb3JFYWNoKGZ1bmN0aW9uKG4sdCl7eC5zZXQobi50b0xvd2VyQ2FzZSgp
LHQpfSk7dmFyIEM9e2E6ZnVuY3Rpb24obil7cmV0dXJuIGRbbi5nZXREYXkoKV19LEE6ZnVuY3Rp
b24obil7cmV0dXJuIHZbbi5nZXREYXkoKV19LGI6ZnVuY3Rpb24obil7cmV0dXJuIHlbbi5nZXRN
b250aCgpXX0sQjpmdW5jdGlvbihuKXtyZXR1cm4gbVtuLmdldE1vbnRoKCldfSxjOnQoZiksZDpm
dW5jdGlvbihuLHQpe3JldHVybiBJdChuLmdldERhdGUoKSx0LDIpfSxlOmZ1bmN0aW9uKG4sdCl7
cmV0dXJuIEl0KG4uZ2V0RGF0ZSgpLHQsMil9LEg6ZnVuY3Rpb24obix0KXtyZXR1cm4gSXQobi5n
ZXRIb3VycygpLHQsMil9LEk6ZnVuY3Rpb24obix0KXtyZXR1cm4gSXQobi5nZXRIb3VycygpJTEy
fHwxMix0LDIpfSxqOmZ1bmN0aW9uKG4sdCl7cmV0dXJuIEl0KDEraWMuZGF5T2ZZZWFyKG4pLHQs
Myl9LEw6ZnVuY3Rpb24obix0KXtyZXR1cm4gSXQobi5nZXRNaWxsaXNlY29uZHMoKSx0LDMpfSxt
OmZ1bmN0aW9uKG4sdCl7cmV0dXJuIEl0KG4uZ2V0TW9udGgoKSsxLHQsMil9LE06ZnVuY3Rpb24o
bix0KXtyZXR1cm4gSXQobi5nZXRNaW51dGVzKCksdCwyKX0scDpmdW5jdGlvbihuKXtyZXR1cm4g
cFsrKG4uZ2V0SG91cnMoKT49MTIpXX0sUzpmdW5jdGlvbihuLHQpe3JldHVybiBJdChuLmdldFNl
Y29uZHMoKSx0LDIpfSxVOmZ1bmN0aW9uKG4sdCl7cmV0dXJuIEl0KGljLnN1bmRheU9mWWVhcihu
KSx0LDIpfSx3OmZ1bmN0aW9uKG4pe3JldHVybiBuLmdldERheSgpfSxXOmZ1bmN0aW9uKG4sdCl7
cmV0dXJuIEl0KGljLm1vbmRheU9mWWVhcihuKSx0LDIpfSx4OnQoaCksWDp0KGcpLHk6ZnVuY3Rp
b24obix0KXtyZXR1cm4gSXQobi5nZXRGdWxsWWVhcigpJTEwMCx0LDIpfSxZOmZ1bmN0aW9uKG4s
dCl7cmV0dXJuIEl0KG4uZ2V0RnVsbFllYXIoKSUxZTQsdCw0KX0sWjppZSwiJSI6ZnVuY3Rpb24o
KXtyZXR1cm4iJSJ9fSxOPXthOnIsQTp1LGI6aSxCOm8sYzphLGQ6UXQsZTpRdCxIOnRlLEk6dGUs
ajpuZSxMOnVlLG06S3QsTTplZSxwOmwsUzpyZSxVOiR0LHc6VnQsVzpYdCx4OmMsWDpzLHk6SnQs
WTpCdCxaOld0LCIlIjpvZX07cmV0dXJuIHR9ZnVuY3Rpb24gSXQobix0LGUpe3ZhciByPTA+bj8i
LSI6IiIsdT0ocj8tbjpuKSsiIixpPXUubGVuZ3RoO3JldHVybiByKyhlPmk/bmV3IEFycmF5KGUt
aSsxKS5qb2luKHQpK3U6dSl9ZnVuY3Rpb24gWXQobil7cmV0dXJuIG5ldyBSZWdFeHAoIl4oPzoi
K24ubWFwKEdvLnJlcXVvdGUpLmpvaW4oInwiKSsiKSIsImkiKX1mdW5jdGlvbiBadChuKXtmb3Io
dmFyIHQ9bmV3IG8sZT0tMSxyPW4ubGVuZ3RoOysrZTxyOyl0LnNldChuW2VdLnRvTG93ZXJDYXNl
KCksZSk7cmV0dXJuIHR9ZnVuY3Rpb24gVnQobix0LGUpe3NjLmxhc3RJbmRleD0wO3ZhciByPXNj
LmV4ZWModC5zdWJzdHJpbmcoZSxlKzEpKTtyZXR1cm4gcj8obi53PStyWzBdLGUrclswXS5sZW5n
dGgpOi0xfWZ1bmN0aW9uICR0KG4sdCxlKXtzYy5sYXN0SW5kZXg9MDt2YXIgcj1zYy5leGVjKHQu
c3Vic3RyaW5nKGUpKTtyZXR1cm4gcj8obi5VPStyWzBdLGUrclswXS5sZW5ndGgpOi0xfWZ1bmN0
aW9uIFh0KG4sdCxlKXtzYy5sYXN0SW5kZXg9MDt2YXIgcj1zYy5leGVjKHQuc3Vic3RyaW5nKGUp
KTtyZXR1cm4gcj8obi5XPStyWzBdLGUrclswXS5sZW5ndGgpOi0xfWZ1bmN0aW9uIEJ0KG4sdCxl
KXtzYy5sYXN0SW5kZXg9MDt2YXIgcj1zYy5leGVjKHQuc3Vic3RyaW5nKGUsZSs0KSk7cmV0dXJu
IHI/KG4ueT0rclswXSxlK3JbMF0ubGVuZ3RoKTotMX1mdW5jdGlvbiBKdChuLHQsZSl7c2MubGFz
dEluZGV4PTA7dmFyIHI9c2MuZXhlYyh0LnN1YnN0cmluZyhlLGUrMikpO3JldHVybiByPyhuLnk9
R3QoK3JbMF0pLGUrclswXS5sZW5ndGgpOi0xfWZ1bmN0aW9uIFd0KG4sdCxlKXtyZXR1cm4vXlsr
LV1cZHs0fSQvLnRlc3QodD10LnN1YnN0cmluZyhlLGUrNSkpPyhuLlo9LXQsZSs1KTotMX1mdW5j
dGlvbiBHdChuKXtyZXR1cm4gbisobj42OD8xOTAwOjJlMyl9ZnVuY3Rpb24gS3Qobix0LGUpe3Nj
Lmxhc3RJbmRleD0wO3ZhciByPXNjLmV4ZWModC5zdWJzdHJpbmcoZSxlKzIpKTtyZXR1cm4gcj8o
bi5tPXJbMF0tMSxlK3JbMF0ubGVuZ3RoKTotMX1mdW5jdGlvbiBRdChuLHQsZSl7c2MubGFzdElu
ZGV4PTA7dmFyIHI9c2MuZXhlYyh0LnN1YnN0cmluZyhlLGUrMikpO3JldHVybiByPyhuLmQ9K3Jb
MF0sZStyWzBdLmxlbmd0aCk6LTF9ZnVuY3Rpb24gbmUobix0LGUpe3NjLmxhc3RJbmRleD0wO3Zh
ciByPXNjLmV4ZWModC5zdWJzdHJpbmcoZSxlKzMpKTtyZXR1cm4gcj8obi5qPStyWzBdLGUrclsw
XS5sZW5ndGgpOi0xfWZ1bmN0aW9uIHRlKG4sdCxlKXtzYy5sYXN0SW5kZXg9MDt2YXIgcj1zYy5l
eGVjKHQuc3Vic3RyaW5nKGUsZSsyKSk7cmV0dXJuIHI/KG4uSD0rclswXSxlK3JbMF0ubGVuZ3Ro
KTotMX1mdW5jdGlvbiBlZShuLHQsZSl7c2MubGFzdEluZGV4PTA7dmFyIHI9c2MuZXhlYyh0LnN1
YnN0cmluZyhlLGUrMikpO3JldHVybiByPyhuLk09K3JbMF0sZStyWzBdLmxlbmd0aCk6LTF9ZnVu
Y3Rpb24gcmUobix0LGUpe3NjLmxhc3RJbmRleD0wO3ZhciByPXNjLmV4ZWModC5zdWJzdHJpbmco
ZSxlKzIpKTtyZXR1cm4gcj8obi5TPStyWzBdLGUrclswXS5sZW5ndGgpOi0xfWZ1bmN0aW9uIHVl
KG4sdCxlKXtzYy5sYXN0SW5kZXg9MDt2YXIgcj1zYy5leGVjKHQuc3Vic3RyaW5nKGUsZSszKSk7
cmV0dXJuIHI/KG4uTD0rclswXSxlK3JbMF0ubGVuZ3RoKTotMX1mdW5jdGlvbiBpZShuKXt2YXIg
dD1uLmdldFRpbWV6b25lT2Zmc2V0KCksZT10PjA/Ii0iOiIrIixyPX5+KGZhKHQpLzYwKSx1PWZh
KHQpJTYwO3JldHVybiBlK0l0KHIsIjAiLDIpK0l0KHUsIjAiLDIpfWZ1bmN0aW9uIG9lKG4sdCxl
KXtsYy5sYXN0SW5kZXg9MDt2YXIgcj1sYy5leGVjKHQuc3Vic3RyaW5nKGUsZSsxKSk7cmV0dXJu
IHI/ZStyWzBdLmxlbmd0aDotMX1mdW5jdGlvbiBhZShuKXtmb3IodmFyIHQ9bi5sZW5ndGgsZT0t
MTsrK2U8dDspbltlXVswXT10aGlzKG5bZV1bMF0pO3JldHVybiBmdW5jdGlvbih0KXtmb3IodmFy
IGU9MCxyPW5bZV07IXJbMV0odCk7KXI9blsrK2VdO3JldHVybiByWzBdKHQpfX1mdW5jdGlvbiBj
ZSgpe31mdW5jdGlvbiBzZShuLHQsZSl7dmFyIHI9ZS5zPW4rdCx1PXItbixpPXItdTtlLnQ9bi1p
Kyh0LXUpfWZ1bmN0aW9uIGxlKG4sdCl7biYmcGMuaGFzT3duUHJvcGVydHkobi50eXBlKSYmcGNb
bi50eXBlXShuLHQpfWZ1bmN0aW9uIGZlKG4sdCxlKXt2YXIgcix1PS0xLGk9bi5sZW5ndGgtZTtm
b3IodC5saW5lU3RhcnQoKTsrK3U8aTspcj1uW3VdLHQucG9pbnQoclswXSxyWzFdLHJbMl0pO3Qu
bGluZUVuZCgpfWZ1bmN0aW9uIGhlKG4sdCl7dmFyIGU9LTEscj1uLmxlbmd0aDtmb3IodC5wb2x5
Z29uU3RhcnQoKTsrK2U8cjspZmUobltlXSx0LDEpO3QucG9seWdvbkVuZCgpfWZ1bmN0aW9uIGdl
KCl7ZnVuY3Rpb24gbihuLHQpe24qPXphLHQ9dCp6YS8yK0NhLzQ7dmFyIGU9bi1yLG89ZT49MD8x
Oi0xLGE9byplLGM9TWF0aC5jb3ModCkscz1NYXRoLnNpbih0KSxsPWkqcyxmPXUqYytsKk1hdGgu
Y29zKGEpLGg9bCpvKk1hdGguc2luKGEpO2RjLmFkZChNYXRoLmF0YW4yKGgsZikpLHI9bix1PWMs
aT1zfXZhciB0LGUscix1LGk7bWMucG9pbnQ9ZnVuY3Rpb24obyxhKXttYy5wb2ludD1uLHI9KHQ9
bykqemEsdT1NYXRoLmNvcyhhPShlPWEpKnphLzIrQ2EvNCksaT1NYXRoLnNpbihhKX0sbWMubGlu
ZUVuZD1mdW5jdGlvbigpe24odCxlKX19ZnVuY3Rpb24gcGUobil7dmFyIHQ9blswXSxlPW5bMV0s
cj1NYXRoLmNvcyhlKTtyZXR1cm5bcipNYXRoLmNvcyh0KSxyKk1hdGguc2luKHQpLE1hdGguc2lu
KGUpXX1mdW5jdGlvbiB2ZShuLHQpe3JldHVybiBuWzBdKnRbMF0rblsxXSp0WzFdK25bMl0qdFsy
XX1mdW5jdGlvbiBkZShuLHQpe3JldHVybltuWzFdKnRbMl0tblsyXSp0WzFdLG5bMl0qdFswXS1u
WzBdKnRbMl0sblswXSp0WzFdLW5bMV0qdFswXV19ZnVuY3Rpb24gbWUobix0KXtuWzBdKz10WzBd
LG5bMV0rPXRbMV0sblsyXSs9dFsyXX1mdW5jdGlvbiB5ZShuLHQpe3JldHVybltuWzBdKnQsblsx
XSp0LG5bMl0qdF19ZnVuY3Rpb24geGUobil7dmFyIHQ9TWF0aC5zcXJ0KG5bMF0qblswXStuWzFd
Km5bMV0rblsyXSpuWzJdKTtuWzBdLz10LG5bMV0vPXQsblsyXS89dH1mdW5jdGlvbiBNZShuKXty
ZXR1cm5bTWF0aC5hdGFuMihuWzFdLG5bMF0pLEcoblsyXSldfWZ1bmN0aW9uIF9lKG4sdCl7cmV0
dXJuIGZhKG5bMF0tdFswXSk8VGEmJmZhKG5bMV0tdFsxXSk8VGF9ZnVuY3Rpb24gYmUobix0KXtu
Kj16YTt2YXIgZT1NYXRoLmNvcyh0Kj16YSk7d2UoZSpNYXRoLmNvcyhuKSxlKk1hdGguc2luKG4p
LE1hdGguc2luKHQpKX1mdW5jdGlvbiB3ZShuLHQsZSl7Kyt5YyxNYys9KG4tTWMpL3ljLF9jKz0o
dC1fYykveWMsYmMrPShlLWJjKS95Y31mdW5jdGlvbiBTZSgpe2Z1bmN0aW9uIG4obix1KXtuKj16
YTt2YXIgaT1NYXRoLmNvcyh1Kj16YSksbz1pKk1hdGguY29zKG4pLGE9aSpNYXRoLnNpbihuKSxj
PU1hdGguc2luKHUpLHM9TWF0aC5hdGFuMihNYXRoLnNxcnQoKHM9ZSpjLXIqYSkqcysocz1yKm8t
dCpjKSpzKyhzPXQqYS1lKm8pKnMpLHQqbytlKmErcipjKTt4Yys9cyx3Yys9cyoodCsodD1vKSks
U2MrPXMqKGUrKGU9YSkpLGtjKz1zKihyKyhyPWMpKSx3ZSh0LGUscil9dmFyIHQsZSxyO05jLnBv
aW50PWZ1bmN0aW9uKHUsaSl7dSo9emE7dmFyIG89TWF0aC5jb3MoaSo9emEpO3Q9bypNYXRoLmNv
cyh1KSxlPW8qTWF0aC5zaW4odSkscj1NYXRoLnNpbihpKSxOYy5wb2ludD1uLHdlKHQsZSxyKX19
ZnVuY3Rpb24ga2UoKXtOYy5wb2ludD1iZX1mdW5jdGlvbiBFZSgpe2Z1bmN0aW9uIG4obix0KXtu
Kj16YTt2YXIgZT1NYXRoLmNvcyh0Kj16YSksbz1lKk1hdGguY29zKG4pLGE9ZSpNYXRoLnNpbihu
KSxjPU1hdGguc2luKHQpLHM9dSpjLWkqYSxsPWkqby1yKmMsZj1yKmEtdSpvLGg9TWF0aC5zcXJ0
KHMqcytsKmwrZipmKSxnPXIqbyt1KmEraSpjLHA9aCYmLVcoZykvaCx2PU1hdGguYXRhbjIoaCxn
KTtFYys9cCpzLEFjKz1wKmwsQ2MrPXAqZix4Yys9dix3Yys9dioocisocj1vKSksU2MrPXYqKHUr
KHU9YSkpLGtjKz12KihpKyhpPWMpKSx3ZShyLHUsaSl9dmFyIHQsZSxyLHUsaTtOYy5wb2ludD1m
dW5jdGlvbihvLGEpe3Q9byxlPWEsTmMucG9pbnQ9bixvKj16YTt2YXIgYz1NYXRoLmNvcyhhKj16
YSk7cj1jKk1hdGguY29zKG8pLHU9YypNYXRoLnNpbihvKSxpPU1hdGguc2luKGEpLHdlKHIsdSxp
KX0sTmMubGluZUVuZD1mdW5jdGlvbigpe24odCxlKSxOYy5saW5lRW5kPWtlLE5jLnBvaW50PWJl
fX1mdW5jdGlvbiBBZSgpe3JldHVybiEwfWZ1bmN0aW9uIENlKG4sdCxlLHIsdSl7dmFyIGk9W10s
bz1bXTtpZihuLmZvckVhY2goZnVuY3Rpb24obil7aWYoISgodD1uLmxlbmd0aC0xKTw9MCkpe3Zh
ciB0LGU9blswXSxyPW5bdF07aWYoX2UoZSxyKSl7dS5saW5lU3RhcnQoKTtmb3IodmFyIGE9MDt0
PmE7KythKXUucG9pbnQoKGU9blthXSlbMF0sZVsxXSk7cmV0dXJuIHUubGluZUVuZCgpLHZvaWQg
MH12YXIgYz1uZXcgTGUoZSxuLG51bGwsITApLHM9bmV3IExlKGUsbnVsbCxjLCExKTtjLm89cyxp
LnB1c2goYyksby5wdXNoKHMpLGM9bmV3IExlKHIsbixudWxsLCExKSxzPW5ldyBMZShyLG51bGws
YywhMCksYy5vPXMsaS5wdXNoKGMpLG8ucHVzaChzKX19KSxvLnNvcnQodCksTmUoaSksTmUobyks
aS5sZW5ndGgpe2Zvcih2YXIgYT0wLGM9ZSxzPW8ubGVuZ3RoO3M+YTsrK2Epb1thXS5lPWM9IWM7
Zm9yKHZhciBsLGYsaD1pWzBdOzspe2Zvcih2YXIgZz1oLHA9ITA7Zy52OylpZigoZz1nLm4pPT09
aClyZXR1cm47bD1nLnosdS5saW5lU3RhcnQoKTtkb3tpZihnLnY9Zy5vLnY9ITAsZy5lKXtpZihw
KWZvcih2YXIgYT0wLHM9bC5sZW5ndGg7cz5hOysrYSl1LnBvaW50KChmPWxbYV0pWzBdLGZbMV0p
O2Vsc2UgcihnLngsZy5uLngsMSx1KTtnPWcubn1lbHNle2lmKHApe2w9Zy5wLno7Zm9yKHZhciBh
PWwubGVuZ3RoLTE7YT49MDstLWEpdS5wb2ludCgoZj1sW2FdKVswXSxmWzFdKX1lbHNlIHIoZy54
LGcucC54LC0xLHUpO2c9Zy5wfWc9Zy5vLGw9Zy56LHA9IXB9d2hpbGUoIWcudik7dS5saW5lRW5k
KCl9fX1mdW5jdGlvbiBOZShuKXtpZih0PW4ubGVuZ3RoKXtmb3IodmFyIHQsZSxyPTAsdT1uWzBd
Oysrcjx0Oyl1Lm49ZT1uW3JdLGUucD11LHU9ZTt1Lm49ZT1uWzBdLGUucD11fX1mdW5jdGlvbiBM
ZShuLHQsZSxyKXt0aGlzLng9bix0aGlzLno9dCx0aGlzLm89ZSx0aGlzLmU9cix0aGlzLnY9ITEs
dGhpcy5uPXRoaXMucD1udWxsfWZ1bmN0aW9uIFRlKG4sdCxlLHIpe3JldHVybiBmdW5jdGlvbih1
LGkpe2Z1bmN0aW9uIG8odCxlKXt2YXIgcj11KHQsZSk7bih0PXJbMF0sZT1yWzFdKSYmaS5wb2lu
dCh0LGUpfWZ1bmN0aW9uIGEobix0KXt2YXIgZT11KG4sdCk7ZC5wb2ludChlWzBdLGVbMV0pfWZ1
bmN0aW9uIGMoKXt5LnBvaW50PWEsZC5saW5lU3RhcnQoKX1mdW5jdGlvbiBzKCl7eS5wb2ludD1v
LGQubGluZUVuZCgpfWZ1bmN0aW9uIGwobix0KXt2LnB1c2goW24sdF0pO3ZhciBlPXUobix0KTtN
LnBvaW50KGVbMF0sZVsxXSl9ZnVuY3Rpb24gZigpe00ubGluZVN0YXJ0KCksdj1bXX1mdW5jdGlv
biBoKCl7bCh2WzBdWzBdLHZbMF1bMV0pLE0ubGluZUVuZCgpO3ZhciBuLHQ9TS5jbGVhbigpLGU9
eC5idWZmZXIoKSxyPWUubGVuZ3RoO2lmKHYucG9wKCkscC5wdXNoKHYpLHY9bnVsbCxyKWlmKDEm
dCl7bj1lWzBdO3ZhciB1LHI9bi5sZW5ndGgtMSxvPS0xO2lmKHI+MCl7Zm9yKF98fChpLnBvbHln
b25TdGFydCgpLF89ITApLGkubGluZVN0YXJ0KCk7KytvPHI7KWkucG9pbnQoKHU9bltvXSlbMF0s
dVsxXSk7aS5saW5lRW5kKCl9fWVsc2Ugcj4xJiYyJnQmJmUucHVzaChlLnBvcCgpLmNvbmNhdChl
LnNoaWZ0KCkpKSxnLnB1c2goZS5maWx0ZXIocWUpKX12YXIgZyxwLHYsZD10KGkpLG09dS5pbnZl
cnQoclswXSxyWzFdKSx5PXtwb2ludDpvLGxpbmVTdGFydDpjLGxpbmVFbmQ6cyxwb2x5Z29uU3Rh
cnQ6ZnVuY3Rpb24oKXt5LnBvaW50PWwseS5saW5lU3RhcnQ9Zix5LmxpbmVFbmQ9aCxnPVtdLHA9
W119LHBvbHlnb25FbmQ6ZnVuY3Rpb24oKXt5LnBvaW50PW8seS5saW5lU3RhcnQ9Yyx5LmxpbmVF
bmQ9cyxnPUdvLm1lcmdlKGcpO3ZhciBuPURlKG0scCk7Zy5sZW5ndGg/KF98fChpLnBvbHlnb25T
dGFydCgpLF89ITApLENlKGcsUmUsbixlLGkpKTpuJiYoX3x8KGkucG9seWdvblN0YXJ0KCksXz0h
MCksaS5saW5lU3RhcnQoKSxlKG51bGwsbnVsbCwxLGkpLGkubGluZUVuZCgpKSxfJiYoaS5wb2x5
Z29uRW5kKCksXz0hMSksZz1wPW51bGx9LHNwaGVyZTpmdW5jdGlvbigpe2kucG9seWdvblN0YXJ0
KCksaS5saW5lU3RhcnQoKSxlKG51bGwsbnVsbCwxLGkpLGkubGluZUVuZCgpLGkucG9seWdvbkVu
ZCgpfX0seD16ZSgpLE09dCh4KSxfPSExO3JldHVybiB5fX1mdW5jdGlvbiBxZShuKXtyZXR1cm4g
bi5sZW5ndGg+MX1mdW5jdGlvbiB6ZSgpe3ZhciBuLHQ9W107cmV0dXJue2xpbmVTdGFydDpmdW5j
dGlvbigpe3QucHVzaChuPVtdKX0scG9pbnQ6ZnVuY3Rpb24odCxlKXtuLnB1c2goW3QsZV0pfSxs
aW5lRW5kOnYsYnVmZmVyOmZ1bmN0aW9uKCl7dmFyIGU9dDtyZXR1cm4gdD1bXSxuPW51bGwsZX0s
cmVqb2luOmZ1bmN0aW9uKCl7dC5sZW5ndGg+MSYmdC5wdXNoKHQucG9wKCkuY29uY2F0KHQuc2hp
ZnQoKSkpfX19ZnVuY3Rpb24gUmUobix0KXtyZXR1cm4oKG49bi54KVswXTwwP25bMV0tTGEtVGE6
TGEtblsxXSktKCh0PXQueClbMF08MD90WzFdLUxhLVRhOkxhLXRbMV0pfWZ1bmN0aW9uIERlKG4s
dCl7dmFyIGU9blswXSxyPW5bMV0sdT1bTWF0aC5zaW4oZSksLU1hdGguY29zKGUpLDBdLGk9MCxv
PTA7ZGMucmVzZXQoKTtmb3IodmFyIGE9MCxjPXQubGVuZ3RoO2M+YTsrK2Epe3ZhciBzPXRbYV0s
bD1zLmxlbmd0aDtpZihsKWZvcih2YXIgZj1zWzBdLGg9ZlswXSxnPWZbMV0vMitDYS80LHA9TWF0
aC5zaW4oZyksdj1NYXRoLmNvcyhnKSxkPTE7Oyl7ZD09PWwmJihkPTApLG49c1tkXTt2YXIgbT1u
WzBdLHk9blsxXS8yK0NhLzQseD1NYXRoLnNpbih5KSxNPU1hdGguY29zKHkpLF89bS1oLGI9Xz49
MD8xOi0xLHc9YipfLFM9dz5DYSxrPXAqeDtpZihkYy5hZGQoTWF0aC5hdGFuMihrKmIqTWF0aC5z
aW4odyksdipNK2sqTWF0aC5jb3ModykpKSxpKz1TP18rYipOYTpfLFNeaD49ZV5tPj1lKXt2YXIg
RT1kZShwZShmKSxwZShuKSk7eGUoRSk7dmFyIEE9ZGUodSxFKTt4ZShBKTt2YXIgQz0oU15fPj0w
Py0xOjEpKkcoQVsyXSk7KHI+Q3x8cj09PUMmJihFWzBdfHxFWzFdKSkmJihvKz1TXl8+PTA/MTot
MSl9aWYoIWQrKylicmVhaztoPW0scD14LHY9TSxmPW59fXJldHVybigtVGE+aXx8VGE+aSYmMD5k
YyleMSZvfWZ1bmN0aW9uIFBlKG4pe3ZhciB0LGU9MC8wLHI9MC8wLHU9MC8wO3JldHVybntsaW5l
U3RhcnQ6ZnVuY3Rpb24oKXtuLmxpbmVTdGFydCgpLHQ9MX0scG9pbnQ6ZnVuY3Rpb24oaSxvKXt2
YXIgYT1pPjA/Q2E6LUNhLGM9ZmEoaS1lKTtmYShjLUNhKTxUYT8obi5wb2ludChlLHI9KHIrbykv
Mj4wP0xhOi1MYSksbi5wb2ludCh1LHIpLG4ubGluZUVuZCgpLG4ubGluZVN0YXJ0KCksbi5wb2lu
dChhLHIpLG4ucG9pbnQoaSxyKSx0PTApOnUhPT1hJiZjPj1DYSYmKGZhKGUtdSk8VGEmJihlLT11
KlRhKSxmYShpLWEpPFRhJiYoaS09YSpUYSkscj1VZShlLHIsaSxvKSxuLnBvaW50KHUsciksbi5s
aW5lRW5kKCksbi5saW5lU3RhcnQoKSxuLnBvaW50KGEsciksdD0wKSxuLnBvaW50KGU9aSxyPW8p
LHU9YX0sbGluZUVuZDpmdW5jdGlvbigpe24ubGluZUVuZCgpLGU9cj0wLzB9LGNsZWFuOmZ1bmN0
aW9uKCl7cmV0dXJuIDItdH19fWZ1bmN0aW9uIFVlKG4sdCxlLHIpe3ZhciB1LGksbz1NYXRoLnNp
bihuLWUpO3JldHVybiBmYShvKT5UYT9NYXRoLmF0YW4oKE1hdGguc2luKHQpKihpPU1hdGguY29z
KHIpKSpNYXRoLnNpbihlKS1NYXRoLnNpbihyKSoodT1NYXRoLmNvcyh0KSkqTWF0aC5zaW4obikp
Lyh1KmkqbykpOih0K3IpLzJ9ZnVuY3Rpb24gamUobix0LGUscil7dmFyIHU7aWYobnVsbD09bil1
PWUqTGEsci5wb2ludCgtQ2EsdSksci5wb2ludCgwLHUpLHIucG9pbnQoQ2EsdSksci5wb2ludChD
YSwwKSxyLnBvaW50KENhLC11KSxyLnBvaW50KDAsLXUpLHIucG9pbnQoLUNhLC11KSxyLnBvaW50
KC1DYSwwKSxyLnBvaW50KC1DYSx1KTtlbHNlIGlmKGZhKG5bMF0tdFswXSk+VGEpe3ZhciBpPW5b
MF08dFswXT9DYTotQ2E7dT1lKmkvMixyLnBvaW50KC1pLHUpLHIucG9pbnQoMCx1KSxyLnBvaW50
KGksdSl9ZWxzZSByLnBvaW50KHRbMF0sdFsxXSl9ZnVuY3Rpb24gSGUobil7ZnVuY3Rpb24gdChu
LHQpe3JldHVybiBNYXRoLmNvcyhuKSpNYXRoLmNvcyh0KT5pfWZ1bmN0aW9uIGUobil7dmFyIGUs
aSxjLHMsbDtyZXR1cm57bGluZVN0YXJ0OmZ1bmN0aW9uKCl7cz1jPSExLGw9MX0scG9pbnQ6ZnVu
Y3Rpb24oZixoKXt2YXIgZyxwPVtmLGhdLHY9dChmLGgpLGQ9bz92PzA6dShmLGgpOnY/dShmKygw
PmY/Q2E6LUNhKSxoKTowO2lmKCFlJiYocz1jPXYpJiZuLmxpbmVTdGFydCgpLHYhPT1jJiYoZz1y
KGUscCksKF9lKGUsZyl8fF9lKHAsZykpJiYocFswXSs9VGEscFsxXSs9VGEsdj10KHBbMF0scFsx
XSkpKSx2IT09YylsPTAsdj8obi5saW5lU3RhcnQoKSxnPXIocCxlKSxuLnBvaW50KGdbMF0sZ1sx
XSkpOihnPXIoZSxwKSxuLnBvaW50KGdbMF0sZ1sxXSksbi5saW5lRW5kKCkpLGU9ZztlbHNlIGlm
KGEmJmUmJm9edil7dmFyIG07ZCZpfHwhKG09cihwLGUsITApKXx8KGw9MCxvPyhuLmxpbmVTdGFy
dCgpLG4ucG9pbnQobVswXVswXSxtWzBdWzFdKSxuLnBvaW50KG1bMV1bMF0sbVsxXVsxXSksbi5s
aW5lRW5kKCkpOihuLnBvaW50KG1bMV1bMF0sbVsxXVsxXSksbi5saW5lRW5kKCksbi5saW5lU3Rh
cnQoKSxuLnBvaW50KG1bMF1bMF0sbVswXVsxXSkpKX0hdnx8ZSYmX2UoZSxwKXx8bi5wb2ludChw
WzBdLHBbMV0pLGU9cCxjPXYsaT1kfSxsaW5lRW5kOmZ1bmN0aW9uKCl7YyYmbi5saW5lRW5kKCks
ZT1udWxsfSxjbGVhbjpmdW5jdGlvbigpe3JldHVybiBsfChzJiZjKTw8MX19fWZ1bmN0aW9uIHIo
bix0LGUpe3ZhciByPXBlKG4pLHU9cGUodCksbz1bMSwwLDBdLGE9ZGUocix1KSxjPXZlKGEsYSks
cz1hWzBdLGw9Yy1zKnM7aWYoIWwpcmV0dXJuIWUmJm47dmFyIGY9aSpjL2wsaD0taSpzL2wsZz1k
ZShvLGEpLHA9eWUobyxmKSx2PXllKGEsaCk7bWUocCx2KTt2YXIgZD1nLG09dmUocCxkKSx5PXZl
KGQsZCkseD1tKm0teSoodmUocCxwKS0xKTtpZighKDA+eCkpe3ZhciBNPU1hdGguc3FydCh4KSxf
PXllKGQsKC1tLU0pL3kpO2lmKG1lKF8scCksXz1NZShfKSwhZSlyZXR1cm4gXzt2YXIgYix3PW5b
MF0sUz10WzBdLGs9blsxXSxFPXRbMV07dz5TJiYoYj13LHc9UyxTPWIpO3ZhciBBPVMtdyxDPWZh
KEEtQ2EpPFRhLE49Q3x8VGE+QTtpZighQyYmaz5FJiYoYj1rLGs9RSxFPWIpLE4/Qz9rK0U+MF5f
WzFdPChmYShfWzBdLXcpPFRhP2s6RSk6azw9X1sxXSYmX1sxXTw9RTpBPkNhXih3PD1fWzBdJiZf
WzBdPD1TKSl7dmFyIEw9eWUoZCwoLW0rTSkveSk7cmV0dXJuIG1lKEwscCksW18sTWUoTCldfX19
ZnVuY3Rpb24gdSh0LGUpe3ZhciByPW8/bjpDYS1uLHU9MDtyZXR1cm4tcj50P3V8PTE6dD5yJiYo
dXw9MiksLXI+ZT91fD00OmU+ciYmKHV8PTgpLHV9dmFyIGk9TWF0aC5jb3Mobiksbz1pPjAsYT1m
YShpKT5UYSxjPWdyKG4sNip6YSk7cmV0dXJuIFRlKHQsZSxjLG8/WzAsLW5dOlstQ2Esbi1DYV0p
fWZ1bmN0aW9uIEZlKG4sdCxlLHIpe3JldHVybiBmdW5jdGlvbih1KXt2YXIgaSxvPXUuYSxhPXUu
YixjPW8ueCxzPW8ueSxsPWEueCxmPWEueSxoPTAsZz0xLHA9bC1jLHY9Zi1zO2lmKGk9bi1jLHB8
fCEoaT4wKSl7aWYoaS89cCwwPnApe2lmKGg+aSlyZXR1cm47Zz5pJiYoZz1pKX1lbHNlIGlmKHA+
MCl7aWYoaT5nKXJldHVybjtpPmgmJihoPWkpfWlmKGk9ZS1jLHB8fCEoMD5pKSl7aWYoaS89cCww
PnApe2lmKGk+ZylyZXR1cm47aT5oJiYoaD1pKX1lbHNlIGlmKHA+MCl7aWYoaD5pKXJldHVybjtn
PmkmJihnPWkpfWlmKGk9dC1zLHZ8fCEoaT4wKSl7aWYoaS89diwwPnYpe2lmKGg+aSlyZXR1cm47
Zz5pJiYoZz1pKX1lbHNlIGlmKHY+MCl7aWYoaT5nKXJldHVybjtpPmgmJihoPWkpfWlmKGk9ci1z
LHZ8fCEoMD5pKSl7aWYoaS89diwwPnYpe2lmKGk+ZylyZXR1cm47aT5oJiYoaD1pKX1lbHNlIGlm
KHY+MCl7aWYoaD5pKXJldHVybjtnPmkmJihnPWkpfXJldHVybiBoPjAmJih1LmE9e3g6YytoKnAs
eTpzK2gqdn0pLDE+ZyYmKHUuYj17eDpjK2cqcCx5OnMrZyp2fSksdX19fX19fWZ1bmN0aW9uIE9l
KG4sdCxlLHIpe2Z1bmN0aW9uIHUocix1KXtyZXR1cm4gZmEoclswXS1uKTxUYT91PjA/MDozOmZh
KHJbMF0tZSk8VGE/dT4wPzI6MTpmYShyWzFdLXQpPFRhP3U+MD8xOjA6dT4wPzM6Mn1mdW5jdGlv
biBpKG4sdCl7cmV0dXJuIG8obi54LHQueCl9ZnVuY3Rpb24gbyhuLHQpe3ZhciBlPXUobiwxKSxy
PXUodCwxKTtyZXR1cm4gZSE9PXI/ZS1yOjA9PT1lP3RbMV0tblsxXToxPT09ZT9uWzBdLXRbMF06
Mj09PWU/blsxXS10WzFdOnRbMF0tblswXX1yZXR1cm4gZnVuY3Rpb24oYSl7ZnVuY3Rpb24gYyhu
KXtmb3IodmFyIHQ9MCxlPWQubGVuZ3RoLHI9blsxXSx1PTA7ZT51OysrdSlmb3IodmFyIGksbz0x
LGE9ZFt1XSxjPWEubGVuZ3RoLHM9YVswXTtjPm87KytvKWk9YVtvXSxzWzFdPD1yP2lbMV0+ciYm
SihzLGksbik+MCYmKyt0OmlbMV08PXImJkoocyxpLG4pPDAmJi0tdCxzPWk7cmV0dXJuIDAhPT10
fWZ1bmN0aW9uIHMoaSxhLGMscyl7dmFyIGw9MCxmPTA7aWYobnVsbD09aXx8KGw9dShpLGMpKSE9
PShmPXUoYSxjKSl8fG8oaSxhKTwwXmM+MCl7ZG8gcy5wb2ludCgwPT09bHx8Mz09PWw/bjplLGw+
MT9yOnQpO3doaWxlKChsPShsK2MrNCklNCkhPT1mKX1lbHNlIHMucG9pbnQoYVswXSxhWzFdKX1m
dW5jdGlvbiBsKHUsaSl7cmV0dXJuIHU+PW4mJmU+PXUmJmk+PXQmJnI+PWl9ZnVuY3Rpb24gZihu
LHQpe2wobix0KSYmYS5wb2ludChuLHQpfWZ1bmN0aW9uIGgoKXtOLnBvaW50PXAsZCYmZC5wdXNo
KG09W10pLFM9ITAsdz0hMSxfPWI9MC8wfWZ1bmN0aW9uIGcoKXt2JiYocCh5LHgpLE0mJncmJkEu
cmVqb2luKCksdi5wdXNoKEEuYnVmZmVyKCkpKSxOLnBvaW50PWYsdyYmYS5saW5lRW5kKCl9ZnVu
Y3Rpb24gcChuLHQpe249TWF0aC5tYXgoLVRjLE1hdGgubWluKFRjLG4pKSx0PU1hdGgubWF4KC1U
YyxNYXRoLm1pbihUYyx0KSk7dmFyIGU9bChuLHQpO2lmKGQmJm0ucHVzaChbbix0XSksUyl5PW4s
eD10LE09ZSxTPSExLGUmJihhLmxpbmVTdGFydCgpLGEucG9pbnQobix0KSk7ZWxzZSBpZihlJiZ3
KWEucG9pbnQobix0KTtlbHNle3ZhciByPXthOnt4Ol8seTpifSxiOnt4Om4seTp0fX07QyhyKT8o
d3x8KGEubGluZVN0YXJ0KCksYS5wb2ludChyLmEueCxyLmEueSkpLGEucG9pbnQoci5iLngsci5i
LnkpLGV8fGEubGluZUVuZCgpLGs9ITEpOmUmJihhLmxpbmVTdGFydCgpLGEucG9pbnQobix0KSxr
PSExKX1fPW4sYj10LHc9ZX12YXIgdixkLG0seSx4LE0sXyxiLHcsUyxrLEU9YSxBPXplKCksQz1G
ZShuLHQsZSxyKSxOPXtwb2ludDpmLGxpbmVTdGFydDpoLGxpbmVFbmQ6Zyxwb2x5Z29uU3RhcnQ6
ZnVuY3Rpb24oKXthPUEsdj1bXSxkPVtdLGs9ITB9LHBvbHlnb25FbmQ6ZnVuY3Rpb24oKXthPUUs
dj1Hby5tZXJnZSh2KTt2YXIgdD1jKFtuLHJdKSxlPWsmJnQsdT12Lmxlbmd0aDsoZXx8dSkmJihh
LnBvbHlnb25TdGFydCgpLGUmJihhLmxpbmVTdGFydCgpLHMobnVsbCxudWxsLDEsYSksYS5saW5l
RW5kKCkpLHUmJkNlKHYsaSx0LHMsYSksYS5wb2x5Z29uRW5kKCkpLHY9ZD1tPW51bGx9fTtyZXR1
cm4gTn19ZnVuY3Rpb24gSWUobix0KXtmdW5jdGlvbiBlKGUscil7cmV0dXJuIGU9bihlLHIpLHQo
ZVswXSxlWzFdKX1yZXR1cm4gbi5pbnZlcnQmJnQuaW52ZXJ0JiYoZS5pbnZlcnQ9ZnVuY3Rpb24o
ZSxyKXtyZXR1cm4gZT10LmludmVydChlLHIpLGUmJm4uaW52ZXJ0KGVbMF0sZVsxXSl9KSxlfWZ1
bmN0aW9uIFllKG4pe3ZhciB0PTAsZT1DYS8zLHI9aXIobiksdT1yKHQsZSk7cmV0dXJuIHUucGFy
YWxsZWxzPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoP3IodD1uWzBdKkNhLzE4
MCxlPW5bMV0qQ2EvMTgwKTpbMTgwKih0L0NhKSwxODAqKGUvQ2EpXX0sdX1mdW5jdGlvbiBaZShu
LHQpe2Z1bmN0aW9uIGUobix0KXt2YXIgZT1NYXRoLnNxcnQoaS0yKnUqTWF0aC5zaW4odCkpL3U7
cmV0dXJuW2UqTWF0aC5zaW4obio9dSksby1lKk1hdGguY29zKG4pXX12YXIgcj1NYXRoLnNpbihu
KSx1PShyK01hdGguc2luKHQpKS8yLGk9MStyKigyKnUtciksbz1NYXRoLnNxcnQoaSkvdTtyZXR1
cm4gZS5pbnZlcnQ9ZnVuY3Rpb24obix0KXt2YXIgZT1vLXQ7cmV0dXJuW01hdGguYXRhbjIobixl
KS91LEcoKGktKG4qbitlKmUpKnUqdSkvKDIqdSkpXX0sZX1mdW5jdGlvbiBWZSgpe2Z1bmN0aW9u
IG4obix0KXt6Yys9dSpuLXIqdCxyPW4sdT10fXZhciB0LGUscix1O2pjLnBvaW50PWZ1bmN0aW9u
KGksbyl7amMucG9pbnQ9bix0PXI9aSxlPXU9b30samMubGluZUVuZD1mdW5jdGlvbigpe24odCxl
KX19ZnVuY3Rpb24gJGUobix0KXtSYz5uJiYoUmM9biksbj5QYyYmKFBjPW4pLERjPnQmJihEYz10
KSx0PlVjJiYoVWM9dCl9ZnVuY3Rpb24gWGUoKXtmdW5jdGlvbiBuKG4sdCl7by5wdXNoKCJNIixu
LCIsIix0LGkpfWZ1bmN0aW9uIHQobix0KXtvLnB1c2goIk0iLG4sIiwiLHQpLGEucG9pbnQ9ZX1m
dW5jdGlvbiBlKG4sdCl7by5wdXNoKCJMIixuLCIsIix0KX1mdW5jdGlvbiByKCl7YS5wb2ludD1u
fWZ1bmN0aW9uIHUoKXtvLnB1c2goIloiKX12YXIgaT1CZSg0LjUpLG89W10sYT17cG9pbnQ6bixs
aW5lU3RhcnQ6ZnVuY3Rpb24oKXthLnBvaW50PXR9LGxpbmVFbmQ6cixwb2x5Z29uU3RhcnQ6ZnVu
Y3Rpb24oKXthLmxpbmVFbmQ9dX0scG9seWdvbkVuZDpmdW5jdGlvbigpe2EubGluZUVuZD1yLGEu
cG9pbnQ9bn0scG9pbnRSYWRpdXM6ZnVuY3Rpb24obil7cmV0dXJuIGk9QmUobiksYX0scmVzdWx0
OmZ1bmN0aW9uKCl7aWYoby5sZW5ndGgpe3ZhciBuPW8uam9pbigiIik7cmV0dXJuIG89W10sbn19
fTtyZXR1cm4gYX1mdW5jdGlvbiBCZShuKXtyZXR1cm4ibTAsIituKyJhIituKyIsIituKyIgMCAx
LDEgMCwiKy0yKm4rImEiK24rIiwiK24rIiAwIDEsMSAwLCIrMipuKyJ6In1mdW5jdGlvbiBKZShu
LHQpe01jKz1uLF9jKz10LCsrYmN9ZnVuY3Rpb24gV2UoKXtmdW5jdGlvbiBuKG4scil7dmFyIHU9
bi10LGk9ci1lLG89TWF0aC5zcXJ0KHUqdStpKmkpO3djKz1vKih0K24pLzIsU2MrPW8qKGUrcikv
MixrYys9byxKZSh0PW4sZT1yKX12YXIgdCxlO0ZjLnBvaW50PWZ1bmN0aW9uKHIsdSl7RmMucG9p
bnQ9bixKZSh0PXIsZT11KX19ZnVuY3Rpb24gR2UoKXtGYy5wb2ludD1KZX1mdW5jdGlvbiBLZSgp
e2Z1bmN0aW9uIG4obix0KXt2YXIgZT1uLXIsaT10LXUsbz1NYXRoLnNxcnQoZSplK2kqaSk7d2Mr
PW8qKHIrbikvMixTYys9byoodSt0KS8yLGtjKz1vLG89dSpuLXIqdCxFYys9byoocituKSxBYys9
byoodSt0KSxDYys9MypvLEplKHI9bix1PXQpfXZhciB0LGUscix1O0ZjLnBvaW50PWZ1bmN0aW9u
KGksbyl7RmMucG9pbnQ9bixKZSh0PXI9aSxlPXU9byl9LEZjLmxpbmVFbmQ9ZnVuY3Rpb24oKXtu
KHQsZSl9fWZ1bmN0aW9uIFFlKG4pe2Z1bmN0aW9uIHQodCxlKXtuLm1vdmVUbyh0LGUpLG4uYXJj
KHQsZSxvLDAsTmEpfWZ1bmN0aW9uIGUodCxlKXtuLm1vdmVUbyh0LGUpLGEucG9pbnQ9cn1mdW5j
dGlvbiByKHQsZSl7bi5saW5lVG8odCxlKX1mdW5jdGlvbiB1KCl7YS5wb2ludD10fWZ1bmN0aW9u
IGkoKXtuLmNsb3NlUGF0aCgpfXZhciBvPTQuNSxhPXtwb2ludDp0LGxpbmVTdGFydDpmdW5jdGlv
bigpe2EucG9pbnQ9ZX0sbGluZUVuZDp1LHBvbHlnb25TdGFydDpmdW5jdGlvbigpe2EubGluZUVu
ZD1pfSxwb2x5Z29uRW5kOmZ1bmN0aW9uKCl7YS5saW5lRW5kPXUsYS5wb2ludD10fSxwb2ludFJh
ZGl1czpmdW5jdGlvbihuKXtyZXR1cm4gbz1uLGF9LHJlc3VsdDp2fTtyZXR1cm4gYX1mdW5jdGlv
biBucihuKXtmdW5jdGlvbiB0KG4pe3JldHVybihhP3I6ZSkobil9ZnVuY3Rpb24gZSh0KXtyZXR1
cm4gcnIodCxmdW5jdGlvbihlLHIpe2U9bihlLHIpLHQucG9pbnQoZVswXSxlWzFdKX0pfWZ1bmN0
aW9uIHIodCl7ZnVuY3Rpb24gZShlLHIpe2U9bihlLHIpLHQucG9pbnQoZVswXSxlWzFdKX1mdW5j
dGlvbiByKCl7eD0wLzAsUy5wb2ludD1pLHQubGluZVN0YXJ0KCl9ZnVuY3Rpb24gaShlLHIpe3Zh
ciBpPXBlKFtlLHJdKSxvPW4oZSxyKTt1KHgsTSx5LF8sYix3LHg9b1swXSxNPW9bMV0seT1lLF89
aVswXSxiPWlbMV0sdz1pWzJdLGEsdCksdC5wb2ludCh4LE0pfWZ1bmN0aW9uIG8oKXtTLnBvaW50
PWUsdC5saW5lRW5kKCl9ZnVuY3Rpb24gYygpe3IoKSxTLnBvaW50PXMsUy5saW5lRW5kPWx9ZnVu
Y3Rpb24gcyhuLHQpe2koZj1uLGg9dCksZz14LHA9TSx2PV8sZD1iLG09dyxTLnBvaW50PWl9ZnVu
Y3Rpb24gbCgpe3UoeCxNLHksXyxiLHcsZyxwLGYsdixkLG0sYSx0KSxTLmxpbmVFbmQ9byxvKCl9
dmFyIGYsaCxnLHAsdixkLG0seSx4LE0sXyxiLHcsUz17cG9pbnQ6ZSxsaW5lU3RhcnQ6cixsaW5l
RW5kOm8scG9seWdvblN0YXJ0OmZ1bmN0aW9uKCl7dC5wb2x5Z29uU3RhcnQoKSxTLmxpbmVTdGFy
dD1jfSxwb2x5Z29uRW5kOmZ1bmN0aW9uKCl7dC5wb2x5Z29uRW5kKCksUy5saW5lU3RhcnQ9cn19
O3JldHVybiBTfWZ1bmN0aW9uIHUodCxlLHIsYSxjLHMsbCxmLGgsZyxwLHYsZCxtKXt2YXIgeT1s
LXQseD1mLWUsTT15KnkreCp4O2lmKE0+NCppJiZkLS0pe3ZhciBfPWErZyxiPWMrcCx3PXMrdixT
PU1hdGguc3FydChfKl8rYipiK3cqdyksaz1NYXRoLmFzaW4ody89UyksRT1mYShmYSh3KS0xKTxU
YXx8ZmEoci1oKTxUYT8ocitoKS8yOk1hdGguYXRhbjIoYixfKSxBPW4oRSxrKSxDPUFbMF0sTj1B
WzFdLEw9Qy10LFQ9Ti1lLHE9eCpMLXkqVDsocSpxL00+aXx8ZmEoKHkqTCt4KlQpL00tLjUpPi4z
fHxvPmEqZytjKnArcyp2KSYmKHUodCxlLHIsYSxjLHMsQyxOLEUsXy89UyxiLz1TLHcsZCxtKSxt
LnBvaW50KEMsTiksdShDLE4sRSxfLGIsdyxsLGYsaCxnLHAsdixkLG0pKX19dmFyIGk9LjUsbz1N
YXRoLmNvcygzMCp6YSksYT0xNjtyZXR1cm4gdC5wcmVjaXNpb249ZnVuY3Rpb24obil7cmV0dXJu
IGFyZ3VtZW50cy5sZW5ndGg/KGE9KGk9bipuKT4wJiYxNix0KTpNYXRoLnNxcnQoaSl9LHR9ZnVu
Y3Rpb24gdHIobil7dmFyIHQ9bnIoZnVuY3Rpb24odCxlKXtyZXR1cm4gbihbdCpSYSxlKlJhXSl9
KTtyZXR1cm4gZnVuY3Rpb24obil7cmV0dXJuIG9yKHQobikpfX1mdW5jdGlvbiBlcihuKXt0aGlz
LnN0cmVhbT1ufWZ1bmN0aW9uIHJyKG4sdCl7cmV0dXJue3BvaW50OnQsc3BoZXJlOmZ1bmN0aW9u
KCl7bi5zcGhlcmUoKX0sbGluZVN0YXJ0OmZ1bmN0aW9uKCl7bi5saW5lU3RhcnQoKX0sbGluZUVu
ZDpmdW5jdGlvbigpe24ubGluZUVuZCgpfSxwb2x5Z29uU3RhcnQ6ZnVuY3Rpb24oKXtuLnBvbHln
b25TdGFydCgpfSxwb2x5Z29uRW5kOmZ1bmN0aW9uKCl7bi5wb2x5Z29uRW5kKCl9fX1mdW5jdGlv
biB1cihuKXtyZXR1cm4gaXIoZnVuY3Rpb24oKXtyZXR1cm4gbn0pKCl9ZnVuY3Rpb24gaXIobil7
ZnVuY3Rpb24gdChuKXtyZXR1cm4gbj1hKG5bMF0qemEsblsxXSp6YSksW25bMF0qaCtjLHMtblsx
XSpoXX1mdW5jdGlvbiBlKG4pe3JldHVybiBuPWEuaW52ZXJ0KChuWzBdLWMpL2gsKHMtblsxXSkv
aCksbiYmW25bMF0qUmEsblsxXSpSYV19ZnVuY3Rpb24gcigpe2E9SWUobz1zcihtLHkseCksaSk7
dmFyIG49aSh2LGQpO3JldHVybiBjPWctblswXSpoLHM9cCtuWzFdKmgsdSgpCn1mdW5jdGlvbiB1
KCl7cmV0dXJuIGwmJihsLnZhbGlkPSExLGw9bnVsbCksdH12YXIgaSxvLGEsYyxzLGwsZj1ucihm
dW5jdGlvbihuLHQpe3JldHVybiBuPWkobix0KSxbblswXSpoK2Mscy1uWzFdKmhdfSksaD0xNTAs
Zz00ODAscD0yNTAsdj0wLGQ9MCxtPTAseT0wLHg9MCxNPUxjLF89QXQsYj1udWxsLHc9bnVsbDty
ZXR1cm4gdC5zdHJlYW09ZnVuY3Rpb24obil7cmV0dXJuIGwmJihsLnZhbGlkPSExKSxsPW9yKE0o
byxmKF8obikpKSksbC52YWxpZD0hMCxsfSx0LmNsaXBBbmdsZT1mdW5jdGlvbihuKXtyZXR1cm4g
YXJndW1lbnRzLmxlbmd0aD8oTT1udWxsPT1uPyhiPW4sTGMpOkhlKChiPStuKSp6YSksdSgpKTpi
fSx0LmNsaXBFeHRlbnQ9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHc9bixf
PW4/T2UoblswXVswXSxuWzBdWzFdLG5bMV1bMF0sblsxXVsxXSk6QXQsdSgpKTp3fSx0LnNjYWxl
PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhoPStuLHIoKSk6aH0sdC50cmFu
c2xhdGU9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGc9K25bMF0scD0rblsx
XSxyKCkpOltnLHBdfSx0LmNlbnRlcj1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0
aD8odj1uWzBdJTM2MCp6YSxkPW5bMV0lMzYwKnphLHIoKSk6W3YqUmEsZCpSYV19LHQucm90YXRl
PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhtPW5bMF0lMzYwKnphLHk9blsx
XSUzNjAqemEseD1uLmxlbmd0aD4yP25bMl0lMzYwKnphOjAscigpKTpbbSpSYSx5KlJhLHgqUmFd
fSxHby5yZWJpbmQodCxmLCJwcmVjaXNpb24iKSxmdW5jdGlvbigpe3JldHVybiBpPW4uYXBwbHko
dGhpcyxhcmd1bWVudHMpLHQuaW52ZXJ0PWkuaW52ZXJ0JiZlLHIoKX19ZnVuY3Rpb24gb3Iobil7
cmV0dXJuIHJyKG4sZnVuY3Rpb24odCxlKXtuLnBvaW50KHQqemEsZSp6YSl9KX1mdW5jdGlvbiBh
cihuLHQpe3JldHVybltuLHRdfWZ1bmN0aW9uIGNyKG4sdCl7cmV0dXJuW24+Q2E/bi1OYTotQ2E+
bj9uK05hOm4sdF19ZnVuY3Rpb24gc3Iobix0LGUpe3JldHVybiBuP3R8fGU/SWUoZnIobiksaHIo
dCxlKSk6ZnIobik6dHx8ZT9ocih0LGUpOmNyfWZ1bmN0aW9uIGxyKG4pe3JldHVybiBmdW5jdGlv
bih0LGUpe3JldHVybiB0Kz1uLFt0PkNhP3QtTmE6LUNhPnQ/dCtOYTp0LGVdfX1mdW5jdGlvbiBm
cihuKXt2YXIgdD1scihuKTtyZXR1cm4gdC5pbnZlcnQ9bHIoLW4pLHR9ZnVuY3Rpb24gaHIobix0
KXtmdW5jdGlvbiBlKG4sdCl7dmFyIGU9TWF0aC5jb3ModCksYT1NYXRoLmNvcyhuKSplLGM9TWF0
aC5zaW4obikqZSxzPU1hdGguc2luKHQpLGw9cypyK2EqdTtyZXR1cm5bTWF0aC5hdGFuMihjKmkt
bCpvLGEqci1zKnUpLEcobCppK2MqbyldfXZhciByPU1hdGguY29zKG4pLHU9TWF0aC5zaW4obiks
aT1NYXRoLmNvcyh0KSxvPU1hdGguc2luKHQpO3JldHVybiBlLmludmVydD1mdW5jdGlvbihuLHQp
e3ZhciBlPU1hdGguY29zKHQpLGE9TWF0aC5jb3MobikqZSxjPU1hdGguc2luKG4pKmUscz1NYXRo
LnNpbih0KSxsPXMqaS1jKm87cmV0dXJuW01hdGguYXRhbjIoYyppK3MqbyxhKnIrbCp1KSxHKGwq
ci1hKnUpXX0sZX1mdW5jdGlvbiBncihuLHQpe3ZhciBlPU1hdGguY29zKG4pLHI9TWF0aC5zaW4o
bik7cmV0dXJuIGZ1bmN0aW9uKHUsaSxvLGEpe3ZhciBjPW8qdDtudWxsIT11Pyh1PXByKGUsdSks
aT1wcihlLGkpLChvPjA/aT51OnU+aSkmJih1Kz1vKk5hKSk6KHU9bitvKk5hLGk9bi0uNSpjKTtm
b3IodmFyIHMsbD11O28+MD9sPmk6aT5sO2wtPWMpYS5wb2ludCgocz1NZShbZSwtcipNYXRoLmNv
cyhsKSwtcipNYXRoLnNpbihsKV0pKVswXSxzWzFdKX19ZnVuY3Rpb24gcHIobix0KXt2YXIgZT1w
ZSh0KTtlWzBdLT1uLHhlKGUpO3ZhciByPVcoLWVbMV0pO3JldHVybigoLWVbMl08MD8tcjpyKSsy
Kk1hdGguUEktVGEpJSgyKk1hdGguUEkpfWZ1bmN0aW9uIHZyKG4sdCxlKXt2YXIgcj1Hby5yYW5n
ZShuLHQtVGEsZSkuY29uY2F0KHQpO3JldHVybiBmdW5jdGlvbihuKXtyZXR1cm4gci5tYXAoZnVu
Y3Rpb24odCl7cmV0dXJuW24sdF19KX19ZnVuY3Rpb24gZHIobix0LGUpe3ZhciByPUdvLnJhbmdl
KG4sdC1UYSxlKS5jb25jYXQodCk7cmV0dXJuIGZ1bmN0aW9uKG4pe3JldHVybiByLm1hcChmdW5j
dGlvbih0KXtyZXR1cm5bdCxuXX0pfX1mdW5jdGlvbiBtcihuKXtyZXR1cm4gbi5zb3VyY2V9ZnVu
Y3Rpb24geXIobil7cmV0dXJuIG4udGFyZ2V0fWZ1bmN0aW9uIHhyKG4sdCxlLHIpe3ZhciB1PU1h
dGguY29zKHQpLGk9TWF0aC5zaW4odCksbz1NYXRoLmNvcyhyKSxhPU1hdGguc2luKHIpLGM9dSpN
YXRoLmNvcyhuKSxzPXUqTWF0aC5zaW4obiksbD1vKk1hdGguY29zKGUpLGY9bypNYXRoLnNpbihl
KSxoPTIqTWF0aC5hc2luKE1hdGguc3FydCh0dChyLXQpK3Uqbyp0dChlLW4pKSksZz0xL01hdGgu
c2luKGgpLHA9aD9mdW5jdGlvbihuKXt2YXIgdD1NYXRoLnNpbihuKj1oKSpnLGU9TWF0aC5zaW4o
aC1uKSpnLHI9ZSpjK3QqbCx1PWUqcyt0KmYsbz1lKmkrdCphO3JldHVybltNYXRoLmF0YW4yKHUs
cikqUmEsTWF0aC5hdGFuMihvLE1hdGguc3FydChyKnIrdSp1KSkqUmFdfTpmdW5jdGlvbigpe3Jl
dHVybltuKlJhLHQqUmFdfTtyZXR1cm4gcC5kaXN0YW5jZT1oLHB9ZnVuY3Rpb24gTXIoKXtmdW5j
dGlvbiBuKG4sdSl7dmFyIGk9TWF0aC5zaW4odSo9emEpLG89TWF0aC5jb3ModSksYT1mYSgobio9
emEpLXQpLGM9TWF0aC5jb3MoYSk7T2MrPU1hdGguYXRhbjIoTWF0aC5zcXJ0KChhPW8qTWF0aC5z
aW4oYSkpKmErKGE9cippLWUqbypjKSphKSxlKmkrcipvKmMpLHQ9bixlPWkscj1vfXZhciB0LGUs
cjtJYy5wb2ludD1mdW5jdGlvbih1LGkpe3Q9dSp6YSxlPU1hdGguc2luKGkqPXphKSxyPU1hdGgu
Y29zKGkpLEljLnBvaW50PW59LEljLmxpbmVFbmQ9ZnVuY3Rpb24oKXtJYy5wb2ludD1JYy5saW5l
RW5kPXZ9fWZ1bmN0aW9uIF9yKG4sdCl7ZnVuY3Rpb24gZSh0LGUpe3ZhciByPU1hdGguY29zKHQp
LHU9TWF0aC5jb3MoZSksaT1uKHIqdSk7cmV0dXJuW2kqdSpNYXRoLnNpbih0KSxpKk1hdGguc2lu
KGUpXX1yZXR1cm4gZS5pbnZlcnQ9ZnVuY3Rpb24obixlKXt2YXIgcj1NYXRoLnNxcnQobipuK2Uq
ZSksdT10KHIpLGk9TWF0aC5zaW4odSksbz1NYXRoLmNvcyh1KTtyZXR1cm5bTWF0aC5hdGFuMihu
KmkscipvKSxNYXRoLmFzaW4ociYmZSppL3IpXX0sZX1mdW5jdGlvbiBicihuLHQpe2Z1bmN0aW9u
IGUobix0KXtvPjA/LUxhK1RhPnQmJih0PS1MYStUYSk6dD5MYS1UYSYmKHQ9TGEtVGEpO3ZhciBl
PW8vTWF0aC5wb3codSh0KSxpKTtyZXR1cm5bZSpNYXRoLnNpbihpKm4pLG8tZSpNYXRoLmNvcyhp
Km4pXX12YXIgcj1NYXRoLmNvcyhuKSx1PWZ1bmN0aW9uKG4pe3JldHVybiBNYXRoLnRhbihDYS80
K24vMil9LGk9bj09PXQ/TWF0aC5zaW4obik6TWF0aC5sb2coci9NYXRoLmNvcyh0KSkvTWF0aC5s
b2codSh0KS91KG4pKSxvPXIqTWF0aC5wb3codShuKSxpKS9pO3JldHVybiBpPyhlLmludmVydD1m
dW5jdGlvbihuLHQpe3ZhciBlPW8tdCxyPUIoaSkqTWF0aC5zcXJ0KG4qbitlKmUpO3JldHVybltN
YXRoLmF0YW4yKG4sZSkvaSwyKk1hdGguYXRhbihNYXRoLnBvdyhvL3IsMS9pKSktTGFdfSxlKTpT
cn1mdW5jdGlvbiB3cihuLHQpe2Z1bmN0aW9uIGUobix0KXt2YXIgZT1pLXQ7cmV0dXJuW2UqTWF0
aC5zaW4odSpuKSxpLWUqTWF0aC5jb3ModSpuKV19dmFyIHI9TWF0aC5jb3MobiksdT1uPT09dD9N
YXRoLnNpbihuKTooci1NYXRoLmNvcyh0KSkvKHQtbiksaT1yL3UrbjtyZXR1cm4gZmEodSk8VGE/
YXI6KGUuaW52ZXJ0PWZ1bmN0aW9uKG4sdCl7dmFyIGU9aS10O3JldHVybltNYXRoLmF0YW4yKG4s
ZSkvdSxpLUIodSkqTWF0aC5zcXJ0KG4qbitlKmUpXX0sZSl9ZnVuY3Rpb24gU3Iobix0KXtyZXR1
cm5bbixNYXRoLmxvZyhNYXRoLnRhbihDYS80K3QvMikpXX1mdW5jdGlvbiBrcihuKXt2YXIgdCxl
PXVyKG4pLHI9ZS5zY2FsZSx1PWUudHJhbnNsYXRlLGk9ZS5jbGlwRXh0ZW50O3JldHVybiBlLnNj
YWxlPWZ1bmN0aW9uKCl7dmFyIG49ci5hcHBseShlLGFyZ3VtZW50cyk7cmV0dXJuIG49PT1lP3Q/
ZS5jbGlwRXh0ZW50KG51bGwpOmU6bn0sZS50cmFuc2xhdGU9ZnVuY3Rpb24oKXt2YXIgbj11LmFw
cGx5KGUsYXJndW1lbnRzKTtyZXR1cm4gbj09PWU/dD9lLmNsaXBFeHRlbnQobnVsbCk6ZTpufSxl
LmNsaXBFeHRlbnQ9ZnVuY3Rpb24obil7dmFyIG89aS5hcHBseShlLGFyZ3VtZW50cyk7aWYobz09
PWUpe2lmKHQ9bnVsbD09bil7dmFyIGE9Q2EqcigpLGM9dSgpO2koW1tjWzBdLWEsY1sxXS1hXSxb
Y1swXSthLGNbMV0rYV1dKX19ZWxzZSB0JiYobz1udWxsKTtyZXR1cm4gb30sZS5jbGlwRXh0ZW50
KG51bGwpfWZ1bmN0aW9uIEVyKG4sdCl7cmV0dXJuW01hdGgubG9nKE1hdGgudGFuKENhLzQrdC8y
KSksLW5dfWZ1bmN0aW9uIEFyKG4pe3JldHVybiBuWzBdfWZ1bmN0aW9uIENyKG4pe3JldHVybiBu
WzFdfWZ1bmN0aW9uIE5yKG4pe2Zvcih2YXIgdD1uLmxlbmd0aCxlPVswLDFdLHI9Mix1PTI7dD51
O3UrKyl7Zm9yKDtyPjEmJkoobltlW3ItMl1dLG5bZVtyLTFdXSxuW3VdKTw9MDspLS1yO2Vbcisr
XT11fXJldHVybiBlLnNsaWNlKDAscil9ZnVuY3Rpb24gTHIobix0KXtyZXR1cm4gblswXS10WzBd
fHxuWzFdLXRbMV19ZnVuY3Rpb24gVHIobix0LGUpe3JldHVybihlWzBdLXRbMF0pKihuWzFdLXRb
MV0pPChlWzFdLXRbMV0pKihuWzBdLXRbMF0pfWZ1bmN0aW9uIHFyKG4sdCxlLHIpe3ZhciB1PW5b
MF0saT1lWzBdLG89dFswXS11LGE9clswXS1pLGM9blsxXSxzPWVbMV0sbD10WzFdLWMsZj1yWzFd
LXMsaD0oYSooYy1zKS1mKih1LWkpKS8oZipvLWEqbCk7cmV0dXJuW3UraCpvLGMraCpsXX1mdW5j
dGlvbiB6cihuKXt2YXIgdD1uWzBdLGU9bltuLmxlbmd0aC0xXTtyZXR1cm4hKHRbMF0tZVswXXx8
dFsxXS1lWzFdKX1mdW5jdGlvbiBScigpe3R1KHRoaXMpLHRoaXMuZWRnZT10aGlzLnNpdGU9dGhp
cy5jaXJjbGU9bnVsbH1mdW5jdGlvbiBEcihuKXt2YXIgdD1ucy5wb3AoKXx8bmV3IFJyO3JldHVy
biB0LnNpdGU9bix0fWZ1bmN0aW9uIFByKG4peyRyKG4pLEdjLnJlbW92ZShuKSxucy5wdXNoKG4p
LHR1KG4pfWZ1bmN0aW9uIFVyKG4pe3ZhciB0PW4uY2lyY2xlLGU9dC54LHI9dC5jeSx1PXt4OmUs
eTpyfSxpPW4uUCxvPW4uTixhPVtuXTtQcihuKTtmb3IodmFyIGM9aTtjLmNpcmNsZSYmZmEoZS1j
LmNpcmNsZS54KTxUYSYmZmEoci1jLmNpcmNsZS5jeSk8VGE7KWk9Yy5QLGEudW5zaGlmdChjKSxQ
cihjKSxjPWk7YS51bnNoaWZ0KGMpLCRyKGMpO2Zvcih2YXIgcz1vO3MuY2lyY2xlJiZmYShlLXMu
Y2lyY2xlLngpPFRhJiZmYShyLXMuY2lyY2xlLmN5KTxUYTspbz1zLk4sYS5wdXNoKHMpLFByKHMp
LHM9bzthLnB1c2gocyksJHIocyk7dmFyIGwsZj1hLmxlbmd0aDtmb3IobD0xO2Y+bDsrK2wpcz1h
W2xdLGM9YVtsLTFdLEtyKHMuZWRnZSxjLnNpdGUscy5zaXRlLHUpO2M9YVswXSxzPWFbZi0xXSxz
LmVkZ2U9V3IoYy5zaXRlLHMuc2l0ZSxudWxsLHUpLFZyKGMpLFZyKHMpfWZ1bmN0aW9uIGpyKG4p
e2Zvcih2YXIgdCxlLHIsdSxpPW4ueCxvPW4ueSxhPUdjLl87YTspaWYocj1IcihhLG8pLWkscj5U
YSlhPWEuTDtlbHNle2lmKHU9aS1GcihhLG8pLCEodT5UYSkpe3I+LVRhPyh0PWEuUCxlPWEpOnU+
LVRhPyh0PWEsZT1hLk4pOnQ9ZT1hO2JyZWFrfWlmKCFhLlIpe3Q9YTticmVha31hPWEuUn12YXIg
Yz1EcihuKTtpZihHYy5pbnNlcnQodCxjKSx0fHxlKXtpZih0PT09ZSlyZXR1cm4gJHIodCksZT1E
cih0LnNpdGUpLEdjLmluc2VydChjLGUpLGMuZWRnZT1lLmVkZ2U9V3IodC5zaXRlLGMuc2l0ZSks
VnIodCksVnIoZSksdm9pZCAwO2lmKCFlKXJldHVybiBjLmVkZ2U9V3IodC5zaXRlLGMuc2l0ZSks
dm9pZCAwOyRyKHQpLCRyKGUpO3ZhciBzPXQuc2l0ZSxsPXMueCxmPXMueSxoPW4ueC1sLGc9bi55
LWYscD1lLnNpdGUsdj1wLngtbCxkPXAueS1mLG09MiooaCpkLWcqdikseT1oKmgrZypnLHg9dip2
K2QqZCxNPXt4OihkKnktZyp4KS9tK2wseTooaCp4LXYqeSkvbStmfTtLcihlLmVkZ2UscyxwLE0p
LGMuZWRnZT1XcihzLG4sbnVsbCxNKSxlLmVkZ2U9V3IobixwLG51bGwsTSksVnIodCksVnIoZSl9
fWZ1bmN0aW9uIEhyKG4sdCl7dmFyIGU9bi5zaXRlLHI9ZS54LHU9ZS55LGk9dS10O2lmKCFpKXJl
dHVybiByO3ZhciBvPW4uUDtpZighbylyZXR1cm4tMS8wO2U9by5zaXRlO3ZhciBhPWUueCxjPWUu
eSxzPWMtdDtpZighcylyZXR1cm4gYTt2YXIgbD1hLXIsZj0xL2ktMS9zLGg9bC9zO3JldHVybiBm
PygtaCtNYXRoLnNxcnQoaCpoLTIqZioobCpsLygtMipzKS1jK3MvMit1LWkvMikpKS9mK3I6KHIr
YSkvMn1mdW5jdGlvbiBGcihuLHQpe3ZhciBlPW4uTjtpZihlKXJldHVybiBIcihlLHQpO3ZhciBy
PW4uc2l0ZTtyZXR1cm4gci55PT09dD9yLng6MS8wfWZ1bmN0aW9uIE9yKG4pe3RoaXMuc2l0ZT1u
LHRoaXMuZWRnZXM9W119ZnVuY3Rpb24gSXIobil7Zm9yKHZhciB0LGUscix1LGksbyxhLGMscyxs
LGY9blswXVswXSxoPW5bMV1bMF0sZz1uWzBdWzFdLHA9blsxXVsxXSx2PVdjLGQ9di5sZW5ndGg7
ZC0tOylpZihpPXZbZF0saSYmaS5wcmVwYXJlKCkpZm9yKGE9aS5lZGdlcyxjPWEubGVuZ3RoLG89
MDtjPm87KWw9YVtvXS5lbmQoKSxyPWwueCx1PWwueSxzPWFbKytvJWNdLnN0YXJ0KCksdD1zLngs
ZT1zLnksKGZhKHItdCk+VGF8fGZhKHUtZSk+VGEpJiYoYS5zcGxpY2UobywwLG5ldyBRcihHcihp
LnNpdGUsbCxmYShyLWYpPFRhJiZwLXU+VGE/e3g6Zix5OmZhKHQtZik8VGE/ZTpwfTpmYSh1LXAp
PFRhJiZoLXI+VGE/e3g6ZmEoZS1wKTxUYT90OmgseTpwfTpmYShyLWgpPFRhJiZ1LWc+VGE/e3g6
aCx5OmZhKHQtaCk8VGE/ZTpnfTpmYSh1LWcpPFRhJiZyLWY+VGE/e3g6ZmEoZS1nKTxUYT90OmYs
eTpnfTpudWxsKSxpLnNpdGUsbnVsbCkpLCsrYyl9ZnVuY3Rpb24gWXIobix0KXtyZXR1cm4gdC5h
bmdsZS1uLmFuZ2xlfWZ1bmN0aW9uIFpyKCl7dHUodGhpcyksdGhpcy54PXRoaXMueT10aGlzLmFy
Yz10aGlzLnNpdGU9dGhpcy5jeT1udWxsfWZ1bmN0aW9uIFZyKG4pe3ZhciB0PW4uUCxlPW4uTjtp
Zih0JiZlKXt2YXIgcj10LnNpdGUsdT1uLnNpdGUsaT1lLnNpdGU7aWYociE9PWkpe3ZhciBvPXUu
eCxhPXUueSxjPXIueC1vLHM9ci55LWEsbD1pLngtbyxmPWkueS1hLGg9MiooYypmLXMqbCk7aWYo
IShoPj0tcWEpKXt2YXIgZz1jKmMrcypzLHA9bCpsK2YqZix2PShmKmctcypwKS9oLGQ9KGMqcC1s
KmcpL2gsZj1kK2EsbT10cy5wb3AoKXx8bmV3IFpyO20uYXJjPW4sbS5zaXRlPXUsbS54PXYrbyxt
Lnk9ZitNYXRoLnNxcnQodip2K2QqZCksbS5jeT1mLG4uY2lyY2xlPW07Zm9yKHZhciB5PW51bGws
eD1RYy5fO3g7KWlmKG0ueTx4Lnl8fG0ueT09PXgueSYmbS54PD14Lngpe2lmKCF4Lkwpe3k9eC5Q
O2JyZWFrfXg9eC5MfWVsc2V7aWYoIXguUil7eT14O2JyZWFrfXg9eC5SfVFjLmluc2VydCh5LG0p
LHl8fChLYz1tKX19fX1mdW5jdGlvbiAkcihuKXt2YXIgdD1uLmNpcmNsZTt0JiYodC5QfHwoS2M9
dC5OKSxRYy5yZW1vdmUodCksdHMucHVzaCh0KSx0dSh0KSxuLmNpcmNsZT1udWxsKX1mdW5jdGlv
biBYcihuKXtmb3IodmFyIHQsZT1KYyxyPUZlKG5bMF1bMF0sblswXVsxXSxuWzFdWzBdLG5bMV1b
MV0pLHU9ZS5sZW5ndGg7dS0tOyl0PWVbdV0sKCFCcih0LG4pfHwhcih0KXx8ZmEodC5hLngtdC5i
LngpPFRhJiZmYSh0LmEueS10LmIueSk8VGEpJiYodC5hPXQuYj1udWxsLGUuc3BsaWNlKHUsMSkp
fWZ1bmN0aW9uIEJyKG4sdCl7dmFyIGU9bi5iO2lmKGUpcmV0dXJuITA7dmFyIHIsdSxpPW4uYSxv
PXRbMF1bMF0sYT10WzFdWzBdLGM9dFswXVsxXSxzPXRbMV1bMV0sbD1uLmwsZj1uLnIsaD1sLngs
Zz1sLnkscD1mLngsdj1mLnksZD0oaCtwKS8yLG09KGcrdikvMjtpZih2PT09Zyl7aWYobz5kfHxk
Pj1hKXJldHVybjtpZihoPnApe2lmKGkpe2lmKGkueT49cylyZXR1cm59ZWxzZSBpPXt4OmQseTpj
fTtlPXt4OmQseTpzfX1lbHNle2lmKGkpe2lmKGkueTxjKXJldHVybn1lbHNlIGk9e3g6ZCx5OnN9
O2U9e3g6ZCx5OmN9fX1lbHNlIGlmKHI9KGgtcCkvKHYtZyksdT1tLXIqZCwtMT5yfHxyPjEpaWYo
aD5wKXtpZihpKXtpZihpLnk+PXMpcmV0dXJufWVsc2UgaT17eDooYy11KS9yLHk6Y307ZT17eDoo
cy11KS9yLHk6c319ZWxzZXtpZihpKXtpZihpLnk8YylyZXR1cm59ZWxzZSBpPXt4OihzLXUpL3Is
eTpzfTtlPXt4OihjLXUpL3IseTpjfX1lbHNlIGlmKHY+Zyl7aWYoaSl7aWYoaS54Pj1hKXJldHVy
bn1lbHNlIGk9e3g6byx5OnIqbyt1fTtlPXt4OmEseTpyKmErdX19ZWxzZXtpZihpKXtpZihpLng8
bylyZXR1cm59ZWxzZSBpPXt4OmEseTpyKmErdX07ZT17eDpvLHk6cipvK3V9fXJldHVybiBuLmE9
aSxuLmI9ZSwhMH1mdW5jdGlvbiBKcihuLHQpe3RoaXMubD1uLHRoaXMucj10LHRoaXMuYT10aGlz
LmI9bnVsbH1mdW5jdGlvbiBXcihuLHQsZSxyKXt2YXIgdT1uZXcgSnIobix0KTtyZXR1cm4gSmMu
cHVzaCh1KSxlJiZLcih1LG4sdCxlKSxyJiZLcih1LHQsbixyKSxXY1tuLmldLmVkZ2VzLnB1c2go
bmV3IFFyKHUsbix0KSksV2NbdC5pXS5lZGdlcy5wdXNoKG5ldyBRcih1LHQsbikpLHV9ZnVuY3Rp
b24gR3Iobix0LGUpe3ZhciByPW5ldyBKcihuLG51bGwpO3JldHVybiByLmE9dCxyLmI9ZSxKYy5w
dXNoKHIpLHJ9ZnVuY3Rpb24gS3Iobix0LGUscil7bi5hfHxuLmI/bi5sPT09ZT9uLmI9cjpuLmE9
cjoobi5hPXIsbi5sPXQsbi5yPWUpfWZ1bmN0aW9uIFFyKG4sdCxlKXt2YXIgcj1uLmEsdT1uLmI7
dGhpcy5lZGdlPW4sdGhpcy5zaXRlPXQsdGhpcy5hbmdsZT1lP01hdGguYXRhbjIoZS55LXQueSxl
LngtdC54KTpuLmw9PT10P01hdGguYXRhbjIodS54LXIueCxyLnktdS55KTpNYXRoLmF0YW4yKHIu
eC11LngsdS55LXIueSl9ZnVuY3Rpb24gbnUoKXt0aGlzLl89bnVsbH1mdW5jdGlvbiB0dShuKXtu
LlU9bi5DPW4uTD1uLlI9bi5QPW4uTj1udWxsfWZ1bmN0aW9uIGV1KG4sdCl7dmFyIGU9dCxyPXQu
Uix1PWUuVTt1P3UuTD09PWU/dS5MPXI6dS5SPXI6bi5fPXIsci5VPXUsZS5VPXIsZS5SPXIuTCxl
LlImJihlLlIuVT1lKSxyLkw9ZX1mdW5jdGlvbiBydShuLHQpe3ZhciBlPXQscj10LkwsdT1lLlU7
dT91Lkw9PT1lP3UuTD1yOnUuUj1yOm4uXz1yLHIuVT11LGUuVT1yLGUuTD1yLlIsZS5MJiYoZS5M
LlU9ZSksci5SPWV9ZnVuY3Rpb24gdXUobil7Zm9yKDtuLkw7KW49bi5MO3JldHVybiBufWZ1bmN0
aW9uIGl1KG4sdCl7dmFyIGUscix1LGk9bi5zb3J0KG91KS5wb3AoKTtmb3IoSmM9W10sV2M9bmV3
IEFycmF5KG4ubGVuZ3RoKSxHYz1uZXcgbnUsUWM9bmV3IG51OzspaWYodT1LYyxpJiYoIXV8fGku
eTx1Lnl8fGkueT09PXUueSYmaS54PHUueCkpKGkueCE9PWV8fGkueSE9PXIpJiYoV2NbaS5pXT1u
ZXcgT3IoaSksanIoaSksZT1pLngscj1pLnkpLGk9bi5wb3AoKTtlbHNle2lmKCF1KWJyZWFrO1Vy
KHUuYXJjKX10JiYoWHIodCksSXIodCkpO3ZhciBvPXtjZWxsczpXYyxlZGdlczpKY307cmV0dXJu
IEdjPVFjPUpjPVdjPW51bGwsb31mdW5jdGlvbiBvdShuLHQpe3JldHVybiB0Lnktbi55fHx0Lngt
bi54fWZ1bmN0aW9uIGF1KG4sdCxlKXtyZXR1cm4obi54LWUueCkqKHQueS1uLnkpLShuLngtdC54
KSooZS55LW4ueSl9ZnVuY3Rpb24gY3Uobil7cmV0dXJuIG4ueH1mdW5jdGlvbiBzdShuKXtyZXR1
cm4gbi55fWZ1bmN0aW9uIGx1KCl7cmV0dXJue2xlYWY6ITAsbm9kZXM6W10scG9pbnQ6bnVsbCx4
Om51bGwseTpudWxsfX1mdW5jdGlvbiBmdShuLHQsZSxyLHUsaSl7aWYoIW4odCxlLHIsdSxpKSl7
dmFyIG89LjUqKGUrdSksYT0uNSoocitpKSxjPXQubm9kZXM7Y1swXSYmZnUobixjWzBdLGUscixv
LGEpLGNbMV0mJmZ1KG4sY1sxXSxvLHIsdSxhKSxjWzJdJiZmdShuLGNbMl0sZSxhLG8saSksY1sz
XSYmZnUobixjWzNdLG8sYSx1LGkpfX1mdW5jdGlvbiBodShuLHQpe249R28ucmdiKG4pLHQ9R28u
cmdiKHQpO3ZhciBlPW4ucixyPW4uZyx1PW4uYixpPXQuci1lLG89dC5nLXIsYT10LmItdTtyZXR1
cm4gZnVuY3Rpb24obil7cmV0dXJuIiMiK010KE1hdGgucm91bmQoZStpKm4pKStNdChNYXRoLnJv
dW5kKHIrbypuKSkrTXQoTWF0aC5yb3VuZCh1K2EqbikpfX1mdW5jdGlvbiBndShuLHQpe3ZhciBl
LHI9e30sdT17fTtmb3IoZSBpbiBuKWUgaW4gdD9yW2VdPWR1KG5bZV0sdFtlXSk6dVtlXT1uW2Vd
O2ZvcihlIGluIHQpZSBpbiBufHwodVtlXT10W2VdKTtyZXR1cm4gZnVuY3Rpb24obil7Zm9yKGUg
aW4gcil1W2VdPXJbZV0obik7cmV0dXJuIHV9fWZ1bmN0aW9uIHB1KG4sdCl7cmV0dXJuIHQtPW49
K24sZnVuY3Rpb24oZSl7cmV0dXJuIG4rdCplfX1mdW5jdGlvbiB2dShuLHQpe3ZhciBlLHIsdSxp
PXJzLmxhc3RJbmRleD11cy5sYXN0SW5kZXg9MCxvPS0xLGE9W10sYz1bXTtmb3Iobis9IiIsdCs9
IiI7KGU9cnMuZXhlYyhuKSkmJihyPXVzLmV4ZWModCkpOykodT1yLmluZGV4KT5pJiYodT10LnN1
YnN0cmluZyhpLHUpLGFbb10/YVtvXSs9dTphWysrb109dSksKGU9ZVswXSk9PT0ocj1yWzBdKT9h
W29dP2Fbb10rPXI6YVsrK29dPXI6KGFbKytvXT1udWxsLGMucHVzaCh7aTpvLHg6cHUoZSxyKX0p
KSxpPXVzLmxhc3RJbmRleDtyZXR1cm4gaTx0Lmxlbmd0aCYmKHU9dC5zdWJzdHJpbmcoaSksYVtv
XT9hW29dKz11OmFbKytvXT11KSxhLmxlbmd0aDwyP2NbMF0/KHQ9Y1swXS54LGZ1bmN0aW9uKG4p
e3JldHVybiB0KG4pKyIifSk6ZnVuY3Rpb24oKXtyZXR1cm4gdH06KHQ9Yy5sZW5ndGgsZnVuY3Rp
b24obil7Zm9yKHZhciBlLHI9MDt0PnI7KytyKWFbKGU9Y1tyXSkuaV09ZS54KG4pO3JldHVybiBh
LmpvaW4oIiIpfSl9ZnVuY3Rpb24gZHUobix0KXtmb3IodmFyIGUscj1Hby5pbnRlcnBvbGF0b3Jz
Lmxlbmd0aDstLXI+PTAmJiEoZT1Hby5pbnRlcnBvbGF0b3JzW3JdKG4sdCkpOyk7cmV0dXJuIGV9
ZnVuY3Rpb24gbXUobix0KXt2YXIgZSxyPVtdLHU9W10saT1uLmxlbmd0aCxvPXQubGVuZ3RoLGE9
TWF0aC5taW4obi5sZW5ndGgsdC5sZW5ndGgpO2ZvcihlPTA7YT5lOysrZSlyLnB1c2goZHUobltl
XSx0W2VdKSk7Zm9yKDtpPmU7KytlKXVbZV09bltlXTtmb3IoO28+ZTsrK2UpdVtlXT10W2VdO3Jl
dHVybiBmdW5jdGlvbihuKXtmb3IoZT0wO2E+ZTsrK2UpdVtlXT1yW2VdKG4pO3JldHVybiB1fX1m
dW5jdGlvbiB5dShuKXtyZXR1cm4gZnVuY3Rpb24odCl7cmV0dXJuIDA+PXQ/MDp0Pj0xPzE6bih0
KX19ZnVuY3Rpb24geHUobil7cmV0dXJuIGZ1bmN0aW9uKHQpe3JldHVybiAxLW4oMS10KX19ZnVu
Y3Rpb24gTXUobil7cmV0dXJuIGZ1bmN0aW9uKHQpe3JldHVybi41KiguNT50P24oMip0KToyLW4o
Mi0yKnQpKX19ZnVuY3Rpb24gX3Uobil7cmV0dXJuIG4qbn1mdW5jdGlvbiBidShuKXtyZXR1cm4g
bipuKm59ZnVuY3Rpb24gd3Uobil7aWYoMD49bilyZXR1cm4gMDtpZihuPj0xKXJldHVybiAxO3Zh
ciB0PW4qbixlPXQqbjtyZXR1cm4gNCooLjU+bj9lOjMqKG4tdCkrZS0uNzUpfWZ1bmN0aW9uIFN1
KG4pe3JldHVybiBmdW5jdGlvbih0KXtyZXR1cm4gTWF0aC5wb3codCxuKX19ZnVuY3Rpb24ga3Uo
bil7cmV0dXJuIDEtTWF0aC5jb3MobipMYSl9ZnVuY3Rpb24gRXUobil7cmV0dXJuIE1hdGgucG93
KDIsMTAqKG4tMSkpfWZ1bmN0aW9uIEF1KG4pe3JldHVybiAxLU1hdGguc3FydCgxLW4qbil9ZnVu
Y3Rpb24gQ3Uobix0KXt2YXIgZTtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aDwyJiYodD0uNDUpLGFy
Z3VtZW50cy5sZW5ndGg/ZT10L05hKk1hdGguYXNpbigxL24pOihuPTEsZT10LzQpLGZ1bmN0aW9u
KHIpe3JldHVybiAxK24qTWF0aC5wb3coMiwtMTAqcikqTWF0aC5zaW4oKHItZSkqTmEvdCl9fWZ1
bmN0aW9uIE51KG4pe3JldHVybiBufHwobj0xLjcwMTU4KSxmdW5jdGlvbih0KXtyZXR1cm4gdCp0
KigobisxKSp0LW4pfX1mdW5jdGlvbiBMdShuKXtyZXR1cm4gMS8yLjc1Pm4/Ny41NjI1Km4qbjoy
LzIuNzU+bj83LjU2MjUqKG4tPTEuNS8yLjc1KSpuKy43NToyLjUvMi43NT5uPzcuNTYyNSoobi09
Mi4yNS8yLjc1KSpuKy45Mzc1OjcuNTYyNSoobi09Mi42MjUvMi43NSkqbisuOTg0Mzc1fWZ1bmN0
aW9uIFR1KG4sdCl7bj1Hby5oY2wobiksdD1Hby5oY2wodCk7dmFyIGU9bi5oLHI9bi5jLHU9bi5s
LGk9dC5oLWUsbz10LmMtcixhPXQubC11O3JldHVybiBpc05hTihvKSYmKG89MCxyPWlzTmFOKHIp
P3QuYzpyKSxpc05hTihpKT8oaT0wLGU9aXNOYU4oZSk/dC5oOmUpOmk+MTgwP2ktPTM2MDotMTgw
PmkmJihpKz0zNjApLGZ1bmN0aW9uKG4pe3JldHVybiBjdChlK2kqbixyK28qbix1K2EqbikrIiJ9
fWZ1bmN0aW9uIHF1KG4sdCl7bj1Hby5oc2wobiksdD1Hby5oc2wodCk7dmFyIGU9bi5oLHI9bi5z
LHU9bi5sLGk9dC5oLWUsbz10LnMtcixhPXQubC11O3JldHVybiBpc05hTihvKSYmKG89MCxyPWlz
TmFOKHIpP3QuczpyKSxpc05hTihpKT8oaT0wLGU9aXNOYU4oZSk/dC5oOmUpOmk+MTgwP2ktPTM2
MDotMTgwPmkmJihpKz0zNjApLGZ1bmN0aW9uKG4pe3JldHVybiBpdChlK2kqbixyK28qbix1K2Eq
bikrIiJ9fWZ1bmN0aW9uIHp1KG4sdCl7bj1Hby5sYWIobiksdD1Hby5sYWIodCk7dmFyIGU9bi5s
LHI9bi5hLHU9bi5iLGk9dC5sLWUsbz10LmEtcixhPXQuYi11O3JldHVybiBmdW5jdGlvbihuKXty
ZXR1cm4gZnQoZStpKm4scitvKm4sdSthKm4pKyIifX1mdW5jdGlvbiBSdShuLHQpe3JldHVybiB0
LT1uLGZ1bmN0aW9uKGUpe3JldHVybiBNYXRoLnJvdW5kKG4rdCplKX19ZnVuY3Rpb24gRHUobil7
dmFyIHQ9W24uYSxuLmJdLGU9W24uYyxuLmRdLHI9VXUodCksdT1QdSh0LGUpLGk9VXUoanUoZSx0
LC11KSl8fDA7dFswXSplWzFdPGVbMF0qdFsxXSYmKHRbMF0qPS0xLHRbMV0qPS0xLHIqPS0xLHUq
PS0xKSx0aGlzLnJvdGF0ZT0ocj9NYXRoLmF0YW4yKHRbMV0sdFswXSk6TWF0aC5hdGFuMigtZVsw
XSxlWzFdKSkqUmEsdGhpcy50cmFuc2xhdGU9W24uZSxuLmZdLHRoaXMuc2NhbGU9W3IsaV0sdGhp
cy5za2V3PWk/TWF0aC5hdGFuMih1LGkpKlJhOjB9ZnVuY3Rpb24gUHUobix0KXtyZXR1cm4gblsw
XSp0WzBdK25bMV0qdFsxXX1mdW5jdGlvbiBVdShuKXt2YXIgdD1NYXRoLnNxcnQoUHUobixuKSk7
cmV0dXJuIHQmJihuWzBdLz10LG5bMV0vPXQpLHR9ZnVuY3Rpb24ganUobix0LGUpe3JldHVybiBu
WzBdKz1lKnRbMF0sblsxXSs9ZSp0WzFdLG59ZnVuY3Rpb24gSHUobix0KXt2YXIgZSxyPVtdLHU9
W10saT1Hby50cmFuc2Zvcm0obiksbz1Hby50cmFuc2Zvcm0odCksYT1pLnRyYW5zbGF0ZSxjPW8u
dHJhbnNsYXRlLHM9aS5yb3RhdGUsbD1vLnJvdGF0ZSxmPWkuc2tldyxoPW8uc2tldyxnPWkuc2Nh
bGUscD1vLnNjYWxlO3JldHVybiBhWzBdIT1jWzBdfHxhWzFdIT1jWzFdPyhyLnB1c2goInRyYW5z
bGF0ZSgiLG51bGwsIiwiLG51bGwsIikiKSx1LnB1c2goe2k6MSx4OnB1KGFbMF0sY1swXSl9LHtp
OjMseDpwdShhWzFdLGNbMV0pfSkpOmNbMF18fGNbMV0/ci5wdXNoKCJ0cmFuc2xhdGUoIitjKyIp
Iik6ci5wdXNoKCIiKSxzIT1sPyhzLWw+MTgwP2wrPTM2MDpsLXM+MTgwJiYocys9MzYwKSx1LnB1
c2goe2k6ci5wdXNoKHIucG9wKCkrInJvdGF0ZSgiLG51bGwsIikiKS0yLHg6cHUocyxsKX0pKTps
JiZyLnB1c2goci5wb3AoKSsicm90YXRlKCIrbCsiKSIpLGYhPWg/dS5wdXNoKHtpOnIucHVzaChy
LnBvcCgpKyJza2V3WCgiLG51bGwsIikiKS0yLHg6cHUoZixoKX0pOmgmJnIucHVzaChyLnBvcCgp
KyJza2V3WCgiK2grIikiKSxnWzBdIT1wWzBdfHxnWzFdIT1wWzFdPyhlPXIucHVzaChyLnBvcCgp
KyJzY2FsZSgiLG51bGwsIiwiLG51bGwsIikiKSx1LnB1c2goe2k6ZS00LHg6cHUoZ1swXSxwWzBd
KX0se2k6ZS0yLHg6cHUoZ1sxXSxwWzFdKX0pKTooMSE9cFswXXx8MSE9cFsxXSkmJnIucHVzaChy
LnBvcCgpKyJzY2FsZSgiK3ArIikiKSxlPXUubGVuZ3RoLGZ1bmN0aW9uKG4pe2Zvcih2YXIgdCxp
PS0xOysraTxlOylyWyh0PXVbaV0pLmldPXQueChuKTtyZXR1cm4gci5qb2luKCIiKX19ZnVuY3Rp
b24gRnUobix0KXtyZXR1cm4gdD10LShuPStuKT8xLyh0LW4pOjAsZnVuY3Rpb24oZSl7cmV0dXJu
KGUtbikqdH19ZnVuY3Rpb24gT3Uobix0KXtyZXR1cm4gdD10LShuPStuKT8xLyh0LW4pOjAsZnVu
Y3Rpb24oZSl7cmV0dXJuIE1hdGgubWF4KDAsTWF0aC5taW4oMSwoZS1uKSp0KSl9fWZ1bmN0aW9u
IEl1KG4pe2Zvcih2YXIgdD1uLnNvdXJjZSxlPW4udGFyZ2V0LHI9WnUodCxlKSx1PVt0XTt0IT09
cjspdD10LnBhcmVudCx1LnB1c2godCk7Zm9yKHZhciBpPXUubGVuZ3RoO2UhPT1yOyl1LnNwbGlj
ZShpLDAsZSksZT1lLnBhcmVudDtyZXR1cm4gdX1mdW5jdGlvbiBZdShuKXtmb3IodmFyIHQ9W10s
ZT1uLnBhcmVudDtudWxsIT1lOyl0LnB1c2gobiksbj1lLGU9ZS5wYXJlbnQ7cmV0dXJuIHQucHVz
aChuKSx0fWZ1bmN0aW9uIFp1KG4sdCl7aWYobj09PXQpcmV0dXJuIG47Zm9yKHZhciBlPVl1KG4p
LHI9WXUodCksdT1lLnBvcCgpLGk9ci5wb3AoKSxvPW51bGw7dT09PWk7KW89dSx1PWUucG9wKCks
aT1yLnBvcCgpO3JldHVybiBvfWZ1bmN0aW9uIFZ1KG4pe24uZml4ZWR8PTJ9ZnVuY3Rpb24gJHUo
bil7bi5maXhlZCY9LTd9ZnVuY3Rpb24gWHUobil7bi5maXhlZHw9NCxuLnB4PW4ueCxuLnB5PW4u
eX1mdW5jdGlvbiBCdShuKXtuLmZpeGVkJj0tNX1mdW5jdGlvbiBKdShuLHQsZSl7dmFyIHI9MCx1
PTA7aWYobi5jaGFyZ2U9MCwhbi5sZWFmKWZvcih2YXIgaSxvPW4ubm9kZXMsYT1vLmxlbmd0aCxj
PS0xOysrYzxhOylpPW9bY10sbnVsbCE9aSYmKEp1KGksdCxlKSxuLmNoYXJnZSs9aS5jaGFyZ2Us
cis9aS5jaGFyZ2UqaS5jeCx1Kz1pLmNoYXJnZSppLmN5KTtpZihuLnBvaW50KXtuLmxlYWZ8fChu
LnBvaW50LngrPU1hdGgucmFuZG9tKCktLjUsbi5wb2ludC55Kz1NYXRoLnJhbmRvbSgpLS41KTt2
YXIgcz10KmVbbi5wb2ludC5pbmRleF07bi5jaGFyZ2UrPW4ucG9pbnRDaGFyZ2U9cyxyKz1zKm4u
cG9pbnQueCx1Kz1zKm4ucG9pbnQueX1uLmN4PXIvbi5jaGFyZ2Usbi5jeT11L24uY2hhcmdlfWZ1
bmN0aW9uIFd1KG4sdCl7cmV0dXJuIEdvLnJlYmluZChuLHQsInNvcnQiLCJjaGlsZHJlbiIsInZh
bHVlIiksbi5ub2Rlcz1uLG4ubGlua3M9bmksbn1mdW5jdGlvbiBHdShuKXtyZXR1cm4gbi5jaGls
ZHJlbn1mdW5jdGlvbiBLdShuKXtyZXR1cm4gbi52YWx1ZX1mdW5jdGlvbiBRdShuLHQpe3JldHVy
biB0LnZhbHVlLW4udmFsdWV9ZnVuY3Rpb24gbmkobil7cmV0dXJuIEdvLm1lcmdlKG4ubWFwKGZ1
bmN0aW9uKG4pe3JldHVybihuLmNoaWxkcmVufHxbXSkubWFwKGZ1bmN0aW9uKHQpe3JldHVybntz
b3VyY2U6bix0YXJnZXQ6dH19KX0pKX1mdW5jdGlvbiB0aShuKXtyZXR1cm4gbi54fWZ1bmN0aW9u
IGVpKG4pe3JldHVybiBuLnl9ZnVuY3Rpb24gcmkobix0LGUpe24ueTA9dCxuLnk9ZX1mdW5jdGlv
biB1aShuKXtyZXR1cm4gR28ucmFuZ2Uobi5sZW5ndGgpfWZ1bmN0aW9uIGlpKG4pe2Zvcih2YXIg
dD0tMSxlPW5bMF0ubGVuZ3RoLHI9W107Kyt0PGU7KXJbdF09MDtyZXR1cm4gcn1mdW5jdGlvbiBv
aShuKXtmb3IodmFyIHQsZT0xLHI9MCx1PW5bMF1bMV0saT1uLmxlbmd0aDtpPmU7KytlKSh0PW5b
ZV1bMV0pPnUmJihyPWUsdT10KTtyZXR1cm4gcn1mdW5jdGlvbiBhaShuKXtyZXR1cm4gbi5yZWR1
Y2UoY2ksMCl9ZnVuY3Rpb24gY2kobix0KXtyZXR1cm4gbit0WzFdfWZ1bmN0aW9uIHNpKG4sdCl7
cmV0dXJuIGxpKG4sTWF0aC5jZWlsKE1hdGgubG9nKHQubGVuZ3RoKS9NYXRoLkxOMisxKSl9ZnVu
Y3Rpb24gbGkobix0KXtmb3IodmFyIGU9LTEscj0rblswXSx1PShuWzFdLXIpL3QsaT1bXTsrK2U8
PXQ7KWlbZV09dSplK3I7cmV0dXJuIGl9ZnVuY3Rpb24gZmkobil7cmV0dXJuW0dvLm1pbihuKSxH
by5tYXgobildfWZ1bmN0aW9uIGhpKG4sdCl7cmV0dXJuIG4ucGFyZW50PT10LnBhcmVudD8xOjJ9
ZnVuY3Rpb24gZ2kobil7dmFyIHQ9bi5jaGlsZHJlbjtyZXR1cm4gdCYmdC5sZW5ndGg/dFswXTpu
Ll90cmVlLnRocmVhZH1mdW5jdGlvbiBwaShuKXt2YXIgdCxlPW4uY2hpbGRyZW47cmV0dXJuIGUm
Jih0PWUubGVuZ3RoKT9lW3QtMV06bi5fdHJlZS50aHJlYWR9ZnVuY3Rpb24gdmkobix0KXt2YXIg
ZT1uLmNoaWxkcmVuO2lmKGUmJih1PWUubGVuZ3RoKSlmb3IodmFyIHIsdSxpPS0xOysraTx1Oyl0
KHI9dmkoZVtpXSx0KSxuKT4wJiYobj1yKTtyZXR1cm4gbn1mdW5jdGlvbiBkaShuLHQpe3JldHVy
biBuLngtdC54fWZ1bmN0aW9uIG1pKG4sdCl7cmV0dXJuIHQueC1uLnh9ZnVuY3Rpb24geWkobix0
KXtyZXR1cm4gbi5kZXB0aC10LmRlcHRofWZ1bmN0aW9uIHhpKG4sdCl7ZnVuY3Rpb24gZShuLHIp
e3ZhciB1PW4uY2hpbGRyZW47aWYodSYmKG89dS5sZW5ndGgpKWZvcih2YXIgaSxvLGE9bnVsbCxj
PS0xOysrYzxvOylpPXVbY10sZShpLGEpLGE9aTt0KG4scil9ZShuLG51bGwpfWZ1bmN0aW9uIE1p
KG4pe2Zvcih2YXIgdCxlPTAscj0wLHU9bi5jaGlsZHJlbixpPXUubGVuZ3RoOy0taT49MDspdD11
W2ldLl90cmVlLHQucHJlbGltKz1lLHQubW9kKz1lLGUrPXQuc2hpZnQrKHIrPXQuY2hhbmdlKX1m
dW5jdGlvbiBfaShuLHQsZSl7bj1uLl90cmVlLHQ9dC5fdHJlZTt2YXIgcj1lLyh0Lm51bWJlci1u
Lm51bWJlcik7bi5jaGFuZ2UrPXIsdC5jaGFuZ2UtPXIsdC5zaGlmdCs9ZSx0LnByZWxpbSs9ZSx0
Lm1vZCs9ZX1mdW5jdGlvbiBiaShuLHQsZSl7cmV0dXJuIG4uX3RyZWUuYW5jZXN0b3IucGFyZW50
PT10LnBhcmVudD9uLl90cmVlLmFuY2VzdG9yOmV9ZnVuY3Rpb24gd2kobix0KXtyZXR1cm4gbi52
YWx1ZS10LnZhbHVlfWZ1bmN0aW9uIFNpKG4sdCl7dmFyIGU9bi5fcGFja19uZXh0O24uX3BhY2tf
bmV4dD10LHQuX3BhY2tfcHJldj1uLHQuX3BhY2tfbmV4dD1lLGUuX3BhY2tfcHJldj10fWZ1bmN0
aW9uIGtpKG4sdCl7bi5fcGFja19uZXh0PXQsdC5fcGFja19wcmV2PW59ZnVuY3Rpb24gRWkobix0
KXt2YXIgZT10Lngtbi54LHI9dC55LW4ueSx1PW4ucit0LnI7cmV0dXJuLjk5OSp1KnU+ZSplK3Iq
cn1mdW5jdGlvbiBBaShuKXtmdW5jdGlvbiB0KG4pe2w9TWF0aC5taW4obi54LW4ucixsKSxmPU1h
dGgubWF4KG4ueCtuLnIsZiksaD1NYXRoLm1pbihuLnktbi5yLGgpLGc9TWF0aC5tYXgobi55K24u
cixnKX1pZigoZT1uLmNoaWxkcmVuKSYmKHM9ZS5sZW5ndGgpKXt2YXIgZSxyLHUsaSxvLGEsYyxz
LGw9MS8wLGY9LTEvMCxoPTEvMCxnPS0xLzA7aWYoZS5mb3JFYWNoKENpKSxyPWVbMF0sci54PS1y
LnIsci55PTAsdChyKSxzPjEmJih1PWVbMV0sdS54PXUucix1Lnk9MCx0KHUpLHM+MikpZm9yKGk9
ZVsyXSxUaShyLHUsaSksdChpKSxTaShyLGkpLHIuX3BhY2tfcHJldj1pLFNpKGksdSksdT1yLl9w
YWNrX25leHQsbz0zO3M+bztvKyspe1RpKHIsdSxpPWVbb10pO3ZhciBwPTAsdj0xLGQ9MTtmb3Io
YT11Ll9wYWNrX25leHQ7YSE9PXU7YT1hLl9wYWNrX25leHQsdisrKWlmKEVpKGEsaSkpe3A9MTti
cmVha31pZigxPT1wKWZvcihjPXIuX3BhY2tfcHJldjtjIT09YS5fcGFja19wcmV2JiYhRWkoYyxp
KTtjPWMuX3BhY2tfcHJldixkKyspO3A/KGQ+dnx8dj09ZCYmdS5yPHIucj9raShyLHU9YSk6a2ko
cj1jLHUpLG8tLSk6KFNpKHIsaSksdT1pLHQoaSkpfXZhciBtPShsK2YpLzIseT0oaCtnKS8yLHg9
MDtmb3Iobz0wO3M+bztvKyspaT1lW29dLGkueC09bSxpLnktPXkseD1NYXRoLm1heCh4LGkucitN
YXRoLnNxcnQoaS54KmkueCtpLnkqaS55KSk7bi5yPXgsZS5mb3JFYWNoKE5pKX19ZnVuY3Rpb24g
Q2kobil7bi5fcGFja19uZXh0PW4uX3BhY2tfcHJldj1ufWZ1bmN0aW9uIE5pKG4pe2RlbGV0ZSBu
Ll9wYWNrX25leHQsZGVsZXRlIG4uX3BhY2tfcHJldn1mdW5jdGlvbiBMaShuLHQsZSxyKXt2YXIg
dT1uLmNoaWxkcmVuO2lmKG4ueD10Kz1yKm4ueCxuLnk9ZSs9cipuLnksbi5yKj1yLHUpZm9yKHZh
ciBpPS0xLG89dS5sZW5ndGg7KytpPG87KUxpKHVbaV0sdCxlLHIpfWZ1bmN0aW9uIFRpKG4sdCxl
KXt2YXIgcj1uLnIrZS5yLHU9dC54LW4ueCxpPXQueS1uLnk7aWYociYmKHV8fGkpKXt2YXIgbz10
LnIrZS5yLGE9dSp1K2kqaTtvKj1vLHIqPXI7dmFyIGM9LjUrKHItbykvKDIqYSkscz1NYXRoLnNx
cnQoTWF0aC5tYXgoMCwyKm8qKHIrYSktKHItPWEpKnItbypvKSkvKDIqYSk7ZS54PW4ueCtjKnUr
cyppLGUueT1uLnkrYyppLXMqdX1lbHNlIGUueD1uLngrcixlLnk9bi55fWZ1bmN0aW9uIHFpKG4p
e3JldHVybiAxK0dvLm1heChuLGZ1bmN0aW9uKG4pe3JldHVybiBuLnl9KX1mdW5jdGlvbiB6aShu
KXtyZXR1cm4gbi5yZWR1Y2UoZnVuY3Rpb24obix0KXtyZXR1cm4gbit0Lnh9LDApL24ubGVuZ3Ro
fWZ1bmN0aW9uIFJpKG4pe3ZhciB0PW4uY2hpbGRyZW47cmV0dXJuIHQmJnQubGVuZ3RoP1JpKHRb
MF0pOm59ZnVuY3Rpb24gRGkobil7dmFyIHQsZT1uLmNoaWxkcmVuO3JldHVybiBlJiYodD1lLmxl
bmd0aCk/RGkoZVt0LTFdKTpufWZ1bmN0aW9uIFBpKG4pe3JldHVybnt4Om4ueCx5Om4ueSxkeDpu
LmR4LGR5Om4uZHl9fWZ1bmN0aW9uIFVpKG4sdCl7dmFyIGU9bi54K3RbM10scj1uLnkrdFswXSx1
PW4uZHgtdFsxXS10WzNdLGk9bi5keS10WzBdLXRbMl07cmV0dXJuIDA+dSYmKGUrPXUvMix1PTAp
LDA+aSYmKHIrPWkvMixpPTApLHt4OmUseTpyLGR4OnUsZHk6aX19ZnVuY3Rpb24gamkobil7dmFy
IHQ9blswXSxlPW5bbi5sZW5ndGgtMV07cmV0dXJuIGU+dD9bdCxlXTpbZSx0XX1mdW5jdGlvbiBI
aShuKXtyZXR1cm4gbi5yYW5nZUV4dGVudD9uLnJhbmdlRXh0ZW50KCk6amkobi5yYW5nZSgpKX1m
dW5jdGlvbiBGaShuLHQsZSxyKXt2YXIgdT1lKG5bMF0sblsxXSksaT1yKHRbMF0sdFsxXSk7cmV0
dXJuIGZ1bmN0aW9uKG4pe3JldHVybiBpKHUobikpfX1mdW5jdGlvbiBPaShuLHQpe3ZhciBlLHI9
MCx1PW4ubGVuZ3RoLTEsaT1uW3JdLG89blt1XTtyZXR1cm4gaT5vJiYoZT1yLHI9dSx1PWUsZT1p
LGk9byxvPWUpLG5bcl09dC5mbG9vcihpKSxuW3VdPXQuY2VpbChvKSxufWZ1bmN0aW9uIElpKG4p
e3JldHVybiBuP3tmbG9vcjpmdW5jdGlvbih0KXtyZXR1cm4gTWF0aC5mbG9vcih0L24pKm59LGNl
aWw6ZnVuY3Rpb24odCl7cmV0dXJuIE1hdGguY2VpbCh0L24pKm59fTp2c31mdW5jdGlvbiBZaShu
LHQsZSxyKXt2YXIgdT1bXSxpPVtdLG89MCxhPU1hdGgubWluKG4ubGVuZ3RoLHQubGVuZ3RoKS0x
O2ZvcihuW2FdPG5bMF0mJihuPW4uc2xpY2UoKS5yZXZlcnNlKCksdD10LnNsaWNlKCkucmV2ZXJz
ZSgpKTsrK288PWE7KXUucHVzaChlKG5bby0xXSxuW29dKSksaS5wdXNoKHIodFtvLTFdLHRbb10p
KTtyZXR1cm4gZnVuY3Rpb24odCl7dmFyIGU9R28uYmlzZWN0KG4sdCwxLGEpLTE7cmV0dXJuIGlb
ZV0odVtlXSh0KSl9fWZ1bmN0aW9uIFppKG4sdCxlLHIpe2Z1bmN0aW9uIHUoKXt2YXIgdT1NYXRo
Lm1pbihuLmxlbmd0aCx0Lmxlbmd0aCk+Mj9ZaTpGaSxjPXI/T3U6RnU7cmV0dXJuIG89dShuLHQs
YyxlKSxhPXUodCxuLGMsZHUpLGl9ZnVuY3Rpb24gaShuKXtyZXR1cm4gbyhuKX12YXIgbyxhO3Jl
dHVybiBpLmludmVydD1mdW5jdGlvbihuKXtyZXR1cm4gYShuKX0saS5kb21haW49ZnVuY3Rpb24o
dCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KG49dC5tYXAoTnVtYmVyKSx1KCkpOm59LGkucmFu
Z2U9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHQ9bix1KCkpOnR9LGkucmFu
Z2VSb3VuZD1mdW5jdGlvbihuKXtyZXR1cm4gaS5yYW5nZShuKS5pbnRlcnBvbGF0ZShSdSl9LGku
Y2xhbXA9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHI9bix1KCkpOnJ9LGku
aW50ZXJwb2xhdGU9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGU9bix1KCkp
OmV9LGkudGlja3M9ZnVuY3Rpb24odCl7cmV0dXJuIEJpKG4sdCl9LGkudGlja0Zvcm1hdD1mdW5j
dGlvbih0LGUpe3JldHVybiBKaShuLHQsZSl9LGkubmljZT1mdW5jdGlvbih0KXtyZXR1cm4gJGko
bix0KSx1KCl9LGkuY29weT1mdW5jdGlvbigpe3JldHVybiBaaShuLHQsZSxyKX0sdSgpfWZ1bmN0
aW9uIFZpKG4sdCl7cmV0dXJuIEdvLnJlYmluZChuLHQsInJhbmdlIiwicmFuZ2VSb3VuZCIsImlu
dGVycG9sYXRlIiwiY2xhbXAiKX1mdW5jdGlvbiAkaShuLHQpe3JldHVybiBPaShuLElpKFhpKG4s
dClbMl0pKX1mdW5jdGlvbiBYaShuLHQpe251bGw9PXQmJih0PTEwKTt2YXIgZT1qaShuKSxyPWVb
MV0tZVswXSx1PU1hdGgucG93KDEwLE1hdGguZmxvb3IoTWF0aC5sb2coci90KS9NYXRoLkxOMTAp
KSxpPXQvcip1O3JldHVybi4xNT49aT91Kj0xMDouMzU+PWk/dSo9NTouNzU+PWkmJih1Kj0yKSxl
WzBdPU1hdGguY2VpbChlWzBdL3UpKnUsZVsxXT1NYXRoLmZsb29yKGVbMV0vdSkqdSsuNSp1LGVb
Ml09dSxlfWZ1bmN0aW9uIEJpKG4sdCl7cmV0dXJuIEdvLnJhbmdlLmFwcGx5KEdvLFhpKG4sdCkp
fWZ1bmN0aW9uIEppKG4sdCxlKXt2YXIgcj1YaShuLHQpO2lmKGUpe3ZhciB1PXJjLmV4ZWMoZSk7
aWYodS5zaGlmdCgpLCJzIj09PXVbOF0pe3ZhciBpPUdvLmZvcm1hdFByZWZpeChNYXRoLm1heChm
YShyWzBdKSxmYShyWzFdKSkpO3JldHVybiB1WzddfHwodVs3XT0iLiIrV2koaS5zY2FsZShyWzJd
KSkpLHVbOF09ImYiLGU9R28uZm9ybWF0KHUuam9pbigiIikpLGZ1bmN0aW9uKG4pe3JldHVybiBl
KGkuc2NhbGUobikpK2kuc3ltYm9sfX11WzddfHwodVs3XT0iLiIrR2kodVs4XSxyKSksZT11Lmpv
aW4oIiIpfWVsc2UgZT0iLC4iK1dpKHJbMl0pKyJmIjtyZXR1cm4gR28uZm9ybWF0KGUpfWZ1bmN0
aW9uIFdpKG4pe3JldHVybi1NYXRoLmZsb29yKE1hdGgubG9nKG4pL01hdGguTE4xMCsuMDEpfWZ1
bmN0aW9uIEdpKG4sdCl7dmFyIGU9V2kodFsyXSk7cmV0dXJuIG4gaW4gZHM/TWF0aC5hYnMoZS1X
aShNYXRoLm1heChmYSh0WzBdKSxmYSh0WzFdKSkpKSsgKygiZSIhPT1uKTplLTIqKCIlIj09PW4p
fWZ1bmN0aW9uIEtpKG4sdCxlLHIpe2Z1bmN0aW9uIHUobil7cmV0dXJuKGU/TWF0aC5sb2coMD5u
PzA6bik6LU1hdGgubG9nKG4+MD8wOi1uKSkvTWF0aC5sb2codCl9ZnVuY3Rpb24gaShuKXtyZXR1
cm4gZT9NYXRoLnBvdyh0LG4pOi1NYXRoLnBvdyh0LC1uKX1mdW5jdGlvbiBvKHQpe3JldHVybiBu
KHUodCkpfXJldHVybiBvLmludmVydD1mdW5jdGlvbih0KXtyZXR1cm4gaShuLmludmVydCh0KSl9
LG8uZG9tYWluPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhlPXRbMF0+PTAs
bi5kb21haW4oKHI9dC5tYXAoTnVtYmVyKSkubWFwKHUpKSxvKTpyfSxvLmJhc2U9ZnVuY3Rpb24o
ZSl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHQ9K2Usbi5kb21haW4oci5tYXAodSkpLG8pOnR9
LG8ubmljZT1mdW5jdGlvbigpe3ZhciB0PU9pKHIubWFwKHUpLGU/TWF0aDp5cyk7cmV0dXJuIG4u
ZG9tYWluKHQpLHI9dC5tYXAoaSksb30sby50aWNrcz1mdW5jdGlvbigpe3ZhciBuPWppKHIpLG89
W10sYT1uWzBdLGM9blsxXSxzPU1hdGguZmxvb3IodShhKSksbD1NYXRoLmNlaWwodShjKSksZj10
JTE/Mjp0O2lmKGlzRmluaXRlKGwtcykpe2lmKGUpe2Zvcig7bD5zO3MrKylmb3IodmFyIGg9MTtm
Pmg7aCsrKW8ucHVzaChpKHMpKmgpO28ucHVzaChpKHMpKX1lbHNlIGZvcihvLnB1c2goaShzKSk7
cysrPGw7KWZvcih2YXIgaD1mLTE7aD4wO2gtLSlvLnB1c2goaShzKSpoKTtmb3Iocz0wO29bc108
YTtzKyspO2ZvcihsPW8ubGVuZ3RoO29bbC0xXT5jO2wtLSk7bz1vLnNsaWNlKHMsbCl9cmV0dXJu
IG99LG8udGlja0Zvcm1hdD1mdW5jdGlvbihuLHQpe2lmKCFhcmd1bWVudHMubGVuZ3RoKXJldHVy
biBtczthcmd1bWVudHMubGVuZ3RoPDI/dD1tczoiZnVuY3Rpb24iIT10eXBlb2YgdCYmKHQ9R28u
Zm9ybWF0KHQpKTt2YXIgcixhPU1hdGgubWF4KC4xLG4vby50aWNrcygpLmxlbmd0aCksYz1lPyhy
PTFlLTEyLE1hdGguY2VpbCk6KHI9LTFlLTEyLE1hdGguZmxvb3IpO3JldHVybiBmdW5jdGlvbihu
KXtyZXR1cm4gbi9pKGModShuKStyKSk8PWE/dChuKToiIn19LG8uY29weT1mdW5jdGlvbigpe3Jl
dHVybiBLaShuLmNvcHkoKSx0LGUscil9LFZpKG8sbil9ZnVuY3Rpb24gUWkobix0LGUpe2Z1bmN0
aW9uIHIodCl7cmV0dXJuIG4odSh0KSl9dmFyIHU9bm8odCksaT1ubygxL3QpO3JldHVybiByLmlu
dmVydD1mdW5jdGlvbih0KXtyZXR1cm4gaShuLmludmVydCh0KSl9LHIuZG9tYWluPWZ1bmN0aW9u
KHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhuLmRvbWFpbigoZT10Lm1hcChOdW1iZXIpKS5t
YXAodSkpLHIpOmV9LHIudGlja3M9ZnVuY3Rpb24obil7cmV0dXJuIEJpKGUsbil9LHIudGlja0Zv
cm1hdD1mdW5jdGlvbihuLHQpe3JldHVybiBKaShlLG4sdCl9LHIubmljZT1mdW5jdGlvbihuKXty
ZXR1cm4gci5kb21haW4oJGkoZSxuKSl9LHIuZXhwb25lbnQ9ZnVuY3Rpb24obyl7cmV0dXJuIGFy
Z3VtZW50cy5sZW5ndGg/KHU9bm8odD1vKSxpPW5vKDEvdCksbi5kb21haW4oZS5tYXAodSkpLHIp
OnR9LHIuY29weT1mdW5jdGlvbigpe3JldHVybiBRaShuLmNvcHkoKSx0LGUpfSxWaShyLG4pfWZ1
bmN0aW9uIG5vKG4pe3JldHVybiBmdW5jdGlvbih0KXtyZXR1cm4gMD50Py1NYXRoLnBvdygtdCxu
KTpNYXRoLnBvdyh0LG4pfX1mdW5jdGlvbiB0byhuLHQpe2Z1bmN0aW9uIGUoZSl7cmV0dXJuIGlb
KCh1LmdldChlKXx8KCJyYW5nZSI9PT10LnQ/dS5zZXQoZSxuLnB1c2goZSkpOjAvMCkpLTEpJWku
bGVuZ3RoXX1mdW5jdGlvbiByKHQsZSl7cmV0dXJuIEdvLnJhbmdlKG4ubGVuZ3RoKS5tYXAoZnVu
Y3Rpb24obil7cmV0dXJuIHQrZSpufSl9dmFyIHUsaSxhO3JldHVybiBlLmRvbWFpbj1mdW5jdGlv
bihyKXtpZighYXJndW1lbnRzLmxlbmd0aClyZXR1cm4gbjtuPVtdLHU9bmV3IG87Zm9yKHZhciBp
LGE9LTEsYz1yLmxlbmd0aDsrK2E8YzspdS5oYXMoaT1yW2FdKXx8dS5zZXQoaSxuLnB1c2goaSkp
O3JldHVybiBlW3QudF0uYXBwbHkoZSx0LmEpfSxlLnJhbmdlPWZ1bmN0aW9uKG4pe3JldHVybiBh
cmd1bWVudHMubGVuZ3RoPyhpPW4sYT0wLHQ9e3Q6InJhbmdlIixhOmFyZ3VtZW50c30sZSk6aX0s
ZS5yYW5nZVBvaW50cz1mdW5jdGlvbih1LG8pe2FyZ3VtZW50cy5sZW5ndGg8MiYmKG89MCk7dmFy
IGM9dVswXSxzPXVbMV0sbD0ocy1jKS8oTWF0aC5tYXgoMSxuLmxlbmd0aC0xKStvKTtyZXR1cm4g
aT1yKG4ubGVuZ3RoPDI/KGMrcykvMjpjK2wqby8yLGwpLGE9MCx0PXt0OiJyYW5nZVBvaW50cyIs
YTphcmd1bWVudHN9LGV9LGUucmFuZ2VCYW5kcz1mdW5jdGlvbih1LG8sYyl7YXJndW1lbnRzLmxl
bmd0aDwyJiYobz0wKSxhcmd1bWVudHMubGVuZ3RoPDMmJihjPW8pO3ZhciBzPXVbMV08dVswXSxs
PXVbcy0wXSxmPXVbMS1zXSxoPShmLWwpLyhuLmxlbmd0aC1vKzIqYyk7cmV0dXJuIGk9cihsK2gq
YyxoKSxzJiZpLnJldmVyc2UoKSxhPWgqKDEtbyksdD17dDoicmFuZ2VCYW5kcyIsYTphcmd1bWVu
dHN9LGV9LGUucmFuZ2VSb3VuZEJhbmRzPWZ1bmN0aW9uKHUsbyxjKXthcmd1bWVudHMubGVuZ3Ro
PDImJihvPTApLGFyZ3VtZW50cy5sZW5ndGg8MyYmKGM9byk7dmFyIHM9dVsxXTx1WzBdLGw9dVtz
LTBdLGY9dVsxLXNdLGg9TWF0aC5mbG9vcigoZi1sKS8obi5sZW5ndGgtbysyKmMpKSxnPWYtbC0o
bi5sZW5ndGgtbykqaDtyZXR1cm4gaT1yKGwrTWF0aC5yb3VuZChnLzIpLGgpLHMmJmkucmV2ZXJz
ZSgpLGE9TWF0aC5yb3VuZChoKigxLW8pKSx0PXt0OiJyYW5nZVJvdW5kQmFuZHMiLGE6YXJndW1l
bnRzfSxlfSxlLnJhbmdlQmFuZD1mdW5jdGlvbigpe3JldHVybiBhfSxlLnJhbmdlRXh0ZW50PWZ1
bmN0aW9uKCl7cmV0dXJuIGppKHQuYVswXSl9LGUuY29weT1mdW5jdGlvbigpe3JldHVybiB0byhu
LHQpfSxlLmRvbWFpbihuKX1mdW5jdGlvbiBlbyhlLHIpe2Z1bmN0aW9uIHUoKXt2YXIgbj0wLHQ9
ci5sZW5ndGg7Zm9yKG89W107KytuPHQ7KW9bbi0xXT1Hby5xdWFudGlsZShlLG4vdCk7cmV0dXJu
IGl9ZnVuY3Rpb24gaShuKXtyZXR1cm4gaXNOYU4obj0rbik/dm9pZCAwOnJbR28uYmlzZWN0KG8s
bildfXZhciBvO3JldHVybiBpLmRvbWFpbj1mdW5jdGlvbihyKXtyZXR1cm4gYXJndW1lbnRzLmxl
bmd0aD8oZT1yLmZpbHRlcih0KS5zb3J0KG4pLHUoKSk6ZX0saS5yYW5nZT1mdW5jdGlvbihuKXty
ZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8ocj1uLHUoKSk6cn0saS5xdWFudGlsZXM9ZnVuY3Rpb24o
KXtyZXR1cm4gb30saS5pbnZlcnRFeHRlbnQ9ZnVuY3Rpb24obil7cmV0dXJuIG49ci5pbmRleE9m
KG4pLDA+bj9bMC8wLDAvMF06W24+MD9vW24tMV06ZVswXSxuPG8ubGVuZ3RoP29bbl06ZVtlLmxl
bmd0aC0xXV19LGkuY29weT1mdW5jdGlvbigpe3JldHVybiBlbyhlLHIpfSx1KCl9ZnVuY3Rpb24g
cm8obix0LGUpe2Z1bmN0aW9uIHIodCl7cmV0dXJuIGVbTWF0aC5tYXgoMCxNYXRoLm1pbihvLE1h
dGguZmxvb3IoaSoodC1uKSkpKV19ZnVuY3Rpb24gdSgpe3JldHVybiBpPWUubGVuZ3RoLyh0LW4p
LG89ZS5sZW5ndGgtMSxyfXZhciBpLG87cmV0dXJuIHIuZG9tYWluPWZ1bmN0aW9uKGUpe3JldHVy
biBhcmd1bWVudHMubGVuZ3RoPyhuPStlWzBdLHQ9K2VbZS5sZW5ndGgtMV0sdSgpKTpbbix0XX0s
ci5yYW5nZT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZT1uLHUoKSk6ZX0s
ci5pbnZlcnRFeHRlbnQ9ZnVuY3Rpb24odCl7cmV0dXJuIHQ9ZS5pbmRleE9mKHQpLHQ9MD50PzAv
MDp0L2krbixbdCx0KzEvaV19LHIuY29weT1mdW5jdGlvbigpe3JldHVybiBybyhuLHQsZSl9LHUo
KX1mdW5jdGlvbiB1byhuLHQpe2Z1bmN0aW9uIGUoZSl7cmV0dXJuIGU+PWU/dFtHby5iaXNlY3Qo
bixlKV06dm9pZCAwfXJldHVybiBlLmRvbWFpbj1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRz
Lmxlbmd0aD8obj10LGUpOm59LGUucmFuZ2U9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5s
ZW5ndGg/KHQ9bixlKTp0fSxlLmludmVydEV4dGVudD1mdW5jdGlvbihlKXtyZXR1cm4gZT10Lmlu
ZGV4T2YoZSksW25bZS0xXSxuW2VdXX0sZS5jb3B5PWZ1bmN0aW9uKCl7cmV0dXJuIHVvKG4sdCl9
LGV9ZnVuY3Rpb24gaW8obil7ZnVuY3Rpb24gdChuKXtyZXR1cm4rbn1yZXR1cm4gdC5pbnZlcnQ9
dCx0LmRvbWFpbj10LnJhbmdlPWZ1bmN0aW9uKGUpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhu
PWUubWFwKHQpLHQpOm59LHQudGlja3M9ZnVuY3Rpb24odCl7cmV0dXJuIEJpKG4sdCl9LHQudGlj
a0Zvcm1hdD1mdW5jdGlvbih0LGUpe3JldHVybiBKaShuLHQsZSl9LHQuY29weT1mdW5jdGlvbigp
e3JldHVybiBpbyhuKX0sdH1mdW5jdGlvbiBvbyhuKXtyZXR1cm4gbi5pbm5lclJhZGl1c31mdW5j
dGlvbiBhbyhuKXtyZXR1cm4gbi5vdXRlclJhZGl1c31mdW5jdGlvbiBjbyhuKXtyZXR1cm4gbi5z
dGFydEFuZ2xlfWZ1bmN0aW9uIHNvKG4pe3JldHVybiBuLmVuZEFuZ2xlfWZ1bmN0aW9uIGxvKG4p
e2Z1bmN0aW9uIHQodCl7ZnVuY3Rpb24gbygpe3MucHVzaCgiTSIsaShuKGwpLGEpKX1mb3IodmFy
IGMscz1bXSxsPVtdLGY9LTEsaD10Lmxlbmd0aCxnPUV0KGUpLHA9RXQocik7KytmPGg7KXUuY2Fs
bCh0aGlzLGM9dFtmXSxmKT9sLnB1c2goWytnLmNhbGwodGhpcyxjLGYpLCtwLmNhbGwodGhpcyxj
LGYpXSk6bC5sZW5ndGgmJihvKCksbD1bXSk7cmV0dXJuIGwubGVuZ3RoJiZvKCkscy5sZW5ndGg/
cy5qb2luKCIiKTpudWxsfXZhciBlPUFyLHI9Q3IsdT1BZSxpPWZvLG89aS5rZXksYT0uNztyZXR1
cm4gdC54PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhlPW4sdCk6ZX0sdC55
PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhyPW4sdCk6cn0sdC5kZWZpbmVk
PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh1PW4sdCk6dX0sdC5pbnRlcnBv
bGF0ZT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8obz0iZnVuY3Rpb24iPT10
eXBlb2Ygbj9pPW46KGk9a3MuZ2V0KG4pfHxmbykua2V5LHQpOm99LHQudGVuc2lvbj1mdW5jdGlv
bihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oYT1uLHQpOmF9LHR9ZnVuY3Rpb24gZm8obil7
cmV0dXJuIG4uam9pbigiTCIpfWZ1bmN0aW9uIGhvKG4pe3JldHVybiBmbyhuKSsiWiJ9ZnVuY3Rp
b24gZ28obil7Zm9yKHZhciB0PTAsZT1uLmxlbmd0aCxyPW5bMF0sdT1bclswXSwiLCIsclsxXV07
Kyt0PGU7KXUucHVzaCgiSCIsKHJbMF0rKHI9blt0XSlbMF0pLzIsIlYiLHJbMV0pO3JldHVybiBl
PjEmJnUucHVzaCgiSCIsclswXSksdS5qb2luKCIiKX1mdW5jdGlvbiBwbyhuKXtmb3IodmFyIHQ9
MCxlPW4ubGVuZ3RoLHI9blswXSx1PVtyWzBdLCIsIixyWzFdXTsrK3Q8ZTspdS5wdXNoKCJWIiwo
cj1uW3RdKVsxXSwiSCIsclswXSk7cmV0dXJuIHUuam9pbigiIil9ZnVuY3Rpb24gdm8obil7Zm9y
KHZhciB0PTAsZT1uLmxlbmd0aCxyPW5bMF0sdT1bclswXSwiLCIsclsxXV07Kyt0PGU7KXUucHVz
aCgiSCIsKHI9blt0XSlbMF0sIlYiLHJbMV0pO3JldHVybiB1LmpvaW4oIiIpfWZ1bmN0aW9uIG1v
KG4sdCl7cmV0dXJuIG4ubGVuZ3RoPDQ/Zm8obik6blsxXStNbyhuLnNsaWNlKDEsbi5sZW5ndGgt
MSksX28obix0KSl9ZnVuY3Rpb24geW8obix0KXtyZXR1cm4gbi5sZW5ndGg8Mz9mbyhuKTpuWzBd
K01vKChuLnB1c2goblswXSksbiksX28oW25bbi5sZW5ndGgtMl1dLmNvbmNhdChuLFtuWzFdXSks
dCkpfWZ1bmN0aW9uIHhvKG4sdCl7cmV0dXJuIG4ubGVuZ3RoPDM/Zm8obik6blswXStNbyhuLF9v
KG4sdCkpfWZ1bmN0aW9uIE1vKG4sdCl7aWYodC5sZW5ndGg8MXx8bi5sZW5ndGghPXQubGVuZ3Ro
JiZuLmxlbmd0aCE9dC5sZW5ndGgrMilyZXR1cm4gZm8obik7dmFyIGU9bi5sZW5ndGghPXQubGVu
Z3RoLHI9IiIsdT1uWzBdLGk9blsxXSxvPXRbMF0sYT1vLGM9MTtpZihlJiYocis9IlEiKyhpWzBd
LTIqb1swXS8zKSsiLCIrKGlbMV0tMipvWzFdLzMpKyIsIitpWzBdKyIsIitpWzFdLHU9blsxXSxj
PTIpLHQubGVuZ3RoPjEpe2E9dFsxXSxpPW5bY10sYysrLHIrPSJDIisodVswXStvWzBdKSsiLCIr
KHVbMV0rb1sxXSkrIiwiKyhpWzBdLWFbMF0pKyIsIisoaVsxXS1hWzFdKSsiLCIraVswXSsiLCIr
aVsxXTtmb3IodmFyIHM9MjtzPHQubGVuZ3RoO3MrKyxjKyspaT1uW2NdLGE9dFtzXSxyKz0iUyIr
KGlbMF0tYVswXSkrIiwiKyhpWzFdLWFbMV0pKyIsIitpWzBdKyIsIitpWzFdfWlmKGUpe3ZhciBs
PW5bY107cis9IlEiKyhpWzBdKzIqYVswXS8zKSsiLCIrKGlbMV0rMiphWzFdLzMpKyIsIitsWzBd
KyIsIitsWzFdfXJldHVybiByfWZ1bmN0aW9uIF9vKG4sdCl7Zm9yKHZhciBlLHI9W10sdT0oMS10
KS8yLGk9blswXSxvPW5bMV0sYT0xLGM9bi5sZW5ndGg7KythPGM7KWU9aSxpPW8sbz1uW2FdLHIu
cHVzaChbdSoob1swXS1lWzBdKSx1KihvWzFdLWVbMV0pXSk7cmV0dXJuIHJ9ZnVuY3Rpb24gYm8o
bil7aWYobi5sZW5ndGg8MylyZXR1cm4gZm8obik7dmFyIHQ9MSxlPW4ubGVuZ3RoLHI9blswXSx1
PXJbMF0saT1yWzFdLG89W3UsdSx1LChyPW5bMV0pWzBdXSxhPVtpLGksaSxyWzFdXSxjPVt1LCIs
IixpLCJMIixFbyhDcyxvKSwiLCIsRW8oQ3MsYSldO2ZvcihuLnB1c2gobltlLTFdKTsrK3Q8PWU7
KXI9blt0XSxvLnNoaWZ0KCksby5wdXNoKHJbMF0pLGEuc2hpZnQoKSxhLnB1c2goclsxXSksQW8o
YyxvLGEpO3JldHVybiBuLnBvcCgpLGMucHVzaCgiTCIsciksYy5qb2luKCIiKX1mdW5jdGlvbiB3
byhuKXtpZihuLmxlbmd0aDw0KXJldHVybiBmbyhuKTtmb3IodmFyIHQsZT1bXSxyPS0xLHU9bi5s
ZW5ndGgsaT1bMF0sbz1bMF07KytyPDM7KXQ9bltyXSxpLnB1c2godFswXSksby5wdXNoKHRbMV0p
O2ZvcihlLnB1c2goRW8oQ3MsaSkrIiwiK0VvKENzLG8pKSwtLXI7KytyPHU7KXQ9bltyXSxpLnNo
aWZ0KCksaS5wdXNoKHRbMF0pLG8uc2hpZnQoKSxvLnB1c2godFsxXSksQW8oZSxpLG8pO3JldHVy
biBlLmpvaW4oIiIpfWZ1bmN0aW9uIFNvKG4pe2Zvcih2YXIgdCxlLHI9LTEsdT1uLmxlbmd0aCxp
PXUrNCxvPVtdLGE9W107KytyPDQ7KWU9bltyJXVdLG8ucHVzaChlWzBdKSxhLnB1c2goZVsxXSk7
Zm9yKHQ9W0VvKENzLG8pLCIsIixFbyhDcyxhKV0sLS1yOysrcjxpOyllPW5bciV1XSxvLnNoaWZ0
KCksby5wdXNoKGVbMF0pLGEuc2hpZnQoKSxhLnB1c2goZVsxXSksQW8odCxvLGEpO3JldHVybiB0
LmpvaW4oIiIpfWZ1bmN0aW9uIGtvKG4sdCl7dmFyIGU9bi5sZW5ndGgtMTtpZihlKWZvcih2YXIg
cix1LGk9blswXVswXSxvPW5bMF1bMV0sYT1uW2VdWzBdLWksYz1uW2VdWzFdLW8scz0tMTsrK3M8
PWU7KXI9bltzXSx1PXMvZSxyWzBdPXQqclswXSsoMS10KSooaSt1KmEpLHJbMV09dCpyWzFdKygx
LXQpKihvK3UqYyk7cmV0dXJuIGJvKG4pfWZ1bmN0aW9uIEVvKG4sdCl7cmV0dXJuIG5bMF0qdFsw
XStuWzFdKnRbMV0rblsyXSp0WzJdK25bM10qdFszXX1mdW5jdGlvbiBBbyhuLHQsZSl7bi5wdXNo
KCJDIixFbyhFcyx0KSwiLCIsRW8oRXMsZSksIiwiLEVvKEFzLHQpLCIsIixFbyhBcyxlKSwiLCIs
RW8oQ3MsdCksIiwiLEVvKENzLGUpKX1mdW5jdGlvbiBDbyhuLHQpe3JldHVybih0WzFdLW5bMV0p
Lyh0WzBdLW5bMF0pfWZ1bmN0aW9uIE5vKG4pe2Zvcih2YXIgdD0wLGU9bi5sZW5ndGgtMSxyPVtd
LHU9blswXSxpPW5bMV0sbz1yWzBdPUNvKHUsaSk7Kyt0PGU7KXJbdF09KG8rKG89Q28odT1pLGk9
blt0KzFdKSkpLzI7cmV0dXJuIHJbdF09byxyfWZ1bmN0aW9uIExvKG4pe2Zvcih2YXIgdCxlLHIs
dSxpPVtdLG89Tm8obiksYT0tMSxjPW4ubGVuZ3RoLTE7KythPGM7KXQ9Q28oblthXSxuW2ErMV0p
LGZhKHQpPFRhP29bYV09b1thKzFdPTA6KGU9b1thXS90LHI9b1thKzFdL3QsdT1lKmUrcipyLHU+
OSYmKHU9Myp0L01hdGguc3FydCh1KSxvW2FdPXUqZSxvW2ErMV09dSpyKSk7Zm9yKGE9LTE7Kyth
PD1jOyl1PShuW01hdGgubWluKGMsYSsxKV1bMF0tbltNYXRoLm1heCgwLGEtMSldWzBdKS8oNioo
MStvW2FdKm9bYV0pKSxpLnB1c2goW3V8fDAsb1thXSp1fHwwXSk7cmV0dXJuIGl9ZnVuY3Rpb24g
VG8obil7cmV0dXJuIG4ubGVuZ3RoPDM/Zm8obik6blswXStNbyhuLExvKG4pKX1mdW5jdGlvbiBx
byhuKXtmb3IodmFyIHQsZSxyLHU9LTEsaT1uLmxlbmd0aDsrK3U8aTspdD1uW3VdLGU9dFswXSxy
PXRbMV0rd3MsdFswXT1lKk1hdGguY29zKHIpLHRbMV09ZSpNYXRoLnNpbihyKTtyZXR1cm4gbn1m
dW5jdGlvbiB6byhuKXtmdW5jdGlvbiB0KHQpe2Z1bmN0aW9uIGMoKXt2LnB1c2goIk0iLGEobiht
KSxmKSxsLHMobihkLnJldmVyc2UoKSksZiksIloiKX1mb3IodmFyIGgsZyxwLHY9W10sZD1bXSxt
PVtdLHk9LTEseD10Lmxlbmd0aCxNPUV0KGUpLF89RXQodSksYj1lPT09cj9mdW5jdGlvbigpe3Jl
dHVybiBnfTpFdChyKSx3PXU9PT1pP2Z1bmN0aW9uKCl7cmV0dXJuIHB9OkV0KGkpOysreTx4Oylv
LmNhbGwodGhpcyxoPXRbeV0seSk/KGQucHVzaChbZz0rTS5jYWxsKHRoaXMsaCx5KSxwPStfLmNh
bGwodGhpcyxoLHkpXSksbS5wdXNoKFsrYi5jYWxsKHRoaXMsaCx5KSwrdy5jYWxsKHRoaXMsaCx5
KV0pKTpkLmxlbmd0aCYmKGMoKSxkPVtdLG09W10pO3JldHVybiBkLmxlbmd0aCYmYygpLHYubGVu
Z3RoP3Yuam9pbigiIik6bnVsbH12YXIgZT1BcixyPUFyLHU9MCxpPUNyLG89QWUsYT1mbyxjPWEu
a2V5LHM9YSxsPSJMIixmPS43O3JldHVybiB0Lng9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50
cy5sZW5ndGg/KGU9cj1uLHQpOnJ9LHQueDA9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5s
ZW5ndGg/KGU9bix0KTplfSx0LngxPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3Ro
PyhyPW4sdCk6cn0sdC55PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh1PWk9
bix0KTppfSx0LnkwPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh1PW4sdCk6
dX0sdC55MT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oaT1uLHQpOml9LHQu
ZGVmaW5lZD1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8obz1uLHQpOm99LHQu
aW50ZXJwb2xhdGU9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGM9ImZ1bmN0
aW9uIj09dHlwZW9mIG4/YT1uOihhPWtzLmdldChuKXx8Zm8pLmtleSxzPWEucmV2ZXJzZXx8YSxs
PWEuY2xvc2VkPyJNIjoiTCIsdCk6Y30sdC50ZW5zaW9uPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1
bWVudHMubGVuZ3RoPyhmPW4sdCk6Zn0sdH1mdW5jdGlvbiBSbyhuKXtyZXR1cm4gbi5yYWRpdXN9
ZnVuY3Rpb24gRG8obil7cmV0dXJuW24ueCxuLnldfWZ1bmN0aW9uIFBvKG4pe3JldHVybiBmdW5j
dGlvbigpe3ZhciB0PW4uYXBwbHkodGhpcyxhcmd1bWVudHMpLGU9dFswXSxyPXRbMV0rd3M7cmV0
dXJuW2UqTWF0aC5jb3MociksZSpNYXRoLnNpbihyKV19fWZ1bmN0aW9uIFVvKCl7cmV0dXJuIDY0
fWZ1bmN0aW9uIGpvKCl7cmV0dXJuImNpcmNsZSJ9ZnVuY3Rpb24gSG8obil7dmFyIHQ9TWF0aC5z
cXJ0KG4vQ2EpO3JldHVybiJNMCwiK3QrIkEiK3QrIiwiK3QrIiAwIDEsMSAwLCIrLXQrIkEiK3Qr
IiwiK3QrIiAwIDEsMSAwLCIrdCsiWiJ9ZnVuY3Rpb24gRm8obix0KXtyZXR1cm4gZGEobixScyks
bi5pZD10LG59ZnVuY3Rpb24gT28obix0LGUscil7dmFyIHU9bi5pZDtyZXR1cm4gUChuLCJmdW5j
dGlvbiI9PXR5cGVvZiBlP2Z1bmN0aW9uKG4saSxvKXtuLl9fdHJhbnNpdGlvbl9fW3VdLnR3ZWVu
LnNldCh0LHIoZS5jYWxsKG4sbi5fX2RhdGFfXyxpLG8pKSl9OihlPXIoZSksZnVuY3Rpb24obil7
bi5fX3RyYW5zaXRpb25fX1t1XS50d2Vlbi5zZXQodCxlKX0pKX1mdW5jdGlvbiBJbyhuKXtyZXR1
cm4gbnVsbD09biYmKG49IiIpLGZ1bmN0aW9uKCl7dGhpcy50ZXh0Q29udGVudD1ufX1mdW5jdGlv
biBZbyhuLHQsZSxyKXt2YXIgdT1uLl9fdHJhbnNpdGlvbl9ffHwobi5fX3RyYW5zaXRpb25fXz17
YWN0aXZlOjAsY291bnQ6MH0pLGk9dVtlXTtpZighaSl7dmFyIGE9ci50aW1lO2k9dVtlXT17dHdl
ZW46bmV3IG8sdGltZTphLGVhc2U6ci5lYXNlLGRlbGF5OnIuZGVsYXksZHVyYXRpb246ci5kdXJh
dGlvbn0sKyt1LmNvdW50LEdvLnRpbWVyKGZ1bmN0aW9uKHIpe2Z1bmN0aW9uIG8ocil7cmV0dXJu
IHUuYWN0aXZlPmU/cygpOih1LmFjdGl2ZT1lLGkuZXZlbnQmJmkuZXZlbnQuc3RhcnQuY2FsbChu
LGwsdCksaS50d2Vlbi5mb3JFYWNoKGZ1bmN0aW9uKGUscil7KHI9ci5jYWxsKG4sbCx0KSkmJnYu
cHVzaChyKX0pLEdvLnRpbWVyKGZ1bmN0aW9uKCl7cmV0dXJuIHAuYz1jKHJ8fDEpP0FlOmMsMX0s
MCxhKSx2b2lkIDApfWZ1bmN0aW9uIGMocil7aWYodS5hY3RpdmUhPT1lKXJldHVybiBzKCk7Zm9y
KHZhciBvPXIvZyxhPWYobyksYz12Lmxlbmd0aDtjPjA7KXZbLS1jXS5jYWxsKG4sYSk7cmV0dXJu
IG8+PTE/KGkuZXZlbnQmJmkuZXZlbnQuZW5kLmNhbGwobixsLHQpLHMoKSk6dm9pZCAwfWZ1bmN0
aW9uIHMoKXtyZXR1cm4tLXUuY291bnQ/ZGVsZXRlIHVbZV06ZGVsZXRlIG4uX190cmFuc2l0aW9u
X18sMX12YXIgbD1uLl9fZGF0YV9fLGY9aS5lYXNlLGg9aS5kZWxheSxnPWkuZHVyYXRpb24scD1u
Yyx2PVtdO3JldHVybiBwLnQ9aCthLHI+PWg/byhyLWgpOihwLmM9byx2b2lkIDApfSwwLGEpfX1m
dW5jdGlvbiBabyhuLHQpe24uYXR0cigidHJhbnNmb3JtIixmdW5jdGlvbihuKXtyZXR1cm4idHJh
bnNsYXRlKCIrdChuKSsiLDApIn0pfWZ1bmN0aW9uIFZvKG4sdCl7bi5hdHRyKCJ0cmFuc2Zvcm0i
LGZ1bmN0aW9uKG4pe3JldHVybiJ0cmFuc2xhdGUoMCwiK3QobikrIikifSl9ZnVuY3Rpb24gJG8o
bil7cmV0dXJuIG4udG9JU09TdHJpbmcoKX1mdW5jdGlvbiBYbyhuLHQsZSl7ZnVuY3Rpb24gcih0
KXtyZXR1cm4gbih0KQp9ZnVuY3Rpb24gdShuLGUpe3ZhciByPW5bMV0tblswXSx1PXIvZSxpPUdv
LmJpc2VjdChZcyx1KTtyZXR1cm4gaT09WXMubGVuZ3RoP1t0LnllYXIsWGkobi5tYXAoZnVuY3Rp
b24obil7cmV0dXJuIG4vMzE1MzZlNn0pLGUpWzJdXTppP3RbdS9Zc1tpLTFdPFlzW2ldL3U/aS0x
OmldOlskcyxYaShuLGUpWzJdXX1yZXR1cm4gci5pbnZlcnQ9ZnVuY3Rpb24odCl7cmV0dXJuIEJv
KG4uaW52ZXJ0KHQpKX0sci5kb21haW49ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5n
dGg/KG4uZG9tYWluKHQpLHIpOm4uZG9tYWluKCkubWFwKEJvKX0sci5uaWNlPWZ1bmN0aW9uKG4s
dCl7ZnVuY3Rpb24gZShlKXtyZXR1cm4haXNOYU4oZSkmJiFuLnJhbmdlKGUsQm8oK2UrMSksdCku
bGVuZ3RofXZhciBpPXIuZG9tYWluKCksbz1qaShpKSxhPW51bGw9PW4/dShvLDEwKToibnVtYmVy
Ij09dHlwZW9mIG4mJnUobyxuKTtyZXR1cm4gYSYmKG49YVswXSx0PWFbMV0pLHIuZG9tYWluKE9p
KGksdD4xP3tmbG9vcjpmdW5jdGlvbih0KXtmb3IoO2UodD1uLmZsb29yKHQpKTspdD1Cbyh0LTEp
O3JldHVybiB0fSxjZWlsOmZ1bmN0aW9uKHQpe2Zvcig7ZSh0PW4uY2VpbCh0KSk7KXQ9Qm8oK3Qr
MSk7cmV0dXJuIHR9fTpuKSl9LHIudGlja3M9ZnVuY3Rpb24obix0KXt2YXIgZT1qaShyLmRvbWFp
bigpKSxpPW51bGw9PW4/dShlLDEwKToibnVtYmVyIj09dHlwZW9mIG4/dShlLG4pOiFuLnJhbmdl
JiZbe3JhbmdlOm59LHRdO3JldHVybiBpJiYobj1pWzBdLHQ9aVsxXSksbi5yYW5nZShlWzBdLEJv
KCtlWzFdKzEpLDE+dD8xOnQpfSxyLnRpY2tGb3JtYXQ9ZnVuY3Rpb24oKXtyZXR1cm4gZX0sci5j
b3B5PWZ1bmN0aW9uKCl7cmV0dXJuIFhvKG4uY29weSgpLHQsZSl9LFZpKHIsbil9ZnVuY3Rpb24g
Qm8obil7cmV0dXJuIG5ldyBEYXRlKG4pfWZ1bmN0aW9uIEpvKG4pe3JldHVybiBKU09OLnBhcnNl
KG4ucmVzcG9uc2VUZXh0KX1mdW5jdGlvbiBXbyhuKXt2YXIgdD1uYS5jcmVhdGVSYW5nZSgpO3Jl
dHVybiB0LnNlbGVjdE5vZGUobmEuYm9keSksdC5jcmVhdGVDb250ZXh0dWFsRnJhZ21lbnQobi5y
ZXNwb25zZVRleHQpfXZhciBHbz17dmVyc2lvbjoiMy40LjYifTtEYXRlLm5vd3x8KERhdGUubm93
PWZ1bmN0aW9uKCl7cmV0dXJuK25ldyBEYXRlfSk7dmFyIEtvPVtdLnNsaWNlLFFvPWZ1bmN0aW9u
KG4pe3JldHVybiBLby5jYWxsKG4pfSxuYT1kb2N1bWVudCx0YT1uYS5kb2N1bWVudEVsZW1lbnQs
ZWE9d2luZG93O3RyeXtRbyh0YS5jaGlsZE5vZGVzKVswXS5ub2RlVHlwZX1jYXRjaChyYSl7UW89
ZnVuY3Rpb24obil7Zm9yKHZhciB0PW4ubGVuZ3RoLGU9bmV3IEFycmF5KHQpO3QtLTspZVt0XT1u
W3RdO3JldHVybiBlfX10cnl7bmEuY3JlYXRlRWxlbWVudCgiZGl2Iikuc3R5bGUuc2V0UHJvcGVy
dHkoIm9wYWNpdHkiLDAsIiIpfWNhdGNoKHVhKXt2YXIgaWE9ZWEuRWxlbWVudC5wcm90b3R5cGUs
b2E9aWEuc2V0QXR0cmlidXRlLGFhPWlhLnNldEF0dHJpYnV0ZU5TLGNhPWVhLkNTU1N0eWxlRGVj
bGFyYXRpb24ucHJvdG90eXBlLHNhPWNhLnNldFByb3BlcnR5O2lhLnNldEF0dHJpYnV0ZT1mdW5j
dGlvbihuLHQpe29hLmNhbGwodGhpcyxuLHQrIiIpfSxpYS5zZXRBdHRyaWJ1dGVOUz1mdW5jdGlv
bihuLHQsZSl7YWEuY2FsbCh0aGlzLG4sdCxlKyIiKX0sY2Euc2V0UHJvcGVydHk9ZnVuY3Rpb24o
bix0LGUpe3NhLmNhbGwodGhpcyxuLHQrIiIsZSl9fUdvLmFzY2VuZGluZz1uLEdvLmRlc2NlbmRp
bmc9ZnVuY3Rpb24obix0KXtyZXR1cm4gbj50Py0xOnQ+bj8xOnQ+PW4/MDowLzB9LEdvLm1pbj1m
dW5jdGlvbihuLHQpe3ZhciBlLHIsdT0tMSxpPW4ubGVuZ3RoO2lmKDE9PT1hcmd1bWVudHMubGVu
Z3RoKXtmb3IoOysrdTxpJiYhKG51bGwhPShlPW5bdV0pJiZlPj1lKTspZT12b2lkIDA7Zm9yKDsr
K3U8aTspbnVsbCE9KHI9blt1XSkmJmU+ciYmKGU9cil9ZWxzZXtmb3IoOysrdTxpJiYhKG51bGwh
PShlPXQuY2FsbChuLG5bdV0sdSkpJiZlPj1lKTspZT12b2lkIDA7Zm9yKDsrK3U8aTspbnVsbCE9
KHI9dC5jYWxsKG4sblt1XSx1KSkmJmU+ciYmKGU9cil9cmV0dXJuIGV9LEdvLm1heD1mdW5jdGlv
bihuLHQpe3ZhciBlLHIsdT0tMSxpPW4ubGVuZ3RoO2lmKDE9PT1hcmd1bWVudHMubGVuZ3RoKXtm
b3IoOysrdTxpJiYhKG51bGwhPShlPW5bdV0pJiZlPj1lKTspZT12b2lkIDA7Zm9yKDsrK3U8aTsp
bnVsbCE9KHI9blt1XSkmJnI+ZSYmKGU9cil9ZWxzZXtmb3IoOysrdTxpJiYhKG51bGwhPShlPXQu
Y2FsbChuLG5bdV0sdSkpJiZlPj1lKTspZT12b2lkIDA7Zm9yKDsrK3U8aTspbnVsbCE9KHI9dC5j
YWxsKG4sblt1XSx1KSkmJnI+ZSYmKGU9cil9cmV0dXJuIGV9LEdvLmV4dGVudD1mdW5jdGlvbihu
LHQpe3ZhciBlLHIsdSxpPS0xLG89bi5sZW5ndGg7aWYoMT09PWFyZ3VtZW50cy5sZW5ndGgpe2Zv
cig7KytpPG8mJiEobnVsbCE9KGU9dT1uW2ldKSYmZT49ZSk7KWU9dT12b2lkIDA7Zm9yKDsrK2k8
bzspbnVsbCE9KHI9bltpXSkmJihlPnImJihlPXIpLHI+dSYmKHU9cikpfWVsc2V7Zm9yKDsrK2k8
byYmIShudWxsIT0oZT11PXQuY2FsbChuLG5baV0saSkpJiZlPj1lKTspZT12b2lkIDA7Zm9yKDsr
K2k8bzspbnVsbCE9KHI9dC5jYWxsKG4sbltpXSxpKSkmJihlPnImJihlPXIpLHI+dSYmKHU9cikp
fXJldHVybltlLHVdfSxHby5zdW09ZnVuY3Rpb24obix0KXt2YXIgZSxyPTAsdT1uLmxlbmd0aCxp
PS0xO2lmKDE9PT1hcmd1bWVudHMubGVuZ3RoKWZvcig7KytpPHU7KWlzTmFOKGU9K25baV0pfHwo
cis9ZSk7ZWxzZSBmb3IoOysraTx1Oylpc05hTihlPSt0LmNhbGwobixuW2ldLGkpKXx8KHIrPWUp
O3JldHVybiByfSxHby5tZWFuPWZ1bmN0aW9uKG4sZSl7dmFyIHIsdT0wLGk9bi5sZW5ndGgsbz0t
MSxhPWk7aWYoMT09PWFyZ3VtZW50cy5sZW5ndGgpZm9yKDsrK288aTspdChyPW5bb10pP3UrPXI6
LS1hO2Vsc2UgZm9yKDsrK288aTspdChyPWUuY2FsbChuLG5bb10sbykpP3UrPXI6LS1hO3JldHVy
biBhP3UvYTp2b2lkIDB9LEdvLnF1YW50aWxlPWZ1bmN0aW9uKG4sdCl7dmFyIGU9KG4ubGVuZ3Ro
LTEpKnQrMSxyPU1hdGguZmxvb3IoZSksdT0rbltyLTFdLGk9ZS1yO3JldHVybiBpP3UraSooblty
XS11KTp1fSxHby5tZWRpYW49ZnVuY3Rpb24oZSxyKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD4x
JiYoZT1lLm1hcChyKSksZT1lLmZpbHRlcih0KSxlLmxlbmd0aD9Hby5xdWFudGlsZShlLnNvcnQo
biksLjUpOnZvaWQgMH07dmFyIGxhPWUobik7R28uYmlzZWN0TGVmdD1sYS5sZWZ0LEdvLmJpc2Vj
dD1Hby5iaXNlY3RSaWdodD1sYS5yaWdodCxHby5iaXNlY3Rvcj1mdW5jdGlvbih0KXtyZXR1cm4g
ZSgxPT09dC5sZW5ndGg/ZnVuY3Rpb24oZSxyKXtyZXR1cm4gbih0KGUpLHIpfTp0KX0sR28uc2h1
ZmZsZT1mdW5jdGlvbihuKXtmb3IodmFyIHQsZSxyPW4ubGVuZ3RoO3I7KWU9MHxNYXRoLnJhbmRv
bSgpKnItLSx0PW5bcl0sbltyXT1uW2VdLG5bZV09dDtyZXR1cm4gbn0sR28ucGVybXV0ZT1mdW5j
dGlvbihuLHQpe2Zvcih2YXIgZT10Lmxlbmd0aCxyPW5ldyBBcnJheShlKTtlLS07KXJbZV09blt0
W2VdXTtyZXR1cm4gcn0sR28ucGFpcnM9ZnVuY3Rpb24obil7Zm9yKHZhciB0LGU9MCxyPW4ubGVu
Z3RoLTEsdT1uWzBdLGk9bmV3IEFycmF5KDA+cj8wOnIpO3I+ZTspaVtlXT1bdD11LHU9blsrK2Vd
XTtyZXR1cm4gaX0sR28uemlwPWZ1bmN0aW9uKCl7aWYoISh1PWFyZ3VtZW50cy5sZW5ndGgpKXJl
dHVybltdO2Zvcih2YXIgbj0tMSx0PUdvLm1pbihhcmd1bWVudHMsciksZT1uZXcgQXJyYXkodCk7
KytuPHQ7KWZvcih2YXIgdSxpPS0xLG89ZVtuXT1uZXcgQXJyYXkodSk7KytpPHU7KW9baV09YXJn
dW1lbnRzW2ldW25dO3JldHVybiBlfSxHby50cmFuc3Bvc2U9ZnVuY3Rpb24obil7cmV0dXJuIEdv
LnppcC5hcHBseShHbyxuKX0sR28ua2V5cz1mdW5jdGlvbihuKXt2YXIgdD1bXTtmb3IodmFyIGUg
aW4gbil0LnB1c2goZSk7cmV0dXJuIHR9LEdvLnZhbHVlcz1mdW5jdGlvbihuKXt2YXIgdD1bXTtm
b3IodmFyIGUgaW4gbil0LnB1c2gobltlXSk7cmV0dXJuIHR9LEdvLmVudHJpZXM9ZnVuY3Rpb24o
bil7dmFyIHQ9W107Zm9yKHZhciBlIGluIG4pdC5wdXNoKHtrZXk6ZSx2YWx1ZTpuW2VdfSk7cmV0
dXJuIHR9LEdvLm1lcmdlPWZ1bmN0aW9uKG4pe2Zvcih2YXIgdCxlLHIsdT1uLmxlbmd0aCxpPS0x
LG89MDsrK2k8dTspbys9bltpXS5sZW5ndGg7Zm9yKGU9bmV3IEFycmF5KG8pOy0tdT49MDspZm9y
KHI9blt1XSx0PXIubGVuZ3RoOy0tdD49MDspZVstLW9dPXJbdF07cmV0dXJuIGV9O3ZhciBmYT1N
YXRoLmFicztHby5yYW5nZT1mdW5jdGlvbihuLHQsZSl7aWYoYXJndW1lbnRzLmxlbmd0aDwzJiYo
ZT0xLGFyZ3VtZW50cy5sZW5ndGg8MiYmKHQ9bixuPTApKSwxLzA9PT0odC1uKS9lKXRocm93IG5l
dyBFcnJvcigiaW5maW5pdGUgcmFuZ2UiKTt2YXIgcixpPVtdLG89dShmYShlKSksYT0tMTtpZihu
Kj1vLHQqPW8sZSo9bywwPmUpZm9yKDsocj1uK2UqKythKT50OylpLnB1c2goci9vKTtlbHNlIGZv
cig7KHI9bitlKisrYSk8dDspaS5wdXNoKHIvbyk7cmV0dXJuIGl9LEdvLm1hcD1mdW5jdGlvbihu
KXt2YXIgdD1uZXcgbztpZihuIGluc3RhbmNlb2YgbyluLmZvckVhY2goZnVuY3Rpb24obixlKXt0
LnNldChuLGUpfSk7ZWxzZSBmb3IodmFyIGUgaW4gbil0LnNldChlLG5bZV0pO3JldHVybiB0fSxp
KG8se2hhczphLGdldDpmdW5jdGlvbihuKXtyZXR1cm4gdGhpc1toYStuXX0sc2V0OmZ1bmN0aW9u
KG4sdCl7cmV0dXJuIHRoaXNbaGErbl09dH0scmVtb3ZlOmMsa2V5czpzLHZhbHVlczpmdW5jdGlv
bigpe3ZhciBuPVtdO3JldHVybiB0aGlzLmZvckVhY2goZnVuY3Rpb24odCxlKXtuLnB1c2goZSl9
KSxufSxlbnRyaWVzOmZ1bmN0aW9uKCl7dmFyIG49W107cmV0dXJuIHRoaXMuZm9yRWFjaChmdW5j
dGlvbih0LGUpe24ucHVzaCh7a2V5OnQsdmFsdWU6ZX0pfSksbn0sc2l6ZTpsLGVtcHR5OmYsZm9y
RWFjaDpmdW5jdGlvbihuKXtmb3IodmFyIHQgaW4gdGhpcyl0LmNoYXJDb2RlQXQoMCk9PT1nYSYm
bi5jYWxsKHRoaXMsdC5zdWJzdHJpbmcoMSksdGhpc1t0XSl9fSk7dmFyIGhhPSJceDAwIixnYT1o
YS5jaGFyQ29kZUF0KDApO0dvLm5lc3Q9ZnVuY3Rpb24oKXtmdW5jdGlvbiBuKHQsYSxjKXtpZihj
Pj1pLmxlbmd0aClyZXR1cm4gcj9yLmNhbGwodSxhKTplP2Euc29ydChlKTphO2Zvcih2YXIgcyxs
LGYsaCxnPS0xLHA9YS5sZW5ndGgsdj1pW2MrK10sZD1uZXcgbzsrK2c8cDspKGg9ZC5nZXQocz12
KGw9YVtnXSkpKT9oLnB1c2gobCk6ZC5zZXQocyxbbF0pO3JldHVybiB0PyhsPXQoKSxmPWZ1bmN0
aW9uKGUscil7bC5zZXQoZSxuKHQscixjKSl9KToobD17fSxmPWZ1bmN0aW9uKGUscil7bFtlXT1u
KHQscixjKX0pLGQuZm9yRWFjaChmKSxsfWZ1bmN0aW9uIHQobixlKXtpZihlPj1pLmxlbmd0aCly
ZXR1cm4gbjt2YXIgcj1bXSx1PWFbZSsrXTtyZXR1cm4gbi5mb3JFYWNoKGZ1bmN0aW9uKG4sdSl7
ci5wdXNoKHtrZXk6bix2YWx1ZXM6dCh1LGUpfSl9KSx1P3Iuc29ydChmdW5jdGlvbihuLHQpe3Jl
dHVybiB1KG4ua2V5LHQua2V5KX0pOnJ9dmFyIGUscix1PXt9LGk9W10sYT1bXTtyZXR1cm4gdS5t
YXA9ZnVuY3Rpb24odCxlKXtyZXR1cm4gbihlLHQsMCl9LHUuZW50cmllcz1mdW5jdGlvbihlKXty
ZXR1cm4gdChuKEdvLm1hcCxlLDApLDApfSx1LmtleT1mdW5jdGlvbihuKXtyZXR1cm4gaS5wdXNo
KG4pLHV9LHUuc29ydEtleXM9ZnVuY3Rpb24obil7cmV0dXJuIGFbaS5sZW5ndGgtMV09bix1fSx1
LnNvcnRWYWx1ZXM9ZnVuY3Rpb24obil7cmV0dXJuIGU9bix1fSx1LnJvbGx1cD1mdW5jdGlvbihu
KXtyZXR1cm4gcj1uLHV9LHV9LEdvLnNldD1mdW5jdGlvbihuKXt2YXIgdD1uZXcgaDtpZihuKWZv
cih2YXIgZT0wLHI9bi5sZW5ndGg7cj5lOysrZSl0LmFkZChuW2VdKTtyZXR1cm4gdH0saShoLHto
YXM6YSxhZGQ6ZnVuY3Rpb24obil7cmV0dXJuIHRoaXNbaGErbl09ITAsbn0scmVtb3ZlOmZ1bmN0
aW9uKG4pe3JldHVybiBuPWhhK24sbiBpbiB0aGlzJiZkZWxldGUgdGhpc1tuXX0sdmFsdWVzOnMs
c2l6ZTpsLGVtcHR5OmYsZm9yRWFjaDpmdW5jdGlvbihuKXtmb3IodmFyIHQgaW4gdGhpcyl0LmNo
YXJDb2RlQXQoMCk9PT1nYSYmbi5jYWxsKHRoaXMsdC5zdWJzdHJpbmcoMSkpfX0pLEdvLmJlaGF2
aW9yPXt9LEdvLnJlYmluZD1mdW5jdGlvbihuLHQpe2Zvcih2YXIgZSxyPTEsdT1hcmd1bWVudHMu
bGVuZ3RoOysrcjx1OyluW2U9YXJndW1lbnRzW3JdXT1nKG4sdCx0W2VdKTtyZXR1cm4gbn07dmFy
IHBhPVsid2Via2l0IiwibXMiLCJtb3oiLCJNb3oiLCJvIiwiTyJdO0dvLmRpc3BhdGNoPWZ1bmN0
aW9uKCl7Zm9yKHZhciBuPW5ldyBkLHQ9LTEsZT1hcmd1bWVudHMubGVuZ3RoOysrdDxlOyluW2Fy
Z3VtZW50c1t0XV09bShuKTtyZXR1cm4gbn0sZC5wcm90b3R5cGUub249ZnVuY3Rpb24obix0KXt2
YXIgZT1uLmluZGV4T2YoIi4iKSxyPSIiO2lmKGU+PTAmJihyPW4uc3Vic3RyaW5nKGUrMSksbj1u
LnN1YnN0cmluZygwLGUpKSxuKXJldHVybiBhcmd1bWVudHMubGVuZ3RoPDI/dGhpc1tuXS5vbihy
KTp0aGlzW25dLm9uKHIsdCk7aWYoMj09PWFyZ3VtZW50cy5sZW5ndGgpe2lmKG51bGw9PXQpZm9y
KG4gaW4gdGhpcyl0aGlzLmhhc093blByb3BlcnR5KG4pJiZ0aGlzW25dLm9uKHIsbnVsbCk7cmV0
dXJuIHRoaXN9fSxHby5ldmVudD1udWxsLEdvLnJlcXVvdGU9ZnVuY3Rpb24obil7cmV0dXJuIG4u
cmVwbGFjZSh2YSwiXFwkJiIpfTt2YXIgdmE9L1tcXFxeXCRcKlwrXD9cfFxbXF1cKFwpXC5ce1x9
XS9nLGRhPXt9Ll9fcHJvdG9fXz9mdW5jdGlvbihuLHQpe24uX19wcm90b19fPXR9OmZ1bmN0aW9u
KG4sdCl7Zm9yKHZhciBlIGluIHQpbltlXT10W2VdfSxtYT1mdW5jdGlvbihuLHQpe3JldHVybiB0
LnF1ZXJ5U2VsZWN0b3Iobil9LHlhPWZ1bmN0aW9uKG4sdCl7cmV0dXJuIHQucXVlcnlTZWxlY3Rv
ckFsbChuKX0seGE9dGFbcCh0YSwibWF0Y2hlc1NlbGVjdG9yIildLE1hPWZ1bmN0aW9uKG4sdCl7
cmV0dXJuIHhhLmNhbGwobix0KX07ImZ1bmN0aW9uIj09dHlwZW9mIFNpenpsZSYmKG1hPWZ1bmN0
aW9uKG4sdCl7cmV0dXJuIFNpenpsZShuLHQpWzBdfHxudWxsfSx5YT1TaXp6bGUsTWE9U2l6emxl
Lm1hdGNoZXNTZWxlY3RvciksR28uc2VsZWN0aW9uPWZ1bmN0aW9uKCl7cmV0dXJuIFNhfTt2YXIg
X2E9R28uc2VsZWN0aW9uLnByb3RvdHlwZT1bXTtfYS5zZWxlY3Q9ZnVuY3Rpb24obil7dmFyIHQs
ZSxyLHUsaT1bXTtuPWIobik7Zm9yKHZhciBvPS0xLGE9dGhpcy5sZW5ndGg7KytvPGE7KXtpLnB1
c2godD1bXSksdC5wYXJlbnROb2RlPShyPXRoaXNbb10pLnBhcmVudE5vZGU7Zm9yKHZhciBjPS0x
LHM9ci5sZW5ndGg7KytjPHM7KSh1PXJbY10pPyh0LnB1c2goZT1uLmNhbGwodSx1Ll9fZGF0YV9f
LGMsbykpLGUmJiJfX2RhdGFfXyJpbiB1JiYoZS5fX2RhdGFfXz11Ll9fZGF0YV9fKSk6dC5wdXNo
KG51bGwpfXJldHVybiBfKGkpfSxfYS5zZWxlY3RBbGw9ZnVuY3Rpb24obil7dmFyIHQsZSxyPVtd
O249dyhuKTtmb3IodmFyIHU9LTEsaT10aGlzLmxlbmd0aDsrK3U8aTspZm9yKHZhciBvPXRoaXNb
dV0sYT0tMSxjPW8ubGVuZ3RoOysrYTxjOykoZT1vW2FdKSYmKHIucHVzaCh0PVFvKG4uY2FsbChl
LGUuX19kYXRhX18sYSx1KSkpLHQucGFyZW50Tm9kZT1lKTtyZXR1cm4gXyhyKX07dmFyIGJhPXtz
dmc6Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIix4aHRtbDoiaHR0cDovL3d3dy53My5vcmcv
MTk5OS94aHRtbCIseGxpbms6Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiLHhtbDoiaHR0
cDovL3d3dy53My5vcmcvWE1MLzE5OTgvbmFtZXNwYWNlIix4bWxuczoiaHR0cDovL3d3dy53My5v
cmcvMjAwMC94bWxucy8ifTtHby5ucz17cHJlZml4OmJhLHF1YWxpZnk6ZnVuY3Rpb24obil7dmFy
IHQ9bi5pbmRleE9mKCI6IiksZT1uO3JldHVybiB0Pj0wJiYoZT1uLnN1YnN0cmluZygwLHQpLG49
bi5zdWJzdHJpbmcodCsxKSksYmEuaGFzT3duUHJvcGVydHkoZSk/e3NwYWNlOmJhW2VdLGxvY2Fs
Om59Om59fSxfYS5hdHRyPWZ1bmN0aW9uKG4sdCl7aWYoYXJndW1lbnRzLmxlbmd0aDwyKXtpZigi
c3RyaW5nIj09dHlwZW9mIG4pe3ZhciBlPXRoaXMubm9kZSgpO3JldHVybiBuPUdvLm5zLnF1YWxp
Znkobiksbi5sb2NhbD9lLmdldEF0dHJpYnV0ZU5TKG4uc3BhY2Usbi5sb2NhbCk6ZS5nZXRBdHRy
aWJ1dGUobil9Zm9yKHQgaW4gbil0aGlzLmVhY2goUyh0LG5bdF0pKTtyZXR1cm4gdGhpc31yZXR1
cm4gdGhpcy5lYWNoKFMobix0KSl9LF9hLmNsYXNzZWQ9ZnVuY3Rpb24obix0KXtpZihhcmd1bWVu
dHMubGVuZ3RoPDIpe2lmKCJzdHJpbmciPT10eXBlb2Ygbil7dmFyIGU9dGhpcy5ub2RlKCkscj0o
bj1BKG4pKS5sZW5ndGgsdT0tMTtpZih0PWUuY2xhc3NMaXN0KXtmb3IoOysrdTxyOylpZighdC5j
b250YWlucyhuW3VdKSlyZXR1cm4hMX1lbHNlIGZvcih0PWUuZ2V0QXR0cmlidXRlKCJjbGFzcyIp
OysrdTxyOylpZighRShuW3VdKS50ZXN0KHQpKXJldHVybiExO3JldHVybiEwfWZvcih0IGluIG4p
dGhpcy5lYWNoKEModCxuW3RdKSk7cmV0dXJuIHRoaXN9cmV0dXJuIHRoaXMuZWFjaChDKG4sdCkp
fSxfYS5zdHlsZT1mdW5jdGlvbihuLHQsZSl7dmFyIHI9YXJndW1lbnRzLmxlbmd0aDtpZigzPnIp
e2lmKCJzdHJpbmciIT10eXBlb2Ygbil7Mj5yJiYodD0iIik7Zm9yKGUgaW4gbil0aGlzLmVhY2go
TChlLG5bZV0sdCkpO3JldHVybiB0aGlzfWlmKDI+cilyZXR1cm4gZWEuZ2V0Q29tcHV0ZWRTdHls
ZSh0aGlzLm5vZGUoKSxudWxsKS5nZXRQcm9wZXJ0eVZhbHVlKG4pO2U9IiJ9cmV0dXJuIHRoaXMu
ZWFjaChMKG4sdCxlKSl9LF9hLnByb3BlcnR5PWZ1bmN0aW9uKG4sdCl7aWYoYXJndW1lbnRzLmxl
bmd0aDwyKXtpZigic3RyaW5nIj09dHlwZW9mIG4pcmV0dXJuIHRoaXMubm9kZSgpW25dO2Zvcih0
IGluIG4pdGhpcy5lYWNoKFQodCxuW3RdKSk7cmV0dXJuIHRoaXN9cmV0dXJuIHRoaXMuZWFjaChU
KG4sdCkpfSxfYS50ZXh0PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoP3RoaXMu
ZWFjaCgiZnVuY3Rpb24iPT10eXBlb2Ygbj9mdW5jdGlvbigpe3ZhciB0PW4uYXBwbHkodGhpcyxh
cmd1bWVudHMpO3RoaXMudGV4dENvbnRlbnQ9bnVsbD09dD8iIjp0fTpudWxsPT1uP2Z1bmN0aW9u
KCl7dGhpcy50ZXh0Q29udGVudD0iIn06ZnVuY3Rpb24oKXt0aGlzLnRleHRDb250ZW50PW59KTp0
aGlzLm5vZGUoKS50ZXh0Q29udGVudH0sX2EuaHRtbD1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1l
bnRzLmxlbmd0aD90aGlzLmVhY2goImZ1bmN0aW9uIj09dHlwZW9mIG4/ZnVuY3Rpb24oKXt2YXIg
dD1uLmFwcGx5KHRoaXMsYXJndW1lbnRzKTt0aGlzLmlubmVySFRNTD1udWxsPT10PyIiOnR9Om51
bGw9PW4/ZnVuY3Rpb24oKXt0aGlzLmlubmVySFRNTD0iIn06ZnVuY3Rpb24oKXt0aGlzLmlubmVy
SFRNTD1ufSk6dGhpcy5ub2RlKCkuaW5uZXJIVE1MfSxfYS5hcHBlbmQ9ZnVuY3Rpb24obil7cmV0
dXJuIG49cShuKSx0aGlzLnNlbGVjdChmdW5jdGlvbigpe3JldHVybiB0aGlzLmFwcGVuZENoaWxk
KG4uYXBwbHkodGhpcyxhcmd1bWVudHMpKX0pfSxfYS5pbnNlcnQ9ZnVuY3Rpb24obix0KXtyZXR1
cm4gbj1xKG4pLHQ9Yih0KSx0aGlzLnNlbGVjdChmdW5jdGlvbigpe3JldHVybiB0aGlzLmluc2Vy
dEJlZm9yZShuLmFwcGx5KHRoaXMsYXJndW1lbnRzKSx0LmFwcGx5KHRoaXMsYXJndW1lbnRzKXx8
bnVsbCl9KX0sX2EucmVtb3ZlPWZ1bmN0aW9uKCl7cmV0dXJuIHRoaXMuZWFjaChmdW5jdGlvbigp
e3ZhciBuPXRoaXMucGFyZW50Tm9kZTtuJiZuLnJlbW92ZUNoaWxkKHRoaXMpfSl9LF9hLmRhdGE9
ZnVuY3Rpb24obix0KXtmdW5jdGlvbiBlKG4sZSl7dmFyIHIsdSxpLGE9bi5sZW5ndGgsZj1lLmxl
bmd0aCxoPU1hdGgubWluKGEsZiksZz1uZXcgQXJyYXkoZikscD1uZXcgQXJyYXkoZiksdj1uZXcg
QXJyYXkoYSk7aWYodCl7dmFyIGQsbT1uZXcgbyx5PW5ldyBvLHg9W107Zm9yKHI9LTE7KytyPGE7
KWQ9dC5jYWxsKHU9bltyXSx1Ll9fZGF0YV9fLHIpLG0uaGFzKGQpP3Zbcl09dTptLnNldChkLHUp
LHgucHVzaChkKTtmb3Iocj0tMTsrK3I8ZjspZD10LmNhbGwoZSxpPWVbcl0sciksKHU9bS5nZXQo
ZCkpPyhnW3JdPXUsdS5fX2RhdGFfXz1pKTp5LmhhcyhkKXx8KHBbcl09eihpKSkseS5zZXQoZCxp
KSxtLnJlbW92ZShkKTtmb3Iocj0tMTsrK3I8YTspbS5oYXMoeFtyXSkmJih2W3JdPW5bcl0pfWVs
c2V7Zm9yKHI9LTE7KytyPGg7KXU9bltyXSxpPWVbcl0sdT8odS5fX2RhdGFfXz1pLGdbcl09dSk6
cFtyXT16KGkpO2Zvcig7Zj5yOysrcilwW3JdPXooZVtyXSk7Zm9yKDthPnI7KytyKXZbcl09blty
XX1wLnVwZGF0ZT1nLHAucGFyZW50Tm9kZT1nLnBhcmVudE5vZGU9di5wYXJlbnROb2RlPW4ucGFy
ZW50Tm9kZSxjLnB1c2gocCkscy5wdXNoKGcpLGwucHVzaCh2KX12YXIgcix1LGk9LTEsYT10aGlz
Lmxlbmd0aDtpZighYXJndW1lbnRzLmxlbmd0aCl7Zm9yKG49bmV3IEFycmF5KGE9KHI9dGhpc1sw
XSkubGVuZ3RoKTsrK2k8YTspKHU9cltpXSkmJihuW2ldPXUuX19kYXRhX18pO3JldHVybiBufXZh
ciBjPVUoW10pLHM9XyhbXSksbD1fKFtdKTtpZigiZnVuY3Rpb24iPT10eXBlb2Ygbilmb3IoOysr
aTxhOyllKHI9dGhpc1tpXSxuLmNhbGwocixyLnBhcmVudE5vZGUuX19kYXRhX18saSkpO2Vsc2Ug
Zm9yKDsrK2k8YTspZShyPXRoaXNbaV0sbik7cmV0dXJuIHMuZW50ZXI9ZnVuY3Rpb24oKXtyZXR1
cm4gY30scy5leGl0PWZ1bmN0aW9uKCl7cmV0dXJuIGx9LHN9LF9hLmRhdHVtPWZ1bmN0aW9uKG4p
e3JldHVybiBhcmd1bWVudHMubGVuZ3RoP3RoaXMucHJvcGVydHkoIl9fZGF0YV9fIixuKTp0aGlz
LnByb3BlcnR5KCJfX2RhdGFfXyIpfSxfYS5maWx0ZXI9ZnVuY3Rpb24obil7dmFyIHQsZSxyLHU9
W107ImZ1bmN0aW9uIiE9dHlwZW9mIG4mJihuPVIobikpO2Zvcih2YXIgaT0wLG89dGhpcy5sZW5n
dGg7bz5pO2krKyl7dS5wdXNoKHQ9W10pLHQucGFyZW50Tm9kZT0oZT10aGlzW2ldKS5wYXJlbnRO
b2RlO2Zvcih2YXIgYT0wLGM9ZS5sZW5ndGg7Yz5hO2ErKykocj1lW2FdKSYmbi5jYWxsKHIsci5f
X2RhdGFfXyxhLGkpJiZ0LnB1c2gocil9cmV0dXJuIF8odSl9LF9hLm9yZGVyPWZ1bmN0aW9uKCl7
Zm9yKHZhciBuPS0xLHQ9dGhpcy5sZW5ndGg7KytuPHQ7KWZvcih2YXIgZSxyPXRoaXNbbl0sdT1y
Lmxlbmd0aC0xLGk9clt1XTstLXU+PTA7KShlPXJbdV0pJiYoaSYmaSE9PWUubmV4dFNpYmxpbmcm
JmkucGFyZW50Tm9kZS5pbnNlcnRCZWZvcmUoZSxpKSxpPWUpO3JldHVybiB0aGlzfSxfYS5zb3J0
PWZ1bmN0aW9uKG4pe249RC5hcHBseSh0aGlzLGFyZ3VtZW50cyk7Zm9yKHZhciB0PS0xLGU9dGhp
cy5sZW5ndGg7Kyt0PGU7KXRoaXNbdF0uc29ydChuKTtyZXR1cm4gdGhpcy5vcmRlcigpfSxfYS5l
YWNoPWZ1bmN0aW9uKG4pe3JldHVybiBQKHRoaXMsZnVuY3Rpb24odCxlLHIpe24uY2FsbCh0LHQu
X19kYXRhX18sZSxyKX0pfSxfYS5jYWxsPWZ1bmN0aW9uKG4pe3ZhciB0PVFvKGFyZ3VtZW50cyk7
cmV0dXJuIG4uYXBwbHkodFswXT10aGlzLHQpLHRoaXN9LF9hLmVtcHR5PWZ1bmN0aW9uKCl7cmV0
dXJuIXRoaXMubm9kZSgpfSxfYS5ub2RlPWZ1bmN0aW9uKCl7Zm9yKHZhciBuPTAsdD10aGlzLmxl
bmd0aDt0Pm47bisrKWZvcih2YXIgZT10aGlzW25dLHI9MCx1PWUubGVuZ3RoO3U+cjtyKyspe3Zh
ciBpPWVbcl07aWYoaSlyZXR1cm4gaX1yZXR1cm4gbnVsbH0sX2Euc2l6ZT1mdW5jdGlvbigpe3Zh
ciBuPTA7cmV0dXJuIHRoaXMuZWFjaChmdW5jdGlvbigpeysrbn0pLG59O3ZhciB3YT1bXTtHby5z
ZWxlY3Rpb24uZW50ZXI9VSxHby5zZWxlY3Rpb24uZW50ZXIucHJvdG90eXBlPXdhLHdhLmFwcGVu
ZD1fYS5hcHBlbmQsd2EuZW1wdHk9X2EuZW1wdHksd2Eubm9kZT1fYS5ub2RlLHdhLmNhbGw9X2Eu
Y2FsbCx3YS5zaXplPV9hLnNpemUsd2Euc2VsZWN0PWZ1bmN0aW9uKG4pe2Zvcih2YXIgdCxlLHIs
dSxpLG89W10sYT0tMSxjPXRoaXMubGVuZ3RoOysrYTxjOyl7cj0odT10aGlzW2FdKS51cGRhdGUs
by5wdXNoKHQ9W10pLHQucGFyZW50Tm9kZT11LnBhcmVudE5vZGU7Zm9yKHZhciBzPS0xLGw9dS5s
ZW5ndGg7KytzPGw7KShpPXVbc10pPyh0LnB1c2gocltzXT1lPW4uY2FsbCh1LnBhcmVudE5vZGUs
aS5fX2RhdGFfXyxzLGEpKSxlLl9fZGF0YV9fPWkuX19kYXRhX18pOnQucHVzaChudWxsKX1yZXR1
cm4gXyhvKX0sd2EuaW5zZXJ0PWZ1bmN0aW9uKG4sdCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg8
MiYmKHQ9aih0aGlzKSksX2EuaW5zZXJ0LmNhbGwodGhpcyxuLHQpfSxfYS50cmFuc2l0aW9uPWZ1
bmN0aW9uKCl7Zm9yKHZhciBuLHQsZT1Mc3x8KytEcyxyPVtdLHU9VHN8fHt0aW1lOkRhdGUubm93
KCksZWFzZTp3dSxkZWxheTowLGR1cmF0aW9uOjI1MH0saT0tMSxvPXRoaXMubGVuZ3RoOysraTxv
Oyl7ci5wdXNoKG49W10pO2Zvcih2YXIgYT10aGlzW2ldLGM9LTEscz1hLmxlbmd0aDsrK2M8czsp
KHQ9YVtjXSkmJllvKHQsYyxlLHUpLG4ucHVzaCh0KX1yZXR1cm4gRm8ocixlKX0sX2EuaW50ZXJy
dXB0PWZ1bmN0aW9uKCl7cmV0dXJuIHRoaXMuZWFjaChIKX0sR28uc2VsZWN0PWZ1bmN0aW9uKG4p
e3ZhciB0PVsic3RyaW5nIj09dHlwZW9mIG4/bWEobixuYSk6bl07cmV0dXJuIHQucGFyZW50Tm9k
ZT10YSxfKFt0XSl9LEdvLnNlbGVjdEFsbD1mdW5jdGlvbihuKXt2YXIgdD1Rbygic3RyaW5nIj09
dHlwZW9mIG4/eWEobixuYSk6bik7cmV0dXJuIHQucGFyZW50Tm9kZT10YSxfKFt0XSl9O3ZhciBT
YT1Hby5zZWxlY3QodGEpO19hLm9uPWZ1bmN0aW9uKG4sdCxlKXt2YXIgcj1hcmd1bWVudHMubGVu
Z3RoO2lmKDM+cil7aWYoInN0cmluZyIhPXR5cGVvZiBuKXsyPnImJih0PSExKTtmb3IoZSBpbiBu
KXRoaXMuZWFjaChGKGUsbltlXSx0KSk7cmV0dXJuIHRoaXN9aWYoMj5yKXJldHVybihyPXRoaXMu
bm9kZSgpWyJfX29uIituXSkmJnIuXztlPSExfXJldHVybiB0aGlzLmVhY2goRihuLHQsZSkpfTt2
YXIga2E9R28ubWFwKHttb3VzZWVudGVyOiJtb3VzZW92ZXIiLG1vdXNlbGVhdmU6Im1vdXNlb3V0
In0pO2thLmZvckVhY2goZnVuY3Rpb24obil7Im9uIituIGluIG5hJiZrYS5yZW1vdmUobil9KTt2
YXIgRWE9Im9uc2VsZWN0c3RhcnQiaW4gbmE/bnVsbDpwKHRhLnN0eWxlLCJ1c2VyU2VsZWN0Iiks
QWE9MDtHby5tb3VzZT1mdW5jdGlvbihuKXtyZXR1cm4gWihuLHgoKSl9LEdvLnRvdWNoZXM9ZnVu
Y3Rpb24obix0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aDwyJiYodD14KCkudG91Y2hlcyksdD9R
byh0KS5tYXAoZnVuY3Rpb24odCl7dmFyIGU9WihuLHQpO3JldHVybiBlLmlkZW50aWZpZXI9dC5p
ZGVudGlmaWVyLGV9KTpbXX0sR28uYmVoYXZpb3IuZHJhZz1mdW5jdGlvbigpe2Z1bmN0aW9uIG4o
KXt0aGlzLm9uKCJtb3VzZWRvd24uZHJhZyIsdSkub24oInRvdWNoc3RhcnQuZHJhZyIsaSl9ZnVu
Y3Rpb24gdChuLHQsdSxpLG8pe3JldHVybiBmdW5jdGlvbigpe2Z1bmN0aW9uIGEoKXt2YXIgbixl
LHI9dChoLHYpO3ImJihuPXJbMF0teFswXSxlPXJbMV0teFsxXSxwfD1ufGUseD1yLGcoe3R5cGU6
ImRyYWciLHg6clswXStzWzBdLHk6clsxXStzWzFdLGR4Om4sZHk6ZX0pKX1mdW5jdGlvbiBjKCl7
dChoLHYpJiYobS5vbihpK2QsbnVsbCkub24obytkLG51bGwpLHkocCYmR28uZXZlbnQudGFyZ2V0
PT09ZiksZyh7dHlwZToiZHJhZ2VuZCJ9KSl9dmFyIHMsbD10aGlzLGY9R28uZXZlbnQudGFyZ2V0
LGg9bC5wYXJlbnROb2RlLGc9ZS5vZihsLGFyZ3VtZW50cykscD0wLHY9bigpLGQ9Ii5kcmFnIiso
bnVsbD09dj8iIjoiLSIrdiksbT1Hby5zZWxlY3QodSgpKS5vbihpK2QsYSkub24obytkLGMpLHk9
WSgpLHg9dChoLHYpO3I/KHM9ci5hcHBseShsLGFyZ3VtZW50cykscz1bcy54LXhbMF0scy55LXhb
MV1dKTpzPVswLDBdLGcoe3R5cGU6ImRyYWdzdGFydCJ9KX19dmFyIGU9TShuLCJkcmFnIiwiZHJh
Z3N0YXJ0IiwiZHJhZ2VuZCIpLHI9bnVsbCx1PXQodixHby5tb3VzZSxYLCJtb3VzZW1vdmUiLCJt
b3VzZXVwIiksaT10KFYsR28udG91Y2gsJCwidG91Y2htb3ZlIiwidG91Y2hlbmQiKTtyZXR1cm4g
bi5vcmlnaW49ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHI9dCxuKTpyfSxH
by5yZWJpbmQobixlLCJvbiIpfTt2YXIgQ2E9TWF0aC5QSSxOYT0yKkNhLExhPUNhLzIsVGE9MWUt
NixxYT1UYSpUYSx6YT1DYS8xODAsUmE9MTgwL0NhLERhPU1hdGguU1FSVDIsUGE9MixVYT00O0dv
LmludGVycG9sYXRlWm9vbT1mdW5jdGlvbihuLHQpe2Z1bmN0aW9uIGUobil7dmFyIHQ9bip5O2lm
KG0pe3ZhciBlPVEodiksbz1pLyhQYSpoKSooZSpudChEYSp0K3YpLUsodikpO3JldHVybltyK28q
cyx1K28qbCxpKmUvUShEYSp0K3YpXX1yZXR1cm5bcituKnMsdStuKmwsaSpNYXRoLmV4cChEYSp0
KV19dmFyIHI9blswXSx1PW5bMV0saT1uWzJdLG89dFswXSxhPXRbMV0sYz10WzJdLHM9by1yLGw9
YS11LGY9cypzK2wqbCxoPU1hdGguc3FydChmKSxnPShjKmMtaSppK1VhKmYpLygyKmkqUGEqaCks
cD0oYypjLWkqaS1VYSpmKS8oMipjKlBhKmgpLHY9TWF0aC5sb2coTWF0aC5zcXJ0KGcqZysxKS1n
KSxkPU1hdGgubG9nKE1hdGguc3FydChwKnArMSktcCksbT1kLXYseT0obXx8TWF0aC5sb2coYy9p
KSkvRGE7cmV0dXJuIGUuZHVyYXRpb249MWUzKnksZX0sR28uYmVoYXZpb3Iuem9vbT1mdW5jdGlv
bigpe2Z1bmN0aW9uIG4obil7bi5vbihBLHMpLm9uKEZhKyIuem9vbSIsZikub24oQyxoKS5vbigi
ZGJsY2xpY2suem9vbSIsZykub24oTCxsKX1mdW5jdGlvbiB0KG4pe3JldHVyblsoblswXS1TLngp
L1MuaywoblsxXS1TLnkpL1Mua119ZnVuY3Rpb24gZShuKXtyZXR1cm5bblswXSpTLmsrUy54LG5b
MV0qUy5rK1MueV19ZnVuY3Rpb24gcihuKXtTLms9TWF0aC5tYXgoRVswXSxNYXRoLm1pbihFWzFd
LG4pKX1mdW5jdGlvbiB1KG4sdCl7dD1lKHQpLFMueCs9blswXS10WzBdLFMueSs9blsxXS10WzFd
fWZ1bmN0aW9uIGkoKXtfJiZfLmRvbWFpbih4LnJhbmdlKCkubWFwKGZ1bmN0aW9uKG4pe3JldHVy
bihuLVMueCkvUy5rfSkubWFwKHguaW52ZXJ0KSksdyYmdy5kb21haW4oYi5yYW5nZSgpLm1hcChm
dW5jdGlvbihuKXtyZXR1cm4obi1TLnkpL1Mua30pLm1hcChiLmludmVydCkpfWZ1bmN0aW9uIG8o
bil7bih7dHlwZToiem9vbXN0YXJ0In0pfWZ1bmN0aW9uIGEobil7aSgpLG4oe3R5cGU6Inpvb20i
LHNjYWxlOlMuayx0cmFuc2xhdGU6W1MueCxTLnldfSl9ZnVuY3Rpb24gYyhuKXtuKHt0eXBlOiJ6
b29tZW5kIn0pfWZ1bmN0aW9uIHMoKXtmdW5jdGlvbiBuKCl7bD0xLHUoR28ubW91c2UociksZyks
YShzKX1mdW5jdGlvbiBlKCl7Zi5vbihDLGVhPT09cj9oOm51bGwpLm9uKE4sbnVsbCkscChsJiZH
by5ldmVudC50YXJnZXQ9PT1pKSxjKHMpfXZhciByPXRoaXMsaT1Hby5ldmVudC50YXJnZXQscz1U
Lm9mKHIsYXJndW1lbnRzKSxsPTAsZj1Hby5zZWxlY3QoZWEpLm9uKEMsbikub24oTixlKSxnPXQo
R28ubW91c2UocikpLHA9WSgpO0guY2FsbChyKSxvKHMpfWZ1bmN0aW9uIGwoKXtmdW5jdGlvbiBu
KCl7dmFyIG49R28udG91Y2hlcyhnKTtyZXR1cm4gaD1TLmssbi5mb3JFYWNoKGZ1bmN0aW9uKG4p
e24uaWRlbnRpZmllciBpbiB2JiYodltuLmlkZW50aWZpZXJdPXQobikpfSksbn1mdW5jdGlvbiBl
KCl7Zm9yKHZhciB0PUdvLmV2ZW50LmNoYW5nZWRUb3VjaGVzLGU9MCxpPXQubGVuZ3RoO2k+ZTsr
K2Updlt0W2VdLmlkZW50aWZpZXJdPW51bGw7dmFyIG89bigpLGM9RGF0ZS5ub3coKTtpZigxPT09
by5sZW5ndGgpe2lmKDUwMD5jLW0pe3ZhciBzPW9bMF0sbD12W3MuaWRlbnRpZmllcl07cigyKlMu
ayksdShzLGwpLHkoKSxhKHApfW09Y31lbHNlIGlmKG8ubGVuZ3RoPjEpe3ZhciBzPW9bMF0sZj1v
WzFdLGg9c1swXS1mWzBdLGc9c1sxXS1mWzFdO2Q9aCpoK2cqZ319ZnVuY3Rpb24gaSgpe2Zvcih2
YXIgbix0LGUsaSxvPUdvLnRvdWNoZXMoZyksYz0wLHM9by5sZW5ndGg7cz5jOysrYyxpPW51bGwp
aWYoZT1vW2NdLGk9dltlLmlkZW50aWZpZXJdKXtpZih0KWJyZWFrO249ZSx0PWl9aWYoaSl7dmFy
IGw9KGw9ZVswXS1uWzBdKSpsKyhsPWVbMV0tblsxXSkqbCxmPWQmJk1hdGguc3FydChsL2QpO249
WyhuWzBdK2VbMF0pLzIsKG5bMV0rZVsxXSkvMl0sdD1bKHRbMF0raVswXSkvMiwodFsxXStpWzFd
KS8yXSxyKGYqaCl9bT1udWxsLHUobix0KSxhKHApfWZ1bmN0aW9uIGYoKXtpZihHby5ldmVudC50
b3VjaGVzLmxlbmd0aCl7Zm9yKHZhciB0PUdvLmV2ZW50LmNoYW5nZWRUb3VjaGVzLGU9MCxyPXQu
bGVuZ3RoO3I+ZTsrK2UpZGVsZXRlIHZbdFtlXS5pZGVudGlmaWVyXTtmb3IodmFyIHUgaW4gdily
ZXR1cm4gdm9pZCBuKCl9Yi5vbih4LG51bGwpLHcub24oQSxzKS5vbihMLGwpLGsoKSxjKHApfXZh
ciBoLGc9dGhpcyxwPVQub2YoZyxhcmd1bWVudHMpLHY9e30sZD0wLHg9Ii56b29tLSIrR28uZXZl
bnQuY2hhbmdlZFRvdWNoZXNbMF0uaWRlbnRpZmllcixNPSJ0b3VjaG1vdmUiK3gsXz0idG91Y2hl
bmQiK3gsYj1Hby5zZWxlY3QoR28uZXZlbnQudGFyZ2V0KS5vbihNLGkpLm9uKF8sZiksdz1Hby5z
ZWxlY3QoZykub24oQSxudWxsKS5vbihMLGUpLGs9WSgpO0guY2FsbChnKSxlKCksbyhwKX1mdW5j
dGlvbiBmKCl7dmFyIG49VC5vZih0aGlzLGFyZ3VtZW50cyk7ZD9jbGVhclRpbWVvdXQoZCk6KEgu
Y2FsbCh0aGlzKSxvKG4pKSxkPXNldFRpbWVvdXQoZnVuY3Rpb24oKXtkPW51bGwsYyhuKX0sNTAp
LHkoKTt2YXIgZT12fHxHby5tb3VzZSh0aGlzKTtwfHwocD10KGUpKSxyKE1hdGgucG93KDIsLjAw
MipqYSgpKSpTLmspLHUoZSxwKSxhKG4pfWZ1bmN0aW9uIGgoKXtwPW51bGx9ZnVuY3Rpb24gZygp
e3ZhciBuPVQub2YodGhpcyxhcmd1bWVudHMpLGU9R28ubW91c2UodGhpcyksaT10KGUpLHM9TWF0
aC5sb2coUy5rKS9NYXRoLkxOMjtvKG4pLHIoTWF0aC5wb3coMixHby5ldmVudC5zaGlmdEtleT9N
YXRoLmNlaWwocyktMTpNYXRoLmZsb29yKHMpKzEpKSx1KGUsaSksYShuKSxjKG4pfXZhciBwLHYs
ZCxtLHgsXyxiLHcsUz17eDowLHk6MCxrOjF9LGs9Wzk2MCw1MDBdLEU9SGEsQT0ibW91c2Vkb3du
Lnpvb20iLEM9Im1vdXNlbW92ZS56b29tIixOPSJtb3VzZXVwLnpvb20iLEw9InRvdWNoc3RhcnQu
em9vbSIsVD1NKG4sInpvb21zdGFydCIsInpvb20iLCJ6b29tZW5kIik7cmV0dXJuIG4uZXZlbnQ9
ZnVuY3Rpb24obil7bi5lYWNoKGZ1bmN0aW9uKCl7dmFyIG49VC5vZih0aGlzLGFyZ3VtZW50cyks
dD1TO0xzP0dvLnNlbGVjdCh0aGlzKS50cmFuc2l0aW9uKCkuZWFjaCgic3RhcnQuem9vbSIsZnVu
Y3Rpb24oKXtTPXRoaXMuX19jaGFydF9ffHx7eDowLHk6MCxrOjF9LG8obil9KS50d2Vlbigiem9v
bTp6b29tIixmdW5jdGlvbigpe3ZhciBlPWtbMF0scj1rWzFdLHU9ZS8yLGk9ci8yLG89R28uaW50
ZXJwb2xhdGVab29tKFsodS1TLngpL1MuaywoaS1TLnkpL1MuayxlL1Mua10sWyh1LXQueCkvdC5r
LChpLXQueSkvdC5rLGUvdC5rXSk7cmV0dXJuIGZ1bmN0aW9uKHQpe3ZhciByPW8odCksYz1lL3Jb
Ml07dGhpcy5fX2NoYXJ0X189Uz17eDp1LXJbMF0qYyx5OmktclsxXSpjLGs6Y30sYShuKX19KS5l
YWNoKCJlbmQuem9vbSIsZnVuY3Rpb24oKXtjKG4pfSk6KHRoaXMuX19jaGFydF9fPVMsbyhuKSxh
KG4pLGMobikpfSl9LG4udHJhbnNsYXRlPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVu
Z3RoPyhTPXt4Oit0WzBdLHk6K3RbMV0sazpTLmt9LGkoKSxuKTpbUy54LFMueV19LG4uc2NhbGU9
ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KFM9e3g6Uy54LHk6Uy55LGs6K3R9
LGkoKSxuKTpTLmt9LG4uc2NhbGVFeHRlbnQ9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5s
ZW5ndGg/KEU9bnVsbD09dD9IYTpbK3RbMF0sK3RbMV1dLG4pOkV9LG4uY2VudGVyPWZ1bmN0aW9u
KHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh2PXQmJlsrdFswXSwrdFsxXV0sbik6dn0sbi5z
aXplPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhrPXQmJlsrdFswXSwrdFsx
XV0sbik6a30sbi54PWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhfPXQseD10
LmNvcHkoKSxTPXt4OjAseTowLGs6MX0sbik6X30sbi55PWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1
bWVudHMubGVuZ3RoPyh3PXQsYj10LmNvcHkoKSxTPXt4OjAseTowLGs6MX0sbik6d30sR28ucmVi
aW5kKG4sVCwib24iKX07dmFyIGphLEhhPVswLDEvMF0sRmE9Im9ud2hlZWwiaW4gbmE/KGphPWZ1
bmN0aW9uKCl7cmV0dXJuLUdvLmV2ZW50LmRlbHRhWSooR28uZXZlbnQuZGVsdGFNb2RlPzEyMDox
KX0sIndoZWVsIik6Im9ubW91c2V3aGVlbCJpbiBuYT8oamE9ZnVuY3Rpb24oKXtyZXR1cm4gR28u
ZXZlbnQud2hlZWxEZWx0YX0sIm1vdXNld2hlZWwiKTooamE9ZnVuY3Rpb24oKXtyZXR1cm4tR28u
ZXZlbnQuZGV0YWlsfSwiTW96TW91c2VQaXhlbFNjcm9sbCIpO2V0LnByb3RvdHlwZS50b1N0cmlu
Zz1mdW5jdGlvbigpe3JldHVybiB0aGlzLnJnYigpKyIifSxHby5oc2w9ZnVuY3Rpb24obix0LGUp
e3JldHVybiAxPT09YXJndW1lbnRzLmxlbmd0aD9uIGluc3RhbmNlb2YgdXQ/cnQobi5oLG4ucyxu
LmwpOl90KCIiK24sYnQscnQpOnJ0KCtuLCt0LCtlKX07dmFyIE9hPXV0LnByb3RvdHlwZT1uZXcg
ZXQ7T2EuYnJpZ2h0ZXI9ZnVuY3Rpb24obil7cmV0dXJuIG49TWF0aC5wb3coLjcsYXJndW1lbnRz
Lmxlbmd0aD9uOjEpLHJ0KHRoaXMuaCx0aGlzLnMsdGhpcy5sL24pfSxPYS5kYXJrZXI9ZnVuY3Rp
b24obil7cmV0dXJuIG49TWF0aC5wb3coLjcsYXJndW1lbnRzLmxlbmd0aD9uOjEpLHJ0KHRoaXMu
aCx0aGlzLnMsbip0aGlzLmwpfSxPYS5yZ2I9ZnVuY3Rpb24oKXtyZXR1cm4gaXQodGhpcy5oLHRo
aXMucyx0aGlzLmwpfSxHby5oY2w9ZnVuY3Rpb24obix0LGUpe3JldHVybiAxPT09YXJndW1lbnRz
Lmxlbmd0aD9uIGluc3RhbmNlb2YgYXQ/b3Qobi5oLG4uYyxuLmwpOm4gaW5zdGFuY2VvZiBsdD9o
dChuLmwsbi5hLG4uYik6aHQoKG49d3QoKG49R28ucmdiKG4pKS5yLG4uZyxuLmIpKS5sLG4uYSxu
LmIpOm90KCtuLCt0LCtlKX07dmFyIElhPWF0LnByb3RvdHlwZT1uZXcgZXQ7SWEuYnJpZ2h0ZXI9
ZnVuY3Rpb24obil7cmV0dXJuIG90KHRoaXMuaCx0aGlzLmMsTWF0aC5taW4oMTAwLHRoaXMubCtZ
YSooYXJndW1lbnRzLmxlbmd0aD9uOjEpKSl9LElhLmRhcmtlcj1mdW5jdGlvbihuKXtyZXR1cm4g
b3QodGhpcy5oLHRoaXMuYyxNYXRoLm1heCgwLHRoaXMubC1ZYSooYXJndW1lbnRzLmxlbmd0aD9u
OjEpKSl9LElhLnJnYj1mdW5jdGlvbigpe3JldHVybiBjdCh0aGlzLmgsdGhpcy5jLHRoaXMubCku
cmdiKCl9LEdvLmxhYj1mdW5jdGlvbihuLHQsZSl7cmV0dXJuIDE9PT1hcmd1bWVudHMubGVuZ3Ro
P24gaW5zdGFuY2VvZiBsdD9zdChuLmwsbi5hLG4uYik6biBpbnN0YW5jZW9mIGF0P2N0KG4ubCxu
LmMsbi5oKTp3dCgobj1Hby5yZ2IobikpLnIsbi5nLG4uYik6c3QoK24sK3QsK2UpfTt2YXIgWWE9
MTgsWmE9Ljk1MDQ3LFZhPTEsJGE9MS4wODg4MyxYYT1sdC5wcm90b3R5cGU9bmV3IGV0O1hhLmJy
aWdodGVyPWZ1bmN0aW9uKG4pe3JldHVybiBzdChNYXRoLm1pbigxMDAsdGhpcy5sK1lhKihhcmd1
bWVudHMubGVuZ3RoP246MSkpLHRoaXMuYSx0aGlzLmIpfSxYYS5kYXJrZXI9ZnVuY3Rpb24obil7
cmV0dXJuIHN0KE1hdGgubWF4KDAsdGhpcy5sLVlhKihhcmd1bWVudHMubGVuZ3RoP246MSkpLHRo
aXMuYSx0aGlzLmIpfSxYYS5yZ2I9ZnVuY3Rpb24oKXtyZXR1cm4gZnQodGhpcy5sLHRoaXMuYSx0
aGlzLmIpfSxHby5yZ2I9ZnVuY3Rpb24obix0LGUpe3JldHVybiAxPT09YXJndW1lbnRzLmxlbmd0
aD9uIGluc3RhbmNlb2YgeHQ/eXQobi5yLG4uZyxuLmIpOl90KCIiK24seXQsaXQpOnl0KH5+bix+
fnQsfn5lKX07dmFyIEJhPXh0LnByb3RvdHlwZT1uZXcgZXQ7QmEuYnJpZ2h0ZXI9ZnVuY3Rpb24o
bil7bj1NYXRoLnBvdyguNyxhcmd1bWVudHMubGVuZ3RoP246MSk7dmFyIHQ9dGhpcy5yLGU9dGhp
cy5nLHI9dGhpcy5iLHU9MzA7cmV0dXJuIHR8fGV8fHI/KHQmJnU+dCYmKHQ9dSksZSYmdT5lJiYo
ZT11KSxyJiZ1PnImJihyPXUpLHl0KE1hdGgubWluKDI1NSx+fih0L24pKSxNYXRoLm1pbigyNTUs
fn4oZS9uKSksTWF0aC5taW4oMjU1LH5+KHIvbikpKSk6eXQodSx1LHUpfSxCYS5kYXJrZXI9ZnVu
Y3Rpb24obil7cmV0dXJuIG49TWF0aC5wb3coLjcsYXJndW1lbnRzLmxlbmd0aD9uOjEpLHl0KH5+
KG4qdGhpcy5yKSx+fihuKnRoaXMuZyksfn4obip0aGlzLmIpKX0sQmEuaHNsPWZ1bmN0aW9uKCl7
cmV0dXJuIGJ0KHRoaXMucix0aGlzLmcsdGhpcy5iKX0sQmEudG9TdHJpbmc9ZnVuY3Rpb24oKXty
ZXR1cm4iIyIrTXQodGhpcy5yKStNdCh0aGlzLmcpK010KHRoaXMuYil9O3ZhciBKYT1Hby5tYXAo
e2FsaWNlYmx1ZToxNTc5MjM4MyxhbnRpcXVld2hpdGU6MTY0NDQzNzUsYXF1YTo2NTUzNSxhcXVh
bWFyaW5lOjgzODg1NjQsYXp1cmU6MTU3OTQxNzUsYmVpZ2U6MTYxMTkyNjAsYmlzcXVlOjE2Nzcw
MjQ0LGJsYWNrOjAsYmxhbmNoZWRhbG1vbmQ6MTY3NzIwNDUsYmx1ZToyNTUsYmx1ZXZpb2xldDo5
MDU1MjAyLGJyb3duOjEwODI0MjM0LGJ1cmx5d29vZDoxNDU5NjIzMSxjYWRldGJsdWU6NjI2NjUy
OCxjaGFydHJldXNlOjgzODgzNTIsY2hvY29sYXRlOjEzNzg5NDcwLGNvcmFsOjE2NzQ0MjcyLGNv
cm5mbG93ZXJibHVlOjY1OTE5ODEsY29ybnNpbGs6MTY3NzUzODgsY3JpbXNvbjoxNDQyMzEwMCxj
eWFuOjY1NTM1LGRhcmtibHVlOjEzOSxkYXJrY3lhbjozNTcyMyxkYXJrZ29sZGVucm9kOjEyMDky
OTM5LGRhcmtncmF5OjExMTE5MDE3LGRhcmtncmVlbjoyNTYwMCxkYXJrZ3JleToxMTExOTAxNyxk
YXJra2hha2k6MTI0MzMyNTksZGFya21hZ2VudGE6OTEwOTY0MyxkYXJrb2xpdmVncmVlbjo1NTk3
OTk5LGRhcmtvcmFuZ2U6MTY3NDc1MjAsZGFya29yY2hpZDoxMDA0MDAxMixkYXJrcmVkOjkxMDk1
MDQsZGFya3NhbG1vbjoxNTMwODQxMCxkYXJrc2VhZ3JlZW46OTQxOTkxOSxkYXJrc2xhdGVibHVl
OjQ3MzQzNDcsZGFya3NsYXRlZ3JheTozMTAwNDk1LGRhcmtzbGF0ZWdyZXk6MzEwMDQ5NSxkYXJr
dHVycXVvaXNlOjUyOTQ1LGRhcmt2aW9sZXQ6OTY5OTUzOSxkZWVwcGluazoxNjcxNjk0NyxkZWVw
c2t5Ymx1ZTo0OTE1MSxkaW1ncmF5OjY5MDgyNjUsZGltZ3JleTo2OTA4MjY1LGRvZGdlcmJsdWU6
MjAwMzE5OSxmaXJlYnJpY2s6MTE2NzQxNDYsZmxvcmFsd2hpdGU6MTY3NzU5MjAsZm9yZXN0Z3Jl
ZW46MjI2Mzg0MixmdWNoc2lhOjE2NzExOTM1LGdhaW5zYm9ybzoxNDQ3NDQ2MCxnaG9zdHdoaXRl
OjE2MzE2NjcxLGdvbGQ6MTY3NjY3MjAsZ29sZGVucm9kOjE0MzI5MTIwLGdyYXk6ODQyMTUwNCxn
cmVlbjozMjc2OCxncmVlbnllbGxvdzoxMTQwMzA1NSxncmV5Ojg0MjE1MDQsaG9uZXlkZXc6MTU3
OTQxNjAsaG90cGluazoxNjczODc0MCxpbmRpYW5yZWQ6MTM0NTg1MjQsaW5kaWdvOjQ5MTUzMzAs
aXZvcnk6MTY3NzcyMDAsa2hha2k6MTU3ODc2NjAsbGF2ZW5kZXI6MTUxMzI0MTAsbGF2ZW5kZXJi
bHVzaDoxNjc3MzM2NSxsYXduZ3JlZW46ODE5MDk3NixsZW1vbmNoaWZmb246MTY3NzU4ODUsbGln
aHRibHVlOjExMzkzMjU0LGxpZ2h0Y29yYWw6MTU3NjE1MzYsbGlnaHRjeWFuOjE0NzQ1NTk5LGxp
Z2h0Z29sZGVucm9keWVsbG93OjE2NDQ4MjEwLGxpZ2h0Z3JheToxMzg4MjMyMyxsaWdodGdyZWVu
Ojk0OTgyNTYsbGlnaHRncmV5OjEzODgyMzIzLGxpZ2h0cGluazoxNjc1ODQ2NSxsaWdodHNhbG1v
bjoxNjc1Mjc2MixsaWdodHNlYWdyZWVuOjIxNDI4OTAsbGlnaHRza3libHVlOjg5MDAzNDYsbGln
aHRzbGF0ZWdyYXk6NzgzMzc1MyxsaWdodHNsYXRlZ3JleTo3ODMzNzUzLGxpZ2h0c3RlZWxibHVl
OjExNTg0NzM0LGxpZ2h0eWVsbG93OjE2Nzc3MTg0LGxpbWU6NjUyODAsbGltZWdyZWVuOjMzMjkz
MzAsbGluZW46MTY0NDU2NzAsbWFnZW50YToxNjcxMTkzNSxtYXJvb246ODM4ODYwOCxtZWRpdW1h
cXVhbWFyaW5lOjY3MzczMjIsbWVkaXVtYmx1ZToyMDUsbWVkaXVtb3JjaGlkOjEyMjExNjY3LG1l
ZGl1bXB1cnBsZTo5NjYyNjgzLG1lZGl1bXNlYWdyZWVuOjM5NzgwOTcsbWVkaXVtc2xhdGVibHVl
OjgwODc3OTAsbWVkaXVtc3ByaW5nZ3JlZW46NjQxNTQsbWVkaXVtdHVycXVvaXNlOjQ3NzIzMDAs
bWVkaXVtdmlvbGV0cmVkOjEzMDQ3MTczLG1pZG5pZ2h0Ymx1ZToxNjQ0OTEyLG1pbnRjcmVhbTox
NjEyMTg1MCxtaXN0eXJvc2U6MTY3NzAyNzMsbW9jY2FzaW46MTY3NzAyMjksbmF2YWpvd2hpdGU6
MTY3Njg2ODUsbmF2eToxMjgsb2xkbGFjZToxNjY0MzU1OCxvbGl2ZTo4NDIxMzc2LG9saXZlZHJh
Yjo3MDQ4NzM5LG9yYW5nZToxNjc1MzkyMCxvcmFuZ2VyZWQ6MTY3MjkzNDQsb3JjaGlkOjE0MzE1
NzM0LHBhbGVnb2xkZW5yb2Q6MTU2NTcxMzAscGFsZWdyZWVuOjEwMDI1ODgwLHBhbGV0dXJxdW9p
c2U6MTE1Mjk5NjYscGFsZXZpb2xldHJlZDoxNDM4MTIwMyxwYXBheWF3aGlwOjE2NzczMDc3LHBl
YWNocHVmZjoxNjc2NzY3MyxwZXJ1OjEzNDY4OTkxLHBpbms6MTY3NjEwMzUscGx1bToxNDUyNDYz
Nyxwb3dkZXJibHVlOjExNTkxOTEwLHB1cnBsZTo4Mzg4NzM2LHJlZDoxNjcxMTY4MCxyb3N5YnJv
d246MTIzNTc1MTkscm95YWxibHVlOjQyODY5NDUsc2FkZGxlYnJvd246OTEyNzE4NyxzYWxtb246
MTY0MTY4ODIsc2FuZHlicm93bjoxNjAzMjg2NCxzZWFncmVlbjozMDUwMzI3LHNlYXNoZWxsOjE2
Nzc0NjM4LHNpZW5uYToxMDUwNjc5NyxzaWx2ZXI6MTI2MzIyNTYsc2t5Ymx1ZTo4OTAwMzMxLHNs
YXRlYmx1ZTo2OTcwMDYxLHNsYXRlZ3JheTo3MzcyOTQ0LHNsYXRlZ3JleTo3MzcyOTQ0LHNub3c6
MTY3NzU5MzAsc3ByaW5nZ3JlZW46NjU0MDcsc3RlZWxibHVlOjQ2MjA5ODAsdGFuOjEzODA4Nzgw
LHRlYWw6MzI4OTYsdGhpc3RsZToxNDIwNDg4OCx0b21hdG86MTY3MzcwOTUsdHVycXVvaXNlOjQy
NTE4NTYsdmlvbGV0OjE1NjMxMDg2LHdoZWF0OjE2MTEzMzMxLHdoaXRlOjE2Nzc3MjE1LHdoaXRl
c21va2U6MTYxMTkyODUseWVsbG93OjE2Nzc2OTYwLHllbGxvd2dyZWVuOjEwMTQ1MDc0fSk7SmEu
Zm9yRWFjaChmdW5jdGlvbihuLHQpe0phLnNldChuLGR0KHQpKX0pLEdvLmZ1bmN0b3I9RXQsR28u
eGhyPUN0KEF0KSxHby5kc3Y9ZnVuY3Rpb24obix0KXtmdW5jdGlvbiBlKG4sZSxpKXthcmd1bWVu
dHMubGVuZ3RoPDMmJihpPWUsZT1udWxsKTt2YXIgbz1OdChuLHQsbnVsbD09ZT9yOnUoZSksaSk7
cmV0dXJuIG8ucm93PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoP28ucmVzcG9u
c2UobnVsbD09KGU9bik/cjp1KG4pKTplfSxvfWZ1bmN0aW9uIHIobil7cmV0dXJuIGUucGFyc2Uo
bi5yZXNwb25zZVRleHQpfWZ1bmN0aW9uIHUobil7cmV0dXJuIGZ1bmN0aW9uKHQpe3JldHVybiBl
LnBhcnNlKHQucmVzcG9uc2VUZXh0LG4pfX1mdW5jdGlvbiBpKHQpe3JldHVybiB0Lm1hcChvKS5q
b2luKG4pfWZ1bmN0aW9uIG8obil7cmV0dXJuIGEudGVzdChuKT8nIicrbi5yZXBsYWNlKC9cIi9n
LCciIicpKyciJzpufXZhciBhPW5ldyBSZWdFeHAoJ1siJytuKyJcbl0iKSxjPW4uY2hhckNvZGVB
dCgwKTtyZXR1cm4gZS5wYXJzZT1mdW5jdGlvbihuLHQpe3ZhciByO3JldHVybiBlLnBhcnNlUm93
cyhuLGZ1bmN0aW9uKG4sZSl7aWYocilyZXR1cm4gcihuLGUtMSk7dmFyIHU9bmV3IEZ1bmN0aW9u
KCJkIiwicmV0dXJuIHsiK24ubWFwKGZ1bmN0aW9uKG4sdCl7cmV0dXJuIEpTT04uc3RyaW5naWZ5
KG4pKyI6IGRbIit0KyJdIn0pLmpvaW4oIiwiKSsifSIpO3I9dD9mdW5jdGlvbihuLGUpe3JldHVy
biB0KHUobiksZSl9OnV9KX0sZS5wYXJzZVJvd3M9ZnVuY3Rpb24obix0KXtmdW5jdGlvbiBlKCl7
aWYobD49cylyZXR1cm4gbztpZih1KXJldHVybiB1PSExLGk7dmFyIHQ9bDtpZigzND09PW4uY2hh
ckNvZGVBdCh0KSl7Zm9yKHZhciBlPXQ7ZSsrPHM7KWlmKDM0PT09bi5jaGFyQ29kZUF0KGUpKXtp
ZigzNCE9PW4uY2hhckNvZGVBdChlKzEpKWJyZWFrOysrZX1sPWUrMjt2YXIgcj1uLmNoYXJDb2Rl
QXQoZSsxKTtyZXR1cm4gMTM9PT1yPyh1PSEwLDEwPT09bi5jaGFyQ29kZUF0KGUrMikmJisrbCk6
MTA9PT1yJiYodT0hMCksbi5zdWJzdHJpbmcodCsxLGUpLnJlcGxhY2UoLyIiL2csJyInKX1mb3Io
O3M+bDspe3ZhciByPW4uY2hhckNvZGVBdChsKyspLGE9MTtpZigxMD09PXIpdT0hMDtlbHNlIGlm
KDEzPT09cil1PSEwLDEwPT09bi5jaGFyQ29kZUF0KGwpJiYoKytsLCsrYSk7ZWxzZSBpZihyIT09
Yyljb250aW51ZTtyZXR1cm4gbi5zdWJzdHJpbmcodCxsLWEpfXJldHVybiBuLnN1YnN0cmluZyh0
KX1mb3IodmFyIHIsdSxpPXt9LG89e30sYT1bXSxzPW4ubGVuZ3RoLGw9MCxmPTA7KHI9ZSgpKSE9
PW87KXtmb3IodmFyIGg9W107ciE9PWkmJnIhPT1vOyloLnB1c2gocikscj1lKCk7KCF0fHwoaD10
KGgsZisrKSkpJiZhLnB1c2goaCl9cmV0dXJuIGF9LGUuZm9ybWF0PWZ1bmN0aW9uKHQpe2lmKEFy
cmF5LmlzQXJyYXkodFswXSkpcmV0dXJuIGUuZm9ybWF0Um93cyh0KTt2YXIgcj1uZXcgaCx1PVtd
O3JldHVybiB0LmZvckVhY2goZnVuY3Rpb24obil7Zm9yKHZhciB0IGluIG4pci5oYXModCl8fHUu
cHVzaChyLmFkZCh0KSl9KSxbdS5tYXAobykuam9pbihuKV0uY29uY2F0KHQubWFwKGZ1bmN0aW9u
KHQpe3JldHVybiB1Lm1hcChmdW5jdGlvbihuKXtyZXR1cm4gbyh0W25dKX0pLmpvaW4obil9KSku
am9pbigiXG4iKX0sZS5mb3JtYXRSb3dzPWZ1bmN0aW9uKG4pe3JldHVybiBuLm1hcChpKS5qb2lu
KCJcbiIpfSxlfSxHby5jc3Y9R28uZHN2KCIsIiwidGV4dC9jc3YiKSxHby50c3Y9R28uZHN2KCIJ
IiwidGV4dC90YWItc2VwYXJhdGVkLXZhbHVlcyIpLEdvLnRvdWNoPWZ1bmN0aW9uKG4sdCxlKXtp
Zihhcmd1bWVudHMubGVuZ3RoPDMmJihlPXQsdD14KCkuY2hhbmdlZFRvdWNoZXMpLHQpZm9yKHZh
ciByLHU9MCxpPXQubGVuZ3RoO2k+dTsrK3UpaWYoKHI9dFt1XSkuaWRlbnRpZmllcj09PWUpcmV0
dXJuIFoobixyKX07dmFyIFdhLEdhLEthLFFhLG5jLHRjPWVhW3AoZWEsInJlcXVlc3RBbmltYXRp
b25GcmFtZSIpXXx8ZnVuY3Rpb24obil7c2V0VGltZW91dChuLDE3KX07R28udGltZXI9ZnVuY3Rp
b24obix0LGUpe3ZhciByPWFyZ3VtZW50cy5sZW5ndGg7Mj5yJiYodD0wKSwzPnImJihlPURhdGUu
bm93KCkpO3ZhciB1PWUrdCxpPXtjOm4sdDp1LGY6ITEsbjpudWxsfTtHYT9HYS5uPWk6V2E9aSxH
YT1pLEthfHwoUWE9Y2xlYXJUaW1lb3V0KFFhKSxLYT0xLHRjKFR0KSl9LEdvLnRpbWVyLmZsdXNo
PWZ1bmN0aW9uKCl7cXQoKSx6dCgpfSxHby5yb3VuZD1mdW5jdGlvbihuLHQpe3JldHVybiB0P01h
dGgucm91bmQobioodD1NYXRoLnBvdygxMCx0KSkpL3Q6TWF0aC5yb3VuZChuKX07dmFyIGVjPVsi
eSIsInoiLCJhIiwiZiIsInAiLCJuIiwiXHhiNSIsIm0iLCIiLCJrIiwiTSIsIkciLCJUIiwiUCIs
IkUiLCJaIiwiWSJdLm1hcChEdCk7R28uZm9ybWF0UHJlZml4PWZ1bmN0aW9uKG4sdCl7dmFyIGU9
MDtyZXR1cm4gbiYmKDA+biYmKG4qPS0xKSx0JiYobj1Hby5yb3VuZChuLFJ0KG4sdCkpKSxlPTEr
TWF0aC5mbG9vcigxZS0xMitNYXRoLmxvZyhuKS9NYXRoLkxOMTApLGU9TWF0aC5tYXgoLTI0LE1h
dGgubWluKDI0LDMqTWF0aC5mbG9vcigoZS0xKS8zKSkpKSxlY1s4K2UvM119O3ZhciByYz0vKD86
KFtee10pPyhbPD49Xl0pKT8oWytcLSBdKT8oWyQjXSk/KDApPyhcZCspPygsKT8oXC4tP1xkKyk/
KFthLXolXSk/L2ksdWM9R28ubWFwKHtiOmZ1bmN0aW9uKG4pe3JldHVybiBuLnRvU3RyaW5nKDIp
fSxjOmZ1bmN0aW9uKG4pe3JldHVybiBTdHJpbmcuZnJvbUNoYXJDb2RlKG4pfSxvOmZ1bmN0aW9u
KG4pe3JldHVybiBuLnRvU3RyaW5nKDgpfSx4OmZ1bmN0aW9uKG4pe3JldHVybiBuLnRvU3RyaW5n
KDE2KX0sWDpmdW5jdGlvbihuKXtyZXR1cm4gbi50b1N0cmluZygxNikudG9VcHBlckNhc2UoKX0s
ZzpmdW5jdGlvbihuLHQpe3JldHVybiBuLnRvUHJlY2lzaW9uKHQpfSxlOmZ1bmN0aW9uKG4sdCl7
cmV0dXJuIG4udG9FeHBvbmVudGlhbCh0KX0sZjpmdW5jdGlvbihuLHQpe3JldHVybiBuLnRvRml4
ZWQodCl9LHI6ZnVuY3Rpb24obix0KXtyZXR1cm4obj1Hby5yb3VuZChuLFJ0KG4sdCkpKS50b0Zp
eGVkKE1hdGgubWF4KDAsTWF0aC5taW4oMjAsUnQobiooMSsxZS0xNSksdCkpKSl9fSksaWM9R28u
dGltZT17fSxvYz1EYXRlO2p0LnByb3RvdHlwZT17Z2V0RGF0ZTpmdW5jdGlvbigpe3JldHVybiB0
aGlzLl8uZ2V0VVRDRGF0ZSgpfSxnZXREYXk6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5fLmdldFVU
Q0RheSgpfSxnZXRGdWxsWWVhcjpmdW5jdGlvbigpe3JldHVybiB0aGlzLl8uZ2V0VVRDRnVsbFll
YXIoKX0sZ2V0SG91cnM6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5fLmdldFVUQ0hvdXJzKCl9LGdl
dE1pbGxpc2Vjb25kczpmdW5jdGlvbigpe3JldHVybiB0aGlzLl8uZ2V0VVRDTWlsbGlzZWNvbmRz
KCl9LGdldE1pbnV0ZXM6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5fLmdldFVUQ01pbnV0ZXMoKX0s
Z2V0TW9udGg6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5fLmdldFVUQ01vbnRoKCl9LGdldFNlY29u
ZHM6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5fLmdldFVUQ1NlY29uZHMoKX0sZ2V0VGltZTpmdW5j
dGlvbigpe3JldHVybiB0aGlzLl8uZ2V0VGltZSgpfSxnZXRUaW1lem9uZU9mZnNldDpmdW5jdGlv
bigpe3JldHVybiAwfSx2YWx1ZU9mOmZ1bmN0aW9uKCl7cmV0dXJuIHRoaXMuXy52YWx1ZU9mKCl9
LHNldERhdGU6ZnVuY3Rpb24oKXthYy5zZXRVVENEYXRlLmFwcGx5KHRoaXMuXyxhcmd1bWVudHMp
fSxzZXREYXk6ZnVuY3Rpb24oKXthYy5zZXRVVENEYXkuYXBwbHkodGhpcy5fLGFyZ3VtZW50cyl9
LHNldEZ1bGxZZWFyOmZ1bmN0aW9uKCl7YWMuc2V0VVRDRnVsbFllYXIuYXBwbHkodGhpcy5fLGFy
Z3VtZW50cyl9LHNldEhvdXJzOmZ1bmN0aW9uKCl7YWMuc2V0VVRDSG91cnMuYXBwbHkodGhpcy5f
LGFyZ3VtZW50cyl9LHNldE1pbGxpc2Vjb25kczpmdW5jdGlvbigpe2FjLnNldFVUQ01pbGxpc2Vj
b25kcy5hcHBseSh0aGlzLl8sYXJndW1lbnRzKX0sc2V0TWludXRlczpmdW5jdGlvbigpe2FjLnNl
dFVUQ01pbnV0ZXMuYXBwbHkodGhpcy5fLGFyZ3VtZW50cyl9LHNldE1vbnRoOmZ1bmN0aW9uKCl7
YWMuc2V0VVRDTW9udGguYXBwbHkodGhpcy5fLGFyZ3VtZW50cyl9LHNldFNlY29uZHM6ZnVuY3Rp
b24oKXthYy5zZXRVVENTZWNvbmRzLmFwcGx5KHRoaXMuXyxhcmd1bWVudHMpfSxzZXRUaW1lOmZ1
bmN0aW9uKCl7YWMuc2V0VGltZS5hcHBseSh0aGlzLl8sYXJndW1lbnRzKX19O3ZhciBhYz1EYXRl
LnByb3RvdHlwZTtpYy55ZWFyPUh0KGZ1bmN0aW9uKG4pe3JldHVybiBuPWljLmRheShuKSxuLnNl
dE1vbnRoKDAsMSksbn0sZnVuY3Rpb24obix0KXtuLnNldEZ1bGxZZWFyKG4uZ2V0RnVsbFllYXIo
KSt0KX0sZnVuY3Rpb24obil7cmV0dXJuIG4uZ2V0RnVsbFllYXIoKX0pLGljLnllYXJzPWljLnll
YXIucmFuZ2UsaWMueWVhcnMudXRjPWljLnllYXIudXRjLnJhbmdlLGljLmRheT1IdChmdW5jdGlv
bihuKXt2YXIgdD1uZXcgb2MoMmUzLDApO3JldHVybiB0LnNldEZ1bGxZZWFyKG4uZ2V0RnVsbFll
YXIoKSxuLmdldE1vbnRoKCksbi5nZXREYXRlKCkpLHR9LGZ1bmN0aW9uKG4sdCl7bi5zZXREYXRl
KG4uZ2V0RGF0ZSgpK3QpfSxmdW5jdGlvbihuKXtyZXR1cm4gbi5nZXREYXRlKCktMX0pLGljLmRh
eXM9aWMuZGF5LnJhbmdlLGljLmRheXMudXRjPWljLmRheS51dGMucmFuZ2UsaWMuZGF5T2ZZZWFy
PWZ1bmN0aW9uKG4pe3ZhciB0PWljLnllYXIobik7cmV0dXJuIE1hdGguZmxvb3IoKG4tdC02ZTQq
KG4uZ2V0VGltZXpvbmVPZmZzZXQoKS10LmdldFRpbWV6b25lT2Zmc2V0KCkpKS84NjRlNSl9LFsi
c3VuZGF5IiwibW9uZGF5IiwidHVlc2RheSIsIndlZG5lc2RheSIsInRodXJzZGF5IiwiZnJpZGF5
Iiwic2F0dXJkYXkiXS5mb3JFYWNoKGZ1bmN0aW9uKG4sdCl7dD03LXQ7dmFyIGU9aWNbbl09SHQo
ZnVuY3Rpb24obil7cmV0dXJuKG49aWMuZGF5KG4pKS5zZXREYXRlKG4uZ2V0RGF0ZSgpLShuLmdl
dERheSgpK3QpJTcpLG59LGZ1bmN0aW9uKG4sdCl7bi5zZXREYXRlKG4uZ2V0RGF0ZSgpKzcqTWF0
aC5mbG9vcih0KSl9LGZ1bmN0aW9uKG4pe3ZhciBlPWljLnllYXIobikuZ2V0RGF5KCk7cmV0dXJu
IE1hdGguZmxvb3IoKGljLmRheU9mWWVhcihuKSsoZSt0KSU3KS83KS0oZSE9PXQpfSk7aWNbbisi
cyJdPWUucmFuZ2UsaWNbbisicyJdLnV0Yz1lLnV0Yy5yYW5nZSxpY1tuKyJPZlllYXIiXT1mdW5j
dGlvbihuKXt2YXIgZT1pYy55ZWFyKG4pLmdldERheSgpO3JldHVybiBNYXRoLmZsb29yKChpYy5k
YXlPZlllYXIobikrKGUrdCklNykvNyl9fSksaWMud2Vlaz1pYy5zdW5kYXksaWMud2Vla3M9aWMu
c3VuZGF5LnJhbmdlLGljLndlZWtzLnV0Yz1pYy5zdW5kYXkudXRjLnJhbmdlLGljLndlZWtPZlll
YXI9aWMuc3VuZGF5T2ZZZWFyO3ZhciBjYz17Ii0iOiIiLF86IiAiLDA6IjAifSxzYz0vXlxzKlxk
Ky8sbGM9L14lLztHby5sb2NhbGU9ZnVuY3Rpb24obil7cmV0dXJue251bWJlckZvcm1hdDpQdChu
KSx0aW1lRm9ybWF0Ok90KG4pfX07dmFyIGZjPUdvLmxvY2FsZSh7ZGVjaW1hbDoiLiIsdGhvdXNh
bmRzOiIsIixncm91cGluZzpbM10sY3VycmVuY3k6WyIkIiwiIl0sZGF0ZVRpbWU6IiVhICViICVl
ICVYICVZIixkYXRlOiIlbS8lZC8lWSIsdGltZToiJUg6JU06JVMiLHBlcmlvZHM6WyJBTSIsIlBN
Il0sZGF5czpbIlN1bmRheSIsIk1vbmRheSIsIlR1ZXNkYXkiLCJXZWRuZXNkYXkiLCJUaHVyc2Rh
eSIsIkZyaWRheSIsIlNhdHVyZGF5Il0sc2hvcnREYXlzOlsiU3VuIiwiTW9uIiwiVHVlIiwiV2Vk
IiwiVGh1IiwiRnJpIiwiU2F0Il0sbW9udGhzOlsiSmFudWFyeSIsIkZlYnJ1YXJ5IiwiTWFyY2gi
LCJBcHJpbCIsIk1heSIsIkp1bmUiLCJKdWx5IiwiQXVndXN0IiwiU2VwdGVtYmVyIiwiT2N0b2Jl
ciIsIk5vdmVtYmVyIiwiRGVjZW1iZXIiXSxzaG9ydE1vbnRoczpbIkphbiIsIkZlYiIsIk1hciIs
IkFwciIsIk1heSIsIkp1biIsIkp1bCIsIkF1ZyIsIlNlcCIsIk9jdCIsIk5vdiIsIkRlYyJdfSk7
R28uZm9ybWF0PWZjLm51bWJlckZvcm1hdCxHby5nZW89e30sY2UucHJvdG90eXBlPXtzOjAsdDow
LGFkZDpmdW5jdGlvbihuKXtzZShuLHRoaXMudCxoYyksc2UoaGMucyx0aGlzLnMsdGhpcyksdGhp
cy5zP3RoaXMudCs9aGMudDp0aGlzLnM9aGMudH0scmVzZXQ6ZnVuY3Rpb24oKXt0aGlzLnM9dGhp
cy50PTB9LHZhbHVlT2Y6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5zfX07dmFyIGhjPW5ldyBjZTtH
by5nZW8uc3RyZWFtPWZ1bmN0aW9uKG4sdCl7biYmZ2MuaGFzT3duUHJvcGVydHkobi50eXBlKT9n
Y1tuLnR5cGVdKG4sdCk6bGUobix0KX07dmFyIGdjPXtGZWF0dXJlOmZ1bmN0aW9uKG4sdCl7bGUo
bi5nZW9tZXRyeSx0KX0sRmVhdHVyZUNvbGxlY3Rpb246ZnVuY3Rpb24obix0KXtmb3IodmFyIGU9
bi5mZWF0dXJlcyxyPS0xLHU9ZS5sZW5ndGg7KytyPHU7KWxlKGVbcl0uZ2VvbWV0cnksdCl9fSxw
Yz17U3BoZXJlOmZ1bmN0aW9uKG4sdCl7dC5zcGhlcmUoKX0sUG9pbnQ6ZnVuY3Rpb24obix0KXtu
PW4uY29vcmRpbmF0ZXMsdC5wb2ludChuWzBdLG5bMV0sblsyXSl9LE11bHRpUG9pbnQ6ZnVuY3Rp
b24obix0KXtmb3IodmFyIGU9bi5jb29yZGluYXRlcyxyPS0xLHU9ZS5sZW5ndGg7KytyPHU7KW49
ZVtyXSx0LnBvaW50KG5bMF0sblsxXSxuWzJdKX0sTGluZVN0cmluZzpmdW5jdGlvbihuLHQpe2Zl
KG4uY29vcmRpbmF0ZXMsdCwwKX0sTXVsdGlMaW5lU3RyaW5nOmZ1bmN0aW9uKG4sdCl7Zm9yKHZh
ciBlPW4uY29vcmRpbmF0ZXMscj0tMSx1PWUubGVuZ3RoOysrcjx1OylmZShlW3JdLHQsMCl9LFBv
bHlnb246ZnVuY3Rpb24obix0KXtoZShuLmNvb3JkaW5hdGVzLHQpfSxNdWx0aVBvbHlnb246ZnVu
Y3Rpb24obix0KXtmb3IodmFyIGU9bi5jb29yZGluYXRlcyxyPS0xLHU9ZS5sZW5ndGg7KytyPHU7
KWhlKGVbcl0sdCl9LEdlb21ldHJ5Q29sbGVjdGlvbjpmdW5jdGlvbihuLHQpe2Zvcih2YXIgZT1u
Lmdlb21ldHJpZXMscj0tMSx1PWUubGVuZ3RoOysrcjx1OylsZShlW3JdLHQpfX07R28uZ2VvLmFy
ZWE9ZnVuY3Rpb24obil7cmV0dXJuIHZjPTAsR28uZ2VvLnN0cmVhbShuLG1jKSx2Y307dmFyIHZj
LGRjPW5ldyBjZSxtYz17c3BoZXJlOmZ1bmN0aW9uKCl7dmMrPTQqQ2F9LHBvaW50OnYsbGluZVN0
YXJ0OnYsbGluZUVuZDp2LHBvbHlnb25TdGFydDpmdW5jdGlvbigpe2RjLnJlc2V0KCksbWMubGlu
ZVN0YXJ0PWdlfSxwb2x5Z29uRW5kOmZ1bmN0aW9uKCl7dmFyIG49MipkYzt2Yys9MD5uPzQqQ2Er
bjpuLG1jLmxpbmVTdGFydD1tYy5saW5lRW5kPW1jLnBvaW50PXZ9fTtHby5nZW8uYm91bmRzPWZ1
bmN0aW9uKCl7ZnVuY3Rpb24gbihuLHQpe3gucHVzaChNPVtsPW4saD1uXSksZj50JiYoZj10KSx0
PmcmJihnPXQpfWZ1bmN0aW9uIHQodCxlKXt2YXIgcj1wZShbdCp6YSxlKnphXSk7aWYobSl7dmFy
IHU9ZGUobSxyKSxpPVt1WzFdLC11WzBdLDBdLG89ZGUoaSx1KTt4ZShvKSxvPU1lKG8pO3ZhciBj
PXQtcCxzPWM+MD8xOi0xLHY9b1swXSpSYSpzLGQ9ZmEoYyk+MTgwO2lmKGReKHY+cypwJiZzKnQ+
dikpe3ZhciB5PW9bMV0qUmE7eT5nJiYoZz15KX1lbHNlIGlmKHY9KHYrMzYwKSUzNjAtMTgwLGRe
KHY+cypwJiZzKnQ+dikpe3ZhciB5PS1vWzFdKlJhO2Y+eSYmKGY9eSl9ZWxzZSBmPmUmJihmPWUp
LGU+ZyYmKGc9ZSk7ZD9wPnQ/YShsLHQpPmEobCxoKSYmKGg9dCk6YSh0LGgpPmEobCxoKSYmKGw9
dCk6aD49bD8obD50JiYobD10KSx0PmgmJihoPXQpKTp0PnA/YShsLHQpPmEobCxoKSYmKGg9dCk6
YSh0LGgpPmEobCxoKSYmKGw9dCl9ZWxzZSBuKHQsZSk7bT1yLHA9dH1mdW5jdGlvbiBlKCl7Xy5w
b2ludD10fWZ1bmN0aW9uIHIoKXtNWzBdPWwsTVsxXT1oLF8ucG9pbnQ9bixtPW51bGx9ZnVuY3Rp
b24gdShuLGUpe2lmKG0pe3ZhciByPW4tcDt5Kz1mYShyKT4xODA/cisocj4wPzM2MDotMzYwKTpy
fWVsc2Ugdj1uLGQ9ZTttYy5wb2ludChuLGUpLHQobixlKX1mdW5jdGlvbiBpKCl7bWMubGluZVN0
YXJ0KCl9ZnVuY3Rpb24gbygpe3UodixkKSxtYy5saW5lRW5kKCksZmEoeSk+VGEmJihsPS0oaD0x
ODApKSxNWzBdPWwsTVsxXT1oLG09bnVsbH1mdW5jdGlvbiBhKG4sdCl7cmV0dXJuKHQtPW4pPDA/
dCszNjA6dH1mdW5jdGlvbiBjKG4sdCl7cmV0dXJuIG5bMF0tdFswXX1mdW5jdGlvbiBzKG4sdCl7
cmV0dXJuIHRbMF08PXRbMV0/dFswXTw9biYmbjw9dFsxXTpuPHRbMF18fHRbMV08bn12YXIgbCxm
LGgsZyxwLHYsZCxtLHkseCxNLF89e3BvaW50Om4sbGluZVN0YXJ0OmUsbGluZUVuZDpyLHBvbHln
b25TdGFydDpmdW5jdGlvbigpe18ucG9pbnQ9dSxfLmxpbmVTdGFydD1pLF8ubGluZUVuZD1vLHk9
MCxtYy5wb2x5Z29uU3RhcnQoKX0scG9seWdvbkVuZDpmdW5jdGlvbigpe21jLnBvbHlnb25FbmQo
KSxfLnBvaW50PW4sXy5saW5lU3RhcnQ9ZSxfLmxpbmVFbmQ9ciwwPmRjPyhsPS0oaD0xODApLGY9
LShnPTkwKSk6eT5UYT9nPTkwOi1UYT55JiYoZj0tOTApLE1bMF09bCxNWzFdPWh9fTtyZXR1cm4g
ZnVuY3Rpb24obil7Zz1oPS0obD1mPTEvMCkseD1bXSxHby5nZW8uc3RyZWFtKG4sXyk7dmFyIHQ9
eC5sZW5ndGg7aWYodCl7eC5zb3J0KGMpO2Zvcih2YXIgZSxyPTEsdT14WzBdLGk9W3VdO3Q+cjsr
K3IpZT14W3JdLHMoZVswXSx1KXx8cyhlWzFdLHUpPyhhKHVbMF0sZVsxXSk+YSh1WzBdLHVbMV0p
JiYodVsxXT1lWzFdKSxhKGVbMF0sdVsxXSk+YSh1WzBdLHVbMV0pJiYodVswXT1lWzBdKSk6aS5w
dXNoKHU9ZSk7CmZvcih2YXIgbyxlLHA9LTEvMCx0PWkubGVuZ3RoLTEscj0wLHU9aVt0XTt0Pj1y
O3U9ZSwrK3IpZT1pW3JdLChvPWEodVsxXSxlWzBdKSk+cCYmKHA9byxsPWVbMF0saD11WzFdKX1y
ZXR1cm4geD1NPW51bGwsMS8wPT09bHx8MS8wPT09Zj9bWzAvMCwwLzBdLFswLzAsMC8wXV06W1ts
LGZdLFtoLGddXX19KCksR28uZ2VvLmNlbnRyb2lkPWZ1bmN0aW9uKG4pe3ljPXhjPU1jPV9jPWJj
PXdjPVNjPWtjPUVjPUFjPUNjPTAsR28uZ2VvLnN0cmVhbShuLE5jKTt2YXIgdD1FYyxlPUFjLHI9
Q2MsdT10KnQrZSplK3IqcjtyZXR1cm4gcWE+dSYmKHQ9d2MsZT1TYyxyPWtjLFRhPnhjJiYodD1N
YyxlPV9jLHI9YmMpLHU9dCp0K2UqZStyKnIscWE+dSk/WzAvMCwwLzBdOltNYXRoLmF0YW4yKGUs
dCkqUmEsRyhyL01hdGguc3FydCh1KSkqUmFdfTt2YXIgeWMseGMsTWMsX2MsYmMsd2MsU2Msa2Ms
RWMsQWMsQ2MsTmM9e3NwaGVyZTp2LHBvaW50OmJlLGxpbmVTdGFydDpTZSxsaW5lRW5kOmtlLHBv
bHlnb25TdGFydDpmdW5jdGlvbigpe05jLmxpbmVTdGFydD1FZX0scG9seWdvbkVuZDpmdW5jdGlv
bigpe05jLmxpbmVTdGFydD1TZX19LExjPVRlKEFlLFBlLGplLFstQ2EsLUNhLzJdKSxUYz0xZTk7
R28uZ2VvLmNsaXBFeHRlbnQ9ZnVuY3Rpb24oKXt2YXIgbix0LGUscix1LGksbz17c3RyZWFtOmZ1
bmN0aW9uKG4pe3JldHVybiB1JiYodS52YWxpZD0hMSksdT1pKG4pLHUudmFsaWQ9ITAsdX0sZXh0
ZW50OmZ1bmN0aW9uKGEpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhpPU9lKG49K2FbMF1bMF0s
dD0rYVswXVsxXSxlPSthWzFdWzBdLHI9K2FbMV1bMV0pLHUmJih1LnZhbGlkPSExLHU9bnVsbCks
byk6W1tuLHRdLFtlLHJdXX19O3JldHVybiBvLmV4dGVudChbWzAsMF0sWzk2MCw1MDBdXSl9LChH
by5nZW8uY29uaWNFcXVhbEFyZWE9ZnVuY3Rpb24oKXtyZXR1cm4gWWUoWmUpfSkucmF3PVplLEdv
Lmdlby5hbGJlcnM9ZnVuY3Rpb24oKXtyZXR1cm4gR28uZ2VvLmNvbmljRXF1YWxBcmVhKCkucm90
YXRlKFs5NiwwXSkuY2VudGVyKFstLjYsMzguN10pLnBhcmFsbGVscyhbMjkuNSw0NS41XSkuc2Nh
bGUoMTA3MCl9LEdvLmdlby5hbGJlcnNVc2E9ZnVuY3Rpb24oKXtmdW5jdGlvbiBuKG4pe3ZhciBp
PW5bMF0sbz1uWzFdO3JldHVybiB0PW51bGwsZShpLG8pLHR8fChyKGksbyksdCl8fHUoaSxvKSx0
fXZhciB0LGUscix1LGk9R28uZ2VvLmFsYmVycygpLG89R28uZ2VvLmNvbmljRXF1YWxBcmVhKCku
cm90YXRlKFsxNTQsMF0pLmNlbnRlcihbLTIsNTguNV0pLnBhcmFsbGVscyhbNTUsNjVdKSxhPUdv
Lmdlby5jb25pY0VxdWFsQXJlYSgpLnJvdGF0ZShbMTU3LDBdKS5jZW50ZXIoWy0zLDE5LjldKS5w
YXJhbGxlbHMoWzgsMThdKSxjPXtwb2ludDpmdW5jdGlvbihuLGUpe3Q9W24sZV19fTtyZXR1cm4g
bi5pbnZlcnQ9ZnVuY3Rpb24obil7dmFyIHQ9aS5zY2FsZSgpLGU9aS50cmFuc2xhdGUoKSxyPShu
WzBdLWVbMF0pL3QsdT0oblsxXS1lWzFdKS90O3JldHVybih1Pj0uMTImJi4yMzQ+dSYmcj49LS40
MjUmJi0uMjE0PnI/bzp1Pj0uMTY2JiYuMjM0PnUmJnI+PS0uMjE0JiYtLjExNT5yP2E6aSkuaW52
ZXJ0KG4pfSxuLnN0cmVhbT1mdW5jdGlvbihuKXt2YXIgdD1pLnN0cmVhbShuKSxlPW8uc3RyZWFt
KG4pLHI9YS5zdHJlYW0obik7cmV0dXJue3BvaW50OmZ1bmN0aW9uKG4sdSl7dC5wb2ludChuLHUp
LGUucG9pbnQobix1KSxyLnBvaW50KG4sdSl9LHNwaGVyZTpmdW5jdGlvbigpe3Quc3BoZXJlKCks
ZS5zcGhlcmUoKSxyLnNwaGVyZSgpfSxsaW5lU3RhcnQ6ZnVuY3Rpb24oKXt0LmxpbmVTdGFydCgp
LGUubGluZVN0YXJ0KCksci5saW5lU3RhcnQoKX0sbGluZUVuZDpmdW5jdGlvbigpe3QubGluZUVu
ZCgpLGUubGluZUVuZCgpLHIubGluZUVuZCgpfSxwb2x5Z29uU3RhcnQ6ZnVuY3Rpb24oKXt0LnBv
bHlnb25TdGFydCgpLGUucG9seWdvblN0YXJ0KCksci5wb2x5Z29uU3RhcnQoKX0scG9seWdvbkVu
ZDpmdW5jdGlvbigpe3QucG9seWdvbkVuZCgpLGUucG9seWdvbkVuZCgpLHIucG9seWdvbkVuZCgp
fX19LG4ucHJlY2lzaW9uPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhpLnBy
ZWNpc2lvbih0KSxvLnByZWNpc2lvbih0KSxhLnByZWNpc2lvbih0KSxuKTppLnByZWNpc2lvbigp
fSxuLnNjYWxlPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhpLnNjYWxlKHQp
LG8uc2NhbGUoLjM1KnQpLGEuc2NhbGUodCksbi50cmFuc2xhdGUoaS50cmFuc2xhdGUoKSkpOmku
c2NhbGUoKX0sbi50cmFuc2xhdGU9ZnVuY3Rpb24odCl7aWYoIWFyZ3VtZW50cy5sZW5ndGgpcmV0
dXJuIGkudHJhbnNsYXRlKCk7dmFyIHM9aS5zY2FsZSgpLGw9K3RbMF0sZj0rdFsxXTtyZXR1cm4g
ZT1pLnRyYW5zbGF0ZSh0KS5jbGlwRXh0ZW50KFtbbC0uNDU1KnMsZi0uMjM4KnNdLFtsKy40NTUq
cyxmKy4yMzgqc11dKS5zdHJlYW0oYykucG9pbnQscj1vLnRyYW5zbGF0ZShbbC0uMzA3KnMsZisu
MjAxKnNdKS5jbGlwRXh0ZW50KFtbbC0uNDI1KnMrVGEsZisuMTIqcytUYV0sW2wtLjIxNCpzLVRh
LGYrLjIzNCpzLVRhXV0pLnN0cmVhbShjKS5wb2ludCx1PWEudHJhbnNsYXRlKFtsLS4yMDUqcyxm
Ky4yMTIqc10pLmNsaXBFeHRlbnQoW1tsLS4yMTQqcytUYSxmKy4xNjYqcytUYV0sW2wtLjExNSpz
LVRhLGYrLjIzNCpzLVRhXV0pLnN0cmVhbShjKS5wb2ludCxufSxuLnNjYWxlKDEwNzApfTt2YXIg
cWMsemMsUmMsRGMsUGMsVWMsamM9e3BvaW50OnYsbGluZVN0YXJ0OnYsbGluZUVuZDp2LHBvbHln
b25TdGFydDpmdW5jdGlvbigpe3pjPTAsamMubGluZVN0YXJ0PVZlfSxwb2x5Z29uRW5kOmZ1bmN0
aW9uKCl7amMubGluZVN0YXJ0PWpjLmxpbmVFbmQ9amMucG9pbnQ9dixxYys9ZmEoemMvMil9fSxI
Yz17cG9pbnQ6JGUsbGluZVN0YXJ0OnYsbGluZUVuZDp2LHBvbHlnb25TdGFydDp2LHBvbHlnb25F
bmQ6dn0sRmM9e3BvaW50OkplLGxpbmVTdGFydDpXZSxsaW5lRW5kOkdlLHBvbHlnb25TdGFydDpm
dW5jdGlvbigpe0ZjLmxpbmVTdGFydD1LZX0scG9seWdvbkVuZDpmdW5jdGlvbigpe0ZjLnBvaW50
PUplLEZjLmxpbmVTdGFydD1XZSxGYy5saW5lRW5kPUdlfX07R28uZ2VvLnBhdGg9ZnVuY3Rpb24o
KXtmdW5jdGlvbiBuKG4pe3JldHVybiBuJiYoImZ1bmN0aW9uIj09dHlwZW9mIGEmJmkucG9pbnRS
YWRpdXMoK2EuYXBwbHkodGhpcyxhcmd1bWVudHMpKSxvJiZvLnZhbGlkfHwobz11KGkpKSxHby5n
ZW8uc3RyZWFtKG4sbykpLGkucmVzdWx0KCl9ZnVuY3Rpb24gdCgpe3JldHVybiBvPW51bGwsbn12
YXIgZSxyLHUsaSxvLGE9NC41O3JldHVybiBuLmFyZWE9ZnVuY3Rpb24obil7cmV0dXJuIHFjPTAs
R28uZ2VvLnN0cmVhbShuLHUoamMpKSxxY30sbi5jZW50cm9pZD1mdW5jdGlvbihuKXtyZXR1cm4g
TWM9X2M9YmM9d2M9U2M9a2M9RWM9QWM9Q2M9MCxHby5nZW8uc3RyZWFtKG4sdShGYykpLENjP1tF
Yy9DYyxBYy9DY106a2M/W3djL2tjLFNjL2tjXTpiYz9bTWMvYmMsX2MvYmNdOlswLzAsMC8wXX0s
bi5ib3VuZHM9ZnVuY3Rpb24obil7cmV0dXJuIFBjPVVjPS0oUmM9RGM9MS8wKSxHby5nZW8uc3Ry
ZWFtKG4sdShIYykpLFtbUmMsRGNdLFtQYyxVY11dfSxuLnByb2plY3Rpb249ZnVuY3Rpb24obil7
cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHU9KGU9bik/bi5zdHJlYW18fHRyKG4pOkF0LHQoKSk6
ZX0sbi5jb250ZXh0PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhpPW51bGw9
PShyPW4pP25ldyBYZTpuZXcgUWUobiksImZ1bmN0aW9uIiE9dHlwZW9mIGEmJmkucG9pbnRSYWRp
dXMoYSksdCgpKTpyfSxuLnBvaW50UmFkaXVzPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMu
bGVuZ3RoPyhhPSJmdW5jdGlvbiI9PXR5cGVvZiB0P3Q6KGkucG9pbnRSYWRpdXMoK3QpLCt0KSxu
KTphfSxuLnByb2plY3Rpb24oR28uZ2VvLmFsYmVyc1VzYSgpKS5jb250ZXh0KG51bGwpfSxHby5n
ZW8udHJhbnNmb3JtPWZ1bmN0aW9uKG4pe3JldHVybntzdHJlYW06ZnVuY3Rpb24odCl7dmFyIGU9
bmV3IGVyKHQpO2Zvcih2YXIgciBpbiBuKWVbcl09bltyXTtyZXR1cm4gZX19fSxlci5wcm90b3R5
cGU9e3BvaW50OmZ1bmN0aW9uKG4sdCl7dGhpcy5zdHJlYW0ucG9pbnQobix0KX0sc3BoZXJlOmZ1
bmN0aW9uKCl7dGhpcy5zdHJlYW0uc3BoZXJlKCl9LGxpbmVTdGFydDpmdW5jdGlvbigpe3RoaXMu
c3RyZWFtLmxpbmVTdGFydCgpfSxsaW5lRW5kOmZ1bmN0aW9uKCl7dGhpcy5zdHJlYW0ubGluZUVu
ZCgpfSxwb2x5Z29uU3RhcnQ6ZnVuY3Rpb24oKXt0aGlzLnN0cmVhbS5wb2x5Z29uU3RhcnQoKX0s
cG9seWdvbkVuZDpmdW5jdGlvbigpe3RoaXMuc3RyZWFtLnBvbHlnb25FbmQoKX19LEdvLmdlby5w
cm9qZWN0aW9uPXVyLEdvLmdlby5wcm9qZWN0aW9uTXV0YXRvcj1pciwoR28uZ2VvLmVxdWlyZWN0
YW5ndWxhcj1mdW5jdGlvbigpe3JldHVybiB1cihhcil9KS5yYXc9YXIuaW52ZXJ0PWFyLEdvLmdl
by5yb3RhdGlvbj1mdW5jdGlvbihuKXtmdW5jdGlvbiB0KHQpe3JldHVybiB0PW4odFswXSp6YSx0
WzFdKnphKSx0WzBdKj1SYSx0WzFdKj1SYSx0fXJldHVybiBuPXNyKG5bMF0lMzYwKnphLG5bMV0q
emEsbi5sZW5ndGg+Mj9uWzJdKnphOjApLHQuaW52ZXJ0PWZ1bmN0aW9uKHQpe3JldHVybiB0PW4u
aW52ZXJ0KHRbMF0qemEsdFsxXSp6YSksdFswXSo9UmEsdFsxXSo9UmEsdH0sdH0sY3IuaW52ZXJ0
PWFyLEdvLmdlby5jaXJjbGU9ZnVuY3Rpb24oKXtmdW5jdGlvbiBuKCl7dmFyIG49ImZ1bmN0aW9u
Ij09dHlwZW9mIHI/ci5hcHBseSh0aGlzLGFyZ3VtZW50cyk6cix0PXNyKC1uWzBdKnphLC1uWzFd
KnphLDApLmludmVydCx1PVtdO3JldHVybiBlKG51bGwsbnVsbCwxLHtwb2ludDpmdW5jdGlvbihu
LGUpe3UucHVzaChuPXQobixlKSksblswXSo9UmEsblsxXSo9UmF9fSkse3R5cGU6IlBvbHlnb24i
LGNvb3JkaW5hdGVzOlt1XX19dmFyIHQsZSxyPVswLDBdLHU9NjtyZXR1cm4gbi5vcmlnaW49ZnVu
Y3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHI9dCxuKTpyfSxuLmFuZ2xlPWZ1bmN0
aW9uKHIpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhlPWdyKCh0PStyKSp6YSx1KnphKSxuKTp0
fSxuLnByZWNpc2lvbj1mdW5jdGlvbihyKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZT1ncih0
KnphLCh1PStyKSp6YSksbik6dX0sbi5hbmdsZSg5MCl9LEdvLmdlby5kaXN0YW5jZT1mdW5jdGlv
bihuLHQpe3ZhciBlLHI9KHRbMF0tblswXSkqemEsdT1uWzFdKnphLGk9dFsxXSp6YSxvPU1hdGgu
c2luKHIpLGE9TWF0aC5jb3MociksYz1NYXRoLnNpbih1KSxzPU1hdGguY29zKHUpLGw9TWF0aC5z
aW4oaSksZj1NYXRoLmNvcyhpKTtyZXR1cm4gTWF0aC5hdGFuMihNYXRoLnNxcnQoKGU9ZipvKSpl
KyhlPXMqbC1jKmYqYSkqZSksYypsK3MqZiphKX0sR28uZ2VvLmdyYXRpY3VsZT1mdW5jdGlvbigp
e2Z1bmN0aW9uIG4oKXtyZXR1cm57dHlwZToiTXVsdGlMaW5lU3RyaW5nIixjb29yZGluYXRlczp0
KCl9fWZ1bmN0aW9uIHQoKXtyZXR1cm4gR28ucmFuZ2UoTWF0aC5jZWlsKGkvZCkqZCx1LGQpLm1h
cChoKS5jb25jYXQoR28ucmFuZ2UoTWF0aC5jZWlsKHMvbSkqbSxjLG0pLm1hcChnKSkuY29uY2F0
KEdvLnJhbmdlKE1hdGguY2VpbChyL3ApKnAsZSxwKS5maWx0ZXIoZnVuY3Rpb24obil7cmV0dXJu
IGZhKG4lZCk+VGF9KS5tYXAobCkpLmNvbmNhdChHby5yYW5nZShNYXRoLmNlaWwoYS92KSp2LG8s
dikuZmlsdGVyKGZ1bmN0aW9uKG4pe3JldHVybiBmYShuJW0pPlRhfSkubWFwKGYpKX12YXIgZSxy
LHUsaSxvLGEsYyxzLGwsZixoLGcscD0xMCx2PXAsZD05MCxtPTM2MCx5PTIuNTtyZXR1cm4gbi5s
aW5lcz1mdW5jdGlvbigpe3JldHVybiB0KCkubWFwKGZ1bmN0aW9uKG4pe3JldHVybnt0eXBlOiJM
aW5lU3RyaW5nIixjb29yZGluYXRlczpufX0pfSxuLm91dGxpbmU9ZnVuY3Rpb24oKXtyZXR1cm57
dHlwZToiUG9seWdvbiIsY29vcmRpbmF0ZXM6W2goaSkuY29uY2F0KGcoYykuc2xpY2UoMSksaCh1
KS5yZXZlcnNlKCkuc2xpY2UoMSksZyhzKS5yZXZlcnNlKCkuc2xpY2UoMSkpXX19LG4uZXh0ZW50
PWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoP24ubWFqb3JFeHRlbnQodCkubWlu
b3JFeHRlbnQodCk6bi5taW5vckV4dGVudCgpfSxuLm1ham9yRXh0ZW50PWZ1bmN0aW9uKHQpe3Jl
dHVybiBhcmd1bWVudHMubGVuZ3RoPyhpPSt0WzBdWzBdLHU9K3RbMV1bMF0scz0rdFswXVsxXSxj
PSt0WzFdWzFdLGk+dSYmKHQ9aSxpPXUsdT10KSxzPmMmJih0PXMscz1jLGM9dCksbi5wcmVjaXNp
b24oeSkpOltbaSxzXSxbdSxjXV19LG4ubWlub3JFeHRlbnQ9ZnVuY3Rpb24odCl7cmV0dXJuIGFy
Z3VtZW50cy5sZW5ndGg/KHI9K3RbMF1bMF0sZT0rdFsxXVswXSxhPSt0WzBdWzFdLG89K3RbMV1b
MV0scj5lJiYodD1yLHI9ZSxlPXQpLGE+byYmKHQ9YSxhPW8sbz10KSxuLnByZWNpc2lvbih5KSk6
W1tyLGFdLFtlLG9dXX0sbi5zdGVwPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3Ro
P24ubWFqb3JTdGVwKHQpLm1pbm9yU3RlcCh0KTpuLm1pbm9yU3RlcCgpfSxuLm1ham9yU3RlcD1m
dW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZD0rdFswXSxtPSt0WzFdLG4pOltk
LG1dfSxuLm1pbm9yU3RlcD1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8ocD0r
dFswXSx2PSt0WzFdLG4pOltwLHZdfSxuLnByZWNpc2lvbj1mdW5jdGlvbih0KXtyZXR1cm4gYXJn
dW1lbnRzLmxlbmd0aD8oeT0rdCxsPXZyKGEsbyw5MCksZj1kcihyLGUseSksaD12cihzLGMsOTAp
LGc9ZHIoaSx1LHkpLG4pOnl9LG4ubWFqb3JFeHRlbnQoW1stMTgwLC05MCtUYV0sWzE4MCw5MC1U
YV1dKS5taW5vckV4dGVudChbWy0xODAsLTgwLVRhXSxbMTgwLDgwK1RhXV0pfSxHby5nZW8uZ3Jl
YXRBcmM9ZnVuY3Rpb24oKXtmdW5jdGlvbiBuKCl7cmV0dXJue3R5cGU6IkxpbmVTdHJpbmciLGNv
b3JkaW5hdGVzOlt0fHxyLmFwcGx5KHRoaXMsYXJndW1lbnRzKSxlfHx1LmFwcGx5KHRoaXMsYXJn
dW1lbnRzKV19fXZhciB0LGUscj1tcix1PXlyO3JldHVybiBuLmRpc3RhbmNlPWZ1bmN0aW9uKCl7
cmV0dXJuIEdvLmdlby5kaXN0YW5jZSh0fHxyLmFwcGx5KHRoaXMsYXJndW1lbnRzKSxlfHx1LmFw
cGx5KHRoaXMsYXJndW1lbnRzKSl9LG4uc291cmNlPWZ1bmN0aW9uKGUpe3JldHVybiBhcmd1bWVu
dHMubGVuZ3RoPyhyPWUsdD0iZnVuY3Rpb24iPT10eXBlb2YgZT9udWxsOmUsbik6cn0sbi50YXJn
ZXQ9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHU9dCxlPSJmdW5jdGlvbiI9
PXR5cGVvZiB0P251bGw6dCxuKTp1fSxuLnByZWNpc2lvbj1mdW5jdGlvbigpe3JldHVybiBhcmd1
bWVudHMubGVuZ3RoP246MH0sbn0sR28uZ2VvLmludGVycG9sYXRlPWZ1bmN0aW9uKG4sdCl7cmV0
dXJuIHhyKG5bMF0qemEsblsxXSp6YSx0WzBdKnphLHRbMV0qemEpfSxHby5nZW8ubGVuZ3RoPWZ1
bmN0aW9uKG4pe3JldHVybiBPYz0wLEdvLmdlby5zdHJlYW0obixJYyksT2N9O3ZhciBPYyxJYz17
c3BoZXJlOnYscG9pbnQ6dixsaW5lU3RhcnQ6TXIsbGluZUVuZDp2LHBvbHlnb25TdGFydDp2LHBv
bHlnb25FbmQ6dn0sWWM9X3IoZnVuY3Rpb24obil7cmV0dXJuIE1hdGguc3FydCgyLygxK24pKX0s
ZnVuY3Rpb24obil7cmV0dXJuIDIqTWF0aC5hc2luKG4vMil9KTsoR28uZ2VvLmF6aW11dGhhbEVx
dWFsQXJlYT1mdW5jdGlvbigpe3JldHVybiB1cihZYyl9KS5yYXc9WWM7dmFyIFpjPV9yKGZ1bmN0
aW9uKG4pe3ZhciB0PU1hdGguYWNvcyhuKTtyZXR1cm4gdCYmdC9NYXRoLnNpbih0KX0sQXQpOyhH
by5nZW8uYXppbXV0aGFsRXF1aWRpc3RhbnQ9ZnVuY3Rpb24oKXtyZXR1cm4gdXIoWmMpfSkucmF3
PVpjLChHby5nZW8uY29uaWNDb25mb3JtYWw9ZnVuY3Rpb24oKXtyZXR1cm4gWWUoYnIpfSkucmF3
PWJyLChHby5nZW8uY29uaWNFcXVpZGlzdGFudD1mdW5jdGlvbigpe3JldHVybiBZZSh3cil9KS5y
YXc9d3I7dmFyIFZjPV9yKGZ1bmN0aW9uKG4pe3JldHVybiAxL259LE1hdGguYXRhbik7KEdvLmdl
by5nbm9tb25pYz1mdW5jdGlvbigpe3JldHVybiB1cihWYyl9KS5yYXc9VmMsU3IuaW52ZXJ0PWZ1
bmN0aW9uKG4sdCl7cmV0dXJuW24sMipNYXRoLmF0YW4oTWF0aC5leHAodCkpLUxhXX0sKEdvLmdl
by5tZXJjYXRvcj1mdW5jdGlvbigpe3JldHVybiBrcihTcil9KS5yYXc9U3I7dmFyICRjPV9yKGZ1
bmN0aW9uKCl7cmV0dXJuIDF9LE1hdGguYXNpbik7KEdvLmdlby5vcnRob2dyYXBoaWM9ZnVuY3Rp
b24oKXtyZXR1cm4gdXIoJGMpfSkucmF3PSRjO3ZhciBYYz1fcihmdW5jdGlvbihuKXtyZXR1cm4g
MS8oMStuKX0sZnVuY3Rpb24obil7cmV0dXJuIDIqTWF0aC5hdGFuKG4pfSk7KEdvLmdlby5zdGVy
ZW9ncmFwaGljPWZ1bmN0aW9uKCl7cmV0dXJuIHVyKFhjKX0pLnJhdz1YYyxFci5pbnZlcnQ9ZnVu
Y3Rpb24obix0KXtyZXR1cm5bLXQsMipNYXRoLmF0YW4oTWF0aC5leHAobikpLUxhXX0sKEdvLmdl
by50cmFuc3ZlcnNlTWVyY2F0b3I9ZnVuY3Rpb24oKXt2YXIgbj1rcihFciksdD1uLmNlbnRlcixl
PW4ucm90YXRlO3JldHVybiBuLmNlbnRlcj1mdW5jdGlvbihuKXtyZXR1cm4gbj90KFstblsxXSxu
WzBdXSk6KG49dCgpLFstblsxXSxuWzBdXSl9LG4ucm90YXRlPWZ1bmN0aW9uKG4pe3JldHVybiBu
P2UoW25bMF0sblsxXSxuLmxlbmd0aD4yP25bMl0rOTA6OTBdKToobj1lKCksW25bMF0sblsxXSxu
WzJdLTkwXSl9LG4ucm90YXRlKFswLDBdKX0pLnJhdz1FcixHby5nZW9tPXt9LEdvLmdlb20uaHVs
bD1mdW5jdGlvbihuKXtmdW5jdGlvbiB0KG4pe2lmKG4ubGVuZ3RoPDMpcmV0dXJuW107dmFyIHQs
dT1FdChlKSxpPUV0KHIpLG89bi5sZW5ndGgsYT1bXSxjPVtdO2Zvcih0PTA7bz50O3QrKylhLnB1
c2goWyt1LmNhbGwodGhpcyxuW3RdLHQpLCtpLmNhbGwodGhpcyxuW3RdLHQpLHRdKTtmb3IoYS5z
b3J0KExyKSx0PTA7bz50O3QrKyljLnB1c2goW2FbdF1bMF0sLWFbdF1bMV1dKTt2YXIgcz1Ocihh
KSxsPU5yKGMpLGY9bFswXT09PXNbMF0saD1sW2wubGVuZ3RoLTFdPT09c1tzLmxlbmd0aC0xXSxn
PVtdO2Zvcih0PXMubGVuZ3RoLTE7dD49MDstLXQpZy5wdXNoKG5bYVtzW3RdXVsyXV0pO2Zvcih0
PStmO3Q8bC5sZW5ndGgtaDsrK3QpZy5wdXNoKG5bYVtsW3RdXVsyXV0pO3JldHVybiBnfXZhciBl
PUFyLHI9Q3I7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/dChuKToodC54PWZ1bmN0aW9uKG4pe3Jl
dHVybiBhcmd1bWVudHMubGVuZ3RoPyhlPW4sdCk6ZX0sdC55PWZ1bmN0aW9uKG4pe3JldHVybiBh
cmd1bWVudHMubGVuZ3RoPyhyPW4sdCk6cn0sdCl9LEdvLmdlb20ucG9seWdvbj1mdW5jdGlvbihu
KXtyZXR1cm4gZGEobixCYyksbn07dmFyIEJjPUdvLmdlb20ucG9seWdvbi5wcm90b3R5cGU9W107
QmMuYXJlYT1mdW5jdGlvbigpe2Zvcih2YXIgbix0PS0xLGU9dGhpcy5sZW5ndGgscj10aGlzW2Ut
MV0sdT0wOysrdDxlOyluPXIscj10aGlzW3RdLHUrPW5bMV0qclswXS1uWzBdKnJbMV07cmV0dXJu
LjUqdX0sQmMuY2VudHJvaWQ9ZnVuY3Rpb24obil7dmFyIHQsZSxyPS0xLHU9dGhpcy5sZW5ndGgs
aT0wLG89MCxhPXRoaXNbdS0xXTtmb3IoYXJndW1lbnRzLmxlbmd0aHx8KG49LTEvKDYqdGhpcy5h
cmVhKCkpKTsrK3I8dTspdD1hLGE9dGhpc1tyXSxlPXRbMF0qYVsxXS1hWzBdKnRbMV0saSs9KHRb
MF0rYVswXSkqZSxvKz0odFsxXSthWzFdKSplO3JldHVybltpKm4sbypuXX0sQmMuY2xpcD1mdW5j
dGlvbihuKXtmb3IodmFyIHQsZSxyLHUsaSxvLGE9enIobiksYz0tMSxzPXRoaXMubGVuZ3RoLXpy
KHRoaXMpLGw9dGhpc1tzLTFdOysrYzxzOyl7Zm9yKHQ9bi5zbGljZSgpLG4ubGVuZ3RoPTAsdT10
aGlzW2NdLGk9dFsocj10Lmxlbmd0aC1hKS0xXSxlPS0xOysrZTxyOylvPXRbZV0sVHIobyxsLHUp
PyhUcihpLGwsdSl8fG4ucHVzaChxcihpLG8sbCx1KSksbi5wdXNoKG8pKTpUcihpLGwsdSkmJm4u
cHVzaChxcihpLG8sbCx1KSksaT1vO2EmJm4ucHVzaChuWzBdKSxsPXV9cmV0dXJuIG59O3ZhciBK
YyxXYyxHYyxLYyxRYyxucz1bXSx0cz1bXTtPci5wcm90b3R5cGUucHJlcGFyZT1mdW5jdGlvbigp
e2Zvcih2YXIgbix0PXRoaXMuZWRnZXMsZT10Lmxlbmd0aDtlLS07KW49dFtlXS5lZGdlLG4uYiYm
bi5hfHx0LnNwbGljZShlLDEpO3JldHVybiB0LnNvcnQoWXIpLHQubGVuZ3RofSxRci5wcm90b3R5
cGU9e3N0YXJ0OmZ1bmN0aW9uKCl7cmV0dXJuIHRoaXMuZWRnZS5sPT09dGhpcy5zaXRlP3RoaXMu
ZWRnZS5hOnRoaXMuZWRnZS5ifSxlbmQ6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5lZGdlLmw9PT10
aGlzLnNpdGU/dGhpcy5lZGdlLmI6dGhpcy5lZGdlLmF9fSxudS5wcm90b3R5cGU9e2luc2VydDpm
dW5jdGlvbihuLHQpe3ZhciBlLHIsdTtpZihuKXtpZih0LlA9bix0Lk49bi5OLG4uTiYmKG4uTi5Q
PXQpLG4uTj10LG4uUil7Zm9yKG49bi5SO24uTDspbj1uLkw7bi5MPXR9ZWxzZSBuLlI9dDtlPW59
ZWxzZSB0aGlzLl8/KG49dXUodGhpcy5fKSx0LlA9bnVsbCx0Lk49bixuLlA9bi5MPXQsZT1uKToo
dC5QPXQuTj1udWxsLHRoaXMuXz10LGU9bnVsbCk7Zm9yKHQuTD10LlI9bnVsbCx0LlU9ZSx0LkM9
ITAsbj10O2UmJmUuQzspcj1lLlUsZT09PXIuTD8odT1yLlIsdSYmdS5DPyhlLkM9dS5DPSExLHIu
Qz0hMCxuPXIpOihuPT09ZS5SJiYoZXUodGhpcyxlKSxuPWUsZT1uLlUpLGUuQz0hMSxyLkM9ITAs
cnUodGhpcyxyKSkpOih1PXIuTCx1JiZ1LkM/KGUuQz11LkM9ITEsci5DPSEwLG49cik6KG49PT1l
LkwmJihydSh0aGlzLGUpLG49ZSxlPW4uVSksZS5DPSExLHIuQz0hMCxldSh0aGlzLHIpKSksZT1u
LlU7dGhpcy5fLkM9ITF9LHJlbW92ZTpmdW5jdGlvbihuKXtuLk4mJihuLk4uUD1uLlApLG4uUCYm
KG4uUC5OPW4uTiksbi5OPW4uUD1udWxsO3ZhciB0LGUscix1PW4uVSxpPW4uTCxvPW4uUjtpZihl
PWk/bz91dShvKTppOm8sdT91Lkw9PT1uP3UuTD1lOnUuUj1lOnRoaXMuXz1lLGkmJm8/KHI9ZS5D
LGUuQz1uLkMsZS5MPWksaS5VPWUsZSE9PW8/KHU9ZS5VLGUuVT1uLlUsbj1lLlIsdS5MPW4sZS5S
PW8sby5VPWUpOihlLlU9dSx1PWUsbj1lLlIpKToocj1uLkMsbj1lKSxuJiYobi5VPXUpLCFyKXtp
ZihuJiZuLkMpcmV0dXJuIG4uQz0hMSx2b2lkIDA7ZG97aWYobj09PXRoaXMuXylicmVhaztpZihu
PT09dS5MKXtpZih0PXUuUix0LkMmJih0LkM9ITEsdS5DPSEwLGV1KHRoaXMsdSksdD11LlIpLHQu
TCYmdC5MLkN8fHQuUiYmdC5SLkMpe3QuUiYmdC5SLkN8fCh0LkwuQz0hMSx0LkM9ITAscnUodGhp
cyx0KSx0PXUuUiksdC5DPXUuQyx1LkM9dC5SLkM9ITEsZXUodGhpcyx1KSxuPXRoaXMuXzticmVh
a319ZWxzZSBpZih0PXUuTCx0LkMmJih0LkM9ITEsdS5DPSEwLHJ1KHRoaXMsdSksdD11LkwpLHQu
TCYmdC5MLkN8fHQuUiYmdC5SLkMpe3QuTCYmdC5MLkN8fCh0LlIuQz0hMSx0LkM9ITAsZXUodGhp
cyx0KSx0PXUuTCksdC5DPXUuQyx1LkM9dC5MLkM9ITEscnUodGhpcyx1KSxuPXRoaXMuXzticmVh
a310LkM9ITAsbj11LHU9dS5VfXdoaWxlKCFuLkMpO24mJihuLkM9ITEpfX19LEdvLmdlb20udm9y
b25vaT1mdW5jdGlvbihuKXtmdW5jdGlvbiB0KG4pe3ZhciB0PW5ldyBBcnJheShuLmxlbmd0aCks
cj1hWzBdWzBdLHU9YVswXVsxXSxpPWFbMV1bMF0sbz1hWzFdWzFdO3JldHVybiBpdShlKG4pLGEp
LmNlbGxzLmZvckVhY2goZnVuY3Rpb24oZSxhKXt2YXIgYz1lLmVkZ2VzLHM9ZS5zaXRlLGw9dFth
XT1jLmxlbmd0aD9jLm1hcChmdW5jdGlvbihuKXt2YXIgdD1uLnN0YXJ0KCk7cmV0dXJuW3QueCx0
LnldfSk6cy54Pj1yJiZzLng8PWkmJnMueT49dSYmcy55PD1vP1tbcixvXSxbaSxvXSxbaSx1XSxb
cix1XV06W107bC5wb2ludD1uW2FdfSksdH1mdW5jdGlvbiBlKG4pe3JldHVybiBuLm1hcChmdW5j
dGlvbihuLHQpe3JldHVybnt4Ok1hdGgucm91bmQoaShuLHQpL1RhKSpUYSx5Ok1hdGgucm91bmQo
byhuLHQpL1RhKSpUYSxpOnR9fSl9dmFyIHI9QXIsdT1DcixpPXIsbz11LGE9ZXM7cmV0dXJuIG4/
dChuKToodC5saW5rcz1mdW5jdGlvbihuKXtyZXR1cm4gaXUoZShuKSkuZWRnZXMuZmlsdGVyKGZ1
bmN0aW9uKG4pe3JldHVybiBuLmwmJm4ucn0pLm1hcChmdW5jdGlvbih0KXtyZXR1cm57c291cmNl
Om5bdC5sLmldLHRhcmdldDpuW3Quci5pXX19KX0sdC50cmlhbmdsZXM9ZnVuY3Rpb24obil7dmFy
IHQ9W107cmV0dXJuIGl1KGUobikpLmNlbGxzLmZvckVhY2goZnVuY3Rpb24oZSxyKXtmb3IodmFy
IHUsaSxvPWUuc2l0ZSxhPWUuZWRnZXMuc29ydChZciksYz0tMSxzPWEubGVuZ3RoLGw9YVtzLTFd
LmVkZ2UsZj1sLmw9PT1vP2wucjpsLmw7KytjPHM7KXU9bCxpPWYsbD1hW2NdLmVkZ2UsZj1sLmw9
PT1vP2wucjpsLmwscjxpLmkmJnI8Zi5pJiZhdShvLGksZik8MCYmdC5wdXNoKFtuW3JdLG5baS5p
XSxuW2YuaV1dKX0pLHR9LHQueD1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8o
aT1FdChyPW4pLHQpOnJ9LHQueT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8o
bz1FdCh1PW4pLHQpOnV9LHQuY2xpcEV4dGVudD1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRz
Lmxlbmd0aD8oYT1udWxsPT1uP2VzOm4sdCk6YT09PWVzP251bGw6YX0sdC5zaXplPWZ1bmN0aW9u
KG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoP3QuY2xpcEV4dGVudChuJiZbWzAsMF0sbl0pOmE9
PT1lcz9udWxsOmEmJmFbMV19LHQpfTt2YXIgZXM9W1stMWU2LC0xZTZdLFsxZTYsMWU2XV07R28u
Z2VvbS5kZWxhdW5heT1mdW5jdGlvbihuKXtyZXR1cm4gR28uZ2VvbS52b3Jvbm9pKCkudHJpYW5n
bGVzKG4pfSxHby5nZW9tLnF1YWR0cmVlPWZ1bmN0aW9uKG4sdCxlLHIsdSl7ZnVuY3Rpb24gaShu
KXtmdW5jdGlvbiBpKG4sdCxlLHIsdSxpLG8sYSl7aWYoIWlzTmFOKGUpJiYhaXNOYU4ocikpaWYo
bi5sZWFmKXt2YXIgYz1uLngsbD1uLnk7aWYobnVsbCE9YylpZihmYShjLWUpK2ZhKGwtcik8LjAx
KXMobix0LGUscix1LGksbyxhKTtlbHNle3ZhciBmPW4ucG9pbnQ7bi54PW4ueT1uLnBvaW50PW51
bGwscyhuLGYsYyxsLHUsaSxvLGEpLHMobix0LGUscix1LGksbyxhKX1lbHNlIG4ueD1lLG4ueT1y
LG4ucG9pbnQ9dH1lbHNlIHMobix0LGUscix1LGksbyxhKX1mdW5jdGlvbiBzKG4sdCxlLHIsdSxv
LGEsYyl7dmFyIHM9LjUqKHUrYSksbD0uNSoobytjKSxmPWU+PXMsaD1yPj1sLGc9KGg8PDEpK2Y7
bi5sZWFmPSExLG49bi5ub2Rlc1tnXXx8KG4ubm9kZXNbZ109bHUoKSksZj91PXM6YT1zLGg/bz1s
OmM9bCxpKG4sdCxlLHIsdSxvLGEsYyl9dmFyIGwsZixoLGcscCx2LGQsbSx5LHg9RXQoYSksTT1F
dChjKTtpZihudWxsIT10KXY9dCxkPWUsbT1yLHk9dTtlbHNlIGlmKG09eT0tKHY9ZD0xLzApLGY9
W10saD1bXSxwPW4ubGVuZ3RoLG8pZm9yKGc9MDtwPmc7KytnKWw9bltnXSxsLng8diYmKHY9bC54
KSxsLnk8ZCYmKGQ9bC55KSxsLng+bSYmKG09bC54KSxsLnk+eSYmKHk9bC55KSxmLnB1c2gobC54
KSxoLnB1c2gobC55KTtlbHNlIGZvcihnPTA7cD5nOysrZyl7dmFyIF89K3gobD1uW2ddLGcpLGI9
K00obCxnKTt2Pl8mJih2PV8pLGQ+YiYmKGQ9YiksXz5tJiYobT1fKSxiPnkmJih5PWIpLGYucHVz
aChfKSxoLnB1c2goYil9dmFyIHc9bS12LFM9eS1kO3c+Uz95PWQrdzptPXYrUzt2YXIgaz1sdSgp
O2lmKGsuYWRkPWZ1bmN0aW9uKG4pe2koayxuLCt4KG4sKytnKSwrTShuLGcpLHYsZCxtLHkpfSxr
LnZpc2l0PWZ1bmN0aW9uKG4pe2Z1KG4sayx2LGQsbSx5KX0sZz0tMSxudWxsPT10KXtmb3IoOysr
ZzxwOylpKGssbltnXSxmW2ddLGhbZ10sdixkLG0seSk7LS1nfWVsc2Ugbi5mb3JFYWNoKGsuYWRk
KTtyZXR1cm4gZj1oPW49bD1udWxsLGt9dmFyIG8sYT1BcixjPUNyO3JldHVybihvPWFyZ3VtZW50
cy5sZW5ndGgpPyhhPWN1LGM9c3UsMz09PW8mJih1PWUscj10LGU9dD0wKSxpKG4pKTooaS54PWZ1
bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhhPW4saSk6YX0saS55PWZ1bmN0aW9u
KG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhjPW4saSk6Y30saS5leHRlbnQ9ZnVuY3Rpb24o
bil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KG51bGw9PW4/dD1lPXI9dT1udWxsOih0PStuWzBd
WzBdLGU9K25bMF1bMV0scj0rblsxXVswXSx1PStuWzFdWzFdKSxpKTpudWxsPT10P251bGw6W1t0
LGVdLFtyLHVdXX0saS5zaXplPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhu
dWxsPT1uP3Q9ZT1yPXU9bnVsbDoodD1lPTAscj0rblswXSx1PStuWzFdKSxpKTpudWxsPT10P251
bGw6W3ItdCx1LWVdfSxpKX0sR28uaW50ZXJwb2xhdGVSZ2I9aHUsR28uaW50ZXJwb2xhdGVPYmpl
Y3Q9Z3UsR28uaW50ZXJwb2xhdGVOdW1iZXI9cHUsR28uaW50ZXJwb2xhdGVTdHJpbmc9dnU7dmFy
IHJzPS9bLStdPyg/OlxkK1wuP1xkKnxcLj9cZCspKD86W2VFXVstK10/XGQrKT8vZyx1cz1uZXcg
UmVnRXhwKHJzLnNvdXJjZSwiZyIpO0dvLmludGVycG9sYXRlPWR1LEdvLmludGVycG9sYXRvcnM9
W2Z1bmN0aW9uKG4sdCl7dmFyIGU9dHlwZW9mIHQ7cmV0dXJuKCJzdHJpbmciPT09ZT9KYS5oYXMo
dCl8fC9eKCN8cmdiXCh8aHNsXCgpLy50ZXN0KHQpP2h1OnZ1OnQgaW5zdGFuY2VvZiBldD9odTpB
cnJheS5pc0FycmF5KHQpP211OiJvYmplY3QiPT09ZSYmaXNOYU4odCk/Z3U6cHUpKG4sdCl9XSxH
by5pbnRlcnBvbGF0ZUFycmF5PW11O3ZhciBpcz1mdW5jdGlvbigpe3JldHVybiBBdH0sb3M9R28u
bWFwKHtsaW5lYXI6aXMscG9seTpTdSxxdWFkOmZ1bmN0aW9uKCl7cmV0dXJuIF91fSxjdWJpYzpm
dW5jdGlvbigpe3JldHVybiBidX0sc2luOmZ1bmN0aW9uKCl7cmV0dXJuIGt1fSxleHA6ZnVuY3Rp
b24oKXtyZXR1cm4gRXV9LGNpcmNsZTpmdW5jdGlvbigpe3JldHVybiBBdX0sZWxhc3RpYzpDdSxi
YWNrOk51LGJvdW5jZTpmdW5jdGlvbigpe3JldHVybiBMdX19KSxhcz1Hby5tYXAoeyJpbiI6QXQs
b3V0Onh1LCJpbi1vdXQiOk11LCJvdXQtaW4iOmZ1bmN0aW9uKG4pe3JldHVybiBNdSh4dShuKSl9
fSk7R28uZWFzZT1mdW5jdGlvbihuKXt2YXIgdD1uLmluZGV4T2YoIi0iKSxlPXQ+PTA/bi5zdWJz
dHJpbmcoMCx0KTpuLHI9dD49MD9uLnN1YnN0cmluZyh0KzEpOiJpbiI7cmV0dXJuIGU9b3MuZ2V0
KGUpfHxpcyxyPWFzLmdldChyKXx8QXQseXUocihlLmFwcGx5KG51bGwsS28uY2FsbChhcmd1bWVu
dHMsMSkpKSl9LEdvLmludGVycG9sYXRlSGNsPVR1LEdvLmludGVycG9sYXRlSHNsPXF1LEdvLmlu
dGVycG9sYXRlTGFiPXp1LEdvLmludGVycG9sYXRlUm91bmQ9UnUsR28udHJhbnNmb3JtPWZ1bmN0
aW9uKG4pe3ZhciB0PW5hLmNyZWF0ZUVsZW1lbnROUyhHby5ucy5wcmVmaXguc3ZnLCJnIik7cmV0
dXJuKEdvLnRyYW5zZm9ybT1mdW5jdGlvbihuKXtpZihudWxsIT1uKXt0LnNldEF0dHJpYnV0ZSgi
dHJhbnNmb3JtIixuKTt2YXIgZT10LnRyYW5zZm9ybS5iYXNlVmFsLmNvbnNvbGlkYXRlKCl9cmV0
dXJuIG5ldyBEdShlP2UubWF0cml4OmNzKX0pKG4pfSxEdS5wcm90b3R5cGUudG9TdHJpbmc9ZnVu
Y3Rpb24oKXtyZXR1cm4idHJhbnNsYXRlKCIrdGhpcy50cmFuc2xhdGUrIilyb3RhdGUoIit0aGlz
LnJvdGF0ZSsiKXNrZXdYKCIrdGhpcy5za2V3KyIpc2NhbGUoIit0aGlzLnNjYWxlKyIpIn07dmFy
IGNzPXthOjEsYjowLGM6MCxkOjEsZTowLGY6MH07R28uaW50ZXJwb2xhdGVUcmFuc2Zvcm09SHUs
R28ubGF5b3V0PXt9LEdvLmxheW91dC5idW5kbGU9ZnVuY3Rpb24oKXtyZXR1cm4gZnVuY3Rpb24o
bil7Zm9yKHZhciB0PVtdLGU9LTEscj1uLmxlbmd0aDsrK2U8cjspdC5wdXNoKEl1KG5bZV0pKTty
ZXR1cm4gdH19LEdvLmxheW91dC5jaG9yZD1mdW5jdGlvbigpe2Z1bmN0aW9uIG4oKXt2YXIgbixz
LGYsaCxnLHA9e30sdj1bXSxkPUdvLnJhbmdlKGkpLG09W107Zm9yKGU9W10scj1bXSxuPTAsaD0t
MTsrK2g8aTspe2ZvcihzPTAsZz0tMTsrK2c8aTspcys9dVtoXVtnXTt2LnB1c2gocyksbS5wdXNo
KEdvLnJhbmdlKGkpKSxuKz1zfWZvcihvJiZkLnNvcnQoZnVuY3Rpb24obix0KXtyZXR1cm4gbyh2
W25dLHZbdF0pfSksYSYmbS5mb3JFYWNoKGZ1bmN0aW9uKG4sdCl7bi5zb3J0KGZ1bmN0aW9uKG4s
ZSl7cmV0dXJuIGEodVt0XVtuXSx1W3RdW2VdKX0pfSksbj0oTmEtbCppKS9uLHM9MCxoPS0xOysr
aDxpOyl7Zm9yKGY9cyxnPS0xOysrZzxpOyl7dmFyIHk9ZFtoXSx4PW1beV1bZ10sTT11W3ldW3hd
LF89cyxiPXMrPU0qbjtwW3krIi0iK3hdPXtpbmRleDp5LHN1YmluZGV4Ongsc3RhcnRBbmdsZTpf
LGVuZEFuZ2xlOmIsdmFsdWU6TX19clt5XT17aW5kZXg6eSxzdGFydEFuZ2xlOmYsZW5kQW5nbGU6
cyx2YWx1ZToocy1mKS9ufSxzKz1sfWZvcihoPS0xOysraDxpOylmb3IoZz1oLTE7KytnPGk7KXt2
YXIgdz1wW2grIi0iK2ddLFM9cFtnKyItIitoXTsody52YWx1ZXx8Uy52YWx1ZSkmJmUucHVzaCh3
LnZhbHVlPFMudmFsdWU/e3NvdXJjZTpTLHRhcmdldDp3fTp7c291cmNlOncsdGFyZ2V0OlN9KX1j
JiZ0KCl9ZnVuY3Rpb24gdCgpe2Uuc29ydChmdW5jdGlvbihuLHQpe3JldHVybiBjKChuLnNvdXJj
ZS52YWx1ZStuLnRhcmdldC52YWx1ZSkvMiwodC5zb3VyY2UudmFsdWUrdC50YXJnZXQudmFsdWUp
LzIpfSl9dmFyIGUscix1LGksbyxhLGMscz17fSxsPTA7cmV0dXJuIHMubWF0cml4PWZ1bmN0aW9u
KG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhpPSh1PW4pJiZ1Lmxlbmd0aCxlPXI9bnVsbCxz
KTp1fSxzLnBhZGRpbmc9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGw9bixl
PXI9bnVsbCxzKTpsfSxzLnNvcnRHcm91cHM9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5s
ZW5ndGg/KG89bixlPXI9bnVsbCxzKTpvfSxzLnNvcnRTdWJncm91cHM9ZnVuY3Rpb24obil7cmV0
dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGE9bixlPW51bGwscyk6YX0scy5zb3J0Q2hvcmRzPWZ1bmN0
aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhjPW4sZSYmdCgpLHMpOmN9LHMuY2hvcmRz
PWZ1bmN0aW9uKCl7cmV0dXJuIGV8fG4oKSxlfSxzLmdyb3Vwcz1mdW5jdGlvbigpe3JldHVybiBy
fHxuKCkscn0sc30sR28ubGF5b3V0LmZvcmNlPWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbihuKXtyZXR1
cm4gZnVuY3Rpb24odCxlLHIsdSl7aWYodC5wb2ludCE9PW4pe3ZhciBpPXQuY3gtbi54LG89dC5j
eS1uLnksYT11LWUsYz1pKmkrbypvO2lmKGM+YSphL2Qpe2lmKHA+Yyl7dmFyIHM9dC5jaGFyZ2Uv
YztuLnB4LT1pKnMsbi5weS09bypzfXJldHVybiEwfWlmKHQucG9pbnQmJmMmJnA+Yyl7dmFyIHM9
dC5wb2ludENoYXJnZS9jO24ucHgtPWkqcyxuLnB5LT1vKnN9fXJldHVybiF0LmNoYXJnZX19ZnVu
Y3Rpb24gdChuKXtuLnB4PUdvLmV2ZW50Lngsbi5weT1Hby5ldmVudC55LGEucmVzdW1lKCl9dmFy
IGUscix1LGksbyxhPXt9LGM9R28uZGlzcGF0Y2goInN0YXJ0IiwidGljayIsImVuZCIpLHM9WzEs
MV0sbD0uOSxmPXNzLGg9bHMsZz0tMzAscD1mcyx2PS4xLGQ9LjY0LG09W10seT1bXTtyZXR1cm4g
YS50aWNrPWZ1bmN0aW9uKCl7aWYoKHIqPS45OSk8LjAwNSlyZXR1cm4gYy5lbmQoe3R5cGU6ImVu
ZCIsYWxwaGE6cj0wfSksITA7dmFyIHQsZSxhLGYsaCxwLGQseCxNLF89bS5sZW5ndGgsYj15Lmxl
bmd0aDtmb3IoZT0wO2I+ZTsrK2UpYT15W2VdLGY9YS5zb3VyY2UsaD1hLnRhcmdldCx4PWgueC1m
LngsTT1oLnktZi55LChwPXgqeCtNKk0pJiYocD1yKmlbZV0qKChwPU1hdGguc3FydChwKSktdVtl
XSkvcCx4Kj1wLE0qPXAsaC54LT14KihkPWYud2VpZ2h0LyhoLndlaWdodCtmLndlaWdodCkpLGgu
eS09TSpkLGYueCs9eCooZD0xLWQpLGYueSs9TSpkKTtpZigoZD1yKnYpJiYoeD1zWzBdLzIsTT1z
WzFdLzIsZT0tMSxkKSlmb3IoOysrZTxfOylhPW1bZV0sYS54Kz0oeC1hLngpKmQsYS55Kz0oTS1h
LnkpKmQ7aWYoZylmb3IoSnUodD1Hby5nZW9tLnF1YWR0cmVlKG0pLHIsbyksZT0tMTsrK2U8Xzsp
KGE9bVtlXSkuZml4ZWR8fHQudmlzaXQobihhKSk7Zm9yKGU9LTE7KytlPF87KWE9bVtlXSxhLmZp
eGVkPyhhLng9YS5weCxhLnk9YS5weSk6KGEueC09KGEucHgtKGEucHg9YS54KSkqbCxhLnktPShh
LnB5LShhLnB5PWEueSkpKmwpO2MudGljayh7dHlwZToidGljayIsYWxwaGE6cn0pfSxhLm5vZGVz
PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhtPW4sYSk6bX0sYS5saW5rcz1m
dW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oeT1uLGEpOnl9LGEuc2l6ZT1mdW5j
dGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8ocz1uLGEpOnN9LGEubGlua0Rpc3RhbmNl
PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhmPSJmdW5jdGlvbiI9PXR5cGVv
ZiBuP246K24sYSk6Zn0sYS5kaXN0YW5jZT1hLmxpbmtEaXN0YW5jZSxhLmxpbmtTdHJlbmd0aD1m
dW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oaD0iZnVuY3Rpb24iPT10eXBlb2Yg
bj9uOituLGEpOmh9LGEuZnJpY3Rpb249ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5n
dGg/KGw9K24sYSk6bH0sYS5jaGFyZ2U9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5n
dGg/KGc9ImZ1bmN0aW9uIj09dHlwZW9mIG4/bjorbixhKTpnfSxhLmNoYXJnZURpc3RhbmNlPWZ1
bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhwPW4qbixhKTpNYXRoLnNxcnQocCl9
LGEuZ3Jhdml0eT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8odj0rbixhKTp2
fSxhLnRoZXRhPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhkPW4qbixhKTpN
YXRoLnNxcnQoZCl9LGEuYWxwaGE9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/
KG49K24scj9yPW4+MD9uOjA6bj4wJiYoYy5zdGFydCh7dHlwZToic3RhcnQiLGFscGhhOnI9bn0p
LEdvLnRpbWVyKGEudGljaykpLGEpOnJ9LGEuc3RhcnQ9ZnVuY3Rpb24oKXtmdW5jdGlvbiBuKG4s
cil7aWYoIWUpe2ZvcihlPW5ldyBBcnJheShjKSxhPTA7Yz5hOysrYSllW2FdPVtdO2ZvcihhPTA7
cz5hOysrYSl7dmFyIHU9eVthXTtlW3Uuc291cmNlLmluZGV4XS5wdXNoKHUudGFyZ2V0KSxlW3Uu
dGFyZ2V0LmluZGV4XS5wdXNoKHUuc291cmNlKX19Zm9yKHZhciBpLG89ZVt0XSxhPS0xLHM9by5s
ZW5ndGg7KythPHM7KWlmKCFpc05hTihpPW9bYV1bbl0pKXJldHVybiBpO3JldHVybiBNYXRoLnJh
bmRvbSgpKnJ9dmFyIHQsZSxyLGM9bS5sZW5ndGgsbD15Lmxlbmd0aCxwPXNbMF0sdj1zWzFdO2Zv
cih0PTA7Yz50OysrdCkocj1tW3RdKS5pbmRleD10LHIud2VpZ2h0PTA7Zm9yKHQ9MDtsPnQ7Kyt0
KXI9eVt0XSwibnVtYmVyIj09dHlwZW9mIHIuc291cmNlJiYoci5zb3VyY2U9bVtyLnNvdXJjZV0p
LCJudW1iZXIiPT10eXBlb2Ygci50YXJnZXQmJihyLnRhcmdldD1tW3IudGFyZ2V0XSksKytyLnNv
dXJjZS53ZWlnaHQsKytyLnRhcmdldC53ZWlnaHQ7Zm9yKHQ9MDtjPnQ7Kyt0KXI9bVt0XSxpc05h
TihyLngpJiYoci54PW4oIngiLHApKSxpc05hTihyLnkpJiYoci55PW4oInkiLHYpKSxpc05hTihy
LnB4KSYmKHIucHg9ci54KSxpc05hTihyLnB5KSYmKHIucHk9ci55KTtpZih1PVtdLCJmdW5jdGlv
biI9PXR5cGVvZiBmKWZvcih0PTA7bD50OysrdCl1W3RdPStmLmNhbGwodGhpcyx5W3RdLHQpO2Vs
c2UgZm9yKHQ9MDtsPnQ7Kyt0KXVbdF09ZjtpZihpPVtdLCJmdW5jdGlvbiI9PXR5cGVvZiBoKWZv
cih0PTA7bD50OysrdClpW3RdPStoLmNhbGwodGhpcyx5W3RdLHQpO2Vsc2UgZm9yKHQ9MDtsPnQ7
Kyt0KWlbdF09aDtpZihvPVtdLCJmdW5jdGlvbiI9PXR5cGVvZiBnKWZvcih0PTA7Yz50OysrdClv
W3RdPStnLmNhbGwodGhpcyxtW3RdLHQpO2Vsc2UgZm9yKHQ9MDtjPnQ7Kyt0KW9bdF09ZztyZXR1
cm4gYS5yZXN1bWUoKX0sYS5yZXN1bWU9ZnVuY3Rpb24oKXtyZXR1cm4gYS5hbHBoYSguMSl9LGEu
c3RvcD1mdW5jdGlvbigpe3JldHVybiBhLmFscGhhKDApfSxhLmRyYWc9ZnVuY3Rpb24oKXtyZXR1
cm4gZXx8KGU9R28uYmVoYXZpb3IuZHJhZygpLm9yaWdpbihBdCkub24oImRyYWdzdGFydC5mb3Jj
ZSIsVnUpLm9uKCJkcmFnLmZvcmNlIix0KS5vbigiZHJhZ2VuZC5mb3JjZSIsJHUpKSxhcmd1bWVu
dHMubGVuZ3RoPyh0aGlzLm9uKCJtb3VzZW92ZXIuZm9yY2UiLFh1KS5vbigibW91c2VvdXQuZm9y
Y2UiLEJ1KS5jYWxsKGUpLHZvaWQgMCk6ZX0sR28ucmViaW5kKGEsYywib24iKX07dmFyIHNzPTIw
LGxzPTEsZnM9MS8wO0dvLmxheW91dC5oaWVyYXJjaHk9ZnVuY3Rpb24oKXtmdW5jdGlvbiBuKHQs
byxhKXt2YXIgYz11LmNhbGwoZSx0LG8pO2lmKHQuZGVwdGg9byxhLnB1c2godCksYyYmKHM9Yy5s
ZW5ndGgpKXtmb3IodmFyIHMsbCxmPS0xLGg9dC5jaGlsZHJlbj1uZXcgQXJyYXkocyksZz0wLHA9
bysxOysrZjxzOylsPWhbZl09bihjW2ZdLHAsYSksbC5wYXJlbnQ9dCxnKz1sLnZhbHVlO3ImJmgu
c29ydChyKSxpJiYodC52YWx1ZT1nKX1lbHNlIGRlbGV0ZSB0LmNoaWxkcmVuLGkmJih0LnZhbHVl
PStpLmNhbGwoZSx0LG8pfHwwKTtyZXR1cm4gdH1mdW5jdGlvbiB0KG4scil7dmFyIHU9bi5jaGls
ZHJlbixvPTA7aWYodSYmKGE9dS5sZW5ndGgpKWZvcih2YXIgYSxjPS0xLHM9cisxOysrYzxhOylv
Kz10KHVbY10scyk7ZWxzZSBpJiYobz0raS5jYWxsKGUsbixyKXx8MCk7cmV0dXJuIGkmJihuLnZh
bHVlPW8pLG99ZnVuY3Rpb24gZSh0KXt2YXIgZT1bXTtyZXR1cm4gbih0LDAsZSksZX12YXIgcj1R
dSx1PUd1LGk9S3U7cmV0dXJuIGUuc29ydD1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxl
bmd0aD8ocj1uLGUpOnJ9LGUuY2hpbGRyZW49ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5s
ZW5ndGg/KHU9bixlKTp1fSxlLnZhbHVlPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVu
Z3RoPyhpPW4sZSk6aX0sZS5yZXZhbHVlPWZ1bmN0aW9uKG4pe3JldHVybiB0KG4sMCksbn0sZX0s
R28ubGF5b3V0LnBhcnRpdGlvbj1mdW5jdGlvbigpe2Z1bmN0aW9uIG4odCxlLHIsdSl7dmFyIGk9
dC5jaGlsZHJlbjtpZih0Lng9ZSx0Lnk9dC5kZXB0aCp1LHQuZHg9cix0LmR5PXUsaSYmKG89aS5s
ZW5ndGgpKXt2YXIgbyxhLGMscz0tMTtmb3Iocj10LnZhbHVlP3IvdC52YWx1ZTowOysrczxvOylu
KGE9aVtzXSxlLGM9YS52YWx1ZSpyLHUpLGUrPWN9fWZ1bmN0aW9uIHQobil7dmFyIGU9bi5jaGls
ZHJlbixyPTA7aWYoZSYmKHU9ZS5sZW5ndGgpKWZvcih2YXIgdSxpPS0xOysraTx1OylyPU1hdGgu
bWF4KHIsdChlW2ldKSk7cmV0dXJuIDErcn1mdW5jdGlvbiBlKGUsaSl7dmFyIG89ci5jYWxsKHRo
aXMsZSxpKTtyZXR1cm4gbihvWzBdLDAsdVswXSx1WzFdL3Qob1swXSkpLG99dmFyIHI9R28ubGF5
b3V0LmhpZXJhcmNoeSgpLHU9WzEsMV07cmV0dXJuIGUuc2l6ZT1mdW5jdGlvbihuKXtyZXR1cm4g
YXJndW1lbnRzLmxlbmd0aD8odT1uLGUpOnV9LFd1KGUscil9LEdvLmxheW91dC5waWU9ZnVuY3Rp
b24oKXtmdW5jdGlvbiBuKGkpe3ZhciBvPWkubWFwKGZ1bmN0aW9uKGUscil7cmV0dXJuK3QuY2Fs
bChuLGUscil9KSxhPSsoImZ1bmN0aW9uIj09dHlwZW9mIHI/ci5hcHBseSh0aGlzLGFyZ3VtZW50
cyk6ciksYz0oKCJmdW5jdGlvbiI9PXR5cGVvZiB1P3UuYXBwbHkodGhpcyxhcmd1bWVudHMpOnUp
LWEpL0dvLnN1bShvKSxzPUdvLnJhbmdlKGkubGVuZ3RoKTtudWxsIT1lJiZzLnNvcnQoZT09PWhz
P2Z1bmN0aW9uKG4sdCl7cmV0dXJuIG9bdF0tb1tuXX06ZnVuY3Rpb24obix0KXtyZXR1cm4gZShp
W25dLGlbdF0pfSk7dmFyIGw9W107cmV0dXJuIHMuZm9yRWFjaChmdW5jdGlvbihuKXt2YXIgdDts
W25dPXtkYXRhOmlbbl0sdmFsdWU6dD1vW25dLHN0YXJ0QW5nbGU6YSxlbmRBbmdsZTphKz10KmN9
fSksbH12YXIgdD1OdW1iZXIsZT1ocyxyPTAsdT1OYTtyZXR1cm4gbi52YWx1ZT1mdW5jdGlvbihl
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8odD1lLG4pOnR9LG4uc29ydD1mdW5jdGlvbih0KXty
ZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZT10LG4pOmV9LG4uc3RhcnRBbmdsZT1mdW5jdGlvbih0
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8ocj10LG4pOnJ9LG4uZW5kQW5nbGU9ZnVuY3Rpb24o
dCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHU9dCxuKTp1fSxufTt2YXIgaHM9e307R28ubGF5
b3V0LnN0YWNrPWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbihhLGMpe3ZhciBzPWEubWFwKGZ1bmN0aW9u
KGUscil7cmV0dXJuIHQuY2FsbChuLGUscil9KSxsPXMubWFwKGZ1bmN0aW9uKHQpe3JldHVybiB0
Lm1hcChmdW5jdGlvbih0LGUpe3JldHVybltpLmNhbGwobix0LGUpLG8uY2FsbChuLHQsZSldfSl9
KSxmPWUuY2FsbChuLGwsYyk7cz1Hby5wZXJtdXRlKHMsZiksbD1Hby5wZXJtdXRlKGwsZik7dmFy
IGgsZyxwLHY9ci5jYWxsKG4sbCxjKSxkPXMubGVuZ3RoLG09c1swXS5sZW5ndGg7Zm9yKGc9MDtt
Pmc7KytnKWZvcih1LmNhbGwobixzWzBdW2ddLHA9dltnXSxsWzBdW2ddWzFdKSxoPTE7ZD5oOysr
aCl1LmNhbGwobixzW2hdW2ddLHArPWxbaC0xXVtnXVsxXSxsW2hdW2ddWzFdKTtyZXR1cm4gYX12
YXIgdD1BdCxlPXVpLHI9aWksdT1yaSxpPXRpLG89ZWk7cmV0dXJuIG4udmFsdWVzPWZ1bmN0aW9u
KGUpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh0PWUsbik6dH0sbi5vcmRlcj1mdW5jdGlvbih0
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZT0iZnVuY3Rpb24iPT10eXBlb2YgdD90OmdzLmdl
dCh0KXx8dWksbik6ZX0sbi5vZmZzZXQ9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5n
dGg/KHI9ImZ1bmN0aW9uIj09dHlwZW9mIHQ/dDpwcy5nZXQodCl8fGlpLG4pOnJ9LG4ueD1mdW5j
dGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oaT10LG4pOml9LG4ueT1mdW5jdGlvbih0
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8obz10LG4pOm99LG4ub3V0PWZ1bmN0aW9uKHQpe3Jl
dHVybiBhcmd1bWVudHMubGVuZ3RoPyh1PXQsbik6dX0sbn07dmFyIGdzPUdvLm1hcCh7Imluc2lk
ZS1vdXQiOmZ1bmN0aW9uKG4pe3ZhciB0LGUscj1uLmxlbmd0aCx1PW4ubWFwKG9pKSxpPW4ubWFw
KGFpKSxvPUdvLnJhbmdlKHIpLnNvcnQoZnVuY3Rpb24obix0KXtyZXR1cm4gdVtuXS11W3RdfSks
YT0wLGM9MCxzPVtdLGw9W107Zm9yKHQ9MDtyPnQ7Kyt0KWU9b1t0XSxjPmE/KGErPWlbZV0scy5w
dXNoKGUpKTooYys9aVtlXSxsLnB1c2goZSkpO3JldHVybiBsLnJldmVyc2UoKS5jb25jYXQocyl9
LHJldmVyc2U6ZnVuY3Rpb24obil7cmV0dXJuIEdvLnJhbmdlKG4ubGVuZ3RoKS5yZXZlcnNlKCl9
LCJkZWZhdWx0Ijp1aX0pLHBzPUdvLm1hcCh7c2lsaG91ZXR0ZTpmdW5jdGlvbihuKXt2YXIgdCxl
LHIsdT1uLmxlbmd0aCxpPW5bMF0ubGVuZ3RoLG89W10sYT0wLGM9W107Zm9yKGU9MDtpPmU7Kytl
KXtmb3IodD0wLHI9MDt1PnQ7dCsrKXIrPW5bdF1bZV1bMV07cj5hJiYoYT1yKSxvLnB1c2gocil9
Zm9yKGU9MDtpPmU7KytlKWNbZV09KGEtb1tlXSkvMjtyZXR1cm4gY30sd2lnZ2xlOmZ1bmN0aW9u
KG4pe3ZhciB0LGUscix1LGksbyxhLGMscyxsPW4ubGVuZ3RoLGY9blswXSxoPWYubGVuZ3RoLGc9
W107Zm9yKGdbMF09Yz1zPTAsZT0xO2g+ZTsrK2Upe2Zvcih0PTAsdT0wO2w+dDsrK3QpdSs9blt0
XVtlXVsxXTtmb3IodD0wLGk9MCxhPWZbZV1bMF0tZltlLTFdWzBdO2w+dDsrK3Qpe2ZvcihyPTAs
bz0oblt0XVtlXVsxXS1uW3RdW2UtMV1bMV0pLygyKmEpO3Q+cjsrK3Ipbys9KG5bcl1bZV1bMV0t
bltyXVtlLTFdWzFdKS9hO2krPW8qblt0XVtlXVsxXX1nW2VdPWMtPXU/aS91KmE6MCxzPmMmJihz
PWMpfWZvcihlPTA7aD5lOysrZSlnW2VdLT1zO3JldHVybiBnfSxleHBhbmQ6ZnVuY3Rpb24obil7
dmFyIHQsZSxyLHU9bi5sZW5ndGgsaT1uWzBdLmxlbmd0aCxvPTEvdSxhPVtdO2ZvcihlPTA7aT5l
OysrZSl7Zm9yKHQ9MCxyPTA7dT50O3QrKylyKz1uW3RdW2VdWzFdO2lmKHIpZm9yKHQ9MDt1PnQ7
dCsrKW5bdF1bZV1bMV0vPXI7ZWxzZSBmb3IodD0wO3U+dDt0Kyspblt0XVtlXVsxXT1vfWZvcihl
PTA7aT5lOysrZSlhW2VdPTA7cmV0dXJuIGF9LHplcm86aWl9KTtHby5sYXlvdXQuaGlzdG9ncmFt
PWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbihuLGkpe2Zvcih2YXIgbyxhLGM9W10scz1uLm1hcChlLHRo
aXMpLGw9ci5jYWxsKHRoaXMscyxpKSxmPXUuY2FsbCh0aGlzLGwscyxpKSxpPS0xLGg9cy5sZW5n
dGgsZz1mLmxlbmd0aC0xLHA9dD8xOjEvaDsrK2k8Zzspbz1jW2ldPVtdLG8uZHg9ZltpKzFdLShv
Lng9ZltpXSksby55PTA7aWYoZz4wKWZvcihpPS0xOysraTxoOylhPXNbaV0sYT49bFswXSYmYTw9
bFsxXSYmKG89Y1tHby5iaXNlY3QoZixhLDEsZyktMV0sby55Kz1wLG8ucHVzaChuW2ldKSk7cmV0
dXJuIGN9dmFyIHQ9ITAsZT1OdW1iZXIscj1maSx1PXNpO3JldHVybiBuLnZhbHVlPWZ1bmN0aW9u
KHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhlPXQsbik6ZX0sbi5yYW5nZT1mdW5jdGlvbih0
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8ocj1FdCh0KSxuKTpyfSxuLmJpbnM9ZnVuY3Rpb24o
dCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHU9Im51bWJlciI9PXR5cGVvZiB0P2Z1bmN0aW9u
KG4pe3JldHVybiBsaShuLHQpfTpFdCh0KSxuKTp1fSxuLmZyZXF1ZW5jeT1mdW5jdGlvbihlKXty
ZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8odD0hIWUsbik6dH0sbn0sR28ubGF5b3V0LnRyZWU9ZnVu
Y3Rpb24oKXtmdW5jdGlvbiBuKG4saSl7ZnVuY3Rpb24gbyhuLHQpe3ZhciByPW4uY2hpbGRyZW4s
dT1uLl90cmVlO2lmKHImJihpPXIubGVuZ3RoKSl7Zm9yKHZhciBpLGEscyxsPXJbMF0sZj1sLGg9
LTE7KytoPGk7KXM9cltoXSxvKHMsYSksZj1jKHMsYSxmKSxhPXM7TWkobik7dmFyIGc9LjUqKGwu
X3RyZWUucHJlbGltK3MuX3RyZWUucHJlbGltKTt0Pyh1LnByZWxpbT10Ll90cmVlLnByZWxpbStl
KG4sdCksdS5tb2Q9dS5wcmVsaW0tZyk6dS5wcmVsaW09Z31lbHNlIHQmJih1LnByZWxpbT10Ll90
cmVlLnByZWxpbStlKG4sdCkpfWZ1bmN0aW9uIGEobix0KXtuLng9bi5fdHJlZS5wcmVsaW0rdDt2
YXIgZT1uLmNoaWxkcmVuO2lmKGUmJihyPWUubGVuZ3RoKSl7dmFyIHIsdT0tMTtmb3IodCs9bi5f
dHJlZS5tb2Q7Kyt1PHI7KWEoZVt1XSx0KX19ZnVuY3Rpb24gYyhuLHQscil7aWYodCl7Zm9yKHZh
ciB1LGk9bixvPW4sYT10LGM9bi5wYXJlbnQuY2hpbGRyZW5bMF0scz1pLl90cmVlLm1vZCxsPW8u
X3RyZWUubW9kLGY9YS5fdHJlZS5tb2QsaD1jLl90cmVlLm1vZDthPXBpKGEpLGk9Z2koaSksYSYm
aTspYz1naShjKSxvPXBpKG8pLG8uX3RyZWUuYW5jZXN0b3I9bix1PWEuX3RyZWUucHJlbGltK2Yt
aS5fdHJlZS5wcmVsaW0tcytlKGEsaSksdT4wJiYoX2koYmkoYSxuLHIpLG4sdSkscys9dSxsKz11
KSxmKz1hLl90cmVlLm1vZCxzKz1pLl90cmVlLm1vZCxoKz1jLl90cmVlLm1vZCxsKz1vLl90cmVl
Lm1vZDthJiYhcGkobykmJihvLl90cmVlLnRocmVhZD1hLG8uX3RyZWUubW9kKz1mLWwpLGkmJiFn
aShjKSYmKGMuX3RyZWUudGhyZWFkPWksYy5fdHJlZS5tb2QrPXMtaCxyPW4pfXJldHVybiByfXZh
ciBzPXQuY2FsbCh0aGlzLG4saSksbD1zWzBdO3hpKGwsZnVuY3Rpb24obix0KXtuLl90cmVlPXth
bmNlc3RvcjpuLHByZWxpbTowLG1vZDowLGNoYW5nZTowLHNoaWZ0OjAsbnVtYmVyOnQ/dC5fdHJl
ZS5udW1iZXIrMTowfX0pLG8obCksYShsLC1sLl90cmVlLnByZWxpbSk7dmFyIGY9dmkobCxtaSks
aD12aShsLGRpKSxnPXZpKGwseWkpLHA9Zi54LWUoZixoKS8yLHY9aC54K2UoaCxmKS8yLGQ9Zy5k
ZXB0aHx8MTtyZXR1cm4geGkobCx1P2Z1bmN0aW9uKG4pe24ueCo9clswXSxuLnk9bi5kZXB0aCpy
WzFdLGRlbGV0ZSBuLl90cmVlfTpmdW5jdGlvbihuKXtuLng9KG4ueC1wKS8odi1wKSpyWzBdLG4u
eT1uLmRlcHRoL2QqclsxXSxkZWxldGUgbi5fdHJlZX0pLHN9dmFyIHQ9R28ubGF5b3V0LmhpZXJh
cmNoeSgpLnNvcnQobnVsbCkudmFsdWUobnVsbCksZT1oaSxyPVsxLDFdLHU9ITE7cmV0dXJuIG4u
c2VwYXJhdGlvbj1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZT10LG4pOmV9
LG4uc2l6ZT1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8odT1udWxsPT0ocj10
KSxuKTp1P251bGw6cn0sbi5ub2RlU2l6ZT1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxl
bmd0aD8odT1udWxsIT0ocj10KSxuKTp1P3I6bnVsbH0sV3Uobix0KX0sR28ubGF5b3V0LnBhY2s9
ZnVuY3Rpb24oKXtmdW5jdGlvbiBuKG4saSl7dmFyIG89ZS5jYWxsKHRoaXMsbixpKSxhPW9bMF0s
Yz11WzBdLHM9dVsxXSxsPW51bGw9PXQ/TWF0aC5zcXJ0OiJmdW5jdGlvbiI9PXR5cGVvZiB0P3Q6
ZnVuY3Rpb24oKXtyZXR1cm4gdH07aWYoYS54PWEueT0wLHhpKGEsZnVuY3Rpb24obil7bi5yPSts
KG4udmFsdWUpfSkseGkoYSxBaSkscil7dmFyIGY9cioodD8xOk1hdGgubWF4KDIqYS5yL2MsMiph
LnIvcykpLzI7eGkoYSxmdW5jdGlvbihuKXtuLnIrPWZ9KSx4aShhLEFpKSx4aShhLGZ1bmN0aW9u
KG4pe24uci09Zn0pfXJldHVybiBMaShhLGMvMixzLzIsdD8xOjEvTWF0aC5tYXgoMiphLnIvYywy
KmEuci9zKSksb312YXIgdCxlPUdvLmxheW91dC5oaWVyYXJjaHkoKS5zb3J0KHdpKSxyPTAsdT1b
MSwxXTtyZXR1cm4gbi5zaXplPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh1
PXQsbik6dX0sbi5yYWRpdXM9ZnVuY3Rpb24oZSl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHQ9
bnVsbD09ZXx8ImZ1bmN0aW9uIj09dHlwZW9mIGU/ZTorZSxuKTp0fSxuLnBhZGRpbmc9ZnVuY3Rp
b24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHI9K3Qsbik6cn0sV3UobixlKX0sR28ubGF5
b3V0LmNsdXN0ZXI9ZnVuY3Rpb24oKXtmdW5jdGlvbiBuKG4saSl7dmFyIG8sYT10LmNhbGwodGhp
cyxuLGkpLGM9YVswXSxzPTA7eGkoYyxmdW5jdGlvbihuKXt2YXIgdD1uLmNoaWxkcmVuO3QmJnQu
bGVuZ3RoPyhuLng9emkodCksbi55PXFpKHQpKToobi54PW8/cys9ZShuLG8pOjAsbi55PTAsbz1u
KX0pO3ZhciBsPVJpKGMpLGY9RGkoYyksaD1sLngtZShsLGYpLzIsZz1mLngrZShmLGwpLzI7cmV0
dXJuIHhpKGMsdT9mdW5jdGlvbihuKXtuLng9KG4ueC1jLngpKnJbMF0sbi55PShjLnktbi55KSpy
WzFdfTpmdW5jdGlvbihuKXtuLng9KG4ueC1oKS8oZy1oKSpyWzBdLG4ueT0oMS0oYy55P24ueS9j
Lnk6MSkpKnJbMV19KSxhfXZhciB0PUdvLmxheW91dC5oaWVyYXJjaHkoKS5zb3J0KG51bGwpLnZh
bHVlKG51bGwpLGU9aGkscj1bMSwxXSx1PSExO3JldHVybiBuLnNlcGFyYXRpb249ZnVuY3Rpb24o
dCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGU9dCxuKTplfSxuLnNpemU9ZnVuY3Rpb24odCl7
cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHU9bnVsbD09KHI9dCksbik6dT9udWxsOnJ9LG4ubm9k
ZVNpemU9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHU9bnVsbCE9KHI9dCks
bik6dT9yOm51bGx9LFd1KG4sdCl9LEdvLmxheW91dC50cmVlbWFwPWZ1bmN0aW9uKCl7ZnVuY3Rp
b24gbihuLHQpe2Zvcih2YXIgZSxyLHU9LTEsaT1uLmxlbmd0aDsrK3U8aTspcj0oZT1uW3VdKS52
YWx1ZSooMD50PzA6dCksZS5hcmVhPWlzTmFOKHIpfHwwPj1yPzA6cn1mdW5jdGlvbiB0KGUpe3Zh
ciBpPWUuY2hpbGRyZW47aWYoaSYmaS5sZW5ndGgpe3ZhciBvLGEsYyxzPWYoZSksbD1bXSxoPWku
c2xpY2UoKSxwPTEvMCx2PSJzbGljZSI9PT1nP3MuZHg6ImRpY2UiPT09Zz9zLmR5OiJzbGljZS1k
aWNlIj09PWc/MSZlLmRlcHRoP3MuZHk6cy5keDpNYXRoLm1pbihzLmR4LHMuZHkpO2ZvcihuKGgs
cy5keCpzLmR5L2UudmFsdWUpLGwuYXJlYT0wOyhjPWgubGVuZ3RoKT4wOylsLnB1c2gobz1oW2Mt
MV0pLGwuYXJlYSs9by5hcmVhLCJzcXVhcmlmeSIhPT1nfHwoYT1yKGwsdikpPD1wPyhoLnBvcCgp
LHA9YSk6KGwuYXJlYS09bC5wb3AoKS5hcmVhLHUobCx2LHMsITEpLHY9TWF0aC5taW4ocy5keCxz
LmR5KSxsLmxlbmd0aD1sLmFyZWE9MCxwPTEvMCk7bC5sZW5ndGgmJih1KGwsdixzLCEwKSxsLmxl
bmd0aD1sLmFyZWE9MCksaS5mb3JFYWNoKHQpfX1mdW5jdGlvbiBlKHQpe3ZhciByPXQuY2hpbGRy
ZW47aWYociYmci5sZW5ndGgpe3ZhciBpLG89Zih0KSxhPXIuc2xpY2UoKSxjPVtdO2ZvcihuKGEs
by5keCpvLmR5L3QudmFsdWUpLGMuYXJlYT0wO2k9YS5wb3AoKTspYy5wdXNoKGkpLGMuYXJlYSs9
aS5hcmVhLG51bGwhPWkueiYmKHUoYyxpLno/by5keDpvLmR5LG8sIWEubGVuZ3RoKSxjLmxlbmd0
aD1jLmFyZWE9MCk7ci5mb3JFYWNoKGUpfX1mdW5jdGlvbiByKG4sdCl7Zm9yKHZhciBlLHI9bi5h
cmVhLHU9MCxpPTEvMCxvPS0xLGE9bi5sZW5ndGg7KytvPGE7KShlPW5bb10uYXJlYSkmJihpPmUm
JihpPWUpLGU+dSYmKHU9ZSkpO3JldHVybiByKj1yLHQqPXQscj9NYXRoLm1heCh0KnUqcC9yLHIv
KHQqaSpwKSk6MS8wfWZ1bmN0aW9uIHUobix0LGUscil7dmFyIHUsaT0tMSxvPW4ubGVuZ3RoLGE9
ZS54LHM9ZS55LGw9dD9jKG4uYXJlYS90KTowO2lmKHQ9PWUuZHgpe2Zvcigocnx8bD5lLmR5KSYm
KGw9ZS5keSk7KytpPG87KXU9bltpXSx1Lng9YSx1Lnk9cyx1LmR5PWwsYSs9dS5keD1NYXRoLm1p
bihlLngrZS5keC1hLGw/Yyh1LmFyZWEvbCk6MCk7dS56PSEwLHUuZHgrPWUueCtlLmR4LWEsZS55
Kz1sLGUuZHktPWx9ZWxzZXtmb3IoKHJ8fGw+ZS5keCkmJihsPWUuZHgpOysraTxvOyl1PW5baV0s
dS54PWEsdS55PXMsdS5keD1sLHMrPXUuZHk9TWF0aC5taW4oZS55K2UuZHktcyxsP2ModS5hcmVh
L2wpOjApO3Uuej0hMSx1LmR5Kz1lLnkrZS5keS1zLGUueCs9bCxlLmR4LT1sfX1mdW5jdGlvbiBp
KHIpe3ZhciB1PW98fGEociksaT11WzBdO3JldHVybiBpLng9MCxpLnk9MCxpLmR4PXNbMF0saS5k
eT1zWzFdLG8mJmEucmV2YWx1ZShpKSxuKFtpXSxpLmR4KmkuZHkvaS52YWx1ZSksKG8/ZTp0KShp
KSxoJiYobz11KSx1fXZhciBvLGE9R28ubGF5b3V0LmhpZXJhcmNoeSgpLGM9TWF0aC5yb3VuZCxz
PVsxLDFdLGw9bnVsbCxmPVBpLGg9ITEsZz0ic3F1YXJpZnkiLHA9LjUqKDErTWF0aC5zcXJ0KDUp
KTtyZXR1cm4gaS5zaXplPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhzPW4s
aSk6c30saS5wYWRkaW5nPWZ1bmN0aW9uKG4pe2Z1bmN0aW9uIHQodCl7dmFyIGU9bi5jYWxsKGks
dCx0LmRlcHRoKTtyZXR1cm4gbnVsbD09ZT9QaSh0KTpVaSh0LCJudW1iZXIiPT10eXBlb2YgZT9b
ZSxlLGUsZV06ZSl9ZnVuY3Rpb24gZSh0KXtyZXR1cm4gVWkodCxuKX1pZighYXJndW1lbnRzLmxl
bmd0aClyZXR1cm4gbDt2YXIgcjtyZXR1cm4gZj1udWxsPT0obD1uKT9QaToiZnVuY3Rpb24iPT0o
cj10eXBlb2Ygbik/dDoibnVtYmVyIj09PXI/KG49W24sbixuLG5dLGUpOmUsaX0saS5yb3VuZD1m
dW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oYz1uP01hdGgucm91bmQ6TnVtYmVy
LGkpOmMhPU51bWJlcn0saS5zdGlja3k9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5n
dGg/KGg9bixvPW51bGwsaSk6aH0saS5yYXRpbz1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRz
Lmxlbmd0aD8ocD1uLGkpOnB9LGkubW9kZT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxl
bmd0aD8oZz1uKyIiLGkpOmd9LFd1KGksYSl9LEdvLnJhbmRvbT17bm9ybWFsOmZ1bmN0aW9uKG4s
dCl7dmFyIGU9YXJndW1lbnRzLmxlbmd0aDtyZXR1cm4gMj5lJiYodD0xKSwxPmUmJihuPTApLGZ1
bmN0aW9uKCl7dmFyIGUscix1O2RvIGU9MipNYXRoLnJhbmRvbSgpLTEscj0yKk1hdGgucmFuZG9t
KCktMSx1PWUqZStyKnI7d2hpbGUoIXV8fHU+MSk7cmV0dXJuIG4rdCplKk1hdGguc3FydCgtMipN
YXRoLmxvZyh1KS91KX19LGxvZ05vcm1hbDpmdW5jdGlvbigpe3ZhciBuPUdvLnJhbmRvbS5ub3Jt
YWwuYXBwbHkoR28sYXJndW1lbnRzKTtyZXR1cm4gZnVuY3Rpb24oKXtyZXR1cm4gTWF0aC5leHAo
bigpKX19LGJhdGVzOmZ1bmN0aW9uKG4pe3ZhciB0PUdvLnJhbmRvbS5pcndpbkhhbGwobik7cmV0
dXJuIGZ1bmN0aW9uKCl7cmV0dXJuIHQoKS9ufX0saXJ3aW5IYWxsOmZ1bmN0aW9uKG4pe3JldHVy
biBmdW5jdGlvbigpe2Zvcih2YXIgdD0wLGU9MDtuPmU7ZSsrKXQrPU1hdGgucmFuZG9tKCk7cmV0
dXJuIHR9fX0sR28uc2NhbGU9e307dmFyIHZzPXtmbG9vcjpBdCxjZWlsOkF0fTtHby5zY2FsZS5s
aW5lYXI9ZnVuY3Rpb24oKXtyZXR1cm4gWmkoWzAsMV0sWzAsMV0sZHUsITEpfTt2YXIgZHM9e3M6
MSxnOjEscDoxLHI6MSxlOjF9O0dvLnNjYWxlLmxvZz1mdW5jdGlvbigpe3JldHVybiBLaShHby5z
Y2FsZS5saW5lYXIoKS5kb21haW4oWzAsMV0pLDEwLCEwLFsxLDEwXSl9O3ZhciBtcz1Hby5mb3Jt
YXQoIi4wZSIpLHlzPXtmbG9vcjpmdW5jdGlvbihuKXtyZXR1cm4tTWF0aC5jZWlsKC1uKX0sY2Vp
bDpmdW5jdGlvbihuKXtyZXR1cm4tTWF0aC5mbG9vcigtbil9fTtHby5zY2FsZS5wb3c9ZnVuY3Rp
b24oKXtyZXR1cm4gUWkoR28uc2NhbGUubGluZWFyKCksMSxbMCwxXSl9LEdvLnNjYWxlLnNxcnQ9
ZnVuY3Rpb24oKXtyZXR1cm4gR28uc2NhbGUucG93KCkuZXhwb25lbnQoLjUpfSxHby5zY2FsZS5v
cmRpbmFsPWZ1bmN0aW9uKCl7cmV0dXJuIHRvKFtdLHt0OiJyYW5nZSIsYTpbW11dfSl9LEdvLnNj
YWxlLmNhdGVnb3J5MTA9ZnVuY3Rpb24oKXtyZXR1cm4gR28uc2NhbGUub3JkaW5hbCgpLnJhbmdl
KHhzKX0sR28uc2NhbGUuY2F0ZWdvcnkyMD1mdW5jdGlvbigpe3JldHVybiBHby5zY2FsZS5vcmRp
bmFsKCkucmFuZ2UoTXMpfSxHby5zY2FsZS5jYXRlZ29yeTIwYj1mdW5jdGlvbigpe3JldHVybiBH
by5zY2FsZS5vcmRpbmFsKCkucmFuZ2UoX3MpfSxHby5zY2FsZS5jYXRlZ29yeTIwYz1mdW5jdGlv
bigpe3JldHVybiBHby5zY2FsZS5vcmRpbmFsKCkucmFuZ2UoYnMpfTt2YXIgeHM9WzIwNjIyNjAs
MTY3NDQyMDYsMjkyNDU4OCwxNDAzNDcyOCw5NzI1ODg1LDkxOTcxMzEsMTQ5MDczMzAsODM1NTcx
MSwxMjM2OTE4NiwxNTU2MTc1XS5tYXAobXQpLE1zPVsyMDYyMjYwLDExNDU0NDQwLDE2NzQ0MjA2
LDE2NzU5NjcyLDI5MjQ1ODgsMTAwMTg2OTgsMTQwMzQ3MjgsMTY3NTA3NDIsOTcyNTg4NSwxMjk1
NTg2MSw5MTk3MTMxLDEyODg1MTQwLDE0OTA3MzMwLDE2MjM0MTk0LDgzNTU3MTEsMTMwOTI4MDcs
MTIzNjkxODYsMTQ0MDg1ODksMTU1NjE3NSwxMDQxMDcyNV0ubWFwKG10KSxfcz1bMzc1MDc3Nyw1
Mzk1NjE5LDcwNDA3MTksMTAyNjQyODYsNjUxOTA5Nyw5MjE2NTk0LDExOTE1MTE1LDEzNTU2NjM2
LDkyMDI5OTMsMTI0MjY4MDksMTUxODY1MTQsMTUxOTA5MzIsODY2NjE2OSwxMTM1NjQ5MCwxNDA0
OTY0MywxNTE3NzM3Miw4MDc3NjgzLDEwODM0MzI0LDEzNTI4NTA5LDE0NTg5NjU0XS5tYXAobXQp
LGJzPVszMjQ0NzMzLDcwNTcxMTAsMTA0MDY2MjUsMTMwMzI0MzEsMTUwOTUwNTMsMTY2MTY3NjQs
MTY2MjUyNTksMTY2MzQwMTgsMzI1MzA3Niw3NjUyNDcwLDEwNjA3MDAzLDEzMTAxNTA0LDc2OTUy
ODEsMTAzOTQzMTIsMTIzNjkzNzIsMTQzNDI4OTEsNjUxMzUwNyw5ODY4OTUwLDEyNDM0ODc3LDE0
Mjc3MDgxXS5tYXAobXQpO0dvLnNjYWxlLnF1YW50aWxlPWZ1bmN0aW9uKCl7cmV0dXJuIGVvKFtd
LFtdKX0sR28uc2NhbGUucXVhbnRpemU9ZnVuY3Rpb24oKXtyZXR1cm4gcm8oMCwxLFswLDFdKX0s
R28uc2NhbGUudGhyZXNob2xkPWZ1bmN0aW9uKCl7cmV0dXJuIHVvKFsuNV0sWzAsMV0pfSxHby5z
Y2FsZS5pZGVudGl0eT1mdW5jdGlvbigpe3JldHVybiBpbyhbMCwxXSl9LEdvLnN2Zz17fSxHby5z
dmcuYXJjPWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbigpe3ZhciBuPXQuYXBwbHkodGhpcyxhcmd1bWVu
dHMpLGk9ZS5hcHBseSh0aGlzLGFyZ3VtZW50cyksbz1yLmFwcGx5KHRoaXMsYXJndW1lbnRzKSt3
cyxhPXUuYXBwbHkodGhpcyxhcmd1bWVudHMpK3dzLGM9KG8+YSYmKGM9byxvPWEsYT1jKSxhLW8p
LHM9Q2E+Yz8iMCI6IjEiLGw9TWF0aC5jb3MobyksZj1NYXRoLnNpbihvKSxoPU1hdGguY29zKGEp
LGc9TWF0aC5zaW4oYSk7CnJldHVybiBjPj1Tcz9uPyJNMCwiK2krIkEiK2krIiwiK2krIiAwIDEs
MSAwLCIrLWkrIkEiK2krIiwiK2krIiAwIDEsMSAwLCIraSsiTTAsIituKyJBIituKyIsIituKyIg
MCAxLDAgMCwiKy1uKyJBIituKyIsIituKyIgMCAxLDAgMCwiK24rIloiOiJNMCwiK2krIkEiK2kr
IiwiK2krIiAwIDEsMSAwLCIrLWkrIkEiK2krIiwiK2krIiAwIDEsMSAwLCIraSsiWiI6bj8iTSIr
aSpsKyIsIitpKmYrIkEiK2krIiwiK2krIiAwICIrcysiLDEgIitpKmgrIiwiK2kqZysiTCIrbipo
KyIsIituKmcrIkEiK24rIiwiK24rIiAwICIrcysiLDAgIituKmwrIiwiK24qZisiWiI6Ik0iK2kq
bCsiLCIraSpmKyJBIitpKyIsIitpKyIgMCAiK3MrIiwxICIraSpoKyIsIitpKmcrIkwwLDAiKyJa
In12YXIgdD1vbyxlPWFvLHI9Y28sdT1zbztyZXR1cm4gbi5pbm5lclJhZGl1cz1mdW5jdGlvbihl
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8odD1FdChlKSxuKTp0fSxuLm91dGVyUmFkaXVzPWZ1
bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhlPUV0KHQpLG4pOmV9LG4uc3RhcnRB
bmdsZT1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8ocj1FdCh0KSxuKTpyfSxu
LmVuZEFuZ2xlPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh1PUV0KHQpLG4p
OnV9LG4uY2VudHJvaWQ9ZnVuY3Rpb24oKXt2YXIgbj0odC5hcHBseSh0aGlzLGFyZ3VtZW50cykr
ZS5hcHBseSh0aGlzLGFyZ3VtZW50cykpLzIsaT0oci5hcHBseSh0aGlzLGFyZ3VtZW50cykrdS5h
cHBseSh0aGlzLGFyZ3VtZW50cykpLzIrd3M7cmV0dXJuW01hdGguY29zKGkpKm4sTWF0aC5zaW4o
aSkqbl19LG59O3ZhciB3cz0tTGEsU3M9TmEtVGE7R28uc3ZnLmxpbmU9ZnVuY3Rpb24oKXtyZXR1
cm4gbG8oQXQpfTt2YXIga3M9R28ubWFwKHtsaW5lYXI6Zm8sImxpbmVhci1jbG9zZWQiOmhvLHN0
ZXA6Z28sInN0ZXAtYmVmb3JlIjpwbywic3RlcC1hZnRlciI6dm8sYmFzaXM6Ym8sImJhc2lzLW9w
ZW4iOndvLCJiYXNpcy1jbG9zZWQiOlNvLGJ1bmRsZTprbyxjYXJkaW5hbDp4bywiY2FyZGluYWwt
b3BlbiI6bW8sImNhcmRpbmFsLWNsb3NlZCI6eW8sbW9ub3RvbmU6VG99KTtrcy5mb3JFYWNoKGZ1
bmN0aW9uKG4sdCl7dC5rZXk9bix0LmNsb3NlZD0vLWNsb3NlZCQvLnRlc3Qobil9KTt2YXIgRXM9
WzAsMi8zLDEvMywwXSxBcz1bMCwxLzMsMi8zLDBdLENzPVswLDEvNiwyLzMsMS82XTtHby5zdmcu
bGluZS5yYWRpYWw9ZnVuY3Rpb24oKXt2YXIgbj1sbyhxbyk7cmV0dXJuIG4ucmFkaXVzPW4ueCxk
ZWxldGUgbi54LG4uYW5nbGU9bi55LGRlbGV0ZSBuLnksbn0scG8ucmV2ZXJzZT12byx2by5yZXZl
cnNlPXBvLEdvLnN2Zy5hcmVhPWZ1bmN0aW9uKCl7cmV0dXJuIHpvKEF0KX0sR28uc3ZnLmFyZWEu
cmFkaWFsPWZ1bmN0aW9uKCl7dmFyIG49em8ocW8pO3JldHVybiBuLnJhZGl1cz1uLngsZGVsZXRl
IG4ueCxuLmlubmVyUmFkaXVzPW4ueDAsZGVsZXRlIG4ueDAsbi5vdXRlclJhZGl1cz1uLngxLGRl
bGV0ZSBuLngxLG4uYW5nbGU9bi55LGRlbGV0ZSBuLnksbi5zdGFydEFuZ2xlPW4ueTAsZGVsZXRl
IG4ueTAsbi5lbmRBbmdsZT1uLnkxLGRlbGV0ZSBuLnkxLG59LEdvLnN2Zy5jaG9yZD1mdW5jdGlv
bigpe2Z1bmN0aW9uIG4obixhKXt2YXIgYz10KHRoaXMsaSxuLGEpLHM9dCh0aGlzLG8sbixhKTty
ZXR1cm4iTSIrYy5wMCtyKGMucixjLnAxLGMuYTEtYy5hMCkrKGUoYyxzKT91KGMucixjLnAxLGMu
cixjLnAwKTp1KGMucixjLnAxLHMucixzLnAwKStyKHMucixzLnAxLHMuYTEtcy5hMCkrdShzLnIs
cy5wMSxjLnIsYy5wMCkpKyJaIn1mdW5jdGlvbiB0KG4sdCxlLHIpe3ZhciB1PXQuY2FsbChuLGUs
ciksaT1hLmNhbGwobix1LHIpLG89Yy5jYWxsKG4sdSxyKSt3cyxsPXMuY2FsbChuLHUscikrd3M7
cmV0dXJue3I6aSxhMDpvLGExOmwscDA6W2kqTWF0aC5jb3MobyksaSpNYXRoLnNpbihvKV0scDE6
W2kqTWF0aC5jb3MobCksaSpNYXRoLnNpbihsKV19fWZ1bmN0aW9uIGUobix0KXtyZXR1cm4gbi5h
MD09dC5hMCYmbi5hMT09dC5hMX1mdW5jdGlvbiByKG4sdCxlKXtyZXR1cm4iQSIrbisiLCIrbisi
IDAgIisgKyhlPkNhKSsiLDEgIit0fWZ1bmN0aW9uIHUobix0LGUscil7cmV0dXJuIlEgMCwwICIr
cn12YXIgaT1tcixvPXlyLGE9Um8sYz1jbyxzPXNvO3JldHVybiBuLnJhZGl1cz1mdW5jdGlvbih0
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oYT1FdCh0KSxuKTphfSxuLnNvdXJjZT1mdW5jdGlv
bih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oaT1FdCh0KSxuKTppfSxuLnRhcmdldD1mdW5j
dGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8obz1FdCh0KSxuKTpvfSxuLnN0YXJ0QW5n
bGU9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGM9RXQodCksbik6Y30sbi5l
bmRBbmdsZT1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8ocz1FdCh0KSxuKTpz
fSxufSxHby5zdmcuZGlhZ29uYWw9ZnVuY3Rpb24oKXtmdW5jdGlvbiBuKG4sdSl7dmFyIGk9dC5j
YWxsKHRoaXMsbix1KSxvPWUuY2FsbCh0aGlzLG4sdSksYT0oaS55K28ueSkvMixjPVtpLHt4Omku
eCx5OmF9LHt4Om8ueCx5OmF9LG9dO3JldHVybiBjPWMubWFwKHIpLCJNIitjWzBdKyJDIitjWzFd
KyIgIitjWzJdKyIgIitjWzNdfXZhciB0PW1yLGU9eXIscj1EbztyZXR1cm4gbi5zb3VyY2U9ZnVu
Y3Rpb24oZSl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHQ9RXQoZSksbik6dH0sbi50YXJnZXQ9
ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGU9RXQodCksbik6ZX0sbi5wcm9q
ZWN0aW9uPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhyPXQsbik6cn0sbn0s
R28uc3ZnLmRpYWdvbmFsLnJhZGlhbD1mdW5jdGlvbigpe3ZhciBuPUdvLnN2Zy5kaWFnb25hbCgp
LHQ9RG8sZT1uLnByb2plY3Rpb247cmV0dXJuIG4ucHJvamVjdGlvbj1mdW5jdGlvbihuKXtyZXR1
cm4gYXJndW1lbnRzLmxlbmd0aD9lKFBvKHQ9bikpOnR9LG59LEdvLnN2Zy5zeW1ib2w9ZnVuY3Rp
b24oKXtmdW5jdGlvbiBuKG4scil7cmV0dXJuKE5zLmdldCh0LmNhbGwodGhpcyxuLHIpKXx8SG8p
KGUuY2FsbCh0aGlzLG4scikpfXZhciB0PWpvLGU9VW87cmV0dXJuIG4udHlwZT1mdW5jdGlvbihl
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8odD1FdChlKSxuKTp0fSxuLnNpemU9ZnVuY3Rpb24o
dCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGU9RXQodCksbik6ZX0sbn07dmFyIE5zPUdvLm1h
cCh7Y2lyY2xlOkhvLGNyb3NzOmZ1bmN0aW9uKG4pe3ZhciB0PU1hdGguc3FydChuLzUpLzI7cmV0
dXJuIk0iKy0zKnQrIiwiKy10KyJIIistdCsiViIrLTMqdCsiSCIrdCsiViIrLXQrIkgiKzMqdCsi
ViIrdCsiSCIrdCsiViIrMyp0KyJIIistdCsiViIrdCsiSCIrLTMqdCsiWiJ9LGRpYW1vbmQ6ZnVu
Y3Rpb24obil7dmFyIHQ9TWF0aC5zcXJ0KG4vKDIqenMpKSxlPXQqenM7cmV0dXJuIk0wLCIrLXQr
IkwiK2UrIiwwIisiIDAsIit0KyIgIistZSsiLDAiKyJaIn0sc3F1YXJlOmZ1bmN0aW9uKG4pe3Zh
ciB0PU1hdGguc3FydChuKS8yO3JldHVybiJNIistdCsiLCIrLXQrIkwiK3QrIiwiKy10KyIgIit0
KyIsIit0KyIgIistdCsiLCIrdCsiWiJ9LCJ0cmlhbmdsZS1kb3duIjpmdW5jdGlvbihuKXt2YXIg
dD1NYXRoLnNxcnQobi9xcyksZT10KnFzLzI7cmV0dXJuIk0wLCIrZSsiTCIrdCsiLCIrLWUrIiAi
Ky10KyIsIistZSsiWiJ9LCJ0cmlhbmdsZS11cCI6ZnVuY3Rpb24obil7dmFyIHQ9TWF0aC5zcXJ0
KG4vcXMpLGU9dCpxcy8yO3JldHVybiJNMCwiKy1lKyJMIit0KyIsIitlKyIgIistdCsiLCIrZSsi
WiJ9fSk7R28uc3ZnLnN5bWJvbFR5cGVzPU5zLmtleXMoKTt2YXIgTHMsVHMscXM9TWF0aC5zcXJ0
KDMpLHpzPU1hdGgudGFuKDMwKnphKSxScz1bXSxEcz0wO1JzLmNhbGw9X2EuY2FsbCxScy5lbXB0
eT1fYS5lbXB0eSxScy5ub2RlPV9hLm5vZGUsUnMuc2l6ZT1fYS5zaXplLEdvLnRyYW5zaXRpb249
ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/THM/bi50cmFuc2l0aW9uKCk6bjpT
YS50cmFuc2l0aW9uKCl9LEdvLnRyYW5zaXRpb24ucHJvdG90eXBlPVJzLFJzLnNlbGVjdD1mdW5j
dGlvbihuKXt2YXIgdCxlLHIsdT10aGlzLmlkLGk9W107bj1iKG4pO2Zvcih2YXIgbz0tMSxhPXRo
aXMubGVuZ3RoOysrbzxhOyl7aS5wdXNoKHQ9W10pO2Zvcih2YXIgYz10aGlzW29dLHM9LTEsbD1j
Lmxlbmd0aDsrK3M8bDspKHI9Y1tzXSkmJihlPW4uY2FsbChyLHIuX19kYXRhX18scyxvKSk/KCJf
X2RhdGFfXyJpbiByJiYoZS5fX2RhdGFfXz1yLl9fZGF0YV9fKSxZbyhlLHMsdSxyLl9fdHJhbnNp
dGlvbl9fW3VdKSx0LnB1c2goZSkpOnQucHVzaChudWxsKX1yZXR1cm4gRm8oaSx1KX0sUnMuc2Vs
ZWN0QWxsPWZ1bmN0aW9uKG4pe3ZhciB0LGUscix1LGksbz10aGlzLmlkLGE9W107bj13KG4pO2Zv
cih2YXIgYz0tMSxzPXRoaXMubGVuZ3RoOysrYzxzOylmb3IodmFyIGw9dGhpc1tjXSxmPS0xLGg9
bC5sZW5ndGg7KytmPGg7KWlmKHI9bFtmXSl7aT1yLl9fdHJhbnNpdGlvbl9fW29dLGU9bi5jYWxs
KHIsci5fX2RhdGFfXyxmLGMpLGEucHVzaCh0PVtdKTtmb3IodmFyIGc9LTEscD1lLmxlbmd0aDsr
K2c8cDspKHU9ZVtnXSkmJllvKHUsZyxvLGkpLHQucHVzaCh1KX1yZXR1cm4gRm8oYSxvKX0sUnMu
ZmlsdGVyPWZ1bmN0aW9uKG4pe3ZhciB0LGUscix1PVtdOyJmdW5jdGlvbiIhPXR5cGVvZiBuJiYo
bj1SKG4pKTtmb3IodmFyIGk9MCxvPXRoaXMubGVuZ3RoO28+aTtpKyspe3UucHVzaCh0PVtdKTtm
b3IodmFyIGU9dGhpc1tpXSxhPTAsYz1lLmxlbmd0aDtjPmE7YSsrKShyPWVbYV0pJiZuLmNhbGwo
cixyLl9fZGF0YV9fLGEsaSkmJnQucHVzaChyKX1yZXR1cm4gRm8odSx0aGlzLmlkKX0sUnMudHdl
ZW49ZnVuY3Rpb24obix0KXt2YXIgZT10aGlzLmlkO3JldHVybiBhcmd1bWVudHMubGVuZ3RoPDI/
dGhpcy5ub2RlKCkuX190cmFuc2l0aW9uX19bZV0udHdlZW4uZ2V0KG4pOlAodGhpcyxudWxsPT10
P2Z1bmN0aW9uKHQpe3QuX190cmFuc2l0aW9uX19bZV0udHdlZW4ucmVtb3ZlKG4pfTpmdW5jdGlv
bihyKXtyLl9fdHJhbnNpdGlvbl9fW2VdLnR3ZWVuLnNldChuLHQpfSl9LFJzLmF0dHI9ZnVuY3Rp
b24obix0KXtmdW5jdGlvbiBlKCl7dGhpcy5yZW1vdmVBdHRyaWJ1dGUoYSl9ZnVuY3Rpb24gcigp
e3RoaXMucmVtb3ZlQXR0cmlidXRlTlMoYS5zcGFjZSxhLmxvY2FsKX1mdW5jdGlvbiB1KG4pe3Jl
dHVybiBudWxsPT1uP2U6KG4rPSIiLGZ1bmN0aW9uKCl7dmFyIHQsZT10aGlzLmdldEF0dHJpYnV0
ZShhKTtyZXR1cm4gZSE9PW4mJih0PW8oZSxuKSxmdW5jdGlvbihuKXt0aGlzLnNldEF0dHJpYnV0
ZShhLHQobikpfSl9KX1mdW5jdGlvbiBpKG4pe3JldHVybiBudWxsPT1uP3I6KG4rPSIiLGZ1bmN0
aW9uKCl7dmFyIHQsZT10aGlzLmdldEF0dHJpYnV0ZU5TKGEuc3BhY2UsYS5sb2NhbCk7cmV0dXJu
IGUhPT1uJiYodD1vKGUsbiksZnVuY3Rpb24obil7dGhpcy5zZXRBdHRyaWJ1dGVOUyhhLnNwYWNl
LGEubG9jYWwsdChuKSl9KX0pfWlmKGFyZ3VtZW50cy5sZW5ndGg8Mil7Zm9yKHQgaW4gbil0aGlz
LmF0dHIodCxuW3RdKTtyZXR1cm4gdGhpc312YXIgbz0idHJhbnNmb3JtIj09bj9IdTpkdSxhPUdv
Lm5zLnF1YWxpZnkobik7cmV0dXJuIE9vKHRoaXMsImF0dHIuIituLHQsYS5sb2NhbD9pOnUpfSxS
cy5hdHRyVHdlZW49ZnVuY3Rpb24obix0KXtmdW5jdGlvbiBlKG4sZSl7dmFyIHI9dC5jYWxsKHRo
aXMsbixlLHRoaXMuZ2V0QXR0cmlidXRlKHUpKTtyZXR1cm4gciYmZnVuY3Rpb24obil7dGhpcy5z
ZXRBdHRyaWJ1dGUodSxyKG4pKX19ZnVuY3Rpb24gcihuLGUpe3ZhciByPXQuY2FsbCh0aGlzLG4s
ZSx0aGlzLmdldEF0dHJpYnV0ZU5TKHUuc3BhY2UsdS5sb2NhbCkpO3JldHVybiByJiZmdW5jdGlv
bihuKXt0aGlzLnNldEF0dHJpYnV0ZU5TKHUuc3BhY2UsdS5sb2NhbCxyKG4pKX19dmFyIHU9R28u
bnMucXVhbGlmeShuKTtyZXR1cm4gdGhpcy50d2VlbigiYXR0ci4iK24sdS5sb2NhbD9yOmUpfSxS
cy5zdHlsZT1mdW5jdGlvbihuLHQsZSl7ZnVuY3Rpb24gcigpe3RoaXMuc3R5bGUucmVtb3ZlUHJv
cGVydHkobil9ZnVuY3Rpb24gdSh0KXtyZXR1cm4gbnVsbD09dD9yOih0Kz0iIixmdW5jdGlvbigp
e3ZhciByLHU9ZWEuZ2V0Q29tcHV0ZWRTdHlsZSh0aGlzLG51bGwpLmdldFByb3BlcnR5VmFsdWUo
bik7cmV0dXJuIHUhPT10JiYocj1kdSh1LHQpLGZ1bmN0aW9uKHQpe3RoaXMuc3R5bGUuc2V0UHJv
cGVydHkobixyKHQpLGUpfSl9KX12YXIgaT1hcmd1bWVudHMubGVuZ3RoO2lmKDM+aSl7aWYoInN0
cmluZyIhPXR5cGVvZiBuKXsyPmkmJih0PSIiKTtmb3IoZSBpbiBuKXRoaXMuc3R5bGUoZSxuW2Vd
LHQpO3JldHVybiB0aGlzfWU9IiJ9cmV0dXJuIE9vKHRoaXMsInN0eWxlLiIrbix0LHUpfSxScy5z
dHlsZVR3ZWVuPWZ1bmN0aW9uKG4sdCxlKXtmdW5jdGlvbiByKHIsdSl7dmFyIGk9dC5jYWxsKHRo
aXMscix1LGVhLmdldENvbXB1dGVkU3R5bGUodGhpcyxudWxsKS5nZXRQcm9wZXJ0eVZhbHVlKG4p
KTtyZXR1cm4gaSYmZnVuY3Rpb24odCl7dGhpcy5zdHlsZS5zZXRQcm9wZXJ0eShuLGkodCksZSl9
fXJldHVybiBhcmd1bWVudHMubGVuZ3RoPDMmJihlPSIiKSx0aGlzLnR3ZWVuKCJzdHlsZS4iK24s
cil9LFJzLnRleHQ9ZnVuY3Rpb24obil7cmV0dXJuIE9vKHRoaXMsInRleHQiLG4sSW8pfSxScy5y
ZW1vdmU9ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5lYWNoKCJlbmQudHJhbnNpdGlvbiIsZnVuY3Rp
b24oKXt2YXIgbjt0aGlzLl9fdHJhbnNpdGlvbl9fLmNvdW50PDImJihuPXRoaXMucGFyZW50Tm9k
ZSkmJm4ucmVtb3ZlQ2hpbGQodGhpcyl9KX0sUnMuZWFzZT1mdW5jdGlvbihuKXt2YXIgdD10aGlz
LmlkO3JldHVybiBhcmd1bWVudHMubGVuZ3RoPDE/dGhpcy5ub2RlKCkuX190cmFuc2l0aW9uX19b
dF0uZWFzZTooImZ1bmN0aW9uIiE9dHlwZW9mIG4mJihuPUdvLmVhc2UuYXBwbHkoR28sYXJndW1l
bnRzKSksUCh0aGlzLGZ1bmN0aW9uKGUpe2UuX190cmFuc2l0aW9uX19bdF0uZWFzZT1ufSkpfSxS
cy5kZWxheT1mdW5jdGlvbihuKXt2YXIgdD10aGlzLmlkO3JldHVybiBhcmd1bWVudHMubGVuZ3Ro
PDE/dGhpcy5ub2RlKCkuX190cmFuc2l0aW9uX19bdF0uZGVsYXk6UCh0aGlzLCJmdW5jdGlvbiI9
PXR5cGVvZiBuP2Z1bmN0aW9uKGUscix1KXtlLl9fdHJhbnNpdGlvbl9fW3RdLmRlbGF5PStuLmNh
bGwoZSxlLl9fZGF0YV9fLHIsdSl9OihuPStuLGZ1bmN0aW9uKGUpe2UuX190cmFuc2l0aW9uX19b
dF0uZGVsYXk9bn0pKX0sUnMuZHVyYXRpb249ZnVuY3Rpb24obil7dmFyIHQ9dGhpcy5pZDtyZXR1
cm4gYXJndW1lbnRzLmxlbmd0aDwxP3RoaXMubm9kZSgpLl9fdHJhbnNpdGlvbl9fW3RdLmR1cmF0
aW9uOlAodGhpcywiZnVuY3Rpb24iPT10eXBlb2Ygbj9mdW5jdGlvbihlLHIsdSl7ZS5fX3RyYW5z
aXRpb25fX1t0XS5kdXJhdGlvbj1NYXRoLm1heCgxLG4uY2FsbChlLGUuX19kYXRhX18scix1KSl9
OihuPU1hdGgubWF4KDEsbiksZnVuY3Rpb24oZSl7ZS5fX3RyYW5zaXRpb25fX1t0XS5kdXJhdGlv
bj1ufSkpfSxScy5lYWNoPWZ1bmN0aW9uKG4sdCl7dmFyIGU9dGhpcy5pZDtpZihhcmd1bWVudHMu
bGVuZ3RoPDIpe3ZhciByPVRzLHU9THM7THM9ZSxQKHRoaXMsZnVuY3Rpb24odCxyLHUpe1RzPXQu
X190cmFuc2l0aW9uX19bZV0sbi5jYWxsKHQsdC5fX2RhdGFfXyxyLHUpfSksVHM9cixMcz11fWVs
c2UgUCh0aGlzLGZ1bmN0aW9uKHIpe3ZhciB1PXIuX190cmFuc2l0aW9uX19bZV07KHUuZXZlbnR8
fCh1LmV2ZW50PUdvLmRpc3BhdGNoKCJzdGFydCIsImVuZCIpKSkub24obix0KX0pO3JldHVybiB0
aGlzfSxScy50cmFuc2l0aW9uPWZ1bmN0aW9uKCl7Zm9yKHZhciBuLHQsZSxyLHU9dGhpcy5pZCxp
PSsrRHMsbz1bXSxhPTAsYz10aGlzLmxlbmd0aDtjPmE7YSsrKXtvLnB1c2gobj1bXSk7Zm9yKHZh
ciB0PXRoaXNbYV0scz0wLGw9dC5sZW5ndGg7bD5zO3MrKykoZT10W3NdKSYmKHI9T2JqZWN0LmNy
ZWF0ZShlLl9fdHJhbnNpdGlvbl9fW3VdKSxyLmRlbGF5Kz1yLmR1cmF0aW9uLFlvKGUscyxpLHIp
KSxuLnB1c2goZSl9cmV0dXJuIEZvKG8saSl9LEdvLnN2Zy5heGlzPWZ1bmN0aW9uKCl7ZnVuY3Rp
b24gbihuKXtuLmVhY2goZnVuY3Rpb24oKXt2YXIgbixzPUdvLnNlbGVjdCh0aGlzKSxsPXRoaXMu
X19jaGFydF9ffHxlLGY9dGhpcy5fX2NoYXJ0X189ZS5jb3B5KCksaD1udWxsPT1jP2YudGlja3M/
Zi50aWNrcy5hcHBseShmLGEpOmYuZG9tYWluKCk6YyxnPW51bGw9PXQ/Zi50aWNrRm9ybWF0P2Yu
dGlja0Zvcm1hdC5hcHBseShmLGEpOkF0OnQscD1zLnNlbGVjdEFsbCgiLnRpY2siKS5kYXRhKGgs
Ziksdj1wLmVudGVyKCkuaW5zZXJ0KCJnIiwiLmRvbWFpbiIpLmF0dHIoImNsYXNzIiwidGljayIp
LnN0eWxlKCJvcGFjaXR5IixUYSksZD1Hby50cmFuc2l0aW9uKHAuZXhpdCgpKS5zdHlsZSgib3Bh
Y2l0eSIsVGEpLnJlbW92ZSgpLG09R28udHJhbnNpdGlvbihwLm9yZGVyKCkpLnN0eWxlKCJvcGFj
aXR5IiwxKSx5PUhpKGYpLHg9cy5zZWxlY3RBbGwoIi5kb21haW4iKS5kYXRhKFswXSksTT0oeC5l
bnRlcigpLmFwcGVuZCgicGF0aCIpLmF0dHIoImNsYXNzIiwiZG9tYWluIiksR28udHJhbnNpdGlv
bih4KSk7di5hcHBlbmQoImxpbmUiKSx2LmFwcGVuZCgidGV4dCIpO3ZhciBfPXYuc2VsZWN0KCJs
aW5lIiksYj1tLnNlbGVjdCgibGluZSIpLHc9cC5zZWxlY3QoInRleHQiKS50ZXh0KGcpLFM9di5z
ZWxlY3QoInRleHQiKSxrPW0uc2VsZWN0KCJ0ZXh0Iik7c3dpdGNoKHIpe2Nhc2UiYm90dG9tIjpu
PVpvLF8uYXR0cigieTIiLHUpLFMuYXR0cigieSIsTWF0aC5tYXgodSwwKStvKSxiLmF0dHIoIngy
IiwwKS5hdHRyKCJ5MiIsdSksay5hdHRyKCJ4IiwwKS5hdHRyKCJ5IixNYXRoLm1heCh1LDApK28p
LHcuYXR0cigiZHkiLCIuNzFlbSIpLnN0eWxlKCJ0ZXh0LWFuY2hvciIsIm1pZGRsZSIpLE0uYXR0
cigiZCIsIk0iK3lbMF0rIiwiK2krIlYwSCIreVsxXSsiViIraSk7YnJlYWs7Y2FzZSJ0b3AiOm49
Wm8sXy5hdHRyKCJ5MiIsLXUpLFMuYXR0cigieSIsLShNYXRoLm1heCh1LDApK28pKSxiLmF0dHIo
IngyIiwwKS5hdHRyKCJ5MiIsLXUpLGsuYXR0cigieCIsMCkuYXR0cigieSIsLShNYXRoLm1heCh1
LDApK28pKSx3LmF0dHIoImR5IiwiMGVtIikuc3R5bGUoInRleHQtYW5jaG9yIiwibWlkZGxlIiks
TS5hdHRyKCJkIiwiTSIreVswXSsiLCIrLWkrIlYwSCIreVsxXSsiViIrLWkpO2JyZWFrO2Nhc2Ui
bGVmdCI6bj1WbyxfLmF0dHIoIngyIiwtdSksUy5hdHRyKCJ4IiwtKE1hdGgubWF4KHUsMCkrbykp
LGIuYXR0cigieDIiLC11KS5hdHRyKCJ5MiIsMCksay5hdHRyKCJ4IiwtKE1hdGgubWF4KHUsMCkr
bykpLmF0dHIoInkiLDApLHcuYXR0cigiZHkiLCIuMzJlbSIpLnN0eWxlKCJ0ZXh0LWFuY2hvciIs
ImVuZCIpLE0uYXR0cigiZCIsIk0iKy1pKyIsIit5WzBdKyJIMFYiK3lbMV0rIkgiKy1pKTticmVh
aztjYXNlInJpZ2h0IjpuPVZvLF8uYXR0cigieDIiLHUpLFMuYXR0cigieCIsTWF0aC5tYXgodSww
KStvKSxiLmF0dHIoIngyIix1KS5hdHRyKCJ5MiIsMCksay5hdHRyKCJ4IixNYXRoLm1heCh1LDAp
K28pLmF0dHIoInkiLDApLHcuYXR0cigiZHkiLCIuMzJlbSIpLnN0eWxlKCJ0ZXh0LWFuY2hvciIs
InN0YXJ0IiksTS5hdHRyKCJkIiwiTSIraSsiLCIreVswXSsiSDBWIit5WzFdKyJIIitpKX1pZihm
LnJhbmdlQmFuZCl7dmFyIEU9ZixBPUUucmFuZ2VCYW5kKCkvMjtsPWY9ZnVuY3Rpb24obil7cmV0
dXJuIEUobikrQX19ZWxzZSBsLnJhbmdlQmFuZD9sPWY6ZC5jYWxsKG4sZik7di5jYWxsKG4sbCks
bS5jYWxsKG4sZil9KX12YXIgdCxlPUdvLnNjYWxlLmxpbmVhcigpLHI9UHMsdT02LGk9NixvPTMs
YT1bMTBdLGM9bnVsbDtyZXR1cm4gbi5zY2FsZT1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRz
Lmxlbmd0aD8oZT10LG4pOmV9LG4ub3JpZW50PWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMu
bGVuZ3RoPyhyPXQgaW4gVXM/dCsiIjpQcyxuKTpyfSxuLnRpY2tzPWZ1bmN0aW9uKCl7cmV0dXJu
IGFyZ3VtZW50cy5sZW5ndGg/KGE9YXJndW1lbnRzLG4pOmF9LG4udGlja1ZhbHVlcz1mdW5jdGlv
bih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oYz10LG4pOmN9LG4udGlja0Zvcm1hdD1mdW5j
dGlvbihlKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8odD1lLG4pOnR9LG4udGlja1NpemU9ZnVu
Y3Rpb24odCl7dmFyIGU9YXJndW1lbnRzLmxlbmd0aDtyZXR1cm4gZT8odT0rdCxpPSthcmd1bWVu
dHNbZS0xXSxuKTp1fSxuLmlubmVyVGlja1NpemU9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50
cy5sZW5ndGg/KHU9K3Qsbik6dX0sbi5vdXRlclRpY2tTaXplPWZ1bmN0aW9uKHQpe3JldHVybiBh
cmd1bWVudHMubGVuZ3RoPyhpPSt0LG4pOml9LG4udGlja1BhZGRpbmc9ZnVuY3Rpb24odCl7cmV0
dXJuIGFyZ3VtZW50cy5sZW5ndGg/KG89K3Qsbik6b30sbi50aWNrU3ViZGl2aWRlPWZ1bmN0aW9u
KCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGgmJm59LG59O3ZhciBQcz0iYm90dG9tIixVcz17dG9w
OjEscmlnaHQ6MSxib3R0b206MSxsZWZ0OjF9O0dvLnN2Zy5icnVzaD1mdW5jdGlvbigpe2Z1bmN0
aW9uIG4oaSl7aS5lYWNoKGZ1bmN0aW9uKCl7dmFyIGk9R28uc2VsZWN0KHRoaXMpLnN0eWxlKCJw
b2ludGVyLWV2ZW50cyIsImFsbCIpLnN0eWxlKCItd2Via2l0LXRhcC1oaWdobGlnaHQtY29sb3Ii
LCJyZ2JhKDAsMCwwLDApIikub24oIm1vdXNlZG93bi5icnVzaCIsdSkub24oInRvdWNoc3RhcnQu
YnJ1c2giLHUpLG89aS5zZWxlY3RBbGwoIi5iYWNrZ3JvdW5kIikuZGF0YShbMF0pO28uZW50ZXIo
KS5hcHBlbmQoInJlY3QiKS5hdHRyKCJjbGFzcyIsImJhY2tncm91bmQiKS5zdHlsZSgidmlzaWJp
bGl0eSIsImhpZGRlbiIpLnN0eWxlKCJjdXJzb3IiLCJjcm9zc2hhaXIiKSxpLnNlbGVjdEFsbCgi
LmV4dGVudCIpLmRhdGEoWzBdKS5lbnRlcigpLmFwcGVuZCgicmVjdCIpLmF0dHIoImNsYXNzIiwi
ZXh0ZW50Iikuc3R5bGUoImN1cnNvciIsIm1vdmUiKTt2YXIgYT1pLnNlbGVjdEFsbCgiLnJlc2l6
ZSIpLmRhdGEocCxBdCk7YS5leGl0KCkucmVtb3ZlKCksYS5lbnRlcigpLmFwcGVuZCgiZyIpLmF0
dHIoImNsYXNzIixmdW5jdGlvbihuKXtyZXR1cm4icmVzaXplICIrbn0pLnN0eWxlKCJjdXJzb3Ii
LGZ1bmN0aW9uKG4pe3JldHVybiBqc1tuXX0pLmFwcGVuZCgicmVjdCIpLmF0dHIoIngiLGZ1bmN0
aW9uKG4pe3JldHVybi9bZXddJC8udGVzdChuKT8tMzpudWxsfSkuYXR0cigieSIsZnVuY3Rpb24o
bil7cmV0dXJuL15bbnNdLy50ZXN0KG4pPy0zOm51bGx9KS5hdHRyKCJ3aWR0aCIsNikuYXR0cigi
aGVpZ2h0Iiw2KS5zdHlsZSgidmlzaWJpbGl0eSIsImhpZGRlbiIpLGEuc3R5bGUoImRpc3BsYXki
LG4uZW1wdHkoKT8ibm9uZSI6bnVsbCk7dmFyIGwsZj1Hby50cmFuc2l0aW9uKGkpLGg9R28udHJh
bnNpdGlvbihvKTtjJiYobD1IaShjKSxoLmF0dHIoIngiLGxbMF0pLmF0dHIoIndpZHRoIixsWzFd
LWxbMF0pLGUoZikpLHMmJihsPUhpKHMpLGguYXR0cigieSIsbFswXSkuYXR0cigiaGVpZ2h0Iixs
WzFdLWxbMF0pLHIoZikpLHQoZil9KX1mdW5jdGlvbiB0KG4pe24uc2VsZWN0QWxsKCIucmVzaXpl
IikuYXR0cigidHJhbnNmb3JtIixmdW5jdGlvbihuKXtyZXR1cm4idHJhbnNsYXRlKCIrbFsrL2Uk
Ly50ZXN0KG4pXSsiLCIrZlsrL15zLy50ZXN0KG4pXSsiKSJ9KX1mdW5jdGlvbiBlKG4pe24uc2Vs
ZWN0KCIuZXh0ZW50IikuYXR0cigieCIsbFswXSksbi5zZWxlY3RBbGwoIi5leHRlbnQsLm4+cmVj
dCwucz5yZWN0IikuYXR0cigid2lkdGgiLGxbMV0tbFswXSl9ZnVuY3Rpb24gcihuKXtuLnNlbGVj
dCgiLmV4dGVudCIpLmF0dHIoInkiLGZbMF0pLG4uc2VsZWN0QWxsKCIuZXh0ZW50LC5lPnJlY3Qs
Lnc+cmVjdCIpLmF0dHIoImhlaWdodCIsZlsxXS1mWzBdKX1mdW5jdGlvbiB1KCl7ZnVuY3Rpb24g
dSgpezMyPT1Hby5ldmVudC5rZXlDb2RlJiYoQ3x8KHg9bnVsbCxMWzBdLT1sWzFdLExbMV0tPWZb
MV0sQz0yKSx5KCkpfWZ1bmN0aW9uIHAoKXszMj09R28uZXZlbnQua2V5Q29kZSYmMj09QyYmKExb
MF0rPWxbMV0sTFsxXSs9ZlsxXSxDPTAseSgpKX1mdW5jdGlvbiB2KCl7dmFyIG49R28ubW91c2Uo
XyksdT0hMTtNJiYoblswXSs9TVswXSxuWzFdKz1NWzFdKSxDfHwoR28uZXZlbnQuYWx0S2V5Pyh4
fHwoeD1bKGxbMF0rbFsxXSkvMiwoZlswXStmWzFdKS8yXSksTFswXT1sWysoblswXTx4WzBdKV0s
TFsxXT1mWysoblsxXTx4WzFdKV0pOng9bnVsbCksRSYmZChuLGMsMCkmJihlKFMpLHU9ITApLEEm
JmQobixzLDEpJiYocihTKSx1PSEwKSx1JiYodChTKSx3KHt0eXBlOiJicnVzaCIsbW9kZTpDPyJt
b3ZlIjoicmVzaXplIn0pKX1mdW5jdGlvbiBkKG4sdCxlKXt2YXIgcix1LGE9SGkodCksYz1hWzBd
LHM9YVsxXSxwPUxbZV0sdj1lP2Y6bCxkPXZbMV0tdlswXTtyZXR1cm4gQyYmKGMtPXAscy09ZCtw
KSxyPShlP2c6aCk/TWF0aC5tYXgoYyxNYXRoLm1pbihzLG5bZV0pKTpuW2VdLEM/dT0ocis9cCkr
ZDooeCYmKHA9TWF0aC5tYXgoYyxNYXRoLm1pbihzLDIqeFtlXS1yKSkpLHI+cD8odT1yLHI9cCk6
dT1wKSx2WzBdIT1yfHx2WzFdIT11PyhlP289bnVsbDppPW51bGwsdlswXT1yLHZbMV09dSwhMCk6
dm9pZCAwfWZ1bmN0aW9uIG0oKXt2KCksUy5zdHlsZSgicG9pbnRlci1ldmVudHMiLCJhbGwiKS5z
ZWxlY3RBbGwoIi5yZXNpemUiKS5zdHlsZSgiZGlzcGxheSIsbi5lbXB0eSgpPyJub25lIjpudWxs
KSxHby5zZWxlY3QoImJvZHkiKS5zdHlsZSgiY3Vyc29yIixudWxsKSxULm9uKCJtb3VzZW1vdmUu
YnJ1c2giLG51bGwpLm9uKCJtb3VzZXVwLmJydXNoIixudWxsKS5vbigidG91Y2htb3ZlLmJydXNo
IixudWxsKS5vbigidG91Y2hlbmQuYnJ1c2giLG51bGwpLm9uKCJrZXlkb3duLmJydXNoIixudWxs
KS5vbigia2V5dXAuYnJ1c2giLG51bGwpLE4oKSx3KHt0eXBlOiJicnVzaGVuZCJ9KX12YXIgeCxN
LF89dGhpcyxiPUdvLnNlbGVjdChHby5ldmVudC50YXJnZXQpLHc9YS5vZihfLGFyZ3VtZW50cyks
Uz1Hby5zZWxlY3QoXyksaz1iLmRhdHVtKCksRT0hL14obnxzKSQvLnRlc3QoaykmJmMsQT0hL14o
ZXx3KSQvLnRlc3QoaykmJnMsQz1iLmNsYXNzZWQoImV4dGVudCIpLE49WSgpLEw9R28ubW91c2Uo
XyksVD1Hby5zZWxlY3QoZWEpLm9uKCJrZXlkb3duLmJydXNoIix1KS5vbigia2V5dXAuYnJ1c2gi
LHApO2lmKEdvLmV2ZW50LmNoYW5nZWRUb3VjaGVzP1Qub24oInRvdWNobW92ZS5icnVzaCIsdiku
b24oInRvdWNoZW5kLmJydXNoIixtKTpULm9uKCJtb3VzZW1vdmUuYnJ1c2giLHYpLm9uKCJtb3Vz
ZXVwLmJydXNoIixtKSxTLmludGVycnVwdCgpLnNlbGVjdEFsbCgiKiIpLmludGVycnVwdCgpLEMp
TFswXT1sWzBdLUxbMF0sTFsxXT1mWzBdLUxbMV07ZWxzZSBpZihrKXt2YXIgcT0rL3ckLy50ZXN0
KGspLHo9Ky9ebi8udGVzdChrKTtNPVtsWzEtcV0tTFswXSxmWzEtel0tTFsxXV0sTFswXT1sW3Fd
LExbMV09Zlt6XX1lbHNlIEdvLmV2ZW50LmFsdEtleSYmKHg9TC5zbGljZSgpKTtTLnN0eWxlKCJw
b2ludGVyLWV2ZW50cyIsIm5vbmUiKS5zZWxlY3RBbGwoIi5yZXNpemUiKS5zdHlsZSgiZGlzcGxh
eSIsbnVsbCksR28uc2VsZWN0KCJib2R5Iikuc3R5bGUoImN1cnNvciIsYi5zdHlsZSgiY3Vyc29y
IikpLHcoe3R5cGU6ImJydXNoc3RhcnQifSksdigpfXZhciBpLG8sYT1NKG4sImJydXNoc3RhcnQi
LCJicnVzaCIsImJydXNoZW5kIiksYz1udWxsLHM9bnVsbCxsPVswLDBdLGY9WzAsMF0saD0hMCxn
PSEwLHA9SHNbMF07cmV0dXJuIG4uZXZlbnQ9ZnVuY3Rpb24obil7bi5lYWNoKGZ1bmN0aW9uKCl7
dmFyIG49YS5vZih0aGlzLGFyZ3VtZW50cyksdD17eDpsLHk6ZixpOmksajpvfSxlPXRoaXMuX19j
aGFydF9ffHx0O3RoaXMuX19jaGFydF9fPXQsTHM/R28uc2VsZWN0KHRoaXMpLnRyYW5zaXRpb24o
KS5lYWNoKCJzdGFydC5icnVzaCIsZnVuY3Rpb24oKXtpPWUuaSxvPWUuaixsPWUueCxmPWUueSxu
KHt0eXBlOiJicnVzaHN0YXJ0In0pfSkudHdlZW4oImJydXNoOmJydXNoIixmdW5jdGlvbigpe3Zh
ciBlPW11KGwsdC54KSxyPW11KGYsdC55KTtyZXR1cm4gaT1vPW51bGwsZnVuY3Rpb24odSl7bD10
Lng9ZSh1KSxmPXQueT1yKHUpLG4oe3R5cGU6ImJydXNoIixtb2RlOiJyZXNpemUifSl9fSkuZWFj
aCgiZW5kLmJydXNoIixmdW5jdGlvbigpe2k9dC5pLG89dC5qLG4oe3R5cGU6ImJydXNoIixtb2Rl
OiJyZXNpemUifSksbih7dHlwZToiYnJ1c2hlbmQifSl9KToobih7dHlwZToiYnJ1c2hzdGFydCJ9
KSxuKHt0eXBlOiJicnVzaCIsbW9kZToicmVzaXplIn0pLG4oe3R5cGU6ImJydXNoZW5kIn0pKX0p
fSxuLng9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGM9dCxwPUhzWyFjPDwx
fCFzXSxuKTpjfSxuLnk9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHM9dCxw
PUhzWyFjPDwxfCFzXSxuKTpzfSxuLmNsYW1wPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMu
bGVuZ3RoPyhjJiZzPyhoPSEhdFswXSxnPSEhdFsxXSk6Yz9oPSEhdDpzJiYoZz0hIXQpLG4pOmMm
JnM/W2gsZ106Yz9oOnM/ZzpudWxsfSxuLmV4dGVudD1mdW5jdGlvbih0KXt2YXIgZSxyLHUsYSxo
O3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhjJiYoZT10WzBdLHI9dFsxXSxzJiYoZT1lWzBdLHI9
clswXSksaT1bZSxyXSxjLmludmVydCYmKGU9YyhlKSxyPWMocikpLGU+ciYmKGg9ZSxlPXIscj1o
KSwoZSE9bFswXXx8ciE9bFsxXSkmJihsPVtlLHJdKSkscyYmKHU9dFswXSxhPXRbMV0sYyYmKHU9
dVsxXSxhPWFbMV0pLG89W3UsYV0scy5pbnZlcnQmJih1PXModSksYT1zKGEpKSx1PmEmJihoPXUs
dT1hLGE9aCksKHUhPWZbMF18fGEhPWZbMV0pJiYoZj1bdSxhXSkpLG4pOihjJiYoaT8oZT1pWzBd
LHI9aVsxXSk6KGU9bFswXSxyPWxbMV0sYy5pbnZlcnQmJihlPWMuaW52ZXJ0KGUpLHI9Yy5pbnZl
cnQocikpLGU+ciYmKGg9ZSxlPXIscj1oKSkpLHMmJihvPyh1PW9bMF0sYT1vWzFdKToodT1mWzBd
LGE9ZlsxXSxzLmludmVydCYmKHU9cy5pbnZlcnQodSksYT1zLmludmVydChhKSksdT5hJiYoaD11
LHU9YSxhPWgpKSksYyYmcz9bW2UsdV0sW3IsYV1dOmM/W2Uscl06cyYmW3UsYV0pfSxuLmNsZWFy
PWZ1bmN0aW9uKCl7cmV0dXJuIG4uZW1wdHkoKXx8KGw9WzAsMF0sZj1bMCwwXSxpPW89bnVsbCks
bn0sbi5lbXB0eT1mdW5jdGlvbigpe3JldHVybiEhYyYmbFswXT09bFsxXXx8ISFzJiZmWzBdPT1m
WzFdfSxHby5yZWJpbmQobixhLCJvbiIpfTt2YXIganM9e246Im5zLXJlc2l6ZSIsZToiZXctcmVz
aXplIixzOiJucy1yZXNpemUiLHc6ImV3LXJlc2l6ZSIsbnc6Im53c2UtcmVzaXplIixuZToibmVz
dy1yZXNpemUiLHNlOiJud3NlLXJlc2l6ZSIsc3c6Im5lc3ctcmVzaXplIn0sSHM9W1sibiIsImUi
LCJzIiwidyIsIm53IiwibmUiLCJzZSIsInN3Il0sWyJlIiwidyJdLFsibiIsInMiXSxbXV0sRnM9
aWMuZm9ybWF0PWZjLnRpbWVGb3JtYXQsT3M9RnMudXRjLElzPU9zKCIlWS0lbS0lZFQlSDolTTol
Uy4lTFoiKTtGcy5pc289RGF0ZS5wcm90b3R5cGUudG9JU09TdHJpbmcmJituZXcgRGF0ZSgiMjAw
MC0wMS0wMVQwMDowMDowMC4wMDBaIik/JG86SXMsJG8ucGFyc2U9ZnVuY3Rpb24obil7dmFyIHQ9
bmV3IERhdGUobik7cmV0dXJuIGlzTmFOKHQpP251bGw6dH0sJG8udG9TdHJpbmc9SXMudG9TdHJp
bmcsaWMuc2Vjb25kPUh0KGZ1bmN0aW9uKG4pe3JldHVybiBuZXcgb2MoMWUzKk1hdGguZmxvb3Io
bi8xZTMpKX0sZnVuY3Rpb24obix0KXtuLnNldFRpbWUobi5nZXRUaW1lKCkrMWUzKk1hdGguZmxv
b3IodCkpfSxmdW5jdGlvbihuKXtyZXR1cm4gbi5nZXRTZWNvbmRzKCl9KSxpYy5zZWNvbmRzPWlj
LnNlY29uZC5yYW5nZSxpYy5zZWNvbmRzLnV0Yz1pYy5zZWNvbmQudXRjLnJhbmdlLGljLm1pbnV0
ZT1IdChmdW5jdGlvbihuKXtyZXR1cm4gbmV3IG9jKDZlNCpNYXRoLmZsb29yKG4vNmU0KSl9LGZ1
bmN0aW9uKG4sdCl7bi5zZXRUaW1lKG4uZ2V0VGltZSgpKzZlNCpNYXRoLmZsb29yKHQpKX0sZnVu
Y3Rpb24obil7cmV0dXJuIG4uZ2V0TWludXRlcygpfSksaWMubWludXRlcz1pYy5taW51dGUucmFu
Z2UsaWMubWludXRlcy51dGM9aWMubWludXRlLnV0Yy5yYW5nZSxpYy5ob3VyPUh0KGZ1bmN0aW9u
KG4pe3ZhciB0PW4uZ2V0VGltZXpvbmVPZmZzZXQoKS82MDtyZXR1cm4gbmV3IG9jKDM2ZTUqKE1h
dGguZmxvb3Iobi8zNmU1LXQpK3QpKX0sZnVuY3Rpb24obix0KXtuLnNldFRpbWUobi5nZXRUaW1l
KCkrMzZlNSpNYXRoLmZsb29yKHQpKX0sZnVuY3Rpb24obil7cmV0dXJuIG4uZ2V0SG91cnMoKX0p
LGljLmhvdXJzPWljLmhvdXIucmFuZ2UsaWMuaG91cnMudXRjPWljLmhvdXIudXRjLnJhbmdlLGlj
Lm1vbnRoPUh0KGZ1bmN0aW9uKG4pe3JldHVybiBuPWljLmRheShuKSxuLnNldERhdGUoMSksbn0s
ZnVuY3Rpb24obix0KXtuLnNldE1vbnRoKG4uZ2V0TW9udGgoKSt0KX0sZnVuY3Rpb24obil7cmV0
dXJuIG4uZ2V0TW9udGgoKX0pLGljLm1vbnRocz1pYy5tb250aC5yYW5nZSxpYy5tb250aHMudXRj
PWljLm1vbnRoLnV0Yy5yYW5nZTt2YXIgWXM9WzFlMyw1ZTMsMTVlMywzZTQsNmU0LDNlNSw5ZTUs
MThlNSwzNmU1LDEwOGU1LDIxNmU1LDQzMmU1LDg2NGU1LDE3MjhlNSw2MDQ4ZTUsMjU5MmU2LDc3
NzZlNiwzMTUzNmU2XSxacz1bW2ljLnNlY29uZCwxXSxbaWMuc2Vjb25kLDVdLFtpYy5zZWNvbmQs
MTVdLFtpYy5zZWNvbmQsMzBdLFtpYy5taW51dGUsMV0sW2ljLm1pbnV0ZSw1XSxbaWMubWludXRl
LDE1XSxbaWMubWludXRlLDMwXSxbaWMuaG91ciwxXSxbaWMuaG91ciwzXSxbaWMuaG91ciw2XSxb
aWMuaG91ciwxMl0sW2ljLmRheSwxXSxbaWMuZGF5LDJdLFtpYy53ZWVrLDFdLFtpYy5tb250aCwx
XSxbaWMubW9udGgsM10sW2ljLnllYXIsMV1dLFZzPUZzLm11bHRpKFtbIi4lTCIsZnVuY3Rpb24o
bil7cmV0dXJuIG4uZ2V0TWlsbGlzZWNvbmRzKCl9XSxbIjolUyIsZnVuY3Rpb24obil7cmV0dXJu
IG4uZ2V0U2Vjb25kcygpfV0sWyIlSTolTSIsZnVuY3Rpb24obil7cmV0dXJuIG4uZ2V0TWludXRl
cygpfV0sWyIlSSAlcCIsZnVuY3Rpb24obil7cmV0dXJuIG4uZ2V0SG91cnMoKX1dLFsiJWEgJWQi
LGZ1bmN0aW9uKG4pe3JldHVybiBuLmdldERheSgpJiYxIT1uLmdldERhdGUoKX1dLFsiJWIgJWQi
LGZ1bmN0aW9uKG4pe3JldHVybiAxIT1uLmdldERhdGUoKX1dLFsiJUIiLGZ1bmN0aW9uKG4pe3Jl
dHVybiBuLmdldE1vbnRoKCl9XSxbIiVZIixBZV1dKSwkcz17cmFuZ2U6ZnVuY3Rpb24obix0LGUp
e3JldHVybiBHby5yYW5nZShNYXRoLmNlaWwobi9lKSplLCt0LGUpLm1hcChCbyl9LGZsb29yOkF0
LGNlaWw6QXR9O1pzLnllYXI9aWMueWVhcixpYy5zY2FsZT1mdW5jdGlvbigpe3JldHVybiBYbyhH
by5zY2FsZS5saW5lYXIoKSxacyxWcyl9O3ZhciBYcz1acy5tYXAoZnVuY3Rpb24obil7cmV0dXJu
W25bMF0udXRjLG5bMV1dfSksQnM9T3MubXVsdGkoW1siLiVMIixmdW5jdGlvbihuKXtyZXR1cm4g
bi5nZXRVVENNaWxsaXNlY29uZHMoKX1dLFsiOiVTIixmdW5jdGlvbihuKXtyZXR1cm4gbi5nZXRV
VENTZWNvbmRzKCl9XSxbIiVJOiVNIixmdW5jdGlvbihuKXtyZXR1cm4gbi5nZXRVVENNaW51dGVz
KCl9XSxbIiVJICVwIixmdW5jdGlvbihuKXtyZXR1cm4gbi5nZXRVVENIb3VycygpfV0sWyIlYSAl
ZCIsZnVuY3Rpb24obil7cmV0dXJuIG4uZ2V0VVRDRGF5KCkmJjEhPW4uZ2V0VVRDRGF0ZSgpfV0s
WyIlYiAlZCIsZnVuY3Rpb24obil7cmV0dXJuIDEhPW4uZ2V0VVRDRGF0ZSgpfV0sWyIlQiIsZnVu
Y3Rpb24obil7cmV0dXJuIG4uZ2V0VVRDTW9udGgoKX1dLFsiJVkiLEFlXV0pO1hzLnllYXI9aWMu
eWVhci51dGMsaWMuc2NhbGUudXRjPWZ1bmN0aW9uKCl7cmV0dXJuIFhvKEdvLnNjYWxlLmxpbmVh
cigpLFhzLEJzKX0sR28udGV4dD1DdChmdW5jdGlvbihuKXtyZXR1cm4gbi5yZXNwb25zZVRleHR9
KSxHby5qc29uPWZ1bmN0aW9uKG4sdCl7cmV0dXJuIE50KG4sImFwcGxpY2F0aW9uL2pzb24iLEpv
LHQpfSxHby5odG1sPWZ1bmN0aW9uKG4sdCl7cmV0dXJuIE50KG4sInRleHQvaHRtbCIsV28sdCl9
LEdvLnhtbD1DdChmdW5jdGlvbihuKXtyZXR1cm4gbi5yZXNwb25zZVhNTH0pLCJmdW5jdGlvbiI9
PXR5cGVvZiBkZWZpbmUmJmRlZmluZS5hbWQ/ZGVmaW5lKEdvKToib2JqZWN0Ij09dHlwZW9mIG1v
ZHVsZSYmbW9kdWxlLmV4cG9ydHM/bW9kdWxlLmV4cG9ydHM9R286dGhpcy5kMz1Hb30oKTsK"
             )))
  repository)

(defun c/produce-json (repository)
  "Produce json for REPOSITORY."
  (message "Produce json...")
  (shell-command
   (format
    "cd /tmp; python csv_as_enclosure_json.py --structure cloc-%s.csv --weights %s-revisions.csv > /tmp/%s/%s_hotspot_proto.json"
    (f-filename repository)
    (f-filename repository)
    (f-filename repository)
    (f-filename repository)))
  repository)

(defun c/generate-host-enclosure-diagram-html (repository)
  "Generate host html from REPOSITORY."
  (with-temp-file (format "/tmp/%s/%szoomable.html" (f-filename repository) (f-filename repository))
    (insert
     (concat
      "<!DOCTYPE html>
<meta charset=\"utf-8\">
<style>

.node {
  cursor: pointer;
}

.node:hover {
  stroke: #000;
  stroke-width: 1.5px;
}

.node--root {
  stroke: #777;
  stroke-width: 2px;
}

.node--leaf {
  fill: white;
  stroke: #777;
  stroke-width: 1px;
}

.label {
  font: 14px \"Helvetica Neue\", Helvetica, Arial, sans-serif;
  text-anchor: middle;
  fill: white;
  //text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, -1px 0 0 #fff, 0 -1px 0 #fff;
}

.label,
.node--root,
.node--leaf {
  pointer-events: none;
}

</style>
<body>
<script src=\"d3/d3.min.js\"></script>
<script>

var margin = 10,
    outerDiameter = 960,
    innerDiameter = outerDiameter - margin - margin;

var x = d3.scale.linear()
    .range([0, innerDiameter]);

var y = d3.scale.linear()
    .range([0, innerDiameter]);

var color = d3.scale.linear()
    .domain([-1, 5])
    .range([\"hsl(185,60%,99%)\", \"hsl(187,40%,70%)\"])
    .interpolate(d3.interpolateHcl);

var pack = d3.layout.pack()
    .padding(2)
    .size([innerDiameter, innerDiameter])
    .value(function(d) { return d.size; })

var svg = d3.select(\"body\").append(\"svg\")
    .attr(\"width\", outerDiameter)
    .attr(\"height\", outerDiameter)
  .append(\"g\")
    .attr(\"transform\", \"translate(\" + margin + \",\" + margin + \")\");

d3.json(\""
      (f-filename repository)
      "_hotspot_proto.json"
      "\", function(error, root) {
  var focus = root,
      nodes = pack.nodes(root);

  svg.append(\"g\").selectAll(\"circle\")
      .data(nodes)
    .enter().append(\"circle\")
      .attr(\"class\", function(d) { return d.parent ? d.children ? \"node\" : \"node node--leaf\" : \"node node--root\"; })
      .attr(\"transform\", function(d) { return \"translate(\" + d.x + \",\" + d.y + \")\"; })
      .attr(\"r\", function(d) { return d.r; })
      .style(\"fill\", function(d) { return d.weight > 0.0 ? \"darkred\" :
      d.children ? color(d.depth) : \"black\"; })
      .style(\"fill-opacity\", function(d) { return d.weight; })
      .on(\"click\", function(d) { return zoom(focus == d ? root : d); });

  svg.append(\"g\").selectAll(\"text\")
      .data(nodes)
    .enter().append(\"text\")
      .attr(\"class\", \"label\")
      .attr(\"transform\", function(d) { return \"translate(\" + d.x + \",\" + d.y + \")\"; })
      .style(\"fill-opacity\", function(d) { return d.parent === root ? 1 : 0; })
      .style(\"display\", function(d) { return d.parent === root ? null : \"none\"; })
      .text(function(d) { return d.name; });

  d3.select(window)
      .on(\"click\", function() { zoom(root); });

  function zoom(d, i) {
    var focus0 = focus;
    focus = d;

    var k = innerDiameter / d.r / 2;
    x.domain([d.x - d.r, d.x + d.r]);
    y.domain([d.y - d.r, d.y + d.r]);
    d3.event.stopPropagation();

    var transition = d3.selectAll(\"text,circle\").transition()
        .duration(d3.event.altKey ? 7500 : 750)
        .attr(\"transform\", function(d) { return \"translate(\" + x(d.x) + \",\" + y(d.y) + \")\"; });

    transition.filter(\"circle\")
        .attr(\"r\", function(d) { return k * d.r; });

    transition.filter(\"text\")
      .filter(function(d) { return d.parent === focus || d.parent === focus0; })
        .style(\"fill-opacity\", function(d) { return d.parent === focus ? 1 : 0; })
        .each(\"start\", function(d) { if (d.parent === focus) this.style.display = \"inline\"; })
        .each(\"end\", function(d) { if (d.parent !== focus) this.style.display = \"none\"; });
  }}
);

d3.select(self.frameElement).style(\"height\", outerDiameter + \"px\");

</script>


")))
  repository)

(defun c/navigate-to-localhost (repository &optional port)
  "Navigate to served directory for REPOSITORY, optionally at specified PORT."
  (let ((port (or port 8888)))
    (browse-url (format "http://localhost:%s/%szoomable.html" port (f-filename repository))))
  (sleep-for 1)
  repository)

(defun c/run-server (repository &optional port)
  "Serve directory for REPOSITORY, optionally at PORT."
  (let ((httpd-host 'local)
        (httpd-port (or port 8888)))
    (httpd-stop)
    (ignore-errors (httpd-serve-directory  (format "/tmp/%s/" (f-filename repository)))))
  repository)

(defun c/run-server-and-navigate (repository &optional port)
  "Serve and navigate to REPOSITORY, optionally at PORT."
  (when port
    (c/run-server repository port)
    (c/navigate-to-localhost repository port)))

(defun c/async-run (command repository date &optional port)
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
        (c/run-server-and-navigate (if (eq ',command 'c/show-hotspot-cluster-sync) "system" ,repository) (or ,port 8888))))))

;;;###autoload
(defun c/show-hotspots-sync (repository date &optional port)
  "Show REPOSITORY enclosure diagram for hotspots starting at DATE, optionally served at PORT."
  (interactive
   (list
    (read-directory-name "Choose git repository directory:" (vc-root-dir))
    (call-interactively 'c/request-date)))
  (--> repository
       (c/produce-git-report it date)
       c/produce-code-maat-revisions-report
       c/produce-cloc-report
       c/generate-merger-script
       c/generate-d3-lib
       c/produce-json
       c/generate-host-enclosure-diagram-html
       (c/run-server-and-navigate it port)))

(defun c/show-hotspots (repository date &optional port)
  "Show REPOSITORY enclosure diagram for hotspots. Starting DATE reduces scope of Git log and PORT defines where the html is served."
  (interactive
   (list
    (read-directory-name "Choose git repository directory:" (vc-root-dir))
    (call-interactively 'c/request-date)))
  (c/async-run 'c/show-hotspots-sync repository date port))



(provide 'code-compass)
;;; code-compass ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\?[ \t]+1.%02y%02m%02d\\?\n"
;; End:
