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
        """ Parameterize with the column - this allows us
                to generate data from different analysis result types.
        """
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
        """ Recursively traverse the hierarchy and insert the individual parts
                of the module, one by one.
                The parts specify branches. If any branch is missing, it's
                created during the traversal.
                The final part specifies a module name (sans its path, of course).
                This is where we add size and weight to the leaf.
        """
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

if __name__ == "__main__":
        parser = argparse.ArgumentParser(description='Generates a JSON document suitable for enclosure diagrams.')
        parser.add_argument('--structure', required=True, help='A CSV file generated by cloc')
        parser.add_argument('--weights', required=True, help='A CSV file with hotspot results from Code Maat')
        parser.add_argument('--weightcolumn', type=int, default=1, help="The index specifying the columnt to use in the weight table")
        # TODO: add arguments to specify which CSV columns to use!

        args = parser.parse_args()
        run(args)
