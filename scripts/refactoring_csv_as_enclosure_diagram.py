#!/bin/env python

#######################################################################
## This program generates a JSON document suitable for a D3.js
## enclosure diagram visualization.
## The input data is read from three CSV files:
##  1) A cloc output file specifying the static structure of the system.
##  2) A file containing Code Maat main developer results.
##  3) A file specifying the color to use for a certain author. If that
##     information is absent, this script will treat that code as a
##     dead spot.
##     This CSV file must have two columns: author, color
#######################################################################

import argparse
import csv
import json
import sys

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
                p = next(r)
                while p == []:
                        p = next(r)
                return p
        with open(filename, 'r') as csvfile:
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

def parse_author_color(csv_row):
                author = csv_row[0]
                color = csv_row[1]
                return author,color

class Ownership(object):
        def __init__(self, module, main_author, ownership):
                self.module = module
                self.main_author = main_author
                self.ownership = ownership

def parse_ownership(csv_row):
        module = csv_row[0]
        main_author = csv_row[1]
        ownership = csv_row[4]
        return Ownership(module, main_author,ownership)

######################################################################
## Organizational information to augment the structure
######################################################################

class Knowledge(object):
        DEFAULT_COLOR = "black"
        def __init__(self, authors_colors, ownerships):
                self._authors_colors = authors_colors
                self._ownership = dict([(o.module, o) for o in ownerships])

        def color_of(self, author):
                if author in self._authors_colors:
                        return self._authors_colors[author]
                return self.DEFAULT_COLOR

        def owner_of(self, module_name):
                if module_name in self._ownership:
                        o = self._ownership[module_name]
                        return o.main_author
                return None

        def degree_of_ownership_for(self, module_name):
                if module_name in self._ownership:
                        o = self._ownership[module_name]
                        return o.ownership
                return 0.0

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

def _add_leaf(hierarchy, module, knowledge, name):
        owner = knowledge.owner_of(module.name)
        new_leaf = {'name':name, 'children':[],
                    'size':module.complexity,
                    'weight':knowledge.degree_of_ownership_for(module.name),
                    'author': owner,
                    'author_color':knowledge.color_of(owner)}
        hierarchy.append(new_leaf)
        return hierarchy

def _insert_parts_into(hierarchy, module, knowledge, parts):
        """ Recursively traverse the hierarchy and insert the individual parts
                of the module, one by one.
                The parts specify branches. If any branch is missing, it's
                created during the traversal.
                The final part specifies a module name (sans its path, of course).
                This is where we add size and weight to the leaf.
        """
        if len(parts) == 1:
                return _add_leaf(hierarchy, module, knowledge, name=parts[0])
        next_branch = parts[0]
        existing_branch = _ensure_branch_exists(hierarchy, next_branch)
        return _insert_parts_into(existing_branch['children'],
                                                          module,
                                                          knowledge,
                                                          parts=parts[1:])

def generate_structure_from(modules, knowledge):
        hierarchy = []
        for module in modules:
                parts = module.parts()
                _insert_parts_into(hierarchy, module, knowledge, parts)

        structure = {'name':'root', 'children':hierarchy}
        return structure

######################################################################
## Output
######################################################################

def write_json(result):
        print(json.dumps(result))

######################################################################
## Main
######################################################################

def run(args):
        authors_colors = dict(parse_csv(args.authors,
                                                            expected_format='author,color',
                                                                parse_action=parse_author_color))
        module_ownership = parse_csv(args.owners,
                                                             expected_format='entity,main-dev,removed,total-removed,ownership',
                                                             parse_action=parse_ownership)
        structure_input = parse_csv(args.structure,
                                                                expected_format='language,filename,blank,comment,code',
                                                                parse_action=parse_structural_element)
        knowledge = Knowledge(authors_colors, module_ownership)
        knowledge_structure = generate_structure_from(structure_input, knowledge)
        write_json(knowledge_structure)

if __name__ == "__main__":
        parser = argparse.ArgumentParser(description='Generates a JSON document suitable for knowledge diagrams.')
        parser.add_argument('--structure', required=True, help='A CSV file generated by cloc')
        parser.add_argument('--owners', required=True, help='A CSV file generated by a Code Maat main-dev analysis')
        parser.add_argument('--authors', required=True, help='A CSV file specifying the color to use for each author')

        args = parser.parse_args()
        run(args)
