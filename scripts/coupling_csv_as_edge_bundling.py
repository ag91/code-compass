"""
The input data is read from a Code Maat CSV file containing the result
of a <coupling> analysis.
"""

import argparse
import csv
import json
import sys

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
        with open(filename, 'rt', encoding="utf8") as csvfile:
                r = csv.reader(csvfile, delimiter=',')
                heading = read_heading_from(r)
                validate_content_by(heading, expected_format)
                return [parse_action(row) for row in r]

class LinkBetweenCoupled(object):
        def __init__(self, entity, coupled, degree):
                self.entity = entity
                self.coupled = coupled
                self.degree = int(degree)

def parse_coupleds(csv_row):
        return LinkBetweenCoupled(csv_row[0], csv_row[1], csv_row[2]) # 2020-07-05 AG: editing this to make it work with software dependencies, TODO rename all entity based naming to coupled stuff


def link_to(existing_entitys, new_link):
        if not new_link.entity in existing_entitys:
                return {'name':new_link.entity, 'size':new_link.degree, 'imports':[new_link.coupled]}
        existing_entity = existing_entitys[new_link.entity]
        existing_entity['imports'].append(new_link.coupled)
        existing_entity['size'] = existing_entity['size'] + new_link.degree
        return existing_entity

def aggregate_links_per_entity_in(coupled_links):
        links_per_entity = {}
        for coupled in coupled_links:
                links_per_entity[coupled.entity] = link_to(links_per_entity, coupled)
        return links_per_entity


def write_json(result):
        print(json.dumps(result))


def run(args):
        coupled_links = parse_csv(args.coupling,
                                                        expected_format='entity,coupled,degree,average-revs',
                                                        parse_action=parse_coupleds)
        links_by_entity = aggregate_links_per_entity_in(coupled_links)
        write_json(list(links_by_entity.values()))

if __name__ == "__main__":
        parser = argparse.ArgumentParser(description='Generates a JSON document suitable for coupling diagrams.')
        parser.add_argument('--coupling', required=True, help='A CSV file containing the result of a coupling analysis')

        args = parser.parse_args()
        run(args)

