#!/bin/env python

#######################################################################
## This program generates a JSON document suitable for a D3.js
## Hierarchical Edge Bundling visualization (see https://gist.github.com/mbostock/7607999)
##
## The input data is read from a Code Maat CSV file containing the result
## of a <communication> analysis.
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
        with open(filename, 'rt', encoding="utf8") as csvfile:
                r = csv.reader(csvfile, delimiter=',')
                heading = read_heading_from(r)
                validate_content_by(heading, expected_format)
                return [parse_action(row) for row in r]

class LinkBetweenPeer(object):
        def __init__(self, author, peer, strength):
                self.author = author
                self.peer = peer
                self.strength = int(strength)

def parse_peers(csv_row):
        return LinkBetweenPeer(csv_row[0], csv_row[1], csv_row[4])

######################################################################
## Assemble the individual entries into an aggregated structure
######################################################################

def link_to(existing_authors, new_link):
        if not new_link.author in existing_authors:
                return {'name':new_link.author, 'size':new_link.strength, 'imports':[new_link.peer]}
        existing_author = existing_authors[new_link.author]
        existing_author['imports'].append(new_link.peer)
        existing_author['size'] = existing_author['size'] + new_link.strength
        return existing_author

def aggregate_links_per_author_in(peer_links):
        links_per_author = {}
        for peer in peer_links:
                links_per_author[peer.author] = link_to(links_per_author, peer)
        return links_per_author

######################################################################
## Output
######################################################################

def write_json(result):
        print(json.dumps(result))

######################################################################
## Main
######################################################################

def run(args):
        peer_links = parse_csv(args.communication,
                                                        expected_format='author,peer,shared,average,strength',
                                                        parse_action=parse_peers)
        links_by_author = aggregate_links_per_author_in(peer_links)
        write_json(list(links_by_author.values()))

if __name__ == "__main__":
        parser = argparse.ArgumentParser(description='Generates a JSON document suitable for communication diagrams.')
        parser.add_argument('--communication', required=True, help='A CSV file containing the result of a communication analysis')

        args = parser.parse_args()
        run(args)
