#!/usr/bin/env python3
import json


class State(object):

    def __init__(self, line):
        jsondict = json.loads(line)
        self.map = []
        for j in jsondict["field"]:
            self.map.append([k for k in j])

        self.R = len(self.map)
        self.C = len(self.map[0])

        self.maxColonizationDistance = jsondict["maxColonisationDistance"]
        self.cellsRemaining = jsondict["cellsRemaining"]
        self.cellGainPerTurn = jsondict["cellGainPerTurn"]
        self.maxGameIterations = jsondict["maxGameIterations"]
        self.maxCellCapacity = jsondict["maxCellCapacity"]
        self.currIteration = jsondict["currIteration"]

    def __getitem__(self, key):
        return self.map[key]

    def out_of_bounds(self, x, y):
        return x < 0 or y < 0 or x > self.C or y > self.R

    def is_my_cell(self, x, y):
        return not self.out_of_bounds(x, y) and self.map[x][y] == '#'

    def is_enemy_cell(self, x, y):
        return not self.out_of_bounds(x, y) and self.map[x][y] == 'O'

    def is_empty(self, x, y):
        return not self.out_of_bounds(x, y) and self.map[x][y] == '.'

    @staticmethod
    def distance( p1, p2 ):
        return max( abs( p1[ 0 ] - p2[ 0 ] ), abs( p1[ 1 ] - p2[ 1 ] ) )
