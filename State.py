#!/usr/bin/env python3
import json


class State(object):

    def __init__(self, line):
        # Read config file and set config values from it
        config = open("configFinal.json", "r", encoding="utf-8")
        jsondict = json.loads(config.read())

        self.field = list()
        for j in jsondict["field"]:
            self.field.append([k for k in j])

        self.rows = len(self.field)
        self.cols = len(self.field[0])

        self.maxColonisationDistance = jsondict["maxColonisationDistance"]
        self.cellsRemaining = jsondict["cellsRemaining"]
        self.cellGainPerTurn = jsondict["cellGainPerTurn"]
        self.maxGameIterations = jsondict["maxGameIterations"]
        self.maxCellCapacity = jsondict["maxCellCapacity"]
        self.currIteration = jsondict["currIteration"]

    def __getitem__(self, key):
        return self.field[key]

    def out_of_bounds(self, x, y):
        return x < 0 or y < 0 or x > self.cols or y > self.rows

    def is_my_cell(self, x, y):
        return not self.out_of_bounds(x, y) and self.field[x][y] == '#'

    def is_enemy_cell(self, x, y):
        return not self.out_of_bounds(x, y) and self.field[x][y] == 'O'

    def is_empty(self, x, y):
        return not self.out_of_bounds(x, y) and self.field[x][y] == '.'

    @staticmethod
    def submit_move(cell_list):
        print(json.dumps({"cells": cell_list}), end='\n', flush=True)

