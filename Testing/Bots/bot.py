import json
import sys
from state import State

class Bot:
    def __init__( self ):
        pass

    def run( self ):
        for line in sys.stdin:
            state = State( line )

            own = []
            for i in range( 0, state.R ):
                for j in range( 0, state.C ):
                    if state.is_my_cell( i, j ):
                        own.append( ( i, j ) )

            possible = []
            for i in range( 0, state.R ):
                for j in range( 0, state.C ):
                    if not state.is_empty( i, j ): continue
                    for p in own:
                        if State.distance( p, ( i, j ) ) < 2:
                            possible.append( ( i, j ) )
                            break

            xx = ( -1, -1 )
            my = ( state.R, state.C )
            mx = ( state.R, state.C )
            for p in possible:
                if p[ 0 ] > xx[ 0 ]:
                    xx = p
                if p[ 1 ] < my[ 1 ]:
                    my = p
                if p[ 0 ] < mx[ 0 ]:
                    mx = p

            Bot.send_move( [ mx, my, xx ] )

    @staticmethod
    def send_move( cells ):
        print( json.dumps( { "cells": cells } ), end = '\n', flush = True )


if __name__ == '__main__':
    Bot().run()
