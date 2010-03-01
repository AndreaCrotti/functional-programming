#!/usr/bin/env python
"""
Experiments about occur checking and unification
algorithms, useful for haskell, lambda calculus and prolog
"""

class Subst(object):
    "Contains a variable and the term you want to substitute"
    def __init__(self, var, term):
        self.var = var
        self.term = term

# see how a term could be constructed exactly
class Term(object):
    "A term is a lambda expr and a term or just a term"
    def __init__(self):
        pass

    # redefine some operator to get the substitution working

class Var(object):
    "Variable"
    pass
    
class Constant(object):
    "Constant element"
    pass
        
