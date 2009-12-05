from itertools import product, ifilter
import pydot

SYM = ["T", "F"]
UNDEF = "_|_"

class Sym(object):
    """
    >>> s = Sym(UNDEF)
    >>> s1 = Sym("T")
    >>> s < s1
    True
    """
    def __init__(self, sym):
        self.sym = sym
        
    def __str__(self):
        return str(self.sym)

    def __cmp__(self, other):
        # here we also handle the case where they're both undefined
        if self.sym == other.sym:
            return 0
        if self.sym == UNDEF:
            return (-1)
        if other.sym == UNDEF:
            return 1

    @classmethod
    def comparable(fst, other):
        if (fst != other and\
                fst != UNDEF and other != UNDEF):
            return False
        else:
            return True

class Dom(object):
    def __init__(self, symbols, dim):
        self.symbols = symbols
        self.dim = dim
        alsym = set(self.symbols).union(UNDEF)
        self.comparables = ifilter(Sym.comparable, product(alsym, repeat = dim))
        print list(self.comparables)

    def __iter__(self):
        return self.comparables

