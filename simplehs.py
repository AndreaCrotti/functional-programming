#!/usr/bin/env python
"""
Convert haskell programs into simple haskell programs via term rewriting.
There must be
- no type synonyms, no type classes, no pre-defined lists

From haskell to simple haskell there are 12 rules to follow
"""

import pyparsing, re

def rule2(prog):
    "Transform a multiple lambda expr in a concatenation of them"
    multiple = re.compile(r"(\w+) (\w+)->")
    # see how the matching shoul be done instead
    return re.sub(multiple, prog, "\1 -> \2 ->")

if __name__ == '__main__':
    print rule2(r"\x x2 ->")

