#!/usr/bin/env python

"""
From complex to simple haskell only using
regular expression
"""
# TODO: take a symbolic representation of the rule, transform it into a real regexp and apply it
# TODO: use http://www.dabeaz.com/ply/ply.html instead
# TODO: try to rewrite with pyparsing which allows more freedom to define the grammar

import re
import ply.lex as lex
import ply.yacc as yacc

import pyparsing as pyp
# set the correct parseAactions to different things

case = pyp.Literal('case')
of = pyp.Literal('of')
lam = pyp.Literal('\\').setParseAction(lambda _ : pass)
var = pyp.Regex(r"[xyz][0-9]?")
arr = pyp.Literal("->")

# declaring as a recursive definition, because it can contain other \ expr
l_expr = Forward()
l_expr << lam + 

# see this for example
expr = Forward()
atom = ( ( e | floatnumber | integer | ident ).setParseAction(pushFirst) | 
         ( lpar + expr.suppress() + rpar )
       )
        
factor = Forward()
factor << atom + ZeroOrMore( ( expop + factor ).setParseAction( pushFirst ) )
        
term = factor + ZeroOrMore( ( multop + factor ).setParseAction( pushFirst ) )
expr << term + ZeroOrMore( ( addop + term ).setParseAction( pushFirst ) )
bnf = Optional((ident + assign).setParseAction(assignVar)) + expr

pattern =  bnf + StringEnd()

tokens = [
    'ID',
    'FUN',
    'ARROW',
    'LAMBDA',
    'CONSTR',
    'DEF'
    # 'RCURLY',
    # 'LCURLY',
    # 'LROUND',
    # 'RROUND'
    ]

reserved = {
   'case' : 'CASE',
   'of'   : 'OF',
   'if'   : 'IF',
   'then' : 'THEN',
   'else' : 'ELSE'
}

tokens = reserved.values() + tokens

# for defining a token prepend a T, for yacc a P
t_CONSTR = r"[A-Z][a-z]+"
t_FUN = r"[a-z]+"
t_ARROW = r"->"
t_LAMBDA = r"\\"
t_DEF = r"="
# t_LCURLY = r"{"
# t_RCURLY = r"\}"
# t_LROUND = r"("
# t_RROUND = r")"

# use something like this instead to manage also the reserved values
def t_ID(t):
    r'[xyz][0-9]?'
    t.type = reserved.get(t.value,'ID')    # Check for reserved words
    return t

# adding tokens for constructors, types and more

t_ignore = " \t"

# Error handling rule
def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.lexer.skip(1)

data = r"append x1 = \x2 -> if Var y3 then else x2"

lexer = lex.lex()

# Give the lexer some input
lexer.input(data)

# Tokenize
while True:
    tok = lexer.token()
    if not tok: break      # No more input
    print tok


class Case(object):
    def __init__(self, exp, tuples = None):
        if tuples:
            self.tuples = tuples
        else:
            self.tuples = {}

    def __setitem__(self, i, y):
        self.tuples[i] = y

class Match(object):
    def __init__(self, pat1, exp, exp1, rest):
        # getting in input what can be matched and the rest of the things
        pass

c = Case("x^2 + 1")
c["_"] = "100"
print c.tuples
            
# yacc part, here we actually start to create the structure


# define all the possible tokens

# see how to combine different regular expressions into one (template maybe?)
# use a simple template string to combine different regular exprs
# at least one letter and at most one number at the end

# spaces should be managed automatically
# everything is not predefined function is probably a new variable to instantiate
###################################################################
# rule3 = (r"\pat -> exp", r"\var -> case var of pat -> exp")     #
#                                                                 #
# filter_lam = lambda x: x[x.find('\\')+1 : ]                     #
# predef = ("->", "case", "of")                                   #
#                                                                 #
# rule_dict = {}                                                  #
#                                                                 #
# bound = []                                                      #
# for word in rule3[1].split(' '):                                #
#     if (word not in predef) and (filter_lam(word) in rule3[0]): #
#         bound.append(word)                                      #
#                                                                 #
# r0 = rule3[0]                                                   #
# r1 = rule3[1]                                                   #
# print r                                                         #
# # $1 and so on is what you've just match                        #
# for i, b in enumerate(bound):                                   #
#     r0 = re.sub(b, "(?P<%s>\w+)" % b, r)                        #
#     to_s = lambda m: m.group(b)                                 #
#     r1 = re.sub(b, to_s, r1)                                    #
#     print (r0, r1)                                              #
###################################################################
# if match then substitute the right side of the tuple, otherwise don't

# http://stackoverflow.com/questions/490597/regex-replace-in-python-a-more-simple-way for callable and subs

# I need repeated application until fix point and other things
