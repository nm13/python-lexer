#!/usr/bin/python
## ^^^ python 2.7 ~~~

#
# author:  nikomu@ gmail.com
# state: "proof of concept" / "pre-alpha"
# licence:  LGPL (  WTFPL - friendly )
#

"""
    implementing ~lex format in python:
    
    -- no definitions yet ;
    
    -- a line should start with either '<', space, or start a string ; 
    
        -- '<' or string start a "trigger", anything started with space characters is action code and lasts till the next "trigger"
        
    -- an example:
    
===

<CODE> ".|\n" # actually, we could probably have optional comment here -- and just skip to '\n'

    print ... # yet have to decide on the variable names
    
    # more comments
    
    
<STRING, COMMENT> ".|\n"

    # some action

===

NB: this module may contain some not-so-well thought bits since it was written in ~1.5 days by a human with a slightly raised temperature ))

NB(2): states are exclusive!
NB(3): at the moment *not* the longest, but *the earliest* sequence is matched!
       // could be changed by matching the regexps one-by-one and then comparing the lenghts of the matches
NB(4): if 'INITIAL' state is not silently defined, then the first defined state becomes, well, the first state ) /* the one that starts the state machine for the lexer */
NB(5): can't use '''-delimiters for strings in the action code ( only \"\"\", ' , " delimiters left ) )
       // this could be avoided via using repr(), but repr() kills code readabilty

TODO: replace all the fragments of the following kind with a decent function!

    msg = "expected an '(' after a GOTO:"
    e = ParseError(msg, tok_tuple = _last_tuple)
    # dbg
    print >>sys.stderr, msg, '\n', e.make_message( _last_tuple )
    raise e

TODO(2): write an action checker that looks for '''/r''' string prefixes and issues a warning (see NB(3)) ;

## TODO(3): parse "pre-trigger action code" and move it into __init__() ) // DONE

TODO(4): longest match instead of 'first matched' ?

TODO(5): 'yy-ify' local variables and attributes for the generated code )


---
ref: [ http://eli.thegreenplace.net/2013/06/25/regex-based-lexical-analysis-in-python-and-javascript/ ]

"""

# ----------------------------------------------------------------------------------
## ===============================================================================

import re
# import os
import sys # stderr

import tokenize as T

from cStringIO import StringIO as io

## from collections import deque

from pprint import pprint

from collections import OrderedDict # not that important, but let's preserve the order of the states for better readability

import os # os.path.split(), etc

# ----------------------------------------------------------------------------------
## ===============================================================================

#
# our exception class
#

class ParseError(Exception):
    """ for syntactic errors in the spec, mostly """

    @staticmethod
    def make_message( tok_tuple ):

        toktype, token, (srow, scol), (erow, ecol), line = tok_tuple
        lineno = srow

        message = "unexpected token '%(token)s' at line %(lineno)d, %(scol)d:%(ecol)d (logical line follows): \n%(line)s" % locals()

        return message


    def __init__(self, message = '', tok_tuple = None):

        if tok_tuple is not None:
            msg = self.make_message( tok_tuple )
            
            if message: # prepend it to msg if 'message' is not empty
                message = message + ":\n\t" + msg
            else:
                message = msg

        Exception.__init__(self, message)



# ----------------------------------------------------------------------------------
## ===============================================================================

VERSION_STRING = '0.0.0' # a very minimalist way to compare this file to future bugfixes )

#
# accessory constants
#


# since tokenize constants live within { 0 .. 54, 256 }, let us start our own constants from 1000
_CONST_BASE  =  max(max(T.tok_name.keys()), 1000 )
## _NL = NEWLINE = 1
## CR = 2 # DEDENT ''

LT     = 3   + _CONST_BASE   #  OP, '<'
GT     = 4   + _CONST_BASE   #  OP, '>'
COMMA  = 5   + _CONST_BASE   #  OP, ','
STR    = 6   + _CONST_BASE   #  STRING, 'sth'
NAME   = 7   + _CONST_BASE   #  NAME, 'sth'

# "default state"
INITIAL = 'INITIAL'

GOTO_METHOD_NAME = '_yy_set_state'

# "macro expansions"
YYLVAL_STRING = 'self._yylval' # <= 'yylval'
RETURN_STRING = 'self._yy_result' # <= 'RETURN'

# ----------------------------------------------------------------------------------
## ===============================================================================

#
# utils
#

def indent(text, indentation):
    
    lines = []
    for L in text.split('\n'):
        lines.append( indentation + L )
        
    return '\n'.join( lines )

# ----------------------------------------------------------------------------------

'''
def _list_find( L, item, default = None ):
    """ returns the index or None if not found """
    
    i = default
    try:
        i = L.index( item )
    except IndexError:
        pass
        
    return i


def _list_rfind( L, item, default = None ):
    """ terribly inefficient """
    
    L.reverse()
    r_ind = _list_find( L, item )
    ret = None
    if r_ind is not None:
        ret = len( L ) - 1 - r_ind
        
    L.reverse() # return back )
    
    return ret

# a test:
if 0:
    L = list("abcdef")
    print L[ _list_rfind(L, 'e') ] # works )
    sys.exit(0)
'''

'''
# [ http://stackoverflow.com/questions/8534256/python-find-first-element-in-a-sequence-that-matches-a-predicate ]
# works, but is completely unreadable ))
def seq_find( sequence, predicate ):
    """ returns a tuple (ind, elem) where predicate(seq[ind]) is True or None """
    
    gen = ( (n,el) for n, el in enumerate(sequence) )
    cond = lambda tu: predicate(tu[1])
    
    ret = next( (x for x in gen if cond(x)), None)
    
    return ret
'''

def seq_find( sequence, predicate ):
    """ returns a tuple (ind, elem) where predicate(seq[ind]) is True or None """
    
    ret = None
    for n, e in enumerate( sequence ):
        
        if predicate(e):
            ret = ( n, e )
            break
    
    return ret


# a test:
if 0:
    L = list("abcdef")
    cond = lambda x: x == 'e'
    print L[ seq_find(L, cond)[0] ] # works
    
    cond2 = lambda x: x == 'A'
    assert seq_find(L, cond2) is None # still works )
    sys.exit(0)


"""
# (1,2,3,4,5,...) => '_1', '_2', ...
def make_next_idder():
    
    def next_id(counter = [0]): # 'counter' is initialized once per make_.._idder() call )
        counter[0] += 1 # starts from '1'
        ret = '_%d' % ( counter[0], )
        return ret
        
    return next_id

##  # actually, we need only one )
##  next_id = make_next_idder()
"""

# ----------------------------------------------------------------------------------

def strip_string_literal( literal ):
    """ <r'''abc'''> => <abc> and so on """
    
    # debugging
    dbg_ref_ = literal
    
    ## literal = literal.lstrip('r')
    # more expressive:
    if literal.startswith('r'):
        literal = literal[1:]

    # longest matches first:
    if literal.startswith("'''"):
        ret = literal[3:-3]
    elif literal.startswith('"""'):
        ret = literal[3:-3]
    elif literal.startswith('"'):
        ret = literal[1:-1]
    elif literal.startswith("'"):
        ret = literal[1:-1]
    else:
        raise ParseError( "weird literal: %s" % ( dbg_ref_, ) )

    return ret

# ----------------------------------------------------------------------------------

#
# "unindent" the code
#


def tokenize_expanded( code_text ) :     
    """ text => [ ( toknum, tokstr, ... ), ... ] """

    buffer = io( code_text )
    g = T.generate_tokens( buffer.readline )
    
    for tok_tuple in g:
        
        yield tok_tuple


def tokenize( code_text ):
    """ text => [ ( toknum, tokstr ), ... ] """

    """
    buffer = io( code_text )
    g = T.generate_tokens( buffer.readline )
    
    for toknum, tokstr, _, _, _ in g:
        
        yield ( toknum, tokstr )
    """
    for toknum, tokstr, _, _, _ in tokenize_expanded( code_text ):
        
        yield ( toknum, tokstr )


def remove_leading_indentation( code_text ):
    
    """
    buffer = io( code_text )
    g = T.generate_tokens( buffer.readline )
    
    indent_level = 0
    tokens = [ ( toknum, tokstr ) for toknum, tokstr, _, _, _ in g ]
    """
    tokens = [ tu for tu in tokenize(code_text) ]
    
    # there is _quite likely_ to be some indentation, nevertheless:
    tu_found = seq_find( tokens, lambda tu: tu[0] == T.INDENT )
    if tu_found is not None:
        
        pos, tu_ = tu_found
        tokens.pop( pos )
        
        tokens.reverse() # in-place !!
        found = seq_find( tokens, lambda tu: tu[0] == T.DEDENT ) # the last one, there should be one for each indent level
    
        # assert found is not None
        if found is None: 

            msg = "no corresponding (final) 'DEDENT'ation found! "
            e = ParseError( msg )
            # dbg
            print >>sys.stderr, msg
            raise e
            
        # else ..
        pos, tu_ = found
        tokens.pop( pos )
        
        # return back
        tokens.reverse()
    
    return T.untokenize( tokens )


# a test
if 0: 
    code = \
    """
        a = 5
        
        for c in "test":
            print c
    """

    print '-' * 10
    print code
    print '-' * 10
    code_2 = remove_leading_indentation( code )
    print code_2
    print '-' * 10

    C = compile( code_2, '', 'exec' )
    exec C                             # works !!

    sys.exit(0)

# ----------------------------------------------------------------------------------

#
# expand the "GOTO(<state>)" macro
#


"""
def _goto_found( toknum, tokstr ):
    
    ## if toknum == T.NAME
    if tokstr
"""

_tu_goto_replacement = \
    ( ( T.NAME    ,     'self' ), # 0
      ( T.OP      ,        '.' ), # 1
      ( T.NAME    ,   '<goto>' ), # 2
      ( T.OP      ,        '(' ), # 3
      ( T.STRING  ,  '<LABEL>' ), # 4 ***
      ( T.OP      ,        ')' )  # 5
    )


def _make_replacement_sequence( label_name, goto_name ):
    """ GOTO ( LABEL ) => self.<goto_name> ( ' LABEL ' ) """
    
    ret = list( _tu_goto_replacement )
    
    ret[2] = ( T.NAME, goto_name )
    
    ret[4] = ( T.STRING, "'%s'" % ( label_name ) )
    
    return ret


## def expand_goto_macro( code_text, goto_name = '_set_state' ):
def expand_goto_macro( code_text, goto_name = GOTO_METHOD_NAME ):
    """ GOTO(STATENAME) => self._set_state('STATENAME') """
    
    ## # could have done that with a sequence, but let's keep it plain simple so far ))
    ## tokens = [ tu for tu in tokenize(code_text) ]
    
    ##  # states
    ##  NOT_FOUND = 0 # no sequence have started yet )
    ##  GOTO_FOUND = 1 # GOTO _ ( LABEL )
    ##  END = -1
    ##  ERROR = -2
    ##  
    ##  state = NOT_FOUND
    
    
    gen = tokenize_expanded( code_text )
    ret = []
    ## while state != END :
    while True :
        
        tok_tuple = next( gen, None )
        
        if tok_tuple is None:
            break
        
        toknum, tokstr, _, _, _ = tok_tuple

        ##      ## if toknum != T.NAME
        ##      # since string literals _include_ the delimiters, we can check only the strings (albeit that would be a bit slower?) 
        ##      # ("slower") -- is it so? (a) we'll still have to unref an object (b) quite frequently it's enough to compare the first character ..
        ##      if tokstr == 'GOTO':
        ##          assert toknum == T.NAME
        ##          state = GOTO_FOUND

        ## if toknum != T.NAME
        # since string literals _include_ the delimiters, we can check only the strings (albeit that would be a bit slower?) 
        # ("slower") -- is it so? (a) we'll still have to unref an object (b) quite frequently it's enough to compare the first character ..
        if tokstr == 'GOTO':
            assert toknum == T.NAME
            
            # for debugging
            _last_tuple = tok_tuple

            # start accepting a sequence:
            
            # 0. GOTO _ ( LABEL )
            tok_tuple = next( gen, None )
            toknum, tokstr, _, _, _ = tok_tuple
            
            if tokstr != '(' :
                
                msg = "expected an '(' after a GOTO:"
                e = ParseError(msg, tok_tuple = _last_tuple)
                # dbg
                print >>sys.stderr, msg, '\n', e.make_message( _last_tuple )
                raise e

            # else .. 
            
            # 1. GOTO ( _ LABEL )
            tok_tuple = next( gen, None )
            toknum, tokstr, _, _, _ = tok_tuple
            
            if toknum != T.NAME :
                
                msg = 'expected a _name_  after a "GOTO(" :'
                e = ParseError(msg, tok_tuple = _last_tuple)
                # dbg
                print >>sys.stderr, msg, '\n', e.make_message( _last_tuple )
                raise e
                
            # else .. 
            label = tokstr
            
            # 2. GOTO ( LABEL _ )
            tok_tuple = next( gen, None )
            toknum, tokstr, _, _, _ = tok_tuple
            
            if tokstr != ')' :
                
                msg = "expected an ')' after a GOTO( %s :" % ( label,  )
                e = ParseError(msg, tok_tuple = _last_tuple)
                # dbg
                print >>sys.stderr, msg, '\n', e.make_message( _last_tuple )
                raise e
            
            # else -- we're done:
            insert = _make_replacement_sequence( label, goto_name )
            
            ret.extend( insert )
            
        else: # any other token but 'GOTO'
            
            ret.append( (toknum, tokstr) )

    # assemble it back:
    return T.untokenize( ret )


# a test
if 0: 
    correct_code = \
"""
    a = 5
    
    if a > 8:
        GOTO( INITIAL )

    print "GOTO"
"""

    print '-' * 10
    print correct_code
    print '-' * 10
    replaced = expand_goto_macro( correct_code, goto_name = 'goto' )
    print replaced
    print '-' * 10

    wrong_code = \
"""
    a = 5
    
    if a > 8:
        GOTO INITIAL 

    print "GOTO"
"""

    print '-' * 10
    print wrong_code
    print '-' * 10
    replaced = expand_goto_macro( wrong_code, goto_name = 'goto' )
    print replaced
    print '-' * 10

    sys.exit(0)


# ----------------------------------------------------------------------------------

#
# replace single tokens: yylval => 'self._yylval', 'return' => 'self._yy_result = '
#

'''
def _replace_name_token( tok_seq, tok_name, tok_repl ):
    """ 'tok_seq' is a sequence of (tok_type, tok_str); 
        'tok_name' is "tok_str" for a .NAME token ;
        finally,
        'tok_repl' is a sequence of tokens to replace;
        
        ## NB: first arg can be a generator, but the last arg
        ##     is concidered to be a 
        
        return value: yields a new sequence )
    """
    
    for tok_type, tok_str in tok_seq:
        
        if tok_str != tok_name:
            
            yield (tok_type, tok_str)
            
        else: # token found
            for tu in tok_repl:
                yield tu
'''

def _replace_tokens( tok_seq, replacements ):
    """ 'tok_seq' is a sequence of (tok_type, tok_str); 
        'replacements' is a dictionary: {
            'token_string' => ( sequence of (type, str) to replace )
        
        return value: yields a new sequence )
    """
    
    keys = replacements.keys()
    for tok_type, tok_str in tok_seq:
        
        if tok_str in keys:
            
            repl = replacements[ tok_str ]
            for tu in repl:
                yield tu
                
        else: # not in replacements
            
            yield (tok_type, tok_str)
            

def _make_tok_list( text ):
    return list( tokenize(text) )


##  YYLVAL_STRING = 'self._yylval' # <= 'yylval'
##  RETURN_STRING = 'self._yy_result' # <= 'RETURN'

_expansions = {
    'yylval' : _make_tok_list( 'self._yylval' ),
    
    # let's stress that this is a macro
    'RETURN' : _make_tok_list( 'self._yy_result =' ) 
    # could have made this a " = .. ; break ; ", 
    # or raise/catch, but let's stick with this for the moment
}

def _expand_tokens( text, expansions = _expansions ):
    
    gen = _replace_tokens( tokenize(text), replacements = expansions )
    
    ret = T.untokenize( gen )
    
    return ret


# a test
if 0: 
    test_code = \
"""
    a = 5
    b = '123'

    yylval = b

    RETURN a
    
"""

    print '-' * 10
    print test_code
    print '-' * 10
    replaced = _expand_tokens( test_code )
    print replaced
    print '-' * 10

    sys.exit(0)


# ----------------------------------------------------------------------------------

def preprocess_action_code( action_code_text ):

    ## action = remove_leading_indentation( action )
    ## action = expand_goto_macro( action )
    
    result = remove_leading_indentation( action_code_text )
    result = expand_goto_macro( result )
    
    result = _expand_tokens( result )
    
    return result


# ----------------------------------------------------------------------------------
## ===============================================================================

# ----------------------------------------------------------------------------------

#
# the template for the generated lexer
#

LEXER_TEMPLATE = \
r"""#!/usr/bin/python
## ^^^ python 2.6+ ~~~

import re

# ----------------------------------------------------------------------------------

#
# our exception object
#
class LexerError(Exception):
    ''' current state has failed to recognize any match '''

# ----------------------------------------------------------------------------------

# ACTIONS = { 
#   '_1' : \
# r'''code_fragment_1''' [,] [\n]
#   '_2' : \
# r'''code_fragment_2''' [,] [\n]
# }
%(code_fragments)s

# compile the action code
__filename = __name__ + ' (inline)' # refer to the same file instead of sth like "''"
kompile = lambda code: compile( code, __filename, 'exec' ) 
COMPILED = { k: kompile(v) for k, v in ACTIONS.iteritems() }

# ----------------------------------------------------------------------------------

#
# regular expression definitions
#
%(regexp_defs)s

# ----------------------------------------------------------------------------------

#
# an accessory method, an alternative would be to compile() it,
# echanging one extra function call 
# for having this function code precompiled with the module
#

def _yy_state_code( self, _yy_regexp, _yy_lex_state_name ):
    ''' internal code shared by any lexer state '''
    #
    # check for end-of-input
    #
    if self._yy_offset == self._yy_length: 
        self._yy_state = None # we're done
        return # None
    
    yymatch = _yy_regexp.match( self.yy_input_text, self._yy_offset )
    if yymatch is None:
        raise LexerError( "no matches found at offset %%s at state %%s" %% (self._yy_offset, _yy_lex_state_name) )
        
    # else ..
    
    yygname = yymatch.lastgroup
    yytext = yymatch.group( yygname )
    
    ## yyspan = yymatch.span()
    yyleng = yymatch.end() - yymatch.start() # does this really optimize anything substantially ?
    self._yy_offset = yymatch.end()
    
    exec ACTIONS[yygname]


class Lexer:
    '''  '''

    def reset( self, input_text = '' ):
        ''' reset the splitter '''

        self.yy_input_text = input_text

        self._yy_offset = 0
        self._yy_state = self.state_%(start_state)s
        
        ## self._yy_result = None # 'yylval'; if != None, we return this
        %(yylval_string)s = None
        %(yyreturn_string)s = None # misleading semantics, don't use this
        
        
        # user-defined initialization code, if any:
%(initialization_code)s

    def add_input( self, input_text, reset=False, eof_as_nl = True ):
        ''' add another piece of code to 'split' '''

        if reset:
            self.reset()
            
        tail = self.yy_input_text
        if tail:
            if eof_as_nl:
                # simulate end-of-line on end of input:
                self.yy_input_text = '\n'.join( (tail, input_text) )
            else: # don't insert any "fake" newlines
                self.yy_input_text += input_text
        else: # empty buffer so far, just reset it
            self.yy_input_text = input_text
        
        # don't check for the length in every sub-state:
        self._yy_length = len( self.yy_input_text )

    # ----------------------------------------------------------------------------------
    def __init__( self, input_text ):
    
        self.reset( input_text )

    # ----------------------------------------------------------------------------------        

    def %(goto_name)s(self, state_name):

        self._yy_state = getattr( self, 'state_' + state_name )

    # ----------------------------------------------------------------------------------        

    def run(self):
        ''' calls self.state_...() one-by-one, 
            until internal ._state reference would be set to None '''

        ##  YYLVAL_STRING = 'self._yylval' # <= 'yylval'
        ##  RETURN_STRING = 'self._yy_result' # <= 'RETURN'

        while( self._yy_state is not None ):
            
            # clear flags:
            %(yylval_string)s = None
            %(yyreturn_string)s = None
            
            self._yy_state()
            
            # return ?
            if %(yylval_string)s is not None:
                return %(yylval_string)s
            # obsolete, misleading semantics, do not use
            if %(yyreturn_string)s is not None:
                return %(yyreturn_string)s
            
        ## return None

    # ----------------------------------------------------------------------------------        
%(sublexer_methods)s

#
# a very simple test: read stdin, then parse it
#
if __name__ == "__main__":
    
    import sys
    text = sys.stdin.read()
    
    lexer = Lexer('')
    
    ## lexer.add_input(''' aaa bbb''')
    lexer.add_input( text )
    

    ## lexer.run()
    ret = lexer.run()
    while ret is not None:
        state, text = ret
        print "\t state: '%%s', text: '%%s'" %% ( state, text )
        ret = lexer.run()

"""
## sss

# ----------------------------------------------------------------------------------
# generate "%(code_fragments)s" for the above:

def print_code_defs_init():
    """ hiding "local"/"static function variables in a closure" """

    header = \
r"""
ACTIONS = {
"""
    ## # remove leading '\n': 
    ## header = header.lstrip()

    footer = \
r"""
}
"""

    entry_template = \
r"""
    '_%d' : \
r'''%s''',
"""

    def print_code_defs( action_map, const_name = 'ACTIONS' ):
        """ { 1: action_1, ... } => 
            ===
            ACTIONS = {
                '_1' : \
            r'''code_fragment_1''',
                '_2': \
            r'''code_fragment_2''',
            ...
            }
            ===
            
            *** NB: we could have used "%s" % repr(code) in the template instaed of "r'''%s'''" % code, but this *kills* readability ***
            
        """
        
        # action_map is assumed to be an OrderedDict, so there's no need for sorting the keys
        
        parts = [ header ]
        for key, value in action_map.iteritems(): 
            
            # repr() is a bit ugly, but since the action code 
            # may possibly contain '''/""" - strings, 
            # this is actually the only way ((( 
            # // TODO: better repr() ? // w/some '''/"""/r'''/r""" heuristics ?
            ## text = repr(value)
            text = value
            parts.append( entry_template % ( key, text ) ) 

        parts.append( footer )
        
        return ''.join( parts )
        
    return print_code_defs

# we need only one instance )
print_code_defs = print_code_defs_init()

# ----------------------------------------------------------------------------------
# generate regular expression definitions

def make_state_regexp_init(  ):
    """ hiding "local"/"static function variables in a closure" """
    
    header = \
r"""
re_%(state_name)s_str = r'''
"""
    
    template = '(?P<_%(action_id)d>%(regexp)s)' # implicitly contains <%d> => '_%d' transformation
    
    footer = \
r"""
'''

re_%(state_name)s = re.compile( re_%(state_name)s_str, re.VERBOSE )
"""
    
    def make_state_regexp( state_name, components, indent = '    ' ):
        """ 'state_name' is, well, a state name, and 
            'components' is a sequence of ( trigger_re, action_id <a number> ) ;
            the result would be an (unindented) method definition, 
            that could be indented with 'indent(text, indentation)' from above
        """
        
        ## make_verbose = lambda reg_exp: reg_exp.replace(' ', '[ ]').replace('\t', r'\t')
        # ^^^ not good inside character classes ~~~
        make_verbose = lambda reg_exp: reg_exp.replace(' ', r'\ ').replace('\t', r'\t')
        
        parts = [ header % locals() ]
        
        regexp_parts = []
        for re_trigger, action_id in components:
            ## regexps are just string literals, so to make them printable, we eval() them: 
            ## regexp = eval( re_trigger ) 
            ### regexp = re.escape( eval( re_trigger ) ) 
            re_str = strip_string_literal( re_trigger )
            regexp = make_verbose( re_str )
            regexp_parts.append( template % locals() )
            
        regexp_part = ( '\n%s|' % indent ).join( regexp_parts )
        # indent the first one:
        prefix = indent + ' ' # ' ' is for '|'
        
        parts.append( prefix + regexp_part )
        parts.append( footer % locals() )
        
        return ''.join( parts )

    return make_state_regexp


# we need only one instance )
make_state_regexp = make_state_regexp_init()

# ----------------------------------------------------------------------------------
# generate "%(sublexer_methods)s" for the above:

def print_sublexer_method_init( ):
    """ hiding "local"/"static function variables in a closure" """
    
    header_template = \
r"""
def state_%(state_name)s(self, _yy_regexp = re_%(state_name)s, _yy_lex_state_name = '%(state_name)s'):
    ''' state '%(state_name)s' '''

    _yy_state_code( self, _yy_regexp, _yy_lex_state_name )
"""
# unfortunately, the compiled code may not have any 'return' statements
##  ret = _yy_state_code( self, _yy_regexp, _yy_lex_state_name )
##  if ret is not None:
##      return ret


## ** All the code below repeats for every method, so we can just compile() it; **
## // we could have mede it an extra method function, since the parameters are rather well defined,
## // but what's the big difference ? ( function is compiled with the module, 
## //                                   but is one extra call; the code is compiled on import instead )
##
##  #
##  # check for end-of-input
##  #
##  if self._yy_offset == self._yy_length: 
##      self._yy_state = None # we're done
##      return # None
##  
##  yymatch = _regexp.match( self.yy_input_text, self._yy_offset )
##  if yymatch is None:
##      raise LexerError( "no matches found at offset %%s at state %%s" %% (self._yy_offset, _yy_lex_state_name) )
##      
##  # else ..
##  
##  yygname = yymatch.lastgroup
##  yytext = yymatch.group( yygname )
##  
##  ## yyspan = yymatch.span()
##  yyleng = yymatch.end() - yymatch.start() # does this really optimize anything substantially ?
##  self._yy_offset = yymatch.end()
##  
##  exec ACTIONS[yygname]
##

    
    def print_sublexer_method( state_name, indentation = '    ' ):
        """ 'state_name' is, well, a state name;
            the result would be an (unindented) method definition, 
            that could be indented with 'indent(text, indentation)' from above
        """

        text = header_template % locals()
        
        return indent(text, indentation)

    return print_sublexer_method

# we need only one instance )
print_sublexer_method = print_sublexer_method_init()


# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------

class LexSpecParser:

    def load_text(self, spec_text):

        self._spec_text = spec_text

    def __init__( self, spec_text = '' ):

        self.load_text( spec_text )
        # ... ...

    # ----------------------------------------------------------------------------------

    ## def _split_spec( self, _re_split = re.compile(r'\n\S') ):
    ## def _split_spec( self, _re_split = re.compile(r'(?<=\n)(?=\S)') ):
    def _split_spec( self, _re_split = re.compile(r'\n(?=\S)') ): # better
    ### def _split_spec( self, _re_split = re.compile( r'$(?=\S)', re.MULTILINE ) ): # !! '$' _precedes_ newlines !
        """ split the spec text into blocks '[<states>] "regexp"\n code' """
        
        parts = _re_split.split( self._spec_text ) # since it will miss a trailing '\n', we'd better add one
        self._parts = [ p + '\n' for p in parts if p ] # remove empty "chunks"
        ## self._parts = [ '\n' + p.strip() + '\n' for p in parts if p ] # remove empty "chunks" and surrounding spaces
        
        self._init_code = None # no initialization block by default
        
        # check for initialization code
        assert len( self._parts ) > 0, "empty specification?"
        # 'else ..'
        first_part = self._parts[0]
        # by construction, the parts are not empty, so we can check the first character:
        first_char = first_part[0]
        # since we break the text on every \n[\S], 
        # only the first line could start with a non-blank character:
        if first_char.isspace(): # then we have an "initialization block"
            ## init_code = remove_leading_indentation( first_part )
            ## init_code = expand_goto_macro( init_code )
            init_code = preprocess_action_code( first_part )

            self._init_code = init_code
            del self._parts[0] # or self._parts.pop(0)
        
        ##  # dbg
        ##  print '-' * 10
        ##  for p in parts:
        ##      print p
        ##      print '-' * 5
        ##  print '-' * 10
        ##  for p in self._parts:
        ##      print p
        ##      print '-' * 5
        ##  print '-' * 10
        ##  sys.exit(0)

    def _dump_parts( self ):
        
        ## pprint( self._parts )
        for p in self._parts:
            print '-' * 20
            ## print repr(p)
            print p
            print '-' * 20

    # ----------------------------------------------------------------------------------

    
    @staticmethod
    def translate( toknum, tokstr ):
        """ recognize a number of tuples to some our pre-defined constants """

        """
        if toknum == T.NL or toknum == T.NEWLINE:
            assert tokstr == '\n'
            return _NL

        elif toknum == DEDENT and tokstr == '':

            return CR
            
        elif toknum == OP:
        """
        if toknum == T.OP:
            
            if tokstr == '<' :
                return LT

            if tokstr == '>' :
                return GT

            if tokstr == ',' :
                return COMMA
        #
        # the next two are to help using some T.* constants as a result of a translate() call
        #
        elif toknum == T.STRING:

            return STR
        
        elif toknum == T.NAME:

            return NAME
        
        # ... 

        else:

            pass
        
        # default:

        return None # not known )


    # HACK hack: since the current tokenizer version does not read any lines "ahead",
    # we can skip till the end of the line and get the rest of the code by simply
    # checking the current position in the buffer:
    """
    @staticmethod
    def get_tail( stringio_buffer ):
        
        # buffer.getvalue()[buffer.tell():]
        ret = stringio_buffer.read() # read the rest )
    
        return ret
    """
    # ^^^ is used as <buffer>.read() directly ~~~

    # ----------------------------------------------------------------------------------
    
    @classmethod # a static method using another one; therefore, it's a class method )
    def _parse_section( cls, section ):
        """ a section starts with an optional number of states: <a,b,c>
            then continues with a string,
            and then has a number of instructions _after a newline_ 
        """
        
        # dbg:
        if 1:
            print '~' * 10
            print section
            print '~' * 10
        
        state_names = []
        str_regexp = None
        action_code = None
        
        buffer = io( section )
        g = T.generate_tokens( buffer.readline )
        
        # simple states:
        START = 0; # start of text
        STATE_DEF = 1; # after '<': name, ','
        EXPECT_REGEXP = 2; # expecting a string defining a regular expression
        EXIT = -1 # exit the "while" loop; ain't actually used )
        ERROR = -2 # also unused (not used directly by the "state machine", only as a internal hack-flag ))
        
        state = START
        while state != EXIT:

            ## # dbg
            ## print "state: %d" % (state, )

            #
            # check any error from the previous reads
            #

            if state == ERROR: # then we already have a tok_tuple
                
                e = ParseError(tok_tuple = tok_tuple)
                # dbg
                print >>sys.stderr, e.make_message( tok_tuple )
                raise e
        
            # else we're good to continue ...

        
            tok_tuple = next(g, None)
            if tok_tuple is None:
                break
                
            # else ..
        
            toknum, tokstr, _, _, _ = tok_tuple
            translated = cls.translate( toknum, tokstr ) # reduce some "known" tuples like OP, '<' to some pre-defined constants like 'LT'

            ## # dbg
            ## print "translated: %d" % ( translated,  )

            #
            # START: _ [ <state_name, ...> ] "regexp"
            #
            
            if START == state:
                
                if translated is None: # unknown line start
                    
                    state = ERROR
                    
                elif translated == LT:
                    
                    state = STATE_DEF
                    ## continue
                    
                elif translated == STR:
                    
                    # ***
                    state_names.append( INITIAL )
                    str_regexp = tokstr
                    action_code = buffer.read()
                    
                    state = EXIT
                    ## break
                    
                else:
            
                    state = ERROR

                """
                #
                # check the state and optionally stop )
                #

                if state == EXIT:
                    break
                
                elif state == ERROR:
                    
                    e = ParseError(tok_tuple = tok_tuple)
                    # dbg
                    print >>sys.stderr, e.make_message( tok_tuple )
                    raise e
                    
                # else:
                #     continue
                """
                
                ## continue
                
            #
            # STATE_DEF: < ... _ state_name [, ] ...> ]
            #
            
            elif state == STATE_DEF:
                
                if translated != NAME : # NAME expected
                    
                    state = ERROR
                    
                else:
                    
                    state_names.append( tokstr )
                    
                    _last_tuple = tok_tuple
                    tok_tuple = next(g, None)
                    
                    if tok_tuple is None:
                        
                        state = ERROR
                        
                        e = ParseError( "unexpected end of input in a state definition", tok_tuple = _last_tuple )
                        # dbg
                        print >>sys.stderr, "unexpected end of input in a state definition: \n", e.make_message( _last_tuple )
                        raise e
                    
                    else:
                        
                        toknum, tokstr, _, _, _ = tok_tuple
                        translated = cls.translate( toknum, tokstr ) # reduce some "known" tuples like OP, '<' to some pre-defined constants like 'LT'
                        
                        if translated == COMMA:
                            
                            # continue with the same state
                            pass
                            
                        elif translated == GT:
                            
                            state = EXPECT_REGEXP
                            # continue
                            
                        else:
                            state = ERROR
                            
                """
                #
                # check the state and optionally stop )
                #

                if state == EXIT:
                    break
                
                elif state == ERROR:
                    
                    e = ParseError(tok_tuple = tok_tuple)
                    # dbg
                    print >>sys.stderr, e.make_message( tok_tuple )
                    raise e
                    
                # else:
                #     continue
                """

            #
            # EXPECT_REGEXP: [ < state_name [, ] ...> ] _ "regexp" \n <code>
            #
            
            elif state == EXPECT_REGEXP:
                
                if translated != STR : # STRING expected
                    
                    state = ERROR
                    
                else:
                    
                    str_regexp = tokstr
                    action_code = buffer.read() # hack HACK: skip to the end of line
                    
                    state = EXIT

        # we're done here; we could optionally replace 'GOTO(state)' for 'GOTO("state")' in the action code, but basically we're done
        
        assert state_names, "states list may not be empty!"
        assert str_regexp, "an empty regular expression; would match anything"
        assert action_code.strip(), "empty code section!" # would be better to strip comments before, what would be in our "to-do" list )
        
        return  state_names, str_regexp, action_code

        
    # ----------------------------------------------------------------------------------

    # obsolete, needs applying of enumerate_actions()
    def parse_v1( self ):
        """ split the input buffer into sections and parse the sections; """

        ## states = {} # state: <list of pairs (regexp, action) >
        states = OrderedDict()
        # may be it's better to use an OrderedDict(0 from collections )

        self._split_spec()
        for p in self._parts:

            state_names, str_regexp, action_code = self._parse_section( p )
            for name in state_names:

                ## trigger = eval( str_regexp ) # just a string, so globals() / locals() can be ignored
                trigger = str_regexp # it's already a 'repr', no need in (eval/repr)-ing it again
                action = action_code

                tu = ( trigger, action )

                ## components = states.get( name, [] )
                components = states.setdefault( name, [] )

                components.append( tu )

        self._lex_states = states

    # related to the above version of .parsed_v1()
    def _dump_parsed_v1(self):

        for state_name, components in self._lex_states.iteritems():

            ## regexps = [ c[0] for c in components ]

            print "state '%s':" % (state_name, )

            for trigger, action in components:

                ## print '\t', repr(trigger)
                print '\t', (trigger)
                print '\t', '-' * 10
                print indent( action, '\t' )
                print '\t', '-' * 10

    # ----------------------------------------------------------------------------------

    def parse( self ):
        """ (a) split the input buffer into sections ; 
            (b) parse the sections ; 
            (c) pre-process and enumerate the actions ;
        """

        ## states = {} # state: <list of pairs (regexp, action) >
        # may be it's better to use an OrderedDict() from collections )
        states = OrderedDict()
        actions = OrderedDict()

        next_id = 0 # enumerating the actions

        self._split_spec()
        for p in self._parts:

            state_names, str_regexp, action_code = self._parse_section( p )
            
            ## action = action_code
            action = '\n' + action_code.strip() + '\n' # remove extra surrounding space fro readability

            next_id += 1 # so they start from 1
            
            ## action = remove_leading_indentation( action )
            ## action = expand_goto_macro( action )
            action = preprocess_action_code( action )
            
            actions[ next_id ] = action
            
            for name in state_names:

                ## trigger = eval( str_regexp ) # just a string, so globals() / locals() can be ignored
                trigger = str_regexp # it's already a 'repr', no need in (eval/repr)-ing it again

                tu = ( trigger, next_id )

                ## components = states.get( name, [] )
                components = states.setdefault( name, [] )

                components.append( tu )

        self._lex_states = states
        self._lex_actions = actions
    
    def _dump_parsed(self):

        actions = self._lex_actions

        for state_name, components in self._lex_states.iteritems():

            ## regexps = [ c[0] for c in components ]

            print "state '%s':" % (state_name, )

            for trigger, action_id in components:

                action = actions[ action_id ]

                ## print '\t', repr(trigger)
                print '\t', (trigger)
                print '\t', '-' * 10
                print indent( action, '\t' )
                print '\t', '-' * 10
    
    # ----------------------------------------------------------------------------------
    # above is the parser; below would be the builder )
    # ----------------------------------------------------------------------------------

    # obsolete, works on ._lex_states from .parse_v1()
    @staticmethod
    def enumerate_actions( lex_states ):
        """ { state => [(trigger, action)] } => { state => [(trigger, action_id)] }, { action_id: action } ;
            also does preprocessing of the code
        """

        ##  derived = {}
        ##  actions = {}
        derived = OrderedDict()
        actions = OrderedDict()
        
        next_id = 0
        for state_name, components in lex_states.iteritems():

            pairs = [] # trigger, action_id
            for trigger, action in components:
                
                next_id += 1 # so they start from 1
                
                action = remove_leading_indentation( action )
                action = expand_goto_macro( action )
                # action = preprocess_action_code( action )
                
                actions[ next_id ] = action
                
                pairs.append( (trigger, next_id) )
                
            derived[ state_name ] = pairs
            
        return derived, actions


    # ----------------------------------------------------------------------------------
    def make_lexer( self, goto_name = GOTO_METHOD_NAME ):
        """ assuming we have the spec parsed, generate the lexing code """
        
        ## LEXER_TEMPLATE
        ##  %(code_fragments)s
        ##  %(regexp_defs)s
        ##  %(start_state)s
        ##  %(yylval_string)s 
        ##  %(yyreturn_string)s 
        ##  %(initialization_code)s
        ##  %(goto_name)s
        ##  %(sublexer_methods)s

        ## self._yy_result = None # 'yylval'; if != None, we return this
        yylval_string = YYLVAL_STRING
        yyreturn_string = RETURN_STRING
        ##  YYLVAL_STRING = 'self._yylval' # <= 'yylval'
        ##  RETURN_STRING = 'self._yy_result' # <= 'RETURN'
        

        # initialization code:
        ## initialization_code = ''
        initialization_code = '# pass' # )
        if self._init_code is not None:
            initialization_code = indent( self._init_code, indentation = ' ' * 8 ) # 4 spaces from the class definition + 4 spaces from the __init__ method definition

        # a shortcut
        lex_states = self._lex_states

        # since states are now an OrderedDict, the order of states is preserved, and
        start_state = INITIAL
        if not lex_states.has_key( INITIAL ):
            for state_name in lex_states.iterkeys():
                start_state = state_name # take the first one
                break
        #   ^^^ 
        # so if the state list would somehow be empty, 
        # then the default state will still be 'INITIAL'
        # 

        code_fragments = print_code_defs( self._lex_actions )
        
        regexp_defs_parts = []
        method_decls = []
        
        method_indent = '    '

        for state_name, components in lex_states.iteritems():
            
            regexp_decl = make_state_regexp( state_name, components, indent = '    ' )
            regexp_defs_parts.append( regexp_decl )
            
            method_decl = print_sublexer_method( state_name, indentation = method_indent )
            method_decls.append( method_decl )
        
        regexp_defs = '\n'.join( regexp_defs_parts )
        sublexer_methods = ( '\n%s' % (method_indent, ) ).join( method_decls )
        
        return LEXER_TEMPLATE % locals()

# ----------------------------------------------------------------------------------
## ===============================================================================

def make_lexer( spec_text ):
    
    parser = LexSpecParser()
    parser.load_text( spec_text )
    parser.parse()
    
    lexer_code = parser.make_lexer( )
    return lexer_code


# ----------------------------------------------------------------------------------

def _lexer_fname_from_spec_fname( spec_fname, suffix = '_lexer.py' ):
    """ spec.lex => spec_lexer.py """
    
    path, fname = os.path.split( spec_fname )
    basename, ext = os.path.splitext( fname )
    
    result = os.path.join( path, basename + suffix )
    return result
    

def make_lexer_from_spec_file( spec_fname, lexer_fname = None ):
    """ read a file with the lexer specification and create a new lexer file """
    
    if lexer_fname is None:
        lexer_fname = _lexer_fname_from_spec_fname( spec_fname )

    spec_text = open( spec_fname ).read()
    lexer_text = make_lexer( spec_text )
    
    with open( lexer_fname, 'wt' ) as w:
        
        w.write( lexer_text )

    # return None


# ----------------------------------------------------------------------------------
## ===============================================================================

# --------------------------------------------------------------------------------------------

TEST_SPEC = \
r"""
<CODE> " [a-z]+" # actually, we could probably have optional comment here -- and just skip to '\n'

    print 'Hi there!'
    
    # more comments
    
<STRING> " [a-z]+" # just to test grouping
    GOTO( CODE ) # some nonsence to test "GOTO" expansion

<STRING, COMMENT> ".|\n"

    # some action
    
    pass

"""

# --------------------------------------------------------------------------------------------

if __name__ == "__main__":

    TEST_MODE = 0
    if not TEST_MODE:
    
        nargs = len( sys.argv[1:] )
        if 0 == nargs or '--help' == sys.argv[1] :
            print \
"""
spec file syntax:
================

----------------------------------------------

    # initialization code

[<state_name_1[, state_name_2, ...]>] "regexp"

    # action code

----------------------------------------------


pre-defined variables and macros:
================================

-- yytext   -- the matched text ;
-- yymatch  -- an internal variable, 
               don't define anything that starts with 'yy'
-- yyleng   -- pre-computed length of the matched text


GOTO( <state_name> ) -- expands to self._yy_set_state( 'state_name' ) 
                        and then acts as a state switch )

finally, to return a value, set sth to 'yylval':

<state> "trigger" :
    # some actions
    yylval = 42 # unfortunately, 'return'-ing a value 
                # is not implemented (e.g. via a raise )),
                # so execution of the action code will continue,
                # and only then a value would be returned

NOTE: if you want to preserve data, assign it to 'self':

    self.someattr = somevalue

, since the lexer would be generated as a class,
to allow multiple lexer instances to co-exist, just in case )


also there is a small number of internal lexer class
attributes and methods, normally starting with ._yy_... ;
if not all the attributes are "fully yy-ified", please
please feel free to examine the generated code for any 
(unintentional) clashes ;

some of these additional internal names / attributes are:

-- _yy_lex_state_name:
            current state name ( as a Python string ) ;
-- ._yy_set_state():
            an internal method that switches between states ;

some methods, such as .reset() [, add_text(), run(), 
state_<state_name>()], and some arguments, 
are *not* "yy-ified" for readability ; 
since the generated code is quite small -- please feel free
to edit it )


usage: 
=====

%s <spec file> [<output_file>]

""" % sys.argv[0]
        
        # ^^^ if 0 == nargs
        
        else: # nargs >= 1 :
            result_name = None
            if nargs >= 2:
                result_name = sys.argv[2]
                
            make_lexer_from_spec_file( sys.argv[1], result_name )
    
    else: # TEST_MODE 'on'
        
        parser = LexSpecParser()
        parser.load_text( TEST_SPEC )
        parser._split_spec()
        
        if 0:
            parser._dump_parts()
            pprint( parser._parts )
        
        parser.parse()
        
        if 0:
            parser._dump_parsed()
            
        if 0:
            ## reduced, actions = parser.enumerate_actions( parser._lex_states )
            print '-' * 10
            ## pprint( dict(actions) )
            print print_code_defs( parser._lex_actions )
            print '-' * 10
            

        if 0:
            print '=' * 10
            for state_name, components in parser._lex_states.iteritems():
                print '-' * 10
                print make_state_regexp( state_name, components, indent = '    ' )
                print '-' * 10
            print '=' * 10

        if 0:
            print '=' * 10
            for state_name, _ in parser._lex_states.iteritems():
                print '-' * 10
                print print_sublexer_method( state_name, indentation = '    ' )
                print '-' * 10
            print '=' * 10


        if 1:
            print '=' * 10
            print parser.make_lexer( )
            print '=' * 10
