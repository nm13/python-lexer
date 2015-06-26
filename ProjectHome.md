## pylex ##
_A simple lexer based on Python regular expressions with lex-alike syntax._

Note: I tend to call this **`pylex`**; to avoid confusion -- there's another, rather stable and mature lexer called [Plex](http://pythonhosted.org/plex/tutorial.html), so you may be looking for this one instead )


---

Usage: `lex.py <spec file> [<output file>]` . This creates a module with a Lexer class, that you can observe (as we don't have to implement regular expression mechanics, the generated file is aimed to be quite readable), and even maybe test right away:
```sh

./lex.py specfile.lex lexer.py

lexer.py <<'EOF'
<some test input>
EOF

```

For help, run `lex.py --help` or just `lex.py` without parameters. For a usage example for the created Lexer class, look into the `if __name__ == "__main__":` section of the generated file.


---

A sample specification (since Google Wiki spoils the code formatting -- try to imagine the code being properly aligned -- or just cut-&-paste it into a normal code editor):
```python


# set an initial state different from "INITIAL" (default):
GOTO( FIRST )

<FIRST, SECOND> "[a-z]+" # "any word in the lower case"

# 'yytext' is, well, "yytext",
# and the state name is also available:
print "<in %s>: '%s'" % ( _yy_lex_state_name, yytext )

# specifying an 'yylval' will return a value from the lexer
# yylval = ( _yy_lex_state_name, yytext)

<FIRST> "\s+" # "separating space"

# "BEGIN(SECOND)" : test switching the states after the first token seen.
#
# NB. I have changed the name from "BEGIN",
#     since there's never an "END" and it's kind of asymmetrical; )
#     Besides, while "BEGIN" may exist in your code,
#     "GOTO" is almost banned from the language --
#     -- and therefore is a reasonably safe bet ))
GOTO( SECOND )


<FIRST, SECOND> ".|\n" # rest of input

## print "<in %s>: '%s'" % ( _yy_lex_state_name, yytext )
print "rest of input: '%s'" % (yytext, )

## usage example (see the generated file):
# import sys
# text = sys.stdin.read() # for the sake of having a simple example )
#
# lexer = Lexer( text )
# # lexer.add_input( text )
#
# lexer.run()


```


---

**`wc`** spec example:
```python


# ==========================================================

# adapted from [ http://epaperpress.com/lexandyacc/prl.html ]

# initialize the counters; since we're making a class,
# any value that one wants to be persistent should go to an attribute
self.nlines = self.nwords = self.nchars = 0

"\n" # if there is no state name, it would be "INITIAL", following the common convention

self.nlines += 1
self.nchars += 1

"[^ \t\n]+"

## print '%s' % ( yytext,  )
self.nwords += 1
self.nchars += yyleng

"."

self.nchars += 1

# ==========================================================

# afterwards, just output the above lexer fields with sth like:
# print "%d lines, %d words, %d chars" % ( lexer.nlines, lexer.nwords, lexer.nchars )

```

<a href='Hidden comment: 
----
'></a>

