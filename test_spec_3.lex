
    # initial state
    GOTO( FIRST )

<FIRST, SECOND> "[a-z]+" # "any tokens"

    print "<in %s>: '%s'" % ( _yy_lex_state_name, yytext )
    # RETURN ( _yy_lex_state_name, yytext)
    # or:
    yylval = ( _yy_lex_state_name, yytext)
    
<FIRST> "\s+" # "any separator"
    ## print "<in %s>: '%s'" % ( _yy_lex_state_name, yytext )
    GOTO( SECOND )
    
    
<FIRST, SECOND> ".|\n" # rest of input

    ## print "<in %s>: '%s'" % ( _yy_lex_state_name, yytext )
    print "rest of input: '%s'" % (yytext, )
    
