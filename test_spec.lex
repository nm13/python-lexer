
    # initial state
    GOTO( FIRST )

<FIRST> "[a-z]+" # "any tokens"

    print "<in FIRST>: '%s'" % ( yytext, )
    
<FIRST> "\s+" # "any separator"
    GOTO( SECOND )
    
<SECOND> "[a-z]+" 

    print "<in SECOND>: '%s'" % ( yytext, )
    
<FIRST, SECOND> "." # rest of input

    print "rest of input: '%s'" % (yytext, )
    
