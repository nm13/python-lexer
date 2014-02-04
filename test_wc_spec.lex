
    # adapted from [ http://epaperpress.com/lexandyacc/prl.html ]

    # initialize the counters
    self.nline = self.nword = self.nchar = 0

"\n"
    self.nline += 1
    self.nchar += 1
"[^ \t\n]+" 
    ## print '%s' % ( yytext,  )
    self.nword += 1
    self.nchar += yyleng
"."
    self.nchar += 1 


    # afterwards, just output the above lexer fields with sth like:
    # print "%d lines, %d words, %d chars" % ( lexer.nline, lexer.nword, lexer.nchar )
    

