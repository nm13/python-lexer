./lex.py test_spec_2.lex >/dev/null
cat test_spec_2_lexer.py | grep 'self\._[^y]'
