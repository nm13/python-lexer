./lex.py test_wc_spec.lex wc_test.py >/dev/null

echo '    print "%d lines, %d words, %d chars" % ( lexer.nline, lexer.nword, lexer.nchar )' >>wc_test.py

python wc_test.py <<'EOF'
aaa bbb ccc
dd ee ff
EOF

