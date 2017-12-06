((Rule
  ((selectors (".foo[bar=\"baz" "quz\"] ")) (ruleset ((foobar ((Any 123)))))))
 (Rule
  ((selectors (.bar "#bar[baz=\"qux" "foo\"]" "#qux "))
   (ruleset ((foobar ((Any 456)))))))
 (Rule
  ((selectors
    (".baz[qux=\"" "foo\"]" ".baz[qux=\"foo" "\"]" ".baz[qux=\"foo" bar
     "baz\"]" ".baz[qux=\"" foo bar baz "\"]" ".baz[qux=\" " "foo " "bar "
     "baz " "\"] "))
   (ruleset ((foobar ((Any 789)))))))
 (Rule
  ((selectors
    (.qux[foo='bar baz'] ".qux[bar=\"baz" "foo\"]" "#qux[foo=\"foobar\"]"
     #qux[foo=' bar baz "'] "))
   (ruleset ((foobar ((Any 012)))))))
 (Rule
  ((selectors
    ("#foo[foo=\"\"]" "#foo[bar=\" \"]" "#foo[bar=\"" "\"]" "#foo[bar=\""
     "\"]" "#foo[bar=\" " "\"]" "#foo[bar=\" " "\"]" #foo[baz='']
     "#foo[qux=' ']" #foo[qux=' '] #foo[qux=' '] "#foo[qux=' " ']
     "#foo[qux=' " "'] "))
   (ruleset ((foobar ((Any 345))))))))