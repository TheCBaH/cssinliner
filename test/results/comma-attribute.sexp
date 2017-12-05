((Rule (".foo[bar=\"baz" "quz\"] ") ((foobar ((Any 123)))))
 (Rule (.bar "#bar[baz=\"qux" "foo\"]" "#qux ") ((foobar ((Any 456)))))
 (Rule
  (".baz[qux=\"" "foo\"]" ".baz[qux=\"foo" "\"]" ".baz[qux=\"foo" bar
   "baz\"]" ".baz[qux=\"" foo bar baz "\"]" ".baz[qux=\" " "foo " "bar "
   "baz " "\"] ")
  ((foobar ((Any 789)))))
 (Rule
  (.qux[foo='bar baz'] ".qux[bar=\"baz" "foo\"]" "#qux[foo=\"foobar\"]"
   #qux[foo=' bar baz "'] ")
  ((foobar ((Any 012)))))
 (Rule
  ("#foo[foo=\"\"]" "#foo[bar=\" \"]" "#foo[bar=\"" "\"]" "#foo[bar=\"" "\"]"
   "#foo[bar=\" " "\"]" "#foo[bar=\" " "\"]" #foo[baz=''] "#foo[qux=' ']"
   #foo[qux=' '] #foo[qux=' '] "#foo[qux=' " '] "#foo[qux=' " "'] ")
  ((foobar ((Any 345))))))