((Rule
  ((selectors
    (".foo:matches(.bar" ".baz)" ".foo:matches(.bar" ".baz)"
     ".foo:matches(.bar " ".baz)" ".foo:matches(.bar " ".baz) "))
   (ruleset ((prop ((Identifier value)))))))
 (Rule
  ((selectors
    (".foo:matches(.bar" .baz ".foobar)" ".foo:matches(.bar" .baz ")"
     ".foo:matches(" ".bar " ".baz) "))
   (ruleset ((anotherprop ((Identifier anothervalue))))))))