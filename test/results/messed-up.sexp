((Rule ((selectors ("body ")) (ruleset ((foo ((String ' bar)))))))
 (Rule
  ((selectors (body))
   (ruleset ((foo ((Identifier bar))) (bar ((Identifier baz)))))))
 (Rule
  ((selectors ( "body\
               \n   "))
   (ruleset ((foo ((Identifier bar))) (bar ((Identifier baz))))))))