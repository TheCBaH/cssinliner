((At keyframes ((Identifier foo))
  (Compound
   (((selectors ("0% ")) (ruleset ((top ((Any 0))) (left ((Any "0 "))))))
    ((selectors ("30.50% ")) (ruleset ((top ((Any "50px "))))))
    ((selectors (".68% "  "72%\
                         \n      " "85% "))
     (ruleset ((left ((Any "50px "))))))
    ((selectors ("100% "))
     (ruleset ((top ((Any 100px))) (left ((Any "100% "))))))))))