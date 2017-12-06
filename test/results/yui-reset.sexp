((Rule
  ((selectors (html))
   (ruleset ((color ((Any #000))) (background ((Any #FFF)))))))
 (Rule
  ((selectors
    (body div dl dt dd ul ol li h1 h2 h3 h4 h5 h6 pre code form fieldset
     legend input textarea p blockquote th td))
   (ruleset ((margin ((Any 0))) (padding ((Any 0)))))))
 (Rule
  ((selectors (table))
   (ruleset
    ((border-collapse ((Identifier collapse))) (border-spacing ((Any 0)))))))
 (Rule ((selectors (fieldset img)) (ruleset ((border ((Any 0)))))))
 (Rule
  ((selectors (address caption cite code dfn em strong th var))
   (ruleset
    ((font-style ((Identifier normal))) (font-weight ((Identifier normal)))))))
 (Rule ((selectors (li)) (ruleset ((list-style ((Identifier none)))))))
 (Rule
  ((selectors (caption th)) (ruleset ((text-align ((Identifier left)))))))
 (Rule
  ((selectors (h1 h2 h3 h4 h5 h6))
   (ruleset ((font-size ((Any 100%))) (font-weight ((Identifier normal)))))))
 (Rule
  ((selectors (q:before q:after)) (ruleset ((content ((String ' "")))))))
 (Rule
  ((selectors (abbr acronym))
   (ruleset ((border ((Any 0))) (font-variant ((Identifier normal)))))))
 (Rule
  ((selectors (sup)) (ruleset ((vertical-align ((Identifier text-top)))))))
 (Rule
  ((selectors (sub)) (ruleset ((vertical-align ((Identifier text-bottom)))))))
 (Rule
  ((selectors (input textarea select))
   (ruleset
    ((font-family ((Identifier inherit))) (font-size ((Identifier inherit)))
     (font-weight ((Identifier inherit)))))))
 (NotParsed "input,textarea,select{*font-size:100%;")
 (Rule ((selectors (legend)) (ruleset ((color ((Any #000))))))))