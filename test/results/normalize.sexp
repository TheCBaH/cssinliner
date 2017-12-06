((Rule
  ((selectors
    (article aside details figcaption figure footer header hgroup main nav
     section "summary "))
   (ruleset ((display ((Identifier block)))))))
 (Rule
  ((selectors (audio canvas "video "))
   (ruleset ((display ((Identifier inline-block)))))))
 (Rule
  ((selectors ("audio:not([controls]) "))
   (ruleset ((display ((Identifier none))) (height ((Any 0)))))))
 (Rule ((selectors ("[hidden] ")) (ruleset ((display ((Identifier none)))))))
 (Rule
  ((selectors ("html "))
   (ruleset
    ((font-family ((Identifier sans-serif)))
     (-webkit-text-size-adjust ((Any 100%)))
     (-ms-text-size-adjust ((Any 100%)))))))
 (Rule ((selectors ("body ")) (ruleset ((margin ((Any 0)))))))
 (Rule
  ((selectors ("a:focus "))
   (ruleset ((outline ((Identifier thin) (Identifier dotted)))))))
 (Rule ((selectors (a:active "a:hover ")) (ruleset ((outline ((Any 0)))))))
 (Rule
  ((selectors ("h1 "))
   (ruleset ((font-size ((Any 2em))) (margin ((Any "0.67em 0")))))))
 (Rule
  ((selectors ("abbr[title] "))
   (ruleset ((border-bottom ((Any "1px dotted")))))))
 (Rule
  ((selectors (b "strong ")) (ruleset ((font-weight ((Identifier bold)))))))
 (Rule ((selectors ("dfn ")) (ruleset ((font-style ((Identifier italic)))))))
 (Rule
  ((selectors ("hr "))
   (ruleset
    ((-moz-box-sizing ((Identifier content-box)))
     (box-sizing ((Identifier content-box))) (height ((Any 0)))))))
 (Rule
  ((selectors ("mark "))
   (ruleset ((background ((Any #ff0))) (color ((Any #000)))))))
 (Rule
  ((selectors (code kbd pre "samp "))
   (ruleset
    ((font-family ((Identifier monospace) (Any ", serif")))
     (font-size ((Any 1em)))))))
 (Rule
  ((selectors ("pre ")) (ruleset ((white-space ((Identifier pre-wrap)))))))
 (Rule
  ((selectors ("q "))
   (ruleset
    ((quotes
      ((String "\"" "\\201C") (String "\"" "\\201D") (String "\"" "\\2018")
       (String "\"" "\\2019")))))))
 (Rule ((selectors ("small ")) (ruleset ((font-size ((Any 80%)))))))
 (Rule
  ((selectors (sub "sup "))
   (ruleset
    ((font-size ((Any 75%))) (line-height ((Any 0)))
     (position ((Identifier relative)))
     (vertical-align ((Identifier baseline)))))))
 (Rule ((selectors ("sup ")) (ruleset ((top ((Identifier -0) (Any .5em)))))))
 (Rule
  ((selectors ("sub ")) (ruleset ((bottom ((Identifier -0) (Any .25em)))))))
 (Rule ((selectors ("img ")) (ruleset ((border ((Any 0)))))))
 (Rule
  ((selectors ("svg:not(:root) "))
   (ruleset ((overflow ((Identifier hidden)))))))
 (Rule ((selectors ("figure ")) (ruleset ((margin ((Any 0)))))))
 (Rule
  ((selectors ("fieldset "))
   (ruleset
    ((border ((Any "1px solid #c0c0c0"))) (margin ((Any "0 2px")))
     (padding ((Any "0.35em 0.625em 0.75em")))))))
 (Rule
  ((selectors ("legend "))
   (ruleset ((border ((Any 0))) (padding ((Any 0)))))))
 (Rule
  ((selectors (button input select "textarea "))
   (ruleset
    ((font-family ((Identifier inherit))) (font-size ((Any 100%)))
     (margin ((Any 0)))))))
 (Rule
  ((selectors (button "input "))
   (ruleset ((line-height ((Identifier normal)))))))
 (Rule
  ((selectors (button "select "))
   (ruleset ((text-transform ((Identifier none)))))))
 (Rule
  ((selectors
    (button "html input[type=\"button\"]" "input[type=\"reset\"]"
     "input[type=\"submit\"] "))
   (ruleset
    ((-webkit-appearance ((Identifier button)))
     (cursor ((Identifier pointer)))))))
 (Rule
  ((selectors (button[disabled] "html input[disabled] "))
   (ruleset ((cursor ((Identifier default)))))))
 (Rule
  ((selectors ("input[type=\"checkbox\"]" "input[type=\"radio\"] "))
   (ruleset ((box-sizing ((Identifier border-box))) (padding ((Any 0)))))))
 (Rule
  ((selectors ("input[type=\"search\"] "))
   (ruleset
    ((-webkit-appearance ((Identifier textfield)))
     (-moz-box-sizing ((Identifier content-box)))
     (-webkit-box-sizing ((Identifier content-box)))
     (box-sizing ((Identifier content-box)))))))
 (Rule
  ((selectors
    ("input[type=\"search\"]::-webkit-search-cancel-button"
     "input[type=\"search\"]::-webkit-search-decoration "))
   (ruleset ((-webkit-appearance ((Identifier none)))))))
 (Rule
  ((selectors (button::-moz-focus-inner "input::-moz-focus-inner "))
   (ruleset ((border ((Any 0))) (padding ((Any 0)))))))
 (Rule
  ((selectors ("textarea "))
   (ruleset
    ((overflow ((Identifier auto))) (vertical-align ((Identifier top)))))))
 (Rule
  ((selectors ("table "))
   (ruleset
    ((border-collapse ((Identifier collapse))) (border-spacing ((Any 0))))))))