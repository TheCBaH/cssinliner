((Rule ((selectors ("#wrap #a ")) (ruleset ((color ((Identifier red)))))))
 (Rule ((selectors ("#wrap a ")) (ruleset ((color ((Any #000)))))))
 (Rule ((selectors ("#a ")) (ruleset ((color ((Any #00f)))))))
 (Rule
  ((selectors ("div[a=b] .wrap div "))
   (ruleset ((display ((Identifier block)))))))
 (Rule
  ((selectors ("#wrap .wrap "))
   (ruleset ((display ((Identifier inline-block)))))))
 (Rule
  ((selectors ("#wrap div.wrap #a ")) (ruleset ((background ((Any #000)))))))
 (Rule
  ((selectors ("#wrap[a=b] div.wrap #a "))
   (ruleset ((background ((Identifier green)))))))
 (Rule
  ((selectors ("#wrap[a=c] div.wrap #a "))
   (ruleset ((background ((Any #00f)))))))
 (Rule
  ((selectors ("#wrap .wrap #a ")) (ruleset ((background ((Any #f00)))))))
 (Rule
  ((selectors ("#wrap .wrap #a "))
   (ruleset ((border-color ((Identifier green)))))))
 (Rule
  ((selectors ("div #wrap-2 #a "))
   (ruleset ((border-color ((Any #008000))))))))