module HTML where

import Text.XHtml

-- page :: String
-- page = concat $ 
--       htmlHeader
--     : pageHead
--     : pageNav
--     : htmlFoot
--     : [[]]

htmlHeader :: String
htmlHeader = concat $
      "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
    : "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
    : "      \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
    : "\n"
    : "\n"
    : "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n"
    : "\n"
    : "  <head>\n"
    :("    <title>" ++ pageTitle ++ "</title>\n")
    : "\n"
    : akvsHead
    : "\n"
    : "    <link rel=\"stylesheet\" href=\"log.css\" type=\"text/css\" />\n"
    : "  </head>\n"
    : [[]]

pageTitle :: String
pageTitle = "Weblog of Frosch03"

akvsHead :: String
akvsHead = concat $ 
      "<!-- AKVS head start v1.5 -->\n"
    : "<style type=\"text/css\">\n"
    : "<!--\n"
    : "div#akct {\n"
    : "    position: absolute; top:0px; right: 0px; z-index: 2342; width:113px; height:88px;\n"
    : "    background-image: url(http://wiki.vorratsdatenspeicherung.de/images/Akvse.gif);\n"
    : "    background-repeat: no-repeat;\n"
    : "    background-position: right top;\n"
    : "    border:none;\n"
    : "    padding:0;\n"
    : "    margin:0;\n"
    : "    text-align: right;\n"
    : "}\n"
    : "\n"
    : "div#akct img {\n"
    : "    border:none;\n"
    : "    padding:0;\n"
    : "    margin:0;\n"
    : "    background: none;\n"
    : "}\n"
    : "\n"
    : "div#akct a#akpeel img {\n"
    : "        width: 113px;\n"
    : "        height: 88px;\n"
    : "}\n"
    : "\n"
    : "div#akct a, div#akct a:hover {\n"
    : "    text-decoration: none;\n"
    : "    border:none;\n"
    : "    padding:0;\n"
    : "    margin:0;\n"
    : "    display: block;\n"
    : "    background: none;\n"
    : "}\n"
    : "\n"
    : "div#akct a#akpeel:hover {\n"
    : "    position: absolute; top:0px; right: 0px; z-index: 4223; width:500px; height:500px;\n"
    : "    display: block;\n"
    : "    background-image: url(http://wiki.vorratsdatenspeicherung.de/images/Akvsb.gif);\n"
    : "    background-repeat: no-repeat;\n"
    : "    background-position: right top;\n"
    : "}\n"
    : "\n"
    : "div#akct a#akpreload {\n"
    : "    background-image: url(http://wiki.vorratsdatenspeicherung.de/images/Akvsb.gif);\n"
    : "    background-repeat: no-repeat;\n"
    : "    background-position: 234px 0px;\n"
    : "}\n"
    : "-->\n"
    : "</style>\n"
    : "<!--[if gte IE 5.5]>\n"
    : "<![if lt IE 7]>\n"
    : "<style type=\"text/css\">\n"
    : "div#akct a#akpeel:hover {\n"
    : "        right: -1px;\n"
    : "}\n"
    : "</style>\n"
    : "<![endif]>\n"
    : "<![endif]-->\n"
    : "<!-- AKVS head end -->\n"
    : [[]]

akvsBody :: String
akvsBody = concat $ 
      "<!-- AKVS body start v1.5 -->\n"
    : "<div id=\"akct\"><a id=\"akpeel\" href=\"http://www.vorratsdatenspeicherung.de\" title=\"Stoppt die Vorratsdatenspeicherung! Jetzt klicken &amp; handeln!\"><img src=\"http://wiki.vorratsdatenspeicherung.de/images/Akvst.gif\" alt=\"Stoppt die Vorratsdatenspeicherung! Jetzt klicken &amp;handeln!\" /></a>\n"
    : "<a id=\"akpreload\" href=\"http://wiki.vorratsdatenspeicherung.de/?title=Online-Demo\" title=\"Willst du auch an der Aktion teilnehmen? Hier findest du alle relevanten Infos und Materialien:\"><img src=\"http://wiki.vorratsdatenspeicherung.de/images/Akvsi.gif\" alt=\"Willst du auch an der Aktion teilnehmen? Hier findest du alle relevanten Infos und Materialien:\" /></a></div>\n"
    : "<!-- AKVS body end -->\n"
    : [[]]


htmlFoot :: String
htmlFoot = concat $ 
      "      <a href=\"http://validator.w3.org/check/referer\">\n"
    : "       <img style=\"border:0;width:88px;height:31px\"\n"
    : "            src=\"http://www.w3.org/Icons/valid-xhtml10\"\n"
    : "            alt=\"Valid XHTML 1.0!\" height=\"31\" width=\"88\" /></a>\n"
    : "\n"
    : "      <a href=\"http://jigsaw.w3.org/css-validator/\">\n"
    : "       <img style=\"border:0;width:88px;height:31px\"\n"
    : "            src=\"http://jigsaw.w3.org/css-validator/images/vcss\" \n"
    : "            alt=\"Valid CSS!\" /></a>\n"
    : "\n"
    : "    </div>\n"
--    : "  </body>\n"
--    : "</html>\n"
    : [[]]


htmlBody :: String
htmlBody = concat $ 
      "  <body>\n"
    : "\n"
    : akvsBody
    : "\n"
    : pageHead
    : [[]]


pageHead :: String
pageHead = concat $ 
      "    <div class=\"top\">\n"
    : "    <a name=\"top\"></a>\n"
    : "    <img src=\"img/www.png\" alt=\"www.frosch03.de\" />\n"
    : "    </div>\n"
    : [[]]


pageNav :: String
pageNav = concat $
      "    <div class=\"left\">\n"
    : "    <pre class=\"code\"> \n"
    : "  Table of Content \n"
    : "--------------------- \n"
    : "  1) <a href=\"index.html#intro\">introduction</a>\n"
    : "  2) <a href=\"index.html#about\">about me</a>\n"
    : "  3) <a href=\"index.html#publi\">pulications</a>\n"
    : "\n"
    : "  4) <a href=\"index.html#secur\">security project</a>\n"
    : "  5) <a href=\"index.html#picts\">pictures</a>\n"
    : "  6) <a href=\"index.html#brain\">brainfuck</a>\n"
    : "  7) <a href=\"index.html#conts\">contact me</a>\n"
    : "  8) <a href=\"index.html#links\">links</a>\n"
    : "\n"
    : "    </pre>\n"
    : "    </div>\n"
    : [[]]
