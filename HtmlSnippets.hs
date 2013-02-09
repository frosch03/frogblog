module HtmlSnippets 
    ( pageHead
    , pageNav
    , pageW3C
    , pageStatics
    , pageTwitter
    , akvsHead
    , akvsBody
    , livefyreSnip
    , latexMathML
    )
where

pageHead :: String
pageHead = concat $ 
      "<div class=\"top\">\n"
    : "    <a href=\"http://frosch03.de/blog\">\n"
    : "    <img src=\"../img/blog.gif\" alt=\"frosch03.de/blog\" />\n"
    : "    </a>\n"
    : "</div>\n"
    : [[]]


pageNav :: String
pageNav = concat $
      "  Table of Content \n"
    : "--------------------- \n"
    : "  1) <a href=\"http://frosch03.de\">Blog</a>\n"
    : "  2) <a href=\"http://frosch03.de/pad/\">Pads</a>\n"
    : "  3) <a href=\"http://pics.frosch03.de\">Pictures</a>\n"
    : [[]]

pageStatics :: String
pageStatics = concat $
      "+---------------------------+\n"
    : "|  Static Content           |\n"
    : "+---------------------------+\n"
    : "|                           |\n"
    : "|  * <a href=\"http://frosch03.de/blogfrog.cgi?subject=Alter%20Kontent\">old frosch03.de</a>        |\n"
    : "|                           |\n"
    : "|  * <a href=\"http://frosch03.de/pad/GrillParty.html\">Pad: GrillParty</a>        |\n"
    : "|                           |\n"
    : "|  * <a href=\"http://frosch03.de/linklist/groupchat.html\">Groupchat Links</a>        |\n"
    : "|                           |\n"
    : "+---------------------------+\n"
    : [[]]

pageTwitter :: String
pageTwitter = concat $ 
      "<script charset=\"utf-8\" src=\"http://widgets.twimg.com/j/2/widget.js\"></script>\n"
    : "<script>\n"
    : "new TWTR.Widget({\n"
    : "  version: 2,\n"
    : "  type: 'profile',\n"
    : "  rpp: 4,\n"
    : "  interval: 30000,\n"
    : "  width: 200,\n"
    : "  height: 300,\n"
    : "  theme: {\n"
    : "    shell: {\n"
    : "      background: '#333333',\n"
    : "      color: '#cccccc'\n"
    : "    },\n"
    : "    tweets: {\n"
    : "      background: '#333333',\n"
    : "      color: '#cccccc',\n"
    : "      links: '#808080'\n"
    : "    }\n"
    : "  },\n"
    : "  features: {\n"
    : "    scrollbar: false,\n"
    : "    loop: false,\n"
    : "    live: false,\n"
    : "    behavior: 'all'\n"
    : "  }\n"
    : "}).render().setUser('frosch03').start();\n"
    : "</script>\n"
    : [[]]


pageW3C :: String
pageW3C = concat $ 
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
    : [[]]

latexMathML :: String
latexMathML = concat $
      "<script type=\"text/javascript\" src=\"http://math.etsu.edu/LaTeXMathML/LaTeXMathML.js\"></script>"
    : "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://math.etsu.edu/LaTeXMathML/LaTeXMathML.standardarticle.css\" />"
    : [[]]

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


livefyreSnip :: String
livefyreSnip = concat $
      "<!-- START: Livefyre Embed -->\n"
    : "<div id=\"livefyre-comments\"></div>\n"
    : "<script type=\"text/javascript\" src=\"http://zor.livefyre.com/wjs/v3.0/javascripts/livefyre.js\"></script>\n"
    : "<script type=\"text/javascript\">\n"
    : "(function () {\n"
    -- : "var articleId = fyre.conv.load.makeArticleId(null, ['subject']);\n"
    : "var articleId = fyre.conv.load.makeArticleId(null);\n"
    : "fyre.conv.load({}, [{\n"
    : "el: 'livefyre-comments',\n"
    : "network: \"livefyre.com\",\n"
    : "siteId: \"322764\",\n"
    : "articleId: articleId,\n"
    : "signed: false,\n"
    : "collectionMeta: {\n"
    : "articleId: articleId,\n"
    : "url: fyre.conv.load.makeCollectionUrl(),\n"
    : "}\n"
    : "}], function() {});\n"
    : "}());\n"
    : "</script>\n"
    : "<!-- END: Livefyre Embed -->\n"
    : [[]]
