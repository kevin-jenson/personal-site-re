// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as React from "react";
import * as Gatsby from "gatsby";
import * as ReactHelmet from "react-helmet";

function Seo(Props) {
  var descriptionOpt = Props.description;
  var langOpt = Props.lang;
  var metaOpt = Props.meta;
  var title = Props.title;
  var description = descriptionOpt !== undefined ? descriptionOpt : "";
  var lang = langOpt !== undefined ? langOpt : "en";
  var meta = metaOpt !== undefined ? metaOpt : /* [] */0;
  var match = Gatsby.useStaticQuery(Gatsby.graphql("\n		query {\n			site {\n				siteMetadata {\n					title\n					description\n					author\n				}\n			}\n		}\n	"));
  var site = match.site;
  var metaDescription = description.length === 0 ? site.siteMetadata.description : description;
  var baseMeta_000 = {
    name: "desctiption",
    content: metaDescription
  };
  var baseMeta_001 = /* :: */[
    {
      content: title,
      property: "og:title"
    },
    /* :: */[
      {
        content: description,
        property: "og:description"
      },
      /* :: */[
        {
          content: "website",
          property: "og:type"
        },
        /* :: */[
          {
            name: "twitter:card",
            content: "summary"
          },
          /* :: */[
            {
              name: "twitter:creator",
              content: site.siteMetadata.author
            },
            /* :: */[
              {
                name: "twitter:title",
                content: title
              },
              /* :: */[
                {
                  name: "twitter:description",
                  content: metaDescription
                },
                /* [] */0
              ]
            ]
          ]
        ]
      ]
    ]
  ];
  var baseMeta = /* :: */[
    baseMeta_000,
    baseMeta_001
  ];
  return React.createElement(ReactHelmet.Helmet, {
              htmlAttributes: {
                lang: lang
              },
              title: title,
              titleTemplate: "%s | " + (String(site) + ".siteMetadata.title"),
              meta: List.concat(/* :: */[
                    baseMeta,
                    /* :: */[
                      meta,
                      /* [] */0
                    ]
                  ])
            });
}

var make = Seo;

export {
  make ,
  
}
/* react Not a pure module */
