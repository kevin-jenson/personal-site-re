open Gatsby;

[@react.component]
let make = (~description="", ~lang="en", ~meta=[], ~title) => {
  let {site}: Gatsby.staticQuery =
    Gatsby.graphql(
      {js|
		query {
			site {
				siteMetadata {
					title
					description
					author
				}
			}
		}
	|js},
    )
    |> Gatsby.useStaticQuery;

  let metaDescription =
    String.length(description) == 0
      ? site.siteMetadata.description : description;

  let baseMeta = [
    ReactHelmet.meta(~name="desctiption", ~content=metaDescription, ()),
    ReactHelmet.meta(~property="og:title", ~content=title, ()),
    ReactHelmet.meta(~property="og:description", ~content=description, ()),
    ReactHelmet.meta(~property="og:type", ~content="website", ()),
    ReactHelmet.meta(~name="twitter:card", ~content="summary", ()),
    ReactHelmet.meta(
      ~name="twitter:creator",
      ~content=site.siteMetadata.author,
      (),
    ),
    ReactHelmet.meta(~name="twitter:title", ~content=title, ()),
    ReactHelmet.meta(
      ~name="twitter:description",
      ~content=metaDescription,
      (),
    ),
  ];

  <ReactHelmet.Helmet
    htmlAttributes={lang: lang}
    title
    titleTemplate={j|%s | $site.siteMetadata.title|j}
    meta={List.concat([baseMeta, meta])}
  />;
};