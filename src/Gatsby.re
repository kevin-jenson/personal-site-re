module Gatsby = {
  type siteMetadata = {
    description: string,
    title: string,
    author: string,
  };

  type site = {siteMetadata};

  type staticQuery = {site};

  [@bs.module "gatsby"]
  external useStaticQuery: string => staticQuery = "useStaticQuery";

  [@bs.module "gatsby"] external graphql: string => string = "graphql";
};

module ReactHelmet = {
  type htmlAttributes = {lang: string};

  [@bs.deriving abstract]
  type meta = {
    [@bs.optional] name: string,
    [@bs.optional] content: string,
    [@bs.optional] property: string,
  };

  module Helmet = {
    [@bs.module "react-helmet"] [@react.component]
    external make:
      (
        ~htmlAttributes: htmlAttributes,
        ~title: string,
        ~titleTemplate: string,
        ~meta: list(meta)
      ) =>
      React.element =
      "Helmet";
  };
};