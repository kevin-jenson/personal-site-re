// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as Mui from "./Mui.js";
import * as React from "react";
import * as MakeStyles from "./MakeStyles.js";
import * as Caml_option from "bs-platform/lib/es6/caml_option.js";
import * as Core from "@material-ui/core";

var body = MakeStyles.css(/* :: */[
      MakeStyles.overflow(MakeStyles.hidden),
      /* [] */0
    ]);

var styles = MakeStyles.create(/* :: */[
      /* tuple */[
        "body",
        body
      ],
      /* [] */0
    ]);

var useThemeProviderStyles = MakeStyles.makeGlobalStyles(styles);

function ThemeProvider$ThemeProvider(Props) {
  var children = Props.children;
  useThemeProviderStyles(Caml_option.some(undefined));
  return React.createElement(Core.CssBaseline, {
              children: React.createElement(Core.ThemeProvider, {
                    children: children,
                    theme: Mui.Theme.theme
                  })
            });
}

var ThemeProvider = {
  make: ThemeProvider$ThemeProvider
};

export {
  styles ,
  useThemeProviderStyles ,
  ThemeProvider ,
  
}
/* body Not a pure module */
