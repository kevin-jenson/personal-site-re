let themeProviderGlobalStyles = {
  "@global": {
    "body": {
      "overflow": "hidden",
    },
  },
};

let useThemeProviderStyles = MakeStyles.makeStyles(themeProviderGlobalStyles);

module ThemeProvider = {
  [@react.component]
  let make = (~children) => {
    let _classes = useThemeProviderStyles();

    <Mui.CssBaseline>
      <Mui.MuiThemeProvider theme=Mui.Theme.theme> children </Mui.MuiThemeProvider>
    </Mui.CssBaseline>;
  };
};