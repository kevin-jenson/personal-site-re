
let styles = {
	open MakeStyles;
	let body = css([overflow(hidden)]);

	create([
		("body", body)
	])
};

let useThemeProviderStyles = MakeStyles.makeGlobalStyles(styles);

module ThemeProvider = {
  [@react.component]
  let make = (~children) => {
    let _getClass = useThemeProviderStyles(~props=());

    <Mui.CssBaseline>
      <Mui.MuiThemeProvider theme=Mui.Theme.theme> children </Mui.MuiThemeProvider>
    </Mui.CssBaseline>;
  };
};