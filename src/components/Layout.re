let styles = (theme: Mui.Theme.t) => {
	"appContainer": {
		"backgroundColor": theme.background.dark,
		"width": "100vw",
		"height": "100vh"
	}
};

let useStyles = Mui.Styles.makeThemeStyles(styles);

[@react.component]
let make = (~children, ~colorMode="dark") => {
	let classes = useStyles();
	
	<div className=classes##appContainer>
		<main>children</main>
	</div>
};
