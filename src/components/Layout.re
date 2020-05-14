type colorMode =
  | Light
  | Dark;

let styles = (theme: Mui.Theme.t) => {
  open MakeStyles;

  let appContainer = props =>
    style([
      backgroundColor(
        switch (props) {
        | Dark => theme.background.dark
        | Light => theme.background.light
        },
      ),
      width(vw(100)),
      height(vh(100)),
    ]);

  create([("appContainer", appContainer)]);
};

let useStyles = MakeStyles.makeThemeStyles(styles);

[@react.component]
let make = (~children, ~colorMode=Dark) => {
  let classes = useStyles(~props=colorMode);

  <div className={classes("appContainer")}> <main> children </main> </div>;
};