type useStyles = unit => Js.t({.});

[@bs.module "@material-ui/styles"]
external makeThemeStyles: (Mui.Theme.t => Js.t({..})) => useStyles =
  "makeStyles";

[@bs.module "@material-ui/styles"]
external makeStyles: Js.t({..}) => useStyles = "makeStyles";