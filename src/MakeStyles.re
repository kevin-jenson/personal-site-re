type styleProps;
type styleDict = Js.Dict.t(Js.Dict.t(string));
type useStyles = styleProps => Js.Dict.t(string);

[@bs.module "@material-ui/styles"] external muiMakeThemeStyles: (Mui.Theme.t => styleDict) => useStyles = "makeStyles";

[@bs.module "@material-ui/styles"] external muiMakeStyles: styleDict => useStyles = "makeStyles";

let clsx = classNames => List.fold_left((classes, cls) => {j|$classes $cls|j}, "", classNames);

// shortened coercion functions
let toFloat = float_of_int;
let toInt = int_of_float;
let toStr = string_of_int;

// css measurement values
let vw = int => toStr(int) ++ "vw";
let vh = int => toStr(int) ++ "vh";
let px = int => toStr(int) ++ "px";
let pct = int => toStr(int) ++ "%";
let deg = int => toStr(int) ++ "deg";
let ms = int => toStr(int) ++ "ms";

// css properties
let backgroundColor = str => ("backgroundColor", str);
let width = str => ("width", str);
let height = str => ("height", str);
let overflow = str => ("overflow", str);
let display = str => ("display", str);
let flexDirection = str => ("flex-direction", str);
let alignItems = str => ("align-items", str);
let padding = pad => {
  let getPaddingStr = (str, p) => {
    str ++ ", " ++ p;
  };

  let paddingStr = getPaddingStr->List.fold_left("", pad);
  ("padding", paddingStr);
};
let transition = str => ("transition", str);
let transform = str => ("transfrom", str);
let zIndex = index => ("z-index", index |> toStr);
let margin = marg => {
  let getMarginStr = (str, m) => {
    str ++ ", " ++ m;
  };

  let marginStr = getMarginStr->List.fold_left("", marg);
  ("margin", marginStr);
};
let animationDuration = str => ("animation-duration", str);
let animationFillMode = str => ("animation-fill-mode", str);
let opacity = num => ("opacity", num |> Js.Float.toString);
let animationName = str => ("animation-name", "$" ++ str);
let textAlign = str => ("text-align", str);
let lineHeight = str => ("line-height", str);
let fontSize = str => ("font-size", str);
let textDecoration = str => ("text-decoration", str);
let color = str => ("color", str);

// sudo selectors
let nthChild = (child, dict) => {
  let child = string_of_int(child);
  ({j|&:nth-child($child)|j}, dict);
};
let var = (str, dict) => ("&$" ++ str, dict);
let hover = dict => ("&:hover", dict);

// css values
let hidden = "hidden";
let flex = "flex";
let column = "column";
let flexEnd = "flexEnd";
let rotate = str => {j|rotate($str)|j};
let translateY = str => {j|translateY($str)|j};
let translate = (x, y) => {j|translate($x, $y)|j};
let important = str => str ++ " !important";
let forwards = "forwards";
let center = "center";
let none = "none";
let underline = "underline";

// keyframes
let keyframes = (name, frames) => {
	let formatFrames = (dict, (percent, styles)) => {
		Js.Dict.set(dict, pct(percent), styles);
		dict;
	}

	let keyframe = formatFrames -> List.fold_left(Js.Dict.empty(), frames);
	({j|@keyframes $name|j}, keyframe) |> Obj.magic;
};

let style = styles => Js.Dict.fromList(styles) |> Obj.magic;

let create = styles => Js.Dict.fromList(styles) |> Obj.magic;

let getClassName = (classes, key) => {
  switch (Js.Dict.get(classes, key)) {
  | Some(className) => className
  | None => ""
  };
};

let useStyles = (~muiUseStyles: useStyles, ~props=?) => {
  let classes =
    switch (props) {
    | Some(props) => props |> Obj.magic |> muiUseStyles
    | None => muiUseStyles(Obj.magic(""))
    };

  getClassName(classes);
};

let makeGlobalStyles = styles => {
  let styleDict = Js.Dict.empty();
  Js.Dict.set(styleDict, "@global", Obj.magic(styles));

  let muiUseStyles = styleDict |> Obj.magic |> muiMakeStyles;
  useStyles(~muiUseStyles);
};

let makeStyles = styles => {
  let muiUseStyles = styles |> Obj.magic |> muiMakeStyles;
  useStyles(~muiUseStyles);
};

let makeThemeStyles = styleFunc => {
  let muiUseStyles = styleFunc |> Obj.magic |> muiMakeThemeStyles;
  useStyles(~muiUseStyles);
};
