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
let underline = "underline";

// keyframes
let keyframes = (name, frames) => {
  let formatFrames = (dict, (percent, styles)) => {
    Js.Dict.set(dict, pct(percent), styles);
    dict;
  };

  let keyframe = formatFrames->List.fold_left(Js.Dict.empty(), frames);
  ({j|@keyframes $name|j}, keyframe) |> Obj.magic;
};

let css = styles => Js.Dict.fromList(styles) |> Obj.magic;

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

// constants
let auto = "auto";
let initial = "initial";
let inherit_ = "inherit";
let none = "none";
let unset = "unset";

module Length = {
  type options =
    | Cm(int)
    | Mm(int)
    | In(int)
    | Px(int)
    | Pt(int)
    | Pc(int)
    | Em(int)
    | Ex(int)
    | Ch(int)
    | Rem(int)
    | Vw(int)
    | Vh(int)
    | Vmin(int)
    | Vmax(int)
    | Pct(int);

  let getLength = length =>
    switch (length) {
    | Cm(length) => toStr(length) ++ "cm"
    | Mm(length) => toStr(length) ++ "mm"
    | In(length) => toStr(length) ++ "in"
    | Px(length) => toStr(length) ++ "px"
    | Pt(length) => toStr(length) ++ "pt"
    | Pc(length) => toStr(length) ++ "pc"
    | Em(length) => toStr(length) ++ "em"
    | Ex(length) => toStr(length) ++ "ex"
    | Ch(length) => toStr(length) ++ "ch"
    | Rem(length) => toStr(length) ++ "rem"
    | Vw(length) => toStr(length) ++ "vw"
    | Vh(length) => toStr(length) ++ "vh"
    | Vmin(length) => toStr(length) ++ "vmin"
    | Vmax(length) => toStr(length) ++ "vmax"
    | Pct(length) => toStr(length) ++ "%"
    };
};

module TimingFunctions = {
  type step =
    | Start
    | End
    | Unsafe_set(string)
  and timingOptions =
    | Linear
    | Ease
    | EaseIn
    | EaseOut
    | EaseInOut
    | StepStart
    | StepEnd
    | Step(int)
    | Steps(int, step)
    | CubicBezier(float, float, float, float)
    | Initial
    | Inherit
    | Unsafe_set(string);

  let timing = opt =>
    switch (opt) {
    | Linear => "linear"
    | Ease => "ease"
    | EaseIn => "ease-in"
    | EaseOut => "ease-out"
    | EaseInOut => "ease-in-out"
    | StepStart => "step-start"
    | StepEnd => "step-end"
    | Step(times) => {j|steps($times)|j}
    | Steps(times, step) =>
      let stepValue =
        switch (step) {
        | Start => "start"
        | End => "end"
        | Unsafe_set(str) => str
        };

      {j|steps($times, $stepValue)|j};
    | CubicBezier(x1, y1, x2, y2) => {j|cubic-bezier($x1, $y1, $x2, $y2)|j}
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };
};

// A's
module Align = {
  type alignContentOptions =
    | Stretch
    | Center
    | FlexStart
    | FlexEnd
    | SpaceBetween
    | SpaceAround
    | Initial
    | Inherit
    | Unsafe_set(string);

  let alignContent = opt => (
    "align-content",
    switch (opt) {
    | Stretch => "stretch"
    | Center => "center"
    | FlexStart => "flex-start"
    | FlexEnd => "flex-end"
    | SpaceBetween => "space-between"
    | SpaceAround => "space-around"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type alignItemsOptions =
    | Stretch
    | Center
    | FlexStart
    | FlexEnd
    | Baseline
    | Initial
    | Inherit
    | Unsafe_set(string);

  let alignItems = opt => (
    "align-items",
    switch (opt) {
    | Stretch => "stretch"
    | Center => "center"
    | FlexStart => "flex-start"
    | FlexEnd => "flex-end"
    | Baseline => "baseline"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type alignSelfOptions =
    | Auto
    | Stretch
    | Center
    | FlexStart
    | FlexEnd
    | Baseline
    | Initial
    | Inherit
    | Unsafe_set(string);

  let alignSelf = opt => (
    "align-self",
    switch (opt) {
    | Auto => "auto"
    | Stretch => "stretch"
    | Center => "center"
    | FlexStart => "flex-start"
    | FlexEnd => "flex-end"
    | Baseline => "baseline"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

module All = {
  type allOptions =
    | Initial
    | Inherit
    | Unset
    | Unsafe_set(string);

  let all = opt => (
    "all",
    switch (opt) {
    | Initial => initial
    | Inherit => inherit_
    | Unset => unset
    | Unsafe_set(str) => str
    },
  );
};

module Animation = {
  type animationDelayOptions =
    | Time(string)
    | Initial
    | Inherit
    | Unsafe_set(string);

  exception Not_valid(string);
  let _animationTime = opt =>
    switch (opt) {
    | Time(str) =>
      if (Js.String.includes("ms", str) || Js.String.includes("s", str)) {
        str;
      } else {
        raise(Not_valid("Time(string) needs to be in seconds or miliseconds"));
      }
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  let animationDelay = opt => ("animation-delay", _animationTime(opt));

  type animationDirectionOptions =
    | Normal
    | Reverse
    | Alternate
    | AlternateReverse
    | Initial
    | Inherit
    | Unsafe_set(string);

  let animationDirection = opt => (
    "animation-direction",
    switch (opt) {
    | Normal => "normal"
    | Reverse => "reverse"
    | Alternate => "alternate"
    | AlternateReverse => "alternate-reverse"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type animationDurationOptions = animationDelayOptions;

  let animationDuration = opt => ("animation-duration", _animationTime(opt));

  type animationFillModeOptions =
    | None
    | Forwards
    | Backwards
    | Both
    | Initial
    | Inherit
    | Unsafe_set(string);

  let animationFillMode = opt => (
    "animation-fill-mode",
    switch (opt) {
    | None => "none"
    | Forwards => "forwards"
    | Backwards => "backwards"
    | Both => "both"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type animationIterationCountOptions =
    | Number(int)
    | Infinite
    | Initial
    | Inherit
    | Unsafe_set(string);

  let animationIterationCount = opt => (
    "animation-iteration-count",
    switch (opt) {
    | Number(times) => toStr(times)
    | Infinite => "infinite"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type animationNameOptions =
    | Name(string)
    | Initial
    | Inherit
    | Unsafe_set(string);

  let animationName = opt => (
    "animation-name",
    switch (opt) {
    | Name(name) => name
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type animationPlayStateOptions =
    | Paused
    | Running
    | Initial
    | Inherit
    | Unsafe_set(string);

  let animationPlayState = opt => (
    "animation-play-state",
    switch (opt) {
    | Paused => "paused"
    | Running => "running"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  let animationTimingFunction = opt => ("animation-timing-function", TimingFunctions.timing(opt));

  type animationOptions =
    | Animation1(animationNameOptions)
    | Animation2(animationNameOptions, animationDurationOptions)
    | Animation3(animationNameOptions, animationDurationOptions, TimingFunctions.timingOptions)
    | Animation4(animationNameOptions, animationDurationOptions, TimingFunctions.timingOptions, animationDelayOptions)
    | Animation5(
        animationNameOptions,
        animationDurationOptions,
        TimingFunctions.timingOptions,
        animationDelayOptions,
        animationIterationCountOptions,
      )
    | Animation6(
        animationNameOptions,
        animationDurationOptions,
        TimingFunctions.timingOptions,
        animationDelayOptions,
        animationIterationCountOptions,
        animationDirectionOptions,
      )
    | Animation7(
        animationNameOptions,
        animationDurationOptions,
        TimingFunctions.timingOptions,
        animationDelayOptions,
        animationIterationCountOptions,
        animationDirectionOptions,
        animationFillModeOptions,
      )
    | Animation8(
        animationNameOptions,
        animationDurationOptions,
        TimingFunctions.timingOptions,
        animationDelayOptions,
        animationIterationCountOptions,
        animationDirectionOptions,
        animationFillModeOptions,
        animationPlayStateOptions,
      )
    | Unsafe_set(string);

  let animation = opt => (
    "animation",
    switch (opt) {
    | Animation1(name) =>
      let (_, value) = animationName(name);
      value;
    | Animation2(name, duration) =>
      let (_, nameValue) = animationName(name);
      let (_, durationValue) = animationDuration(duration);
      {j|$nameValue $durationValue|j};
    | Animation3(name, duration, time) =>
      let (_, nameValue) = animationName(name);
      let (_, durationValue) = animationDuration(duration);
      let timingValue = TimingFunctions.timing(time);
      {j|$nameValue $durationValue $timingValue|j};
    | Animation4(name, duration, time, delay) =>
      let (_, nameValue) = animationName(name);
      let (_, durationValue) = animationDuration(duration);
      let timingValue = TimingFunctions.timing(time);
      let (_, delayValue) = animationDelay(delay);
      {j|$nameValue $durationValue $timingValue $delayValue|j};
    | Animation5(name, duration, time, delay, interationCount) =>
      let (_, nameValue) = animationName(name);
      let (_, durationValue) = animationDuration(duration);
      let timingValue = TimingFunctions.timing(time);
      let (_, delayValue) = animationDelay(delay);
      let (_, iterationCountValue) = animationIterationCount(interationCount);
      {j|$nameValue $durationValue $timingValue $delayValue $iterationCountValue|j};
    | Animation6(name, duration, time, delay, interationCount, direction) =>
      let (_, nameValue) = animationName(name);
      let (_, durationValue) = animationDuration(duration);
      let timingValue = TimingFunctions.timing(time);
      let (_, delayValue) = animationDelay(delay);
      let (_, directionValue) = animationDirection(direction);
      let (_, iterationCountValue) = animationIterationCount(interationCount);
      {j|$nameValue $durationValue $timingValue $delayValue $iterationCountValue $directionValue|j};
    | Animation7(name, duration, time, delay, interationCount, direction, fillMode) =>
      let (_, nameValue) = animationName(name);
      let (_, durationValue) = animationDuration(duration);
      let timingValue = TimingFunctions.timing(time);
      let (_, delayValue) = animationDelay(delay);
      let (_, iterationCountValue) = animationIterationCount(interationCount);
      let (_, directionValue) = animationDirection(direction);
      let (_, fillModeValue) = animationFillMode(fillMode);
      {j|$nameValue $durationValue $timingValue $delayValue $iterationCountValue $directionValue $fillModeValue|j};
    | Animation8(name, duration, time, delay, interationCount, direction, fillMode, playState) =>
      let (_, nameValue) = animationName(name);
      let (_, durationValue) = animationDuration(duration);
      let timingValue = TimingFunctions.timing(time);
      let (_, delayValue) = animationDelay(delay);
      let (_, iterationCountValue) = animationIterationCount(interationCount);
      let (_, directionValue) = animationDirection(direction);
      let (_, fillModeValue) = animationFillMode(fillMode);
      let (_, playStateValue) = animationPlayState(playState);
      {j|$nameValue $durationValue $timingValue $delayValue $iterationCountValue $directionValue $fillModeValue $playStateValue|j};
    | Unsafe_set(str) => str
    },
  );
};

type alignContentOptions = Align.alignContentOptions;
let alignContent = Align.alignContent;
type alignItemsOptions = Align.alignItemsOptions;
let alignItems = Align.alignItems;
type alignSelfOptions = Align.alignSelfOptions;
let alignSelf = Align.alignSelf;

type allOptions = All.allOptions;
let all = All.all;

type animationOptions = Animation.animationOptions;
let animation = Animation.animation;
type animationDelayOptions = Animation.animationDelayOptions;
let animationDelay = Animation.animationDelay;
type animationDirectionOptions = Animation.animationDirectionOptions;
let animationDirection = Animation.animationDirection;
type animationDurationOptions = Animation.animationDurationOptions;
let animationDuration = Animation.animationDuration;
type animationFillModeOptions = Animation.animationFillModeOptions;
let animationFillMode = Animation.animationFillMode;
type animationIterationCountOptions = Animation.animationIterationCountOptions;
let animationIterationCount = Animation.animationIterationCount;
type animationNameOptions = Animation.animationNameOptions;
let animationName = Animation.animationName;
type animationPlayStateOptions = Animation.animationPlayStateOptions;
let animationPlayState = Animation.animationPlayState;
type animationTimingFunctionOptions = TimingFunctions.timingOptions;
let animationTimingFunction = Animation.animationTimingFunction;

// B's
module Backface = {
  type backfaceVisibiltyOptions =
    | Visible
    | Hidden
    | Initial
    | Inherit
    | Unsafe_set(string);

  let backfaceVisibilty = opt => (
    "backface-visibility",
    switch (opt) {
    | Visible => "visible"
    | Hidden => "hidden"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type backfaceVisibiltyOptions = Backface.backfaceVisibiltyOptions;
let backfaceVisibilty = Backface.backfaceVisibilty;

module Background = {
  type backgroundAttachmentOptions =
    | Scroll
    | Fixed
    | Local
    | Initial
    | Inherit
    | Unsafe_set(string);
  let backgroundAttachment = opt => (
    "background-attachment",
    switch (opt) {
    | Scroll => "scroll"
    | Fixed => "fixed"
    | Local => "local"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type backgroundBlendModeOptions =
    | Normal
    | Multiply
    | Screen
    | Overlay
    | Darken
    | Lighten
    | ColorDodge
    | Saturation
    | Color
    | Luminosity
    | Unsafe_set(string);
  let backgroundBlendMode = opt => (
    "background-blend-mode",
    switch (opt) {
    | Normal => "normal"
    | Multiply => "multiply"
    | Screen => "screen"
    | Overlay => "overlay"
    | Darken => "darken"
    | Lighten => "lighten"
    | ColorDodge => "color-dodge"
    | Saturation => "saturation"
    | Color => "color"
    | Luminosity => "luminosity"
    | Unsafe_set(str) => str
    },
  );

  type backgroundClipOptions =
    | BorderBox
    | PaddingBox
    | ContentBox
    | Initial
    | Inherit
    | Unsafe_set(string);

  let _backgroundBoxing = opt =>
    switch (opt) {
    | BorderBox => "border-box"
    | PaddingBox => "padding-box"
    | ContentBox => "content-box"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };
  let backgroundClip = opt => ("background-clip", _backgroundBoxing(opt));

  type backgroundColorOptions =
    | Color(string)
    | Transparent
    | Initial
    | Inherit
    | Unsafe_set(string);
  let backgroundColor = opt => (
    "background-color",
    switch (opt) {
    | Color(color) => color
    | Transparent => "transparent"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type backgroundImageOptions =
    | URL(string)
    | None
    | LinearGradient(string)
    | RadialGradient(string)
    | RepeatingLinearGradient(string)
    | RepeatingRadialGradient(string)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let backgroundImage = opt => (
    "background-image",
    switch (opt) {
    | URL(url) => {j|url($url)|j}
    | None => none
    | LinearGradient(gradient) => {j|linear-gradient($gradient)|j}
    | RadialGradient(gradient) => {j|radial-gradient($gradient)|j}
    | RepeatingLinearGradient(gradient) => {j|repeating-linear-gradient($gradient)|j}
    | RepeatingRadialGradient(gradient) => {j|repeating-radial-gradient($gradient)|j}
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type backgroundOriginOptions = backgroundClipOptions;
  let backgroundOrigin = opt => ("background-origin", _backgroundBoxing(opt));

  type backgroundPositionPositionOption =
    | Bottom
    | Center
    | Left
    | Right
    | Top
  and backgroundPositionOptions =
    | Position(backgroundPositionPositionOption, backgroundPositionPositionOption)
    | PositionExact(Length.options, Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);

  let backgroundPosition = opt => (
    "background-position",
    switch (opt) {
    | Position(pos1, pos2) =>
      switch (pos1, pos2) {
      | (Left, Top) => "left top"
      | (Left, Center) => "left enter"
      | (Left, Bottom) => "left bottom"
      | (Right, Top) => "right top"
      | (Right, Center) => "right center"
      | (Right, Bottom) => "right bottom"
      | (Center, Top) => "center top"
      | (Center, Center) => "center center"
      | (Center, Bottom) => "center bottom"
      | _ => "center center"
      }
    | PositionExact(x, y) =>
      let x = Length.getLength(x);
      let y = Length.getLength(y);
      {j|$x $y|j};
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type backgroundRepeatOptions =
    | Repeat
    | RepeatX
    | RepeatY
    | NoRepeat
    | Space
    | Round
    | Initial
    | Inherit
    | Unsafe_set(string);
  let backgroundRepeat = opt => (
    "background-repeat",
    switch (opt) {
    | Repeat => "repeat"
    | RepeatX => "repeat-x"
    | RepeatY => "repeat-y"
    | NoRepeat => "no-repeat"
    | Space => "space"
    | Round => "round"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type backgroundSizeOptions =
    | Auto
    | Length(Length.options, Length.options)
    | LengthX(Length.options)
    | LengthY(Length.options)
    | Cover
    | Contain
    | Initial
    | Inherit
    | Unsafe_set(string);
  let backgroundSize = opt => (
    "background-size",
    switch (opt) {
    | Auto => auto
    | Length(x, y) =>
      let x = Length.getLength(x);
      let y = Length.getLength(y);
      {j|$x $y|j};
    | LengthX(x) =>
      let x = Length.getLength(x);
      {j|$x|j};
    | LengthY(y) =>
      let y = Length.getLength(y);
      {j|auto $y|j};
    | Cover => "cover"
    | Contain => "contain"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  // TODO: add better types for background
  type backgroundOptions =
    | Whatever(string);
  let background = opt => (
    "background",
    switch (opt) {
    | Whatever(str) => str
    },
  );
};

// TODO: add background
type backgroundOptions = Background.backgroundOptions;
let background = Background.background;
type backgroundAttachmentOptions = Background.backgroundAttachmentOptions;
let backgroundAttachment = Background.backgroundAttachment;
type backgroundBlendModeOptions = Background.backgroundBlendModeOptions;
let backgroundBlendMode = Background.backgroundBlendMode;
type backgroundClipOptions = Background.backgroundClipOptions;
let backgroundClip = Background.backgroundClip;
type backgroundColorOptions = Background.backgroundColorOptions;
let backgroundColor = Background.backgroundColor;
type backgroundImageOptions = Background.backgroundImageOptions;
let backgroundImage = Background.backgroundImage;
type backgroundOriginOptions = Background.backgroundOriginOptions;
let backgroundOrigin = Background.backgroundOrigin;
type backgroundPositionOptions = Background.backgroundPositionOptions;
let backgroundPosition = Background.backgroundPosition;
type backgroundRepeatOptions = Background.backgroundRepeatOptions;
let backgroundRepeat = Background.backgroundRepeat;
type backgroundSizeOptions = Background.backgroundSizeOptions;
let backgroundSize = Background.backgroundSize;

module Border = {
  type borderWidthOptions =
    | Medium
    | Thin
    | Thick
    | Length(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let getBorderWidth = opt =>
    switch (opt) {
    | Medium => "medium"
    | Thin => "thin"
    | Thick => "thick"
    | Length(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  type borderStyleOptions =
    | None
    | Hidden
    | Dotted
    | Dashed
    | Solid
    | Double
    | Groove
    | Ridge
    | Inset
    | Outset
    | Initial
    | Inherit
    | Unsafe_set(string);
  let getBorderStyle = opt =>
    switch (opt) {
    | None => none
    | Hidden => "hidden"
    | Dotted => "dotted"
    | Dashed => "dashed"
    | Solid => "solid"
    | Double => "double"
    | Groove => "groove"
    | Ridge => "ridge"
    | Inset => "inset"
    | Outset => "outset"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  type borderColorOptions =
    | Color(string)
    | Transparent
    | Initial
    | Inherit
    | Unsafe_set(string);
  let getBorderColor = opt =>
    switch (opt) {
    | Color(color) => color
    | Transparent => "transparent"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  let getBorder =
      (
        ~width: borderWidthOptions=Unsafe_set(""),
        ~style: borderStyleOptions=Unsafe_set(""),
        ~color: borderColorOptions=Unsafe_set(""),
        (),
      ) => {
    let width = getBorderWidth(width);
    let style = getBorderStyle(style);
    let color = getBorderColor(color);
    {j|$width $style $color|j} |> String.trim;
  };

  let borderBottomWidth = opt => ("border-bottom-width", getBorderWidth(opt));
  let borderBottomeStyle = opt => ("border-bottom-style", getBorderStyle(opt));
  let borderBottomColor = opt => ("border-bottom-color", getBorderColor(opt));

  let borderBottom = (~width=?, ~style=?, ~color=?, ()) => (
    "border-bottom",
    getBorder(~width?, ~style?, ~color?, ()),
  );

  type borderRadiusOptions =
    | Length(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let getBorderRadius = radius =>
    switch (radius) {
    | Length(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  let borderBottomLeftRadius = opt => ("border-bottom-left-radius", getBorderRadius(opt));
  let borderBottomRightRadius = opt => ("border-bottom-right-radius", getBorderRadius(opt));

  type borderCollapseOptions =
    | Separate
    | Collapse
    | Initial
    | Inherit
    | Unsafe_set(string);
  let borderCollapse = opt => (
    "border-collapse",
    switch (opt) {
    | Separate => "separate"
    | Collapse => "collapse"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  let borderColor = opt => ("border-color", getBorderColor(opt));

  type borderImageSourceOptions =
    | None
    | Image(string)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let borderImageSource = opt => (
    "border-image-source",
    switch (opt) {
    | None => "none"
    | Image(image) => image
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type borderImageSliceOptions =
    | Number(int, int)
    | Pct(int, int)
    | Fill
    | Initial
    | Inherit
    | Unsafe_set(string);
  let borderImageSlice = opt => (
    "border-image-slice",
    switch (opt) {
    | Number(x, y) => {j|$x $y|j}
    | Pct(x, y) => {j|$x% $y%|j}
    | Fill => "fill"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type borderImageWidthOptions =
    | Length(Length.options)
    | Number(int)
    | Pct(int)
    | Auto
    | Initial
    | Inherit
    | Unsafe_set(string);
  let borderImageWidth = opt => (
    "border-image-width",
    switch (opt) {
    | Length(length) => Length.getLength(length)
    | Number(number) => toStr(number)
    | Pct(number) => toStr(number) ++ "%"
    | Auto => auto
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type borderImageOutsetOptions =
    | Length(Length.options)
    | Number(int)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let borderImageOutset = opt => (
    "border-image-outset",
    switch (opt) {
    | Length(length) => Length.getLength(length)
    | Number(number) => toStr(number)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type borderImageRepeatOptions =
    | Stretch
    | Repeat
    | Round
    | Space
    | Initial
    | Inherit
    | Unsafe_set(string);
  let borderImageRepeat = opt => (
    "border-image-repeat",
    switch (opt) {
    | Stretch => "stretch"
    | Repeat => "repeat"
    | Round => "round"
    | Space => "space"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  let borderImage =
      (
        ~source: borderImageSourceOptions=Unsafe_set(""),
        ~slice: borderImageSliceOptions=Unsafe_set(""),
        ~width: borderImageWidthOptions=Unsafe_set(""),
        ~outset: borderImageOutsetOptions=Unsafe_set(""),
        ~repeat: borderImageRepeatOptions=Unsafe_set(""),
        (),
      ) => {
    let (_, source) = borderImageSource(source);
    let (_, slice) = borderImageSlice(slice);
    let (_, width) = borderImageWidth(width);
    let (_, outset) = borderImageOutset(outset);
    let (_, repeat) = borderImageRepeat(repeat);
    ("border-image", {j|$source $slice $width $outset $repeat|j} |> String.trim);
  };

  let borderLeft = (~width=?, ~style=?, ~color=?, ()) => ("border-left", getBorder(~width?, ~style?, ~color?, ()));
  let borderLeftColor = opt => ("border-left-color", getBorderColor(opt));
  let borderLeftStyle = opt => ("border-left-style", getBorderStyle(opt));
  let borderLeftWidth = opt => ("border-left-width", getBorderWidth(opt));

  let borderRadius = optList => {
    let radius = optList |> List.map(getBorderRadius) |> Array.of_list |> Js.Array.joinWith(" ");
    ("border-radius", radius);
  };

  let borderRight = (~width=?, ~style=?, ~color=?, ()) => (
    "border-right",
    getBorder(~width?, ~style?, ~color?, ()),
  );
  let borderRightColor = opt => ("border-right-color", getBorderColor(opt));
  let borderRightStyle = opt => ("border-right-style", getBorderStyle(opt));
  let borderRightWidth = opt => ("border-right-width", getBorderWidth(opt));

  type borderSpacingOptions =
    | Length(list(Length.options))
    | Initial
    | Inherit
    | Unsafe_set(string);
  let borderSpacing = opt => (
    "border-spacing",
    switch (opt) {
    | Length(values) => values |> List.map(Length.getLength) |> Array.of_list |> Js.Array.joinWith(" ")
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  let borderStyle = opt => ("border-style", getBorderStyle(opt));

  let borderTop = (~width=?, ~style=?, ~color=?, ()) => ("border-top", getBorder(~width?, ~style?, ~color?, ()));
  let borderTopColor = opt => ("border-top-color", getBorderColor(opt));
  let borderTopLeftRadius = opt => ("border-top-left-radius", getBorderRadius(opt));
  let borderTopRightRadius = opt => ("border-top-right-radius", getBorderRadius(opt));
  let borderTopStyle = opt => ("border-top-style", getBorderStyle(opt));
  let borderTopWidth = opt => ("border-top-width", getBorderWidth(opt));

  let borderWidth = opt => ("border-width", getBorderWidth(opt));
  let border = (~width=?, ~style=?, ~color=?, ()) => ("border", getBorder(~width?, ~style?, ~color?, ()));
};

let border = Border.border;
type borderColorOptions = Border.borderColorOptions;
type borderStyleOptions = Border.borderStyleOptions;
type borderWidthOptions = Border.borderWidthOptions;
let borderBottom = Border.borderBottom;
let borderBottomColor = Border.borderBottomColor;
type borderRadiusOptions = Border.borderRadiusOptions;
let borderBottomLeftRadius = Border.borderBottomLeftRadius;
let borderBottomRightRadius = Border.borderBottomRightRadius;
let borderBottomStyle = Border.borderBottomeStyle;
let borderBottomWidth = Border.borderBottomWidth;
type borderCollapseOptions = Border.borderCollapseOptions;
let borderCollapse = Border.borderCollapse;
let borderColor = Border.borderColor;
let borderImage = Border.borderImage;
type borderImageOutsetOptions = Border.borderImageOutsetOptions;
let borderImageOutset = Border.borderImageOutset;
type borderImageRepeatOptions = Border.borderImageRepeatOptions;
let borderImageRepeat = Border.borderImageRepeat;
type borderImageSliceOptions = Border.borderImageSliceOptions;
let borderImageSlice = Border.borderImageSlice;
type borderImageSourceOptions = Border.borderImageSourceOptions;
let borderImageSource = Border.borderImageSource;
type borderImageWidthOptions = Border.borderImageWidthOptions;
let borderImageWidth = Border.borderImageWidth;
let borderLeft = Border.borderLeft;
let borderLeftColor = Border.borderLeftColor;
let borderLeftStyle = Border.borderLeftStyle;
let borderLeftWidth = Border.borderLeftWidth;
let borderRadius = Border.borderRadius;
let borderRight = Border.borderRight;
let borderRightColor = Border.borderRightColor;
let borderRightStyle = Border.borderRightStyle;
let borderRightWidth = Border.borderRightWidth;
let borderSpacing = Border.borderSpacing;
let borderStyle = Border.borderStyle;
let borderTop = Border.borderTop;
let borderTopColor = Border.borderTopColor;
let borderTopLeftRadius = Border.borderTopLeftRadius;
let borderTopRightRadius = Border.borderTopRightRadius;
let borderTopStyle = Border.borderTopStyle;
let borderTopWidth = Border.borderTopWidth;
let borderWidth = Border.borderWidth;

module Bottom = {
  type options =
    | Auto
    | Length(Length.options)
    | Pct(int)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let bottom = opt => (
    "bottom",
    switch (opt) {
    | Auto => auto
    | Length(length) => Length.getLength(length)
    | Pct(num) => toStr(num) ++ "%"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type bottomOptions = Bottom.options;
let bottom = Bottom.bottom;

module Box = {
  type boxDecorationBreakOptions =
    | Slice
    | Clone
    | Initial
    | Inherit
    | Unsafe_set(string);
  let boxDecorationBreak = opt => (
    "box-decoration-break",
    switch (opt) {
    | Slice => "slice"
    | Clone => "clone"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  let boxShadow = shadowList => ("box-shadow", shadowList |> Array.of_list |> Js.Array.joinWith(" "));

  type boxSizingOptions =
    | ContentBox
    | BorderBox
    | Initial
    | Inherit
    | Unsafe_set(string);
  let boxSizing = opt => (
    "box-sizing",
    switch (opt) {
    | ContentBox => "content-box"
    | BorderBox => "border-box"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type boxDecorationBreakOptions = Box.boxDecorationBreakOptions;
let boxDecorationBreak = Box.boxDecorationBreak;
let boxShadow = Box.boxShadow;
type boxSizingOptions = Box.boxSizingOptions;
let boxSizing = Box.boxSizing;

module Break = {
  type breakOptions =
    | Auto
    | All
    | Always
    | Avoid
    | AvoidColumn
    | AvoidPage
    | AvoidRegion
    | Column
    | Left
    | Page
    | Recto
    | Region
    | Right
    | Verso
    | Initial
    | Inherit
    | Unsafe_set(string);
  let getBreak = opt =>
    switch (opt) {
    | Auto => auto
    | All => "all"
    | Always => "always"
    | Avoid => "avoid"
    | AvoidColumn => "avoid-column"
    | AvoidPage => "avoid-page"
    | AvoidRegion => "avoid-region"
    | Column => "column"
    | Left => "left"
    | Page => "page"
    | Recto => "recto"
    | Region => "region"
    | Right => "right"
    | Verso => "verso"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  let breakAfter = opt => ("break-after", getBreak(opt));
  let breakBefore = opt => ("break-before", getBreak(opt));

  type breakInsideOptions =
    | Auto
    | Avoid
    | AvoidColumn
    | AvoidPage
    | AvoidRegion
    | Initial
    | Inherit
    | Unsafe_set(string);
  let breakInside = opt => (
    "break-inside",
    switch (opt) {
    | Auto => auto
    | Avoid => "avoid"
    | AvoidColumn => "avoid-column"
    | AvoidPage => "avoid-page"
    | AvoidRegion => "avoid-region"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type breakOptions = Break.breakOptions;
let breakAfter = Break.breakAfter;
let breakBefore = Break.breakBefore;
type breakInsideOptions = Break.breakInsideOptions;
let breakInside = Break.breakInside;

// C's
let captionSide = {};
let caretColor = {};
let charset = {};
let clear = {};
let clip = {};
let color = {};
let columnCount = {};
let columnFill = {};
let columnGap = {};
let columnRule = {};
let columnRuleColor = {};
let columnRuleStyle = {};
let columnRuleWidth = {};
let columnSpan = {};
let columnWidth = {};
let columns = {};
let content = {};
let counterIncrement = {};
let counterReset = {};
let cursor = {};

// D's
let direction = {};
let display = {};

// E's
let emptyCells = {};