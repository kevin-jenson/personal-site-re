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
    | PositionExact(string, string)
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
    | PositionExact(x, y) => {j|$x $y|j}
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
    | Length(string, string)
    | LengthX(string)
    | LengthY(string)
    | Cover
    | Contain
    | Initial
    | Inherit
    | Unsafe_set(string);
  let backgroundSize = opt => (
    "background-size",
    switch (opt) {
    | Auto => auto
    | Length(x, y) => {j|$x $y|j}
    | LengthX(x) => {j|$x|j}
    | LengthY(y) => {j|auto $y|j}
    | Cover => "cover"
    | Contain => "contain"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
  // TODO: add background;
};

type backfaceVisibiltyOptions = Backface.backfaceVisibiltyOptions;
let backfaceVisibilty = Backface.backfaceVisibilty;

// TODO: add background
let background = {};
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

let border = {};
let borderBottom = {};
let borderBottomColor = {};
let borderBottomLeftRadius = {};
let borderBottomRightRadius = {};
let borderBottomStyle = {};
let borderBottomWidth = {};
let borderCollapse = {};
let borderColor = {};
let borderImage = {};
let borderImageOutset = {};
let borderImageRepeat = {};
let borderImageSlice = {};
let borderImageSource = {};
let borderImageWidth = {};
let borderLeft = {};
let borderLeftColor = {};
let borderLeftStyle = {};
let borderLeftWidth = {};
let borderRadius = {};
let borderRight = {};
let borderRightColor = {};
let borderRightStyle = {};
let borderRightWidth = {};
let borderSpacing = {};
let borderStyle = {};
let borderTop = {};
let borderTopColor = {};
let borderTopLeftRadius = {};
let borderTopRightRadius = {};
let borderTopStyle = {};
let borderTopWidth = {};
let borderWidth = {};

let bottom = {};

let boxDecorationBreak = {};
let boxShadow = {};
let boxSizing = {};

let breakAfter = {};
let breakBefore = {};
let breakInside = {};