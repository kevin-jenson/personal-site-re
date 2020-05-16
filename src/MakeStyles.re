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

// constants
let initial = "initial";
let inherit_ = "inherit";
let unset = "unset";

module TimingFunctions = {
  type step =
    | Start
    | End
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
    | Steps(times, step) => {j|steps($times, $step)|j}
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
  let animationDelay = opt => (
    "animation-delay",
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
    },
  );

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

  let animationDuration = animationDelay;

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
      );

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