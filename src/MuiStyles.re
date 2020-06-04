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

module Caption = {
  type captionSideOptions =
    | Top
    | Bottom
    | Initial
    | Inherit
    | Unsafe_set(string);
  let captionSide = opt => (
    "caption-side",
    switch (opt) {
    | Top => "top"
    | Bottom => "bottom"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

// C's
type captionSideOptions = Caption.captionSideOptions;
let captionSide = Caption.captionSide;

module Caret = {
  type caretColorOptions =
    | Auto
    | Color(string)
    | Unsafe_set(string);
  let caretColor = opt => (
    "caret-color",
    switch (opt) {
    | Auto => "auto"
    | Color(color) => color
    | Unsafe_set(str) => str
    },
  );
};

type caretColorOptions = Caret.caretColorOptions;
let caretColor = Caret.caretColor;

let charset = (set: string) => ("@charset", set);

module Clear = {
  type clearOptions =
    | None
    | Left
    | Right
    | Both
    | Initial
    | Inherit
    | Unsafe_set(string);
  let clear = opt => (
    "clear",
    switch (opt) {
    | None => none
    | Left => "left"
    | Right => "right"
    | Both => "both"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type clearOptions = Clear.clearOptions;
let clear = Clear.clear;

module Clip = {
  type clipPathOptions =
    | MarginBox
    | BorderBox
    | PaddingBox
    | ContentBox
    | FillBox
    | StrokeBox
    | ViewBox
    | Shape(string)
    | ClipSource(string)
    | None
    | Unsafe_set(string);
  let clipPath = opt => (
    "clip-path",
    switch (opt) {
    | MarginBox => "margin-box"
    | BorderBox => "border-box"
    | PaddingBox => "padding-box"
    | ContentBox => "content-box"
    | FillBox => "fill-box"
    | StrokeBox => "stroke-box"
    | ViewBox => "view-box"
    | Shape(shape) => shape
    | ClipSource(source) => source
    | None => none
    | Unsafe_set(str) => str
    },
  );
};

type clipPathOptions = Clip.clipPathOptions;
let clip = Clip.clipPath;
let clipPath = Clip.clipPath;

module Color = {
  type colorOptions =
    | Color(string)
    | Initial
    | Inherit;
  let color = opt => (
    "color",
    switch (opt) {
    | Color(color) => color
    | Initial => initial
    | Inherit => inherit_
    },
  );
};

type colorOptions = Color.colorOptions;
let color = Color.color;

module Column = {
  type columnCountOptions =
    | Count(int)
    | Auto
    | Initial
    | Inherit
    | Unsafe_set(string);
  let columnCount = opt => (
    "column-count",
    switch (opt) {
    | Count(count) => toStr(count)
    | Auto => auto
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type columnFillOptions =
    | Balance
    | Auto
    | Initial
    | Inherit
    | Unsafe_set(string);
  let columnFill = opt => (
    "column-fill",
    switch (opt) {
    | Balance => "balance"
    | Auto => auto
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type columnGapOptions =
    | Length(Length.options)
    | Normal
    | Initial
    | Inherit
    | Unsafe_set(string);
  let columnGap = opt => (
    "column-gap",
    switch (opt) {
    | Length(length) => Length.getLength(length)
    | Normal => "normal"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type columnRuleColorOptions =
    | Color(string)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let columnRuleColor = opt => (
    "column-rule-color",
    switch (opt) {
    | Color(color) => color
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type columnRuleStyleOptions =
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
  let columnRuleStyle = opt => (
    "column-rule-style",
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
    },
  );

  type columnRuleWidthOptions =
    | Width(Length.options)
    | Medium
    | Thin
    | Thick
    | Initial
    | Inherit
    | Unsafe_set(string);
  let columnRuleWidth = opt => (
    "column-rule-width",
    switch (opt) {
    | Width(length) => Length.getLength(length)
    | Medium => "medium"
    | Thin => "thin"
    | Thick => "thick"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  let columnRule =
      (
        ~color: option(columnRuleColorOptions)=?,
        ~style: option(columnRuleStyleOptions)=?,
        ~width: option(columnRuleWidthOptions)=?,
        (),
      ) => {
    let (_, color) = color->Belt.Option.getWithDefault(Unsafe_set(""))->columnRuleColor;
    let (_, style) = style->Belt.Option.getWithDefault(Unsafe_set(""))->columnRuleStyle;
    let (_, width) = width->Belt.Option.getWithDefault(Unsafe_set(""))->columnRuleWidth;

    {j|$width $style $color|j};
  };

  type columnSpanOptions =
    | None
    | All
    | Initial
    | Inherit
    | Unsafe_set(string);
  let columnSpan = opt => (
    "column-span",
    switch (opt) {
    | None => none
    | All => "all"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type columnWidthOptions =
    | Auto
    | Width(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let columnWidth = opt => (
    "column-width",
    switch (opt) {
    | Auto => auto
    | Width(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type columnsOptions =
    | Auto
    | Columns(columnWidthOptions, columnCountOptions)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let columns = opt => (
    "columns",
    switch (opt) {
    | Auto => auto
    | Columns(width, count) =>
      let (_, width) = columnWidth(width);
      let (_, count) = columnCount(count);
      {j|$width $count|j};
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type columnCountOptions = Column.columnCountOptions;
let columnCount = Column.columnCount;
type columnFillOptions = Column.columnFillOptions;
let columnFill = Column.columnFill;
type columnGapOptions = Column.columnGapOptions;
let columnGap = Column.columnGap;
let columnRule = Column.columnRule;
type columnRuleColorOptions = Column.columnRuleColorOptions;
let columnRuleColor = Column.columnRuleColor;
type columnRuleStyleOptions = Column.columnRuleStyleOptions;
let columnRuleStyle = Column.columnRuleStyle;
type columnRuleWidthOptions = Column.columnRuleWidthOptions;
let columnRuleWidth = Column.columnRuleWidth;
type columnSpanOptions = Column.columnSpanOptions;
let columnSpan = Column.columnSpan;
type columnWidthOptions = Column.columnWidthOptions;
let columnWidth = Column.columnWidth;
type columnsOptions = Column.columnsOptions;
let columns = Column.columns;

module Content = {
  type contentOptions =
    | Normal
    | None
    | Counter
    | Attr(string)
    | String(string)
    | OpenQuote
    | CloseQuote
    | NoOpenQuote
    | Url(string)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let content = opt => (
    "content",
    switch (opt) {
    | Normal => "normal"
    | None => none
    | Counter => "counter"
    | Attr(attr) => {j|attr($attr)|j}
    | String(str) => str
    | OpenQuote => "open-quote"
    | CloseQuote => "close-quote"
    | NoOpenQuote => "no-open-quote"
    | Url(url) => {j|url($url)|j}
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type contentOptions = Content.contentOptions;
let content = Content.content;

module Counter = {
  type counterOptions =
    | None
    | ID(int)
    | Initial
    | Inherit
    | Unsafe_set(string);

  let getCounterValue = opt =>
    switch (opt) {
    | None => none
    | ID(id) => toStr(id)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  let counterIncrement = opt => ("counter-increment", getCounterValue(opt));

  let counterReset = opt => ("counter-reset", getCounterValue(opt));
};

type counterOptions = Counter.counterOptions;
let counterIncrement = Counter.counterIncrement;
let counterReset = Counter.counterReset;

module Cursor = {
  type options =
    | Alias
    | AllScroll
    | Auto
    | Cell
    | ContextMenu
    | ColResize
    | Copy
    | Crosshair
    | Default
    | EResize
    | EwResize
    | Grab
    | Grabbing
    | Help
    | Move
    | NResize
    | NeResize
    | NeswResize
    | NsResize
    | NwResize
    | NwseResize
    | NoDrop
    | None
    | NotAllowed
    | Pointer
    | Progress
    | RowRezise
    | SResize
    | SeResize
    | SwResize
    | Text
    | Url(string)
    | VerticalText
    | WResize
    | Wait
    | ZoomIn
    | ZoomOut
    | Initial
    | Inherit
    | Unsafe_set(string);

  let cursor = opt => (
    "cursor",
    switch (opt) {
    | Alias => "alias"
    | AllScroll => "all-scroll"
    | Auto => auto
    | Cell => "cell"
    | ContextMenu => "context-menu"
    | ColResize => "col-resize"
    | Copy => "copy"
    | Crosshair => "crosshair"
    | Default => "default"
    | EResize => "e-resize"
    | EwResize => "ew-resize"
    | Grab => "grab"
    | Grabbing => "grabbing"
    | Help => "help"
    | Move => "move"
    | NResize => "n-resize"
    | NeResize => "ne-resize"
    | NeswResize => "nesw-resize"
    | NsResize => "ns-resize"
    | NwResize => "nw-resize"
    | NwseResize => "nwse-resize"
    | NoDrop => "no-drop"
    | None => none
    | NotAllowed => "not-allowed"
    | Pointer => "pointer"
    | Progress => "progress"
    | RowRezise => "row-resize"
    | SResize => "s-resize"
    | SeResize => "se=resize"
    | SwResize => "sw-resize"
    | Text => "text"
    | Url(url) => url
    | VerticalText => "vertical-text"
    | WResize => "w-resize"
    | Wait => "wait"
    | ZoomIn => "zoom-in"
    | ZoomOut => "zoom-out"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type cursorOptions = Cursor.options;
let cursor = Cursor.cursor;

// D's
module Direction = {
  type options =
    | Ltr
    | Rtl
    | Initial
    | Inherit
    | Unsafe_set(string);
  let direction = opt => (
    "direction",
    switch (opt) {
    | Ltr => "ltr"
    | Rtl => "rtl"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type directionOptions = Direction.options;
let direction = Direction.direction;

module Display = {
  type options =
    | Inline
    | Block
    | Contents
    | Flex
    | Grid
    | InlineBlock
    | InlineFlex
    | InlineGrid
    | InlineTable
    | ListItem
    | RunIn
    | Table
    | TableCaption
    | TableColumnGroup
    | TableHeaderGroup
    | TableFooterGroup
    | TableRowGroup
    | TableCell
    | TableColumn
    | TableRow
    | None
    | Initial
    | Inherit
    | Unsafe_set(string);

  let display = opt => (
    "display",
    switch (opt) {
    | Inline => "inline"
    | Block => "block"
    | Contents => "contents"
    | Flex => "flex"
    | Grid => "grid"
    | InlineBlock => "inline-block"
    | InlineFlex => "inline-flex"
    | InlineGrid => "inline-grid"
    | InlineTable => "inline-table"
    | ListItem => "list-item"
    | RunIn => "run-in"
    | Table => "table"
    | TableCaption => "table-caption"
    | TableColumnGroup => "table-column-group"
    | TableHeaderGroup => "table-header-group"
    | TableFooterGroup => "table-footer-group"
    | TableRowGroup => "table-row-group"
    | TableCell => "table-cell"
    | TableColumn => "table-column"
    | TableRow => "table-row"
    | None => none
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type displayOptions = Display.options;
let display = Display.display;

// E's
module EmptyCells = {
  type options =
    | Show
    | Hide
    | Initial
    | Inherit
    | Unsafe_set(string);

  let emptyCells = opt => (
    "empty-cells",
    switch (opt) {
    | Show => "show"
    | Hide => "hide"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type emptyCellsOptions = EmptyCells.options;
let emptyCells = EmptyCells.emptyCells;

// F's
module Filter = {
  type options =
    | None
    | Blur(int)
    | Brightness(float)
    | Contrast(float)
    | DropShadow(list(string))
    | Grayscale(float)
    | HueRotate(int)
    | Invert(float)
    | Opacity(float)
    | Saturate(float)
    | Sepia(float)
    | Url(string)
    | Initial
    | Inherit
    | Unsafe_set(string);

  let filter = opt => (
    "filter",
    switch (opt) {
    | None => none
    | Blur(blur) => {j|blur($(blur)px)|j}
    | Brightness(brightness) => {j|brightness($(brightness)%)|j}
    | Contrast(contrast) => {j|contrast($(contrast)%)|j}
    | DropShadow(shadowList) => Array.of_list(shadowList) |> Js.Array.joinWith(" ")
    | Grayscale(scale) => {j|grayscale($(scale)%)|j}
    | HueRotate(rotate) => {j|hue-rotate($(rotate)deg)|j}
    | Invert(invert) => {j|invert($(invert)%)|j}
    | Opacity(opacity) => {j|opacity($(opacity)%)|j}
    | Saturate(saturate) => {j|saturate($(saturate)%)|j}
    | Sepia(sepia) => {j|sepia($(sepia)%)|j}
    | Url(url) => {j|url($url)|j}
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type filterOptions = Filter.options;
let filter = Filter.filter;

module Flex = {
  type flexBasisOptions =
    | Basis(Length.options)
    | Auto
    | Initial
    | Inherit
    | Unsafe_set(string);
  let flexBasis = opt => (
    "flex-basis",
    switch (opt) {
    | Basis(length) => Length.getLength(length)
    | Auto => auto
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type flexDirectionOptions =
    | Row
    | RowReverse
    | Column
    | ColumnReverse
    | Initial
    | Inherit
    | Unsafe_set(string);
  let flexDirection = opt => (
    "flex-direction",
    switch (opt) {
    | Row => "row"
    | RowReverse => "row-reverse"
    | Column => "column"
    | ColumnReverse => "column-reverse"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type flexGrowOptions =
    | Grow(int)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let flexGrow = opt => (
    "flex-grow",
    switch (opt) {
    | Grow(grow) => toStr(grow)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type flexShrinkOptions =
    | Shrink(int)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let flexShrink = opt => (
    "flex-shrink",
    switch (opt) {
    | Shrink(shrink) => toStr(shrink)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  let flex =
      (
        ~grow: option(flexGrowOptions)=?,
        ~shrink: option(flexShrinkOptions)=?,
        ~basis: option(flexBasisOptions)=?,
        (),
      ) => {
    let (_, grow) = grow->Belt.Option.getWithDefault(Unsafe_set(""))->flexGrow;
    let (_, shrink) = shrink->Belt.Option.getWithDefault(Unsafe_set(""))->flexShrink;
    let (_, basis) = basis->Belt.Option.getWithDefault(Unsafe_set(""))->flexBasis;

    {j|$grow $shrink $basis|j} |> String.trim;
  };

  type flexWrapOptions =
    | NoWrap
    | Wrap
    | WrapReverse
    | Initial
    | Inherit
    | Unsafe_set(string);
  let flexWrap = opt => (
    "flex-wrap",
    switch (opt) {
    | NoWrap => "no-wrap"
    | Wrap => "wrap"
    | WrapReverse => "wrap-reverse"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type flexFlowOptions =
    | Flow(flexDirectionOptions, flexWrapOptions)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let flexFlow = opt => (
    "flex-flow",
    switch (opt) {
    | Flow(direction, wrap) =>
      let (_, direction) = flexDirection(direction);
      let (_, wrap) = flexWrap(wrap);

      {j|$direction $wrap|j};
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

let flex = Flex.flex;
type flexBasisOptions = Flex.flexBasisOptions;
let flexBasis = Flex.flexBasis;
type flexDirectionOptions = Flex.flexDirectionOptions;
let flexDirection = Flex.flexDirection;
type flexFlowOptions = Flex.flexFlowOptions;
let flexFlow = Flex.flexFlow;
type flexGrowOptions = Flex.flexGrowOptions;
let flexGrow = Flex.flexGrow;
type flexShrinkOptions = Flex.flexShrinkOptions;
let flexShrink = Flex.flexShrink;
type flexWrapOptions = Flex.flexWrapOptions;
let flexWrap = Flex.flexWrap;

module Float = {
  type options =
    | None
    | Left
    | Right
    | Initial
    | Inherit
    | Unsafe_set(string);
  let float = opt => (
    "float",
    switch (opt) {
    | None => none
    | Left => "left"
    | Right => "right"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type floatOptions = Float.options;
let float = Float.float;

module Font = {
  type fontFamilyOptions =
    | Family(string)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let fontFamily = opt => (
    "font-family",
    switch (opt) {
    | Family(family) => family
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type fontFeatureSettingsOptions =
    | Normal
    | Feature(list(string))
    | Unsafe_set(string);
  let fontFeatureSettings = opt => (
    "font-feature-settings",
    switch (opt) {
    | Normal => "normal"
    | Feature(features) => Array.of_list(features) |> Js.Array.joinWith(" ")
    | Unsafe_set(str) => str
    },
  );

  type fontKerningOptions =
    | Auto
    | Normal
    | None
    | Unsafe_set(string);
  let fontKerning = opt => (
    "font-kerning",
    switch (opt) {
    | Auto => auto
    | Normal => "normal"
    | None => none
    | Unsafe_set(str) => str
    },
  );

  type fontSizeOptions =
    | Medium
    | XxSmall
    | XSmall
    | Small
    | Large
    | XLarge
    | XxLarge
    | Smaller
    | Larger
    | Size(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let fontSize = opt => (
    "font-size",
    switch (opt) {
    | Medium => "medium"
    | XxSmall => "xx-small"
    | XSmall => "x-small"
    | Small => "small"
    | Large => "large"
    | XLarge => "x-large"
    | XxLarge => "xx-large"
    | Smaller => "smaller"
    | Larger => "larger"
    | Size(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type fontSizeAdjustOptions =
    | Adjust(float)
    | None
    | Initial
    | Inherit
    | Unsafe_set(string);
  let fontSizeAdjust = opt => (
    "font-size-adjust",
    switch (opt) {
    | Adjust(adjust) => string_of_float(adjust)
    | None => none
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type fontStretchOptions =
    | UltraCondensed
    | ExtraCondensed
    | Condensed
    | SemiCondensed
    | Normal
    | SemiExpanded
    | Expanded
    | ExtraExpanded
    | UltraExpanded
    | Initial
    | Inherit
    | Unsafe_set(string);
  let fontStretch = opt => (
    "font-stretch",
    switch (opt) {
    | UltraCondensed => "ultra-condensed"
    | ExtraCondensed => "extra-condensed"
    | Condensed => "condensed"
    | SemiCondensed => "semi-condensed"
    | Normal => "normal"
    | SemiExpanded => "semi-expanded"
    | Expanded => "expanded"
    | ExtraExpanded => "extra-expanded"
    | UltraExpanded => "ultra-expanded"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type fontStyleOptions =
    | Normal
    | Italic
    | Oblique
    | Initial
    | Inherit
    | Unsafe_set(string);
  let fontStyle = opt => (
    "font-style",
    switch (opt) {
    | Normal => "normal"
    | Italic => "italic"
    | Oblique => "oblique"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type fontVariantOptions =
    | Normal
    | SmallCaps
    | Initial
    | Inherit
    | Unsafe_set(string);
  let fontVariant = opt => (
    "font-variant",
    switch (opt) {
    | Normal => "normal"
    | SmallCaps => "small-caps"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type fontVariantCapsOptions =
    | Normal
    | SmallCaps
    | AllSmallCaps
    | PetiteCaps
    | AllPetiteCaps
    | Unicase
    | TitlingCaps
    | Initial
    | Inherit
    | Unsafe_set(string);
  let fontVariantCaps = opt => (
    "font-variant-caps",
    switch (opt) {
    | Normal => "normal"
    | SmallCaps => "small-caps"
    | AllSmallCaps => "all-small-caps"
    | PetiteCaps => "petite-caps"
    | AllPetiteCaps => "all-petite-caps"
    | Unicase => "unicase"
    | TitlingCaps => "titling-caps"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type weightOptions =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
  and fontWeightOptions =
    | Normal
    | Bold
    | Bolder
    | Lighter
    | Weight(weightOptions)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let fontWeight = opt => (
    "font-weight",
    switch (opt) {
    | Normal => "normal"
    | Bold => "bold"
    | Bolder => "bolder"
    | Lighter => "lighter"
    | Weight(weight) =>
      (
        switch (weight) {
        | One => 100
        | Two => 200
        | Three => 300
        | Four => 400
        | Five => 500
        | Six => 600
        | Seven => 700
        | Eight => 800
        | Nine => 900
        }
      )
      |> toStr
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  let font = fontList => Array.of_list(fontList) |> Js.Array.joinWith(" ");

  let fontFace =
      (
        ~family,
        ~src,
        ~stretch: option(fontStretchOptions)=?,
        ~style: option(fontStyleOptions)=?,
        ~weight: option(fontWeightOptions)=?,
        ~unicodeRange: option(string)=?,
        (),
      ) => {
    let stretch = stretch->Belt.Option.getWithDefault(Unsafe_set(""))->fontStretch;
    let style = style->Belt.Option.getWithDefault(Unsafe_set(""))->fontStyle;
    let weight = weight->Belt.Option.getWithDefault(Unsafe_set(""))->fontWeight;
    let unicodeRange = unicodeRange->Belt.Option.getWithDefault("");

    [("font-family", family), ("src", {j|url($src)|j}), stretch, style, weight, ("unicode-range", unicodeRange)]
    |> Js.Dict.fromList;
  };
};

let font = Font.font;
let fontFace = Font.fontFace;
type fontFamilyOptions = Font.fontFamilyOptions;
let fontFamily = Font.fontFamily;
type fontFeatureSettingsOptions = Font.fontFeatureSettingsOptions;
let fontFeatureSettings = Font.fontFeatureSettings;
type fontKerningOptions = Font.fontKerningOptions;
let fontKerning = Font.fontKerning;
type fontSizeOptions = Font.fontSizeOptions;
let fontSize = Font.fontSize;
type fontSizeAdjustOptions = Font.fontSizeAdjustOptions;
let fontSizeAdjust = Font.fontSizeAdjust;
type fontStretchOptions = Font.fontStretchOptions;
let fontStretch = Font.fontStretch;
type fontStyleOptions = Font.fontStyleOptions;
let fontStyle = Font.fontStyle;
type fontVariantOptions = Font.fontVariantOptions;
let fontVariant = Font.fontVariant;
type fontVariantCapsOptions = Font.fontVariantCapsOptions;
let fontVariantCaps = Font.fontVariantCaps;
type fontWeightOptions = Font.fontWeightOptions;
let fontWeight = Font.fontWeight;

// G's
module Grid = {};

let grid = {};
let gridArea = {};
let gridAutoColumns = {};
let gridAutoFlow = {};
let gridAutoRows = {};
let gridColumn = {};
let gridColumnEnd = {};
let gridColumnGap = {};
let gridColumnStart = {};
let gridGap = {};
let gridRow = {};
let gridRowEnd = {};
let gridRowGap = {};
let gridRowStart = {};
let gridTemplate = {};
let gridTemplateAreas = {};
let gridTemplateColumns = {};
let gridTemplateRows = {};

// H's
module HangingPunctuation = {
  type options =
    | None
    | First
    | Last
    | AllowEnd
    | ForceEnd
    | Initial
    | Inherit
    | Unsafe_set(string);
  let hangingPunctuation = opt => (
    "hanging-punctuation",
    switch (opt) {
    | None => none
    | First => "first"
    | Last => "last"
    | AllowEnd => "allow-end"
    | ForceEnd => "force-end"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type hangingPunctuationOptions = HangingPunctuation.options;
let hangingPunctuation = HangingPunctuation.hangingPunctuation;

module Height = {
  type options =
    | Auto
    | Height(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let height = opt => (
    "height",
    switch (opt) {
    | Auto => auto
    | Height(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type heightOptions = Height.options;
let height = Height.height;

module Hyphens = {
  type options =
    | None
    | Manual
    | Auto
    | Initial
    | Inherit
    | Unsafe_set(string);
  let hyphens = opt => (
    "hyphens",
    switch (opt) {
    | None => none
    | Manual => "manual"
    | Auto => auto
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type hyphenOptions = Hyphens.options;
let hyphens = Hyphens.hyphens;

// I's
module Isolation = {
  type options =
    | Auto
    | Isolate
    | Initial
    | Inherit
    | Unsafe_set(string);
  let isolation = opt => (
    "isolation",
    switch (opt) {
    | Auto => auto
    | Isolate => "isolate"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type isolationOptions = Isolation.options;
let isolation = Isolation.isolation;

// J's
module JustifyContent = {
  type options =
    | FlexStart
    | FlexEnd
    | Center
    | SpaceBetween
    | SpaceAround
    | Initial
    | Inherit
    | Unsafe_set(string);
  let justifyContent = opt => (
    "justify-content",
    switch (opt) {
    | FlexStart => "flex-start"
    | FlexEnd => "flex-end"
    | Center => "center"
    | SpaceBetween => "space-between"
    | SpaceAround => "space-around"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type justifyContentOptions = JustifyContent.options;
let justifyContent = JustifyContent.justifyContent;

// K's
module Keyframes = {
  let keyframe = (name, frame) => {
    let dict = Js.Dict.empty();
    frame |> Js.Dict.fromList |> Js.Dict.set(dict, name);
  };
};

// L's
module Left = {
  type options =
    | Auto
    | Left(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let left = opt => (
    "left",
    switch (opt) {
    | Auto => auto
    | Left(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type leftOptions = Left.options;
let left = Left.left;

module Letter = {
  type options =
    | Normal
    | Spacing(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let letterSpacing = opt => (
    "letter-spacing",
    switch (opt) {
    | Normal => "normal"
    | Spacing(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type letterSpacingOptions = Letter.options;
let letterSpacing = Letter.letterSpacing;

module Line = {
  type options =
    | Normal
    | Number(int)
    | Length(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);

  let lineHeight = opt => (
    "line-height",
    switch (opt) {
    | Normal => "normal"
    | Number(num) => toStr(num)
    | Length(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type lineHeightOptions = Line.options;
let lineHeight = Line.lineHeight;

module ListCss = {
  type listStyleImageOptions =
    | None
    | Url(string)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let listStyleImage = opt => (
    "list-style-image",
    switch (opt) {
    | None => none
    | Url(url) => url
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type listStylePositionOptions =
    | Inside
    | Outside
    | Initial
    | Inherit
    | Unsafe_set(string);
  let listStylePosition = opt => (
    "list-style-position",
    switch (opt) {
    | Inside => "inside"
    | Outside => "outside"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type listStyleTypeOptions =
    | Disc
    | Armenian
    | Circle
    | CjkIdeagraphic
    | Decimal
    | DecimalLeadingZero
    | Georgian
    | Hebrew
    | Hiragana
    | HiraganaIroha
    | Katakana
    | KatakanaIroha
    | LowerAlpha
    | LowerGreek
    | LowerLatin
    | LowerRoman
    | None
    | Square
    | UpperAlpha
    | UpperGreek
    | UpperLatin
    | UpperRoman
    | Initial
    | Inherit
    | Unsafe_set(string);
  let listStyleType = opt => (
    "list-style-type",
    switch (opt) {
    | Disc => "disc"
    | Armenian => "armenian"
    | Circle => "circle"
    | CjkIdeagraphic => "cjk-ideographic"
    | Decimal => "decimal"
    | DecimalLeadingZero => "decimal-leading-zero"
    | Georgian => "georgian"
    | Hebrew => "hebrew"
    | Hiragana => "hiragana"
    | HiraganaIroha => "hiragana-iroha"
    | Katakana => "katakana"
    | KatakanaIroha => "katakana-iroha"
    | LowerAlpha => "lower-alpha"
    | LowerGreek => "lower-greek"
    | LowerLatin => "lower-latin"
    | LowerRoman => "lower-roman"
    | None => none
    | Square => "square"
    | UpperAlpha => "upper-alpha"
    | UpperGreek => "upper-greek"
    | UpperLatin => "upper-latin"
    | UpperRoman => "upper-roman"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  let listStyle =
      (
        ~styleType: option(listStyleTypeOptions)=?,
        ~position: option(listStylePositionOptions)=?,
        ~image: option(listStyleImageOptions)=?,
        (),
      ) => {
    let (_, styleType) = styleType->Belt.Option.getWithDefault(Unsafe_set(""))->listStyleType;
    let (_, position) = position->Belt.Option.getWithDefault(Unsafe_set(""))->listStylePosition;
    let (_, image) = image->Belt.Option.getWithDefault(Unsafe_set(""))->listStyleImage;

    {j|$styleType $position $image|j} |> String.trim;
  };
};

let listStyle = ListCss.listStyle;
type listStyleImageOptions = ListCss.listStyleImageOptions;
let listStyleImage = ListCss.listStyleImage;
type listStylePositionOptions = ListCss.listStylePositionOptions;
let listStylePosition = ListCss.listStylePosition;
type listStyleTypeOptions = ListCss.listStyleTypeOptions;
let listStyleType = ListCss.listStyleType;

// M's
module Margin = {
  type options =
    | Margin(Length.options)
    | Auto
    | Initial
    | Inherit
    | Unsafe_set(string);
  let getMargin = opt =>
    switch (opt) {
    | Margin(length) => Length.getLength(length)
    | Auto => auto
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  let marginBottom = opt => ("margin-bottom", getMargin(opt));
  let marginLeft = opt => ("margin-left", getMargin(opt));
  let marginRight = opt => ("margin-right", getMargin(opt));
  let marginTop = opt => ("margin-top", getMargin(opt));

  let margin = margins =>
    margins |> List.map(length => Length.getLength(length)) |> Array.of_list |> Js.Array.joinWith(" ");
};

type marginOptions = Margin.options;
let margin = Margin.margin;
let marginBottom = Margin.marginBottom;
let marginLeft = Margin.marginLeft;
let marginRight = Margin.marginRight;
let marginTop = Margin.marginTop;

module Max = {
  type options =
    | None
    | Max(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let getMax = opt =>
    switch (opt) {
    | None => none
    | Max(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  let maxHeight = opt => ("max-height", getMax(opt));
  let maxWidth = opt => ("max-width", getMax(opt));
};

type maxOptions = Max.options;
let maxHeight = Max.maxHeight;
let maxWidth = Max.maxWidth;

module Media = {
  // TODO add media lol
};

let media = {};

module Min = {
  type options =
    | Min(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let getMin = opt =>
    switch (opt) {
    | Min(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  let minHeight = opt => ("min-height", getMin(opt));
  let minWidth = opt => ("min-width", getMin(opt));
};

type minOptions = Min.options;
let minHeight = Min.minHeight;
let minWidth = Min.minWidth;

module MixBlendMode = {
  type options =
    | Normal
    | Multiply
    | Screen
    | Overlay
    | Darken
    | Lighten
    | ColorDodge
    | ColorBurn
    | Difference
    | Exclusion
    | Hue
    | Saturation
    | Color
    | Luminosity
    | Unsafe_set(string);
  let mixBlendMode = opt => (
    "mix-blend-mode",
    switch (opt) {
    | Normal => "normal"
    | Multiply => "multiply"
    | Screen => "screen"
    | Overlay => "overlay"
    | Darken => "darken"
    | Lighten => "lighten"
    | ColorDodge => "color-dodge"
    | ColorBurn => "color-burn"
    | Difference => "difference"
    | Exclusion => "exclusion"
    | Hue => "hue"
    | Saturation => "saturation"
    | Color => "color"
    | Luminosity => "luminosity"
    | Unsafe_set(str) => str
    },
  );
};

type mixBlendModeOptions = MixBlendMode.options;
let mixBlendMode = MixBlendMode.mixBlendMode;

// O's
module ObjectCss = {
  type objectFitOptions =
    | Fill
    | Contain
    | Cover
    | None
    | ScaleDown
    | Initial
    | Inherit
    | Unsafe_set(string);
  let objectFit = opt => (
    "object-fit",
    switch (opt) {
    | Fill => "fill"
    | Contain => "contain"
    | Cover => "cover"
    | None => none
    | ScaleDown => "scale-down"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type objectPositionOptions =
    | Left
    | Right
    | Center
    | Px(int)
    | Pct(int)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let objectPosition = opt => (
    "object-position",
    switch (opt) {
    | Left => "left"
    | Right => "right"
    | Center => "center"
    | Px(px) => toStr(px) ++ "px"
    | Pct(pct) => toStr(pct) ++ "%"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type objectFitOptions = ObjectCss.objectFitOptions;
let objectFit = ObjectCss.objectFit;
type objectPositionOptions = ObjectCss.objectPositionOptions;
let objectPosition = ObjectCss.objectPosition;

module Opacity = {
  let opacity = o => ("opacity", string_of_float(o));
};

let opacity = Opacity.opacity;

module Order = {
  type options =
    | Order(int)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let order = opt => (
    "order",
    switch (opt) {
    | Order(order) => toStr(order)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type orderOptions = Order.options;
let order = Order.order;

module Outline = {
  type outlineColorOptions =
    | Invert
    | Color(string)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let outlineColor = opt => (
    "outline-color",
    switch (opt) {
    | Invert => "invert"
    | Color(color) => color
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type outlineOffsetOptions =
    | Offset(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let outlineOffset = opt => (
    "outline-offset",
    switch (opt) {
    | Offset(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type outlineStyleOptions =
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
  let outlineStyle = opt => (
    "outline-style",
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
    },
  );

  type outlineWidthOptions =
    | Medium
    | Thin
    | Thick
    | Width(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let outlineWidth = opt => (
    "outline-width",
    switch (opt) {
    | Medium => "medium"
    | Thin => "thin"
    | Thick => "thick"
    | Width(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  let outline =
      (
        ~style: outlineStyleOptions,
        ~width: option(outlineWidthOptions)=?,
        ~color: option(outlineColorOptions)=?,
        (),
      ) => {
    let (_, style) = outlineStyle(style);
    let (_, width) = width->Belt.Option.getWithDefault(Unsafe_set(""))->outlineWidth;
    let (_, color) = color->Belt.Option.getWithDefault(Unsafe_set(""))->outlineColor;

    {j|$style $width $color|j} |> String.trim;
  };
};

let outline = Outline.outline;
type outlineColorOptions = Outline.outlineColorOptions;
let outlineColor = Outline.outlineColor;
type outlineOffsetOptions = Outline.outlineOffsetOptions;
let outlineOffset = Outline.outlineOffset;
type outlineStyleOptions = Outline.outlineStyleOptions;
let outlineStyle = Outline.outlineStyle;
type outlineWidthOptions = Outline.outlineWidthOptions;
let outlineWidth = Outline.outlineWidth;

module Overflow = {
  type options =
    | Visible
    | Hidden
    | Scroll
    | Auto
    | Initial
    | Inherit
    | Unsafe_set(string);
  let getOverflow = opt =>
    switch (opt) {
    | Visible => "visible"
    | Hidden => "hidden"
    | Scroll => "scroll"
    | Auto => auto
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  let overflow = opt => ("overflow", getOverflow(opt));
  let overflowX = opt => ("overflow-x", getOverflow(opt));
  let overflowY = opt => ("overflow-y", getOverflow(opt));
};

type overflowOptions = Overflow.options;
let overflow = Overflow.overflow;
let overflowX = Overflow.overflowX;
let overflowY = Overflow.overflowY;

// P's
module Padding = {
  type options =
    | Padding(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let getPadding = opt =>
    switch (opt) {
    | Padding(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  let paddingBottom = opt => ("padding-bottom", getPadding(opt));
  let paddingLeft = opt => ("padding-left", getPadding(opt));
  let padddingRight = opt => ("padding-right", getPadding(opt));
  let paddingTop = opt => ("padding-top", getPadding(opt));

  let padding = pads =>
    pads |> List.map(length => Length.getLength(length)) |> Array.of_list |> Js.Array.joinWith(" ");
};

type paddingOptions = Padding.options;
let padding = Padding.padding;
let paddingBottom = Padding.paddingBottom;
let paddingLeft = Padding.paddingLeft;
let paddingRight = Padding.padddingRight;
let paddingTop = Padding.paddingTop;

module Page = {
  type pageBreakBeforeAfterOptions =
    | Auto
    | Always
    | Avoid
    | Left
    | Right
    | Initial
    | Inherit
    | Unsafe_set(string);
  let getPageBreakBeforeAfter = opt =>
    switch (opt) {
    | Auto => auto
    | Always => "always"
    | Avoid => "avoid"
    | Left => "left"
    | Right => "right"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    };

  let pageBreakAfter = opt => ("page-break-after", getPageBreakBeforeAfter(opt));
  let pageBreakBefore = opt => ("page-break-before", getPageBreakBeforeAfter(opt));

  type pageBreakInsideOptions =
    | Auto
    | Avoid
    | Initial
    | Inherit
    | Unsafe_set(string);
  let pageBreakInside = opt => (
    "page-break-inside",
    switch (opt) {
    | Auto => auto
    | Avoid => "avoid"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type pageBreakBeforeAfterOptions = Page.pageBreakBeforeAfterOptions;
let pageBreakAfter = Page.pageBreakAfter;
let pageBreakBefore = Page.pageBreakBefore;
type pageBreakInsideOptions = Page.pageBreakInsideOptions;
let pageBreakInside = Page.pageBreakInside;

module Perspective = {
  type perspectiveOptions =
    | Perspective(Length.options)
    | None
    | Initial
    | Inherit
    | Unsafe_set(string);
  let perspective = opt => (
    "perspective",
    switch (opt) {
    | Perspective(length) => Length.getLength(length)
    | None => none
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type perspectiveOriginOptions =
    | Origin(Length.options, Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let perspectiveOrigin = opt => (
    "perspective-origin",
    switch (opt) {
    | Origin(xAxis, yAxis) =>
      let xAxis = Length.getLength(xAxis);
      let yAxis = Length.getLength(yAxis);
      {j|$xAxis $yAxis|j};
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type perspectiveOptions = Perspective.perspectiveOptions;
let perspective = Perspective.perspective;
type perspectiveOriginOptions = Perspective.perspectiveOriginOptions;
let perspectiveOrigin = Perspective.perspectiveOrigin;

module PointerEvents = {
  type options =
    | None
    | Auto
    | Initial
    | Inherit
    | Unsafe_set(string);
  let pointerEvents = opt => (
    "pointer-events",
    switch (opt) {
    | None => none
    | Auto => auto
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type pointerEventsOptions = PointerEvents.options;
let pointerEvents = PointerEvents.pointerEvents;

module Position = {
  type options =
    | Static
    | Absolute
    | Fixed
    | Relative
    | Sticky
    | Initial
    | Inherit
    | Unsafe_set(string);
  let position = opt => (
    "position",
    switch (opt) {
    | Static => "static"
    | Absolute => "absolute"
    | Fixed => "fixed"
    | Relative => "relative"
    | Sticky => "sticky"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type postionOptions = Position.options;
let position = Position.position;

// Q's
module Quotes = {
  type options =
    | None
    | Quotes(list(string))
    | Initial
    | Inherit
    | Unsafe_set(string);
  let quotes = opt => (
    "quotes",
    switch (opt) {
    | None => none
    | Quotes(quotes) => quotes |> Array.of_list |> Js.Array.joinWith(" ")
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type quotesOptions = Quotes.options;
let quotes = Quotes.quotes;

// R's
module Resize = {
  type options =
    | None
    | Both
    | Horizontal
    | Vertical
    | Intiial
    | Inherit
    | Unsafe_set(string);
  let resize = opt => (
    "resize",
    switch (opt) {
    | None => none
    | Both => "both"
    | Horizontal => "horizontal"
    | Vertical => "vertical"
    | Intiial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type resizeOptions = Resize.options;
let resize = Resize.resize;

module Right = {
  type options =
    | Auto
    | Right(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let right = opt => (
    "right",
    switch (opt) {
    | Auto => auto
    | Right(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type rightOptions = Right.options;
let right = Right.right;

// S's
module ScrollBehavior = {
  type options =
    | Auto
    | Smooth
    | Initial
    | Inherit
    | Unsafe_set(string);
  let scrollBehavior = opt => (
    "scroll-behavior",
    switch (opt) {
    | Auto => auto
    | Smooth => "smooth"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type scrollBehaviorOptions = ScrollBehavior.options;
let scrollBehavior = ScrollBehavior.scrollBehavior;

// T's
module TabSize = {
  type options =
    | Number(int)
    | Length(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let tabSize = opt => (
    "tab-size",
    switch (opt) {
    | Number(size) => toStr(size)
    | Length(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type tabSizeOptions = TabSize.options;
let tabSize = TabSize.tabSize;

module TableLayout = {
  type options =
    | Auto
    | Fixed
    | Initial
    | Inherit
    | Unsafe_set(string);
  let tableLayout = opt => (
    "table-layout",
    switch (opt) {
    | Auto => auto
    | Fixed => "fixed"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type tabelLayoutOptions = TableLayout.options;
let tableLayout = TableLayout.tableLayout;

module Text = {
  type textAlignOptions =
    | Left
    | Right
    | Center
    | Justify
    | Initial
    | Inherit
    | Unsafe_set(string);
  let textAlign = opt => (
    "text-align",
    switch (opt) {
    | Left => "left"
    | Right => "right"
    | Center => "center"
    | Justify => "justify"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type textAlignLastOptions =
    | Auto
    | Left
    | Right
    | Center
    | Justify
    | Start
    | End
    | Initial
    | Inherit
    | Unsafe_set(string);
  let textAlignLast = opt => (
    "text-align-last",
    switch (opt) {
    | Auto => auto
    | Left => "left"
    | Right => "right"
    | Center => "center"
    | Justify => "justify"
    | Start => "start"
    | End => "end"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type textDecorationColorOptions =
    | Color(string)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let textDecorationColor = opt => (
    "text-decoration-color",
    switch (opt) {
    | Color(color) => color
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type textDecorationLineOptions =
    | None
    | Underline
    | Overline
    | LineThrough
    | Initial
    | Inherit
    | Unsafe_set(string);
  let textDecorationLine = opt => (
    "text-decoration-line",
    switch (opt) {
    | None => none
    | Underline => "underline"
    | Overline => "overline"
    | LineThrough => "line-through"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type textDecorationStyleOptions =
    | Solid
    | Double
    | Dotted
    | Dashed
    | Wavy
    | Initial
    | Inherit
    | Unsafe_set(string);
  let textDecorationStyle = opt => (
    "text-decoration-style",
    switch (opt) {
    | Solid => "solid"
    | Double => "double"
    | Dotted => "dotted"
    | Dashed => "dashed"
    | Wavy => "wavy"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  let textDecoration =
      (
        ~color: option(textDecorationColorOptions)=?,
        ~line: option(textDecorationLineOptions)=?,
        ~style: option(textDecorationStyleOptions)=?,
        (),
      ) => {
    let (_, color) = color->Belt.Option.getWithDefault(Unsafe_set(""))->textDecorationColor;
    let (_, line) = line->Belt.Option.getWithDefault(Unsafe_set(""))->textDecorationLine;
    let (_, style) = style->Belt.Option.getWithDefault(Unsafe_set(""))->textDecorationStyle;

    {j|$color $line $style|j} |> String.trim;
  };

  type textIndentOptions =
    | Indent(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let textIndent = opt => (
    "text-indent",
    switch (opt) {
    | Indent(length) => Length.getLength(length)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type textJustifyOptions =
    | Auto
    | InterWord
    | InterCharacter
    | None
    | Initial
    | Inherit
    | Unsafe_set(string);
  let textJustify = opt => (
    "text-justify",
    switch (opt) {
    | Auto => auto
    | InterWord => "inter-word"
    | InterCharacter => "inter-character"
    | None => none
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type textOverflowOptions =
    | Clip
    | Ellipsis
    | String(string)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let textOverflow = opt => (
    "text-overflow",
    switch (opt) {
    | Clip => "clip"
    | Ellipsis => "ellipsis"
    | String(str) => str
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type textShadowOptions =
    | Shadow(string)
    | None
    | Initial
    | Inherit
    | Unsafe_set(string);
  let textShadow = opt => (
    "text-shadow",
    switch (opt) {
    | Shadow(shadow) => shadow
    | None => none
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type textTransformOptions =
    | None
    | Capitalize
    | Uppercase
    | Lowercase
    | Initial
    | Inherit
    | Unsafe_set(string);
  let textTransform = opt => (
    "text-transform",
    switch (opt) {
    | None => none
    | Capitalize => "capitalize"
    | Uppercase => "uppercase"
    | Lowercase => "lowercase"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type textAlignOptions = Text.textAlignOptions;
let textAlign = Text.textAlign;
type textAlignLastOptions = Text.textAlignLastOptions;
let textAlignLast = Text.textAlignLast;
let textDecoration = Text.textDecoration;
type textDecorationColorOptions = Text.textDecorationColorOptions;
let textDecorationColor = Text.textDecorationColor;
type textDecorationLineOptions = Text.textDecorationLineOptions;
let textDecorationLine = Text.textDecorationLine;
type textDecorationStyleOptions = Text.textDecorationStyleOptions;
let textDecorationStyle = Text.textDecorationStyle;
type textIndentOptions = Text.textIndentOptions;
let textIndent = Text.textIndent;
type textJustifyOptions = Text.textJustifyOptions;
let textJustify = Text.textJustify;
type textOverflowOptions = Text.textOverflowOptions;
let textOverflow = Text.textOverflow;
type textShadowOptions = Text.textShadowOptions;
let textShadow = Text.textShadow;
type textTransformOptions = Text.textTransformOptions;
let textTransform = Text.textTransform;

module Top = {
  type options =
    | Auto
    | Top(Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let top = opt => (
    "top",
    switch (opt) {
    | Auto => auto
    | Top(top) => Length.getLength(top)
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

type topOptions = Top.options;
let top = Top.top;

module Transform = {
  type transformOptions =
    | None
    | Matrix(int, int, int, int, int, int)
    | Matrix3d(int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int)
    | Translate(Length.options, Length.options)
    | Translate3d(Length.options, Length.options, Length.options)
    | TranslateX(Length.options)
    | TranslateY(Length.options)
    | TranslateZ(Length.options)
    | Scale(float, float)
    | Scale3d(float, float, float)
    | ScaleX(float)
    | ScaleY(float)
    | ScaleZ(float)
    | Rotate(string)
    | Rotate3d(float, float, float, string)
    | RotateX(string)
    | RotateY(string)
    | RotateZ(string)
    | Skew(string, string)
    | SkewX(string)
    | SkewY(string)
    | Perspective(string)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let transform = optList => {
    let value =
      optList
      |> List.map(opt =>
           switch (opt) {
           | None => none
           | Matrix(n1, n2, n3, n4, n5, n6) => {j|matrix($n1, $n2, $n3, $n4, $n5, $n6)|j}
           | Matrix3d(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16) => {j|matrix($n1, $n2, $n3, $n4, $n5, $n6, $n7, $n8, $n9, $n10, $n11, $n12, $n13, $n14, $n15, $n16)|j}
           }
         );

    ("transform", value);
  };

  type xyAxis =
    | Left
    | Center
    | Right
    | Length(Length.options)
  and transformOriginOptions =
    | TwoD(xyAxis, xyAxis)
    | ThreeD(xyAxis, xyAxis, Length.options)
    | Initial
    | Inherit
    | Unsafe_set(string);
  let transformOrigin = opt => (
    "transform-origin",
    switch (opt) {
    | TwoD(xAxis, yAxis) => {j|$xAxis $yAxis|j}
    | ThreeD(xAxis, yAxis, zAxis) => {j|$xAxis $yAxis $zAxis|j}
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );

  type transformStyleOptions =
    | Flat
    | Preserve3D
    | Initial
    | Inherit
    | Unsafe_set(string);
  let transformStyle = opt => (
    "transform-style",
    switch (opt) {
    | Flat => "flat"
    | Preserve3D => "perserve-3d"
    | Initial => initial
    | Inherit => inherit_
    | Unsafe_set(str) => str
    },
  );
};

let transform = {};
let transformOrigin = {};
let transformStyle = {};
let transition = {};
let transitionDelay = {};
let transitionDuration = {};
let transitionProperty = {};
let transitionTimingFunction = {};