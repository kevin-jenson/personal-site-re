// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Js_dict from "bs-platform/lib/es6/js_dict.js";
import * as Caml_option from "bs-platform/lib/es6/caml_option.js";
import * as CamlinternalOO from "bs-platform/lib/es6/camlinternalOO.js";
import * as Caml_exceptions from "bs-platform/lib/es6/caml_exceptions.js";
import * as Styles from "@material-ui/styles";

function clsx(classNames) {
  return List.fold_left((function (classes, cls) {
                return "" + (String(classes) + (" " + (String(cls) + "")));
              }), "", classNames);
}

function toFloat(prim) {
  return prim;
}

function toInt(prim) {
  return prim | 0;
}

function toStr(prim) {
  return String(prim);
}

function vw($$int) {
  return String($$int) + "vw";
}

function vh($$int) {
  return String($$int) + "vh";
}

function px($$int) {
  return String($$int) + "px";
}

function pct($$int) {
  return String($$int) + "%";
}

function deg($$int) {
  return String($$int) + "deg";
}

function ms($$int) {
  return String($$int) + "ms";
}

function width(str) {
  return /* tuple */[
          "width",
          str
        ];
}

function height(str) {
  return /* tuple */[
          "height",
          str
        ];
}

function overflow(str) {
  return /* tuple */[
          "overflow",
          str
        ];
}

function display(str) {
  return /* tuple */[
          "display",
          str
        ];
}

function flexDirection(str) {
  return /* tuple */[
          "flex-direction",
          str
        ];
}

function padding(pad) {
  var getPaddingStr = function (str, p) {
    return str + (", " + p);
  };
  var paddingStr = List.fold_left(getPaddingStr, "", pad);
  return /* tuple */[
          "padding",
          paddingStr
        ];
}

function transition(str) {
  return /* tuple */[
          "transition",
          str
        ];
}

function transform(str) {
  return /* tuple */[
          "transfrom",
          str
        ];
}

function zIndex(index) {
  return /* tuple */[
          "z-index",
          String(index)
        ];
}

function margin(marg) {
  var getMarginStr = function (str, m) {
    return str + (", " + m);
  };
  var marginStr = List.fold_left(getMarginStr, "", marg);
  return /* tuple */[
          "margin",
          marginStr
        ];
}

function opacity(num) {
  return /* tuple */[
          "opacity",
          num.toString()
        ];
}

function textAlign(str) {
  return /* tuple */[
          "text-align",
          str
        ];
}

function lineHeight(str) {
  return /* tuple */[
          "line-height",
          str
        ];
}

function fontSize(str) {
  return /* tuple */[
          "font-size",
          str
        ];
}

function textDecoration(str) {
  return /* tuple */[
          "text-decoration",
          str
        ];
}

function color(str) {
  return /* tuple */[
          "color",
          str
        ];
}

function nthChild(child, dict) {
  var child$1 = String(child);
  return /* tuple */[
          "&:nth-child(" + (String(child$1) + ")"),
          dict
        ];
}

function $$var(str, dict) {
  return /* tuple */[
          "&$" + str,
          dict
        ];
}

function hover(dict) {
  return /* tuple */[
          "&:hover",
          dict
        ];
}

function rotate(str) {
  return "rotate(" + (String(str) + ")");
}

function translateY(str) {
  return "translateY(" + (String(str) + ")");
}

function translate(x, y) {
  return "translate(" + (String(x) + (", " + (String(y) + ")")));
}

function important(str) {
  return str + " !important";
}

function keyframes(name, frames) {
  var formatFrames = function (dict, param) {
    dict[String(param[0]) + "%"] = param[1];
    return dict;
  };
  var keyframe = List.fold_left(formatFrames, { }, frames);
  return /* tuple */[
          "@keyframes " + (String(name) + ""),
          keyframe
        ];
}

var css = Js_dict.fromList;

var create = Js_dict.fromList;

function getClassName(classes, key) {
  var className = Js_dict.get(classes, key);
  if (className !== undefined) {
    return className;
  } else {
    return "";
  }
}

function useStyles(muiUseStyles, props) {
  var partial_arg = props !== undefined ? Curry._1(muiUseStyles, Caml_option.valFromOption(props)) : Curry._1(muiUseStyles, "");
  return (function (param) {
      return getClassName(partial_arg, param);
    });
}

function makeGlobalStyles(styles) {
  var styleDict = { };
  styleDict["@global"] = styles;
  var partial_arg = Styles.makeStyles(styleDict);
  return (function (param) {
      return useStyles(partial_arg, param);
    });
}

function makeStyles(styles) {
  var partial_arg = Styles.makeStyles(styles);
  return (function (param) {
      return useStyles(partial_arg, param);
    });
}

function makeThemeStyles(styleFunc) {
  var partial_arg = Styles.makeStyles(styleFunc);
  return (function (param) {
      return useStyles(partial_arg, param);
    });
}

var auto = "auto";

var initial = "initial";

var inherit_ = "inherit";

var none = "none";

var unset = "unset";

function timing(opt) {
  if (typeof opt === "number") {
    switch (opt) {
      case /* Linear */0 :
          return "linear";
      case /* Ease */1 :
          return "ease";
      case /* EaseIn */2 :
          return "ease-in";
      case /* EaseOut */3 :
          return "ease-out";
      case /* EaseInOut */4 :
          return "ease-in-out";
      case /* StepStart */5 :
          return "step-start";
      case /* StepEnd */6 :
          return "step-end";
      case /* Initial */7 :
          return initial;
      case /* Inherit */8 :
          return inherit_;
      
    }
  } else {
    switch (opt.tag | 0) {
      case /* Step */0 :
          return "steps(" + (String(opt[0]) + ")");
      case /* Steps */1 :
          var step = opt[1];
          var stepValue = typeof step === "number" ? (
              step !== 0 ? "end" : "start"
            ) : step[0];
          return "steps(" + (String(opt[0]) + (", " + (String(stepValue) + ")")));
      case /* CubicBezier */2 :
          return "cubic-bezier(" + (String(opt[0]) + (", " + (String(opt[1]) + (", " + (String(opt[2]) + (", " + (String(opt[3]) + ")")))))));
      case /* Unsafe_set */3 :
          return opt[0];
      
    }
  }
}

var TimingFunctions = {
  timing: timing
};

function alignContent(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Stretch */0 :
          tmp = "stretch";
          break;
      case /* Center */1 :
          tmp = "center";
          break;
      case /* FlexStart */2 :
          tmp = "flex-start";
          break;
      case /* FlexEnd */3 :
          tmp = "flex-end";
          break;
      case /* SpaceBetween */4 :
          tmp = "space-between";
          break;
      case /* SpaceAround */5 :
          tmp = "space-around";
          break;
      case /* Initial */6 :
          tmp = initial;
          break;
      case /* Inherit */7 :
          tmp = inherit_;
          break;
      
    }
  } else {
    tmp = opt[0];
  }
  return /* tuple */[
          "align-content",
          tmp
        ];
}

function alignItems(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Stretch */0 :
          tmp = "stretch";
          break;
      case /* Center */1 :
          tmp = "center";
          break;
      case /* FlexStart */2 :
          tmp = "flex-start";
          break;
      case /* FlexEnd */3 :
          tmp = "flex-end";
          break;
      case /* Baseline */4 :
          tmp = "baseline";
          break;
      case /* Initial */5 :
          tmp = initial;
          break;
      case /* Inherit */6 :
          tmp = inherit_;
          break;
      
    }
  } else {
    tmp = opt[0];
  }
  return /* tuple */[
          "align-items",
          tmp
        ];
}

function alignSelf(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Auto */0 :
          tmp = "auto";
          break;
      case /* Stretch */1 :
          tmp = "stretch";
          break;
      case /* Center */2 :
          tmp = "center";
          break;
      case /* FlexStart */3 :
          tmp = "flex-start";
          break;
      case /* FlexEnd */4 :
          tmp = "flex-end";
          break;
      case /* Baseline */5 :
          tmp = "baseline";
          break;
      case /* Initial */6 :
          tmp = initial;
          break;
      case /* Inherit */7 :
          tmp = inherit_;
          break;
      
    }
  } else {
    tmp = opt[0];
  }
  return /* tuple */[
          "align-self",
          tmp
        ];
}

var Align = {
  alignContent: alignContent,
  alignItems: alignItems,
  alignSelf: alignSelf
};

function all(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Initial */0 :
          tmp = initial;
          break;
      case /* Inherit */1 :
          tmp = inherit_;
          break;
      case /* Unset */2 :
          tmp = unset;
          break;
      
    }
  } else {
    tmp = opt[0];
  }
  return /* tuple */[
          "all",
          tmp
        ];
}

var All = {
  all: all
};

var Not_valid = Caml_exceptions.create("MuiStyles.Animation.Not_valid");

function _animationTime(opt) {
  if (typeof opt === "number") {
    if (opt === /* Initial */0) {
      return initial;
    } else {
      return inherit_;
    }
  }
  if (opt.tag) {
    return opt[0];
  }
  var str = opt[0];
  if (str.includes("ms") || str.includes("s")) {
    return str;
  }
  throw [
        Not_valid,
        "Time(string) needs to be in seconds or miliseconds"
      ];
}

function animationDelay(opt) {
  return /* tuple */[
          "animation-delay",
          _animationTime(opt)
        ];
}

function animationDirection(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Normal */0 :
          tmp = "normal";
          break;
      case /* Reverse */1 :
          tmp = "reverse";
          break;
      case /* Alternate */2 :
          tmp = "alternate";
          break;
      case /* AlternateReverse */3 :
          tmp = "alternate-reverse";
          break;
      case /* Initial */4 :
          tmp = initial;
          break;
      case /* Inherit */5 :
          tmp = inherit_;
          break;
      
    }
  } else {
    tmp = opt[0];
  }
  return /* tuple */[
          "animation-direction",
          tmp
        ];
}

function animationDuration(opt) {
  return /* tuple */[
          "animation-duration",
          _animationTime(opt)
        ];
}

function animationFillMode(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* None */0 :
          tmp = "none";
          break;
      case /* Forwards */1 :
          tmp = "forwards";
          break;
      case /* Backwards */2 :
          tmp = "backwards";
          break;
      case /* Both */3 :
          tmp = "both";
          break;
      case /* Initial */4 :
          tmp = initial;
          break;
      case /* Inherit */5 :
          tmp = inherit_;
          break;
      
    }
  } else {
    tmp = opt[0];
  }
  return /* tuple */[
          "animation-fill-mode",
          tmp
        ];
}

function animationIterationCount(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Infinite */0 :
          tmp = "infinite";
          break;
      case /* Initial */1 :
          tmp = initial;
          break;
      case /* Inherit */2 :
          tmp = inherit_;
          break;
      
    }
  } else {
    tmp = opt.tag ? opt[0] : String(opt[0]);
  }
  return /* tuple */[
          "animation-iteration-count",
          tmp
        ];
}

function animationName(opt) {
  return /* tuple */[
          "animation-name",
          typeof opt === "number" ? (
              opt !== 0 ? inherit_ : initial
            ) : opt[0]
        ];
}

function animationPlayState(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Paused */0 :
          tmp = "paused";
          break;
      case /* Running */1 :
          tmp = "running";
          break;
      case /* Initial */2 :
          tmp = initial;
          break;
      case /* Inherit */3 :
          tmp = inherit_;
          break;
      
    }
  } else {
    tmp = opt[0];
  }
  return /* tuple */[
          "animation-play-state",
          tmp
        ];
}

function animationTimingFunction(opt) {
  return /* tuple */[
          "animation-timing-function",
          timing(opt)
        ];
}

function animation(opt) {
  var tmp;
  switch (opt.tag | 0) {
    case /* Animation1 */0 :
        tmp = animationName(opt[0])[1];
        break;
    case /* Animation2 */1 :
        var match = animationName(opt[0]);
        var durationValue = _animationTime(opt[1]);
        tmp = "" + (String(match[1]) + (" " + (String(durationValue) + "")));
        break;
    case /* Animation3 */2 :
        var match$1 = animationName(opt[0]);
        var durationValue$1 = _animationTime(opt[1]);
        var timingValue = timing(opt[2]);
        tmp = "" + (String(match$1[1]) + (" " + (String(durationValue$1) + (" " + (String(timingValue) + "")))));
        break;
    case /* Animation4 */3 :
        var match$2 = animationName(opt[0]);
        var durationValue$2 = _animationTime(opt[1]);
        var timingValue$1 = timing(opt[2]);
        var delayValue = _animationTime(opt[3]);
        tmp = "" + (String(match$2[1]) + (" " + (String(durationValue$2) + (" " + (String(timingValue$1) + (" " + (String(delayValue) + "")))))));
        break;
    case /* Animation5 */4 :
        var match$3 = animationName(opt[0]);
        var durationValue$3 = _animationTime(opt[1]);
        var timingValue$2 = timing(opt[2]);
        var delayValue$1 = _animationTime(opt[3]);
        var match$4 = animationIterationCount(opt[4]);
        tmp = "" + (String(match$3[1]) + (" " + (String(durationValue$3) + (" " + (String(timingValue$2) + (" " + (String(delayValue$1) + (" " + (String(match$4[1]) + "")))))))));
        break;
    case /* Animation6 */5 :
        var match$5 = animationName(opt[0]);
        var durationValue$4 = _animationTime(opt[1]);
        var timingValue$3 = timing(opt[2]);
        var delayValue$2 = _animationTime(opt[3]);
        var match$6 = animationDirection(opt[5]);
        var match$7 = animationIterationCount(opt[4]);
        tmp = "" + (String(match$5[1]) + (" " + (String(durationValue$4) + (" " + (String(timingValue$3) + (" " + (String(delayValue$2) + (" " + (String(match$7[1]) + (" " + (String(match$6[1]) + "")))))))))));
        break;
    case /* Animation7 */6 :
        var match$8 = animationName(opt[0]);
        var durationValue$5 = _animationTime(opt[1]);
        var timingValue$4 = timing(opt[2]);
        var delayValue$3 = _animationTime(opt[3]);
        var match$9 = animationIterationCount(opt[4]);
        var match$10 = animationDirection(opt[5]);
        var match$11 = animationFillMode(opt[6]);
        tmp = "" + (String(match$8[1]) + (" " + (String(durationValue$5) + (" " + (String(timingValue$4) + (" " + (String(delayValue$3) + (" " + (String(match$9[1]) + (" " + (String(match$10[1]) + (" " + (String(match$11[1]) + "")))))))))))));
        break;
    case /* Animation8 */7 :
        var match$12 = animationName(opt[0]);
        var durationValue$6 = _animationTime(opt[1]);
        var timingValue$5 = timing(opt[2]);
        var delayValue$4 = _animationTime(opt[3]);
        var match$13 = animationIterationCount(opt[4]);
        var match$14 = animationDirection(opt[5]);
        var match$15 = animationFillMode(opt[6]);
        var match$16 = animationPlayState(opt[7]);
        tmp = "" + (String(match$12[1]) + (" " + (String(durationValue$6) + (" " + (String(timingValue$5) + (" " + (String(delayValue$4) + (" " + (String(match$13[1]) + (" " + (String(match$14[1]) + (" " + (String(match$15[1]) + (" " + (String(match$16[1]) + "")))))))))))))));
        break;
    case /* Unsafe_set */8 :
        tmp = opt[0];
        break;
    
  }
  return /* tuple */[
          "animation",
          tmp
        ];
}

var Animation = {
  Not_valid: Not_valid,
  _animationTime: _animationTime,
  animationDelay: animationDelay,
  animationDirection: animationDirection,
  animationDuration: animationDuration,
  animationFillMode: animationFillMode,
  animationIterationCount: animationIterationCount,
  animationName: animationName,
  animationPlayState: animationPlayState,
  animationTimingFunction: animationTimingFunction,
  animation: animation
};

function backfaceVisibilty(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Visible */0 :
          tmp = "visible";
          break;
      case /* Hidden */1 :
          tmp = "hidden";
          break;
      case /* Initial */2 :
          tmp = initial;
          break;
      case /* Inherit */3 :
          tmp = inherit_;
          break;
      
    }
  } else {
    tmp = opt[0];
  }
  return /* tuple */[
          "backface-visibility",
          tmp
        ];
}

var Backface = {
  backfaceVisibilty: backfaceVisibilty
};

function backgroundAttachment(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Scroll */0 :
          tmp = "scroll";
          break;
      case /* Fixed */1 :
          tmp = "fixed";
          break;
      case /* Local */2 :
          tmp = "local";
          break;
      case /* Initial */3 :
          tmp = initial;
          break;
      case /* Inherit */4 :
          tmp = inherit_;
          break;
      
    }
  } else {
    tmp = opt[0];
  }
  return /* tuple */[
          "background-attachment",
          tmp
        ];
}

function backgroundBlendMode(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Normal */0 :
          tmp = "normal";
          break;
      case /* Multiply */1 :
          tmp = "multiply";
          break;
      case /* Screen */2 :
          tmp = "screen";
          break;
      case /* Overlay */3 :
          tmp = "overlay";
          break;
      case /* Darken */4 :
          tmp = "darken";
          break;
      case /* Lighten */5 :
          tmp = "lighten";
          break;
      case /* ColorDodge */6 :
          tmp = "color-dodge";
          break;
      case /* Saturation */7 :
          tmp = "saturation";
          break;
      case /* Color */8 :
          tmp = "color";
          break;
      case /* Luminosity */9 :
          tmp = "luminosity";
          break;
      
    }
  } else {
    tmp = opt[0];
  }
  return /* tuple */[
          "background-blend-mode",
          tmp
        ];
}

function _backgroundBoxing(opt) {
  if (typeof opt !== "number") {
    return opt[0];
  }
  switch (opt) {
    case /* BorderBox */0 :
        return "border-box";
    case /* PaddingBox */1 :
        return "padding-box";
    case /* ContentBox */2 :
        return "content-box";
    case /* Initial */3 :
        return initial;
    case /* Inherit */4 :
        return inherit_;
    
  }
}

function backgroundClip(opt) {
  return /* tuple */[
          "background-clip",
          _backgroundBoxing(opt)
        ];
}

function backgroundColor(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Transparent */0 :
          tmp = "transparent";
          break;
      case /* Initial */1 :
          tmp = initial;
          break;
      case /* Inherit */2 :
          tmp = inherit_;
          break;
      
    }
  } else {
    tmp = opt[0];
  }
  return /* tuple */[
          "background-color",
          tmp
        ];
}

function backgroundImage(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* None */0 :
          tmp = none;
          break;
      case /* Initial */1 :
          tmp = initial;
          break;
      case /* Inherit */2 :
          tmp = inherit_;
          break;
      
    }
  } else {
    switch (opt.tag | 0) {
      case /* URL */0 :
          tmp = "url(" + (String(opt[0]) + ")");
          break;
      case /* LinearGradient */1 :
          tmp = "linear-gradient(" + (String(opt[0]) + ")");
          break;
      case /* RadialGradient */2 :
          tmp = "radial-gradient(" + (String(opt[0]) + ")");
          break;
      case /* RepeatingLinearGradient */3 :
          tmp = "repeating-linear-gradient(" + (String(opt[0]) + ")");
          break;
      case /* RepeatingRadialGradient */4 :
          tmp = "repeating-radial-gradient(" + (String(opt[0]) + ")");
          break;
      case /* Unsafe_set */5 :
          tmp = opt[0];
          break;
      
    }
  }
  return /* tuple */[
          "background-image",
          tmp
        ];
}

function backgroundOrigin(opt) {
  return /* tuple */[
          "background-origin",
          _backgroundBoxing(opt)
        ];
}

function backgroundPosition(opt) {
  var tmp;
  if (typeof opt === "number") {
    tmp = opt === /* Initial */0 ? initial : inherit_;
  } else {
    switch (opt.tag | 0) {
      case /* Position */0 :
          var pos2 = opt[1];
          switch (opt[0]) {
            case /* Center */1 :
                switch (pos2) {
                  case /* Bottom */0 :
                      tmp = "center bottom";
                      break;
                  case /* Center */1 :
                  case /* Left */2 :
                  case /* Right */3 :
                      tmp = "center center";
                      break;
                  case /* Top */4 :
                      tmp = "center top";
                      break;
                  
                }
                break;
            case /* Left */2 :
                switch (pos2) {
                  case /* Bottom */0 :
                      tmp = "left bottom";
                      break;
                  case /* Center */1 :
                      tmp = "left enter";
                      break;
                  case /* Left */2 :
                  case /* Right */3 :
                      tmp = "center center";
                      break;
                  case /* Top */4 :
                      tmp = "left top";
                      break;
                  
                }
                break;
            case /* Right */3 :
                switch (pos2) {
                  case /* Bottom */0 :
                      tmp = "right bottom";
                      break;
                  case /* Center */1 :
                      tmp = "right center";
                      break;
                  case /* Left */2 :
                  case /* Right */3 :
                      tmp = "center center";
                      break;
                  case /* Top */4 :
                      tmp = "right top";
                      break;
                  
                }
                break;
            case /* Bottom */0 :
            case /* Top */4 :
                tmp = "center center";
                break;
            
          }
          break;
      case /* PositionExact */1 :
          tmp = "" + (String(opt[0]) + (" " + (String(opt[1]) + "")));
          break;
      case /* Unsafe_set */2 :
          tmp = opt[0];
          break;
      
    }
  }
  return /* tuple */[
          "background-position",
          tmp
        ];
}

function backgroundRepeat(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Repeat */0 :
          tmp = "repeat";
          break;
      case /* RepeatX */1 :
          tmp = "repeat-x";
          break;
      case /* RepeatY */2 :
          tmp = "repeat-y";
          break;
      case /* NoRepeat */3 :
          tmp = "no-repeat";
          break;
      case /* Space */4 :
          tmp = "space";
          break;
      case /* Round */5 :
          tmp = "round";
          break;
      case /* Initial */6 :
          tmp = initial;
          break;
      case /* Inherit */7 :
          tmp = inherit_;
          break;
      
    }
  } else {
    tmp = opt[0];
  }
  return /* tuple */[
          "background-repeat",
          tmp
        ];
}

function backgroundSize(opt) {
  var tmp;
  if (typeof opt === "number") {
    switch (opt) {
      case /* Auto */0 :
          tmp = auto;
          break;
      case /* Cover */1 :
          tmp = "cover";
          break;
      case /* Contain */2 :
          tmp = "contain";
          break;
      case /* Initial */3 :
          tmp = initial;
          break;
      case /* Inherit */4 :
          tmp = inherit_;
          break;
      
    }
  } else {
    switch (opt.tag | 0) {
      case /* Length */0 :
          tmp = "" + (String(opt[0]) + (" " + (String(opt[1]) + "")));
          break;
      case /* LengthX */1 :
          tmp = "" + (String(opt[0]) + "");
          break;
      case /* LengthY */2 :
          tmp = "auto " + (String(opt[0]) + "");
          break;
      case /* Unsafe_set */3 :
          tmp = opt[0];
          break;
      
    }
  }
  return /* tuple */[
          "background-size",
          tmp
        ];
}

var Background = {
  backgroundAttachment: backgroundAttachment,
  backgroundBlendMode: backgroundBlendMode,
  _backgroundBoxing: _backgroundBoxing,
  backgroundClip: backgroundClip,
  backgroundColor: backgroundColor,
  backgroundImage: backgroundImage,
  backgroundOrigin: backgroundOrigin,
  backgroundPosition: backgroundPosition,
  backgroundRepeat: backgroundRepeat,
  backgroundSize: backgroundSize
};

var $$class = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class);

var background = CamlinternalOO.create_object_opt(undefined, $$class);

var $$class$1 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$1);

var border = CamlinternalOO.create_object_opt(undefined, $$class$1);

var $$class$2 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$2);

var borderBottom = CamlinternalOO.create_object_opt(undefined, $$class$2);

var $$class$3 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$3);

var borderBottomColor = CamlinternalOO.create_object_opt(undefined, $$class$3);

var $$class$4 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$4);

var borderBottomLeftRadius = CamlinternalOO.create_object_opt(undefined, $$class$4);

var $$class$5 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$5);

var borderBottomRightRadius = CamlinternalOO.create_object_opt(undefined, $$class$5);

var $$class$6 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$6);

var borderBottomStyle = CamlinternalOO.create_object_opt(undefined, $$class$6);

var $$class$7 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$7);

var borderBottomWidth = CamlinternalOO.create_object_opt(undefined, $$class$7);

var $$class$8 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$8);

var borderCollapse = CamlinternalOO.create_object_opt(undefined, $$class$8);

var $$class$9 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$9);

var borderColor = CamlinternalOO.create_object_opt(undefined, $$class$9);

var $$class$10 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$10);

var borderImage = CamlinternalOO.create_object_opt(undefined, $$class$10);

var $$class$11 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$11);

var borderImageOutset = CamlinternalOO.create_object_opt(undefined, $$class$11);

var $$class$12 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$12);

var borderImageRepeat = CamlinternalOO.create_object_opt(undefined, $$class$12);

var $$class$13 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$13);

var borderImageSlice = CamlinternalOO.create_object_opt(undefined, $$class$13);

var $$class$14 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$14);

var borderImageSource = CamlinternalOO.create_object_opt(undefined, $$class$14);

var $$class$15 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$15);

var borderImageWidth = CamlinternalOO.create_object_opt(undefined, $$class$15);

var $$class$16 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$16);

var borderLeft = CamlinternalOO.create_object_opt(undefined, $$class$16);

var $$class$17 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$17);

var borderLeftColor = CamlinternalOO.create_object_opt(undefined, $$class$17);

var $$class$18 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$18);

var borderLeftStyle = CamlinternalOO.create_object_opt(undefined, $$class$18);

var $$class$19 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$19);

var borderLeftWidth = CamlinternalOO.create_object_opt(undefined, $$class$19);

var $$class$20 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$20);

var borderRadius = CamlinternalOO.create_object_opt(undefined, $$class$20);

var $$class$21 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$21);

var borderRight = CamlinternalOO.create_object_opt(undefined, $$class$21);

var $$class$22 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$22);

var borderRightColor = CamlinternalOO.create_object_opt(undefined, $$class$22);

var $$class$23 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$23);

var borderRightStyle = CamlinternalOO.create_object_opt(undefined, $$class$23);

var $$class$24 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$24);

var borderRightWidth = CamlinternalOO.create_object_opt(undefined, $$class$24);

var $$class$25 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$25);

var borderSpacing = CamlinternalOO.create_object_opt(undefined, $$class$25);

var $$class$26 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$26);

var borderStyle = CamlinternalOO.create_object_opt(undefined, $$class$26);

var $$class$27 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$27);

var borderTop = CamlinternalOO.create_object_opt(undefined, $$class$27);

var $$class$28 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$28);

var borderTopColor = CamlinternalOO.create_object_opt(undefined, $$class$28);

var $$class$29 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$29);

var borderTopLeftRadius = CamlinternalOO.create_object_opt(undefined, $$class$29);

var $$class$30 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$30);

var borderTopRightRadius = CamlinternalOO.create_object_opt(undefined, $$class$30);

var $$class$31 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$31);

var borderTopStyle = CamlinternalOO.create_object_opt(undefined, $$class$31);

var $$class$32 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$32);

var borderTopWidth = CamlinternalOO.create_object_opt(undefined, $$class$32);

var $$class$33 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$33);

var borderWidth = CamlinternalOO.create_object_opt(undefined, $$class$33);

var $$class$34 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$34);

var bottom = CamlinternalOO.create_object_opt(undefined, $$class$34);

var $$class$35 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$35);

var boxDecorationBreak = CamlinternalOO.create_object_opt(undefined, $$class$35);

var $$class$36 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$36);

var boxShadow = CamlinternalOO.create_object_opt(undefined, $$class$36);

var $$class$37 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$37);

var boxSizing = CamlinternalOO.create_object_opt(undefined, $$class$37);

var $$class$38 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$38);

var breakAfter = CamlinternalOO.create_object_opt(undefined, $$class$38);

var $$class$39 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$39);

var breakBefore = CamlinternalOO.create_object_opt(undefined, $$class$39);

var $$class$40 = CamlinternalOO.create_table(0);

CamlinternalOO.init_class($$class$40);

var breakInside = CamlinternalOO.create_object_opt(undefined, $$class$40);

var hidden = "hidden";

var flex = "flex";

var column = "column";

var flexEnd = "flexEnd";

var forwards = "forwards";

var center = "center";

var underline = "underline";

export {
  clsx ,
  toFloat ,
  toInt ,
  toStr ,
  vw ,
  vh ,
  px ,
  pct ,
  deg ,
  ms ,
  width ,
  height ,
  overflow ,
  display ,
  flexDirection ,
  padding ,
  transition ,
  transform ,
  zIndex ,
  margin ,
  opacity ,
  textAlign ,
  lineHeight ,
  fontSize ,
  textDecoration ,
  color ,
  nthChild ,
  $$var ,
  hover ,
  hidden ,
  flex ,
  column ,
  flexEnd ,
  rotate ,
  translateY ,
  translate ,
  important ,
  forwards ,
  center ,
  underline ,
  keyframes ,
  css ,
  create ,
  getClassName ,
  useStyles ,
  makeGlobalStyles ,
  makeStyles ,
  makeThemeStyles ,
  auto ,
  initial ,
  inherit_ ,
  none ,
  unset ,
  TimingFunctions ,
  Align ,
  All ,
  Animation ,
  alignContent ,
  alignItems ,
  alignSelf ,
  all ,
  animation ,
  animationDelay ,
  animationDirection ,
  animationDuration ,
  animationFillMode ,
  animationIterationCount ,
  animationName ,
  animationPlayState ,
  animationTimingFunction ,
  Backface ,
  Background ,
  backfaceVisibilty ,
  background ,
  backgroundAttachment ,
  backgroundBlendMode ,
  backgroundClip ,
  backgroundColor ,
  backgroundImage ,
  backgroundOrigin ,
  backgroundPosition ,
  backgroundRepeat ,
  backgroundSize ,
  border ,
  borderBottom ,
  borderBottomColor ,
  borderBottomLeftRadius ,
  borderBottomRightRadius ,
  borderBottomStyle ,
  borderBottomWidth ,
  borderCollapse ,
  borderColor ,
  borderImage ,
  borderImageOutset ,
  borderImageRepeat ,
  borderImageSlice ,
  borderImageSource ,
  borderImageWidth ,
  borderLeft ,
  borderLeftColor ,
  borderLeftStyle ,
  borderLeftWidth ,
  borderRadius ,
  borderRight ,
  borderRightColor ,
  borderRightStyle ,
  borderRightWidth ,
  borderSpacing ,
  borderStyle ,
  borderTop ,
  borderTopColor ,
  borderTopLeftRadius ,
  borderTopRightRadius ,
  borderTopStyle ,
  borderTopWidth ,
  borderWidth ,
  bottom ,
  boxDecorationBreak ,
  boxShadow ,
  boxSizing ,
  breakAfter ,
  breakBefore ,
  breakInside ,
  
}
/* class Not a pure module */