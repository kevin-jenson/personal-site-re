module Theme = {
  module CustomTheme = {
    type babyYodaColors = {
      green: string,
      brown: string,
      lightBrown: string,
    }
    and colors = {
      black: string,
      white: string,
      gray: string,
      lightPink: string,
      pink: string,
      babyYoda: babyYodaColors,
    };

    type background = {
      dark: string,
      light: string,
    };

    type typeStyle = {
      fontSize: int,
      fontWeight: int,
    }
    and typography = {h1: typeStyle};

    type t = {
      colors,
      background,
      spacer: int,
      typography,
    };

    let colors = {
      black: "#0A100D",
      white: "#F2F4F3",
      gray: "#40434E",
      lightPink: "#A39BA8",
      pink: "#C297B8",
      babyYoda: {
        green: "#B1CDAF",
        brown: "#9B8B7B",
        lightBrown: "#CECAAC",
      },
    };

    let theme = {
      colors,
      background: {
        dark: colors.black,
        light: colors.white,
      },
      spacer: 8,
      typography: {
        h1: {
          fontSize: 36,
          fontWeight: 700,
        },
      },
    };
  };

  type breakpointValues = {
    xs: int,
    sm: int,
    md: int,
    lg: int,
    xl: int,
  }
  and breakpoints = {
    keys: list(string),
    values: breakpointValues,
    up: string => string,
    down: string => string,
    between: (string, string) => string,
    only: string => string,
    width: string => string,
  };

  [@bs.deriving abstract]
  type transitionOptions = {
    [@bs.optional]
    duration: int,
    [@bs.optional]
    easing: int,
    [@bs.optional]
    delay: int,
  };

  type createTransition = (array(string), transitionOptions) => string;

  type easing = {
    easeInOut: string,
    easeOut: string,
    easeIn: string,
    sharp: string,
  }
  and duration = {
    shortest: int,
    shorter: int,
    short: int,
    standard: int,
    complex: int,
    enteringScreen: int,
    leavingScreen: int,
  }
  and transitions = {
    easing,
    duration,
    create: createTransition,
  };

  type zIndex = {
    appBar: int
  };

  type t = {
    breakpoints,
    direction: string,
    transitions,
    colors: CustomTheme.colors,
    background: CustomTheme.background,
    spacer: int,
    typography: CustomTheme.typography,
    zIndex
  };

  [@bs.module "@material-ui/core"] external createMuiTheme: CustomTheme.t => t = "createMuiTheme";

  let theme = createMuiTheme(CustomTheme.theme);

  [@bs.module "@material-ui/core"] external useTheme: unit => t = "useTheme";
};

module CssBaseline = {
  [@bs.module "@material-ui/core"] [@react.component]
  external make: (~children: React.element) => React.element = "CssBaseline";
};

module MuiThemeProvider = {
  [@bs.module "@material-ui/core"] [@react.component]
  external make: (~children: React.element, ~theme: Theme.t) => React.element = "ThemeProvider";
};