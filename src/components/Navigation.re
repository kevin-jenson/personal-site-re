module HamMenu = {
  let styles = (theme: Mui.Theme.t) => {
    let transitions = theme.transitions;
    let createTransitionOptions = Mui.Theme.transitionOptions;
    open MakeStyles;
    let hamMenu = style([zIndex(theme.zIndex.appBar)]);

    let hamLine = (colorMode: Layout.colorMode) =>
      style([
        backgroundColor(
          switch (colorMode) {
          | Light => theme.background.dark
          | Dark => theme.background.light
          },
        ),
        width(px(50)),
        height(px(2)),
        transition(
          transitions.create(
            [|"transform", "background-color"|],
            createTransitionOptions(~duration=transitions.duration.short, ()),
          ),
        ),
        nthChild(2, style([margin([px(theme.spacer * 2), px(0)])])),
        animationDuration(ms(transitions.duration.shortest)),
        animationFillMode(forwards),
        opacity(0.0),
        var("light", style([animationName("hamFadeInLight")])),
        var("dark", style([animationName("hamFadeInDark")])),
      ]);

    let hamFadeInLight =
      keyframes(
        "hamFadeInLight",
        [
          (0, style([opacity(0.0), backgroundColor(theme.colors.black)])),
          (90, style([opacity(0.5), backgroundColor(theme.colors.pink)])),
          (100, style([opacity(1.0), backgroundColor(theme.colors.black)])),
        ],
      );

    let hamFadeInDark =
      keyframes(
        "hamFadeInDark",
        [
          (0, style([opacity(0.0), backgroundColor(theme.colors.white)])),
          (90, style([opacity(0.5), backgroundColor(theme.colors.pink)])),
          (100, style([opacity(1.0), backgroundColor(theme.colors.white)])),
        ],
      );

    create([
      ("hamMenu", hamMenu),
      ("hamLine", hamLine),
      ("light", style([])),
      ("dark", style([])),
      hamFadeInLight,
      hamFadeInDark,
    ]);
  };

  let useStyles = MakeStyles.makeThemeStyles(styles);

  [@react.component]
  let make =
    React.forwardRef((~links, ~onHover=_e => (), ~onClick=_e => (), ~colorMode: Layout.colorMode, ~className, ref) => {
      let theme = Mui.Theme.useTheme();
      let classes = useStyles(~props=());
      let mode =
        switch (colorMode) {
        | Light => "light"
        | Dark => "dark"
        };

      let ref: option(ReactDOMRe.domRef) = ref->Js.Nullable.toOption->Belt.Option.map(ReactDOMRe.Ref.domRef);

      <div
        ?ref
        role="navigation"
        onClick
        onMouseEnter=onHover
        className={MakeStyles.clsx([classes("hamMenu"), className])}>
        {List.mapi(
           (index, link) =>
             <div
               key=link
               className={MakeStyles.clsx([classes("hamLine"), classes(mode)])}
               style={ReactDOMRe.Style.make(
                 ~animationDelay={theme.transitions.duration.standard * index |> MakeStyles.ms},
                 (),
               )}
             />,
           links,
         )
         |> Array.of_list
         |> React.array}
      </div>;
    });
};

module DesktopHeader = {
  let styles = (theme: Mui.Theme.t) => {
    let transitions = theme.transitions;
    let createTransitionOptions = Mui.Theme.transitionOptions;
    open MakeStyles;

    let header =
      style([
        width(pct(100)),
        display(flex),
        flexDirection(column),
        alignItems(flexEnd),
        padding([px(theme.spacer * 8), px(theme.spacer * 6)]),
        overflow(hidden),
      ]);

    let hamMenu =
      style([
        transition(
          transitions.create([|"transform"|], createTransitionOptions(~duration=transitions.duration.shortest, ())),
        ),
      ]);

    let hamHovered = style([transform(rotate(deg(90)))]);

    let hamLineHovered =
      style([
        backgroundColor(important(theme.colors.pink)),
        nthChild(2, style([transform(translateY(px(toFloat(theme.spacer) *. 16.25 |> toInt)))])),
        nthChild(3, style([transform(translateY(px(toFloat(theme.spacer) *. 32.5 |> toInt)))])),
      ]);

    let headerLinks =
      style([
        display(flex),
        transform(translate(px(450), px(0))),
        transition(
          transitions.create(
            [|"transform"|],
            createTransitionOptions(~duration=transitions.duration.enteringScreen, ()),
          ),
        ),
      ]);

    let headerLinksHover = style([transform(translate(px(0), px(0)))]);

    create([
      ("header", header),
      ("hamMenu", hamMenu),
      ("hamHovered", hamHovered),
      ("hamLineHovered", hamLineHovered),
      ("headerLinks", headerLinks),
      ("headerLinksHover", headerLinksHover),
    ]);
  };

  let useHeaderStyles = MakeStyles.makeThemeStyles(styles);

  let links = ["about", "blog", "contact"];

  [@bs.val] external setTimeout: (unit => unit, int) => float = "setTimeout";
  [@bs.val] external clearTimeout: float => unit = "clearTimeout";
  type classList = {
    add: string => unit,
    remove: string => unit,
  };
  type child = {classList};
  [@bs.get] external getChildNodes: Dom.element => array(child) = "childNodes";
  [@react.component]
  let make = (~colorMode) => {
    let classes = useHeaderStyles(~props=());
    let theme = Mui.Theme.useTheme();
    let hamRef = React.useRef(Js.Nullable.null);
    let leaveTimeout = React.useRef(0.0);

    let handleMouseEnter = _event => {
      let int = hamRef.current
      ->Js.Nullable.toOption
      ->Belt.Option.map((ref: Dom.element) => {
          setTimeout(
            () => {
              ref->getChildNodes
              |> Array.iter(child => {
                   child.classList.add(classes("hamLineHoverd"));
                   ();
                 })
            },
            theme.transitions.duration.shortest,
          );
        });
				();
    };

    <div className={classes("header")}>
      <HamMenu
        ref={ReactDOMRe.Ref.domRef(hamRef)}
        onHover=handleMouseEnter
        colorMode
        links
        className={classes("hamMenu")}
      />
    </div>;
  };
};