module HamMenu = {
  let styles = (theme: Mui.Theme.t) => {
    let transitions = theme.transitions;
    let createTransitionOptions = Mui.Theme.transitionOptions;
    open MakeStyles;
    let hamMenu = css([zIndex(theme.zIndex.appBar)]);

    let hamLine = (colorMode: Layout.colorMode) =>
      css([
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
        nthChild(2, css([margin([px(theme.spacer * 2), px(0)])])),
        animationDuration(Time(ms(transitions.duration.shortest))),
        animationFillMode(Forwards),
        opacity(0.0),
        var("light", css([animationName(Name("hamFadeInLight"))])),
        var("dark", css([animationName(Name("hamFadeInDark"))])),
      ]);

    let hamFadeInLight =
      keyframes(
        "hamFadeInLight",
        [
          (0, css([opacity(0.0), backgroundColor(theme.colors.black)])),
          (90, css([opacity(0.5), backgroundColor(theme.colors.pink)])),
          (100, css([opacity(1.0), backgroundColor(theme.colors.black)])),
        ],
      );

    let hamFadeInDark =
      keyframes(
        "hamFadeInDark",
        [
          (0, css([opacity(0.0), backgroundColor(theme.colors.white)])),
          (90, css([opacity(0.5), backgroundColor(theme.colors.pink)])),
          (100, css([opacity(1.0), backgroundColor(theme.colors.white)])),
        ],
      );

    create([
      ("hamMenu", hamMenu),
      ("hamLine", hamLine),
      ("light", css([])),
      ("dark", css([])),
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

module HeaderLink = {
  let styles = (theme: Mui.Theme.t) => {
    let transitions = theme.transitions;
    open MakeStyles;

    let link = (colorMode: Layout.colorMode) =>
      css([
        height(px(50)),
        width(px(toFloat(theme.spacer) *. 36.5 |> toInt)),
        opacity(0.0),
        textAlign(center),
        lineHeight(px(50)),
        fontSize(px(24)),
        textDecoration(none),
        color(
          switch (colorMode) {
          | Light => theme.background.dark
          | Dark => theme.background.light
          },
        ),
        transition(
          transitions.create(
            [|"opacity"|],
            Mui.Theme.transitionOptions(
              ~duration=transitions.duration.complex + transitions.duration.shortest,
              ~delay=transitions.duration.shorter,
              (),
            ),
          ),
        ),
        hover(css([textDecoration(underline)])),
      ]);

    create([("link", link)]);
  };

  let useStyles = MakeStyles.makeThemeStyles(styles);

  [@react.component]
  let make = () => {
    let classes = useStyles(~props=());
    ();
  };
};

module DesktopHeader = {
  let styles = (theme: Mui.Theme.t) => {
    let transitions = theme.transitions;
    let createTransitionOptions = Mui.Theme.transitionOptions;
    open MakeStyles;

    let header =
      css([
        width(pct(100)),
        display(flex),
        flexDirection(column),
        alignItems(FlexEnd),
        padding([px(theme.spacer * 8), px(theme.spacer * 6)]),
        overflow(hidden),
      ]);

    let hamMenu =
      css([
        transition(
          transitions.create([|"transform"|], createTransitionOptions(~duration=transitions.duration.shortest, ())),
        ),
      ]);

    let hamHovered = css([transform(rotate(deg(90)))]);

    let hamLineHovered =
      css([
        backgroundColor(important(theme.colors.pink)),
        nthChild(2, css([transform(translateY(px(toFloat(theme.spacer) *. 16.25 |> toInt)))])),
        nthChild(3, css([transform(translateY(px(toFloat(theme.spacer) *. 32.5 |> toInt)))])),
      ]);

    let headerLinks =
      css([
        display(flex),
        transform(translate(px(450), px(0))),
        transition(
          transitions.create(
            [|"transform"|],
            createTransitionOptions(~duration=transitions.duration.enteringScreen, ()),
          ),
        ),
      ]);

    let headerLinksHover = css([transform(translate(px(0), px(0)))]);

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
    let linksRef = React.useRef(Js.Nullable.null);
    let leaveTimeout = React.useRef(0.0);
    let enterTimeout = React.useRef(0.0);

    let handleMouseEnter = _event => {
      if (leaveTimeout.current != 0.0) {
        clearTimeout(leaveTimeout.current);
      };
      let hamMenu = DomHelpers.useDomRef(hamRef);
      let linkMenu = DomHelpers.useDomRef(linksRef);

      DomHelpers.elementToNode(hamMenu).classList.add(classes("hamHovered"));

      enterTimeout.current =
        setTimeout(
          () => {
            hamMenu |> DomHelpers.iterChildren(child => {child.classList.add(classes("hamLineHoverd"))});
            DomHelpers.elementToNode(linkMenu).classList.add(classes("headLinksHover"));
            DomHelpers.elementToNode(linkMenu).parentNode.style.zIndex = 0;
            linkMenu |> DomHelpers.iterChildren(child => {child.style.opacity = 1});
          },
          theme.transitions.duration.shortest,
        );
      ();
    };

    let handleMouseLeave = _event => {
      if (enterTimeout.current != 0.0) {
        clearTimeout(0.0);
      };
      let hamMenu = DomHelpers.useDomRef(hamRef);
      let linkMenu = DomHelpers.useDomRef(linksRef);

      hamMenu |> DomHelpers.iterChildren(child => {child.classList.remove(classes("hamLineHovered"))});
      linkMenu |> DomHelpers.iterChildren(child => {child.style.opacity = 0});
      DomHelpers.elementToNode(linkMenu).classList.remove(classes("headLinksHover"));
      DomHelpers.elementToNode(linkMenu).parentNode.style.zIndex = (-1);
      leaveTimeout.current =
        setTimeout(
          () => {DomHelpers.elementToNode(hamMenu).classList.remove(classes("hamHovered"))},
          theme.transitions.duration.shortest,
        );
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
      <div
        style=MakeStyles.(
          ReactDOMRe.Style.make(~transform=translate(px(-6), px(-44)), ~overflow=hidden, ~zIndex="-1", ())
        )>
        <div
          ref={ReactDOMRe.Ref.domRef(linksRef)}
          role="navigation"
          onMouseLeave=handleMouseLeave
          className={classes("headerLinks")}>
          {List.map(_link => <div />, links) |> Array.of_list |> React.array}
        </div>
      </div>
    </div>;
  };
};