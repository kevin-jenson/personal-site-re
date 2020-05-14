// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";
import * as MakeStyles from "../MakeStyles.js";
import * as Belt_Option from "bs-platform/lib/es6/belt_Option.js";
import * as Caml_option from "bs-platform/lib/es6/caml_option.js";
import * as Core from "@material-ui/core";

function styles(theme) {
  var transitions = theme.transitions;
  var createTransitionOptions = function (prim, prim$1, prim$2, prim$3) {
    var tmp = { };
    if (prim !== undefined) {
      tmp.duration = Caml_option.valFromOption(prim);
    }
    if (prim$1 !== undefined) {
      tmp.easing = Caml_option.valFromOption(prim$1);
    }
    if (prim$2 !== undefined) {
      tmp.delay = Caml_option.valFromOption(prim$2);
    }
    return tmp;
  };
  var hamMenu = MakeStyles.style(/* :: */[
        MakeStyles.zIndex(theme.zIndex.appBar),
        /* [] */0
      ]);
  var hamLine = function (colorMode) {
    return MakeStyles.style(/* :: */[
                MakeStyles.backgroundColor(colorMode ? theme.background.light : theme.background.dark),
                /* :: */[
                  MakeStyles.width(MakeStyles.px(50)),
                  /* :: */[
                    MakeStyles.height(MakeStyles.px(2)),
                    /* :: */[
                      MakeStyles.transition(Curry._2(transitions.create, [
                                "transform",
                                "background-color"
                              ], createTransitionOptions(transitions.duration.short, undefined, undefined, undefined))),
                      /* :: */[
                        MakeStyles.nthChild(2, MakeStyles.style(/* :: */[
                                  MakeStyles.margin(/* :: */[
                                        MakeStyles.px((theme.spacer << 1)),
                                        /* :: */[
                                          MakeStyles.px(0),
                                          /* [] */0
                                        ]
                                      ]),
                                  /* [] */0
                                ])),
                        /* :: */[
                          MakeStyles.animationDuration(MakeStyles.ms(transitions.duration.shortest)),
                          /* :: */[
                            MakeStyles.animationFillMode(MakeStyles.forwards),
                            /* :: */[
                              MakeStyles.opacity(0.0),
                              /* :: */[
                                MakeStyles.$$var("light", MakeStyles.style(/* :: */[
                                          MakeStyles.animationName("hamFadeInLight"),
                                          /* [] */0
                                        ])),
                                /* :: */[
                                  MakeStyles.$$var("dark", MakeStyles.style(/* :: */[
                                            MakeStyles.animationName("hamFadeInDark"),
                                            /* [] */0
                                          ])),
                                  /* [] */0
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]);
  };
  var hamFadeInLight = MakeStyles.keyframes("hamFadeInLight", /* :: */[
        /* tuple */[
          0,
          MakeStyles.style(/* :: */[
                MakeStyles.opacity(0.0),
                /* :: */[
                  MakeStyles.backgroundColor(theme.colors.black),
                  /* [] */0
                ]
              ])
        ],
        /* :: */[
          /* tuple */[
            90,
            MakeStyles.style(/* :: */[
                  MakeStyles.opacity(0.5),
                  /* :: */[
                    MakeStyles.backgroundColor(theme.colors.pink),
                    /* [] */0
                  ]
                ])
          ],
          /* :: */[
            /* tuple */[
              100,
              MakeStyles.style(/* :: */[
                    MakeStyles.opacity(1.0),
                    /* :: */[
                      MakeStyles.backgroundColor(theme.colors.black),
                      /* [] */0
                    ]
                  ])
            ],
            /* [] */0
          ]
        ]
      ]);
  var hamFadeInDark = MakeStyles.keyframes("hamFadeInDark", /* :: */[
        /* tuple */[
          0,
          MakeStyles.style(/* :: */[
                MakeStyles.opacity(0.0),
                /* :: */[
                  MakeStyles.backgroundColor(theme.colors.white),
                  /* [] */0
                ]
              ])
        ],
        /* :: */[
          /* tuple */[
            90,
            MakeStyles.style(/* :: */[
                  MakeStyles.opacity(0.5),
                  /* :: */[
                    MakeStyles.backgroundColor(theme.colors.pink),
                    /* [] */0
                  ]
                ])
          ],
          /* :: */[
            /* tuple */[
              100,
              MakeStyles.style(/* :: */[
                    MakeStyles.opacity(1.0),
                    /* :: */[
                      MakeStyles.backgroundColor(theme.colors.white),
                      /* [] */0
                    ]
                  ])
            ],
            /* [] */0
          ]
        ]
      ]);
  return MakeStyles.create(/* :: */[
              /* tuple */[
                "hamMenu",
                hamMenu
              ],
              /* :: */[
                /* tuple */[
                  "hamLine",
                  hamLine
                ],
                /* :: */[
                  /* tuple */[
                    "light",
                    MakeStyles.style(/* [] */0)
                  ],
                  /* :: */[
                    /* tuple */[
                      "dark",
                      MakeStyles.style(/* [] */0)
                    ],
                    /* :: */[
                      hamFadeInLight,
                      /* :: */[
                        hamFadeInDark,
                        /* [] */0
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

var useStyles = MakeStyles.makeThemeStyles(styles);

var make = React.forwardRef((function (Props, ref) {
        var links = Props.links;
        var onHoverOpt = Props.onHover;
        var onClickOpt = Props.onClick;
        var colorMode = Props.colorMode;
        var className = Props.className;
        var onHover = onHoverOpt !== undefined ? onHoverOpt : (function (_e) {
              
            });
        var onClick = onClickOpt !== undefined ? onClickOpt : (function (_e) {
              
            });
        var theme = Core.useTheme();
        var classes = Curry._1(useStyles, Caml_option.some(undefined));
        var mode = colorMode ? "dark" : "light";
        var ref$1 = Belt_Option.map((ref == null) ? undefined : Caml_option.some(ref), (function (prim) {
                return prim;
              }));
        var tmp = {
          className: MakeStyles.clsx(/* :: */[
                Curry._1(classes, "hamMenu"),
                /* :: */[
                  className,
                  /* [] */0
                ]
              ]),
          role: "navigation",
          onClick: onClick,
          onMouseEnter: onHover
        };
        if (ref$1 !== undefined) {
          tmp.ref = Caml_option.valFromOption(ref$1);
        }
        return React.createElement("div", tmp, $$Array.of_list(List.mapi((function (index, link) {
                              return React.createElement("div", {
                                          key: link,
                                          className: MakeStyles.clsx(/* :: */[
                                                Curry._1(classes, "hamLine"),
                                                /* :: */[
                                                  Curry._1(classes, mode),
                                                  /* [] */0
                                                ]
                                              ]),
                                          style: {
                                            animationDelay: MakeStyles.ms(Caml_int32.imul(theme.transitions.duration.standard, index))
                                          }
                                        });
                            }), links)));
      }));

var HamMenu = {
  styles: styles,
  useStyles: useStyles,
  make: make
};

function styles$1(theme) {
  var transitions = theme.transitions;
  var createTransitionOptions = function (prim, prim$1, prim$2, prim$3) {
    var tmp = { };
    if (prim !== undefined) {
      tmp.duration = Caml_option.valFromOption(prim);
    }
    if (prim$1 !== undefined) {
      tmp.easing = Caml_option.valFromOption(prim$1);
    }
    if (prim$2 !== undefined) {
      tmp.delay = Caml_option.valFromOption(prim$2);
    }
    return tmp;
  };
  var header = MakeStyles.style(/* :: */[
        MakeStyles.width(MakeStyles.pct(100)),
        /* :: */[
          MakeStyles.display(MakeStyles.flex),
          /* :: */[
            MakeStyles.flexDirection(MakeStyles.column),
            /* :: */[
              MakeStyles.alignItems(MakeStyles.flexEnd),
              /* :: */[
                MakeStyles.padding(/* :: */[
                      MakeStyles.px((theme.spacer << 3)),
                      /* :: */[
                        MakeStyles.px(Caml_int32.imul(theme.spacer, 6)),
                        /* [] */0
                      ]
                    ]),
                /* :: */[
                  MakeStyles.overflow(MakeStyles.hidden),
                  /* [] */0
                ]
              ]
            ]
          ]
        ]
      ]);
  var hamMenu = MakeStyles.style(/* :: */[
        MakeStyles.transition(Curry._2(transitions.create, ["transform"], createTransitionOptions(transitions.duration.shortest, undefined, undefined, undefined))),
        /* [] */0
      ]);
  var hamHovered = MakeStyles.style(/* :: */[
        MakeStyles.transform(MakeStyles.rotate(MakeStyles.deg(90))),
        /* [] */0
      ]);
  var hamLineHovered = MakeStyles.style(/* :: */[
        MakeStyles.backgroundColor(MakeStyles.important(theme.colors.pink)),
        /* :: */[
          MakeStyles.nthChild(2, MakeStyles.style(/* :: */[
                    MakeStyles.transform(MakeStyles.translateY(MakeStyles.px(MakeStyles.toInt(MakeStyles.toFloat(theme.spacer) * 16.25)))),
                    /* [] */0
                  ])),
          /* :: */[
            MakeStyles.nthChild(3, MakeStyles.style(/* :: */[
                      MakeStyles.transform(MakeStyles.translateY(MakeStyles.px(MakeStyles.toInt(MakeStyles.toFloat(theme.spacer) * 32.5)))),
                      /* [] */0
                    ])),
            /* [] */0
          ]
        ]
      ]);
  var headerLinks = MakeStyles.style(/* :: */[
        MakeStyles.display(MakeStyles.flex),
        /* :: */[
          MakeStyles.transform(MakeStyles.translate(MakeStyles.px(450), MakeStyles.px(0))),
          /* :: */[
            MakeStyles.transition(Curry._2(transitions.create, ["transform"], createTransitionOptions(transitions.duration.enteringScreen, undefined, undefined, undefined))),
            /* [] */0
          ]
        ]
      ]);
  var headerLinksHover = MakeStyles.style(/* :: */[
        MakeStyles.transform(MakeStyles.translate(MakeStyles.px(0), MakeStyles.px(0))),
        /* [] */0
      ]);
  return MakeStyles.create(/* :: */[
              /* tuple */[
                "header",
                header
              ],
              /* :: */[
                /* tuple */[
                  "hamMenu",
                  hamMenu
                ],
                /* :: */[
                  /* tuple */[
                    "hamHovered",
                    hamHovered
                  ],
                  /* :: */[
                    /* tuple */[
                      "hamLineHovered",
                      hamLineHovered
                    ],
                    /* :: */[
                      /* tuple */[
                        "headerLinks",
                        headerLinks
                      ],
                      /* :: */[
                        /* tuple */[
                          "headerLinksHover",
                          headerLinksHover
                        ],
                        /* [] */0
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

var useHeaderStyles = MakeStyles.makeThemeStyles(styles$1);

var links = /* :: */[
  "about",
  /* :: */[
    "blog",
    /* :: */[
      "contact",
      /* [] */0
    ]
  ]
];

function Navigation$DesktopHeader(Props) {
  var colorMode = Props.colorMode;
  var classes = Curry._1(useHeaderStyles, Caml_option.some(undefined));
  var theme = Core.useTheme();
  var hamRef = React.useRef(null);
  React.useRef(0.0);
  var handleMouseEnter = function (_event) {
    Belt_Option.map(Caml_option.nullable_to_opt(hamRef.current), (function (ref) {
            return setTimeout((function (param) {
                          return $$Array.iter((function (child) {
                                        Curry._1(child.classList.add, Curry._1(classes, "hamLineHoverd"));
                                        
                                      }), ref.childNodes);
                        }), theme.transitions.duration.shortest);
          }));
    
  };
  return React.createElement("div", {
              className: Curry._1(classes, "header")
            }, React.createElement(make, {
                  links: links,
                  onHover: handleMouseEnter,
                  colorMode: colorMode,
                  className: Curry._1(classes, "hamMenu"),
                  ref: hamRef
                }));
}

var DesktopHeader = {
  styles: styles$1,
  useHeaderStyles: useHeaderStyles,
  links: links,
  make: Navigation$DesktopHeader
};

export {
  HamMenu ,
  DesktopHeader ,
  
}
/* useStyles Not a pure module */
