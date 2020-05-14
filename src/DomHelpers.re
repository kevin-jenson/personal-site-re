type classList = {
  add: string => unit,
  remove: string => unit,
};

type style = {
  mutable zIndex: int,
  mutable opacity: int,
};

type parentNode = {
	style
};

type node = {
  classList,
  style,
	parentNode
};

[@bs.get] external getChildeNodes: Dom.element => array(node) = "childNodes";

let iterChildren = (mapper, node) => {
  getChildeNodes(node)
  |> Array.iter(node => {
       mapper(node);
       ();
     });
};

let useDomRef = (ref: ReactDOMRe.Ref.currentDomRef) => {
  ref.current->Js.Nullable.toOption->Belt.Option.getExn;
};

external elementToNode: Dom.element => node = "%identity";