open Glamor;

let str = ReasonReact.stringToElement;

type item = {
  id: int,
  completed: bool,
  text: string
};

module TodoItem = {
  let itemStyle =
    css([margin("0.5rem 0 0 1.5rem"), display("flex"), alignItems("center")]);
  let component = ReasonReact.statelessComponent("TodoItem");
  let make = (~item: item, ~onToggle, _children) => {
    ...component,
    render: _self =>
      <div className=itemStyle onClick=(_evt => onToggle())>
        <input
          className=(css([marginRight("10px")]))
          _type="checkbox"
          checked=(Js.Boolean.to_js_boolean(item.completed))
        />
        <div> (str(item.text)) </div>
      </div>
  };
};

let valueFromEvent = evt : string => (
                                       evt
                                       |> ReactEventRe.Form.target
                                       |> ReactDOMRe.domElementToObj
                                     )##value;

module Input = {
  type state = string;
  let inputStyle =
    css([width("18rem"), height("1.5rem"), marginBottom("0.5rem")]);
  let component = ReasonReact.reducerComponent("Input");
  let make = (~onSubmit, _children) => {
    ...component,
    initialState: () => "",
    reducer: (newText, _text) => ReasonReact.Update(newText),
    render: ({state: text, reduce}) =>
      <input
        className=inputStyle
        value=text
        _type="text"
        placeholder="What do you wanna do?"
        onChange=(reduce(evt => valueFromEvent(evt)))
        onKeyDown=(
          evt =>
            if (ReactEventRe.Keyboard.key(evt) == "Enter") {
              onSubmit(text);
              (reduce(() => ""))();
            }
        )
      />
  };
};

let paper =
  css([
    width("25rem"),
    height("28rem"),
    overflow("auto"),
    boxShadow("5px 3px 3px rgba(0,0,0,0.15)"),
    marginTop("1rem"),
    border("1px solid #eee"),
    display("flex"),
    flexDirection("column"),
    alignItems("center"),
    position("relative")
  ]);

let todolist =
  css([
    width("20rem"),
    height("20rem"),
    display("flex"),
    flexWrap("wrap"),
    flexDirection("column")
  ]);

let footer = css([position("absolute"), bottom("1rem"), left("1rem")]);

type state = {items: list(item)};

type action =
  | AddItem(string)
  | ToggleItem(int);

let component = ReasonReact.reducerComponent("App");

let lastId = ref(0);

let newItem = text => {
  lastId := lastId^ + 1;
  {id: lastId^, completed: false, text};
};

let toggledItem = (id, items) =>
  List.map(
    item => item.id === id ? {...item, completed: ! item.completed} : item,
    items
  );

let make = _children => {
  ...component,
  initialState: () => {
    items: [{id: 0, completed: false, text: "Do something."}]
  },
  reducer: (action, {items}) =>
    switch action {
    | AddItem(text) => ReasonReact.Update({items: [newItem(text), ...items]})
    | ToggleItem(id) => ReasonReact.Update({items: toggledItem(id, items)})
    },
  render: ({state: {items}, reduce}) => {
    let nItems = List.length(items);
    let summary =
      nItems > 1 ?
        string_of_int(nItems) ++ " items" : string_of_int(nItems) ++ " item";
    <div className=paper>
      <h2> (str("TODO List")) </h2>
      <Input onSubmit=(reduce(text => AddItem(text))) />
      <div className=todolist>
        (
          List.map(
            item =>
              <TodoItem
                key=(string_of_int(item.id))
                item
                onToggle=(reduce(() => ToggleItem(item.id)))
              />,
            items
          )
          |> Array.of_list
          |> ReasonReact.arrayToElement
        )
      </div>
      <div className=footer> (str(summary)) </div>
    </div>;
  }
};