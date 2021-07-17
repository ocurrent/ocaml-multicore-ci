open Env
open ReScriptUrql

%%raw("import './assets/css/index.css';")

let client = Client.make(~url=env.graphql_endpoint, ())

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(root) => ReactDOM.render(<Context.Provider value=client> <App /> </Context.Provider>, root)
}
