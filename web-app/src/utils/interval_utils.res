open Js.Global
open Js.Nullable

let useInterval = (f: unit => unit, ms: int) => {
  let timeout = React.useRef(null)

  React.useEffect0(() => {
    f()
    timeout.current = setInterval(f, ms)->return

    (() => timeout.current->bind((. i) => i->clearInterval)->ignore)->Some
  })
}
