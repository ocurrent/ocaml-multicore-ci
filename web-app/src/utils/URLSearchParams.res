open Belt

type t

@new external make: string => t = "URLSearchParams"
@send external delete: (t, string) => unit = "delete"
@send external get: (t, string) => Js.Nullable.t<string> = "get"
@send external set: (t, string, string) => unit = "set"
@send external toString: t => string = "toString"

let getWithDefault = (x: t, k: string, def: string) =>
  x->get(k)->Js.Nullable.toOption->Option.getWithDefault(def)

let getBoolean = (x: t, k: string) => {
  let v = x->get(k)->Js.Nullable.toOption->Option.getWithDefault("")
  v != "" && v != "0" && v != "false"
}

let setBoolean = (x: t, k: string, v: bool) => {
  if v {
    x->set(k, "1")
  } else {
    x->delete(k)
  }
}

let toStringQuestionMark = (x: t): string => {
  let v = x->toString
  v == "" ? "" : `?${v}`
}
