let string_opt = str =>
  React.string(
    switch str {
    | Some(str) => str
    | None => ""
    },
  )
