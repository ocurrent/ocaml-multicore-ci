open String_utils

@react.component
let make = (~outcome: string, ~err: option<string>) => {
  let filtered_err = switch err {
  | Some("Docker build exited with status 1") => React.null
  | _ => <> <br /> {err->string_opt} </>
  }
  switch outcome {
  | "Passed" => `✔`->React.string
  | "Failed" => <> {`Failed ✗`->React.string} {filtered_err} </>
  | "Aborted" => <> {"Aborted"->React.string} {filtered_err} </>
  | "NotStarted" => "Not started"->React.string
  | "Active" => "Active"->React.string
  | "Undefined" => "Undefined"->React.string
  | "" => ""->React.string
  | _ => "Unknown"->React.string
  }
}
