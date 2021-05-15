open String_utils

@react.component
let make = (~outcome: string, ~err: option<string>) => {
  switch outcome {
  | "Passed" => `✔`->React.string
  | "Failed" => <> {`Failed ✗`->React.string} <br /> {err->string_opt} </>
  | "Aborted" => <> {"Aborted"->React.string} <br /> {err->string_opt} </>
  | "NotStarted" => "Not started"->React.string
  | "Active" => "Active"->React.string
  | "Undefined" => "Undefined"->React.string
  | "" => React.string("-")
  | _ => "Unknown"->React.string
  }
}
