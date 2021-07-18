module IconRefresh = {
  @react.component @module("@material-ui/icons/Refresh")
  external make: (~color: string=?, ~className: string=?, ~fontSize: string=?) => React.element =
    "default"
}

@react.component
let make = () => {
  <IconRefresh />
}
