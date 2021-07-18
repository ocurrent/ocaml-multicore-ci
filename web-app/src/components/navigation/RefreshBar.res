open MaterialUi

open Dayjs_utils

@react.component
let make = (~lastRefreshed: Day.dayjs, ~onRefresh: ReactEvent.Mouse.t => unit) => {
  <div className="refreshBar">
    <div> {"Last refreshed: "->React.string} {lastRefreshed->fromNow()->React.string} </div>
    <Button variant=#Outlined startIcon={<IconRefresh />} onClick={onRefresh}>
      {"Refresh"->React.string}
    </Button>
  </div>
}
