@module
external relativeTime: Day.plugin = "dayjs/plugin/relativeTime"
@send
external fromNow: (Day.dayjs, unit) => string = "fromNow"

let init = () => {
  Day.extend(relativeTime)
}
