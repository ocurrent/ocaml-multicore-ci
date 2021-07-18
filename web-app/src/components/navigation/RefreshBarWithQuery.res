open ReScriptUrql

let useWithQuery = query => {
  let (epoch, setEpoch) = React.useState(_ => 1)
  let (lastRefreshed, setLastRefreshed) = React.useState(_ => Day.now())
  let ({Hooks.response: response}, executeQuery) = Hooks.useQuery(
    ~query,
    ~requestPolicy=#CacheFirst,
    (),
  )

  let doRefresh = () => {
    executeQuery(~requestPolicy=#CacheAndNetwork, ())
    setLastRefreshed(_ => Day.now())
  }

  let onRefresh = ev => {
    ev->ReactEvent.Synthetic.stopPropagation
    ev->ReactEvent.Synthetic.preventDefault
    doRefresh()
  }

  Interval_utils.useInterval(_ => {
    setEpoch(epoch => {
      if mod(epoch, 10) == 0 {
        doRefresh()
      }
      epoch + 1
    })
  }, 30000)

  (response, <RefreshBar key={epoch->Js.Int.toString} lastRefreshed onRefresh />)
}
