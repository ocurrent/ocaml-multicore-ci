open Belt
open ReScriptUrql

module GetAdminServiceUri = %graphql(`
{ config { admin_service_uri } }
`)

let useConfig = () => {
  let ({Hooks.response: response}, _) = Hooks.useQuery(
    ~query=module(GetAdminServiceUri),
    ~requestPolicy=#CacheFirst,
    (),
  )

  switch response {
  | Fetching
  | Error(_)
  | Empty =>
    None
  | Data(data)
  | PartialData(data, _) =>
    Some(data.config)
  }
}

let useAdminServiceUri = () => {
  let config = useConfig()
  config->Option.mapWithDefault("", config => config.admin_service_uri)
}
