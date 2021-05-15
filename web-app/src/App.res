@react.component
let make = () => {
  let url = RescriptReactRouter.useUrl()
  <>
    <Header />
    {switch url.path {
    | list{"jobs"} => <JobsView />
    | list{} => <GridView />
    | _ => React.null
    }}
  </>
}
