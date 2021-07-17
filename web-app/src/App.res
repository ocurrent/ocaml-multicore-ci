@react.component
let make = () => {
  let url = RescriptReactRouter.useUrl()
  <>
    <Header />
    {switch url.path {
    | list{"github"} => <GitHubView />
    | list{"jobs"} => <JobsView />
    | list{} => <GridView />
    | _ => React.null
    }}
  </>
}
