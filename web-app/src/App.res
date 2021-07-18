let theme = Theme.make()

@react.component
let make = () => {
  Dayjs_utils.init()

  let url = RescriptReactRouter.useUrl()
  <MaterialUi_ThemeProvider theme>
    <Header />
    {switch url.path {
    | list{"github"} => <GitHubView />
    | list{"jobs"} => <JobsView />
    | list{} => <GridView />
    | _ => React.null
    }}
  </MaterialUi_ThemeProvider>
}
