open Belt
open MaterialUi

module GetAllJobs = %graphql(`
{ jobs { owner, name, hash, job_id, variant, outcome, error } }
`)
external outcome_to_string: GetAllJobs.t_jobs_outcome => string = "%identity"

module JobRow = {
  @react.component
  let make = (
    ~repo: string,
    ~jobs: HashMap.String.t<GetAllJobs.t_jobs>,
    ~variants: array<string>,
  ) => {
    let adminServiceUri = ConfigHooks.useAdminServiceUri()

    <tr>
      <td> {repo->React.string} </td>
      {React.array(
        Array.map(variants, variant => {
          switch HashMap.String.get(jobs, variant) {
          | None => <td key={variant}> {"\xa0"->React.string} </td>
          | Some(job) => {
              let onClick = ev => {
                ev->ReactEvent.Synthetic.stopPropagation
                ev->ReactEvent.Synthetic.preventDefault
                let href = `${adminServiceUri}/job/${job.job_id}`
                Window_utils.windowOpen(href)
              }

              <td key={variant} className="clickable" onClick>
                <OutcomeDisplay outcome={outcome_to_string(job.outcome)} err={job.error} />
              </td>
            }
          }
        }),
      )}
    </tr>
  }
}

module Content = {
  let make_jobs_by_repo = (~isIgnoreActive: bool, jobs: array<GetAllJobs.t_jobs>) => {
    let ignoredStates = ["Aborted", "NotStarted", "Undefined"]
    if isIgnoreActive {
      ignoredStates->Js.Array2.push("Active")->ignore
    }
    let result = HashMap.String.make(~hintSize=10)
    Array.forEach(jobs, job =>
      if (
        job.variant != "(analysis)" &&
          !Js.Array2.includes(ignoredStates, outcome_to_string(job.outcome))
      ) {
        let repo = `${job.owner}/${job.name}`
        let d = switch HashMap.String.get(result, repo) {
        | None => HashMap.String.make(~hintSize=10)
        | Some(h) => h
        }
        HashMap.String.set(d, job.variant, job)
        HashMap.String.set(result, repo, d)
      }
    )
    result
  }

  let make_variants = (jobs: array<GetAllJobs.t_jobs>) => {
    let result = HashSet.String.make(~hintSize=10)
    Array.forEach(jobs, job =>
      if job.variant != "(analysis)" {
        HashSet.String.add(result, job.variant)
      }
    )
    result->HashSet.String.toArray->Js.Array.sortInPlace
  }

  @react.component
  let make = (~jobs: array<GetAllJobs.t_jobs>, ~isIgnoreActive: bool) => {
    let variants = make_variants(jobs)
    let jobs_by_repo = make_jobs_by_repo(~isIgnoreActive, jobs)

    <table className="table cell-select">
      <thead>
        <tr>
          <th> {"Repo"->React.string} </th>
          {Array.map(variants, variant =>
            <th key=variant> {React.string(variant)} </th>
          )->React.array}
        </tr>
      </thead>
      <tbody>
        {jobs_by_repo
        ->HashMap.String.toArray
        ->Js.Array2.sortInPlaceWith(((repoA, _), (repoB, _)) => String.compare(repoA, repoB))
        ->Array.map(((repo, jobs)) => <JobRow key={repo} repo jobs variants />)
        ->React.array}
      </tbody>
    </table>
  }
}

@react.component
let make = () => {
  let url = RescriptReactRouter.useUrl()
  let search = URLSearchParams.make(url.search)
  let isIgnoreActive = search->URLSearchParams.getBoolean("ignore-active")
  let (response, bar) = RefreshBarWithQuery.useWithQuery(module(GetAllJobs))

  let onChangeIgnoreActive = _ => {
    search->URLSearchParams.setBoolean("ignore-active", !isIgnoreActive)
    URL.join(url.path, search)->RescriptReactRouter.replace
  }

  <main>
    <div className="p-8 flex-col">
      {bar}
      <div className="flex-row">
        <FormControlLabel
          control={<Checkbox checked={isIgnoreActive} onChange={onChangeIgnoreActive} />}
          label={"Ignore active jobs"->React.string}
        />
      </div>
      {switch response {
      | Fetching => "Loading"->React.string
      | Error(e) => e.message->React.string
      | Empty => "Not Found"->React.string
      | Data(data)
      | PartialData(data, _) =>
        <Content jobs={data.jobs} isIgnoreActive />
      }}
    </div>
  </main>
}
