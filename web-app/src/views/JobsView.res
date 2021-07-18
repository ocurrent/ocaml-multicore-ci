open Belt

module GetAllJobs = %graphql(`
{ jobs { owner, name, hash, job_id, variant, outcome, error } }
`)
external outcome_to_string: GetAllJobs.t_jobs_outcome => string = "%identity"

module JobRow = {
  @react.component
  let make = (~job: GetAllJobs.t_jobs) => {
    let adminServiceUri = ConfigHooks.useAdminServiceUri()

    let onClick = ev => {
      ev->ReactEvent.Synthetic.stopPropagation
      ev->ReactEvent.Synthetic.preventDefault
      let href = `${adminServiceUri}/job/${job.job_id}`
      Window_utils.windowOpen(href)
    }

    if job.variant == "(analysis)" {
      React.null
    } else {
      <tr className="cursor-pointer" onClick={onClick}>
        <td> {job.job_id->React.string} </td>
        <td> {`${job.owner}/${job.name}`->React.string} </td>
        <td> {Js.String2.substring(job.hash, ~from=0, ~to_=8)->React.string} </td>
        <td> {job.variant->React.string} </td>
        <td> <OutcomeDisplay outcome={outcome_to_string(job.outcome)} err={job.error} /> </td>
      </tr>
    }
  }
}

module Content = {
  @react.component
  let make = (~jobs: array<GetAllJobs.t_jobs>) => {
    Js.Array.sortInPlaceWith((j1: GetAllJobs.t_jobs, j2: GetAllJobs.t_jobs) => {
      -String.compare(j1.job_id, j2.job_id)
    }, jobs)->ignore
    <table className="table row-select">
      <thead>
        <tr>
          <th> {"Job ID"->React.string} </th>
          <th> {"Repo"->React.string} </th>
          <th> {"Hash"->React.string} </th>
          <th> {"Variant"->React.string} </th>
          <th> {"Outcome"->React.string} </th>
        </tr>
      </thead>
      <tbody>
        {Array.mapWithIndex(jobs, (idx, job) =>
          <JobRow key={idx->Int.toString} job={job} />
        )->React.array}
      </tbody>
    </table>
  }
}

@react.component
let make = () => {
  let (response, bar) = RefreshBarWithQuery.useWithQuery(module(GetAllJobs))

  <main>
    <div className="p-8">
      {bar}
      {switch response {
      | Fetching => "Loading"->React.string
      | Error(e) => e.message->React.string
      | Empty => "Not Found"->React.string
      | Data(data)
      | PartialData(data, _) =>
        <Content jobs={data.jobs} />
      }}
    </div>
  </main>
}
