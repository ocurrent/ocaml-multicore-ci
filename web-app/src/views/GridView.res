open Belt
open ReScriptUrql

module GetAllJobs = %graphql(`
{ orgs {name, repos {name, master_status, refs {gref, hash, status, jobs { variant, outcome, error } } } } }
`)
external outcome_to_string: GetAllJobs.t_orgs_repos_refs_jobs_outcome => string = "%identity"

module JobRow = {
  @react.component
  let make = (
    ~org_name: string,
    ~repo_name: string,
    ~build_ref: GetAllJobs.t_orgs_repos_refs,
    ~variants: array<string>,
  ) => {
    let outcome_by_variant = Belt_HashMapString.make(~hintSize=10)
    Array.forEach(build_ref.jobs, job =>
      Belt_HashMapString.set(outcome_by_variant, job.variant, (job.outcome, job.error))
    )
    <tr>
      <td> {React.string(repo_name)} </td>
      {React.array(
        Array.map(variants, variant => {
          let onClick = ev => {
            ev->ReactEvent.Synthetic.stopPropagation
            ev->ReactEvent.Synthetic.preventDefault
            let href = `http://localhost:8090/github/${org_name}/${repo_name}/commit/${build_ref.hash}/variant/${variant}`
            Window_utils.windowOpen(href)
          }

          <td key={variant} className="cursor-pointer" onClick>
            {switch Belt_HashMapString.get(outcome_by_variant, variant) {
            | Some((outcome, err)) =>
              <OutcomeDisplay outcome={outcome_to_string(outcome)} err={err} />
            | None => React.string("-")
            }}
          </td>
        }),
      )}
    </tr>
  }
}

module Content = {
  @react.component
  let make = (~orgs: array<GetAllJobs.t_orgs>) => {
    let all_variants = Belt_HashSetString.make(~hintSize=10)
    Array.forEach(orgs, org =>
      Array.forEach(org.repos, repo => {
        Array.forEach(repo.refs, build_ref =>
          Array.forEach(build_ref.jobs, job =>
            if job.variant != "(analysis)" {
              Belt_HashSetString.add(all_variants, job.variant)
            }
          )
        )
      })
    )
    let variants = all_variants->Belt_HashSetString.toArray->Js.Array.sortInPlace

    <table className="table">
      <thead>
        <tr>
          <th> {"Repo"->React.string} </th>
          {Array.map(variants, variant =>
            <th key=variant> {React.string(variant)} </th>
          )->React.array}
        </tr>
      </thead>
      <tbody>
        {Array.map(orgs, org =>
          Array.map(org.repos, repo =>
            Array.map(repo.refs, build_ref =>
              <JobRow
                key={repo.name ++ build_ref.hash}
                org_name=org.name
                repo_name=repo.name
                build_ref
                variants
              />
            )
          )
        )
        ->Array.concatMany
        ->Array.concatMany
        ->React.array}
      </tbody>
    </table>
  }
}

@react.component
let make = () => {
  let ({Hooks.response: response}, _) = Hooks.useQuery(
    ~query=module(GetAllJobs),
    ~requestPolicy=#CacheFirst,
    (),
  )

  <main>
    <div className="p-8">
      {switch response {
      | Fetching => "Loading"->React.string
      | Error(e) => e.message->React.string
      | Empty => "Not Found"->React.string
      | Data(data)
      | PartialData(data, _) =>
        <Content orgs={data.orgs} />
      }}
    </div>
  </main>
}
