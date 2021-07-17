open Belt
open ReScriptUrql

module GetAllOrgs = %graphql(`
{ orgs { name, repos { name, master_status } } }
`)

module RepoDisplay = {
  @react.component
  let make = (~org: GetAllOrgs.t_orgs, ~repo: GetAllOrgs.t_orgs_repos) => {
    <li>
      <a target="_blank" rel="noopener noreferrer" href={`/github/${org.name}/${repo.name}`}>
        {repo.name->React.string}
      </a>
    </li>
  }
}

module OrgDisplay = {
  @react.component
  let make = (~org: GetAllOrgs.t_orgs) => {
    let repos = org.repos
    Js.Array.sortInPlaceWith((o1: GetAllOrgs.t_orgs_repos, o2: GetAllOrgs.t_orgs_repos) => {
      String.compare(o1.name, o2.name)
    }, repos)->ignore

    <li>
      {org.name->React.string}
      <ul>
        {Array.map(repos, repo =>
          <RepoDisplay key={repo.name} org={org} repo={repo} />
        )->React.array}
      </ul>
    </li>
  }
}

module OrgList = {
  @react.component
  let make = (~orgs: array<GetAllOrgs.t_orgs>) => {
    Js.Array.sortInPlaceWith((o1: GetAllOrgs.t_orgs, o2: GetAllOrgs.t_orgs) => {
      String.compare(o1.name, o2.name)
    }, orgs)->ignore

    <ul className="org-list">
      {Array.map(orgs, org => <OrgDisplay key={org.name} org={org} />)->React.array}
    </ul>
  }
}

@react.component
let make = () => {
  let ({Hooks.response: response}, _) = Hooks.useQuery(
    ~query=module(GetAllOrgs),
    ~requestPolicy=#CacheFirst,
    (),
  )

  <main className="p-8">
    <h1> {"GitHub integration"->React.string} </h1>
    <p>
      {"See "->React.string}
      <a href="https://github.com/apps/multicore-ci">
        {"The OCaml-Multicore-CI GitHub App"->React.string}
      </a>
      {" for details."->React.string}
    </p>
    <div>
      <h2> {"Registered repos"->React.string} </h2>
      {switch response {
      | Fetching => "Loading..."->React.string
      | Error(e) => e.message->React.string
      | Empty => "Not Found"->React.string
      | Data(data)
      | PartialData(data, _) =>
        <OrgList orgs={data.orgs} />
      }}
    </div>
  </main>
}
