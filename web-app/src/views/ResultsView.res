open Belt

type result = {component: string}

module ResultRow = {
  @react.component
  let make = (~result: result) => {
    <tr>
      <td> {React.string(result.component)} </td>
      <td> {React.string(`✔`)} </td>
      <td> {React.string(`✗`)} </td>
    </tr>
  }
}

@react.component
let make = () => {
  let (results, setResults) = React.useState(() => [])

  React.useEffect(() => {
    setResults(_ => [{component: "CompCert"}])
    None
  })

  <main>
    <div className="p-8">
      <table className="table">
        <thead>
          <tr>
            <th> {React.string("Component")} </th>
            <th> {React.string("4.12")} </th>
            <th> {React.string("4.12+domains")} </th>
          </tr>
        </thead>
        <tbody> {React.array(Array.map(results, result => <ResultRow result />))} </tbody>
      </table>
    </div>
  </main>
}
