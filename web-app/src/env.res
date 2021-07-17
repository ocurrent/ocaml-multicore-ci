type env = {
  @as("REACT_APP_GRAPHQL_ENDPOINT")
  graphql_endpoint: string,
}
@val @scope("process") external env: env = "env"
