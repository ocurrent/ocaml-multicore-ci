let join = (path, q) => {
  let joinedPath = path->Belt.List.toArray->Js.Array2.joinWith("/")
  `/${joinedPath}${q->URLSearchParams.toStringQuestionMark}`
}
