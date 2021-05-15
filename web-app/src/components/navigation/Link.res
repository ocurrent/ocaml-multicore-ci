@react.component
let make = (~href: string, ~children: React.element) => {
  let onClick = ev => {
    ev->ReactEvent.Synthetic.stopPropagation
    ev->ReactEvent.Synthetic.preventDefault
    RescriptReactRouter.push(href)
  }

  <a href onClick={onClick}> {children} </a>
}
