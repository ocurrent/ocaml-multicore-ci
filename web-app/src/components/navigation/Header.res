open Belt

type option = {name: string, href: string}
let options = [
  {name: "Home", href: "/"},
  {name: "Jobs", href: "/jobs"},
  {name: "GitHub", href: "/github"},
]

module MenuButton = {
  @react.component
  let make = (~name: string, ~href: string) => {
    let url = RescriptReactRouter.useUrl()
    let path = "/" ++ String.concat("/", url.path)
    let is_selected = href == "/" ? path == "/" : Js.String2.startsWith(path, href)
    <li>
      <a
        className={`nav-menu-option ${is_selected ? "selected" : ""}`}
        onClick={_ => RescriptReactRouter.push(href)}>
        {React.string(name)}
      </a>
    </li>
  }
}

@react.component
let make = () => {
  <nav>
    <img src="/images/dashboard-logo.png" alt="" height="29" />
    <div className="site-name"> {"ocaml-multicore-ci"->React.string} </div>
    <ul>
      {React.array(
        Array.map(options, option =>
          <MenuButton key={option.name} name={option.name} href={option.href} />
        ),
      )}
    </ul>
    <ul className="right" />
  </nav>
}
