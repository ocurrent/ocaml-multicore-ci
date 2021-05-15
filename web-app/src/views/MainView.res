open Belt

@react.component
let make = () => {
  <main className="p-8 flex flex-col">
    {React.array(
      Array.map(Header.options, option =>
        <Link href={option.href} key={option.name}> {React.string(option.name)} </Link>
      ),
    )}
  </main>
}
