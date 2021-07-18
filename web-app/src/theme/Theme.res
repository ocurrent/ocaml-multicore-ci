let make = () =>
  MaterialUi_Theme.create({
    open MaterialUi_ThemeOptions
    make(
      ~palette=PaletteOptions.make(
        ~primary=Primary.make(~main="#404040", ()),
        ~secondary=Secondary.make(~main="rgb(236,125,11)", ~contrastText="white", ()),
        (),
      ),
      (),
    )
  })
