open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let component =
  let%sub home_link =
    Route.link
      ~children:(Bonsai.const @@ Vdom.Node.text "Home")
      ~attrs:[ Vdom.Attr.class_ "navbar-brand" ]
      (Value.return "/")
  in
  let%sub jobs_link =
    Route.link
      ~children:(Bonsai.const @@ Vdom.Node.text "Jobs")
      ~attrs:[ Vdom.Attr.class_ "nav-link" ]
      (Value.return "/jobs")
  in

  let%arr home_link = home_link 
  and jobs_link = jobs_link in
  let wrap_link link =
    Vdom.Node.li ~attr:(Vdom.Attr.classes [ "nav-item" ]) [ link ]
  in
  Vdom.Node.div
    ~attr:
      (Vdom.Attr.classes
         [ "navbar"; "navbar-expand-lg"; "navbar-dark"; "bg-dark" ])
    [
      home_link;
      Vdom.Node.div
        ~attr:(Vdom.Attr.classes [ "collapse"; "navbar-collapse" ])
        [
          Vdom.Node.ul
            ~attr:(Vdom.Attr.classes [ "navbar-nav"; "mr-auto" ])
            [
              wrap_link jobs_link;
            ]
        ]
    ]
  