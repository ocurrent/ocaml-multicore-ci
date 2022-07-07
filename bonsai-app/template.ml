open! Core
open Bonsai_web

let skeleton ?(nav = Vdom.Node.none) title body =
  Vdom.Node.div
    ~attr:(Vdom.Attr.classes [ "container"; "min-vh-100" ])
    [
      Vdom.Node.div
        ~attr:(Vdom.Attr.classes [ "row"; "min-vh-100" ])
        [
          Vdom.Node.div
            ~attr:(Vdom.Attr.classes [ "col-md-12"; "min-vh-100" ])
            [
              Vdom.Node.div
                ~attr:(Vdom.Attr.classes [ "p-4"; "text-center"; "bg-primary" ])
                [
                  Vdom.Node.h1
                    ~attr:(Vdom.Attr.classes [ "mb-3"; "text-light" ])
                    [ title ];
                ];
              nav;
              Vdom.Node.div
                ~attr:
                  (Vdom.Attr.classes
                     [ "container-sm"; "bg-white"; "min-vh-100" ])
                [ body ];
            ];
        ];
    ]

let pipelines = [
    Vdom.Node.thead  [ Vdom.Node.tr [
                           Vdom.Node.th [Vdom.Node.text "Pipeline"];
                           Vdom.Node.th [Vdom.Node.text "Repository"];
                           Vdom.Node.th [Vdom.Node.text "Status"];
                           Vdom.Node.th [Vdom.Node.text "Git Forge"];
      ] ]
  ; Vdom.Node.tbody ~key:"pipelines-key" ~attr:(Vdom.Attr.class_ "pipeline-tbody")
    [ Vdom.Node.tr [
          Vdom.Node.th [Vdom.Node.text "123"];
          Vdom.Node.td [Vdom.Node.text "tmcgilchrist/ocaml-gitlab"];
          Vdom.Node.td [Vdom.Node.text "Passed"];
          Vdom.Node.td [Vdom.Node.text "GitHub"];
        ];
      Vdom.Node.tr [
          Vdom.Node.th [Vdom.Node.text "456"];
          Vdom.Node.td [Vdom.Node.text "tmcgilchrist/ocaml-changes"];
          Vdom.Node.td [Vdom.Node.text "Failed"];
          Vdom.Node.td [Vdom.Node.text "GitHub"];
        ];
      Vdom.Node.tr [
          Vdom.Node.th [Vdom.Node.text "456"];
          Vdom.Node.td [Vdom.Node.text "inhabitedtype/ocaml-aws"];
          Vdom.Node.td [Vdom.Node.text "Failed"];
          Vdom.Node.td [Vdom.Node.text "GitHub"];
        ];
    ]
  ]

