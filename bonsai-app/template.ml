open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module G = Multicore_graphql

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



let pipelines (orgs : Multicore_graphql.Queries.OrgQuery.t_orgs array Value.t) : Vdom.Node.t Computation.t =
  let render_master_status (x : G.Queries.OrgQuery.t_orgs_repos_master_status) = match x with
    | `NotStarted -> "not started"
    | `Passed -> "passed"
    | `Failed -> "failed"
    | `Pending -> "pending"
    | `FutureAddedValue s -> s
  in
  let pipeline (p : Multicore_graphql.Queries.OrgQuery.t_orgs) =
    Array.to_list @@ Array.map p.repos ~f:(fun (repo : G.Queries.OrgQuery.t_orgs_repos) ->
        Vdom.Node.tr [
        Vdom.Node.th [Vdom.Node.text p.name];
        Vdom.Node.td [Vdom.Node.text repo.name];
        Vdom.Node.td [Vdom.Node.text (render_master_status repo.master_status)];
        Vdom.Node.td [Vdom.Node.text "GitHub"];
      ] )
  in

  let%arr orgs = orgs in
  Vdom.Node.table ~attr:(Vdom.Attr.classes [ "table" ]) [
    Vdom.Node.thead  [ Vdom.Node.tr [
                           Vdom.Node.th [Vdom.Node.text "Pipeline"];
                           Vdom.Node.th [Vdom.Node.text "Repository"];
                           Vdom.Node.th [Vdom.Node.text "Status"];
                           Vdom.Node.th [Vdom.Node.text "Git Forge"];
      ] ]
  ; Vdom.Node.tbody ~key:"pipelines-key" ~attr:(Vdom.Attr.class_ "pipeline-tbody")
      (List.concat @@ Array.to_list (Array.map orgs ~f:pipeline))
    (* [ Vdom.Node.tr [ *)
    (*       Vdom.Node.th [Vdom.Node.text "123"]; *)
    (*       Vdom.Node.td [Vdom.Node.text "tmcgilchrist/ocaml-gitlab"]; *)
    (*       Vdom.Node.td [Vdom.Node.text "Passed"]; *)
    (*       Vdom.Node.td [Vdom.Node.text "GitHub"]; *)
    (*     ]; *)
      (* Vdom.Node.tr [ *)
      (*     Vdom.Node.th [Vdom.Node.text "456"]; *)
      (*     Vdom.Node.td [Vdom.Node.text "tmcgilchrist/ocaml-changes"]; *)
      (*     Vdom.Node.td [Vdom.Node.text "Failed"]; *)
      (*     Vdom.Node.td [Vdom.Node.text "GitHub"]; *)
      (*   ]; *)
      (* Vdom.Node.tr [ *)
      (*     Vdom.Node.th [Vdom.Node.text "456"]; *)
      (*     Vdom.Node.td [Vdom.Node.text "inhabitedtype/ocaml-aws"]; *)
      (*     Vdom.Node.td [Vdom.Node.text "Failed"]; *)
      (*     Vdom.Node.td [Vdom.Node.text "GitHub"]; *)
      (*   ]; *)
    (* ] *)
  ]

