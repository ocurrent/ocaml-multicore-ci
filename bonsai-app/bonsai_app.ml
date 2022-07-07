open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module G = Multicore_graphql
module M = Models

let component _orgs = 
  let title_router = function
    | path -> (
      match%sub Value.map ~f:(fun p -> p |> String.split ~on:'/' |> List.filter ~f:(fun str -> String.(str <> ""))) path with
      | [] -> Bonsai.const @@ Vdom.Node.text "OCaml Multcore CI" 
      | _ -> Bonsai.const @@ Vdom.Node.text "OCaml Multcore CI")
  in
  let title = Route.router title_router in
  let body_router = function
    | path -> (
      match%sub Value.map ~f:(fun p -> p |> String.split ~on:'/' |> List.filter ~f:(fun str -> String.(str <> ""))) path with
      | [] -> Bonsai.const @@ Vdom.Node.text "Multcore CI - empty body"
      | _ -> Bonsai.const @@ Vdom.Node.table ~attr:(Vdom.Attr.classes [ "table" ]) Template.pipelines
    )
  in

  let body =  Route.router body_router in
  let%map.Computation title = title and body = body and nav = Nav.component in
  Template.skeleton title body ~nav

let foo =
  let module P = G.Queries.OrgQuery in
  let module Loader = Graphql_loader.ForQuery (P) in
  Loader.component
    (fun data -> 
      let orgs = Value.map data ~f:(fun data -> data.orgs) in
      component orgs)
      (Value.return @@ G.Queries.OrgQuery.makeVariables ())

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" foo


