open! Bonsai_web
module G = Multicore_graphql

val skeleton : ?nav:Vdom.Node.t -> Vdom.Node.t -> Vdom.Node.t -> Vdom.Node.t
val pipelines : Multicore_graphql.Queries.OrgQuery.t_orgs array Value.t -> Vdom.Node.t Bonsai.Computation.t

