module type Query = sig
  type t
  type t_variables

  module Raw : sig
    type t
    type t_variables
  end

  val query : string
  val parse : Raw.t -> t
  val serialize : t -> Raw.t
  val serializeVariables : t_variables -> Raw.t_variables
  val unsafe_fromJson : Yojson.Basic.t -> Raw.t
  val toJson : Raw.t -> Yojson.Basic.t
  val variablesToJson : Raw.t_variables -> Yojson.Basic.t
end

module SerializableQuery (Q : Query) = struct
  include Q

  let t_of_sexp s =
    s |> Sexplib0.Sexp.to_string |> Yojson.Basic.from_string
    |> Q.unsafe_fromJson |> Q.parse

  let sexp_of_t t =
    let t_str = t |> Q.serialize |> Q.toJson |> Yojson.Basic.to_string in
    Sexplib0.Sexp.Atom t_str

  let equal a b = Sexplib0.Sexp.equal (sexp_of_t a) (sexp_of_t b)

  let yojson_of_t_variables vars =
    vars |> serializeVariables |> Q.variablesToJson |> Yojson.Basic.to_string
    |> Yojson.Safe.from_string
end
;;

[%graphql
  {|
   query OrgQuery {
     orgs {
       name
       repos {
         name
         master_status
       }
     }
   }
|}]
;;

