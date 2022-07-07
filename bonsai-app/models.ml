module Pipeline = struct
  type t = { name: string }

  (* TODO to_graphql and from_graphql serialisers *)
end

type pipelines = Pipeline.t list

module Owner = struct
  type t = string

  (* TODO to_graphql and from_graphql serialisers *)
end

module Repository = struct
  type t = { owner: Owner.t; name: string; git_forge: string }

  (* TODO to_graphql and from_graphql serialisers *)
end

