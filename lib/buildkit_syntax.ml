(* From `docker manifest inspect docker/dockerfile:experimental` *)
let hash_for = function
  | `Aarch64 ->
      "sha256:9ad2a80da6ecccdc36ffe9a6690d919bdcf36575e01f2fdf0c8c2b9cf838c309"
  | `Ppc64le ->
      "sha256:fd2f1d1028f65af35a1151a43ef8e93a5f7e4bfbfec8c24a862baa7eee69be94"
  | `X86_64 ->
      "sha256:3c244c0c6fc9d6aa3ddb73af4264b3a23597523ac553294218c13735a2c6cf79"

let add arch =
  let hash =
    hash_for
      (match arch with
      | `X86_64 | `I386 -> `X86_64
      | `Aarch64 | `Aarch32 -> `Aarch64
      | `Ppc64le -> `Ppc64le
      | `S390x | `Riscv64 -> assert false)
  in
  Printf.sprintf "# syntax = docker/dockerfile:experimental@%s\n" hash
