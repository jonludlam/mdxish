(lang dune 2.0)

(name mdxish)

(source
  (github jonludlam/mdxish))

(license ISC)

(authors
  "Jon Ludlam")

(generate_opam_files true)

(package
  (name mdxish)
  (synopsis "Mdx/jupyter experiment")
  (description "An experimental client-side mdx/jupyter web thing")
  (depends
    js_of_ocaml
    js_of_ocaml-tyxml
    js_of_ocaml-lwt
    js_top_worker
    js_top_worker-client
    lwt
    brr
    astring
    omd
    code-mirror))

