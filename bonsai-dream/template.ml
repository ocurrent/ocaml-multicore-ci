let home = 
  let open Tyxml.Html in
  html
    (head (title (txt "ocaml-multicore-ci")) [
         meta ~a:[a_charset "UTF-8"] ()
       ; link ~rel:[ `Stylesheet ] ~href:"https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=block" ()
       ; link ~rel:[ `Stylesheet ] ~href:"https://fonts.googleapis.com/css2?family=Roboto+Mono&display=block" ()
       ; link ~rel:[ `Stylesheet ] ~href:"https://cdn.jsdelivr.net/npm/bootstrap@5.2.0-beta1/dist/css/bootstrap.min.css"
           ~a:[a_integrity "sha384-0evHe/X+R7YkIZDRvuzKMRqM+OrBnVFBL6DOitfPri4tjfHxaWutUpFmBp4vmVor"; a_crossorigin `Anonymous] ()
       ; meta ~a:[a_name "viewport"; a_content "width=device-width, initial-scale=1"]  ()
       ; script ~a:[a_src "js/bonsai_app.bc.js"; a_defer ()] (txt "")
    ])
    (body [
         div ~a:[a_id "app"] []
    ])
