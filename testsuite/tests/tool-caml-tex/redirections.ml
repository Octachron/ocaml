(* TEST
   include str
   include unix
   script="${ocamlrun} ${ocamlsrcdir}/tools/caml-tex -repo-root \
   ${ocamlsrcdir} ${test_source_directory}/${test_file} \
   -o redirections.output"
  * script
  ** check-program-output
  reference ="${test_source_directory}/redirections.reference"
  output="redirections.output"
*)

\begin{caml_example}{toplevel}
[@@@warning "+A"];;
1 + 2. [@@expect error];;
let f x = () [@@expect warning 27];;
\end{caml_example}

\begin{caml_example}{toplevel}
Format.printf "Hello@.";
print_endline "world";;
\end{caml_example}
