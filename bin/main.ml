open Rdf
open Sparql

let base_iri = Iri.of_string "http://www.edubbainstitute.org/onto/"

let read_file filename = 
  let ch = Stdlib.open_in_bin filename in 
  let s = Stdlib.really_input_string ch (Stdlib.in_channel_length ch) in 
  close_in ch;
  s

let create_query query_file = 
  let query_str = read_file query_file in 
  let query = 
    try query_from_string query_str 
    with Error e -> failwith(string_of_error e) in 
  query


let run_query turtle_file query_file = 
  let graph = Graph.open_graph base_iri in 
  Ttl.from_file graph turtle_file ;
  let query = create_query query_file in 
  let dataset = Ds.simple_dataset graph in 

  let solutions = select ~base:base_iri dataset query in 
  let print_sol = 
    let print varname term = 
      Printf.printf "%s => %s\n" varname (Term.string_of_term term)
    in 
    fun sol -> 
      print_endline "Solution:";
      solution_iter print sol ;
      print_newline() ; 
  in 
  List.iter print_sol solutions
  


let () = 
  match Sys.argv with 
  | [| _; turtle_file; query_file |] -> run_query turtle_file query_file
  | _ -> failwith "oops"
