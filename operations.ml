(* file will contain file I/O and CamelJockey testing suite *)
open Test_framework
open Droplet
open Fountain
open Goblet

let message = "Checkpoint 1 Write Up
Project name: Fountain Codes
Group member names and emails: 
    Renzo Lucioni: rlucioni@college.harvard.edu
    Vipul Shekhawat: vshekhawat@college.harvard.edu
    Daniel Broudy: daniel.broudy@gmail.com
    Peregrine Badger: pbadger@college.harvard.edu
    Status Report
    Progress
    Over the past week, we have met all of the goals for the week we set out for
    ourselves on the timeline last week. We decided to break down an input
    message into 1-character pieces for the alpha. We cleaned up unnecessary
    code in Fountain and rewrote the format in Droplets store information -
    formerly, it was a list, now it is a record. The majority of the week was
    spent implementing Goblet, which was nonexistent last week. With these
    changes, we have attained basic functionality. Here’s how it works."


let f = new lt_fountain message 1 10
let g = new lt_goblet f#output_droplet 10

let rec transmit () : unit = 
    if g#check_complete
      then g#print_progress
      else ((g#get_droplet f#output_droplet); 
           g#decode; ignore(g#get_message); transmit ()) ;;

transmit () ;;

(* sample usage of the testing framework *)
(*
let sample_test = mk_expect_test
    (fun () -> test_function expected_result "message" ;;

run_test_set [sample_test;] "group message" ;;
*)
