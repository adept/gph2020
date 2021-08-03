open! Core

module Int_int = struct
  include Tuple.Comparable (Int) (Int)
end

module State = struct
  type t =
    { puzzle : char array array
    ; dict : string list String.Map.t
    ; depth : int
    ; available : Int_int.Set.t
    ; taken : Int_int.Set.t
    ; words : (int * int * int * int * string list) list
    }

  let make puzzle dict =
    let coords = ref [] in
    for r = 0 to Array.length puzzle - 1 do
      for c = 0 to Array.length puzzle.(r) - 1 do
        coords := (r, c) :: !coords
      done
    done;
    { puzzle
    ; dict
    ; depth = 0
    ; available = Int_int.Set.of_list !coords
    ; taken = Int_int.Set.empty
    ; words = []
    }
  ;;

  let print_words t =
    List.iteri (List.rev t.words) ~f:(fun _ (_r, _c, _w, _h, ws) ->
        List.iter ws ~f:(printf "%s\n"))
  ;;

  let print t =
    List.iteri (List.rev t.words) ~f:(fun i (r, c, w, h, ws) ->
        printf
          !"Step %d: (%d,%d) to (%d,%d) => %{sexp: string list}\n%!"
          i
          (r + 1)
          (c + 1)
          (w + 1)
          (h + 1)
          ws);
    printf "END\n%!"
  ;;

  let debug_print title t =
    printf "== %s == (a: %d, t: %d)\n" title (Set.length t.available) (Set.length t.taken);
    for r = 0 to Array.length t.puzzle - 1 do
      for c = 0 to Array.length t.puzzle.(r) - 1 do
        match Set.mem t.taken (r, c) with
        | true -> printf " "
        | false -> printf "%c" t.puzzle.(r).(c)
      done;
      printf "\n"
    done;
    printf "=====\n%!"
  ;;

  let is_final t = Set.is_empty t.available

  let children t =
    (* debug_print "START" t; *)
    let children = Queue.create () in
    for r = 0 to Array.length t.puzzle - 1 do
      for c = 0 to Array.length t.puzzle.(r) - 1 do
        (* make rectangle from (r,c) to (w,h) *)
        for w = Array.length t.puzzle - 1 downto r do
          for h = Array.length t.puzzle.(r) - 1 downto c do
            (* collect letters *)
            let ls = Queue.create () in
            let av = ref t.available in
            let ta = ref t.taken in
            for i = r to w do
              for j = c to h do
                if Set.mem !av (i, j)
                then (
                  av := Set.remove !av (i, j);
                  ta := Set.add !ta (i, j);
                  Queue.enqueue ls t.puzzle.(i).(j))
              done
            done;
            if Queue.is_empty ls
            then ()
            else (
              let ls =
                Queue.to_list ls |> List.sort ~compare:Char.compare |> String.of_char_list
              in
              match Map.find t.dict ls with
              | None -> ()
              | Some ws ->
                (* debug_print "before" t;
                 * eprintf !">>> Taking %s with %{sexp:string list}\n%!" ls ws; *)
                let new_state =
                  { t with
                    available = !av
                  ; taken = !ta
                  ; words = (r, c, w, h, ws) :: t.words
                  ; depth = t.depth + 1
                  }
                in
                (* debug_print "after" new_state; *)
                Queue.enqueue children new_state)
          done
        done
      done
    done;
    children
  ;;
end

module Seen = struct
  module T = struct
    type t = Int_int.Set.t [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let of_state s = s.State.available
end

let solve_one puzzle dict max_depth =
  let q = Deque.create () in
  let state = State.make puzzle dict in
  let best = ref state in
  let best_depth = ref 1000000 in
  let seen = ref Seen.Set.empty in
  let cnt = ref 0 in
  Deque.enqueue_front q state;
  let rec loop () =
    incr cnt;
    if !cnt mod 100_000 = 0 then printf "In Q: %d, cnt %d\n%!" (Deque.length q) !cnt;
    match Deque.dequeue_front q with
    | None -> printf "END OF QUEUE\n%!"
    | Some state ->
      let depth = state.State.depth in
      if max_depth = 1 then State.print_words state;
      (* State.debug_print "CONSIDERING" state;
       * State.print state; *)
      if State.is_final state
      then (
        printf !">>> FOUND (queue = %d, best depth = %d)\n%!" (Deque.length q) !best_depth;
        State.print state;
        if Int.( <= ) depth !best_depth
        then (
          best_depth := depth;
          best := state;
          printf !">>> new best depth: %d\n" depth);
        loop ())
      else if Int.( >= ) depth (!best_depth - 1)
      then loop ()
      else if Int.( >= ) depth max_depth
      then loop ()
      else (
        let s = Seen.of_state state in
        if Set.mem !seen s
        then loop ()
        else (
          seen := Set.add !seen s;
          let children = State.children state in
          let children =
            Queue.to_list children
            (* |> List.filter ~f:(fun s -> Int.( < ) s.State.depth (!best_depth - 1) *)
            |> List.sort ~compare:(fun a b ->
                   Int.compare
                     (Set.length b.State.available)
                     (Set.length a.State.available))
          in
          List.iter children ~f:(Deque.enqueue_front q);
          loop ()))
  in
  loop ();
  printf ">>> BEST:\n%!";
  State.print !best
;;

let solve dict max_depth =
  let words = In_channel.read_lines dict in
  let dict =
    List.filter words ~f:(fun w -> String.length w > 2)
    |> List.map ~f:(fun w ->
           String.of_char_list (List.sort ~compare:Char.compare (String.to_list w)), w)
    |> String.Map.of_alist_multi
  in
  let rec do_one () =
    printf "Enter puzzle, end with .:\n%!";
    let rec loop acc =
      let line = In_channel.input_line_exn In_channel.stdin in
      if String.( = ) line "."
      then List.rev acc
      else
        loop
          ((String.lowercase line
           |> String.filter ~f:(fun c -> not (Char.is_whitespace c)))
           :: acc)
    in
    let puzzle = loop [] in
    printf "Solving:\n%s\n" (String.concat ~sep:"\n" puzzle);
    let puzzle =
      List.map puzzle ~f:(fun l -> String.to_list l |> Array.of_list) |> Array.of_list
    in
    solve_one puzzle dict max_depth;
    do_one ()
  in
  do_one ()
;;

let commands =
  let open Command.Let_syntax in
  Command.group
    ~summary:"Solve the Letterboxing"
    [ ( "solve"
      , Command.basic
          ~summary:"solve it"
          (let%map_open filename = anon ("SOWPODS" %: Filename.arg_type)
           and max_depth = flag "max-depth" (optional int) ~doc:"INT search max depth" in
           fun () -> solve filename (Option.value max_depth ~default:1000000)) )
    ]
;;

let () = Command.run commands
