open! Core

module Cardtype = struct
  type t = Action
end

module Card = struct
  type t =
    | Bazaar
    | City
    | Crossroads
    | Lost_City
    | Smithy
    | Unknown

  let context_dependent t =
    match t with
    | Bazaar -> false
    | City -> false
    | Crossroads -> true
    | Lost_City -> false
    | Smithy -> false
    | Unknown -> true
  ;;
end

(*
What are all the states you can get to with KNOWN cards?
What effect do unknown cards have there?
 *)

module State = struct
  type t =
    { stack : Card.t Queue.t
    ; hand : Card.t Queue.t
    ; mutable hand_size : int
    ; mutable actions : int
    ; mutable coins : int
    }

  let create =
    { stack = Queue.create ()
    ; hand = Queue.of_list [ Card.Bazaar; City; Crossroads; Lost_City; Smithy ]
    ; hand_size = 5
    ; actions = 1
    ; coins = 0
    }
  ;;

  let gain_coin t = t.coins <- t.coins + 1
  let gain_actions t count = t.actions <- t.actions + count
  let spend_action t = t.actions <- t.actions - 1

  let add_to_hand t card =
    Queue.enqueue t.hand card;
    t.hand_size <- t.hand_size + 1
  ;;

  let rec draw t num =
    if num = 0
    then ()
    else (
      match Queue.dequeue t.stack with
      | None -> add_to_hand t Unknown
      | Some card ->
        add_to_hand t card;
        draw t (num - 1))
  ;;

  let play t card =
    spend_action t;
    match card with
    | Card.Bazaar ->
      draw t 1;
      gain_coin t;
      gain_actions t 2
    | City ->
      draw t 1;
      gain_actions t 2
    | Crossroads | Lost_City | Smithy -> draw t 3
    | Unknown -> ()
  ;;
end

module Room = struct
  type t =
    { pos : int
    ; name : string
    ; locs : int
    }
  [@@deriving sexp]
end

module Maze = struct
  type t = Room.t String.Map.t

  let rooms =
    let cnt = ref 0 in
    let rooms = Queue.create () in
    let r name ~locs =
      let pos = !cnt in
      incr cnt;
      Queue.enqueue rooms (name, { Room.name; pos; locs })
    in
    r "Bazaar" ~locs:1;
    r "City" ~locs:1;
    r "Crossroads" ~locs:0;
    r "Lost City" ~locs:2;
    r "Smithy" ~locs:3;
    r "? #1" ~locs:2;
    r "? #2" ~locs:1;
    r "? #3" ~locs:2;
    r "? #4" ~locs:0;
    r "? #5" ~locs:0;
    r "? #6" ~locs:0;
    r "? #7" ~locs:1;
    r "? #8" ~locs:1;
    r "? #9" ~locs:1;
    r "? #10" ~locs:0;
    r "? #11" ~locs:0;
    r "? #12" ~locs:1;
    r "? #13" ~locs:1;
    r "? #14" ~locs:1;
    r "? #15" ~locs:2;
    r "? #16" ~locs:2;
    r "? #17" ~locs:2;
    Queue.to_list rooms |> String.Map.of_alist_exn
  ;;

  let find name = Map.find_exn rooms name
end

module Path = struct
  type t = Room.t array [@@deriving sexp]

  let of_input str =
    String.split ~on:',' str
    |> List.map ~f:String.strip
    |> Array.of_list
    |> Array.map ~f:Maze.find
  ;;

  let to_string t =
    Array.to_list t |> List.map ~f:(fun r -> r.Room.name) |> String.concat ~sep:", "
  ;;
end

module Permutation = struct
  type t =
    { path : Path.t
    ; mutable from_ : int
    ; mutable to_ : int
    }

  let of_path path = { path; from_ = 0; to_ = 0 }

  let rec do_next t =
    let p_len = Array.length t.path - 1 in
    if t.from_ >= p_len
    then None
    else if t.to_ >= p_len
    then (
      t.from_ <- t.from_ + 1;
      t.to_ <- t.from_ + 2;
      do_next t)
    else if t.to_ = t.from_
    then (
      t.to_ <- t.to_ + 1;
      do_next t)
    else (
      let path = Array.copy t.path in
      let swap = path.(t.from_) in
      path.(t.from_) <- path.(t.to_);
      path.(t.to_) <- swap;
      t.to_ <- t.to_ + 1;
      Some path)
  ;;
end

module Program = struct
  type t = string list

  let of_path p =
    let pos = ref 0 in
    let locs = ref 5 in
    let cmd = Queue.create () in
    Queue.enqueue cmd "restart";
    let move from_ to_ =
      if to_ > !locs - 1
      then failwithf "Can't go from %d to %d as max locs is %d" from_ to_ !locs ();
      let diff = to_ - from_ in
      if diff > 0
      then List.init diff ~f:(const "e") |> List.iter ~f:(Queue.enqueue cmd)
      else List.init (-diff) ~f:(const "w") |> List.iter ~f:(Queue.enqueue cmd)
    in
    let play room =
      Queue.enqueue cmd "p";
      locs := !locs + room.Room.locs
    in
    for i = 0 to Array.length p - 1 do
      let room = p.(i) in
      let new_pos = room.Room.pos in
      move !pos new_pos;
      play room;
      pos := new_pos
    done;
    Queue.enqueue cmd "i";
    Queue.enqueue cmd "x me";
    Queue.enqueue cmd "e";
    Queue.to_list cmd |> String.concat ~sep:";"
  ;;

  let of_path p = Or_error.try_with (fun () -> of_path p)
end

(* City, ? #1, Smithy, Lost City, Bazaar, Crossroads, ? #2, ? #9, ? #8, ? #7, ? #3, ? #14, ? #13, ? #12, ? #15, ? #16 *)
let solve_one path =
  let path = Path.of_input path in
  printf !"PATH: %{sexp: Path.t}\n" path;
  let perm = Permutation.of_path path in
  let rec loop perm =
    match Permutation.do_next perm with
    | None -> ()
    | Some path ->
      (match Program.of_path path with
      | Ok pgm ->
        printf !"PATH: %{Path}\n%!" path;
        printf !"    %s\n%!" pgm
      | Error _err -> () (* printf !"    IMPOSSIBLE: %{Error#mach}!\n%!" err *));
      loop perm
  in
  loop perm
;;

let solve _max_depth =
  let rec loop () =
    printf "Enter path to improve:\n%!";
    let line = In_channel.input_line_exn In_channel.stdin in
    solve_one line;
    loop ()
  in
  loop ()
;;

let commands =
  let open Command.Let_syntax in
  Command.group
    ~summary:"Try and improve the Adventure solution"
    [ ( "solve"
      , Command.basic
          ~summary:"solve it"
          (let%map_open max_depth =
             flag "max-depth" (optional int) ~doc:"INT search max depth"
           in
           fun () -> solve (Option.value max_depth ~default:1000000)) )
    ]
;;

let () = Command.run commands
