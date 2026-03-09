type t = {
  file : string;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

let create ~file ~start_line ~start_col ~end_line ~end_col =
  { file; start_line; start_col; end_line; end_col }

let file p = p.file
let start_line p = p.start_line
let start_col p = p.start_col
let end_line p = p.end_line
let end_col p = p.end_col

let merge p1 p2 =
  { file = p1.file;
    start_line = p1.start_line;
    start_col = p1.start_col;
    end_line = p2.end_line;
    end_col = p2.end_col }

let dummy = { file = "<dummy>"; start_line = 0; start_col = 0; end_line = 0; end_col = 0 }

let compare p1 p2 =
  let c = String.compare p1.file p2.file in
  if c <> 0 then c
  else
    let c = Int.compare p1.start_line p2.start_line in
    if c <> 0 then c
    else
      let c = Int.compare p1.start_col p2.start_col in
      if c <> 0 then c
      else
        let c = Int.compare p1.end_line p2.end_line in
        if c <> 0 then c
        else Int.compare p1.end_col p2.end_col

let print fmt p =
  Format.fprintf fmt "%s:%d:%d-%d:%d" p.file p.start_line p.start_col p.end_line p.end_col

module Test = struct
  let gen =
    let open QCheck.Gen in
    let* file = pure "test.ncn" in
    let* start_line = 1 -- 100 in
    let* start_col = 0 -- 80 in
    let* end_line = start_line -- (start_line + 10) in
    let* end_col = if Int.compare end_line start_line = 0 then start_col -- (start_col + 20) else 0 -- 80 in
    pure (create ~file ~start_line ~start_col ~end_line ~end_col)

  let test =
    let arb = QCheck.make gen in
    [ QCheck.Test.make ~name:"merge preserves start of first and end of second"
        ~count:100
        (QCheck.pair arb arb)
        (fun (p1, p2) ->
           let m = merge p1 p2 in
           Int.compare (start_line m) (start_line p1) = 0
           && Int.compare (start_col m) (start_col p1) = 0
           && Int.compare (end_line m) (end_line p2) = 0
           && Int.compare (end_col m) (end_col p2) = 0)
    ]
end
