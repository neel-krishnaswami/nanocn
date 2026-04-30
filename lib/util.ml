let rec result_list = function
  | [] -> Ok []
  | Ok x :: rest ->
    (match result_list rest with
     | Ok xs -> Ok (x :: xs)
     | Error _ as e -> e)
  | Error e :: _ -> Error e

module Test = struct
  let test =
    let int_gen = QCheck.Gen.nat_small in
    let result_gen err_gen ok_gen =
      QCheck.Gen.(
        oneof [
          map (fun x -> Ok x) ok_gen;
          map (fun e -> Error e) err_gen;
        ])
    in
    [
      QCheck.Test.make
        ~name:"result_list: all Ok preserves order"
        ~count:200
        (QCheck.make QCheck.Gen.(list_size (0 -- 8) int_gen))
        (fun xs ->
           match result_list (List.map (fun x -> Ok x) xs) with
           | Ok ys -> List.equal Int.equal xs ys
           | Error (_ : int) -> false);

      QCheck.Test.make
        ~name:"result_list: any Error yields the first Error"
        ~count:200
        (QCheck.make QCheck.Gen.(list_size (1 -- 8) (result_gen int_gen int_gen)))
        (fun xs ->
           let first_err =
             List.find_map (function Error e -> Some e | Ok _ -> None) xs
           in
           match result_list xs, first_err with
           | Ok _, None -> true
           | Error e, Some e' -> Int.equal e e'
           | _ -> false);

      QCheck.Test.make
        ~name:"result_list: empty list returns Ok []"
        ~count:1
        (QCheck.make QCheck.Gen.unit)
        (fun () ->
           match result_list ([] : (int, int) result list) with
           | Ok [] -> true
           | _ -> false);
    ]
end
