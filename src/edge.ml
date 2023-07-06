open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)

let one_d_gradient slice matrix =
  let gy_matrix = Int.Set.of_list matrix in
  Image.foldi slice ~init:0 ~f:(fun ~x ~y acc pxl ->
    match Set.nth gy_matrix ((y * 3) + x) with
    | Some elem -> acc + (Pixel.red pxl * elem)
    | None -> failwith "pxl not found")
;;

let gradient slice : float =
  let vert = one_d_gradient slice [ -1; -2; -1; 0; 0; 0; 1; 2; 1 ] in
  let hrz = one_d_gradient slice [ -1; 0; 1; -2; 0; 2; -1; 0; 1 ] in
  Float.sqrt (Float.of_int ((vert * vert) + (hrz * hrz)))
;;

let transform image ~gradient_threshold =
  Image.mapi image ~f:(fun ~x ~y _ ->
    if x < 1
       || x > Image.width image - 1
       || y < 1
       || y > Image.height image - 1
    then (
      let slice =
        Image.slice
          image
          ~x_start:(x - 1)
          ~x_end:(x + 1)
          ~y_start:(y - 1)
          ~y_end:(y + 1)
      in
      let gradient_val = gradient slice in
      if Float.O.(gradient_val > gradient_threshold)
      then Pixel.of_int 0
      else Pixel.of_int (Image.max_val image))
    else Pixel.of_int 0)
;;

let command =
  Command.basic
    ~summary:"Stego an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image_two =
          transform
            image
            ~gradient_threshold:
              (Float.of_int (Image.max_val image * 40 / 100))
        in
        Image.save_ppm image_two ~filename:"images/edge.ppm"]
;;
