open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)

let mse reference input w h : float =
  let sums =
    List.fold (List.range 0 w) ~init:0 ~f:(fun acc x ->
      acc
      + List.fold (List.range 0 h) ~init:0 ~f:(fun acc y ->
          let a = Pixel.red (Image.get reference ~x ~y) in
          let b = Pixel.red (Image.get input ~x ~y) in
          ((a - b) * (a - b)) + acc))
  in
  sums // (w * h)
;;

let swap og_img og_x og_y n_x n_y pxl2 : unit =
  List.iter (List.range (-1) 2) ~f:(fun w ->
    List.iter (List.range (-1) 2) ~f:(fun h ->
      let pix1 = Image.get og_img ~x:(og_x + w) ~y:(og_y + h) in
      Image.set og_img ~x:(og_x + w) ~y:(og_y + h) pxl2;
      Image.set og_img ~x:(n_x + w) ~y:(n_y + h) pix1))
;;

let one_move image mv_width mv_height =
  let w = mv_width / 2 in
  let h = mv_height in
  let start_x =
    List.random_element_exn (List.range w (Image.width image - w))
  in
  let start_y =
    List.random_element_exn (List.range h (Image.height image - h))
  in
  let get_region x y =
    Image.slice
      image
      ~x_start:(x - w)
      ~x_end:(x + w)
      ~y_start:(y - w)
      ~y_end:(y + w)
  in
  let region1 = get_region start_x start_y in
  let _, edit_x, edit_y, edit_pxl =
    image
    |> Image.foldi
         ~init:(Float.infinity, 0, 0, Pixel.of_int 0)
         ~f:(fun ~x ~y (minval, smallest_x, smallest_y, edit_pxl) pxl ->
           if x - w >= 0
              && x + w <= mv_width
              && y - h >= 0
              && y + h <= mv_height
           then (
             let temp = get_region x y in
             let similar = mse region1 temp mv_width mv_height in
             if Float.O.(similar < minval)
             then similar, x, y, pxl
             else minval, smallest_x, smallest_y, edit_pxl)
           else minval, smallest_x, smallest_y, edit_pxl)
  in
  swap image start_x start_y edit_x edit_y edit_pxl;
  image
;;

let transform image mv_width mv_height n_moves =
  List.fold (List.range 0 n_moves) ~init:image ~f:(fun img _ ->
    one_move img mv_width mv_height)
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
        let image_two = transform image 10 10 10000 in
        Image.save_ppm image_two ~filename:"images/mosaic.ppm"]
;;
