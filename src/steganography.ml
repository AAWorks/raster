open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image =
  let process_pixel ((r, g, b) : Pixel.t) : Pixel.t =
    r % 4 * 64, g % 4 * 64, b % 4 * 64
  in
  Image.map image ~f:process_pixel
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
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm image ~filename:"images/mystery.ppm"]
;;
