open Core

(* This should look familiar by now! *)
let transform image =
  Image.foldi ~init:image image ~f:(fun ~x ~y img p ->
    let maxval = Image.max_val image in
    let currval = Pixel.red p // maxval in
    let newval = if Float.O.(0.5 < currval) then maxval else 0 in
    let error = Pixel.red p - newval in
    if x - 1 > 0 && y + 1 < Image.height img
    then (
      let downleft = Image.get img ~x:(x - 1) ~y:(y + 1) |> Pixel.red in
      Image.set
        img
        ~x:(x - 1)
        ~y:(y + 1)
        ((error * 3 / 16) + downleft |> Pixel.of_int));
    if x + 1 < Image.width img
    then (
      let right = Image.get img ~x:(x + 1) ~y |> Pixel.red in
      Image.set img ~x:(x + 1) ~y ((error * 7 / 16) + right |> Pixel.of_int));
    if x + 1 < Image.width img && y + 1 < Image.height img
    then (
      let downright = Image.get img ~x:(x + 1) ~y:(y + 1) |> Pixel.red in
      Image.set
        img
        ~x:(x + 1)
        ~y:(y + 1)
        ((error / 16) + downright |> Pixel.of_int));
    if y + 1 < Image.height img
    then (
      let down = Image.get img ~x ~y:(y + 1) |> Pixel.red in
      Image.set img ~x ~y:(y + 1) ((error * 5 / 16) + down |> Pixel.of_int));
    Image.set img ~x ~y (newval |> Pixel.of_int);
    img)
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
