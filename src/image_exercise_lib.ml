open! Core

let command =
  Command.group
    ~summary:"A tool to perform various image manipulations"
    [ "grayscale", Grayscale.command
    ; "bluescreen", Blue_screen.command
    ; "blur", Blur.command
    ; "dither", Dither.command
    ; "stego", Steganography.command
    ; "edge", Edge.command
    ; "mosaic", Edge.command
    ]
;;
