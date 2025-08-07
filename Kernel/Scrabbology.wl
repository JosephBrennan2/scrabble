SetDirectory[ParentDirectory @ DirectoryName @ $InputFileName];

Get[#]& /@ {
    "Scrabbology`ScrabbleHelper`",
    "Scrabbology`Scrabble-gorithm`",
    "Scrabbology`ScrabbleBoard`"
  (* .wl File Names. *)
};