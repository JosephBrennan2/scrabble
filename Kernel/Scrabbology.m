SetDirectory[ParentDirectory @ DirectoryName @ $InputFileName];

Get[#]& /@ {
    "Scrabbology`ScrabbleHelper`",
    "Scrabbology`Scrabble-gorithm`",
    "Scrabbology`ScrabbleBoard`"
  (* .m File Names. *)
};