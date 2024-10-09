(** State of the current game, which includes the game board and high
    score *)

type tile_state = {
  position : int;
  letter : char;
  is_selected : bool;
}
(** type [tile_state] represents the state of each tile*)

type s = {
  grid : tile_state list;
  input_words : string list;
  score : int;
  high_score : int;
  time : float;
  difficulty : int;
}
(** type [s] is a type representing state of the tile grid.*)

val init_difficult_state : unit -> s
(** [init_difficult_state] creates a random list of tiles, each with a
    given position, random letter, and is_selected field indicated
    whether the user has selected the tile. The [tile_state list] in the
    grid field is used whenever user chooses to play game on 'difficult'
    difficulty.*)

val init_easy_state : unit -> s
(** [init_easy_state] creates a random list of tiles, each with a given
    position, random letter, and is_selected field indicating whether
    the user has selected the tile. This tile_grid is used whenever user
    chooses to play game on 'easy' difficulty. *)

val init_medium_state : unit -> s
(** [init_medium_state] creates a random list of tiles, each with a
    given position, random letter, and is_selected field indicating
    whether the user has selected the tile. This tile_grid is used
    whenever user chooses to play game on 'medium' difficulty. *)

val some_letter : int -> char
(** [some_letter i] is a random uppercase alphabetic character. This is
    used to vary the frequency in which vowels appear on tiles between
    [init_easy_state] and [init_medium_state] (for
    [init_difficult_state], every letter has an equal chance of
    appearing on the grid). The smaller the value chosen for [i] is, the
    more likely [some_letter i] is a vowel (the inverse is also true). *)

val current_grid : s -> tile_state list
(** [current_grid s] is the grid of the game at state [s]*)

val at_tile_state : int -> s -> tile_state
(** [at_tile_state num st] is the tile_state of the tile with position
    [num] in state [st]. *)

val new_state : int -> s -> s
(** [new_state num st] is the tile_state of the tile with position [num]
    in state [st]. *)

val j : Yojson.Basic.t

val convert_json_to_dictionary : Yojson.Basic.t -> Yojson.Basic.t list
(**[convert_json_to_dictionary Yojson.Basic.t ] reads in a json file
   consisting of the string "" paired with the list of words and
   converts the list of words into a string list*)

val word_dictionary : string list
(** [word_dictionary] represents the dictionary of valid words in the
    boggle game, as read from the file dictionary.json. *)

val high_score : s -> int
(** [high_score] gives the high score of state [s] *)

val st_difficulty : s -> int
(** [st_difficulty] gives the difficulty of the state [s]. 0 corresponds
    to easy, 1 to medium, 2 to hard.*)

val input_words : s -> string list
(**[input_words] gives the list of words input in state [s]. *)

val add_word_to_list : s -> string -> s
(**[add_word_to_list st w] is the state with the word [w] added to the
   submitted word list only if the word isn't on the list yet. *)

val word_list_score_helper_easy : string list -> int -> int
(**[word_list_score_helper_easy list acc] is a helper function for
   word_list_score. It is used in both the easy and medium difficulty
   settings.*)

val word_list_score_helper_difficult : string list -> int -> int
(** [word_list_score_helper_difficult list acc] is a helper function for
    word_list_score. It is used in the difficult difficulty setting*)

val word_list_score : s -> int
(** [word_list_score st] is the current score for the list of submitted
    words. For easy difficulty, score is taken to be the sum of the
    number of letters in each word. For medium difficulty, score is the
    sum of number letters in each word plus the total number of inputted
    words. For hard difficulty, score is the sum of letter scores in all
    the submitted words.*)
