open Yojson.Basic.Util

let j = Yojson.Basic.from_file "data/dictionary.json"

type tile_state = {
  position : int;
  letter : char;
  is_selected : bool;
}

type s = {
  grid : tile_state list;
  input_words : string list;
  score : int;
  high_score : int;
  time : float;
  difficulty : int;
}

let st_difficulty st = st.difficulty
let high_score st = st.high_score
let input_words st = st.input_words

(* [convert_json_to_dictionary] converts a Yojson.Basic.t to a list *)
let convert_json_to_dictionary j = to_list j
let word_dictionary = List.map to_string (convert_json_to_dictionary j)

let add_word_to_list st (w : string) =
  let lower_w = String.lowercase_ascii w in
  if
    (not (List.mem lower_w st.input_words))
    && List.mem lower_w word_dictionary
  then { st with input_words = st.input_words @ [ lower_w ] }
  else st

let rec word_list_score_helper_easy (lst : string list) (acc : int) :
    int =
  match lst with
  | [] -> acc
  | h :: t -> word_list_score_helper_easy t (acc + String.length h)

(* Helper function that turns a string into a list of one length
   strings. *)
let rec string_to_string_list (str : string) (acc : string list) :
    string list =
  match str with
  | "" -> acc
  | x when String.length x = 1 -> acc @ [ String.sub x 0 1 ]
  | x ->
      string_to_string_list
        (String.sub x 1 (String.length x - 1))
        (acc @ [ String.sub x 0 1 ])

(* Helper function that takes in a single lowercase letter expressed as
   a string and outputs its point value. *)
let letter_score (l : string) : int =
  match l with
  | "" -> 0
  | "a" | "b" | "c" | "e" | "f" | "h" | "i" | "l" | "m" | "n" | "o"
  | "p" | "r" | "s" | "t" | "u" | "d" ->
      1
  | "g" | "k" | "v" | "w" | "y" -> 2
  | "j" | "x" | "z" -> 3
  | "q" -> 4
  | _ -> 0

(* Helper function that gives a word's score (sum of the values of each
   individual letter) given a word expressed as a string list where each
   entry corresponds to a single character. This is used only in the
   hard mode score calculations.*)
let rec word_score (str_lst : string list) (acc : int) : int =
  match str_lst with
  | [] -> acc
  | h :: t -> word_score t (acc + letter_score h)

(* Helper function that gives the total score for a list of valid
   inputted words. It is used for hard difficulty only.*)
let rec word_list_score_helper_difficult (lst : string list) (acc : int)
    : int =
  match lst with
  | [] -> acc
  | h :: t ->
      word_list_score_helper_difficult t
        (acc + word_score (string_to_string_list h []) 0)

let word_list_score st =
  if st.difficulty = 0 then word_list_score_helper_easy st.input_words 0
  else if st.difficulty = 1 then
    word_list_score_helper_easy st.input_words
      (List.length st.input_words)
  else if st.difficulty = 2 then
    word_list_score_helper_difficult st.input_words 0
  else word_list_score_helper_difficult st.input_words 0

(** [init_difficult_state] creates a random list of tiles, each with a
    given position, random letter, and is_selected field indicated
    whether the user has selected the tile. The [tile_state list] in the
    grid field is used whenever user chooses to play game on 'difficult'
    difficulty.*)
let init_difficult_state () =
  let new_g =
    Random.self_init ();
    [
      {
        position = 0;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 1;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 2;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 3;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 4;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 5;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 6;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 7;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 8;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 9;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 10;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 11;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 12;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 13;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 14;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 15;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 16;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 17;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 18;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 19;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 20;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 21;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 22;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 23;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
      {
        position = 24;
        letter = char_of_int (Random.int 26 + 65);
        is_selected = false;
      };
    ]
  in
  {
    grid = new_g;
    input_words = [];
    score = 0;
    high_score = 0;
    time = 60.;
    difficulty = 2;
  }

let current_grid st = st.grid
let at_tile_state num st = List.nth st.grid num

let new_state num st =
  let old_tile_state = List.nth st.grid num in
  let boo = old_tile_state.is_selected in
  let new_tile_state = { old_tile_state with is_selected = not boo } in
  let new_grid =
    List.map
      (fun t -> if t.position = num then new_tile_state else t)
      st.grid
  in
  { st with grid = new_grid }

let random_letter_helper num =
  Random.self_init ();
  Random.int num

let some_letter random =
  Random.self_init ();
  let vowel_lst = [ 'A'; 'E'; 'I'; 'O'; 'U' ] in
  if random = 1 then List.nth vowel_lst (Random.int 5)
  else char_of_int (Random.int 26 + 65)

(** [init_easy_state] creates a random list of tiles, each with a given
    position, random letter, and is_selected field indicating whether
    the user has selected the tile. This tile_grid is used whenever user
    chooses to play game on 'easy' difficulty. *)
let init_easy_state () =
  Random.self_init ();
  let easy_g =
    [
      {
        position = 0;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 1;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 2;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 3;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 4;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 5;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 6;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 7;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 8;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 9;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 10;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 11;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 12;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 13;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 14;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 15;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 16;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 17;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 18;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 19;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 20;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 21;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 22;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 23;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
      {
        position = 24;
        letter = some_letter (random_letter_helper 5);
        is_selected = false;
      };
    ]
  in
  {
    grid = easy_g;
    input_words = [];
    score = 0;
    high_score = 0;
    time = 60.;
    difficulty = 0;
  }

(** [init_medium_state] creates a random list of tiles, each with a
    given position, random letter, and is_selected field indicating
    whether the user has selected the tile. This tile_grid is used
    whenever user chooses to play game on 'medium' difficulty. *)
let init_medium_state () =
  Random.self_init ();
  let medium_g =
    [
      {
        position = 0;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 1;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 2;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 3;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 4;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 5;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 6;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 7;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 8;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 9;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 10;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 11;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 12;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 13;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 14;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 15;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 16;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 17;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 18;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 19;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 20;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 21;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 22;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 23;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
      {
        position = 24;
        letter = some_letter (random_letter_helper 6);
        is_selected = false;
      };
    ]
  in
  {
    grid = medium_g;
    input_words = [];
    score = 0;
    high_score = 0;
    time = 60.;
    difficulty = 1;
  }
