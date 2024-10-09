(******* TEST PLAN *********)
(** First, after opening the necessary modules and declaring
    pretty-printing functions, we conduct randomized testing (with
    QCheck) on State.some_letter to test that [some_letter i] maintains
    the property of being an alphabetic uppercase char. Next, we conduct
    both black-box testing and glass-box testing on
    [State.input_new a st] to ensure that it properly appends a
    newly-inputted word in the input_words field of the given state. We
    also conduct testing on [State.word_list_score],
    [State.add_word_to_list] to ensure they are implemented properly and
    produce the correct outputs. Next, we conduct randomized testing on
    expression [char_of_int (Random.int 25 + 65)] (which is an
    expression that is frequently used to output random letters in the
    Boggle tiles in the Boggle and State modules) to ensure that it
    maintains the property of being an uppercase alphabetic character.
    Also, lots of things in the GUI had to just be testing by doing make
    play, and are hard to test in the suite.*)

open OUnit2
open State

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: t' ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_string (s : string) = "\"" ^ s ^ "\""

let letter_lst =
  [
    'A';
    'B';
    'C';
    'D';
    'E';
    'F';
    'G';
    'H';
    'I';
    'J';
    'K';
    'L';
    'M';
    'N';
    'O';
    'P';
    'Q';
    'R';
    'S';
    'T';
    'U';
    'V';
    'W';
    'X';
    'Y';
    'Z';
  ]

let some_letter_check a = List.mem (some_letter a) letter_lst

let input_new_state a st =
  let new_s = add_word_to_list st a in
  new_s.input_words

let rand_char_is_alpha a = List.mem (char_of_int (a + 65)) letter_lst

let test_char_of_int_rand =
  QCheck.Test.make ~count:1000
    ~name:
      "Random char generated by char_of_int(Random.int 25 + 65) is \
       uppercase letter"
    QCheck.(0 -- 25)
    rand_char_is_alpha

let some_letter_test =
  QCheck.Test.make ~count:1000
    ~name:"Random char generated by some_letter is uppercase letter"
    QCheck.int some_letter_check

let input_new_test name a state expout =
  name >:: fun _ ->
  assert_equal ~printer:(pp_list pp_string) expout
    (input_new_state a state)

let word_list_score_test
    (name : string)
    (st : s)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int (word_list_score st)
    expected_output

let state_difficulty_test
    (name : string)
    (st : s)
    (expected_output : int) =
  name >:: fun _ -> assert_equal (st_difficulty st) expected_output

let add_word_test
    (name : string)
    (st : s)
    (add : string)
    (expected_member : string) =
  name >:: fun _ ->
  assert (
    List.mem expected_member (input_words (add_word_to_list st add)))

let add_word_test_false
    (name : string)
    (st : s)
    (add : string)
    (expected_member : string) =
  name >:: fun _ ->
  assert (
    not
      (List.mem expected_member (input_words (add_word_to_list st add))))

(** [updated_state st f s t] updates a state [st] by inputting three
    words into the [input_words] field of [st]. *)
let updated_state st first sec third =
  let f = add_word_to_list st first in
  let s = add_word_to_list f sec in
  add_word_to_list s third

let updated_easy_state () =
  updated_state (init_easy_state ()) "course" "work" "over"

let updated_medium_state () =
  updated_state (init_medium_state ()) "nice" "treat" "because"

let updated_difficult_state () =
  updated_state (init_difficult_state ()) "tried" "and" "failed"

let state_tests =
  [
    input_new_test "update empty input_words field in easy_state" "old"
      (init_easy_state ()) [ "old" ];
    input_new_test "update empty input_words field in medium_state"
      "lore" (init_medium_state ()) [ "lore" ];
    input_new_test "update empty input_words field in difficult_state"
      "mine"
      (init_difficult_state ())
      [ "mine" ];
    input_new_test
      "empty string is not an inputtable word in easy_state" ""
      (init_easy_state ()) [];
    input_new_test
      "empty string is not an inputtable word in medium_state" ""
      (init_medium_state ()) [];
    input_new_test
      "empty string is not an inputtable word in difficult_state" ""
      (init_difficult_state ())
      [];
    input_new_test
      "adding an invalid word in updated_easy_state yields the \
       original list"
      "aaaaaa" (updated_easy_state ())
      [ "course"; "work"; "over" ];
    input_new_test
      "adding an invalid word in updated_easy_state yields the \
       original list"
      "123" (updated_easy_state ())
      [ "course"; "work"; "over" ];
    input_new_test "update input_words field in updated_easy_state"
      "told" (updated_easy_state ())
      [ "course"; "work"; "over"; "told" ];
    input_new_test "update input_words field in updated_easy_state"
      "help" (updated_easy_state ())
      [ "course"; "work"; "over"; "help" ];
    input_new_test "update input_words field in updated_easy_state"
      "magnificent" (updated_easy_state ())
      [ "course"; "work"; "over"; "magnificent" ];
    input_new_test "update input_words field in updated_easy_state"
      "gene" (updated_easy_state ())
      [ "course"; "work"; "over"; "gene" ];
    input_new_test "update input_words field in updated_easy_state"
      "hardly" (updated_easy_state ())
      [ "course"; "work"; "over"; "hardly" ];
    input_new_test "update input_words field in updated_easy_state"
      "instantaneous" (updated_easy_state ())
      [ "course"; "work"; "over"; "instantaneous" ];
    input_new_test
      "adding an invalid word in updated_medium_state yields the \
       original list"
      "bbbbb"
      (updated_medium_state ())
      [ "nice"; "treat"; "because" ];
    input_new_test
      "adding an invalid word in updated_medium_state yields the \
       original list"
      "145"
      (updated_medium_state ())
      [ "nice"; "treat"; "because" ];
    input_new_test "update input_words field in updated_medium_state"
      "seldom"
      (updated_medium_state ())
      [ "nice"; "treat"; "because"; "seldom" ];
    input_new_test "update input_words field in updated_medium_state"
      "hey"
      (updated_medium_state ())
      [ "nice"; "treat"; "because"; "hey" ];
    input_new_test "update input_words field in updated_medium_state"
      "magnanimous"
      (updated_medium_state ())
      [ "nice"; "treat"; "because"; "magnanimous" ];
    input_new_test "update input_words field in updated_medium_state"
      "delusional"
      (updated_medium_state ())
      [ "nice"; "treat"; "because"; "delusional" ];
    input_new_test "update input_words field in updated_medium_state"
      "greedy"
      (updated_medium_state ())
      [ "nice"; "treat"; "because"; "greedy" ];
    input_new_test "update input_words field in updated_medium_state"
      "peace"
      (updated_medium_state ())
      [ "nice"; "treat"; "because"; "peace" ];
    input_new_test
      "adding an invalid word in updated_difficult_state yields the \
       original list"
      "notaword"
      (updated_difficult_state ())
      [ "tried"; "and"; "failed" ];
    input_new_test "update input_words field in updated_difficult_state"
      "way"
      (updated_difficult_state ())
      [ "tried"; "and"; "failed"; "way" ];
    input_new_test "update input_words field in updated_difficult_state"
      "crispy"
      (updated_difficult_state ())
      [ "tried"; "and"; "failed"; "crispy" ];
    input_new_test "update input_words field in updated_difficult_state"
      "cough"
      (updated_difficult_state ())
      [ "tried"; "and"; "failed"; "cough" ];
    input_new_test "update input_words field in updated_difficult_state"
      "blonde"
      (updated_difficult_state ())
      [ "tried"; "and"; "failed"; "blonde" ];
    input_new_test
      "Cannot append already-inputted word twice in input_words field \
       in updated_easy_state"
      "course" (updated_easy_state ())
      [ "course"; "work"; "over" ];
    input_new_test
      "Cannot append already-inputted word twice in input_words field \
       in updated_medium_state"
      "nice"
      (updated_medium_state ())
      [ "nice"; "treat"; "because" ];
    input_new_test
      "Cannot append already-inputted word twice in input_words field \
       in updated_difficult_state"
      "tried"
      (updated_difficult_state ())
      [ "tried"; "and"; "failed" ];
    input_new_test
      "Cannot append empty string to input_words field in \
       updated_easy_state"
      "" (updated_easy_state ())
      [ "course"; "work"; "over" ];
    input_new_test
      "Cannot append empty string to input_words field in \
       updated_medium_state"
      ""
      (updated_medium_state ())
      [ "nice"; "treat"; "because" ];
    input_new_test
      "Cannot append empty string to input_words field in \
       updated_difficult_state"
      ""
      (updated_difficult_state ())
      [ "tried"; "and"; "failed" ];
    state_difficulty_test "easy init difficulty test"
      (init_easy_state ()) 0;
    state_difficulty_test "medium init difficulty test"
      (init_medium_state ()) 1;
    state_difficulty_test "difficult init difficulty test"
      (init_difficult_state ())
      2;
    state_difficulty_test "easy difficulty  test"
      (updated_easy_state ()) 0;
    state_difficulty_test "medium difficulty  test"
      (updated_medium_state ())
      1;
    state_difficulty_test "difficult difficulty test"
      (updated_difficult_state ())
      2;
    state_difficulty_test "easy difficulty  test update"
      (updated_state (updated_easy_state ()) "one" "two" "three")
      0;
    state_difficulty_test "easy difficulty  test update 2"
      (updated_state (updated_easy_state ()) "g9u39nu4t" "f9304fuh349f"
         "sdf9u8b9384u")
      0;
    add_word_test "init test" (init_easy_state ()) "test" "test";
    add_word_test_false "init test false" (init_easy_state ()) "green"
      "blue";
    add_word_test_false "init test add wrong word" (init_easy_state ())
      "notarealword" "notarealword";
    add_word_test " add true word old word" (updated_easy_state ())
      "one" "course";
    add_word_test "add true word new word" (updated_easy_state ()) "one"
      "one";
    add_word_test_false "add true word test false word"
      (updated_easy_state ()) "one" "foewihfewoijfwe";
    add_word_test " add false word old word" (updated_easy_state ())
      "vuoshfohuwe" "course";
    add_word_test_false "add false word new word"
      (updated_easy_state ()) "woejfwoeifwoei" "woejfwoeifwoei";
    add_word_test "add multiple words"
      (add_word_to_list (updated_easy_state ()) "two")
      "one" "two";
    add_word_test_false "add multiple words false"
      (add_word_to_list (updated_easy_state ()) "two")
      "one" "three";
    word_list_score_test "easy empty init list score"
      (init_easy_state ()) 0;
    word_list_score_test "medium empty init list score"
      (init_medium_state ()) 0;
    word_list_score_test "hard empty init list score"
      (init_difficult_state ())
      0;
    word_list_score_test "easy init 1 word"
      (add_word_to_list (init_easy_state ()) "aardvark")
      8;
    word_list_score_test "medium init 1 word"
      (add_word_to_list (init_medium_state ()) "aardvark")
      9;
    word_list_score_test "difficult init 1 word"
      (add_word_to_list (init_difficult_state ()) "aardvark")
      10;
    word_list_score_test "easy add 1 wrong word"
      (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
      0;
    word_list_score_test "medium add 1 wrong word"
      (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
      0;
    word_list_score_test "difficult add 1 wrong word"
      (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
      0;
    word_list_score_test "easy list score test" (updated_easy_state ())
      14;
    word_list_score_test "easy list score test add"
      (updated_state (updated_easy_state ()) "one" "two" "three")
      25;
    word_list_score_test "easy list score test add 2"
      (updated_state (updated_easy_state ()) "hat" "cat" "bat")
      23;
    word_list_score_test "easy list score test add twice"
      (updated_state
         (updated_state (updated_easy_state ()) "one" "two" "three")
         "four" "five" "six")
      36;
    word_list_score_test "easy list score test add duplicate"
      (updated_state (updated_easy_state ()) "one" "one" "three")
      22;
    word_list_score_test "easy list score test add invalid"
      (updated_state (updated_easy_state ()) "one"
         "f024uh09uhf90eshf09uhse09ufhse0hu " "three")
      22;
    word_list_score_test "medium list score test"
      (updated_medium_state ())
      19;
    word_list_score_test "medium list score test add"
      (updated_state (updated_medium_state ()) "one" "two" "three")
      33;
    word_list_score_test "medium list score test add 2"
      (updated_state (updated_medium_state ()) "hat" "cat" "bat")
      31;
    word_list_score_test "medium list score test add duplicate"
      (updated_state (updated_medium_state ()) "one" "one" "three")
      29;
    word_list_score_test "medium list score test add twice"
      (updated_state
         (updated_state (updated_medium_state ()) "one" "two" "three")
         "four" "five" "six")
      47;
    word_list_score_test "medium list score test add invalid"
      (updated_state
         (updated_medium_state ())
         "one" "ung09hgre97gh90734h09384h30948gh8934gh" "three")
      29;
    word_list_score_test "difficult list score test"
      (updated_difficult_state ())
      14;
    word_list_score_test "difficult list score test add"
      (updated_state (updated_difficult_state ()) "one" "two" "three")
      26;
    word_list_score_test "difficult list score test add twice"
      (updated_state
         (updated_state
            (updated_difficult_state ())
            "one" "two" "three")
         "four" "five" "six")
      40;
    word_list_score_test
      "difficult list score test add twice 1 invalids"
      (updated_state
         (updated_state
            (updated_difficult_state ())
            "one" "two" "three")
         "jgsdg" "fdsnjdfj" "sdgsg")
      26;
    word_list_score_test
      "difficult list score test add twice 4 invalids"
      (updated_state
         (updated_state
            (updated_difficult_state ())
            "gdslsdg" "twldslsgo" "sdfls")
         "skdlfsdk" "five" "six")
      24;
    word_list_score_test "difficult list score test add invalid"
      (updated_state
         (updated_difficult_state ())
         "pweoifpwoeijfowe" "five" "six")
      24;
    word_list_score_test
      "difficult list score test add twice 4 invalids empty"
      (updated_state
         (updated_state (updated_difficult_state ()) "" "" "")
         "" "five" "six")
      24;
    word_list_score_test
      "difficult list score test add three and 3 invalids empty"
      (updated_state
         (updated_state (updated_difficult_state ()) "" "" "")
         "fun" "five" "six")
      27;
    word_list_score_test
      "difficult list score test add four and 2 invalids empty"
      (updated_state
         (updated_state (updated_difficult_state ()) "" "" "thousand")
         "fun" "five" "six")
      35;
    word_list_score_test
      "difficult list score test add five and 1 invalids empty"
      (updated_state
         (updated_state
            (updated_difficult_state ())
            "" "cripple" "thousand")
         "fun" "five" "six")
      42;
    word_list_score_test "difficult list score test add six"
      (updated_state
         (updated_state
            (updated_difficult_state ())
            "volcano" "cripple" "thousand")
         "fun" "five" "six")
      50;
    word_list_score_test
      "difficult list score test add six with basketball"
      (updated_state
         (updated_state
            (updated_difficult_state ())
            "basketball" "cripple" "thousand")
         "fun" "five" "six")
      53;
    word_list_score_test
      "difficult list score test add six with permanent"
      (updated_state
         (updated_state
            (updated_difficult_state ())
            "permanent" "cripple" "thousand")
         "fun" "five" "six")
      51;
    word_list_score_test "medium add 1 wrong word"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
         "aardvark")
      9;
    word_list_score_test "medium add 2 repeat word"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "aardvark")
         "aardvark")
      9;
    word_list_score_test "medium add 1 wrong word"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
         "sdnfldfskdf")
      0;
    word_list_score_test "medium add 3 wrong word"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
            "sdnfldfskdf")
         "fbsdlsf")
      0;
    word_list_score_test "medium add 4 wrong word"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
               "sdnfldfskdf")
            "fbsdlsf")
         "fsnalnsfk")
      0;
    word_list_score_test "easy add 1 wrong word"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
         "aardvark")
      8;
    word_list_score_test "easy add 2 repeat word"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "aardvark")
         "aardvark")
      8;
    word_list_score_test "easy add 1 wrong word"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
         "sdnfldfskdf")
      0;
    word_list_score_test "easy add 3 wrong word"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
            "sdnfldfskdf")
         "fbsdlsf")
      0;
    word_list_score_test "easy add 4 wrong word"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
               "sdnfldfskdf")
            "fbsdlsf")
         "fsnalnsfk")
      0;
    word_list_score_test "hard add 1 wrong word"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
         "aardvark")
      10;
    word_list_score_test "hard add 2 repeat word"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "aardvark")
         "aardvark")
      10;
    word_list_score_test "hard add 1 wrong word"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
         "sdnfldfskdf")
      0;
    word_list_score_test "medium add 3 wrong word"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (init_difficult_state ())
               "oewifjoweifjwe")
            "sdnfldfskdf")
         "fbsdlsf")
      0;
    word_list_score_test "medium add 4 wrong word"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list
                  (init_difficult_state ())
                  "oewifjoweifjwe")
               "sdnfldfskdf")
            "fbsdlsf")
         "fsnalnsfk")
      0;
    word_list_score_test "medium add 1 wrong word 2"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
         "word")
      5;
    word_list_score_test "medium add 2 repeat word 2"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "word")
         "word")
      5;
    word_list_score_test "medium add 1 wrong word 2"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
         "fdsnlsndf")
      0;
    word_list_score_test "medium add 3 wrong word 2"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
            "fdsnlsndf")
         "fbsdlsf")
      0;
    word_list_score_test "medium add 4 wrong word 2"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
               "fdsnlsndf")
            "fbsdlsf")
         "fsnalnsfk")
      0;
    word_list_score_test "easy add 1 wrong word 2"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
         "word")
      4;
    word_list_score_test "easy add 2 repeat word 2"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "word")
         "word")
      4;
    word_list_score_test "easy add 1 wrong word 2"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
         "fdsnlsndf")
      0;
    word_list_score_test "easy add 3 wrong word 2"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
            "fdsnlsndf")
         "fbsdlsf")
      0;
    word_list_score_test "easy add 4 wrong word 2"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
               "sdnfldfskdf")
            "fbsdlsf")
         "fsnalnsfk")
      0;
    word_list_score_test "hard add 1 wrong word 2"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
         "word")
      5;
    word_list_score_test "hard add 2 repeat word 2"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "word")
         "word")
      5;
    word_list_score_test "hard add 1 wrong word 2"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
         "fdsnlsndf")
      0;
    word_list_score_test "medium add 3 wrong word 2"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (init_difficult_state ())
               "oewifjoweifjwe")
            "fdsnlsndf")
         "fbsdlsf")
      0;
    word_list_score_test "medium add 4 wrong word 2"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list
                  (init_difficult_state ())
                  "oewifjoweifjwe")
               "fdsnlsndf")
            "fbsdlsf")
         "fsnalnsfk")
      0;
    word_list_score_test "medium add 1 wrong word 3"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
         "ward")
      5;
    word_list_score_test "medium add 2 repeat word 3"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "ward")
         "ward")
      5;
    word_list_score_test "medium add 1 wrong word 3"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
         "k;fksdf")
      0;
    word_list_score_test "medium add 3 wrong word 3"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
            "k;fksdf")
         "fbsdlsf")
      0;
    word_list_score_test "medium add 4 wrong word 3"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
               "k;fksdf")
            "fbsdlsf")
         "fsnalnsfk")
      0;
    word_list_score_test "easy add 1 wrong word 3"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
         "ward")
      4;
    word_list_score_test "easy add 2 repeat word 3"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "ward")
         "ward")
      4;
    word_list_score_test "easy add 1 wrong word 3"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
         "k;fksdf")
      0;
    word_list_score_test "easy add 3 wrong word 3"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
            "sdnfldk;fksdffskdf")
         "fbsdlsf")
      0;
    word_list_score_test "easy add 4 wrong word 3"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
               "sdnfldfskdf")
            "k;fksdf")
         "fsnalnsfk")
      0;
    word_list_score_test "hard add 1 wrong word 3"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
         "ward")
      5;
    word_list_score_test "hard add 2 repeat word 3"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "ward")
         "ward")
      5;
    word_list_score_test "hard add 1 wrong word 3"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
         "k;fksdf")
      0;
    word_list_score_test "medium add 3 wrong word 3"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (init_difficult_state ())
               "oewifjoweifjwe")
            "sdnflk;fksdfdfskdf")
         "fbsdlsf")
      0;
    word_list_score_test "medium add 4 wrong word 3"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list
                  (init_difficult_state ())
                  "oewifjoweifjwe")
               "k;fksdf")
            "fbsdlsf")
         "fsnalnsfk")
      0;
    word_list_score_test "medium add 1 wrong word 4"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
         "draw")
      5;
    word_list_score_test "medium add 2 repeat word 4"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "draw")
         "draw")
      5;
    word_list_score_test "medium add 1 wrong word 4"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
         "lsfmamsf")
      0;
    word_list_score_test "medium add 3 wrong word 4"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
            "lsfmamsf")
         "fbsdlsf")
      0;
    word_list_score_test "medium add 4 wrong word 4"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
               "sdnfldfskdf")
            "lsfmamsf")
         "fsnalnsfk")
      0;
    word_list_score_test "easy add 1 wrong word 4"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
         "draw")
      4;
    word_list_score_test "easy add 2 repeat word 4"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "draw")
         "draw")
      4;
    word_list_score_test "easy add 1 wrong word 4"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
         "lsfmamsf")
      0;
    word_list_score_test "easy add 3 wrong word 4"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
            "lsfmamsf")
         "fbsdlsf")
      0;
    word_list_score_test "easy add 4 wrong word 4"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
               "lsfmamsf")
            "fbsdlsf")
         "fsnalnsfk")
      0;
    word_list_score_test "hard add 1 wrong word 4"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
         "draw")
      5;
    word_list_score_test "hard add 2 repeat word 4"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "draw")
         "draw")
      5;
    word_list_score_test "hard add 1 wrong word 4"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
         "lsfmamsf")
      0;
    word_list_score_test "medium add 3 wrong word 4"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_difficult_state ()) "lsfmamsf")
            "sdnfldfskdf")
         "fbsdlsf")
      0;
    word_list_score_test "medium add 4 wrong word 4"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list
                  (init_difficult_state ())
                  "oewifjoweifjwe")
               "lsfmamsf")
            "fbsdlsf")
         "fsnalnsfk")
      0;
    word_list_score_test "medium add 1 wrong word 5"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
         "brow")
      5;
    word_list_score_test "medium add 2 repeat word 5"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "brow")
         "brow")
      5;
    word_list_score_test "medium add 1 wrong word 5"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
         "k;ndsf;klnsfds")
      0;
    word_list_score_test "medium add 3 wrong word 5"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
            "k;ndsf;klnsfds")
         "fbsdlsf")
      0;
    word_list_score_test "medium add 4 wrong word 5"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
               "k;ndsf;klnsfds")
            "fbsdlsf")
         "fsnalnsfk")
      0;
    word_list_score_test "easy add 1 wrong word 5"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
         "brow")
      4;
    word_list_score_test "easy add 2 repeat word 5"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "brow")
         "brow")
      4;
    word_list_score_test "easy add 1 wrong word 5"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
         "k;ndsf;klnsfds")
      0;
    word_list_score_test "easy add 3 wrong word 5"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
            "k;ndsf;klnsfds")
         "fbsdlsf")
      0;
    word_list_score_test "easy add 4 wrong word 5"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
               "k;ndsf;klnsfds")
            "fbsdlsf")
         "fsnalnsfk")
      0;
    word_list_score_test "hard add 1 wrong word 5"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
         "brow")
      5;
    word_list_score_test "hard add 2 repeat word 5"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "brow")
         "brow")
      5;
    word_list_score_test "hard add 1 wrong word 5"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
         "k;ndsf;klnsfds")
      0;
    word_list_score_test "medium add 3 wrong word 5"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (init_difficult_state ())
               "oewifjoweifjwe")
            "k;ndsf;klnsfds")
         "fbsdlsf")
      0;
    word_list_score_test "medium add 4 wrong word 5"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list
                  (init_difficult_state ())
                  "oewifjoweifjwe")
               "sdnfldfskdf")
            "k;ndsf;klnsfds")
         "fsnalnsfk")
      0;
    word_list_score_test "medium add 1 wrong word 6"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
         "crow")
      5;
    word_list_score_test "medium add 2 repeat word 6"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "crow")
         "crow")
      5;
    word_list_score_test "medium add 1 wrong word 6"
      (add_word_to_list
         (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
         "tehiosfolefs")
      0;
    word_list_score_test "medium add 3 wrong word 6"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
            "k;tehiosfolefs;klnsfds")
         "fbsdlsf")
      0;
    word_list_score_test "medium add 4 wrong word 6"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list (init_medium_state ()) "oewifjoweifjwe")
               "k;ndsf;klnsfds")
            "tehiosfolefs")
         "fsnalnsfk")
      0;
    word_list_score_test "easy add 1 wrong word 6"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
         "crow")
      4;
    word_list_score_test "easy add 2 repeat word 6"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "crow")
         "crow")
      4;
    word_list_score_test "easy add 1 wrong word 6"
      (add_word_to_list
         (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
         "k;tehiosfolefs;klnsfds")
      0;
    word_list_score_test "easy add 3 wrong word 6"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
            "k;tehiosfolefs;klnsfds")
         "fbsdlsf")
      0;
    word_list_score_test "easy add 4 wrong word 6"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list (init_easy_state ()) "oewifjoweifjwe")
               "k;ndsf;klnsfds")
            "tehiosfolefs")
         "fsnalnsfk")
      0;
    word_list_score_test "hard add 1 wrong word 6"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
         "crow")
      5;
    word_list_score_test "hard add 2 repeat word 6"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "crow")
         "crow")
      5;
    word_list_score_test "hard add 1 wrong word 6"
      (add_word_to_list
         (add_word_to_list (init_difficult_state ()) "oewifjoweifjwe")
         "k;tehiosfolefs;klnsfds")
      0;
    word_list_score_test "medium add 3 wrong word 6"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list (init_difficult_state ()) "tehiosfolefs")
            "k;ndsf;klnsfds")
         "fbsdlsf")
      0;
    word_list_score_test "medium add 4 wrong word 6"
      (add_word_to_list
         (add_word_to_list
            (add_word_to_list
               (add_word_to_list
                  (init_difficult_state ())
                  "tehiosfolefs")
               "sdnfldfskdf")
            "k;ndsf;klnsfds")
         "fsnalnsfk")
      0;
  ]

let qcheck_tests =
  List.map QCheck_ounit.to_ounit2_test
    [ some_letter_test; test_char_of_int_rand ]

let t_suite =
  "test suite for final project"
  >::: List.flatten [ state_tests; qcheck_tests ]

let _ = run_test_tt_main t_suite
