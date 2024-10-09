(* OCaml Library *)
open Graphics

(** [current_input] are the current tiles selected by the user, in
    order. If no tiles, are selected, [current_input] is empty. *)
let current_input = Stack.create ()

let background_color = ref Graphics.white
let set_background_color col = background_color := col
let init_easy = ref (State.init_easy_state ())
let init_medium = ref (State.init_medium_state ())
let init_hard = ref (State.init_difficult_state ())
let init_e i = i := State.init_easy_state ()
let init_m i = i := State.init_medium_state ()
let init_h i = i := State.init_difficult_state ()
let high_score_easy = ref 0
let high_score_medium = ref 0
let high_score_hard = ref 0

let high_score_update difficulty high =
  match difficulty with
  | "easy" ->
      if high > !high_score_easy then begin
        high_score_easy := high;
        "New High Score! "
      end
      else "Score: "
  | "medium" ->
      if high > !high_score_medium then begin
        high_score_medium := high;
        "New High Score! "
      end
      else "Score: "
  | "hard" ->
      if high > !high_score_hard then begin
        high_score_hard := high;
        "New High Score! "
      end
      else "Score: "
  | _ -> failwith "error"

(** [box_config] is the configuration of each box, including its [num]
    id, coordinates and colors*)

type box_config = {
  num : int;
  x : int;
  y : int;
  w : int;
  h : int;
  border_w : int;
  border_col : Graphics.color;
  b_col : Graphics.color;
}

(** [draw_text str x y] draws [str] at coordinates ([x],[y])*)
let draw_text str x y =
  moveto x y;
  draw_string str

(** [draw_box_outline bcf col] draws the outline for the box with color
    [col]*)
let draw_box_outline bcf col =
  Graphics.set_color col;
  draw_rect bcf.x bcf.y bcf.w bcf.h

(** [draw_box bcf] draws and fills a box represented by [bcf]*)
let draw_box bcf =
  draw_box_outline bcf Graphics.black;
  Graphics.set_color bcf.b_col;
  let border = bcf.border_w in
  Graphics.fill_rect (bcf.x + border) (bcf.y + border)
    (bcf.w - (2 * border))
    (bcf.h - (2 * border))

(** [draw_string_in_box str bcf col] draws the [str] in the center of a
    box represented by [bcf]*)
let draw_string_in_box str bcf col =
  let w, h = Graphics.text_size str in
  let y_pos = bcf.y + ((bcf.h - h) / 2) in
  Graphics.moveto (bcf.x + ((bcf.w - w) / 2)) y_pos;
  Graphics.set_color col;
  Graphics.set_text_size 200;
  Graphics.draw_string str

(** [create_grid nb_col n b] is a list of boxes with [nb_col] rows and
    columns*)
let rec create_grid nb_col n b pos =
  if n < 0 then []
  else
    let px = n mod nb_col and py = n / nb_col in
    let nx = b.x + (px * b.w) and ny = b.y + +(py * b.h) in
    let b1 = { b with x = nx; y = ny; num = pos } in
    b1 :: create_grid nb_col (n - 1) b (pos + 1)

let vb =
  let b =
    {
      num = 0;
      x = 120;
      y = 50;
      w = 62;
      h = 62;
      border_w = 2;
      border_col = Graphics.blue;
      b_col = Graphics.white;
    }
  in
  Array.of_list (create_grid 5 24 b 0)

(** [fill_grid lst] fills the grid represented by [lst] with random
    letters*)
let rec fill_grid lst (grd : State.tile_state list) =
  match (lst, grd) with
  | [], [] -> ()
  | h :: t, { letter; _ } :: tail ->
      draw_string_in_box (Char.escaped letter) h Graphics.black;
      fill_grid t tail
  | _ -> exit 0

(** [in_square x y bcf] is true if the coordinates ([x],[y]) are within
    the square represented by [bcf], otherwise it is false *)
let in_square x y bcf =
  let bx = bcf.x in
  let by = bcf.y in
  (x >= bx && x <= bx + bcf.w) && y >= by && y <= by + bcf.h

(** [change_color lst x y] changes the color of a white box represented
    by [bcf] to yellow if it is clicked on, or a yellow box to white if
    it is clicked on *)
let change_color x y (st : State.s) bcf : box_config =
  if in_square x y bcf && bcf.b_col = Graphics.white then begin
    Stack.push (State.at_tile_state bcf.num st) current_input;
    if !background_color = Graphics.white then
      { bcf with b_col = yellow }
    else { bcf with b_col = !background_color }
  end
  else if
    begin
      in_square x y bcf
      && !background_color = Graphics.white
      && bcf.b_col = Graphics.yellow
      && (Stack.top current_input).position = bcf.num
    end
  then
    let _ = Stack.pop current_input in
    { bcf with b_col = Graphics.white }
  else if
    begin
      in_square x y bcf
      && bcf.b_col = !background_color
      && (Stack.top current_input).position = bcf.num
    end
  then
    let _ = Stack.pop current_input in
    { bcf with b_col = Graphics.white }
  else bcf

let draw_new_box lst = Array.iter draw_box (Array.of_list lst)
let exit_text = "Press q to close this window."
let play_text = "Press p to play."
let play_easy_text = "Press e for easy mode."
let back_text = "Press m to go back to the menu."
let settings_text = "Press s to go to the settings page."
let red_text = "Press r to change background color to red."
let white_text = "Press w to change background color to white."
let green_text = "Press g to change background color to green."
let blue_text = "Press b to change background color to blue."
let yellow_text = "Press y to change background color to yellow."
let cyan_text = "Press c to change background color to cyan."
let magenta_text = "Press p to change background color to purple."
let play_medium_text = "Press n for normal mode."
let play_hard_text = "Press h for hard mode."
let instruction_text = "Press i to read the instructions."

let instruction1 =
  "Form words from the given letters and get highest score in 60 \
   seconds! "

let instruction2 =
  "Words can be formed using any letters on the board. Press letter "

let instruction3 =
  "tiles to select letters. Press the s key to submit a word once you"

let instruction4 =
  " have selected the letters of your word in order. Unselecting "

let instruction5 =
  "letters must be done in the order they were selected in. No \
   Duplicate words!"

let instruction6 =
  "Words are generally worth more points on harder difficulties than \
   easier difficulties. "

let instruction7 =
  "On easy, score is the sum of the number of letters in each word. "

let instruction8 =
  "On medium, score is the sum of the numbers of letters in each word"

let instruction9 = "plus the number of total words submitted. "

let instruction10 =
  "On hard, score is determined by the sum of point values "

let instruction11 = "that are unique for each letter."
let game_over = "GAME OVER!"
let submit_text = "Press s to submit current word."
let score_text st = "Score: " ^ Int.to_string (State.word_list_score st)
let time_text t = "Time: " ^ Int.to_string (int_of_float t)

(** [build_str] builds a string from selected tiles *)
let build_str s =
  Stack.fold
    (fun acc (i : State.tile_state) -> Char.escaped i.letter ^ acc)
    "" s

(** [wait_until_pressed] waits for an input to either submit a word,
    click a box, or quit the game *)
let wait_until_pressed lst st mode =
  let timer_status = { Unix.it_interval = 0.0; Unix.it_value = 60.0 } in
  let _ = Unix.setitimer Unix.ITIMER_REAL timer_status in
  let rec helper lst st poll =
    let current_time = Unix.getitimer ITIMER_REAL in
    let sec_left = current_time.it_value in
    if sec_left <= 0. then State.word_list_score st
    else begin
      set_color !background_color;
      fill_rect 288 425 200 10;
      set_color black;
      draw_text (time_text sec_left) 255 425;
      let event =
        wait_next_event
          (if poll then [ Key_pressed; Button_down; Poll ]
          else [ Key_pressed; Button_down ])
      in
      if event.button then begin
        let new_color_grid =
          List.map (change_color event.mouse_x event.mouse_y st) lst
        in
        draw_new_box new_color_grid;
        fill_grid (Array.to_list vb) (State.current_grid st);
        Unix.sleepf 0.3;
        helper new_color_grid st true
      end
      else if event.key = 's' && Stack.length current_input > 0 then begin
        let new_state =
          State.add_word_to_list st (build_str current_input)
        in
        Stack.clear current_input;
        Array.iter draw_box vb;
        let intial =
          match mode with
          | "easy" -> !init_easy
          | "medium" -> !init_medium
          | "hard" -> !init_hard
          | _ -> failwith "error"
        in
        fill_grid (Array.to_list vb) (State.current_grid intial);
        set_color !background_color;
        fill_rect 285 450 200 10;
        set_color black;
        draw_text (score_text new_state) 252 450;
        Unix.sleepf 0.3;
        helper (Array.to_list vb) new_state false
      end
      else if event.key = 'q' then exit 0
      else helper lst st true
    end
  in
  helper lst st true

let rec wait_until_pressed_lose_menu mode =
  let event2 = wait_next_event [ Key_pressed ] in
  if event2.key = 'a' then mode
  else if event2.key = 'm' then "menu" ^ mode
  else wait_until_pressed_lose_menu mode

let rec menu difficulty high =
  Random.self_init ();
  clear_graph ();

  let rec play mode =
    let lose mode score =
      clear_graph ();
      set_color !background_color;
      draw_rect 0 0 550 500;
      fill_rect 0 0 550 500;
      Graphics.set_color Graphics.black;
      Graphics.set_text_size 300;
      draw_text game_over 250 400;
      draw_text (high_score_update mode score) 200 350;
      draw_text (string_of_int score) 300 350;
      Graphics.set_text_size 100;
      draw_text "Press a to play same mode again." 188 325;
      draw_text back_text 190 300;
      match wait_until_pressed_lose_menu mode with
      | "easy" -> play "easy"
      | "medium" -> play "medium"
      | "hard" -> play "hard"
      | "menueasy" -> menu "easy" score
      | "menumedium" -> menu "medium" score
      | "menuhard" -> menu "hard" score
      | _ -> failwith "error"
    in
    clear_graph ();
    set_color !background_color;
    draw_rect 0 0 550 500;
    fill_rect 0 0 550 500;
    set_color Graphics.black;
    print_endline "play";
    Graphics.set_text_size 100;
    draw_text exit_text 195 400;
    draw_text submit_text 190 375;
    draw_text "Score: 0" 252 450;
    draw_text (time_text 60.) 255 425;
    Array.iter draw_box vb;
    match mode with
    | "easy" ->
        Random.self_init ();
        init_e init_easy;
        fill_grid (Array.to_list vb) (State.current_grid !init_easy);
        lose "easy"
          (wait_until_pressed (Array.to_list vb) !init_easy "easy")
    | "medium" ->
        Random.self_init ();
        init_m init_medium;
        fill_grid (Array.to_list vb) (State.current_grid !init_medium);
        lose "medium"
          (wait_until_pressed (Array.to_list vb) !init_medium "medium")
    | "hard" ->
        Random.self_init ();
        init_h init_hard;
        fill_grid (Array.to_list vb) (State.current_grid !init_hard);
        lose "hard"
          (wait_until_pressed (Array.to_list vb) !init_hard "hard")
    | _ -> failwith "error"
  in

  let rec wait_until_pressed_settings_menu () =
    set_color !background_color;
    draw_rect 0 0 550 500;
    fill_rect 0 0 550 500;
    set_color Graphics.black;
    draw_text white_text 100 350;
    draw_text red_text 100 325;
    draw_text yellow_text 100 300;
    draw_text green_text 100 275;
    draw_text cyan_text 100 250;
    draw_text magenta_text 100 225;
    draw_text blue_text 100 200;
    draw_text back_text 100 175;
    let event4 = wait_next_event [ Key_pressed ] in
    if event4.key = 'r' then begin
      set_background_color Graphics.red;
      set_color !background_color;
      draw_rect 0 0 550 500;
      fill_rect 0 0 550 500;
      wait_until_pressed_settings_menu ()
    end
    else if event4.key = 'w' then begin
      set_background_color Graphics.white;
      set_color !background_color;
      draw_rect 0 0 550 500;
      fill_rect 0 0 550 500;
      wait_until_pressed_settings_menu ()
    end
    else if event4.key = 'g' then begin
      set_background_color Graphics.green;
      set_color !background_color;
      draw_rect 0 0 550 500;
      fill_rect 0 0 550 500;
      wait_until_pressed_settings_menu ()
    end
    else if event4.key = 'b' then begin
      set_background_color Graphics.blue;
      set_color !background_color;
      draw_rect 0 0 550 500;
      fill_rect 0 0 550 500;
      wait_until_pressed_settings_menu ()
    end
    else if event4.key = 'y' then begin
      set_background_color Graphics.yellow;
      set_color !background_color;
      draw_rect 0 0 550 500;
      fill_rect 0 0 550 500;
      wait_until_pressed_settings_menu ()
    end
    else if event4.key = 'c' then begin
      set_background_color Graphics.cyan;
      set_color !background_color;
      draw_rect 0 0 550 500;
      fill_rect 0 0 550 500;
      wait_until_pressed_settings_menu ()
    end
    else if event4.key = 'p' then begin
      set_background_color Graphics.magenta;
      set_color !background_color;
      draw_rect 0 0 550 500;
      fill_rect 0 0 550 500;
      wait_until_pressed_settings_menu ()
    end
    else if event4.key = 'm' then menu "easy" 0
    else wait_until_pressed_settings_menu ()
  in
  let rec wait_until_pressed_instructions_menu () =
    let event3 = wait_next_event [ Key_pressed ] in
    if event3.key = 'm' then menu "easy" 0
    else wait_until_pressed_instructions_menu ()
  in
  let rec wait_until_pressed_play_menu () =
    let event2 = wait_next_event [ Key_pressed ] in
    if event2.key = 'e' then play "easy"
    else if event2.key = 'n' then play "medium"
    else if event2.key = 'h' then play "hard"
    else if event2.key = 'm' then menu "easy" 0
    else wait_until_pressed_play_menu ()
  in
  let rec wait_until_pressed_menu () =
    set_color !background_color;
    draw_rect 0 0 550 500;
    fill_rect 0 0 550 500;
    Graphics.set_color Graphics.black;
    Graphics.set_text_size 300;
    draw_text "BOGGLE: OCAML EDITION" 215 400;
    Graphics.set_text_size 100;
    ignore (high_score_update difficulty high);
    draw_text play_text 100 350;
    draw_text instruction_text 100 325;
    draw_text settings_text 100 300;
    draw_text exit_text 190 150;
    draw_text
      ("Easy High Score: " ^ Int.to_string !high_score_easy)
      100 275;
    draw_text
      ("Medium High Score: " ^ Int.to_string !high_score_medium)
      100 250;
    draw_text
      ("Hard High Score: " ^ Int.to_string !high_score_hard)
      100 225;
    let event = wait_next_event [ Key_pressed ] in
    if event.key = 'p' then begin
      clear_graph ();
      set_color !background_color;
      draw_rect 0 0 550 500;
      fill_rect 0 0 550 500;
      set_color Graphics.black;
      Graphics.set_text_size 100;
      draw_text play_easy_text 220 350;
      draw_text play_medium_text 220 325;
      draw_text play_hard_text 220 300;
      draw_text back_text 190 230;
      wait_until_pressed_play_menu ()
    end
    else if event.key = 'i' then begin
      clear_graph ();
      set_color !background_color;
      draw_rect 0 0 550 500;
      fill_rect 0 0 550 500;
      set_color Graphics.black;
      draw_text instruction1 10 350;
      draw_text instruction2 10 330;
      draw_text instruction3 10 310;
      draw_text instruction4 10 290;
      draw_text instruction5 10 270;
      draw_text instruction6 10 250;
      draw_text instruction7 10 230;
      draw_text instruction8 10 210;
      draw_text instruction9 10 190;
      draw_text instruction10 10 170;
      draw_text instruction11 10 150;
      draw_text back_text 190 100;
      wait_until_pressed_instructions_menu ()
    end
    else if event.key = 's' then begin
      clear_graph ();
      set_color !background_color;
      draw_rect 0 0 550 500;
      fill_rect 0 0 550 500;
      set_color Graphics.black;
      draw_text white_text 100 350;
      draw_text red_text 100 325;
      draw_text yellow_text 100 300;
      draw_text green_text 100 275;
      draw_text cyan_text 100 250;
      draw_text magenta_text 100 225;
      draw_text blue_text 100 200;
      wait_until_pressed_settings_menu ()
    end
    else if event.key = 'q' then exit 0
    else wait_until_pressed_menu ()
  in
  wait_until_pressed_menu ()

let () =
  open_graph " 550x500";
  menu "easy" 0
