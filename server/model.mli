(* [timeid] is timestamp of a gamestate aka [world]
 * the inital world has timeid 0. When a player makes a change, the
 * most up-to-date world has timestamp 1 *)
type timeid = int;

(* [hp] stands for healthpoint *)
type hp = int;

(* score of a player *)
type score = int;

(* [precision] is the confidence level of hitting the target when
 * casting a spell
 * For example, precision of 95 i.e. 95% CI means 95 times out of 100 casts
 * the spell hits the target.
 * In other words, the higher [precision] is, the more times the spell
 * works.
 * Invariants: 0 < precision < 100
 * and by default, precision is 50. *)
type precision = int;

(* exit of each room in 50x50 grids *)
type exit = North | East | South | West

(* location of a room in grid system *)
type room_loc = int * int;

(* emotion of a player *)
type emotion = Happy | Sad | Angry | Scared

(* each room has location row by column based on 50x50 system.
 * The description includes how the room looks like but not the items
 * in the room. *)
type room = {
  rloc : room_loc;
  rdescr : string;
  rexits : exit list;
}

(* effect of a spell that could increase (positive int) or decrease
 * (negative int) health points, turn and precision of the current player.
 * effect on hint takes string hint input and print it to the player who
 * calls. *)
type effect =
  | EHP of hp
  | ETurn of int
  | EPrecision of precision
  | EHint of string
  | EEmotion of emotion

(* A spell is casted to act on an object. However, there are consequences of
 * casting specific spells.
 * An example of a spell:
 * incantation = "Expelliarmus"
 * description = "disarms your opponent"
 * effect = Turn 1
 * consequence = None
 * environment = None *)
type spell = {
  spincant: string;
  spdescr : string;
  speffect : effect;
  spconseq: effect option;
  spenv : emotion option;
}

(* A potion is used for a specific purpose on an object although it could have
 * side effects on it. *)
type potion = {
  poname : string;
  podescr : string;
  poeffect : potion_effect;
  poconseq : effect option;
}

(* attack is used by an animal or a police. It has a name and effect. *)
type attack = {
  atname : string;
  ateffect : effect;
}

(* types of items that can be stored in an inventory *)
type inventory_item =
  | IVSpell of spell
  | IVPotion of potion

(* player of the game *)
type player = {
  pid : int;
  pname : string;
  pdescr : string;
  ploc : room_loc;
  php : hp;
  pemotion : emotion;
  pscore : score;
  pinventory : inventory_item list;
}
(* mutable properties of player that could be changed in a turn.
 * Properties that are not changed are of type None. *)
type player_diff = {
  pdid : int option;
  pdloc : room_loc option;
  pdhp : hp option;
  pdemotion : emotion option;
  pdscore : score option;
  pdinventory : inventory_item list option;
}

(* types of players in the game. AI for one-player mode. *)
type players = User of player | AI of player

(* animal or fantastic beast *)
type animal = {
  aname : string;
  adescr : string;
  aattacks : attack list;
}

(* policeman who is trying to catch players *)
type policeman = {
  plmspells : spell list;
}

(* types of AIs in the game *)
type AI = Animal of animal | Police of policeman

(* possible types of items in a room *)
type item =
  | IPlayer of player
  | IAnimal of animal
  | IPolice of policeman
  | ISpell of spell
  | IPotion of potion

(* world represents game state.
 * id represents the most up-to-date timestamp the world currently is.
 * A world contains information about the room map, spell library,
 * potion library, animal library, police library, player library,
 * and dictionary of room associated with items in the room. *)
type world =  {
  wid : timeid;
  wrooms : room list;
  wspells : spell list;
  wpotions : potion list;
  wanimals : animal list;
  wpolice : policeman list;
  wplayers : players list;
  witems : room_loc * (item list) list;
}

(* diff represents changes that are made in a player's turn.
 * Invariant: only store players and rooms that changes.
 * Steady rooms and players must not be included in a diff. *)
type diff = {
  dplayers : player_diff list;
  ditems : room_loc * (item list) list;
}

