(* [timeid] is timestamp of a gamestate aka [world]
 * the inital world has timeid 0. When the first player makes a change, the
 * most up-to-date world has timestamp 1, and so on *)
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

(* [effect] of a spell could increase (positive int) or decrease
 * (negative int) health points, turn and precision of the current player.
 * [effect] on hint takes string hint input and print it to the player who
 * calls. For example,
 * EHP 10 represents increase 10 hp
 * ETurn 1 represents the player gets 1 more turn added to how many turns
 *   the player has left
 * EPrecision 10 represents the player's spell will be 10% more accurate
 *   than what it already is. If the precision is 50, it increases to 60%.
 * EHint "Use Patronas Charm" represents that the string hint will
 *   be printed on the player's screen.
 * EEmotion Happy represents the player's emotion will be changed to Happy *)
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

(* A potion is used for a specific purpose [poeffect] on an object although
 * it could have a consequence [poconseq] on the user *)
type potion = {
  poname : string;
  podescr : string;
  poeffect : potion_effect;
  poconseq : effect option;
}

(* [attack] is used by an animal or a police. It has a name and effect. *)
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

(* Explanation:
 * [world] represents a game state
 * [wid] is the most up-to-date timestamp [world] is
 * A world contains information about room map [wrooms],
 * spell library [wspells], potion library [wpotions], animal library [animals],
 * police library [wpolice], player library [wplayers],
 * and dictionary of room associated with items in the room [witems] *)
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

(* [diff] represents changes that are made in a player's turn.
 * Invariant: [dplayers] and [ditems] only store players and rooms that change.
 * Steady rooms and players must not be included in a [diff]. *)
type diff = {
  dplayers : player_diff list;
  ditems : room_loc * (item list) list;
}

