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
  rdescr : string;
  rexits : exit list;
}

(* [effect] of a spell could increase (positive int) or decrease
 * (negative int) health points, turn and precision of the current player.
 * [effect] on hint takes string hint input and print it to the player who
 * calls. For example,
 * EHP 10 represents increase 10 hp
 * EPrecision 10 represents the player's spell will be 10% more accurate
 *   than what it already is. If the precision is 50, it increases to 60%.
 * EHint "Use Patronas Charm" represents that the string hint will
 *   be printed on the player's screen.
 * EEmotion Happy represents the player's emotion will be changed to Happy *)
type effect =
  | EHP of hp
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
  spid : int;
  spincant: string;
  spdescr : string;
  speffect : effect;
  spconseq: effect option;
  spcond : emotion option;
}

(* A potion is used for a specific purpose [poeffect] on an object although
 * it could have a consequence [poconseq] on the user *)
type potion = {
  poid : int;
  poname : string;
  podescr : string;
  poeffect : effect;
  poconseq : effect option;
}

(* [attack] is used by an animal or a police. It has a name and effect. *)
type attack = {
  atname : string;
  ateffect : effect;
}

(* types of items id that can be stored in an inventory *)
type inventory_item =
  | IVSpell of int
  | IVPotion of int

(* player of the game *)
type player = {
  pid : int;
  pname : string;
  pdescr : string;
}

(* animal or fantastic beast *)
type animal = {
  aid : int;
  aname : string;
  adescr : string;
  aattacks : attack list;
}

(* policeman who is trying to catch players *)
type policeman = {
  plmid : int;
  plmspells : spell list;
}

(* types of an item's id *)
type id =
  | IDPolice of int
  | IDAnimal of int
  | IDPlayer of int
  | IDPotion of int
  | IDSpell of int

(* difference that can occur in inventory *)
type diff_inv = Remove of inventory_item | Add of inventory_item

(* fields that can be updated in a move *)
type mut_AI = {
  id : id;
  newloc : room_loc;
  hp : hp;
  emotion : emotion option;
  score : score;
  inventory : diff_inv list;
  precision : int;
}

(* possible types of items in a room *)
type item =
  | IPlayer of mut_AI
  | IAnimal of mut_AI
  | IPolice of mut_AI
  | ISpell of int
  | IPotion of int

(* difference that can occur in a room *)
type diff_item = Remove of id | Add of id | Change of item

module LibMap = Map.Make (
    struct
      type t = int
      let compare e1 e2 = compare e1 e2
    end )

module RoomMap = Map.Make (
    struct
      type t = room_loc
      let compare (x1,x2) (y1,y2) = compare x1 y1
    end )

open LibMap
open RoomMap

(* Explanation:
 * [world] represents a game state
 * [wid] is the most up-to-date timestamp [world] is
 * A world contains information about room map [wrooms],
 * spell library [wspells], potion library [wpotions], animal library [animals],
 * police library [wpolice], player library [wplayers],
 * and dictionary of room associated with items in the room [witems] *)
type world =  {
  wid : timeid;
  wrooms : room RoomMap.t;
  wspells : spell LibMap.t;
  wpotions : potion LibMap.t;
  wanimals : animal LibMap.t;
  wpolice : policeman LibMap.t;
  wplayers : players LibMap.t;
  witems : (item list) RoomMap.t;
}

(* [diff] represents changes that are made in a player's turn.
 * Invariant: [dplayers] and [ditems] only store players and rooms that change.
 * Steady rooms and players must not be included in a [diff]. *)
type diff = {
  ditems : room_loc * (diff_item list) list option;
}

