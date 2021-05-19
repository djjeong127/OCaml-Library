let welcome = "Welcome to the Oakland, California Municipal Library (OCaML)"

(* These types are defined for you. You should not modify them *)
type catalog_item = Book of string * string * string | Movie of string * int * string
                  | CD of string * string * string | Computer
type checkout_entry = Item of catalog_item | New of checkout_entry | Extend of checkout_entry
                    | Pair of checkout_entry * checkout_entry
type cart = CartEntry of checkout_entry * int option * cart | Empty 

(* Examples *)
(* These are some examples of checkout_item. You should test locally with these before submitting *)
let i0 = Book ("Types and Programming Languages", "Benjamin Pierce", "The MIT Press")
let i1 = Movie ("The Imitation Game", 2014, "Morten Tyldum")
let i2 = Computer

(* These are some examples of checkout_entry. You should test locally with these before submitting *)
let e0 = Item i0
let e1 = Item i1
let e2 = Item i2

let e3 = Item (CD ("Songs to Test By", "Aperture Science Psychoacoustic Laboratories", "73:39"))
let e4 = New (Item (Book ("UNIX: A History and a Memoir", "Brian W. Kernighan", "Independently published")))

let e5 = Pair (
    Item (Movie ("WarGames", 1983, "John Badham")),
    Item (Movie ("Sneakers", 1992, "Phil Alden Robinson"))
)

let e6 = Pair (
    Pair (
        Item (Book ("The Unix Programming Environment", "Brian W. Kernighan and Rob Pike", "Prentice-Hall")), 
        New (Item (Book ("The C Programming Language", "Brian Kernighan and Dennis Ritchie", "Pearson")))
    ),
    Extend (Item (Book ("The AWK Programming Language", "Alfred V. Aho, Brian W. Kernighan, and Peter J. Weinberger",
                        "Pearson")))
)

(* This is an exmaple of a cart. You should test locally with it before submitting *)
let checked_out = CartEntry (e1, Some 2,
                     CartEntry (e2, None,
                       CartEntry (e4, Some 1,
                         CartEntry (e5, Some 2, Empty))))

(* The following functions you must implement *)

(* Display item as string *)
let string_of_item (i : catalog_item) : string = 
    match i with
    | Book (title, author, publisher) -> title ^ " by " ^ author ^ " (" ^ publisher ^ ")"
    | Movie (title, year, director) -> title ^ " (" ^ (string_of_int year) ^ ") by " ^ director
    | CD (album_name, artist, length) -> album_name ^ " by " ^ artist
    | Computer -> "Public Computer"


(* Display entry as string *)
let rec string_of_entry (e : checkout_entry) : string = 
    match e with
    | Item (i) -> string_of_item i
    | New (entry) -> "(NEW) " ^ string_of_entry entry
    | Extend (entry) -> "(EXT) " ^ string_of_entry entry
    | Pair (entry1, entry2) -> string_of_entry entry1 ^ " and " ^ string_of_entry entry2


(* Return the daily fine for an overdue item *)
let rec daily_fine (entry: checkout_entry) : float = 
    match entry with
    | Item (i) ->
        (match i with
        | Book _ -> 0.25
        | Movie _ -> 0.50
        | CD _ -> 0.50
        | Computer _ -> 0.00)
    | New (e) -> 2.0 *. daily_fine e
    | Extend (e) -> 3.0 *. daily_fine e
    | Pair (e1, e2) -> (daily_fine e1) +. (daily_fine e2)


(* Given a list of items and days overdue, compute the total fine *)
let rec total_fine (l : cart) : float = 
    match l with
    | Empty -> 0.00
    | CartEntry (ckout_entry, opt, cart) ->
        (match opt with
        | None -> total_fine cart
        | Some n-> (daily_fine ckout_entry) *. (float_of_int n) +. (total_fine cart))
;;
