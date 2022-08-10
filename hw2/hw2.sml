(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(str: string, strs: string list) : string list option =
    case strs of
        [] => NONE
        | head::tail => if same_string(head, str)
                        then SOME(tail)
                        else 
                            case all_except_option(str, tail) of
                                NONE => NONE
                                | SOME(e) => SOME(head::e)

fun get_substitutions1(strs: string list list, s: string) : string list =
    case strs of
        [] => []
        | head::tail => let val ans = all_except_option(s, head)
                            val tail_ans = get_substitutions1(tail, s)
                        in
                            case ans of
                                NONE => tail_ans
                                | SOME(e) => e @ tail_ans
                        end

fun get_substitutions2(strs: string list list, s: string) : string list =
    let fun aux(strs, s, acc) =
        case strs of
            [] => acc
            | head::tail => let val ans = all_except_option(s, head)
                            in
                                case ans of
                                    NONE => aux(tail, s, acc)
                                    | SOME(e) => aux(tail, s, acc @ e)
                            end
    in
        aux(strs, s, [])
    end

type full_name = {first: string, middle: string, last: string}
fun similar_names(names: string list list, full_name: full_name) : full_name list =
    let
        val {first=first_name, middle=middle_name, last=last_name} = full_name
        val substitutes = get_substitutions2(names, first_name)
        fun return_substituted(substitutes: string list, middle_name, last_name) : full_name list =
            case substitutes of
            [] => []
            | head::tail => {first=head, middle=middle_name, last=last_name}::return_substituted(tail, middle_name, last_name)
    in
        full_name::return_substituted(substitutes, middle_name, last_name)
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color((suit, rank): card) : color =
    case suit of
        Clubs => Black
        | Spades => Black
        | _ => Red

fun card_value((suit, rank): card) : int =
    case rank of
        Num(value) => value
        | Ace => 11
        | _ => 10

fun remove_card(cs: card list, c: card, e: exn) : card list =
    case cs of
        [] => raise e
        | head::tail => if head = c then tail else head::remove_card(tail, c, e)

fun all_same_color(cs: card list) : bool =
    case cs of
        [] => true
        | _::[] => true
        | head::(neck::rest) => (card_color(head) = card_color(neck) andalso all_same_color(neck::rest))

fun sum_cards(cs: card list) : int =
    let
        fun aux(cs, acc) =
            case cs of
                [] => acc
                | head::tail => aux(tail, acc + card_value(head))
    in
        aux(cs, 0)
    end

fun score(held_cards: card list, goal: int) : int =
    let
        val sum = sum_cards(held_cards)
        val preliminary = if sum > goal
                        then (sum - goal) * 3
                        else goal - sum
    in
        if all_same_color(held_cards) then preliminary div 2 else preliminary
    end

fun officiate(cards_list: card list, moves: move list, goal: int) : int =
    let
        fun play(held_cards, cards_list, moves) =
            case moves of
                (* no moves left: end game *)
                [] => score(held_cards, goal)

                (* discarding *)
                | Discard(c)::tail => play(remove_card(held_cards, c, IllegalMove), cards_list, tail)

                (* drawing *)
                | Draw::tail => 
                    case cards_list of
                        (* empty: end game*)
                        [] => score(held_cards, goal)

                        (* else: *)
                        | c::cards_left => 
                            (* over goal: end game *)
                            if sum_cards(c::held_cards) > goal then score(c::held_cards, goal)
                            (* else: continue playing *)
                            else play(c::held_cards, cards_left, tail)

    in
        (* start with the held-cards being the empty list *)
        play([], cards_list, moves)
    end
