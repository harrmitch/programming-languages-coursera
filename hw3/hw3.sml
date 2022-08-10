(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals(string_list) = List.filter (fn str => Char.isUpper(String.sub(str, 0))) string_list

fun longest_string1(string_list) = List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" string_list

fun longest_string2(string_list) = List.foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" string_list

fun longest_string_helper f string_list = List.foldl f "" string_list

val longest_string3 = longest_string_helper (fn (x, y) => if String.size(x) > String.size(y) then x else y)

val longest_string4 = longest_string_helper (fn (x, y) => if String.size(x) >= String.size(y) then x else y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f lst =
    let 
        fun loop(lst) =
            case lst of
                [] => raise NoAnswer
                | x::xs' => case f(x) of
                                SOME(v) => v
                                | NONE => loop(xs')
    in
        loop(lst)
    end

fun all_answers f lst =
    let
        fun loop(xs, acc) =
            case xs of
                [] => SOME(acc)
                | x::xs' => case f(x) of
                                NONE => NONE
                                | SOME(x) => loop(xs', x @ acc)
    in
        loop(lst, [])
    end


fun count_wildcards(p) = g (fn _ => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths(p) = g (fn _ => 1) (fn x => String.size(x)) p

fun count_some_var(str, p) = g (fn _ => 0) (fn x => if str = x then 1 else 0) p

fun check_pat(p) =
    let
        fun all_var(p) =
            case p of
	            Variable(x)         => x::[]
	            | TupleP ps         => List.foldl (fn (p,i) => (all_var(p)) @ i) [] ps
	            | ConstructorP(_,p) => all_var(p)
	            | _                 => []

        fun no_repeat(string_list) =
            case string_list of
                [] => true
                | n::xs' => (not (List.exists (fn x => n = x) xs')) andalso no_repeat(xs')
    in
        no_repeat(all_var(p))
    end

fun match(v, p) =
    case (v, p) of
        (_, Wildcard) => SOME([])
        | (v, Variable(s)) => SOME([(s, v)])
        | (Unit, UnitP) => SOME([])
        | (Const(x), ConstP(y)) => if x = y then SOME([]) else NONE
        | (Tuple(vs), TupleP(ps)) => if List.length(vs) = List.length(ps) then all_answers (fn x => match(x)) (ListPair.zip(vs, ps)) else NONE
        | (Constructor(s2,v), ConstructorP(s1,p)) => if s1 = s2 then match(v, p) else NONE
        | _ => NONE

fun first_match v ps = SOME(first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE