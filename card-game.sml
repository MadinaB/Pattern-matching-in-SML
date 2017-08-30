(*Madina Bektayeva*)


(* Assume that Num is always used with values 2, 3, ..., 9 *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


fun tail(l:'a list) =
    case l of
    []=>[]
    |x::a=>a; 

 
fun head(l: 'a list) =
    case l of
    x::a=>x; 

fun card_color card =
(*Function card_color, which takes a card and returns its color*)
	case card of
	  (Clubs,_)=> Black|
	  (Spades,_)=> Black|
	   _  =>Red;
	  
fun card_value card =
(*Function card_value, which takes a card and returns its value*)
	case card of
	  (_, Num (i))=>i
	 |(_, Ace)=>11
	 | _=> 10;	


fun remove_card (card_list : card list, card, e) =
(*Raises e if list is null and if till the end of list object was not found*)
(*If fun finds the object, fun stops to recurse and appends to ans tl list*)
	case (card_list, card,e) of
	 ([],_,_)=> raise e
	|(x::[],_,_)=> if x = card then [] else raise e
	|(_,_,_)=> if (head card_list) =  card then
		tail card_list
		else (head card_list):: remove_card((tail card_list),card,e); 

fun all_same_color (card_list : card list) =
	case card_list of
	 []=>true
	|x::[]=> true
	| _ =>if card_color(head card_list)=Red 
		andalso card_color(head(tail card_list)) =Red
		then true andalso all_same_color(tail card_list)
	      else 
		if card_color(head card_list)=Black
                	andalso card_color(head(tail card_list)) =Black
                	then true andalso all_same_color(tail card_list) 
		else false;

fun sum_cards(card_list: card list) =
	case card_list of
	 []=>0
	|_ => card_value(head card_list)+sum_cards(tail card_list);

fun score( card_list : card list,goal) =
	case (card_list,goal) of
	(_,_)=> let 
			val score=sum_cards (card_list)
		in	
		    let val preliminary_score = 
			if  score - goal > 0 then 3*(score-goal) 
			else goal-score
		    in
                       if all_same_color(card_list) then preliminary_score div 2
		       else preliminary_score
	            end
		end;

fun officiate(card_list:card list, move_list: move list, goal) =
let fun final_held_cards(held_cards:card list, card_list:card list, move_list: move list, goal) = 
	case (held_cards, card_list, move_list, goal) of
	(_,_,[],_)=>held_cards
	|(_,_,Discard(card)::_,_)=>final_held_cards(remove_card(held_cards,card, IllegalMove),card_list, tail move_list,goal)
	|(_,[],Draw::_,_)=>held_cards
	|(_,_,Draw::_,_)=>
	    if tail card_list =[] then 
		(head card_list)::held_cards
	    else
		if sum_cards((head card_list)::held_cards)>goal then head card_list::held_cards
		else final_held_cards(((head card_list)::held_cards),(tail card_list), (tail move_list), goal) 
in 
	let val held_cards = final_held_cards([],card_list, move_list, goal)
		in score(held_cards,goal)
	end
end;

fun replace_aces(card_list:card list) =
	case card_list of
	(card::[]) => if #2 card = Ace then [(#1 card, Num(1))] else [card]
	|(card::_)=> if #2 card = Ace then (#1 card, Num(1))::replace_aces(tail card_list) else card::replace_aces(tail card_list);

fun score_challenge( card_list : card list,goal) =
	Int.min(score(replace_aces(card_list),goal),score(card_list,goal));

fun officiate_challenge( card_list : card list,move_list: move list,goal) =
        Int.min(officiate(replace_aces(card_list),move_list,goal),officiate(card_list,move_list,goal));

fun careful_player( card_list : card list,goal) = 
	let fun careful_player_rec(held_cards:card list, card_list:card list, move_list: move list, goal) =
	  if score(held_cards,goal) =0 then move_list
	  else 
	      case (held_cards,card_list, move_list,goal) of
	      (_,[],_,_) =>move_list
	      |(_,_ ,_,_) => 
	      if sum_cards(held_cards)+10<goal then careful_player_rec(head card_list::held_cards, tail card_list, Draw::move_list, goal)
	      else careful_player_rec(remove_card(held_cards, head held_cards,IllegalMove), card_list, Discard(head held_cards)::move_list, goal)	
	in	
		case (card_list, goal) of
		([],_)=> []
		|(_,_)=>rev(careful_player_rec([(head card_list)],(tail card_list), [Draw], goal))
	end;
