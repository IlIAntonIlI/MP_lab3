(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (word : string, sList : string list) =
  case sList of [] => NONE 
  | el::arr => case same_string(word, el) of true => SOME(arr) 
      | false => case all_except_option(word, arr) of NONE => NONE 
         | SOME resultArray => SOME(el::resultArray);

all_except_option("Fred",["Freddie","Fred","Frederick"]);

fun get_substitutions1(lists: string list list, word: string) =
  case lists of [] => [] 
  | arr1::arr' => case all_except_option(word,arr1) of NONE => get_substitutions1(arr', word) 
     | SOME arrWithoutWord => arrWithoutWord @ get_substitutions1(arr',word);

get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff");

fun get_substitutions2(lists: string list list, word: string) =
   let fun f (acc: string list list, word: string) = 
      case acc of [] => [] 
      | arr1::arr2 => case all_except_option(word,arr1) of NONE => f(arr2, word) 
         | SOME arrWithoutWord => arrWithoutWord @ f(arr2,word);
   in
      f(lists,word)
   end;

get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff");

   
fun similar_names(lists: string list list, fullName:{first:string,middle:string,last:string}) =
    let 
        fun findSimilarNames ({first=f,middle=m,last=l}) = 
        let 
         val array = f::get_substitutions1(lists,f)
         fun myFunc (namesList: string list) =
               case namesList of [] => []
               | el::arr => {first=el, middle=m, last=l}::myFunc(arr)
        in
            myFunc(array)
        end
    in
        findSimilarNames(fullName)
    end;

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],{first="Fred", middle="W", last="Smith"});

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (ca : card) = 
      case ca of
         (suit,rank) => case suit of 
         Clubs => Black
         | Diamonds => Red
         | Hearts => Red
         | Spades => Black;

card_color((Diamonds, Queen));

fun card_value (ca : card) = 
   case ca of
      (suit,rank) => case rank of 
      Jack => 10
      | Queen => 10
      | King => 10
      | Ace => 11
      | Num n => n;

card_value((Clubs, Ace));
card_value((Clubs, Num 5));

fun remove_card (cardList: card list, ca : card, excep) = 
   case cardList of 
      [] => raise excep
      | ca1::cardList1 => if ca1=ca then cardList1 else ca1::remove_card(cardList1,ca,excep);

remove_card([(Hearts, Ace), (Diamonds, Num 3), (Hearts, Ace)], (Hearts,Ace), IllegalMove);
remove_card([(Diamonds, Num 3), (Hearts, Ace), (Hearts, Queen)], (Hearts,Ace), IllegalMove);

fun all_same_color (cardList : card list) = 
   case cardList of [] => true
   | el1::cardList1 => case cardList1 of [] => true 
      | el2::cardList2 => if card_color(el1)=card_color(el2) then all_same_color(cardList1) else false;

all_same_color([(Spades, Ace)]);
all_same_color([(Clubs, Ace), (Diamonds, Num 3)]);
all_same_color([(Hearts, Ace), (Diamonds, Num 3)]);
all_same_color([(Spades, Ace), (Clubs, Num 3), (Spades, Ace)]);
all_same_color([(Clubs, Ace), (Hearts, Ace), (Diamonds, Num 3)]);
all_same_color([(Spades, Ace), (Diamonds, Num 3), (Clubs, Ace)]);

fun sum_cards (cardList : card list) = 
   let 
      fun findSum(cardsArray : card list) = 
         case cardsArray of [] => 0
            | el::cardsArray1 => card_value(el) + findSum(cardsArray1)
   in
      findSum(cardList)
   end;

sum_cards([(Hearts, Ace), (Diamonds, Num 3), (Hearts, Ace)]);


fun score (cardList : card list, goal : int) = 
   let 
      fun findFirst(cardList1, goal1) = 
         if ( sum_cards( cardList1 ) > goal1 )
            then 3 * ( sum_cards( cardList1 ) - goal1)
            else goal1 - sum_cards( cardList1 );
   in
      if ( all_same_color ( cardList ) )
         then findFirst( cardList, goal ) div 2 
         else findFirst( cardList, goal )
   end;

score([(Hearts, Ace), (Diamonds, Num 3), (Hearts, Ace)], 10);
score([(Hearts, Ace), (Diamonds, Num 3), (Spades, Ace)], 10);
