include "globals.mzn";

enum Coord={Row,Col};

enum Person;
enum PersonOrNobody=P(Person) ++ {Nobody};

enum Color={Red,Orange,Yellow,Green,Blue,Purple,Gray};
enum ColorOrNothing=C(Color)++{Nothing};

enum Gift={Binoculars,Palette,Phonebook,Compas};
enum GiftOrNot=G(Gift)++{NoGift};

array [1..4,1..4] of var PersonOrNobody: houses;
constraint (alldifferent_except(houses,{Nobody}) /\ nvalue(7+1,houses));
array [Person,Coord] of var 1..4: house_loc;
constraint forall(p in Person)(houses[house_loc[p,Row],house_loc[p,Col]]=P(p));


array [1..4,1..4] of var ColorOrNothing: colors;
constraint (alldifferent_except(colors,{Nothing}) /\ nvalue(7+1,colors));
array [Color,Coord] of var 1..4: color_loc;
constraint forall(c in Color)(colors[color_loc[c,Row],color_loc[c,Col]]=C(c));

% People and colors go together
constraint forall(r in 1..4) (forall(c in 1..4) (
  houses[r,c]=Nobody <-> colors[r,c] =Nothing));
 
array [Person] of var GiftOrNot: gifts;
constraint (alldifferent_except(gifts,{NoGift}) /\ nvalue(4+1, gifts));

enum Compass = {North,South,East,West};
array[Compass] of var Compass: compass;
constraint alldifferent(compass);

array[Color] of var Color: palette;
constraint alldifferent(palette);

array[Person] of var Person: book;
constraint alldifferent(book);

predicate lives_in(var Person: p, var Color: c) = 
  house_loc[p,Row]=color_loc[c,Row] /\ house_loc[p,Col]=color_loc[c,Col];

predicate ortho_adj(var Person: p1, var Person: p2)=
           (house_loc[p1,Row]=house_loc[p2,Row] /\ abs(house_loc[p1,Col]-house_loc[p2,Col])=1)
        \/ (house_loc[p1,Col]=house_loc[p2,Col] /\ abs(house_loc[p1,Row]-house_loc[p2,Row])=1);

predicate diag_adj(var Person: p1, var Person: p2)=
           (abs(house_loc[p1,Col]-house_loc[p2,Col])=1)
        /\ (abs(house_loc[p1,Row]-house_loc[p2,Row])=1);

predicate any_adj(var Person: p1, var Person: p2)=
           p1 != p2
        /\ (abs(house_loc[p1,Col]-house_loc[p2,Col])<=1)
        /\ (abs(house_loc[p1,Row]-house_loc[p2,Row])<=1);

function var bool: p_p(var Person:p1 , var Compass:dir, var Person: p2) =
  if dir=North then house_loc[p1,Row] < house_loc[p2,Row]
  elseif dir=South then house_loc[p1,Row] > house_loc[p2,Row]
  elseif dir=East then house_loc[p1,Col] > house_loc[p2,Col]
  else house_loc[p1,Col] < house_loc[p2,Col]
  endif;

function var bool: c_c(var Color:c1 , var Compass:dir, var Color: c2) =
  if dir=North then color_loc[c1,Row] < color_loc[c2,Row]
  elseif dir=South then color_loc[c1,Row] > color_loc[c2,Row]
  elseif dir=East then color_loc[c1,Col] > color_loc[c2,Col]
  else color_loc[c1,Col] < color_loc[c2,Col]
  endif;

function var bool: c_p(Color:c1 , var Compass:dir, Person: p2) =
  if dir=North then color_loc[c1,Row] < house_loc[p2,Row]
  elseif dir=South then color_loc[c1,Row] > house_loc[p2,Row]
  elseif dir=East then color_loc[c1,Col] > house_loc[p2,Col]
  else color_loc[c1,Col] < house_loc[p2,Col]
  endif;

function var bool: p_c(var Person:p1 , var Compass:dir, var Color: c2) =
  if dir=North then house_loc[p1,Row] < color_loc[c2,Row]
  elseif dir=South then house_loc[p1,Row] > color_loc[c2,Row]
  elseif dir=East then house_loc[p1,Col] > color_loc[c2,Col]
  else house_loc[p1,Col] < color_loc[c2,Col]
  endif;

output ["\nHOUSES\n"];
output [ "\(houses[r,c]) " ++ (if c=4 then "\n" else "" endif) | r in 1..4, c in 1..4]; 

output ["COLORS\n"];
output [ "\(colors[r,c]) " ++ (if c=4 then "\n" else "" endif) | r in 1..4, c in 1..4]; 

output ["GIFTS\n"];
output [ "\(p)->\(gifts[p]) " | p in Person ]; 

output ["\nCOMPASS\n"];
output [ "\(c)->\(compass[c]) " | c in Compass]; 

solve satisfy;