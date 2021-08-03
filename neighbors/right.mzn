include "globals.mzn";
include "main.mzn";

Person={H,I,J,K,L,M,N};

% Clues
% gifts
constraint (gifts[L]=G(Palette) /\ gifts[N]=G(Compas) /\ gifts[M]=G(Binoculars) /\ gifts[book[K]] = G(Phonebook) );

% 1. An orange house is exactly 1 (two) lots southeast of the green house.
constraint(color_loc[Orange,Row]=color_loc[Green,Row]+1 
   /\ color_loc[Orange,Col]=color_loc[Green,Col]+1);
% 2. And in case you forgot, the neighborhood is a square with 4 (eight) lots on each side.			
% 3. Green house is in the second column
constraint(color_loc[Green,Col]=2);

% 4. By process of elimination, there are 3 (six) houses in the southernmost row.
constraint (count_eq(houses[4,1..4],Nobody)=1);

% 5. Currently, 2 (four) of the people who gave you gifts live in the same row or column as Ivan.
constraint (
  sum([1|p in Person where
    gifts[p]!=NoGift /\ (house_loc[p,Row]=house_loc[I,Row] \/ house_loc[p,Col]=house_loc[I,Col])])=2
);

% 7. Fill the southwestern quadrant with 2 (four) houses.								
constraint (count_eq(houses[3..4,1..2],Nobody)=2);
																
% 8. For the record, exactly 1 (two) houses are not on the edge of the neighborhood.				
constraint (count_eq(houses[2..3,2..3],Nobody)=3);

% 9. Four houses, including mine (Larry), are to the east of Harold's house.
constraint house_loc[L,Col]>house_loc[H,Col] /\
           sum([1| r in 1..4, c in house_loc[H,Col]+1..4 where houses[r,c]!=Nobody])=4;

% 10. "Harold" and "Joanne" live orthogonally adjacent to each other.
constraint ortho_adj(book[H],book[J]);

% 11. "Harold" lives in the orange house.
constraint lives_in(book[H],Orange);

% 12. Hi, I'm Larry, and I live orthogonally adjacent to Nancy.
constraint ortho_adj(L,N);

% 13. I ("Keith") didn't give you the artist's palette.
constraint gifts[book[K]]!=G(Palette);

% 14. I (Nancy) live diagonally adjacent to the green house.
constraint exists([ house_loc[N,Row]=color_loc[Green,Row]+dr /\
                   house_loc[N,Col]=color_loc[Green,Col]+dc | dr in [-1,1], dc in [-1,1]]);

% 15. I live in a house that is neither yellow nor gray.
constraint ( not(lives_in(N,Yellow)) /\ not(lives_in(N,Gray)));

% 16.If you look at the corner lots, exactly 1 (two) do not have a house.
constraint (count_eq([ houses[r,c] | r in [1,4], c in [1,4] ], Nobody)=1);

% 17. "Ivan" lives somewhere to the south of me ("Keith").
constraint p_p(book[I],South,book[K]);

% 18. "Joanne" lives in the gray house.
constraint lives_in(book[J],Gray);

% 19. Keith lives somewhere to the "east" of Harold.
constraint p_p(K,compass[East],H);

% 20. My friend "Ivan" gave you the binoculars.
constraint (gifts[book[I]] = G(Binoculars));

% 21. My name is "Keith", and I live in the opposite corner from the gray house.
constraint (
  abs(house_loc[book[K],Row]-color_loc[Gray,Row])=3
  /\ abs(house_loc[book[K],Col]-color_loc[Gray,Col])=3
);

% 22. Remember, 2 (four) gifts come from the people living to the west of Harold.
constraint (sum([1| r in 1..4, c in 1..house_loc[H,Col]-1 where 
                 (houses[r,c]!=Nobody /\ gifts[P^-1(houses[r,c])] != NoGift)])=2);


% 23. Right, Joanne's blue house is adjacent (counting diagonally) to exactly 1 (two) houses.
constraint(lives_in(J,Blue)) /\
   sum([ 1| p in Person where
       (p!=J /\  
       abs(house_loc[J,Row]-house_loc[p,Row])<=1 /\
       abs(house_loc[J,Col]-house_loc[p,Col])<=1)
       ])=1;

% 24. The blue house is somewhere to the "north" of the red house.
constraint c_c(Blue,compass[North],Red);

% 25. The gray house and Joanne's house are on the "western" edge of the neighborhood.
constraint (
  if compass[West]=West then house_loc[J,Col]=1 /\ color_loc[Gray,Col]=1
  elseif compass[West]=East then house_loc[J,Col]=4 /\ color_loc[Gray,Col]=4
  elseif compass[West]=South then house_loc[J,Row]=4 /\ color_loc[Gray,Row]=4
  elseif compass[West]=North then house_loc[J,Row]=1 /\ color_loc[Gray,Row]=1
  else false
  endif
);

% 26. The "orange" house is due north of the "gray" house.
constraint c_c(palette[Orange],North,palette[Gray]) /\
  color_loc[palette[Orange],Col] = color_loc[palette[Gray],Col];						
  
% 27. The person who gave you the compass lives in a "blue" house.
constraint forall(p in Person) (lives_in(p,palette[Blue]) -> gifts[p]=G(Compas));

% 28. The purple house is to the "west" of Harold's house.
constraint c_p(Purple,compass[West],H);

% 29. The "red" house is immediately to the northeast of Melanie's house.
constraint (
  color_loc[palette[Red],Row] = house_loc[M,Row]-1
  /\ color_loc[palette[Red],Col] = house_loc[M,Col]+1
);

% 30. The same number of houses are to the north of "Nancy"'s house as are to the south.
constraint (
  sum([1|p in Person where p_p(book[N],North,p)]) = 
  sum([1|p in Person where p_p(book[N],South,p)])
);

% 31. There are exactly two houses due "west" of the yellow house.
constraint (
  sum([1|p in Person where p_c(p,compass[West],Yellow) /\ (
  if compass[West]=West \/ compass[West]=East then house_loc[p,Row]=color_loc[Yellow,Row]
  elseif compass[West]=North \/ compass[West]=South then house_loc[p,Col]=color_loc[Yellow,Col]
  else false
  endif)])=2  
);

output ["\nPALETTE\n"];
output [ "\(c)->\(palette[c]) " | c in [Orange,Gray,Blue,Red]]; 

output ["\nPHONEBOOK\n"];
output [ "\(c)->\(book[c]) " | c in [I,J,K,H,N]]; 
