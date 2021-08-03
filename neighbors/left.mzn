include "globals.mzn";
include "main.mzn";

Person={A,B,C,D,E,F,G};

% Clues

% gifts
constraint (gifts[B]=G(Palette) /\ gifts[F]=G(Binoculars) 
%/\ (gifts[A]=G(Compas) \/ gifts[D]=G(Compas)) 
/\ gifts[A]=G(Compas) 
);

% 1. A guy named Chris lives in the row two to the north of Gina.
constraint (
  house_loc[C,Row]=house_loc[G,Row]-2
);

% 2. "Amber" did not give you a gift.
constraint (gifts[book[A]] = NoGift);

% 3. Believe me, the red house is in the row three to the north of the phone book gifter's house (Fred).
constraint (
  forall (p in Person)
  (gifts[p] = G(Phonebook) -> c_p(Red,North,p)
   /\ abs(color_loc[Red,Row]-house_loc[p,Row])>=3
   ) 
);

% 4. Bill gave you the binoculars.
constraint (gifts[book[B]] = G(Binoculars));

% 5. Bill lives two blocks due "north" of the yellow house.
constraint (p_c(B,compass[North],Yellow) /\
(
  if compass[North]=North \/ compass[North]=South then house_loc[B,Col]=color_loc[Yellow,Col]
  else house_loc[B,Row]=color_loc[Yellow,Row]
  endif
));

% 6. Diana lives in the "red" house.
constraint lives_in(D,palette[Red]);

% 7. Exactly two people who gave you gifts live south of "Gina".
constraint (
  sum([1|p in Person where
    gifts[p]!=NoGift /\ p_p(p,South,book[G])])=2
);

% 8. Fred lives east of the "purple" house.
constraint p_c(F,East,palette[Purple]);

% 9. "Fred"'s house is west of the orange house.
constraint p_c(book[F],West,Orange);

% 10. Gina lives somewhere "north" and "west" of the yellow house.
constraint (p_c(G,compass[North],Yellow) /\ p_c(G,compass[West],Yellow));

% 11. I gave you the phone book and I live in a row south of the person who gave you the binoculars (Fred).
constraint forall(p1 in Person) 
    (gifts[p1]=G(Phonebook) -> p_p(p1,South,F));

% 12. I know that the neighborhood is a square grid with "eight" lots on a side.
% 13. My name is Bill and I did not give you the phone book.
% 14. My name is not Emma.
constraint (gifts[E]!=G(Compas));

% 15. Remember that Bill's house is adjacent (counting diagonally) to exactly one other houses.
constraint (sum([1|p in Person where any_adj(p,B)])=1);

% 16. Sitting in the 2nd column from the west end of the neighborhood is the purple house.
constraint (color_loc[Purple,Col]=2);

% 17. The "blue" house is in a row at least two blocks south of the "green" house.
constraint (
  c_c(palette[Blue],South,palette[Green])
  /\ abs(color_loc[palette[Blue],Row]-color_loc[palette[Green],Row])>=2
);

% 18. The blue house is somewhere "west" of the red house.
constraint (
  c_c(Blue,compass[West],Red)
);

% 19. The orange house is not in the second row from the south end of the neighborhood.
constraint (color_loc[Orange,Row]!=3);

% 20. The "orange" house is on the eastern end of the neighborhood.
constraint (color_loc[palette[Orange],Col]>=3);

% 21. The person who gave you binoculars (Fred) lives in the "southernmost" row of the neighborhood.
constraint (
  if compass[South]=West then house_loc[F,Col]=1
  elseif compass[South]=East then house_loc[F,Col]=4 
  elseif compass[South]=South then house_loc[F,Row]=4
  elseif compass[South]=North then house_loc[F,Row]=1
  else false
  endif
);

% 22. The person who gave you the compass lives west of "Emma".
constraint forall(p in Person)
  (gifts[p]=G(Compas) -> p_p(p,West,book[E]));

% 23. The phone book came from someone who lives in a column at least two blocks "east" of the blue house.
constraint forall(p in Person)
  (gifts[p]=G(Phonebook) -> (p_c(p,compass[East],Blue) /\ 
  (
  if compass[East]=West \/ compass[East]=East then abs(house_loc[p,Col]-color_loc[Blue,Col])>=2
  else abs(house_loc[p,Row]-color_loc[Blue,Row])>=2
  endif
  ))
  );


% 24. The "yellow" house is orthogonally adjacent to Emma's house.
constraint forall(p in Person)
   (lives_in(p,palette[Yellow]) -> ortho_adj(p,E));

% 25. There are no houses "south" of the red house.
constraint sum([1|p in Person where p_c(p,compass[South],Red)])=0;

% 26. There is exactly one house in the row containing the "purple" house.
constraint sum([1|
  p in Person where house_loc[p,Row]=color_loc[palette[Purple],Row]])=1;

% 27. Unfortunately I forget which ones, but exactly three houses are west of Chris's house.
constraint sum([1|p in Person where p_p(p,West,C)])=3;

% 28. You will find exactly 1 houses in the southwestern quadrant of the neighborhood.
constraint (count_eq(houses[3..4,1..2],Nobody)=3);

% fix colors
%constraint (
% palette[Blue]=Orange
% /\ palette[Yellow]=Purple
% /\ palette[Green]=Red
%);

constraint (
 palette[Red]=Green
 /\ palette[Orange]=Blue
 /\ palette[Yellow]=Purple
 /\ palette[Green]=Red
 /\ palette[Blue]=Orange
 /\ palette[Purple]=Yellow
 /\ palette[Gray]=Gray
);


% fix compass
constraint (
  compass[North]=South /\ compass[South]=North /\
  compass[East]=West /\ compass[West]=East
);

% fix diagonals
constraint (
  book[A] = G
/\ book[B] = F
/\ book[C] = E
/\ book[D] = D
/\ book[E] = C
/\ book[F] = B
/\ book[G] = A
);


output ["\nPALETTE\n"];
output [ "\(c)->\(palette[c]) " | c in [Blue,Yellow,Green]]; 

output ["\nPHONEBOOK\n"];
output [ "\(c)->\(book[c]) " | c in [G,B,A,E]]; 

