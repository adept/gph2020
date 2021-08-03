include "globals.mzn";

int: n=6;                                 % The board is n x n in size.
int: m=n*n;                                 % The length of the tour.

array [1..m] of var 1..n: r;  % The sequence of moves in the tour
array [1..m] of var 1..n: c;  % (row and column of each move).
array [1..n,1..n] of var 1..m: board; % moves on the board

% board corresponds to the sequence of moves
constraint forall(i in 1..m) (board[r[i],c[i]]=i);

% Each move must be to a different square.
constraint alldifferent(board);
constraint alldifferent([n * r[i] + c[i] | i in 1..m]);

    % Set up the possible routes.
predicate move(int: i, int: j) =
      ( abs(r[j] - r[i])=2  /\  abs(c[j] - c[i])=1 )
  \/  ( abs(r[j] - r[i])=1  /\  abs(c[j] - c[i])=2 )
;

constraint
    forall (i in 1..(m - 1)) (move(i,i+1));
constraint move(m,1);

% puzzle constraints
/*
From	To	Word	Jumps	Stop #
Poet	Basketb	SAINT	5	5->6
Basketb	Scratched	CROSS	5	6->7
Scratched	Oaf	TRIPLE	6	7->1
Oaf	Currency	HEX	3	1->2
Currency	Drug	HI	2	2->3
Drug	Brand	???	4	3->4
Brand	Poet	GOOD	4	4->5
*/

constraint r[1]=5 /\ c[1]=5; % +3 from end
constraint r[4]=2 /\ c[4]=3; % +2
constraint r[9]=1 /\ c[9]=3; % +4
constraint r[14]=5 /\ c[14]=2; % +4
constraint r[20]=4 /\ c[20]=5; % +5
constraint r[26]=1 /\ c[26]=4; % +5
constraint r[33]=4 /\ c[33]=2; % +6


solve :: int_search(r, input_order, indomain_random) satisfy;
%solve satisfy;

output [
     "r = ", show(r), "\n",
     "c = ", show(c), "\n"
];

output [ "\(board[x,y])," ++ (if y=n then "\n" else "" endif) | x in 1..n, y in 1..n]; 


