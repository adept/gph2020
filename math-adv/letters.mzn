enum Letter = {A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z};
enum Kind = {Alph,Morse,Scrabble};

array[Letter,Kind] of int: value =
 array2d(Letter,Kind,[
 1,2,1, %A
 2,4,3, %B
 3,4,3, %C
 4,3,2, %D
 5,1,1, %E
 6,4,4, %F
 7,3,2, %G
 8,4,4, %H
 9,2,1, %I
 10,4,8, %J
 11,3,5, %K
 12,4,1, %L
 13,2,3, %M
 14,2,1, %N
 15,3,1, %O
 16,4,3, %P
 17,4,10, %Q
 18,3,1, %R
 19,3,1, %S
 20,1,1, %T
 21,3,1, %U
 22,4,4, %V
 23,3,4, %W
 24,4,8, %X
 25,4,4, %Y
 26,4,10 %Z
 ]);
 
array[Letter] of var 0..1: qtys;

constraint (sum([ qtys[l]*value[l,Alph] |l in Letter]) ) = (sum([ qtys[l]*value[l,Scrabble] |l in Letter]) )+1;

constraint sum(qtys)>0;

solve satisfy; 