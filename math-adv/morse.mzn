enum Letter = {A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z};
enum Kind = {Alph,Morse};

array[Letter,Kind] of int: value =
 array2d(Letter,Kind,[
 1,2, 
 2,4, 
 3,4, 
 4,3,
 5,1,
 6,4,
 7,3,
 8,4,
 9,2,
 10,4,
 11,3,
 12,4,
 13,2,
 14,2,
 15,3,
 16,4,
 17,4,
 18,3,
 19,3,
 20,1,
 21,3,
 22,4,
 23,3,
 24,4,
 25,4,
 26,4]);
 
array[Letter] of var 0..1: qtys;

constraint (sum([ qtys[l]*value[l,Alph] |l in Letter]) ) = (sum([ qtys[l]*value[l,Morse] |l in Letter]) );

constraint sum(qtys)>0;

constraint 300 mod (sum([ qtys[l]*value[l,Morse] |l in Letter])) = 0;

solve satisfy; 