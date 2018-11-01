(* ::Package:: *)

(* ::Title:: *)
(*Chess Game with Mathematica (WLAS18)*)


(* ::Section:: *)
(*Basic Structure and Utils*)


(* Here is the board: x \[Rule] this direction and y in the other direction
This is the basic structure of a piece 
r\[Rule] {"\[WhiteKing]", {x, y}, False}
The pawn is associated to the matching unicode character,
  A boolean which indicates if the piece has already moved
*)
chessboard= {
	{{"\[WhiteRook]",{1,1},False},{"\[WhiteKnight]",{2,1}},{"\[WhiteBishop]",{3,1}},{"\[WhiteQueen]",{4,1}},{"\[WhiteKing]",{5,1},False},{"\[WhiteBishop]",{6,1}},{"\[WhiteKnight]",{7,1}},{"\[WhiteRook]",{8,1},False}},
	{{"\[WhitePawn]",{1,2},False},{"\[WhitePawn]",{2,2},False},{"\[WhitePawn]",{3,2},False},{"\[WhitePawn]",{4,2},False},{"\[WhitePawn]",{5,2},False},{"\[WhitePawn]",{6,2},False},{"\[WhitePawn]",{7,2},False},{"\[WhitePawn]",{8,2},False}},
	{{0,{1,3}}, {0,{2,3}},{0,{3,3}}, {0,{4,3}}, {0,{5,3}}, {0,{6,3}}, {0,{7,3}}, {0,{8,3}}},
	{{0,{1,4}}, {0,{2,4}}, {0,{3,3}},{0,{4,4}}, {0,{5,4}}, {0,{6,4}}, {0,{7,4}}, {0,{8,4}}},
	{{0,{1,5}}, {0,{2,5}}, {0,{3,3}},{0,{4,5}}, {0,{5,5}}, {0,{6,5}}, {0,{7,5}}, {0,{8,5}}},
	{{0,{1,6}}, {0,{2,6}},{0,{3,3}}, {0,{4,6}}, {0,{5,6}}, {0,{6,6}}, {0,{7,6}}, {0,{8,6}}},
	{{"\[BlackPawn]",{1,7},False},{"\[BlackPawn]",{2,7},False},{"\[BlackPawn]",{3,7},False},{"\[BlackPawn]",{4,7},False},{"\[BlackPawn]",{5,7},False},{"\[BlackPawn]",{6,7},False},{"\[BlackPawn]",{7,7},False},{"\[BlackPawn]",{8,7},False}},
	{{"\[BlackRook]",{1,8},False},{"\[BlackKnight]",{2,8},False},{"\[BlackBishop]",{3,8},False},{"\[BlackQueen]",{4,8},False},{"\[BlackKing]",{5,8},False},{"\[BlackBishop]",{6,8},False},{"\[BlackKnight]",{7,8},False},{"\[BlackRook]",{8,8},False}}
};

(* 0 is White, 1 is Black, -1 is undefined *)
getColor[x_String:"\[WhitePawn]"]:= If[("\[WhiteKing]" == x || "\[WhiteQueen]" == x || "\[WhiteKnight]" == x|| "\[WhiteBishop]" == x|| "\[WhiteRook]" == x|| "\[WhitePawn]" == x), 0, If[("\[BlackKing]" == x || "\[BlackQueen]" == x || "\[BlackKnight]" == x|| "\[BlackBishop]" == x|| "\[BlackRook]" == x|| "\[BlackPawn]" == x), 1, -1]]

(* this function is not finished at all !!! *)
isUnderAttack[color_] := (
	Table[Table[If[chessboard[[j]][[i]][[1]] == "\[WhiteKing]" || chessboard[[j]][[i]][[1]] == "\[BlackKing]" && getColor[chessboard[[j]][[i]][[1]]] == color, (king = chessboard[[j]][[i]])], {i, 1, Length[chessboard[[j]]]}], {j, 1, Length[chessboard]}];
	(* The following line is really crashy ! *)
	Table[Table[If[Length[Intersection[If[!getMovePossibilities[chessboard[[j]][[i]]], {}, getMovePossibilities[chessboard[[j]][[i]]]],  {king[[2]]}]] =!= 0, Return[True]], {i, 1, Length[chessboard[[j]]]}], {j, 1, Length[chessboard]}];
	Return[False]
)


(* ::Section:: *)
(*Locations where pieces can go*)


(*Returns the list of all the moving possibilities for a piece*)
(* Thanks to mathematica for returning False on each of these methods ... O_o *)
getMovePossibilities[{pawn:"\[WhitePawn]"|"\[BlackPawn]", {x_,y_}, moved_:True}, ignoreOthers_:False]:= Return[Select[movePawn[{pawn,{x,y}, moved},x,y],isPlacementAllowed[#,getColor[pawn],{pawn, {x, y}, moved}]&]]
getMovePossibilities[{bishop:"\[WhiteBishop]"|"\[BlackBishop]", {x_,y_}, moved_:True}, ignoreOthers_:False]:= Return[Select[moveBishop[{bishop,{x,y}},x,y],isPlacementAllowed[#,getColor[bishop],{bishop, {x, y}, moved}]&]]
getMovePossibilities[{knight:"\[WhiteKnight]"|"\[BlackKnight]", {x_,y_}, moved_:True}, ignoreOthers_:False]:= Return[Select[moveKnight[{knight,{x,y}},x,y],isPlacementAllowed[#,getColor[knight],{knight, {x, y}, moved}]&]]
getMovePossibilities[{queen:"\[WhiteQueen]"|"\[BlackQueen]", {x_,y_}, moved_:True}, ignoreOthers_:False]:= Return[Select[moveQueen[{queen,{x,y}},x,y],isPlacementAllowed[#,getColor[queen],{queen, {x, y}, moved}]&]]
getMovePossibilities[{king:"\[WhiteKing]"|"\[BlackKing]", {x_,y_}, moved_:True}, ignoreOthers_:False]:= Return[Select[moveKing[{king,{x,y}, moved},x,y],isPlacementAllowed[#,getColor[king],{king, {x, y}, moved}]&]]
getMovePossibilities[{rook:"\[WhiteRook]"|"\[BlackRook]", {x_,y_}, moved_:True}, ignoreOthers_:False]:= Return[Select[moveRook[{rook,{x,y}, moved},x,y],isPlacementAllowed[#,getColor[rook],{rook, {x, y}, moved}]&]]

isPlacementAllowed[location_, color_, piece_]:=(
	If[location ==  piece[[2]], Return[False]];
	If[!(tests = 0<location[[1]] <= Length[chessboard] && 0< location[[2]] <= Length[chessboard[[location[[1]]]]]), Return[False]];
	If[ToString[chessboard[[location[[2]]]][[location[[1]]]][[1]]] =!= ToString[0],tests = tests && getColor[chessboard[[location[[2]]]][[location[[1]]]][[1]]] =!= color];
	Return[tests]
)

(* Moves pieces in the matrix.
Please, provide coordiantes as {x, y} *)
finalizeMove[oldPosition_, newPosition_]:=(
	If[0<oldPosition [[1]] <= Length[chessboard] && 0<oldPosition [[2]] <= Length[chessboard[[1]]], (
		If[MemberQ[getMovePossibilities[chessboard[[oldPosition[[2]]]][[oldPosition[[1]]]]], newPosition], (
			chessboard[[newPosition[[2]]]][[newPosition[[1]]]] = {chessboard[[oldPosition[[2]]]][[oldPosition[[1]]]][[1]], newPosition, True};
			chessboard[[oldPosition[[2]]]][[oldPosition[[1]]]] = {0, chessboard[[oldPosition[[2]]]][[oldPosition[[1]]]][[2]]};
			Return[True]
		), Return[False]]
	),
	Return[False]]
)

movePawn[pawn_, x_, y_]:=(
	list = {}; (* The list containing all the possible moves *)
	If[getColor[pawn[[1]]]==0, (* Checks if the pawn is white *)
		(
			If[y + 1 <= Length[chessboard] && getColor[ToString[chessboard[[y+1]][[x]][[1]]]] == -1, AppendTo[list, {x, y+1}]] ; (* Adds the first move to the list *)
			If[x =!= Length[chessboard]&& getColor[chessboard[[y+1]][[x+1]][[1]]] == 1, AppendTo[list, {x+1, y+1}]];
			If[x =!= 1 && getColor[chessboard[[y+1]][[x-1]][[1]]] == 1, AppendTo[list, {x-1, y+1}]];
			If[pawn[[3]] ==False &&  getColor[chessboard[[y+2]][[x]][[1]]] =!= getColor[pawn[[1]]] && getColor[ToString[chessboard[[y+2]][[x]][[1]]]] == -1, (* Adds another possibility if the pawn moves for the first time *)
				AppendTo[list, {x, y+2}]
			]
		),

		(* Now, let's take a look at the black pawn ... *)
		(
			If[0 < y - 1 <= Length[chessboard] &&  getColor[ToString[chessboard[[y-1]][[x]][[1]]]] == -1, AppendTo[list, {x, y-1}]] ; (* Adds the first move to the list *)
			If[x =!= Length[chessboard]&& getColor[chessboard[[y-1]][[x+1]][[1]]] == 0, AppendTo[list, {x+1, y-1}]];
			If[x =!= 1 &&getColor[chessboard[[y-1]][[x-1]][[1]]] ==0, AppendTo[list, {x-1, y-1}]];
			If[pawn[[3]] ==False &&  getColor[chessboard[[y - 2]][[x]][[1]]] =!= getColor[pawn[[1]]] && getColor[ToString[chessboard[[y-2]][[x]][[1]]]] == -1, (* Adds another possibility if the pawn moves for the first time *)
				AppendTo[list, {x, y-2}]
			]
		)
	];
	Return[list]
)

(* The king can't move to locations where there'll be an "echec" 
crash @asdl *)
moveKing[king_, x_, y_, ignoreOthers_:False]:=(
	list={{x+1, y+1}, {x+1, y}, {x+1, y-1}, {x, y+1}, {x, y-1}, {x-1, y + 1}, {x-1, y}, {x-1, y-1}};
	nlist = {};
	Table[If[isPlacementAllowed[list[[i]], getColor[king[[1]]], king], AppendTo[nlist, list[[i]]]], {i, 1, Length[list]}];
	ennemies = {};
	If[!ignoreOthers, (
		Table[
			Table[
				If[getColor[chessboard[[j]][[i]][[1]]] =!= getColor[king[[1]]] && getColor[ToString[chessboard[[j]][[i]][[1]]]] =!= -1 && chessboard[[j]][[i]][[1]] =!= "\[BlackKing]" && chessboard[[j]][[i]][[1]] =!= "\[WhiteKing]" , AppendTo[ennemies, chessboard[[j]][[i]]]], 
				{j, 1, Length[chessboard[[i]]]}],
			{i, 1, Length[chessboard]}
		];
		Table[DeleteCases[nlist, Alternatives @@ Intersection[If[!getMovePossibilities[ennemies[[i]][[2]], True], {}, getMovePossibilities[ennemies[[i]][[2]], True]] , nlist]] , {i, 1, Length[ennemies]}];
	)];
	Return[nlist]
)

(* This method returns all the possibilities for Bishop's moves
The Bishop argument can be null *)
moveBishop[bishop_, x_, y_]:=(
	list = {};
	Table[Table[If[(i ==x + Abs[j -y]) ||(i ==x - Abs[j - y]), AppendTo[list,{i, j}]], {i, 1, Length[chessboard[[j]]]}], {j, 1, Length[chessboard]}];
	nlist = list;
	Table[(
		If[(piece=chessboard[[list[[i]][[2]]]][[list[[i]][[1]]]])[[1]] =!= 0 && bishop[[2]] =!= list[[i]], (
			Delete[list, Position[list,piece]];
			direction = {Sign[x - list[[i]][[1]]], Sign[y - list[[i]][[2]]]};
			nlist = Intersection[nlist, DeleteCases[list, Alternatives @@ Select[list,  Sign[list[[i]][[1]] -#[[1]]] == direction[[1]] && Sign[list[[i]][[2]] - #[[2]]] == direction[[2]]&]]];
		)]
	), {i, 1, Length[list]}];
	Return[nlist]
)

(* This method returns all the possibilities for Knight's moves 
The Knight argument can be null *)
moveKnight[knight_,x_,y_]:=(
	Return[{{x-2,y+1},{x-1,y+2},{x+1,y+2},{x+2,y+1},{x+2,y-1},{x+1,y-2},{x-1,y-2},{x-2,y-1}}]
)

(* This method returns all the possibilities for Rook's moves *)
moveRook[rook_,x_,y_]:=(
	list={};

(* When the rook moves in the y axis *)
	moveCache = True;
	Scan[(
		If[moveCache == True  && y  =!= #,
			(If[chessboard[[#]][[x]][[1]] =!= 0 , moveCache = False];AppendTo[list,{x,#}])
		]
	)&, Reverse[Range[y]]];
	moveCache = True;
	Table[If[moveCache == True && y  =!= i,
		(If[chessboard[[i]][[x]][[1]] =!= 0, moveCache = False];AppendTo[list,{x,i}])
	],{i,y, Length[chessboard]}];

(* When the rook moves in the x axis *)
	moveCache = True;
	Scan[(
		If[moveCache == True  && x  =!= #,
			(If[chessboard[[y]][[#]][[1]] =!= 0 , moveCache = False];AppendTo[list,{#,y}])
		]
	)&, Reverse[Range[x]]];

	moveCache = True;
	Table[If[moveCache == True && x =!= j,
		(If[chessboard[[y]][[j]][[1]] =!= 0, moveCache = False];AppendTo[list,{j,y}])
	],{j, x, Length[chessboard]}];

	Return[list]
)

moveQueen[queen_, x_, y_] := (
	Return[Join[moveRook[queen, x, y], moveBishop[queen, x, y]]]
)


(* ::Section:: *)
(*Rendering*)


(*Rendering...*)
inputCache = {};
Dynamic[inputCache]
roundNumber = 1;
Dynamic[roundNumber]
Board=Table[Mouseover[{If[Divisible[i+j,2],Brown,White],Rectangle[{i,j},{i+1,j+1}]},{If[MemberQ[list,{i+1,j+1}],Green,Red],Rectangle[{i,j},{i+1,j+1}]}],{i,0,7},{j,0,7}];
Pieces=Dynamic[Table[If[#[[1]]=!=0,Inset[Style[#[[1]],25],{#[[2]][[1]]-0.5,#[[2]][[2]]-0.5}]]&/@chessboard[[z]],{z,1,Length[chessboard]}]];
ClickPane[Dynamic@Graphics[{EdgeForm[{Thin,Black}],Board,Pieces}],({

	(* Current Player *)
	playerColor = If[EvenQ[roundNumber], 1, 0];

	(* Moves pieces *)
	If[Length[inputCache] == 0 && getColor[chessboard[[Ceiling[#][[2]]]][[Ceiling[#][[1]]]][[1]]] =!= playerColor, Return[False]];
	AppendTo[inputCache,Ceiling[#]];
	(* Checking if list's length is \[GreaterEqual] 2 *)
	If[Length[inputCache] >= 2, ({
		If[finalizeMove[inputCache[[1]], inputCache[[2]]], ({
			roundNumber = roundNumber + 1;
		})];

		inputCache = {};
	})];

(* Now checking chessmate *)
(* Table[Table[If[chessboard\[LeftDoubleBracket]j\[RightDoubleBracket]\[LeftDoubleBracket]i\[RightDoubleBracket]\[LeftDoubleBracket]1\[RightDoubleBracket] \[Equal] "\[WhiteKing]" || chessboard\[LeftDoubleBracket]j\[RightDoubleBracket]\[LeftDoubleBracket]i\[RightDoubleBracket]\[LeftDoubleBracket]1\[RightDoubleBracket] \[Equal] "\[BlackKing]", king = chessboard\[LeftDoubleBracket]j\[RightDoubleBracket]\[LeftDoubleBracket]i\[RightDoubleBracket]], {i, 1, Length[chessboard\[LeftDoubleBracket]j\[RightDoubleBracket]]}], {j, 1, Length[chessboard]}];
posList = getMovePossibilities[king];
AppendTo[posList, king\[LeftDoubleBracket]2\[RightDoubleBracket]];
Table[Table[DeleteCases[posList, Alternatives @@ Intersection[getMovePossibilities[chessboard\[LeftDoubleBracket]j\[RightDoubleBracket]\[LeftDoubleBracket]i\[RightDoubleBracket]], getMovePossibilities[king]]], {i, 1, Length[chessboard\[LeftDoubleBracket]j\[RightDoubleBracket]]}], {j, 1, Length[chessboard]}]
If[Length[posList] \[Equal] 0, (
locs = {};
Table[Table[If[MemberQ[getMovePossibilities[chessboard\[LeftDoubleBracket]j\[RightDoubleBracket]\[LeftDoubleBracket]i\[RightDoubleBracket]], king\[LeftDoubleBracket]2\[RightDoubleBracket]], (
AppendTo[locs, chessboard\[LeftDoubleBracket]j\[RightDoubleBracket]\[LeftDoubleBracket]i\[RightDoubleBracket]\[LeftDoubleBracket]2\[RightDoubleBracket]];
(* @asdl il faut rajouter les endroits entre les pions et le roi ennemi *)
)], {i, 1, Length[chessboard\[LeftDoubleBracket]j\[RightDoubleBracket]]}], {j, 1, Length[chessboard]}];
)];*)

})&]



