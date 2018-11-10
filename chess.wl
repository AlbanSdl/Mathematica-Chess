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
	{{"\[WhiteRook]",{1,1},False},{"\[WhiteKnight]",{2,1},False},{"\[WhiteBishop]",{3,1},False},{"\[WhiteQueen]",{4,1},False},{"\[WhiteKing]",{5,1},False},{"\[WhiteBishop]",{6,1},False},{"\[WhiteKnight]",{7,1},False},{"\[WhiteRook]",{8,1},False}},
	{{"\[WhitePawn]",{1,2},False},{"\[WhitePawn]",{2,2},False},{"\[WhitePawn]",{3,2},False},{"\[WhitePawn]",{4,2},False},{"\[WhitePawn]",{5,2},False},{"\[WhitePawn]",{6,2},False},{"\[WhitePawn]",{7,2},False},{"\[WhitePawn]",{8,2},False}},
	{{0,{1,3}}, {0,{2,3}},{0,{3,3}}, {0,{4,3}}, {0,{5,3}}, {0,{6,3}}, {0,{7,3}}, {0,{8,3}}},
	{{0,{1,4}}, {0,{2,4}}, {0,{3,3}},{0,{4,4}}, {0,{5,4}}, {0,{6,4}}, {0,{7,4}}, {0,{8,4}}},
	{{0,{1,5}}, {0,{2,5}}, {0,{3,3}},{0,{4,5}}, {0,{5,5}}, {0,{6,5}}, {0,{7,5}}, {0,{8,5}}},
	{{0,{1,6}}, {0,{2,6}},{0,{3,3}}, {0,{4,6}}, {0,{5,6}}, {0,{6,6}}, {0,{7,6}}, {0,{8,6}}},
	{{"\[BlackPawn]",{1,7},False},{"\[BlackPawn]",{2,7},False},{"\[BlackPawn]",{3,7},False},{"\[BlackPawn]",{4,7},False},{"\[BlackPawn]",{5,7},False},{"\[BlackPawn]",{6,7},False},{"\[BlackPawn]",{7,7},False},{"\[BlackPawn]",{8,7},False}},
	{{"\[BlackRook]",{1,8},False},{"\[BlackKnight]",{2,8},False},{"\[BlackBishop]",{3,8},False},{"\[BlackQueen]",{4,8},False},{"\[BlackKing]",{5,8},False},{"\[BlackBishop]",{6,8},False},{"\[BlackKnight]",{7,8},False},{"\[BlackRook]",{8,8},False}}
};

(* This method returns the color of the piece :
	0 is White,
	1 is Black,
	-1 is undefined
The proper method requires a string (piece char) in order to work. The second method manages the 0 arg and returns -1 (undefined) *)
getColor[x_String:"\[WhitePawn]"]:= If[("\[WhiteKing]" == x || "\[WhiteQueen]" == x || "\[WhiteKnight]" == x|| "\[WhiteBishop]" == x|| "\[WhiteRook]" == x|| "\[WhitePawn]" == x), 0, If[("\[BlackKing]" == x || "\[BlackQueen]" == x || "\[BlackKnight]" == x|| "\[BlackBishop]" == x|| "\[BlackRook]" == x|| "\[BlackPawn]" == x), 1, -1]]
getColor[x_Integer]:=Return[-1]

(* Returns the inversed color (see getColor for more information) *)
getInversedColor[x_Integer]:=If[x == 0, Return[1], If[x == 1, Return[0], Return[-1]]]

(* Returns a string corresponding to the color id *)
getColorName[x_Integer]:=If[x==0, Return["White"], If[x==1, Return["Black"], Return["Undefined"]]]

(* Returns an Integer, corresponding to the piece's type:
	-1 is undefined (empty location)
	0 is a pawn
	1 is a rook
	2 is a knight
	3 is a bishop
	4 is a queen
	5 is a king
*)
getType[piece:0]:= Return[-1]
getType[piece:"\[WhitePawn]"|"\[BlackPawn]"]:= Return[0]
getType[piece:"\[WhiteRook]"|"\[BlackRook]"]:= Return[1]
getType[piece:"\[WhiteKnight]"|"\[BlackKnight]"]:= Return[2]
getType[piece:"\[WhiteBishop]"|"\[BlackBishop]"]:= Return[3]
getType[piece:"\[WhiteQueen]"|"\[BlackQueen]"]:= Return[4]
getType[piece:"\[WhiteKing]"|"\[BlackKing]"]:= Return[5]

(* This function will return True if the king of the given color is threat, False instead.
Note: This funtion will return True if no such king is found.
The color MUST be given in the binary format, corresponding to the output of the method getColor[str] *)
isCheck[color_] := (
	chessboardForEach[Function[chessboardPiece, (
		If[chessboardPiece[[1]] == "\[WhiteKing]" || chessboardPiece[[1]] == "\[BlackKing]" && getColor[chessboardPiece[[1]]] == color, king = chessboardPiece]
	)]];
	If[king =!= Null, Return[isUnderAttack[king]], Return[True]]
)

(* This function will return True if the given is attacked by another one (ennemy), False instead
Please, provide colorUnchecked if the given position has no piece, in order to determine which color has to be checked *)
isUnderAttack[piece_, colorUnchecked_:0] := (
	result = False;
	If[getColor[piece[[1]]] =!= -1, cU = getColor[piece[[1]]], cU = colorUnchecked];
	chessboardForEach[Function[chessboardPiece, (
		If[chessboardPiece[[1]] =!= 0 && cU =!= getColor[chessboardPiece[[1]]] && Length[Intersection[getMovePossibilities[chessboardPiece, True, True], {piece[[2]]}]] =!= 0, result=True; Return[True]]
	)]];
	Return[result]
)

(* This function Iterates the whole chessboard
function is a Function (can be Pure Function) which takes a whole piece as argument e.g: {"\[WhiteKing]", {0, 0}, False} *)
chessboardForEach[function_] := (
	chList = Flatten[chessboard, 1];
	Table[function[chList[[i]]], {i, 1, Length[chList]}]
)


(* ::Section:: *)
(*Locations where pieces can go*)


(*Returns the list of all the moving possibilities for a piece*)
getMovePossibilities[{pawn:"\[WhitePawn]"|"\[BlackPawn]", {x_, y_}, moved_:True|False}, ignoreOthers_:False, eating_:False]:= Return[Select[movePawn[{pawn,{x,y}, moved},x,y, eating], isPlacementAllowed[#,getColor[pawn],{pawn, {x, y}, moved}]&]]
getMovePossibilities[{bishop:"\[WhiteBishop]"|"\[BlackBishop]", {x_, y_}, moved_:True|False}, ignoreOthers_:False, eating_:False]:= Return[Select[moveBishop[{bishop,{x,y}},x,y], isPlacementAllowed[#,getColor[bishop],{bishop, {x, y}, moved}]&]]
getMovePossibilities[{knight:"\[WhiteKnight]"|"\[BlackKnight]", {x_, y_}, moved_:True|False}, ignoreOthers_:False, eating_:False]:= Return[Select[moveKnight[{knight,{x,y}},x,y], isPlacementAllowed[#,getColor[knight],{knight, {x, y}, moved}]&]]
getMovePossibilities[{rook:"\[WhiteRook]"|"\[BlackRook]", {x_, y_}, moved_:True|False}, ignoreOthers_:False, eating_:False]:= Return[Select[moveRook[{rook,{x,y}, moved},x,y, ignoreOthers], isPlacementAllowed[#,getColor[rook],{rook, {x, y}, moved}]&]]
getMovePossibilities[{queen:"\[WhiteQueen]"|"\[BlackQueen]", {x_, y_}, moved_:True|False}, ignoreOthers_:False, eating_:False]:= Return[Select[moveQueen[{queen,{x,y}},x,y], isPlacementAllowed[#,getColor[queen],{queen, {x, y}, moved}]&]]
getMovePossibilities[{king:"\[WhiteKing]"|"\[BlackKing]", {x_, y_}, moved_:True|False}, ignoreOthers_:False, eating_:False]:= Return[Select[moveKing[{king,{x,y}, moved},x,y, ignoreOthers], isPlacementAllowed[#,getColor[king],{king, {x, y}, moved}]&]]
getMovePossibilities[{piece: 0, {x_, y_}, moved_:False}, ignoreOthers_:False, eating_:False] := Return[{}]

(* Checks if the move is allowed for the given piece *)
isPlacementAllowed[location_, color_, piece_]:=(
	If[location ==  piece[[2]], Return[False]];
	If[!(tests = 0 < location[[2]] <= Length[chessboard] && 0 < location[[1]] <= Length[chessboard[[location[[2]]]]]), Return[False]];
	(* Checking castling WITHOUT getCastlingLocations[] to prevent infinite recursion *)
	If[Length[piece] >= 3 && !piece[[3]]
		&& MemberQ[{1, 5}, getType[chessboard[[location[[2]], location[[1]], 1]]]]
		&& getColor[chessboard[[location[[2]], location[[1]], 1]]] == getColor[piece[[1]]]
		&& (location[[1]] == 1 || location[[1]] == Length[chessboard] || location[[1]] == 5)
		&& (location[[2]] == 1 || location[[2]] == Length[chessboard]), Return[True]];
	If[ToString[chessboard[[location[[2]], location[[1]], 1]]] =!= ToString[0], tests = tests && getColor[chessboard[[location[[2]], location[[1]], 1]]] =!= color];
	Return[tests]
)

(* Moves pieces in the matrix.
Please, provide coordiantes as {x, y} *)
finalizeMove[oldPosition_, newPosition_]:=(
	If[0 < oldPosition[[1]] <= Length[chessboard] && 0 < oldPosition[[2]] <= Length[chessboard[[1]]], (
		If[MemberQ[getMovePossibilities[chessboard[[oldPosition[[2]], oldPosition[[1]]]]], newPosition], (
			(* Checking castlings *)
			If[MemberQ[getCastlingLocations[chessboard[[oldPosition[[2]], oldPosition[[1]]]]], newPosition], (
				(* As getType[] is a function, it's considered as too long to execute and will not return the rookX value on time. So we prefer using the old way to check *)
				If[chessboard[[oldPosition[[2]], oldPosition[[1]], 1]] == "\[WhiteKing]" || chessboard[[oldPosition[[2]], oldPosition[[1]], 1]] == "\[BlackKing]", rookX = newPosition[[1]], rookX = oldPosition[[1]]];
				If[rookX == 1, ( (* Queen side castling *)
					chessboard[[oldPosition[[2]], 4]] = {chessboard[[oldPosition[[2]], 1, 1]], chessboard[[oldPosition[[2]], 4, 2]], True};
					chessboard[[newPosition[[2]], 3]] = {chessboard[[newPosition[[2]], 5, 1]], chessboard[[newPosition[[2]], 3, 2]], True};
				), ( (* King side castling *)
					chessboard[[oldPosition[[2]], 6]] = {chessboard[[oldPosition[[2]], 8, 1]], chessboard[[oldPosition[[2]], 6, 2]], True};
					chessboard[[newPosition[[2]], 7]] = {chessboard[[newPosition[[2]], 5, 1]], chessboard[[newPosition[[2]], 7, 2]], True};
				)];
				chessboard[[oldPosition[[2]], oldPosition[[1]]]] = {0, chessboard[[oldPosition[[2]], oldPosition[[1]], 2]]};
				chessboard[[newPosition[[2]], newPosition[[1]]]] = {0, chessboard[[newPosition[[2]], newPosition[[1]], 2]]};
				latestMoveInfo = {0, {0, 0}, {0, 0}};
				Return[True]
			), (
				(* En passant pawn deletion *)
				If[getType[chessboard[[oldPosition[[2]], oldPosition[[1]], 1]]] == 0  && getType[latestMoveInfo[[1]]] == 0 && getColor[latestMoveInfo[[1]]] =!= getColor[chessboard[[oldPosition[[2]], oldPosition[[1]], 1]]] 
					&& Abs[latestMoveInfo[[2, 2]] - latestMoveInfo[[3, 2]]] == 2 && oldPosition[[2]] == latestMoveInfo[[3, 2]] && (oldPosition[[1]] == latestMoveInfo[[3, 1]] + 1 || oldPosition[[1]] == latestMoveInfo[[3, 1]] - 1), (
					If[getType[chessboard[[newPosition[[2]], newPosition[[1]], 1]]] == -1 && newPosition == {latestMoveInfo[[3, 1]], latestMoveInfo[[3, 2]] + If[getColor[latestMoveInfo[[1]]] == 0, -1, 1]},
						chessboard[[latestMoveInfo[[3, 2]], latestMoveInfo[[3, 1]]]] = {0, latestMoveInfo[[3]]};
					]
				)];
				(* Normal moves *)
				chessboard[[newPosition[[2]], newPosition[[1]]]] = {chessboard[[oldPosition[[2]], oldPosition[[1]], 1]], newPosition, True};
				chessboard[[oldPosition[[2]], oldPosition[[1]]]] = {0, chessboard[[oldPosition[[2]], oldPosition[[1]], 2]]};
				promotePawn[chessboard[[newPosition[[2]], newPosition[[1]]]], newPosition[[1]], newPosition[[2]]];
				latestMoveInfo = {chessboard[[newPosition[[2]], newPosition[[1]], 1]], oldPosition, newPosition};
				Return[True];
			)];
		), (Return[False])]
	),
	Return[False]]
)

(* Returns a list of all the locations where the given pawn can go *)
movePawn[pawn_, x_, y_, forceEating_]:=(
	list = {}; (* The list containing all the possible moves *)
	If[getColor[pawn[[1]]]==0, (* Checks if the pawn is white *)
		(
			If[!forceEating && y + 1 <= Length[chessboard] && getColor[ToString[chessboard[[y+1, x, 1]]]] == -1, AppendTo[list, {x, y+1}]] ; (* Adds the first move to the list *)
			If[x =!= Length[chessboard] && y + 1 <= Length[chessboard] && (forceEating || getColor[chessboard[[y+1, x+1, 1]]] == 1), AppendTo[list, {x+1, y+1}]];
			If[x =!= 1 && y + 1 <= Length[chessboard] && (forceEating || getColor[chessboard[[y+1, x-1, 1]]] == 1), AppendTo[list, {x-1, y+1}]];
			If[!forceEating && pawn[[3]] == False && y + 2 <= Length[chessboard] &&  getColor[chessboard[[y+2, x, 1]]] =!= getColor[pawn[[1]]] && getColor[ToString[chessboard[[y+2, x, 1]]]] == -1, (* Adds another possibility if the pawn moves for the first time *)
				AppendTo[list, {x, y+2}]
			]
		),

		(* Now, let's take a look at the black pawn ... *)
		(
			If[!forceEating && 0 < y - 1 <= Length[chessboard] &&  getColor[ToString[chessboard[[y-1, x, 1]]]] == -1, AppendTo[list, {x, y-1}]] ; (* Adds the first move to the list *)
			If[0 < y - 1 <= Length[chessboard] && x =!= Length[chessboard] && (forceEating || getColor[chessboard[[y-1, x+1, 1]]] == 0), AppendTo[list, {x+1, y-1}]];
			If[0 < y - 1 <= Length[chessboard] && x =!= 1 && (forceEating || getColor[chessboard[[y-1, x-1, 1]]] == 0), AppendTo[list, {x-1, y-1}]];
			If[!forceEating && 0 < y - 1 <= Length[chessboard] && pawn[[3]] == False && getColor[chessboard[[y - 2, x, 1]]] =!= getColor[pawn[[1]]] && getColor[ToString[chessboard[[y-2, x, 1]]]] == -1, (* Adds another possibility if the pawn moves for the first time *)
				AppendTo[list, {x, y-2}]
			]
		)
	];
	
	(* En passant detection*)
	If[getType[latestMoveInfo[[1]]] == 0 && getColor[latestMoveInfo[[1]]] =!= getColor[pawn[[1]]] && Abs[latestMoveInfo[[2, 2]] - latestMoveInfo[[3, 2]]] == 2 && y == latestMoveInfo[[3, 2]] && (x == latestMoveInfo[[3, 1]] + 1 || x == latestMoveInfo[[3, 1]] - 1), (
		AppendTo[list, {latestMoveInfo[[3, 1]], latestMoveInfo[[3, 2]] + If[getColor[latestMoveInfo[[1]]] == 0, -1, 1]}]
	)];
	Return[list]
)

(* Returns a list of all the locations where the given king can move *)
moveKing[king_, x_, y_, ignoreOthers_:False]:=(
	list={{x+1, y+1}, {x+1, y}, {x+1, y-1}, {x, y+1}, {x, y-1}, {x-1, y + 1}, {x-1, y}, {x-1, y-1}};
	nlist = {};
	Table[If[isPlacementAllowed[list[[i]], getColor[king[[1]]], king], AppendTo[nlist, list[[i]]]], {i, 1, Length[list]}];
	ennemies = {};
	If[!ignoreOthers, (
		chessboardForEach[Function[chessboardPiece, (
			If[getColor[chessboardPiece[[1]]] =!= getColor[king[[1]]] && getColor[ToString[chessboardPiece[[1]]]] =!= -1 && getType[chessboardPiece[[1]]] =!= 5, nlist = Complement[nlist, getMovePossibilities[chessboardPiece, True, True]]]
		)]];
	)];
	Return[If[ignoreOthers, nlist, Join[nlist, getCastlingLocations[king]]]]
)

(* This method returns all the possibilities for Bishop's moves
The Bishop argument can be null *)
moveBishop[bishop_, x_, y_]:=(
	list = {};
	Table[Table[If[(i ==x + Abs[j -y]) ||(i ==x - Abs[j - y]), AppendTo[list,{i, j}]], {i, 1, Length[chessboard[[j]]]}], {j, 1, Length[chessboard]}];
	nlist = list;
	Table[(
		If[(piece=chessboard[[list[[i, 2]], list[[i, 1]]]])[[1]] =!= 0 && bishop[[2]] =!= list[[i]], (
			Delete[list, Position[list,piece]];
			direction = {Sign[x - list[[i, 1]]], Sign[y - list[[i, 2]]]};
			nlist = Intersection[nlist, DeleteCases[list, Alternatives @@ Select[list,  Sign[list[[i, 1]] - #[[1]]] == direction[[1]] && Sign[list[[i, 2]] - #[[2]]] == direction[[2]]&]]];
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
moveRook[rook_,x_,y_, ignoreOthers_:False]:=(
	list={};

(* When the rook moves in the y axis *)
	moveCache = True;
	Scan[(
		If[moveCache == True  && y  =!= #,
			(If[chessboard[[#, x, 1]] =!= 0 , moveCache = False];
			AppendTo[list,{x,#}])
		]
	)&, Reverse[Range[y]]];
	moveCache = True;
	Table[If[moveCache == True && y  =!= i,
		(If[chessboard[[i, x, 1]] =!= 0, moveCache = False];
		AppendTo[list,{x,i}])
	],{i,y, Length[chessboard]}];

(* When the rook moves in the x axis *)
	moveCache = True;
	Scan[(
		If[moveCache == True  && x  =!= #,
			(If[chessboard[[y, #, 1]] =!= 0 , moveCache = False];
			AppendTo[list,{#,y}])
		]
	)&, Reverse[Range[x]]];

	moveCache = True;
	Table[If[moveCache == True && x =!= j,
		(If[chessboard[[y, j, 1]] =!= 0, moveCache = False];
		AppendTo[list,{j,y}])
	],{j, x, Length[chessboard]}];

	Return[If[ignoreOthers, list, Join[list, getCastlingLocations[rook]]]]
)

(* Returns a list of all the locations where the queen can move *)
moveQueen[queen_, x_, y_] := (
	Return[Join[moveBishop[queen, x, y], moveRook[queen, x, y]]]
)


(* ::Section:: *)
(*Special Moves*)


(* Castling goes here. Please, use the isUnderAttack method for detection.
Piece can be a Rook or King !
Castling cant be performed if one of the locations between the king and the rook is currently attacked (including king and rook)
You can add the locations returned by this function with an Intersection, directly in the moveRook and moveKing methods *)
getCastlingLocations[piece_, verifyAttack_:True] := (
	If[piece[[1]] =!= 0 && (getType[piece[[1]]] == 5 || getType[piece[[1]]] == 1), (
		(* The piece is a rook or a king *)
		If[BooleanQ[piece[[3]]] && !piece[[3]] && If[verifyAttack, !isUnderAttack[piece], True], (
			(* The piece has never moved *)
			If[piece[[2, 1]] == 1, (
				If[getType[chessboard[[piece[[2, 2]], 5, 1]]] == 5 && chessboard[[piece[[2, 2]], 5, 3]] == False && If[verifyAttack, !isUnderAttack[chessboard[[piece[[2, 2]], 5]]], True], (
					auth = True;
					Table[If[chessboard[[piece[[2, 2]], i, 1]] =!= 0 || isUnderAttack[chessboard[[piece[[2, 2]], i]], getColor[piece[[1]]]], auth = False], {i, 2, 4}];
					If[auth, Return[{{5, piece[[2, 2]]}}], Return[{}]]
				), Return[{}]]
			), If[piece[[2, 1]] == Length[chessboard], (
				If[getType[chessboard[[piece[[2, 2]], 5, 1]]] == 5 && chessboard[[piece[[2, 2]], 5, 3]] == False && If[verifyAttack, !isUnderAttack[chessboard[[piece[[2, 2]], 5]]], True], (
					auth = True;
					Table[If[chessboard[[piece[[2, 2]], Length[chessboard] - i, 1]] =!= 0 || isUnderAttack[chessboard[[piece[[2, 2]], Length[chessboard] - i]], getColor[piece[[1]]]], auth = False], {i, 1, 2}];
					If[auth, Return[{{5, piece[[2, 2]]}}], Return[{}]]
				), Return[{}]]
			), (
				resultList = {};
				If[getType[chessboard[[piece[[2, 2]], 1, 1]]] == 1 && Length[chessboard[[piece[[2, 2]], 1]]] >= 3 && chessboard[[piece[[2, 2]], 1, 3]] == False  && If[verifyAttack, !isUnderAttack[chessboard[[piece[[2, 2]], 1]]], True], (
					auth = True;
					Table[If[chessboard[[piece[[2, 2]], i, 1]] =!= 0 || isUnderAttack[chessboard[[piece[[2, 2]], i]], getColor[piece[[1]]]], auth = False], {i, 2, 4}];
					If[auth, AppendTo[resultList, {1, piece[[2, 2]]}]]
				)];
				If[getType[chessboard[[piece[[2, 2]], Length[chessboard], 1]]] == 1 && Length[chessboard[[piece[[2, 2]], Length[chessboard]]]] >= 3 && chessboard[[piece[[2, 2]], Length[chessboard], 3]] == False && If[verifyAttack, !isUnderAttack[chessboard[[piece[[2, 2]], Length[chessboard]]]], True], (
					auth = True;
					Table[If[chessboard[[piece[[2, 2]], Length[chessboard] - i, 1]] =!= 0 || isUnderAttack[chessboard[[piece[[2, 2]], Length[chessboard] - i]], getColor[piece[[1]]]], auth = False], {i, 1, 2}];
					If[auth, AppendTo[resultList, {Length[chessboard], piece[[2, 2]]}]]
				)];
				Return[resultList];
			)]]
		), Return[{}]]
	), Return[{}]]
)

(* Here is the promotion : when a pawn reaches the opposite side of the board, he becomes another piece (rook, knight, bishop or queen)
This function MUST be called in finalizeMove[], NOT IN getMovePossibilities[] *)
promotePawn[pawn_, x_, y_] := (

	(* This function can only be called from promotePawn[...] context *)
	endPromotion[value_] := (
		If[value =!= Null && value =!= $Failed && value =!= $Canceled, chessboard[[y, x, 1]] = value];
		checkCheck[If[getColor[pawn[[1]]] == 0, 1, 0]];
	);

	(* Checks whether the pawn is at a side of the board (and its color) or not *)
	If[getType[pawn[[1]]] =!= Null && getType[pawn[[1]]] == 0 && ((getColor[pawn[[1]]] == 1 && y == 1) || (getColor[pawn[[1]]] == 0 && y == Length[chessboard])), (
		val = "";
		If[getColor[pawn[[1]]] == 0, optionList = {"\[WhiteRook]","\[WhiteKnight]","\[WhiteBishop]","\[WhiteQueen]"}, optionList = {"\[BlackRook]","\[BlackKnight]","\[BlackBishop]","\[BlackQueen]"}];
		CreateDialog[{
			TextCell["Please select your pawn's promotion :", "Text",  CellMargins-> {{10,10}, {5, 10}}],
			Column[{
				Row[{
					ActionMenu["Promotion",{optionList[[1]]:>(val=optionList[[1]]; ), optionList[[2]]:>(val=optionList[[2]];), optionList[[3]]:>(val=optionList[[3]];), optionList[[4]]:>(val=optionList[[4]];)}],
					Dynamic[TextCell[val, "Subtitle"]]
				}],
				Button["Ok", (endPromotion[val];DialogReturn[]), Enabled->Dynamic[val =!= ""], Alignment->Center, Method->"Queued", ImageMargins->{{0,0},{5,0}}]
			}]
		}, Modal->True, WindowTitle-> "Pawn promotion", WindowFrameElements-> Null
		];
	)];
)


(* ::Section:: *)
(*Check and Gameover*)


(* This function checks if there is check and will stop the game is necessary *)
checkCheck[playerColor_] := (
	displayedCheck = "";
	If[isCheck[playerColor], (
		If[playerColor == 0,
			checkWhite = True,
			checkBlack = True
		];
		
		(* Now checking checkmate *)
		chessboardForEach[Function[chessboardPiece, (
			If[getType[chessboardPiece[[1]]] == 5 && getColor[chessboardPiece[[1]]] == playerColor, king = chessboardPiece]
		)]];
		(* Stops the game if no such king is found *)
		If[king == Null, running=False; displayedCheck = "No such king detected... Stopping the game, please restart the package to continue."];
		posList = getMovePossibilities[king];
		If[Length[posList] == 0, (
			(* There is check and the king can't move *)
			locs = {};
			(* Adding all the locations where pieces can go, in order to cancel the check *)
			chessboardForEach[Function[chessboardPiece, (
				If[MemberQ[getMovePossibilities[chessboardPiece], king[[2]]] && getColor[chessboardPiece[[1]]] == getInversedColor[getColor[king[[1]]]], (
					dtLocs = {};
					AppendTo[dtLocs, chessboardPiece[[2]]];
					(* adds all the locations between the king and the ennemy *)
					ennemyLocs = getMovePossibilities[chessboardPiece];
					ennemyLocs = Complement[ennemyLocs, {king[[2]]}]; (* removing king location *)
					diff = {Sign[chessboardPiece[[2, 1]] - king[[2, 1]]], Sign[chessboardPiece[[2, 2]] - king[[2, 2]]]};
					(* If the location where the ennemy can go is in the same direction as the king, it's added in dtLocs *)
					Table[If[Sign[chessboardPiece[[2, 1]] - ennemyLocs[[i, 1]]] == diff[[1]] && Sign[chessboardPiece[[2, 2]] - ennemyLocs[[i, 2]]] == diff[[2]], AppendTo[dtLocs, ennemyLocs[[i]]]], {i, 1, Length[ennemyLocs]}];
					(* Adding all the locs IN A LIST to locs *)
					AppendTo[locs, dtLocs];
				)]
			)]];
			(* Now, in each list contained in locs, at least one location must be reached in order to cancel the check *)
			confirmedCheck = False;
			Table[(
				If[confirmedCheck, Return[True]];
				localCheck = True;
				(* Browsing the Chess in order to find pieces of the same color as the king which can cancel the Check *)
				Table[(
					If[!localCheck, Return[False]];
					Table[(
						If[getColor[chessboard[[j, i, 1]]] == getColor[king[[1]]] && Length[Intersection[getMovePossibilities[chessboard[[j, i]]], locs[[k]]]] =!= 0, localCheck = False; Return[False]]
					), {i, 1, Length[chessboard[[1]]]}]
				), {j, 1, Length[chessboard]}];
				confirmedCheck = confirmedCheck || localCheck;
			), {k, 1, Length[locs]}];
			
			If[confirmedCheck, (
				displayedCheck = StringJoin["Checkmate ! ", If[getColor[king[[1]]] == 0, "Black", "White"], " player won !"];
				running = False
			)];
			
		)]),
		(* else clause *)
		(
			If[playerColor == 0, checkWhite = False, checkBlack = False];
			
			(* Checking if next player can move = StaleMate *)
			chessList = Flatten[chessboard, 1];
			isStaleMate = True;
			Table[If[getColor[chessList[[i, 1]]] == If[EvenQ[roundNumber], 1, 0], If[Length[getMovePossibilities[chessList[[i]]]] =!= 0, isStaleMate=False;Return[True]]], {i, 1, Length[chessList]}];
			If[isStaleMate, running = False; displayedCheck = "StaleMate: Player can't move anymore, the game is draw !"]
		)
				
	];
	
	(* Using Dynamic variables instead of Print to prevent mathematica's error console opening *)
	If[displayedCheck == "", 
		If[checkWhite, displayedCheck = "Check ! White player, protect your king !"];
		If[checkBlack, (
			If[displayedCheck =!= "", displayedCheck = StringJoin[displayedCheck, "\n"]];
			displayedCheck = StringJoin[displayedCheck, "Check ! Black player, protect your king !"]
		)]
	]
	
)


(* ::Section:: *)
(*Rendering*)


(*** Most of the following values are dynamic for a debug purpose ***)

(* Determines if the game is running or not *)
running = True;

(* A list which keeps the latest click position (converted in location in chessboard *)
inputCache = {};

(* The number of the round. If the round number is even, black player has to play *)
roundNumber = 1;

(* This list contains all the locations where the selected piece can go. It's used in order to color locations in green with mouseover*)
moveList = {};

(* All the following values are cache for check (True if check)
These values are only updated just before the player's round *)
checkWhite = False;
checkBlack = False;

displayedCheck = "";

(* The latest move information :
the first element of the list is the piece (as string),
the second is the old position,
the third is the new position *)
latestMoveInfo = {0, {0, 0}, {0, 0}};

(* The board is brown and white and updated each time the list used in mouseover changes *)
Board=Dynamic[Table[Mouseover[{If[Divisible[i+j,2],Brown,White],Rectangle[{i,j},{i+1,j+1}]},{If[MemberQ[moveList,{i+1,j+1}],Green,Red],Rectangle[{i,j},{i+1,j+1}]}],{i,0,7},{j,0,7}]];
(* The 'pieces' variable represents all the pieces which are drawn on the board, depending on the matrix 'chessboard' *)
Pieces=Dynamic[
	Table[
		If[#[[1]] =!= 0,
			Inset[Style[#[[1]], 25],
			{#[[2, 1]] - 0.5, #[[2, 2]] - 0.5}]
		] &/@chessboard[[z]],{z,1,Length[chessboard]}
	]
];
(* The clickpane detects all the clicks and will convert the coordinates to locations in the matrix 'chessboard'. All the methods are called from here 
The variable running defined above, will stop the clickpane detection is set to False *)
ClickPane[Dynamic@Graphics[{
		EdgeForm[{Thin,Black}],
		Board,
		Pieces
	},
	PlotLabel->Style[
		StringJoin[
			If[running, StringJoin[getColorName[If[EvenQ[roundNumber], 1, 0]], " player has to play"], "Game has ended !"],
			"\n", displayedCheck
		],
		FontSize->18
	]],({If[running, {

	(* Current Player *)
	playerColor = If[EvenQ[roundNumber], 1, 0];
	
	(* Checking if click is on the board,  to prevent from crashes *)
	If[Ceiling[#][[1]] > Length[chessboard] || Ceiling[#][[2]] > Length[chessboard] || Ceiling[#][[1]] < 1 || Ceiling[#][[2]] < 1, Return[False]];
	(* Moves pieces *)
	If[Length[inputCache] == 0 && getColor[chessboard[[Ceiling[#][[2]], Ceiling[#][[1]], 1]]] =!= playerColor, Return[False]];
	AppendTo[inputCache, Ceiling[#]];
	If[Length[inputCache] == 1, (
		moveList = getMovePossibilities[chessboard[[inputCache[[1, 2]], inputCache[[1, 1]]]]];
	)];
	(* Checking if list's length is \[GreaterEqual] 2 *)
	If[Length[inputCache] >= 2, ({
		If[inputCache[[1]] == inputCache[[2]], (
			inputCache = {};
			moveList = {};
			Return[False]
		)];
		If[finalizeMove[inputCache[[1]], inputCache[[2]]], 
			roundNumber++;
			
			(* Checking check at the end of each round *)
			checkCheck[If[EvenQ[roundNumber], 1, 0]];
		];

		(* Move complete, cleaning cache and lists *)
		moveList = {};
		inputCache = {};
		
	})]

}] (* End of is running check *)})&]
