(* ::Package:: *)

(* Wolfram Language package *)


BeginPackage["HokahokaW`Data`",{"HokahokaW`"}];


HHRaggedArrayDepth::usage = 
"ArrayDepth, but without assuming regular array";


HHRaggedPartition::usage = 
"Partition, but with non-padded overhang at end (potentially sublists of different lengths).";


HHRaggedTranspose::usage = 
"Transpose a ragged (currently 3 dimensional) list.";


HHHistogramListQ::usage = 
"";


HHImportAssociation::usage = 
"Automatically loads a JSON file or HDF5 file to association for easy use";


HHFromLetterNumber::usage="Same as FromLetterNumber, except that numbers over the alphabet count (26 in \"English\" will "<>
"give multi-letter results (e.g. 27 \[Rule] \"aa\"";
Options[HHFromLetterNumber]=Options[FromLetterNumber];


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*HHRaggedArrayDepth*)


(*HHRaggedArrayDepth[list_List] := HHRaggedArrayDepthImpl[list, 1];*)


HHRaggedArrayDepth[{}] := 1;


HHRaggedArrayDepth[list_List] :=
Module[{temp},
	temp = If[ Head[#]===List, HHRaggedArrayDepth[#], 0]& /@ list;
	Max[ temp ] + 1
];  


(*HHRaggedArrayDepthImpl[remainder_List, n_] :=
Module[{ragMin},
	ragMin = Min[ ArrayDepth /@ remainder ];
	If[ ragMin == 0, n, Min[ HHRaggedArrayDepthImpl[#, n+1]& /@ remainder ]]
];  *)


HHRaggedArrayDepth[nonlist_] := 0;


HHRaggedArrayDepth[args___]:=Message[HHRaggedArrayDepth::invalidArgs,{args}];


(* ::Subsection::Closed:: *)
(*HHRaggedPartition*)


HHRaggedPartition[list_List, n_/;IntegerQ[n] ] := 
	Partition[ list, n, n, {1,1},{}];
	
HHRaggedPartition[args___]:=Message[HHRaggedPartition::invalidArgs,{args}];


(* ::Subsection::Closed:: *)
(*HHRaggedTranspose*)


HHRaggedTranspose[list_List/;(
	HHRaggedArrayDepth[list]==3 &&
	Length[Union[ Dimensions[#][[2]]& /@ list]]==1
) ] := 
Module[{tempMax, tempAccum},
	tempMax = Max[ Dimensions[#, 1]& /@ list ];
	Table[
		tempAccum = {};
		Do[
			If[ Length[ list[[n2]] ] >= n1, AppendTo[ tempAccum, list[[n2, n1]] ] ],
			{n2, 1, Length[ list ]}
		];
		tempAccum,
		{n1, 1, tempMax}
	]
];


HHRaggedTranspose[list_List/;(HHRaggedArrayDepth[list]==2) ] := 
Module[{tempMax, tempAccum},
	tempMax = Max[ Dimensions[#, 1]& /@ list ];
	Table[
		tempAccum = {};
		Do[
			If[ Length[ list[[n2]] ] >= n1, AppendTo[ tempAccum, list[[n2, n1]] ] ],
			{n2, 1, Length[ list ]}
		];
		tempAccum,
		{n1, 1, tempMax}
	]
];


HHRaggedTranspose[args___]:=Message[HHRaggedTranspose::invalidArgs,{args}];


(* ::Subsection:: *)
(*HHHistogramListQ*)


HHHistogramListQ[list_List/;(HHRaggedArrayDepth[list]==2 && Length[list]==2 && Length[list[[2]]]>0 )] := 
	Length[ list[[1]] ] - Length[ list[[2]] ] == 1;


HHHistogramListQ[list_] := False;


HHHistogramListQ[args___]:=Message[HHHistogramListQ::invalidArgs,{args}];


(* ::Subsection:: *)
(*HHImportAssociation*)


HHImportAssociation[fileName_String] := 
Module[{},
	Switch[ FileExtension[fileName],
		"h5", HHImportAssociation$HDF5[fileName],
		"json", HHImportAssociation$JSON[fileName],
		_, Message[HHImportAssociation::formatError, FileExtension[fileName]]; <||>
	]
];

HHImportAssociation::formatError="Format for the following extension not programmed: `1`";

HHImportAssociation[args___]:=Message[HHImportAssociation::invalidArgs,{args}];


HHImportAssociation$HDF5[fileName_String] := 
Module[{tempKeys, tempret},
	tempKeys = Import[fileName, {"Datasets"}];
	tempret = Association@@(
	 Rule[StringTake[ #, {2, -1} ], Import[ fileName, {"Datasets", # }]]& /@ tempKeys
	);
	Map[ ToExpression, tempret, {-1} ]
];


HHImportAssociation$JSON[fileName_String] := 
Module[{tempExpr, tempret},
	tempret = Association@@Import[fileName];
	Quiet[
		Map[ (
			tempExpr = ToExpression[#];
			If[tempExpr === $Failed, #, tempExpr]
		)&, tempret, {-1} ],
		{ToExpression::sntx, ToExpression::sntxi}
	]
];


(* ::Subsection:: *)
(*HHFromLetterNumber*)


HHFromLetterNumber[n_]:=HHFromLetterNumber[n,"English"]


HHFromLetterNumber[n_, alpha_]:=
Module[{tempList=Alphabet[alpha]},
	StringJoin[FromLetterNumber[#, alpha]& /@ IntegerDigits[ n, Length[tempList]]]
];


HHFromLetterNumber[args___]:=Message[HHFromLetterNumber::invalidArgs,{args}];


(* ::Section:: *)
(*End*)


End[];

EndPackage[];
