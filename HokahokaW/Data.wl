(* ::Package:: *)

(* Wolfram Language package *)


BeginPackage["HokahokaW`Data`",{"HokahokaW`"}];


HHRaggedArrayDepth::usage = 
"ArrayDepth, but without assuming regular array";


HHRaggedPartition::usage = 
"Partition, but with non-padded overhang at end (potentially sublists of different lengths).";


HHHistogramListQ::usage = 
"";


HHImportAssociation::usage = 
"Automatically loads a JSON file or HDF5 file to association for easy use";


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*HHRaggedArrayDepth*)


HHRaggedArrayDepth[list_List] := HHRaggedArrayDepthImpl[list, 1];


HHRaggedArrayDepthImpl[remainder_List, n_] :=
Module[{ragMin},
	ragMin = Min[ ArrayDepth /@ remainder ];
	If[ ragMin == 0, n, Min[ HHRaggedArrayDepthImpl[#, n+1]& /@ remainder ]]
];  


HHRaggedArrayDepthImpl[{}, n_] := n;


HHRaggedArrayDepth[nonlist_] := 0;


HHRaggedArrayDepth[args___]:=Message[HHRaggedArrayDepth::invalidArgs,{args}];


(* ::Subsection:: *)
(*HHHistogramListQ*)


HHHistogramListQ[list_List/;(HHRaggedArrayDepth[list]==2 && Length[list]==2 && Length[list[[2]]]>0 )] := 
	Length[ list[[1]] ] - Length[ list[[2]] ] == 1;


HHHistogramListQ[list_] := False;


HHHistogramListQ[args___]:=Message[HHHistogramListQ::invalidArgs,{args}];


(* ::Subsection:: *)
(*HHRaggedPartition*)


HHRaggedPartition[list_List, n_/;IntegerQ[n] ] := 
	Partition[ list, n, n, {1,1},{}];
	
HHRaggedPartition[args___]:=Message[HHRaggedPartition::invalidArgs,{args}];


(* ::Subsection::Closed:: *)
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


(* ::Section:: *)
(*End*)


End[];

EndPackage[];
