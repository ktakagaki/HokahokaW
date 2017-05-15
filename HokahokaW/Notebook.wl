(* ::Package:: *)

(* Wolfram Language package *)


BeginPackage["HokahokaW`Notebook`",{"HokahokaW`"}];


HHNotebookCreate::usage="Alias for CreateNotebook[]. Takes options to apply \
to the created NotebookObject. Saves the created notebook in \
$HHCurrentNotebook to use in further writing functions";
Options[HHNotebookCreate]=Options[NotebookObject];


HHOptNotebook::usage = "";
HHOptCellType::usage = "";


HHNotebookWrite::usage="Use NotebookWrite to write a cell.";
Options[HHNotebookWrite]={HHOptNotebook :> $HHCurrentNotebook, HHOptCellType -> Automatic};


HHNotebookSave::usage="Use NotebookSave to save a notebook or \
NotebookPrint to save a *.pdf of the notebook, depending upon the file extension.";
Options[HHNotebookSave]={HHOptNotebook :> $HHCurrentNotebook};


HHNotebookClose::usage="Alias for NotebookClose, applied to $HHCurrentNotebook";
Options[HHNotebookClose]={HHOptNotebook :> $HHCurrentNotebook};


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


$HHCurrentNotebook = Null;


(* ::Subsection::Closed:: *)
(*HHNotebookCreate*)


HHNotebookCreate[variables___, opts:OptionsPattern]:=
Block[{},
	$HHCurrentNotebook = CreateNotebook[variables];
	If[Length[{opts}]>0, SetOptions[ $HHCurrentNotebook, opts]];
];


HHNotebookCreate[args___]:=Message[HHNotebookCreate::invalidArgs,{args}];


(* ::Subsection:: *)
(*HHNotebookWriteCell*)


HHNotebookWriteCell[contents_List, opts:OptionsPattern[]] := 
	HHNotebookWriteCell[#, opts]& /@ contents;


HHNotebookWriteCell[contents_String, opts:OptionsPattern[]] := 
NotebookWrite[ OptionValue[ HHOptNotebook ],
	If[ OptionValue[HHOptCellType] === Automatic,
		Cell[ contents ],
		Cell[ contents, OptionValue[HHOptCellType]]
	]];


HHNotebookWriteCell[contents_, opts:OptionsPattern[]] := 
NotebookWrite[ OptionValue[ HHOptNotebook ],
	If[ OptionValue[HHOptCellType] === Automatic,
		Cell[ BoxData[ToBoxes[ contents ]] ],
		Cell[ BoxData[ToBoxes[ contents ]], OptionValue[HHOptCellType]]
	]];


HHNotebookWriteCell[args___]:=Message[HHNotebookWriteCell::invalidArgs,{args}];


(* ::Subsection:: *)
(*HHNotebookSave*)


HHNotebookSave[fileName_String, opts:OptionsPattern]:=
If[ StringMatchQ[ fileName, __~~".nb" ],
	NotebookSave[ OptionValue[HHOptNotebook], fileName ],
	If[ StringMatchQ[ fileName, __~~".pdf" ],
		NotebookPrint[ OptionValue[HHOptNotebook], fileName ],
		NotebookSave[ OptionValue[HHOptNotebook], fileName<>".nb" ]
	]
];	


HHNotebookSave[args___]:=Message[HHNotebookSave::invalidArgs,{args}];


(* ::Subsection:: *)
(*HHNotebookClose*)


HHNotebookClose[opts:OptionsPattern]:= NotebookClose[ OptionValue[HHOptNotebook] ];


HHNotebookClose[args___]:=Message[HHNotebookClose::invalidArgs,{args}];


(* ::Section:: *)
(*Ending*)


End[];
EndPackage[];


(* ::Section:: *)
(*Bak*)
