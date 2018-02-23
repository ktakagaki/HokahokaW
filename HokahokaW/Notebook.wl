(* ::Package:: *)

(* Wolfram Language package *)


BeginPackage["HokahokaW`Notebook`",{"HokahokaW`"}];


HHNotebookCreate::usage="Alias for CreateNotebook[]. Takes options to apply \
to the created NotebookObject. Saves the created notebook in \
$HHCurrentNotebook to use in further writing functions";
HHNotebookCreate$OverrideOptions = {PrintingOptions->
		{"PrintingMargins"->72*{{0.25(*Left*),0.25},{0.25(*bottom*),0.25(*top*)}}, 
		"PaperOrientation"->"Portrait"}};
Options[HHNotebookCreate]=HHJoinOptionLists[
	HHNotebookCreate$OverrideOptions,
	Options[CreateNotebook]
];


HHOptNotebook::usage = "";
HHOptCellType::usage = "";


HHNotebookWriteCell::usage="Use NotebookWrite to write a cell.";
Options[HHNotebookWriteCell]=Join[
	{HHOptNotebook :> $HHCurrentNotebook},
	Options[Cell]
];


HHNotebookSetOptions::usage="";
Options[HHNotebookSetOptions]={HHOptNotebook :> $HHCurrentNotebook};


HHNotebookSave::usage="Use NotebookSave to save a notebook or \
NotebookPrint to save a *.pdf of the notebook, depending upon the file extension.";
Options[HHNotebookSave]={HHOptNotebook :> $HHCurrentNotebook};


HHNotebookClose::usage="Alias for NotebookClose, applied to $HHCurrentNotebook";
Options[HHNotebookClose]={HHOptNotebook :> $HHCurrentNotebook};


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


$HHCurrentNotebook = Null;


(* ::Subsection:: *)
(*HHNotebookCreate*)


HHNotebookCreate[opts:OptionsPattern[]]:=
($HHCurrentNotebook = 
	CreateNotebook[Sequence@@FilterRules[ 
		Join[{opts}, HHNotebookCreate$OverrideOptions], 
		Options[CreateNotebook]]
	]
(*If[Length[{opts}]>0, SetOptions[ $HHCurrentNotebook, opts]];*)
);


HHNotebookCreate[args___]:=Message[HHNotebookCreate::invalidArgs,{args}];


(* ::Subsection:: *)
(*HHNotebookWriteCell*)


HHNotebookWriteCell[contents_List, cellStyle___String, opts:OptionsPattern[]] := 
	HHNotebookWriteCell[#, cellStyle, opts]& /@ contents;


HHNotebookWriteCell[contents_String, cellStyle___String, opts:OptionsPattern[]] := 
NotebookWrite[ OptionValue[ HHOptNotebook ],
	Cell[ contents, cellStyle, opts ]
];


HHNotebookWriteCell[contents_, cellStyle___String, opts:OptionsPattern[]] := 
NotebookWrite[ OptionValue[ HHOptNotebook ],
	Cell[ BoxData[ToBoxes[ contents ]], 
			FilterRules[{opts}, Options[Cell]], cellStyle, opts 
	]
];


HHNotebookWriteCell[args___]:=Message[HHNotebookWriteCell::invalidArgs,{args}];


(* ::Subsection:: *)
(*HHNotebookSetOptions*)


HHNotebookSetOptions[options_List, opts:OptionsPattern[]]:=
	SetOptions[ OptionValue[HHOptNotebook], options ];	


HHNotebookSetOptions[args___]:=Message[HHNotebookSetOptions::invalidArgs,{args}];


(* ::Subsection:: *)
(*HHNotebookSave*)


HHNotebookSave[fileName_String, opts:OptionsPattern[]]:=
If[ StringMatchQ[ fileName, __~~".nb" ],
	NotebookSave[ OptionValue[HHOptNotebook], fileName ],
	If[ StringMatchQ[ fileName, __~~".pdf" ],
		NotebookPrint[ OptionValue[HHOptNotebook], fileName ],
		NotebookSave[ OptionValue[HHOptNotebook], fileName<>".nb" ]
	]
];	


HHNotebookSave[opts:OptionsPattern[]]:=
	NotebookSave[ OptionValue[HHOptNotebook] ];	


HHNotebookSave[args___]:=Message[HHNotebookSave::invalidArgs,{args}];


(* ::Subsection:: *)
(*HHNotebookClose*)


HHNotebookClose[opts:OptionsPattern[]]:= NotebookClose[ OptionValue[HHOptNotebook] ];


HHNotebookClose[args___]:=Message[HHNotebookClose::invalidArgs,{args}];


(* ::Section:: *)
(*Ending*)


End[];
EndPackage[];


(* ::Section:: *)
(*Bak*)
