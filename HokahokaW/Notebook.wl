(* ::Package:: *)

(* Wolfram Language package *)


BeginPackage["HokahokaW`Notebook`",{"HokahokaW`"}];


HHNotebookCreate::usage="Alias for CreateNotebook[]. Takes options to apply \
to the created NotebookObject. Saves the created notebook in \
$HHCurrentNotebook to use in further writing functions";
HHNotebookCreate$OverrideOptions = {PrintingOptions->
		{
		"PrintingMargins"->72*{{0.25(*Left*), 0.25},{0.25(*bottom*), 0.5(*top*)}}, 
		"PaperOrientation"->"Portrait",
		"PageSize"->72*{8.27, 11.69}
		}};
Options[HHNotebookCreate]=HHJoinOptionLists[
	HHNotebookCreate$OverrideOptions,
	Options[CreateNotebook]
];


Options[CreateNotebook]


HHOptNotebook::usage = "";
HHOptCellType::usage = "";


HHNotebookWriteCell::usage="Use NotebookWrite to write contents as a cell.";
Options[HHNotebookWriteCell]=Join[
	{HHOptNotebook :> $HHCurrentNotebook},
	Options[Cell]
];


HHNotebookWrite::usage="Write given cell to current notebook";
Options[HHNotebookWrite]=Join[
	{HHOptNotebook :> $HHCurrentNotebook},
	Options[NotebookWrite]
];


HHNotebookSetOptions::usage="";
Options[HHNotebookSetOptions]={HHOptNotebook :> $HHCurrentNotebook};


HHNotebookSave::usage="Use NotebookSave to save a notebook or \
NotebookPrint to save a *.pdf of the notebook, depending upon the file extension.";
Options[HHNotebookSave]={HHOptNotebook :> $HHCurrentNotebook(*, 
	"AllowRasterization"\[Rule]True, ImageResoultion\[Rule]300*)};


HHNotebookClose::usage="Alias for NotebookClose, applied to $HHCurrentNotebook";
Options[HHNotebookClose]={HHOptNotebook :> $HHCurrentNotebook};


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


$HHCurrentNotebook = Null;


(* ::Subsection::Closed:: *)
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


(*Writing malformed cells to non-visible notebooks can lead to pernicious Heisenbugs!*)
(*Be really really careful when touching anything!*)


(*HHNotebookWriteCell[contents_TextCell, opts:OptionsPattern[]] := 
NotebookWrite[ OptionValue[ HHOptNotebook ], contents,
 			Sequence@@FilterRules[{opts}, Options[NotebookWrite]]
];*)


HHNotebookWriteCell[contents_, opts:OptionsPattern[]] := HHNotebookWriteCell[contents, "Output", opts];


(*HHNotebookWriteCell[contents_List, cellStyle___String, opts:OptionsPattern[]] := 
	HHNotebookWriteCell[#, cellStyle, opts]& /@ contents;*)


HHNotebookWriteCell[contents_String, cellStyle___String, opts:OptionsPattern[]] := 
NotebookWrite[ OptionValue[ HHOptNotebook ],
	Cell[ contents, cellStyle, opts ]
];


(*HHNotebookWriteCell[contents_Row, cellStyle___String, opts:OptionsPattern[]] := 
NotebookWrite[ OptionValue[ HHOptNotebook ],
	TextCell[ contents, cellStyle, opts ]
];*)


HHNotebookWriteCell[contents_, cellStyle___String, opts:OptionsPattern[]] := 
NotebookWrite[ OptionValue[ HHOptNotebook ],
	Cell[ BoxData[ToBoxes[ contents ]], 
			FilterRules[{opts}, Options[Cell]], cellStyle, opts 
	]
];


HHNotebookWriteCell[args___]:=Message[HHNotebookWriteCell::invalidArgs,{args}];


(* ::Subsection:: *)
(*HHNotebookWrite*)


HHNotebookWrite[contents_Cell, opts:OptionsPattern[]] := 
	NotebookWrite[ OptionValue[ HHOptNotebook ], contents, Sequence@@FilterRules[{opts}, Options[NotebookWrite]]
];


HHNotebookWrite[args___]:=Message[HHNotebookWrite::invalidArgs,{args}];


(* ::Subsection::Closed:: *)
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
