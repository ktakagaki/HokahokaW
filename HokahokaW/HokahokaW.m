(* ::Package:: *)

(* Wolfram Language package *)


BeginPackage["HokahokaW`",{"JLink`"}];


General::invalidArgs="Function called with invalid arguments `1`.";
General::invalidOptionValue="Option argument `2` -> `1` is invalid.";
General::deprecated="Function is deprecated, use `1` instead.";
General::nullArgument="At least one of the required arguments is null!";


(* ::Subsection:: *)
(*Rules*)


HHRuleListQ::usage=
	"HHRuleListQ[ruleList_List]... returns whether the argument ruleList is a list of rules.";
HHRuleQ::usage=
	"HHRuleQ[rule_]... returns whether the argument ruleList is a list of Rule or RuleDelayed objects.";


HHJoinOptionLists::usage=
"HHJoinOptionLists[x_/;RuleQ[x], y_/;RuleQ[y]]...   joins the two option lists, "<>
"and if an option specified in x is repeated in y, the specification in y is dropped.

HHJoinOptionLists[x_/;RuleQ[x], y_/;RuleQ[y], z_/;RuleQ[z]]...    joins the three option lists, "<>
"and if an option specified in x is repeated in y, the specification in y is dropped, etc.

HHJoinOptionLists[symbol_Symbol, x_/;RuleQ[x], y_/;RuleQ[y],... ]...    Does the "<>
"same as above, but filters the rules for Option[Symbol] before returning.";


HHAddOptions::usage=
"HHAddOptions[object, opts]...     returns the original object (e.g. NNMData[<<>>, opts]), "<>
"but with the specified option(s) appended or replaced. opts can be specified either as a Sequence "<>
"or a List of rules (i.e., brackets {opts} are optional).";


HHOptionValue::usage= "Can be used to extract options from an object, such as a Graphic[..., opt->optval]."
HHAbsoluteOptionValue::usage= "Can be used to extract absolute options from an object, such as a Graphic[..., opt->optval]."


(*HHExtractRules[x_[arg___]]:=Flatten[If[HHRuleQ[#],#,{}]& /@ {arg}];
HHExtractRules[args___]:=Message[HHExtractRules::invalidArgs,{args}];*)


(* ::Subsection:: *)
(*Java*)


HHJavaObjectQ::usage="Checks whether something is a Java object and an instance of the given class/interface.";


HHIncreaseJavaStack::usage="Increases the Java stack size.";


(* ::Subsection::Closed:: *)
(*Package Git functions *)


HHPackageNewestFileDate::usage="Prints the newest file change date for all files within the given package or within the current NotebookDirectory[].";
HHPackageGitRemotes::usage="Prints a list of git remotes for either the given package or the current NotebookDirectory[].";
HHPackageGitHEAD::usage="Prints the git HEAD hash for either the given directory or the current NotebookDirectory[].";
HHPackageUpdateGitHEADFile::usage="Bundles a GitHEAD.m file for marking deployments without active Git management.";
HHPackageMessage::usage="Prints standard package message.";
(* TODO: test that notebook message is working with HHPackageMessage[] *)
(*HHNotebookMessage::usage="Prints standard notebook message.";*)


(* ::Subsection:: *)
(*Utilities*)


HHPadZeros::usage =
"HHPadZeros[n] gives the numeral n string padded to 3 digits with zeros. " <>
"HHPadZeros[n,m] gives the numeral n string padded to m digits with zeros.";


HHPrintAssignmentCell::usage =
"Prints the given symbols and their value assignements in a cell. Use to record value assignments in a notebook.";
Options[HHPrintAssignmentCell]={"PrintDateString"-> True, "PrintFilename"->True};


HHSymbolNotNull::usage = 
"Checks whether the given symbol name(s) given as String or List of String are not Null.";


HHCreateDirectoryIfNone::usage = 
"Checks whether the given directory name exists, and if not, calls CreateDirectory.";


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Rules*)


HHRuleListQ[ruleList_List] := And @@ (HHRuleListQ /@ ruleList);
HHRuleListQ[rule_Rule] := True;
HHRuleListQ[rule_RuleDelayed] := True;
HHRuleListQ[rules__] := HHRuleListQ[{rules}];
HHRuleListQ[] := True;
HHRuleListQ[{}] := True;
HHRuleListQ[_] := False;
HHRuleListQ[args___]:=Message[HHRuleListQ::invalidArgs,{args}];


HHRuleQ[rule_Rule] := True;
HHRuleQ[rule_RuleDelayed] := True;
HHRuleQ[_] := False;
HHRuleQ[args___]:=Message[HHRuleQ::invalidArgs,{args}];


HHJoinOptionLists[x_/;HHRuleListQ[x], y_/;HHRuleListQ[y]]:=
	Module[{tempretNN},
		tempretNN=Join[Flatten[x], FilterRules[Flatten[y], Except[x]]];
		Return[tempretNN]
	];

HHJoinOptionLists[x_/;HHRuleListQ[x], y__/;(And@@(HHRuleListQ /@ {y}))]:=
	Module[{tempretNN},
		tempretNN=x;
		Do[tempretNN=HHJoinOptionLists[tempretNN, zz],{zz,{y}}];
		Return[tempretNN]
	];

HHJoinOptionLists[symbol_Symbol, x_/;HHRuleListQ[x]]:=
	Module[{},
		Return[FilterRules[x, Options[symbol]]]
	];

HHJoinOptionLists[symbol_Symbol, x_/;HHRuleListQ[x], y__/;(And@@(HHRuleListQ /@ {y}))]:=
	Module[{tempretNN},
		tempretNN = HHJoinOptionLists[x, y];
		Return[FilterRules[tempretNN, Options[symbol]]]
	];

HHJoinOptionLists[symbol_[contents_], x_/;HHRuleListQ[x], y__/;(And@@(HHRuleListQ /@ {y}))]:=
	Module[{tempretNN},
		tempretNN = HHJoinOptionLists[x, y];
		Return[FilterRules[tempretNN, Options[symbol]]]
	];

HHJoinOptionLists[args___]:=Message[HHJoinOptionLists::invalidArgs,{args}];


HHAddOptions[symbol_[contents___], opts___]:=HHAddOptions[symbol[contents], {opts}];
HHAddOptions[symbol_[contents___], {opts___}]:=
	Module[{tempretNN(*,oldRules*)},
		(*The following will strip off rules from the end.*)
		tempretNN = Select[{contents}, !HHRuleQ[#]&];
		(*Append old rules which are not given in opts.*)

		tempretNN = Append[tempretNN, Hold[Sequence@@HHJoinOptionLists[symbol, {opts}, Select[{contents}, RuleQ]]] ];
		Return[ReleaseHold[ symbol[Sequence@@tempretNN] ]]
		(*oldRules=FilterRules[Options[x], Except[opts]];
			If[Length[oldRules]>0, tempretNN= Append[tempretNN, Hold[Sequence@@oldRules]]];
		(*Append new rules given in opts*)
		tempretNN= Append[tempretNN, Hold[opts]];
		Return[ReleaseHold[tempretNN]]*)
	];

HHAddOptions[args___]:=Message[HHAddOptions::invalidArgs,{args}];


HHOptionValue[x_, optionSymbol_]:=Module[{tempOpts},
	tempOpts=Join[Options[x],Options[Head[x]]];
	OptionValue[ tempOpts ,optionSymbol ]
];
HHOptionValue[args___]:=Message[HHOptionValue::invalidArgs,{args}];

HHAbsoluteOptionValue[x_, optionSymbol_]:=Module[{tempOpts},
	tempOpts=Join[AbsoluteOptions[x],Options[Head[x]]];
	OptionValue[ tempOpts ,optionSymbol ]
];


(* ::Subsection:: *)
(*Java*)


HHJavaObjectQ[x_/;JavaObjectQ[x]]:= True;

HHJavaObjectQ[x_/;JavaObjectQ[x], className_String]:= InstanceOf[x, className];

HHJavaObjectQ[___]:= False;


HHIncreaseJavaStack[stackSize_Integer]:=
	Module[{tempOptStringI,tempOptStringR,tempReI=False, tempReR=False, 
		tempPrint},
		
		tempPrint=PrintTemporary["Checking Java stack size..."];

    	(*Extract the stack settings for InstallJava*)
		tempOptStringI=OptionValue[JLink`InstallJava, JLink`JVMArguments];
		If[tempOptStringI===None, tempReI=True,
		If[Head[tempOptStringI]===String,
			tempOptStringI=StringCases[tempOptStringI,"-"~~Shortest[__]~~tempns:NumberString..~~"m"->tempns];
			If[Length[tempOptStringI]>=1,
				tempOptStringI=ToExpression[tempOptStringI[[1]]];
				If[tempOptStringI<stackSize,tempReI=True]
			];
		]];

    	(*Extract the stack settings for ReinstallJava*)
		tempOptStringR=OptionValue[JLink`ReinstallJava, JLink`JVMArguments];
		If[tempOptStringR===None, tempReR=True,
		If[Head[tempOptStringR]===String,
			tempOptStringR=StringCases[tempOptStringR,"-"~~Shortest[__]~~tempns:NumberString..~~"m"->tempns];
			If[Length[tempOptStringR]>=1,
				tempOptStringR=ToExpression[tempOptStringR[[1]]];
				If[tempOptStringR<stackSize,tempReR=True]
			];
		]];

		(*Change and ReinstallJava as necessary*)
		If[tempReI,
			SetOptions[JLink`InstallJava, JLink`JVMArguments -> "-Xmx"<>ToString[stackSize]<>"m"]
		];
		If[tempReR,
			SetOptions[JLink`ReinstallJava, JLink`JVMArguments -> "-Xmx"<>ToString[stackSize]<>"m"]
		];
		If[tempReI || tempReR,
			JLink`ReinstallJava[];
			Print["<<Set JLink` java stack size to "<>ToString[stackSize]<>"Mb>>"];
		];

		NotebookDelete[tempPrint];
	]; (*Module for HHIncreaseJavaStack*)

HHIncreaseJavaStack[args___]:=Message[HHIncreaseJavaStack::invalidArgs,{args}];


(* ::Subsection::Closed:: *)
(* Package Git functions *)


(* ::Subsubsection::Closed:: *)
(* HHPackageNewestFileDate *)


HHPackageNewestFileDate[package_String]:=
Module[{packageFile,tempdir},
	
	packageFile = FindFile[package];
	
	tempdir = If[packageFile =!= $Failed,
		FileNames[ "*",
			ParentDirectory[DirectoryName[ packageFile ]],
			Infinity
		],
		{}
	];
	
	If[ Length[tempdir]==0,
		Message[HHPackageNewestFileDate::noFilesFound, package]; "",
		DateString[Max @@ AbsoluteTime /@ FileDate /@ tempdir ]
	]
	
];
HHPackageNewestFileDate[args___]:=Message[HHPackageNewestFileDate::invalidArgs,{args}];
HHPackageNewestFileDate::noFilesFound = "No files were found for package:  `1`.";



(* ::Subsubsection::Closed:: *)
(* HHPackageGitImpl (private) *)


HHPackageGitImpl[directory_String, command_String]:=
Module[{errorcode,tempret},
	SetDirectory[ directory ];
	errorcode = Run[command <> " > HHPackageGitImplTemp.txt"];
	If[ errorcode == 0,
		tempret = Import["HHPackageGitImplTemp.txt"];
		DeleteFile["HHPackageGitImplTemp.txt"],
		Message[HHPackageGitImpl::gitError, command, directory];
		tempret = $Failed; 		
	];
	ResetDirectory[];
	tempret
];
HHPackageGitImpl::gitError="Call to Git returned error. It could be that Git is " <>
 "not installed correctly, the command `1` is not valid, or the directory `2` is not valid."; 


(* ::Subsubsection::Closed:: *)
(* HHPackageGitRemotes *)


HHPackageGitRemotes[package_String]:= Module[{tempFile},
	tempFile=FindFile[package];
	If[ tempFile === $Failed,
		Message[HHPackageGitRemotes::noFilesFound, package];
		$Failed,
		HHPackageGitImpl[ ParentDirectory[DirectoryName[ tempFile ]], "git remote -v" ]
	]
];
HHPackageGitRemotes[]:= HHPackageGitImpl[ NotebookDirectory[], "git remote -v" ];
HHPackageGitRemotes[args___]:=Message[HHPackageGitRemotes::invalidArgs,{args}];
HHPackageGitRemotes::noFilesFound = HHPackageNewestFileDate::noFilesFound;



(* ::Subsubsection::Closed:: *)
(* HHPackageGitHEAD *)


HHPackageUpdateGitHEADFile[notebookDirectory_String]:= 
Module[{hhPrePackageGitHead, hhPrePackageGitHeadDate},
	Print[hhPrePackageGitHead = HHPackageGitHEAD[]];
	Print[hhPrePackageGitHeadDate = DateString[]];
	Export[notebookDirectory <> "GitHEAD.m", {hhPrePackageGitHead, 
  		hhPrePackageGitHeadDate}]
];
HHPackageUpdateGitHEADFile[args___]:=Message[HHPackageUpdateGitHEADFile::invalidArgs,{args}];


HHPackageGitHEAD[]:= HHPackageGitImpl[ NotebookDirectory[], "git rev-parse HEAD" ];
HHPackageGitHEAD[package_String]:= Module[{tempFile},
	tempFile=FindFile[package];
	If[ tempFile === $Failed,
		Message[HHPackageGitHEAD::noFilesFound, package];
		" ",
		HHPackageGitImpl[ ParentDirectory[DirectoryName[ tempFile ]], "git rev-parse HEAD" ]
	]
];
HHPackageGitHEAD[]:= HHPackageGitImpl[ NotebookDirectory[], "git rev-parse HEAD" ];


HHPackageGitHEAD[args___]:=Message[HHPackageGitHEAD::invalidArgs,{args}];
HHPackageGitHEAD::noFilesFound = HHPackageNewestFileDate::noFilesFound;



(* ::Subsubsection::Closed:: *)
(* HHPackageMessage *)


HHPackageMessage[package_String]:=
Module[{remotes, head, newest, tempFile, tempPreFile, searchFile},
	remotes=Quiet[HHPackageGitRemotes[package]];
	head=   Quiet[HHPackageGitHEAD[package]];
	newest= Quiet[HHPackageNewestFileDate[package]];

	If[ remotes === $Failed || head === $Failed || newest === $Failed,
		
		(*Problem with Git repository, search for GitHEAD.m and print*)
		tempFile=FindFile[package];
		If[ tempFile === $Failed,
			Message[HHPackageMessage::noFilesFound, package],
			tempPreFile = Import[ ParentDirectory[DirectoryName[tempFile]] <> "\\GitHEAD.m" ];
			If[ tempPreFile === $Failed,
				Message[HHPackageMessage::noPreFileFound, package],
				CellPrint[TextCell[Row[{
					Style[package, 
						FontFamily -> "Helvetica", FontWeight -> "Bold", 
						FontVariations -> {"Underline" -> True}
						], 
					"\n" ,
					Style["Git HEAD hash loaded on " <> tempPreFile[[2]] <> " is " <>
							tempPreFile[[1]] <> ".\nRemember that this is at latest the " <>
							"penultimate commit before deployment. You should use a live git repo if possible, "<>
							"for better version tracking." ,Small, FontFamily->"Courier"]
				}],"Text", Background -> LightGray]]
			];
		],	

		(*Print info extracted from Git repo*)
		CellPrint[TextCell[Row[{
			Style[package, 
				FontFamily -> "Helvetica", FontWeight -> "Bold", 
				FontVariations -> {"Underline" -> True}
				], 
			"\n" ,
			Style[StringJoin@@Riffle[
					"("<> #[[1]]<>")[" <> #[[2]] <>"]"& /@
					Union[ImportString[remotes][[All, 1;;2]]]
				,"\n"],
				Small, FontFamily->"Courier"
				], 
			"\n",
			Style[
				"current Git HEAD:  "<> head <>"\n" <>
				"newest file:  "<> newest <>" ", 
				Small, FontFamily->"Courier"]
			}],"Text", Background -> LightGray]]
	];
];
HHPackageMessage[]:=HHPackageMessage[ NotebookDirectory[] ];


HHPackageMessage::gitError=HHPackageGitImpl::gitError;
HHPackageMessage::noFilesFound = HHPackageNewestFileDate::noFilesFound;
HHPackageMessage::noPreFileFound = "Pre-generated GitHEAD.m file was not found, package may be corrupt.";



(* ::Subsection:: *)
(*Utilities*)


HHPadZeros[n_]:=HHPadZeros[n,3];
HHPadZeros[n_,m_Integer]:=
	If[n < 10^m,
		Apply[StringJoin,Map[ToString,IntegerDigits[n, 10, m] ]],
		n
	];

HHPadZeros[args___]:=Message[HHPadZeros::invalidArgs,{args}];


HHPrintAssignmentCell[symbolNames:{_String..}, opts:OptionsPattern[] ]:=
Module[{names, expressionStrings, symbols, validNameValues},
	names=Flatten[Names/@symbolNames];
	symbols= Symbol[#]& /@ names;
	(* //ToDo3: The following does not work, due to evaluation order issues *)
	(*validNameValues=Select[ Transpose[{names, symbols}], ValueQ[#[[2]]]& ];*)
	validNameValues=Transpose[{names, symbols}];

	If[Length[validNameValues]>=1,
		expressionStrings="";
		expressionStrings=expressionStrings <> If[OptionValue["PrintFilename"], "(* "<> NotebookFileName[] <>" *)\n", ""];
		expressionStrings=expressionStrings <> If[OptionValue["PrintDateString"], "(* "<> DateString[] <>" *)\n", ""];
		expressionStrings=If[StringLength[expressionStrings]>=1, expressionStrings<>"\n", expressionStrings];  
		expressionStrings=expressionStrings <> ( ( #[[1]]<>" = "<> ToString[InputForm[#[[2]]]] <>";\n" )& /@ validNameValues );
		CellPrint[Cell[  
			StringTake[(StringJoin @@ expressionStrings), {1, -2}],
			"Input", Background->LightBlue(*, InitializationCell\[Rule]True*)
		]];,
		Message[HHPrintAssignmentCell::noValidName, symbolNames]
	]
];
HHPrintAssignmentCell[symbolName_String, opts:OptionsPattern[]]:= HHPrintAssignmentCell[{symbolName},opts];
HHPrintAssignmentCell[symbolNames__String, opts:OptionsPattern[]]:= HHPrintAssignmentCell[{symbolNames}, opts];

HHPrintAssignmentCell[args___]:=Message[HHPrintAssignmentCell::invalidArgs,{args}];
HHPrintAssignmentCell::noValidName="No active symbols with value assignments match the symbol name(s) `1`.";


HHSymbolNotNull[symbolName_String]:= (Symbol[symbolName]!=Null);
HHSymbolNotNull[symbolName_List]:= And@@(HHSymbolNotNull /@ symbolName);

HHSymbolNotNull[args___]:=Message[HHSymbolNotNull::invalidArgs,{args}];


HHCreateDirectoryIfNone[directoryName_String]:= If[ !FileExistsQ[directoryName], CreateDirectory[directoryName], Null ];
HHCreateDirectoryIfNone[args___]:=Message[HHCreateDirectoryIfNone::invalidArgs,{args}];


(* ::Section:: *)
(*End*)


End[];

EndPackage[];


HHPackageMessage["HokahokaW`"];
