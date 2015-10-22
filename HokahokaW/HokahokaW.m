(* ::Package:: *)

(* Wolfram Language package *)


BeginPackage["HokahokaW`",{"JLink`"}];


General::invalidArgs="Function called with invalid arguments `1`.";
General::invalidOptionValue="Option argument `1` -> `2` is invalid.";
General::deprecated="Function is deprecated, use `1` instead.";
General::nullArgument="At least one of the required arguments is null!";


(* ::Subsection:: *)
(*Package-wide option keys*)


(* ::Subsection::Closed:: *)
(*Rules/Options*)


HHRuleListQ::usage=
	"HHRuleListQ[ruleList_List]... returns whether the argument ruleList is a list of rules.";
HHRuleQ::usage=
	"HHRuleQ[rule_]... returns whether the argument ruleList is a list of Rule or RuleDelayed objects.";


HHJoinOptionLists::usage=
"HHJoinOptionLists[x_/;RuleQ[x], y_/;RuleQ[y]]...   joins two option lists, "<>
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


(* ::Subsection::Closed:: *)
(*HHFunctionQ*)


HHFunctionQ::usage="Returns whether a given symbol is a pure function or a function rule.";


(* ::Subsection::Closed:: *)
(*Java*)


HHJavaObjectQ::usage="Checks whether something is a Java object and an instance of the given class/interface.";


HHIncreaseJavaStack::usage="Increases the Java stack size.";


(* ::Subsection:: *)
(*Package Git functions *)


HHPackageGitLoad::usage="Loads git repository into jgit. Specify directory or package name. If not specified, NotebookDirectory[] will be taken";
HHPackageGitUnload::usage="Unloads current git repository and resets search string, if they are set.";
HHPackageGitFindRepoDir::usage="Searches up directory tree to try to find a .git repo directory.";
HHPackageGitCurrentBranch::usage="Returns the current branch name for the given package or within the current NotebookDirectory[].";


HHPackageNewestFileDate::usage="Prints the newest file change date for all files within the given package or within the current NotebookDirectory[].";
HHPackageGitRemotes::usage="Returns a list of git remotes for either the given package or the current NotebookDirectory[].";
HHPackageGitRemotesURL::usage="Returns a list of git remote URLs for either the given package or the current NotebookDirectory[].";

HHPackageGitHEAD::usage="Returns the git HEAD hash for either the given directory or the current NotebookDirectory[].";
(*HHPackageUpdateGitHEADFile::usage="Bundles a GitHEAD.m file for marking deployments without active Git management.";*)
HHPackageMessage::usage="Prints standard package message.";
(* TODO: test that notebook message is working with HHPackageMessage[] *)
(*HHNotebookMessage::usage="Prints standard notebook message.";*)


(* ::Subsection::Closed:: *)
(*HHNextPower*)


HHNextPower::usage=" ";


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Rules/Options*)


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


(* ::Subsection::Closed:: *)
(*HHFunctionQ*)


(*Tests whether the symbol is a function or not*)
(*FunctionQ[x_String]:=FunctionQ[ToExpression[x]];*)
HHFunctionQ[x_Function]:=True;
HHFunctionQ[x_Symbol]:=MemberQ[Attributes[x], NumericFunction] (*&& (Length[Flatten[#[x]&/@{DownValues,UpValues}]]>0)*);
(*HHFunctionQ[x_Symbol/;(MemberQ[Attributes[x],NumericFunction] || NumericQ[x[1]])]:=True;*)
(*HHFunctionQ[x_Symbol, sampleArgs_]:=HHFunctionQ[x, sampleArgs, NumericQ];
HHFunctionQ[x_Symbol, sampleArgs_, questionFunc_]:=Quiet[Check[questionFunc[  x[ sampleArgs ]], False]];*)
HHFunctionQ[_]:=False;


HHFunctionQ[args___]:=Message[HHFunctionQ::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
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


(* ::Subsection:: *)
(*Package Git functions *)


$HHCurrentGitRepositoryPath::usage="";
$HHCurrentGitRepositoryPath = "";

$HHCurrentGitRepository::usage="";
$HHCurrentGitRepository = Null;


(* ::Subsubsection:: *)
(* HHPackageGitFindRepoDir*)


HHPackageGitFindRepoDir[directory_String]:=
(*HHPackageGitFindRepoDir[directory]=*)
Module[{tempret, temp},
	tempret = 
	If[ DirectoryQ[directory],
		HHPackageGitFindRepoDirImpl[directory],
		temp = FileNames[directory];
		If[ Length[temp]>0, 
			HHPackageGitFindRepoDirImpl[ DirectoryName[ temp[[1]] ] ],
			temp = FindFile[directory];
			If[ temp === $Failed,
				"",
				HHPackageGitFindRepoDirImpl[ DirectoryName[ temp ] ]
			]
		]
	];
	If[tempret === "", Message[ HHPackageGitFindRepoDir::notGitDirectory, directory ]];
	tempret 		
]; 


HHPackageGitFindRepoDirImpl[directory_String]:=
Module[{parentDirectory, putativeGitDirectory},
	parentDirectory=Quiet[Check[ParentDirectory[directory], ""]]; (*If there is no parent directory, etc.*)
	If[  parentDirectory === "" || parentDirectory == directory,  (*If in root directory, ParentDirectory[] will act as Identity[] *)
		"",
		(*See if there is a ".git" folder in the parent directory, and if not, recurse up tree*)
		putativeGitDirectory=FileNameJoin[{ parentDirectory, ".git"}]; 
		If[ DirectoryQ[ putativeGitDirectory ], putativeGitDirectory, HHPackageGitFindRepoDirImpl[parentDirectory] ]
	]
]; 


HHPackageGitFindRepoDir::notGitDirectory="No git directory \".git\" was found within the parent tree of `1`."; 

HHPackageGitFindRepoDir[args___]:=Message[HHPackageGitFindRepoDir::invalidArgs,{args}];


(* ::Subsubsection:: *)
(* HHPackageGitLoad/Unload*)


(*HHPackageGitLoad[]:= HHPackageGitLoad[ NotebookDirectory[] ];*)
HHPackageGitLoad[directory_String]:=
Module[{gitDirectory, temp},
	gitDirectory = HHPackageGitFindRepoDir[directory];
	If[ gitDirectory === "",
		HHPackageGitUnload[],
		If[ gitDirectory =!= $HHCurrentGitRepositoryPath,
			HHPackageGitUnload[];
			$HHCurrentGitRepositoryPath = gitDirectory;
			(*Print[{gitDirectory,gitDirectory =!= $HHCurrentGitRepositoryPath}];*)
			$HHCurrentGitRepository = 
				JavaNew["org.eclipse.jgit.internal.storage.file.FileRepository", gitDirectory];
			Print["HokahokaW`HHPackageGitLoad: Loaded Git repository located at " <> gitDirectory ]
		]
	]
(*		gitDirectory = HHPackageGitLoadImpl[ directory ];
(*Print[ToString[{gitDirectory, gitDirectory\[Equal]0, gitDirectory===0}]];*)
		If[gitDirectory == 0,
	(*Print["Same"];*)
			Message[HHPackageGitLoad::notGitDirectory, directory];
			HHPackageGitUnload[],

	(*Print["Different " <> ToString[gitDirectory]];*)
			$HHCurrentGitRepositorySearchString = directory;
			$HHCurrentGitRepository = JavaNew["org.eclipse.jgit.internal.storage.file.FileRepository", gitDirectory];
			Print["Loaded Git repository located at " <> gitDirectory ]
		];
	]*)
];

HHPackageGitLoad::notGitDirectory="No git directory \".git\" was found within the parent tree of `1`."; 
HHPackageGitLoad::gitError="Call to Git returned error. It could be that Git is " <>
 "not installed correctly, the command `1` is not valid, or the directory `2` is not valid."; 

HHPackageGitLoad[args___]:=Message[HHPackageGitLoad::invalidArgs,{args}];


(*HHPackageGitLoadImpl[directory_String]:=
Module[{gitDirectory, temp},
	
	gitDirectory = 0;
	If[ temp=FileNameJoin[{ directory, ".git"}]; DirectoryQ[temp], gitDirectory = temp ]; (*If the Git root is specified*)
(*Print[gitDirectory];*)
	If[ gitDirectory \[Equal] 0, gitDirectory = HHPackageGitFindRepoDir[directory] ];
(*Print[gitDirectory];*)
	If[ gitDirectory \[Equal] 0, 
		temp=FindFile[directory];
		If[ temp =!= $Failed, gitDirectory = HHPackageGitFindRepoDir[DirectoryName[ temp ]] ]
	];
(*Print[gitDirectory];*)
	If[ gitDirectory \[Equal] 0, gitDirectory = Quiet[Check[ DirectoryName[directory], 0]]  ];
	
	gitDirectory
];*)


(*HHPackageGitLoaded[]:= ($HHCurrentGitRepositorySearchString === "" || $HHCurrentGitRepository === Null);

HHPackageGitLoaded[args___]:=Message[HHPackageGitLoaded::invalidArgs,{args}];*)


HHPackageGitUnload[]:=
Module[{},
	If[ $HHCurrentGitRepositoryPath =!= "",
		Print[ "Unloading repository: "<> $HHCurrentGitRepositoryPath];
		$HHCurrentGitRepositoryPath = "";
		$HHCurrentGitRepository = Null
	];
];

HHPackageGitUnload[args___]:=Message[HHPackageGitUnload::invalidArgs,{args}];


(* ::Subsubsection:: *)
(* HHPackageGitCurrentBranch*)


HHPackageGitCurrentBranch[]:= HHPackageGitCurrentBranch[NotebookFileName[]];
HHPackageGitCurrentBranch[package_String]:= 
Module[{currBranch, currRef, currObjID},
	HHPackageGitLoad[package];
	If[ $HHCurrentGitRepository =!= Null, 
		$HHCurrentGitRepository@getBranch[], 
		"NO VALID REPOSITORY"
	]
];


HHPackageGitCurrentBranch[args___]:=Message[HHPackageGitCurrentBranch::invalidArgs,{args}];


(* ::Subsubsection:: *)
(* HHPackageGitHEAD *)


HHPackageGitHEAD[]:= HHPackageGitHEAD[NotebookFileName[]];
HHPackageGitHEAD[package_String]:= 
Module[{currBranch, currRef, currObjID},
	HHPackageGitLoad[package];
	If[ $HHCurrentGitRepository =!= Null,
		currRef=$HHCurrentGitRepository@getRef[ HHPackageGitCurrentBranch[package] ];
		currObjID=currRef@getObjectId[];
		currObjID@toString[ currObjID ],
		"NO VALID REPOSITORY"
	]
];


HHPackageGitHEAD[args___]:=Message[HHPackageGitHEAD::invalidArgs,{args}];


(* ::Subsubsection:: *)
(* HHPackageGitRemotes / HHPackageGitRemotesURL*)


HHPackageGitRemotes[]:= HHPackageGitRemotes[NotebookFileName[]];
HHPackageGitRemotes[package_String]:= 
Module[{currConfig},
	HHPackageGitLoad[package];
	If[ $HHCurrentGitRepository =!= Null,
		currConfig=$HHCurrentGitRepository@getConfig[];
		currConfig@getSubsections["remote"]@toArray[],
		"NO VALID REPOSITORY"
	]
];


HHPackageGitRemotes[args___]:=Message[HHPackageGitRemotes::invalidArgs,{args}];


HHPackageGitRemotesURL[]:= HHPackageGitRemotesURL[NotebookFileName[]];
HHPackageGitRemotesURL[package_String]:= 
Module[{remotes, currConfig},
	HHPackageGitLoad[package];
	If[ $HHCurrentGitRepository =!= Null,
		remotes = HHPackageGitRemotes[package];
		currConfig=$HHCurrentGitRepository@getConfig[];
		currConfig@getString["remote", #, "url"]& /@ remotes,
		"NO VALID REPOSITORY"
	]
];


HHPackageGitRemotesURL[args___]:=Message[HHPackageGitRemotesURL::invalidArgs,{args}];


(* ::Subsubsection:: *)
(* HHPackageMessage *)


HHPackageMessage[]:=HHPackageMessage[NotebookFileName[]];
HHPackageMessage[package_String]:=HHPackageMessage[package, ""];

HHPackageMessage[package_String, append_String]:=
Module[{remotes},

	HHPackageGitLoad[package];
	If[$HHCurrentGitRepository === Null,
		HHPackageMessageImpl[ package,
				"This package/notebook does not seem to be tracked by Git: no .git  directory found",
				"You should always track items with Git or another VCS for reproducibility!"
		],
		
		remotes = Transpose[ {HHPackageGitRemotes[package], HHPackageGitRemotesURL[package]} ];
		HHPackageMessageImpl[ package,
				"Current local repository path:   " <> $HHCurrentGitRepositoryPath <> "\n" <>
				"Current branch [hash]:  "<> HHPackageGitCurrentBranch[package] <> " [" <> HHPackageGitHEAD[package] <>"]\n" <>
				StringTake[ StringJoin[ ("Remote:  " <> #[[1]] <> " (" <> #[[2]]<>")\n")& /@ remotes ], {1, -2}],
				append
		]
	];
];


HHPackageMessageImpl[package_String, contentLines_String, appendLines_String]:=
Module[{},
	CellPrint[TextCell[Row[{
		Style[package, FontFamily -> "Helvetica", FontWeight -> "Bold", FontVariations -> {"Underline" -> True}],
		"\n" ,
		Style[ DateString[]<>"     [Mathematica: "<> ToString[$Version]<>"]", Smaller,FontFamily -> "Helvetica" ],
		"\n" ,
		Sequence@@If[ appendLines === "", 
			Flatten[{ Style["     "<>#, Smaller,FontFamily -> "Helvetica"], "\n" }& /@ StringSplit[contentLines,"\n"]][[ ;; -2]],
			Flatten[{ Style["     "<>#, Smaller,FontFamily -> "Helvetica"], "\n" }& /@ StringSplit[contentLines,"\n"]]
		],
		Sequence@@If[ appendLines === "",
			{},
(*Print[appendLines];
*)			Flatten[{ Style["     "<>#, Smaller,FontFamily -> "Helvetica"], "\n" }& /@ StringSplit[appendLines,"\n"]][[ ;; -2]]
		]		
		}],"Text", Background -> LightGray]
	]
];


HHPackageMessage[args___]:=Message[HHPackageMessage::invalidArgs,{args}];


(* ::Subsubsection::Closed:: *)
(* DEPRECATED HHPackageNewestFileDate *)


HHPackageNewestFileDate[package_String]:=
Module[{packageFile,tempdir},
	
	Message[HHPackageNewestFileDate::deprecated, "(don't use, not necessarily informative)"];

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


(* ::Subsubsection:: *)
(*BAK old implementations for HHPackageGitXXX*)


(*HHPackageGitImpl[directory_String, command_String]:=
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
 "not installed correctly, the command `1` is not valid, or the directory `2` is not valid."; *)


(*HHPackageGitRemotes[package_String]:= Module[{tempFile},
	tempFile=FindFile[package];
	If[ tempFile === $Failed,
		Message[HHPackageGitRemotes::noFilesFound, package];
		$Failed,
		HHPackageGitImpl[ ParentDirectory[DirectoryName[ tempFile ]], "git remote -v" ]
	]
];
HHPackageGitRemotes[]:= HHPackageGitImpl[ NotebookDirectory[], "git remote -v" ];
HHPackageGitRemotes[args___]:=Message[HHPackageGitRemotes::invalidArgs,{args}];
HHPackageGitRemotes::noFilesFound = HHPackageNewestFileDate::noFilesFound;*)


(*HHPackageUpdateGitHEADFile[notebookDirectory_String]:= 
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
*)


(* ::Subsubsection:: *)
(*BAK old implementation for HHPackageMessage *)


(*HHPackageMessage[package_String]:=
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
*)


(* ::Subsection::Closed:: *)
(*HHNextPower*)


HHNextPower[base_, n_]:= Ceiling[Log[base, n]];


HHNextPower[args___]:=Message[HHNextPower::invalidArgs,{args}];


(* ::Subsection::Closed:: *)
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
