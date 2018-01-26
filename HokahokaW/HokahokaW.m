(* ::Package:: *)

(* Wolfram Language package *)


BeginPackage["HokahokaW`",{"JLink`"}];


(* ::Section:: *)
(*Declarations*)


General::invalidArgs="Function called with invalid arguments `1`.";
General::invalidOptionValue="Option argument `1` -> `2` is invalid.";
General::deprecated="Function is deprecated, use `1` instead.";
General::deprecatedSignature="Function signature is deprecated, see help for updated signature which should be used.";
General::nullArgument="At least one of the required arguments is null!";


HHVerbose::useage = "An option for several HokahokaW` functions whether audit output should be printed.";


$HHPackageMessageRun::usage="List of packages run, to only run selected package message once.";
$HHPackageMessageRun= {};


(* ::Subsection::Closed:: *)
(*Rule/Option Handling*)


HHRuleListQ::usage=
"returns whether the argument is a list or sequence of Rule or RuleDelayed objects.";
HHRuleQ::usage=
"returns whether the argument is a Rule or RuleDelayed object.";


HHJoinOptionLists::usage=
"joins two ore more option lists, \
and if the first argument is a symbol, filters the joined option list for valid options for that symbol.";


HHAddOptions::usage=
"returns the original object (e.g. NNMData[<<>>, opts]), "<>
"but with the specified option(s) appended or replaced. opts can be specified either as a Sequence "<>
"or a List of rules.";


HHOptionValue::usage= "extract options from an object, such as a Graphic[..., opt->optval], \
and give the default Option value if not specified.";
HHAbsoluteOptionValue::usage= "DEPRECATED: use AbsoluteOptions instead.";


(*HHExtractRules[x_[arg___]]:=Flatten[If[HHRuleQ[#],#,{}]& /@ {arg}];
HHExtractRules[args___]:=Message[HHExtractRules::invalidArgs,{args}];*)


(* ::Subsection::Closed:: *)
(*HHFunctionQ*)


HHFunctionQ::usage="Returns whether a given symbol is a pure function or a function rule.";


(* ::Subsection::Closed:: *)
(*Java*)


HHJavaObjectQ::usage="Checks whether something is a Java object and an instance of the given class/interface.";


HHIncreaseJavaStack::usage="Increases the Java stack size.";


(* ::Subsection::Closed:: *)
(*HHPackageMessage/Package Git functions *)


HHPackageMessage::usage="Prints standard package message, including Git information if available.";
HHPackageWriteGitArtifact::usage="Saves git info if available in artifact file, and returns association.";

(*HHPackageGitArtifact::usage="Checks if a package git artifact is present. Also checks whether currently in a git repository, \
and updates the artifact if yes.";
Options[HHPackageGitArtifact]={HHVerbose -> True};*)


HHPackageGitLoad::usage="Loads git repository into jgit. Specify directory or package name. If not specified, NotebookDirectory[] will be taken";
HHPackageGitUnload::usage="Unloads current git repository and resets search string, if they are set.";
HHPackageGitFindRepoDir::usage="Searches up directory tree to try to find a .git repo directory.";
HHPackageGitCurrentBranch::usage="Returns the current branch name for the given package or within the current NotebookDirectory[].";
Options[HHPackageGitLoad]= Options[HHPackageGitUnload]={HHVerbose -> False};


HHPackageNewestFileDate::usage="Prints the newest file change date for all files within the given package or within the current NotebookDirectory[].";
HHPackageGitRemotes::usage="Returns a list of git remotes for either the given package or the current NotebookDirectory[].";
HHPackageGitRemotesURL::usage="Returns a list of git remote URLs for either the given package or the current NotebookDirectory[].";

HHPackageGitHEAD::usage="Returns the git HEAD hash for either the given directory or the current NotebookDirectory[].";
(*HHPackageUpdateGitHEADFile::usage="Bundles a GitHEAD.m file for marking deployments without active Git management.";*)


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


(* ::Subsection:: *)
(*Utilities: File*)


HHCreateDirectoryIfNone::usage = 
"Checks whether the given directory name exists, and if not, calls CreateDirectory.";


HHExtractArchive::usage = 
"Attempts to use 7-zip program to extract a compressed archive (currently windows only). Files already present in target directory will be overwritten.";

Options[HHExtractArchive]={
	"7ZipPath"-> Automatic
};


HHPrependFilename::usage = 
"Parses a file path and prepends a string to the filename within the path.";
HHAppendFileBaseName::usage = 
"Parses a file path and append a string to the FileBaseName only. Does not add periods.";


HHJitterize::usage = 
"Jitters a list of elements with the given jitter factors.";


(* ::Subsection:: *)
(*Utilities: List manipulation*)


HHTakeCyclical::usage ="Same as Take, but uses Mod in order to take cyclically, i.e. \
if specification is beyond range of list to take from, samples cyclically wrapping to beginning.";


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Rules/Options*)


(* ::Subsubsection::Closed:: *)
(*HHRuleQ/HHRuleListQ*)


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
HHRuleQ[___] := False;


(* ::Subsubsection::Closed:: *)
(*HHJoinOptionLists*)


HHJoinOptionLists[x_/;HHRuleListQ[x], y_/;HHRuleListQ[y]]:=
	Block[{tempretNN},
		tempretNN=Join[Flatten[x], FilterRules[Flatten[y], Except[x]]];
		Return[tempretNN]
	];

HHJoinOptionLists[x_/;HHRuleListQ[x], y__/;(And@@(HHRuleListQ /@ {y}))]:=
	Block[{tempretNN},
		tempretNN=x;
		Do[tempretNN=HHJoinOptionLists[tempretNN, zz],{zz,{y}}];
		Return[tempretNN]
	];

HHJoinOptionLists[symbol_Symbol, x_/;HHRuleListQ[x]]:=
	Block[{},
		Return[FilterRules[x, Options[symbol]]]
	];

HHJoinOptionLists[symbol_Symbol, x_/;HHRuleListQ[x], y__/;(And@@(HHRuleListQ /@ {y}))]:=
	Block[{tempretNN},
		tempretNN = HHJoinOptionLists[x, y];
		Return[FilterRules[tempretNN, Options[symbol]]]
	];

HHJoinOptionLists[symbol_[contents_], x_/;HHRuleListQ[x], y__/;(And@@(HHRuleListQ /@ {y}))]:=
	Block[{tempretNN},
		tempretNN = HHJoinOptionLists[x, y];
		Return[FilterRules[tempretNN, Options[symbol]]]
	];

HHJoinOptionLists[args___]:=Message[HHJoinOptionLists::invalidArgs,{args}];


(* ::Subsubsection::Closed:: *)
(*HHAddOptions*)


HHAddOptions[symbol_[contents___], opts___]:=HHAddOptions[symbol[contents], {opts}];
HHAddOptions[symbol_[contents___], {opts___}]:=
	Block[{tempretNN(*,oldRules*)},
		(*The following will strip off rules from the end.*)
		tempretNN = Select[{contents}, !HHRuleQ[#]&];
		(*Append old rules which are not given in opts.*)

		tempretNN = Append[tempretNN, 
			Hold[Sequence@@HHJoinOptionLists[symbol, {opts}, Select[{contents}, HHRuleQ]]] ];
		Return[ReleaseHold[ symbol[Sequence@@tempretNN] ]]
		(*oldRules=FilterRules[Options[x], Except[opts]];
			If[Length[oldRules]>0, tempretNN= Append[tempretNN, Hold[Sequence@@oldRules]]];
		(*Append new rules given in opts*)
		tempretNN= Append[tempretNN, Hold[opts]];
		Return[ReleaseHold[tempretNN]]*)
	];

HHAddOptions[args___]:=Message[HHAddOptions::invalidArgs,{args}];


(* ::Subsubsection::Closed:: *)
(*HHOptionValue*)


HHOptionValue[x_, optionSymbol_]:=Block[{tempOpts},
	tempOpts=Join[Options[x],Options[Head[x]]];
	OptionValue[ tempOpts ,optionSymbol ]
];
HHOptionValue[args___]:=Message[HHOptionValue::invalidArgs,{args}];


HHAbsoluteOptionValue[___]:=Message[HHOptionValue::deprecated,"AbsoluteOptions"];


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
	Block[{tempOptStringI,tempOptStringR,tempReI=False, tempReR=False, 
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
	]; (*Block for HHIncreaseJavaStack*)

HHIncreaseJavaStack[args___]:=Message[HHIncreaseJavaStack::invalidArgs,{args}];


(* ::Subsection::Closed:: *)
(*HHPackageMessage/Package Git functions*)


$HHCurrentGitRepositoryPath::usage="";
$HHCurrentGitRepositoryPath = "";


$HHCurrentGitRepository::usage="";
$HHCurrentGitRepository = Null;


$HHCurrentGitArtifact::usage="";
$HHCurrentGitArtifact = Null;


(* ::Subsubsection:: *)
(* HHPackageMessage *)


HHPackageMessage[]:=HHPackageMessage[NotebookFileName[]];
HHPackageMessage[package_String]:=HHPackageMessage[package, ""];


HHPackageMessage[package_String, append_String]:=
Block[{tempArtifactFile, 
		temp, tempInfo, tempStrippedPackage,
		infoPath, infoBranch, infoHEAD, infoRemotes, infoArtifactDate},
If[ !MemberQ[$HHPackageMessageRun, package],
	Quiet[ HHPackageGitLoad[package] ];

	tempStrippedPackage = If[StringMatchQ[package,"*`"], StringTake[package, {1,-2}], package];

	(*Where the artifact file will be read/written*)
	tempArtifactFile=
	If[ Head[$HHCurrentGitRepository]=!= String || !DirectoryQ[$HHCurrentGitRepository],
		(*If no repository was loaded*)
		If[  FileExistsQ[ package ], 
			(*For example, if notebook file is specified*)
			FileNameJoin[{DirectoryName[package], "HHGitArtifact.m"}],

			(*If not, "package" is probably package name*)
			temp = FindFile[ tempStrippedPackage <>  "`"];
			If[  temp === $Failed, 
				Null (*Do not write artifact*), 
				FileNameJoin[{ ParentDirectory[DirectoryName[ temp ]], "HHGitArtifact.m"}]
			]
		 ],

		(*If repository was loaded*)
		FileNameJoin[{ParentDirectory[$HHCurrentGitRepositoryPath], tempStrippedPackage, "HHGitArtifact.m"}]
	];

	If[Head[$HHCurrentGitRepository]=!= String || !DirectoryQ[$HHCurrentGitRepository],
		(*If we are not working in an active git repository, try to load artifact file *)
		If[ FileExistsQ[ tempArtifactFile ],
			tempInfo = Import[ tempArtifactFile ],
			tempInfo = Null
		],

		(*If we are working in an active git repository, load/refresh info and write *)
		tempInfo = HHPackageWriteGitArtifact[package, tempArtifactFile]
	];

	(*Printout*)
	If[ tempInfo === Null,	
		HHPackageMessageImpl[ package,
			"No .git  directory found and no HHGitArtifact.m found for this package/notebook.",
			"You should always track items with Git or another VCS for reproducibility!"
		],
		
		HHPackageMessageImpl[ package,
			If[ Head[$HHCurrentGitRepository]=!= String || !DirectoryQ[$HHCurrentGitRepository],
				 "Artifact info as of: ", "Git info loaded: "] <> 
				tempInfo[["GitArtifactDate"]] <> "\n" <>
				"Local repo path:   " <> tempInfo[["GitPath"]] <> "\n" <>
				"Current branch [hash]:  "<> tempInfo[["GitBranch"]] <> " [" <> tempInfo[["GitHEAD"]] <>"]\n" <>
				StringTake[ StringJoin[ ("Remote:  " <> #[[1]] <> " (" <> #[[2]]<>")\n")& /@ tempInfo[["GitRemotes"]] ], {1, -2}],
				append
		]
	];
	AppendTo[$HHPackageMessageRun, package]
]
];


(*HHPackageMessage[]:=HHPackageMessage[NotebookFileName[]];
HHPackageMessage[package_String]:=HHPackageMessage[package, ""];

HHPackageMessage[package_String, append_String]:=
Block[{remotes},

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
];*)


HHPackageMessageImpl[package_String, contentLines_String, appendLines_String]:=
Block[{},
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


(* ::Subsubsection:: *)
(* HHPackageWriteGitArtifact *)


HHPackageWriteGitArtifact[package_String, artifactFile_String: Automatic]:=

Block[{tempInfo,tempArtifactFile,	infoPath, infoBranch, infoHEAD, infoRemotes, infoArtifactDate},

	Quiet[ HHPackageGitLoad[package] ];

	tempArtifactFile = If[ artifactFile === Automatic,
		FileNameJoin[{NotebookDirectory[], "HHGitArtifact.m"}],
		artifactFile
	];

	If[$HHCurrentGitRepository === Null,
	
		Message[HHPackageWriteGitArtifact::noRepo, package];
		<| |>,

		(*If we are working in an active git repository, load info and write *)
		infoPath = $HHCurrentGitRepositoryPath;
		infoBranch = HHPackageGitCurrentBranch[package];
		infoHEAD = HHPackageGitHEAD[package];
		infoRemotes = Apply[Rule, Transpose[ {HHPackageGitRemotes[package], HHPackageGitRemotesURL[package]} ], {1}];
		infoArtifactDate =  DateString[];
		tempInfo = <| "GitPath" -> infoPath, "GitBranch" -> infoBranch, "GitHEAD" -> infoHEAD, 
			"GitRemotes" -> infoRemotes, "GitArtifactDate" -> infoArtifactDate |>;
		Export[ tempArtifactFile, tempInfo ];
		tempInfo
	]
];

HHPackageWriteGitArtifact::noRepo="Package/file `1` is not in .git repo";


HHPackageWriteGitArtifact[args___]:=Message[HHPackageWriteGitArtifact::invalidArgs,{args}];


(* ::Subsubsection::Closed:: *)
(* HHPackageGitFindRepoDir*)


HHPackageGitFindRepoDir[directory_String]:=
(*HHPackageGitFindRepoDir[directory]=*)
Block[{tempret, temp},
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
	If[tempret === "", 
		Message[ HHPackageGitFindRepoDir::notGitDirectory, directory ];
		Null,
		FileNameJoin[{ tempret, ".git" }]
	]		
]; 


HHPackageGitFindRepoDirImpl[directory_String]:=
(* Can assume that DirectoryQ[ directory ] \[Equal] True *)
Block[{parentDirectory, putativeGitDirectory},

	If[  directory === "" || directory === ParentDirectory[ directory ],  (*If in root directory, ParentDirectory[] will act as Identity[] *)
		"",
		(*See if there is a ".git" folder in the parent directory, and if not, recurse up tree*)
		putativeGitDirectory=FileNameJoin[{ directory, ".git"}]; 
		If[ DirectoryQ[ putativeGitDirectory ], 
			directory,
			parentDirectory=Quiet[Check[ParentDirectory[directory], ""]]; (*If there is no parent directory, etc.*) 
			If[parentDirectory === "",
				"",
				HHPackageGitFindRepoDirImpl[ parentDirectory ] 
			]
		]
	]
	
]; 


(*HHPackageGitFindRepoDirImpl[directory_String]:=
Block[{parentDirectory, putativeGitDirectory},
	parentDirectory=Quiet[Check[ParentDirectory[directory], ""]]; (*If there is no parent directory, etc.*)
	(*Print[parentDirectory];*)
	If[  parentDirectory === "" || parentDirectory == directory,  (*If in root directory, ParentDirectory[] will act as Identity[] *)
		"",
		(*See if there is a ".git" folder in the parent directory, and if not, recurse up tree*)
		putativeGitDirectory=FileNameJoin[{ parentDirectory, ".git"}]; 
		(*Print[{putativeGitDirectory, DirectoryQ[ putativeGitDirectory ]}];*)
		If[ DirectoryQ[ putativeGitDirectory ], putativeGitDirectory, HHPackageGitFindRepoDirImpl[parentDirectory] ]
	]
]; *)


HHPackageGitFindRepoDir::notGitDirectory="No git directory \".git\" was found within the parent tree of `1`. Check typos/path."; 

HHPackageGitFindRepoDir[args___]:=Message[HHPackageGitFindRepoDir::invalidArgs,{args}];


(* ::Subsubsection:: *)
(* HHPackageGitLoad*)


HHPackageGitLoad[directory_String, opts:OptionsPattern[](* verbose_:False*)]:=
Block[{gitDirectory, tempArtifact},

	gitDirectory = Quiet[ HHPackageGitFindRepoDir[directory] ];

	If[ gitDirectory === Null,
		(*If not in active git directory, unload package and search for HHGitArtifact.m*)
		HHPackageGitUnload[], (*$HHCurrentGitRepository = Null*)
		
		(*If in active git directory, reload if different*)	
		If[ gitDirectory =!= $HHCurrentGitRepositoryPath,
			HHPackageGitUnload[HHVerbose -> False];
			$HHCurrentGitRepositoryPath = gitDirectory;
			$HHCurrentGitRepository = JavaNew["org.eclipse.jgit.internal.storage.file.FileRepository", gitDirectory];
			If[OptionValue[HHVerbose](*verbose*),
				Print["HokahokaW`HHPackageGitLoad: Loaded Git repository located at " <> gitDirectory ]
			]
		]
	];
	
	(*DEAL WITH ARTIFACTS, INDEPENDENTLY OF REPO => TODO prevent reload if already loaded*)
	(*Is artifact directly in specified directory?*)
	tempArtifact = FileNameJoin[{directory, "HHGitArtifact.m"}];
	If[ FindFile[tempArtifact] === $Failed,
		(*Is "directory" actually a package name/path?*)
		tempArtifact = FindFile[directory];
		If[ tempArtifact =!= $Failed,
			(*Is "directory" actually a package path?*)
			If[ FileNameSplit[ tempArtifact ][[ -1 ]] == "init.m",
				tempArtifact = FileNameJoin[ {ParentDirectory[DirectoryName[tempArtifact]], "HHGitArtifact.m"} ]
			];
			If[ !FileExistsQ[tempArtifact], tempArtifact = $Failed ]
		]
	];
	(*Load artifact*)
	If[ tempArtifact === $Failed,
		$HHCurrentGitArtifact = Null,
		
		$HHCurrentGitArtifact = Quiet[ Import[ tempArtifact ] ];
		If[ $HHCurrentGitArtifact === $Failed || 
				Head[$HHCurrentGitArtifact] =!= Association ||
				Head[ $HHCurrentGitArtifact[["GitPath"]] ] =!= String,
			$HHCurrentGitArtifact = Null
		]				
	]

];

(*HHPackageGitLoad::notGitDirectory="No git directory \".git\" was found within the parent tree of `1`."; 
HHPackageGitLoad::gitError="Call to Git returned error. It could be that Git is " <>
 "not installed correctly, the command `1` is not valid, or the directory `2` is not valid."; *)

HHPackageGitLoad[args___]:=Message[HHPackageGitLoad::invalidArgs,{args}];


(* ::Subsubsection:: *)
(* HHPackageGitUnload*)


HHPackageGitUnload[opts:OptionsPattern[](* verbose_:False*)]:=
Block[{},

	If[ $HHCurrentGitRepository =!= Null,
		If[OptionValue[HHVerbose](*verbose*),	Print[ "Unloading repository: "<> $HHCurrentGitRepositoryPath] ];
		$HHCurrentGitRepositoryPath = Null;
		$HHCurrentGitRepository = Null;
		$HHCurrentGitArtifact = Null
	];

];

HHPackageGitUnload[args___]:=Message[HHPackageGitUnload::invalidArgs,{args}];


(* ::Subsubsection:: *)
(* HHPackageGitCurrentBranch*)


HHPackageGitCurrentBranch[]:= HHPackageGitCurrentBranch[NotebookFileName[]];
HHPackageGitCurrentBranch[package_String]:= 
Block[{currBranch, currRef, currObjID},
	HHPackageGitLoad[package];
	If[ $HHCurrentGitRepository =!= Null, 

		(*If in active git directory*)
		$HHCurrentGitRepository@getBranch[], 
		
		(*If not in active git directory*)
		If[ $HHCurrentGitArtifact === Null,
			"NO VALID REPOSITORY",
			$HHCurrentGitArtifact[[ "GitBranch" ]]
		]
	]
];


HHPackageGitCurrentBranch[args___]:=Message[HHPackageGitCurrentBranch::invalidArgs,{args}];


(* ::Subsubsection:: *)
(* HHPackageGitHEAD *)


HHPackageGitHEAD[]:= HHPackageGitHEAD[NotebookFileName[]];
HHPackageGitHEAD[package_String]:= 
Block[{currBranch, currRef, currObjID},
	HHPackageGitLoad[package];
	If[ $HHCurrentGitRepository =!= Null,
	
		(*If in active git directory*)
		currRef=$HHCurrentGitRepository@getRef[ HHPackageGitCurrentBranch[package] ];
		currObjID=currRef@getObjectId[];
		currObjID@toString[ currObjID ],
		
		(*If not in active git directory*)
		If[ $HHCurrentGitArtifact === Null,
			"NO VALID REPOSITORY",
			$HHCurrentGitArtifact[[ "GitHEAD" ]]
		]
	]
];


HHPackageGitHEAD[args___]:=Message[HHPackageGitHEAD::invalidArgs,{args}];


(* ::Subsubsection:: *)
(* HHPackageGitRemotes / HHPackageGitRemotesURL*)


HHPackageGitRemotes[]:= HHPackageGitRemotes[NotebookFileName[]];
HHPackageGitRemotes[package_String]:= 
Block[{currConfig},
	HHPackageGitLoad[package];
	If[ $HHCurrentGitRepository =!= Null,

		(*If in active git directory*)
		currConfig=$HHCurrentGitRepository@getConfig[];
		currConfig@getSubsections["remote"]@toArray[],

		(*If not in active git directory*)
		If[ $HHCurrentGitArtifact === Null,
			"NO VALID REPOSITORY",
			(List@@#)& /@ $HHCurrentGitArtifact[[ "GitRemotes" ]]
		]
	]
];


HHPackageGitRemotes[args___]:=Message[HHPackageGitRemotes::invalidArgs,{args}];


HHPackageGitRemotesURL[]:= HHPackageGitRemotesURL[NotebookFileName[]];
HHPackageGitRemotesURL[package_String]:= 
Block[{remotes, currConfig},
	HHPackageGitLoad[package];
	If[ $HHCurrentGitRepository =!= Null,

		(*If in active git directory*)
		remotes = HHPackageGitRemotes[package];
		currConfig=$HHCurrentGitRepository@getConfig[];
		currConfig@getString["remote", #, "url"]& /@ remotes,
		
		(*If not in active git directory*)
		If[ $HHCurrentGitArtifact === Null,
			"NO VALID REPOSITORY",
			(#[[2]])& /@ $HHCurrentGitArtifact[[ "GitRemotes" ]]
		]
	]
];


HHPackageGitRemotesURL[args___]:=Message[HHPackageGitRemotesURL::invalidArgs,{args}];


(* ::Subsubsection::Closed:: *)
(* DEPRECATED HHPackageNewestFileDate *)


HHPackageNewestFileDate[package_String]:=
Block[{packageFile,tempdir},
	
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


(* ::Subsubsection::Closed:: *)
(*BAK old implementations for HHPackageGitXXX*)


(*HHPackageGitImpl[directory_String, command_String]:=
Block[{errorcode,tempret},
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


(*HHPackageGitRemotes[package_String]:= Block[{tempFile},
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
Block[{hhPrePackageGitHead, hhPrePackageGitHeadDate},
	Print[hhPrePackageGitHead = HHPackageGitHEAD[]];
	Print[hhPrePackageGitHeadDate = DateString[]];
	Export[notebookDirectory <> "GitHEAD.m", {hhPrePackageGitHead, 
  		hhPrePackageGitHeadDate}]
];
HHPackageUpdateGitHEADFile[args___]:=Message[HHPackageUpdateGitHEADFile::invalidArgs,{args}];
HHPackageGitHEAD[]:= HHPackageGitImpl[ NotebookDirectory[], "git rev-parse HEAD" ];
HHPackageGitHEAD[package_String]:= Block[{tempFile},
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


(* ::Subsubsection::Closed:: *)
(*BAK old implementation for HHPackageMessage *)


(*HHPackageMessage[package_String]:=
Block[{remotes, head, newest, tempFile, tempPreFile, searchFile},
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
Block[{names, expressionStrings, symbols, validNameValues},
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


HHJitterize[list_List, {lowerMultiply_, upperMultiply_}]:=
	list*RandomReal[{lowerMultiply,upperMultiply}, Length[list]];
HHJitterize[args___]:=Message[HHJitterize::invalidArgs,{args}];


(* ::Subsection::Closed:: *)
(*Utilities: Files*)


HHCreateDirectoryIfNone[directoryName_String]:= 
	If[ !FileExistsQ[directoryName], CreateDirectory[directoryName], Null ];
HHCreateDirectoryIfNone[args___]:=Message[HHCreateDirectoryIfNone::invalidArgs,{args}];


linux7z::usage="Null if not checked yet, 0 if linux 7z is present in system path";
linux7z = Null;


HHExtractArchive[archiveFileName_String, opts:OptionsPattern[] ]:= 
		HHExtractArchive[archiveFileName, "", opts];

HHExtractArchive[archiveFileName_String, targetDir_String, opts:OptionsPattern[] ]:= 
Block[
	{path7z, outputDir, commandString, preChecks = True},

	(*===Check system=== *)
	If[!StringMatchQ[ $System, "*Windows*"] && !StringMatchQ[ $System, "*Linux*"],
		Message[HHExtractArchive::winLinuxOnly, $System];
		preChecks=False
	];

	(*===Find 7zip directory=== *)
	path7z = OptionValue["7ZipPath"];
	If[ StringMatchQ[ $System, "*Linux*"],

		If[ preChecks && path7z === Automatic, 
			If[ linux7z === Null, linux7z = Run["7z"] ];
			If[ linux7z === 0, 
				path7z = "",
				Message[HHExtractArchive::cannotFind7Zip];
				preChecks = False
			];
		];
		
		If[ preChecks && linux7z =!= 0 && Head[path7z]===String && !FileExistsQ[path7z],
			Message[HHExtractArchive::cannotFind7Zip];
			preChecks=False
		],


		If[ StringMatchQ[ $System, "*Windows*"],
			If[ preChecks && path7z === Automatic, path7z = FindFile["7z.exe"] ];
			If[ preChecks && path7z === $Failed, path7z = FindFile["C:\\Program Files\\7-Zip\\7z.exe"] ];
			If[ preChecks && Head[path7z]===String && FileExistsQ[path7z], 
				path7z = DirectoryName[path7z],
				Message[HHExtractArchive::cannotFind7Zip];
				preChecks=False
			]
		]
	];

	If[ preChecks && !FileExistsQ[archiveFileName],
		Message[HHExtractArchive::archiveFileInvalid, outputDir];
		preChecks=False
	];

	outputDir=targetDir;
	If[ preChecks,
		If[ "" =!= outputDir,
			HHCreateDirectoryIfNone[outputDir],
			outputDir = NotebookDirectory[]
		]
	];
	
	If[preChecks,
		If[ path7z =!= "", SetDirectory[path7z] ];
		
		commandString = "7z"<> " x \""<> archiveFileName <> "\" -y ";
		commandString = commandString <> " -o\"" <> outputDir <>"\"";

		Run[commandString];
		If[ path7z =!= "", ResetDirectory[]];
	];
];

HHExtractArchive[args___]:=Message[HHExtractArchive::invalidArgs,{args}];

HHExtractArchive::winLinuxOnly = "HHExtractArchive currently works on windows/linux only, not on `1`";
HHExtractArchive::cannotFind7Zip = "Cannot find 7z executable! Please specify option \
\"7ZipPath\" or put 7z on the system path or standard location on C: (for windows).";
HHExtractArchive::archiveFileInvalid = "Archive file `1` is invalid!";


HHPrependFilename[fileName_String, prepend_String]:= 
Block[{fileNameSplit},
	fileNameSplit = FileNameSplit[fileName];
	FileNameJoin[ 
		Join[ fileNameSplit[[ ;; -2]],{prepend <> fileNameSplit[[-1]]}]
	]
];
HHPrependFilename[args___]:=Message[HHPrependFilename::invalidArgs,{args}];


HHAppendFileBaseName[fileName_String, append_String]:= 
Block[{fileNameSplit, fileBaseName, fileExtension},
	fileNameSplit = FileNameSplit[fileName];
	fileBaseName = fileNameSplit[[-1]];
		fileExtension = FileExtension[fileBaseName];
		fileBaseName = FileBaseName[fileBaseName];

	FileNameJoin[ 
		Join[ fileNameSplit[[ ;; -2]],{fileBaseName <> append <>"."<>fileExtension}]
	]
];
HHAppendFileBaseName[args___]:=Message[HHAppendFileBaseName::invalidArgs,{args}];


(* ::Subsection:: *)
(*Utilities: List manipulation*)


HHTakeCyclical[ list_List, indices_List/;Depth[indices]==2]:=
	list[[ Mod[indices - 1, Length[list]]+1 ]];


HHTakeCyclical[ list_List, index_Integer]:=
	list[[ Mod[index - 1, Length[list]]+1 ]];


HHTakeCyclical[ list_List, span_Span]:=
Module[{start, last, step},
	step = If[ Length[span] == 2, 1, span[[3]]];
	start = If[ span[[1]] < 0, Length[list] + span[[1]], span[[1]] ];
	last = If[ span[[2]] === All, 
			Length[list],
			If[ span[[2]] < 0, Length[list] + span[[2]], span[[2]] ]];
	Table[ HHTakeCyclical[ list, n ], {n, start, last, step}]				
];


HHTakeCyclical[args___] := Message[HHTakeCyclical::invalidArgs, {args}];


(* ::Section:: *)
(*End*)


End[];

EndPackage[];


HHPackageMessage["HokahokaW`"];
