(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Jan 31, 2015 *)

BeginPackage["HokahokaW`"]

General::invalidArgs="Function called with invalid arguments `1`.";
General::invalidOptionValue="Option argument `2` -> `1` is invalid.";
General::deprecated="Function is deprecated, use `1` instead.";

(* ::Subsubsection::Closed:: *)
(* Package Git functions *)
HHPackageNewestFileDate::usage="Prints the newest file change date for all files within the given package directory.";
HHPackageGitRemotes::usage="Prints a list of git remotes for either the given package or the current NotebookDirectory[]";
(*HHGitHEADHash::usage="Prints the git HEAD hash for either the given directory or the current NotebookDirectory[]";
HHPackageMessage::usage"Prints standard package message.";
HHNotebookMessage::usage"Prints standard notebook message.";*)

Begin["`Private`"]
(* Implementation of the package *)


(* ::Subsection:: *)
(* Package Git functions *)

(* ::SubSubsection:: *)
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

(* ::SubSubsection:: *)
(* HHPackageGitRemotes *)

HHPackageGitRemotes[package_String]:= Module[{tempFile},
	tempFile=FindFile[package];
	If[ tempFile === $Failed,
		Message[HHPackageGitRemotes::noFilesFound, package];
		" ",
		HHPackageGitRemotesImpl[ ParentDirectory[DirectoryName[ tempFile ]] ]
	]
];
HHPackageGitRemotes[]:= HHPackageGitRemotes[ NotebookDirectory[] ];

HHPackageGitRemotesImpl[directory_String]:=
Module[{errorcode,tempret},
	SetDirectory[ directory ];
	errorcode = Run["git remote -v > HHTempGitRemotes.txt"];
	If[ errorcode == 0,
		tempret = Import["HHTempGitRemotes.txt"];
		DeleteFile["HHTempGitRemotes.txt"],
		tempret = {} 		
	];
	ResetDirectory[];
	tempret
];

HHPackageGitRemotes[args___]:=Message[HHPackageGitRemotes::invalidArgs,{args}];
HHPackageGitRemotes::noFilesFound = HHPackageNewestFileDate::noFilesFound;
HHPackageGitRemotes::gitError = "git did not run correctly, may not be installed";

(* ::SubSubsection:: *)
(* HHPackageGitHEAD *)

HHPackageGitHEAD[package_String]:= Module[{tempFile},
	tempFile=FindFile[package];
	If[ tempFile === $Failed,
		Message[HHPackageGitHEAD::noFilesFound, package];
		" ",
		HHGitHEADHashImpl[ ParentDirectory[DirectoryName[ tempFile ]] ]
	]
];
HHGitHEADHash[]:= HHGitHEADHashImpl[ NotebookDirectory[] ];

HHGitHEADHashImpl[directory_String]:=
Module[{tempret},
	SetDirectory[ directory ];
	Run["git rev-parse HEAD > HHTempGitCurrentHEADHash.txt"];
	tempret= Import["HHTempGitCurrentHEADHash.txt"];
	DeleteFile["HHTempGitCurrentHEADHash.txt"];
	ResetDirectory[];
	tempret
];

HHGitHEADHash[args___]:=Message[HHGitHEADHash::invalidArgs,{args}];

HHGitHEADHash::noFilesFound = HHNewestFileDate::noFilesFound;
End[]

EndPackage[]

