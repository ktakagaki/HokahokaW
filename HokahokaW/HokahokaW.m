(* ::Package:: *)

(* Wolfram Language package *)


BeginPackage["HokahokaW`"];


HHPadZeros::usage =
"HHPadZeros[n] gives the numeral n string padded to 3 digits with zeros. " <>
"HHPadZeros[n,m] gives the numeral n string padded to m digits with zeros.";


HHPrintAssignmentCell::usage =
"Prints the given symbols and their value assignements in a cell. Use to record value assignments in a notebook.";
Options[HHPrintAssignmentCell]={"PrintDateString"-> True, "PrintFilename"->True};


Begin["`Private`"];


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


End[];

EndPackage[];
