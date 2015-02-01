(* Wolfram Language package *)


BeginPackage["HokahokaW`"];

HHPadZeros::usage =
"HHPadZeros[n] gives the numeral n string padded to 3 digits with zeros. " <>
"HHPadZeros[n,m] gives the numeral n string padded to m digits with zeros.";

Begin["`Private`"];

HHPadZeros[n_]:=HHPadZeros[n,3];
HHPadZeros[n_,m_Integer]:=
	If[n < 10^m,
		Apply[StringJoin,Map[ToString,IntegerDigits[n, 10, m] ]],
		n
	];

HHPadZeros[args___]:=Message[HHPadZeros::invalidArgs,{args}];

End[];

EndPackage[];
