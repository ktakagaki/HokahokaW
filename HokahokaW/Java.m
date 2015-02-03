(* Wolfram Language package *)


(* Wolfram Language package *)

BeginPackage["HokahokaW`Java`"];

HHJavaObjectQ::usage="Checks whether something is a Java object and an instance of the given class/interface.";

Begin["`Private`"];

(* TODO: Implement "HokahokaW`Java`"


HHJavaObjectQ[x_/;JavaObjectQ[x]]:= True;
HHJavaObjectQ[x_/;JavaObjectQ[x], className_String]:= InstanceOf[x, className];
HHJavaObjectQ[___]:= False;

*)

End[];

EndPackage[];
