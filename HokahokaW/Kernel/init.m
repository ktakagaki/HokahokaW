(* ::Package:: *)

(* Wolfram Language Init File *)


(*Needed to ensure jgit loading functions available below during certain call chains*)
<<JLink`;
InstallJava[];
AddToClassPath[FileNameJoin[{ParentDirectory[DirectoryName[FindFile["HokahokaW`"]]], "Java"}]];

Get[ "HokahokaW`HokahokaW`"];


Needs[ "HokahokaW`Graphics`"];
Needs[ "HokahokaW`Signal`"];
