(* ::Package:: *)

(* Wolfram Language package *)

BeginPackage["HokahokaW`Graphics`", {"HokahokaW`", "HokahokaW`Data`"}];


(* ::Section:: *)
(*Declarations*)


HHOptLabelStyleSpecifications::usage = "Option for HHLabelGraphics.";


(* ::Subsection:: *)
(*HHAbsoluteOptionsPlotRange*)


HHAbsoluteOptionsPlotRange::usage="";
HHAbsoluteOptionsAspectRatio::usage="";


(* ::Subsection::Closed:: *)
(*HHGraphicsColumn*)


HHGraphicsColumn::usage="Stacks images vertically. In contrast to the standard GraphicsColumn, adjusts widths to be equal.";
Options[HHGraphicsColumn]= Join[ {Spacings -> Scaled[0]}, Options[Graphics]];


HHGraphicsRow::usage="";
Options[HHGraphicsRow]= Options[HHGraphicsColumn];


(* ::Subsection::Closed:: *)
(*HHStackLists / HHListLinePlotStack*)


HHStackLists::usage = 
"HHStackLists takes lists and stacks the values (e.g. for stacked list plots with HHListLineStackPlot[])";

Options[HHStackLists] = {HHOptBaselineCorrection -> None(*, HHOptStack -> Automatic*)};


HHOptBaselineCorrection::usage =
"Option for HHStackTraces and HHListLinePlotStack. Specify how to correct the baseline for \
individual traces. Specify a function: for example, Mean  (the default) will subtract \
individual trace means, First will subtract the first value. \
(#[[1]]*2)& will subtract twice the First value.";


HHOptStack::usage="Option for HHStackTraces and HHListLinePlotStack. \
By what interval to stack lists. \n
Automatic: stack at x1.1 of the 95% Min-Max \
quantile (i.e. Quantile[ (# - Min[#])&[ Flatten[traces] ], 0.95]*1  \n \
None: no increment";


HHPlotRangeClipping::usage="";


HHListLinePlotStack::usage=
"HHListLineStaciPlot plots multiple traces together, stacked vertically.";

(*HHListLinePlotStack::usage=
"DEPRECATED HHListLinePlotStack plots multiple traces together, stacked vertically.";*)


HHListLinePlotStack$UniqueOptions = { };
HHListLinePlotStack$OverrideOptions = { PlotRange -> Automatic};
 
Options[HHListLinePlotStack] =
HHJoinOptionLists[
	HHListLinePlotStack$UniqueOptions, 
	HHListLinePlotStack$OverrideOptions,
	Options[HHStackLists],
	Options[ListLinePlot]
];


(* ::Subsection::Closed:: *)
(*HHListLinePlotGroups/HHListLinePlotGroupsStack*)


HHListLinePlotGroups::usage= 
"HHListLinePlotGroups is the same as ListLinePlot, but it allows one \
to specify multiple groups of plotting data, which are plotted as an overlay \
with different PlotStyle specifications.";


HHListLinePlotGroups$OverrideOptions = { PlotStyle -> Automatic };
Options[HHListLinePlotGroups] = HHJoinOptionLists[
	(*HHListLineGroupPlot$UniqueOptions, *)
	HHListLinePlotGroups$OverrideOptions,
	Options[ListLinePlot]
];


HHListLinePlotGroupsStack::usage= 
"";


HHListLinePlotGroupsStack$OverrideOptions = { PlotStyle -> Automatic };
Options[HHListLinePlotGroupsStack] = HHJoinOptionLists[
	(*HHListLineGroupPlot$UniqueOptions, *)
	HHListLinePlotGroupsStack$OverrideOptions,
	Options[ListLinePlot]
];


(* ::Subsection::Closed:: *)
(*HHListLinePlotMean*)


HHListLinePlotMean::usage= "HHListLinePlotMean plots multiple traces together, along with mean and standard error.";


HHOptMeanPlot::usage= "Option for HHListLinePlotMean. Whether to plot a mean trace in NNListLinePlotMean. True (plots mean) or False/None,  or \
functional specification such as Mean, Median, etc.";
HHOptMeanPlotStyle::usage= "PlotStyle for mean trace in NNListLinePlotMean.";

HHOptErrorPlot::usage= "Option for HHListLinePlotMean. Whether to plot error bound traces in NNListLinePlotMean.";
HHOptErrorPlotStyle::usage= "Option for HHListLinePlotMean. How to plot the upper and lower error bounds, also see HHErrorPlotFillingStyle.\
True, False/None, or \"StandardDeviation\", \"StandardError\", \"Quartiles\", \"MinMax\".";
HHOptErrorPlotFillingStyle::usage= "Option for HHListLinePlotMean. How to shade between the upper and lower error bounds, also see HHErrorPlotStyle.";


HHListLinePlotMean$UniqueOptions = 
	{
	HHOptMeanPlot -> True, HHOptMeanPlotStyle->Directive[Opacity[0.5]], 
	HHOptErrorPlot -> True,  HHOptErrorPlotStyle -> None, 
	HHOptErrorPlotFillingStyle -> Automatic
	};
HHListLinePlotMean$OverrideOptions = { PlotStyle -> None };

Options[HHListLinePlotMean] =
HHJoinOptionLists[
	HHListLinePlotMean$UniqueOptions, 
	HHListLinePlotMean$OverrideOptions,
	Options[ListLinePlot]
];


(* ::Subsection::Closed:: *)
(*HHListDensityPlot*)


HHListDensityPlot::usage= "ListDensityPlot allowing arrays smaller than 2x2";


Options[HHListDensityPlot] = Options[ListDensityPlot];


(* ::Subsection:: *)
(*HHLineHistogram*)


HHLineHistogram::usage = "";


HHLineHistogram$OverrideOptions = { PlotRange -> All, PlotStyle -> Automatic};
Options[HHLineHistogram]= HHJoinOptionLists[
	HHLineHistogram$OverrideOptions,
	Options[ListLinePlot]
];


(* ::Subsection::Closed:: *)
(*HHLabelGraphics*)


HHLabelGraphics::usage= "Labels a graphic with information on one of the corners.";
Options[HHLabelGraphics]={ HHOptLabelStyleSpecifications -> Tiny };


(* ::Subsection::Closed:: *)
(*Image Related*)


HHImageMean::usage="Gives the mean of a series of images. Image data must have the same dimensions and depths.";
HHImageCommon::usage="Gives the most common pixel cluster for each pixel in a series of images.  Image data must have the same dimensions and depths.";
HHImageDifference::usage="Filters an image list based on the common and threshold lists generated by HHImageCommon.";
Options[HHImageDifference]={Normalized->False};

HHImageSubtract::usage="Subtracts two images to give the difference.";


HHImageThresholdNormalize::usage="Normalizes an image to uniform brightness after thresholding.";
HHImageThresholdLinearNormalize::usage="Normalizes an image to uniform summed vector length after thresholding.";


HHImageThreshold::usage="Thresholds an image by closeness to the given color.";
HHImageThresholdLinear::usage="Thresholds an image by linear closeness to the given color.";


(* //ToDo2 create HHImageTestImage[] for help files??*)


(* ::Subsection:: *)
(*Plotting Utility Functions*)


HHColorDirectiveQ::usage = 
"HHColorDirectiveQ returns True if the argument given is a color directive (RGBColor, Hue, GrayLevel, ...).";


HHPlotStyleTable::usage = 
"HHPlotStyleTable creates a List of specified length, consisting of repeated plot style directives. \
If no color specification is given, the list of Automatic colors is applied cyclically.";


HHColorData::usage = 
"HHColorData is a utility extension to the standard ColorData function which \
allows easier access to the default Mathematica color scheme and other coloring \
schemes. 

In the most basic call pattern, HHColorData takes an Integer, and gives back the \
corresponding color specification within the Mathematica default \
color list, sampling circularly beyond the given color list. \
This circumvents ColorData not being callable beyond the given number of color samples.";
HHOptColorData::usage = "";

Options[HHColorData] = {HHOptColorData -> ColorData[97, "ColorList"]};


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*HHAbsoluteOptionsPlotRange*)


HHAbsoluteOptionsPlotRange[ gr_Graphics]:= 
Module[{tempPlotRange, tempPlotRangePadding},
	tempPlotRange = AbsoluteOptions[gr,PlotRange][[1,2]];
	tempPlotRangePadding = AbsoluteOptions[gr,PlotRangePadding][[1,2]];
	tempPlotRangePadding = {
		Map[
			If[Head[#]===List, #,
			If[Head[#]===Scaled, 
				#[[1]]*(tempPlotRange[[1,2]]-tempPlotRange[[1,1]]), #]]&,
				tempPlotRangePadding[[1]], 2
		],
		Map[
			If[Head[#]===List, #,
			If[Head[#]===Scaled, 
				#[[1]]*(tempPlotRange[[2,2]]-tempPlotRange[[2,1]]), #]]&,
				tempPlotRangePadding[[2]], 2
		]};
	tempPlotRange+tempPlotRangePadding*{{-1, 1},{-1, 1}}
];

HHAbsoluteOptionsPlotRange[args___]:=Message[HHAbsoluteOptionsPlotRange::invalidArgs,{args}];


HHAbsoluteOptionsAspectRatio[ gr_Graphics]:= 
Module[{tempDimensions(*tempPlotRange, tempPlotRangeReal*)},
	AbsoluteOptions[gr, AspectRatio][[1,2]]
	(*tempDimensions = ImageDimensions[Image[gr]];
	N[tempDimensions[[2]]/tempDimensions[[1]]]*)
	
	(*tempAbsolutePlotRange = HHAbsoluteOptionsPlotRange[gr];
	((#[[2,2]]-#[[2,1]])/(#[[1,2]]-#[[1,1]]))& [tempAbsolutePlotRange]*)
	(*tempPlotRange = AbsoluteOptions[gr,PlotRange][[1,2]];
	tempPlotRangeReal = HHAbsoluteOptionsPlotRange[gr];
	AbsoluteOptions[gr,AspectRatio][[1,2]]
		*(Subtract@@tempPlotRangeReal[[1]] / Subtract@@tempPlotRange[[1]])
		/(Subtract@@tempPlotRangeReal[[2]] / Subtract@@tempPlotRange[[2]])*)
];

HHAbsoluteOptionsAspectRatio[args___]:=Message[HHAbsoluteOptionsAspectRatio::invalidArgs,{args}];


(* ::Subsection::Closed:: *)
(*HHGraphicsColumn/Row*)


HHGraphicsColumn[list:{__}, opts:OptionsPattern[]]:= 
Module[{tempPlotRange, optSpacings, tempPlotWidth, tailHeightAccumulate},
	(*ToDo: With AbsoluteOption for ImageSize, once MMA bug is fixed*)

	tempPlotRange = HHAbsoluteOptionsPlotRange[list[[1]]];
	optSpacings = OptionValue[Spacings];
	optSpacings = If[ Head[optSpacings]===Scaled,
						(tempPlotRange[[2,2]]-tempPlotRange[[2,1]])*(optSpacings[[1]]),
						optSpacings
						]; 
	If[Dimensions[tempPlotRange]!={2,2},
 		Message[ HHGraphicsColumn::headNotGraphicsObject, list[[1]] ],
 		If[ Length[list]==1,
 			list[[1]],
 			
 			tempPlotWidth = tempPlotRange[[1,2]] - tempPlotRange[[1,1]];
 			tailHeightAccumulate = Accumulate[ 
 				tempPlotWidth * (HHAbsoluteOptionsAspectRatio[#]& /@ list[[2;;]]) ];
			Graphics[
				Prepend[
					Table[ 
						Inset[ Show[list[[n]], ImageSize->tempPlotWidth], 
							{tempPlotRange[[1,1]], tempPlotRange[[2,1]]-tailHeightAccumulate[[n-1]]- optSpacings*(n-1)},
							{Left, Bottom}, 
							tempPlotWidth
						], {n,2,Length[list]}
					],
					Inset[ Show[list[[1]], ImageSize->tempPlotWidth],
						{tempPlotRange[[1,1]], tempPlotRange[[2,1]]},
						{Left, Bottom}, 
						tempPlotWidth
					] 
				],
				PlotRange->{
					tempPlotRange[[1]](*+{-1,1}*tempPlotWidth*0.02*), {tempPlotRange[[2,1]]-tailHeightAccumulate[[-1]]-optSpacings*(Length[list]-1),
					tempPlotRange[[2,1]]+tempPlotWidth*(HHAbsoluteOptionsAspectRatio[list[[1]]])}
				},
				(*ImageSize->AbsoluteOptions[list[[1]], ImageSize],*)
				Sequence@@HHJoinOptionLists[ Graphics, {opts}, Options[HHGraphicsColumn] ]
			]
		]
	]
];


HHGraphicsColumn[args___]:=Message[HHGraphicsColumn::invalidArgs,{args}];
HHGraphicsColumn::headNotGraphicsObject="The first list element `1` must be a Graphics object with a PlotRange specification!";


HHGraphicsRow[list:{__}, opts:OptionsPattern[]]:= 
Module[{tempPlotRange, optSpacings, tempPlotHeight, tempPlotWidths, tempWidthAccumulate},
	(*ToDo: With AbsoluteOption for ImageSize, once MMA bug is fixed*)

	tempPlotRange = AbsoluteOptions[list[[1]], PlotRange][[1, 2]];
		tempPlotHeight = tempPlotRange[[2,2]]-tempPlotRange[[2,1]];
		tempPlotWidths = (tempPlotHeight / HHAbsoluteOptionsAspectRatio[#])& /@ list;
	optSpacings = OptionValue[Spacings];
	optSpacings = If[ Head[optSpacings]===Scaled,
						(tempPlotRange[[1,2]]-tempPlotRange[[1,1]])*(optSpacings[[1]]),
						optSpacings]; 
		tempWidthAccumulate = Accumulate[tempPlotWidths];
	tempPlotRange = {tempPlotRange[[1]], tempPlotRange[[2]]+{-1, 1}*tempPlotHeight*0.02};
	
	
	If[Dimensions[tempPlotRange]!={2,2},
		Message[ HHGraphicsRow::headNotGraphicsObject, list[[1]] ],
		If[ Length[list]==1,
			list[[1]],


			Graphics[
				Prepend[
					Table[ 
						Inset[ list[[n]], 
							{tempPlotRange[[1,1]]+tempWidthAccumulate[[n-1]], tempPlotRange[[2,1]]},
							{Left, Bottom}, 
							tempPlotWidths[[n]] 
 						], {n,2,Length[list]}
 					],
					Inset[ list[[1]],
						{tempPlotRange[[1,1]], tempPlotRange[[2,1]]},
						{Left, Bottom}, 
						tempPlotWidths[[1]] 
					] 
				],
				PlotRange-> {
					{0, tempWidthAccumulate[[-1]]} + tempPlotRange[[1,1]], 
					tempPlotRange[[2]]
				},
				Sequence@@HHJoinOptionLists[ Graphics, {opts}, Options[HHGraphicsRow] ]
 			]
 		]
 	]
 ];


HHGraphicsRow[args___]:=Message[HHGraphicsRow::invalidArgs,{args}];
HHGraphicsRow::headNotGraphicsObject="The first list element `1` must be a Graphics object with a PlotRange specification!";


(* ::Section::Closed:: *)
(*Stacking*)


(* ::Subsection::Closed:: *)
(*HHStackLists*)


HHStackLists[
	traces_ /; HHRaggedArrayDepth[traces]==2, 
	increment_/;NumericQ[increment], 
	opts:OptionsPattern[]
] :=
Module[{tempTraces, temp, 
		opHHOptBaselineCorrection, baselineSubtractFactors, 
		opHHOptStack, stackAddFactors, stackFactorsCumulated},
	
	tempTraces = traces;

	opHHOptBaselineCorrection = OptionValue[HHOptBaselineCorrection];

	(*====================*)
	(* Baseline subtraction *)
	(*====================*)
	baselineSubtractFactors = Switch[opHHOptBaselineCorrection,
		None, None,
		(*This covers specifications such as (#[[1]])& *)
		f_/;HHFunctionQ[f],
		    opHHOptBaselineCorrection/@traces, 
		(*This covers specifications such as Mean and First, which are function names *)
		f_/;(Quiet[temp=f[#]&/@traces]; And@@(NumericQ /@ temp) ), 
			temp, 
		_, Message[ HHStackLists::invalidOptionValue, "HHOptBaselineCorrection", ToString[opHHOptBaselineCorrection]]; 
			None
	];
	If[ baselineSubtractFactors =!= None,
		tempTraces = tempTraces - baselineSubtractFactors
	];

	(*====================*)
	(* Stack incrementation *)
	(*====================*)
	stackFactorsCumulated = HHStackLists$StackFactorsCumulated[increment, Length[traces]];
	tempTraces + stackFactorsCumulated

 ];


(*Stack lists of {{t1, x1}, {t2, x2}, ...} pairs in the second dimension*)
HHStackLists[
	traces_ /; (HHRaggedArrayDepth[traces] == 3), 
	increment_/;NumericQ[increment], 
	opts:OptionsPattern[]] :=
Module[{tempTimes, tempTraces},

	tempTimes = traces[[All, All, 1]];
	tempTraces = traces[[All, All, 2]];

	tempTraces =  HHStackLists[tempTraces, increment, opts];

	HHRaggedTranspose /@ MapThread[{#1, #2}&, {tempTimes, tempTraces}]
];


(* ::Subsubsection::Closed:: *)
(*Old Signatures*)


(*HHStackLists[traces_ /; Depth[traces]==3, opts:OptionsPattern[]] :=
Block[{tempTraces, temp, 
		opHHOptBaselineCorrection, baselineSubtractFactors, 
		opHHOptStack, stackAddFactors, stackFactorsCumulated},
	
	Message[ HHStackLists::deprecatedSignature ];

	tempTraces = traces;

	opHHOptBaselineCorrection = OptionValue[HHOptBaselineCorrection];

	(*====================*)
	(* Baseline subtraction *)
	(*====================*)
	baselineSubtractFactors = Switch[opHHOptBaselineCorrection,
		None, None,
		f_/;HHFunctionQ[f],    opHHOptBaselineCorrection/@traces, (*This covers specifications such as Mean and First or (#[[1]])& *)
		f_/;(Quiet[temp=f[#]&/@traces]; And@@(NumericQ /@ temp) ), 
					temp, (*This covers specifications such as Mean and First or (#[[1]])& *)
		_, Message[ HHStackLists::invalidOptionValue, "HHOptBaselineCorrection", ToString[opHHOptBaselineCorrection]]; 
		   None
	];
	If[ baselineSubtractFactors =!= None,
		tempTraces = tempTraces - baselineSubtractFactors
	];
	
	(*====================*)
	(* Stack incrementation *)
	(*====================*)
	opHHOptStack = OptionValue[HHOptStack];
	stackAddFactors = Switch[ opHHOptStack,
		None,                    None,
		Automatic,               Table[ Quantile[ (# - Min[#])&[ Flatten[traces] ], 0.95]*1.1, {Length[traces]}], (*- Subtract@@MinMax[ Flatten[traces] ]*)
		x_/;NumericQ[x],         Table[ opHHOptStack, {Length[traces]}],
		f_/;HHFunctionQ[f],      Table[ opHHOptStack[ Flatten[traces] ], {Length[traces]}], 
										(*This covers specifications such as Mean[#]& or (#[[1]])& *)
		f_/;(Quiet[temp=f[ Flatten[traces]]]; And@@(NumericQ /@ temp) ), 
								  temp, (*This covers specifications such as Mean and First *)
		_, Message[ HHStackLists::invalidOptionValue, "HHOptStack", ToString[opHHOptStack]]; Table[0, {Length[traces]}]
	];
	stackFactorsCumulated = FoldList[Plus, 0, stackAddFactors];
	$ActualStackRange = {- stackAddFactors[[1]], stackFactorsCumulated[[ -1 ]]};
	tempTraces + stackFactorsCumulated[[ ;; -2]](*FoldList[Plus, 0, stackAddFactors[[ ;; -2]] ]*)
						(*last stack add factor is not used here... nothing to stack on top*)

   ];*)


(*(*Stack lists of {{t1, x1}, {t2, x2}, ...} pairs in the second dimension*)
HHStackLists[
	traces_ /; (Depth[traces]==4 && Union[(Dimensions /@ traces)[[All, 2]]]=={2}), 
	opts:OptionsPattern[]] :=

Block[{tempTimes, tempTraces},

	Message[ HHStackLists::deprecatedSignature ];

	tempTimes = traces[[All, All, 1]];
	tempTraces = traces[[All, All, 2]];

	tempTraces =  HHStackLists[tempTraces, opts];

	Transpose /@ MapThread[{#1, #2}&, {tempTimes, tempTraces}]
];*)


(* ::Subsubsection:: *)
(*Fallthrough*)


HHStackLists[args___] := Message[HHStackLists::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*HHStackLists$StackFactorsCumulated*)


HHStackLists$StackFactorsCumulated[ increment_, count_ ]:=
	FoldList[Plus, 0, Table[ increment, {count}] ][[ ;; -2]];


(* ::Subsection::Closed:: *)
(*HHListLinePlotStack*)


(*Used to pass variables to HHListLinePlotStack in an extra-functional manner*)
$ActualStackRange={0,0};


HHListLinePlotStack[
	traces_/;(Length[Dimensions[traces]] == 2 || 
		(Length[Dimensions[traces]] == 3 && Union[(Dimensions /@ traces)[[All, 2]]]=={2})), 
	increment_/;NumericQ[increment], 
	opts:OptionsPattern[]
]:=
Module[{tempData, tempPlotRangeOpts},
	
	tempData = HHStackLists[traces, increment, Sequence@@FilterRules[{opts}, Options[HHStackLists]]];
	tempPlotRangeOpts = 
		If[ OptionValue[PlotRange] === Automatic, 
			{PlotRange -> {All, {- increment/2, (Length[traces]-1/2)*increment}},
			AxesOrigin -> If[OptionValue[AxesOrigin] === Automatic, {Automatic, - increment/2}, OptionValue[AxesOrigin]]},
			{PlotRange -> OptionValue[PlotRange]}
		];

	ListLinePlot[tempData,
		Sequence@@HHJoinOptionLists[ ListLinePlot, tempPlotRangeOpts, {opts}, 
			HHListLinePlotStack$UniqueOptions 
		]
	]
];


HHListLinePlotStack[
	{},
	increment_/;NumericQ[increment], 
	opts:OptionsPattern[]
]:=
Module[{},
	Graphics[ {},
		Sequence@@FilterRules[Join[{opts},Options[HHListLinePlotStack]], Options[Graphics]] 
	]
];


HHListLinePlotStack[args___] := Message[HHListLinePlotStack::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*HHListLinePlotGroups*)


HHListLinePlotGroups[
	traces_/;(HHRaggedArrayDepth[traces] == 3 || Depth[traces] == 4),
	opts:OptionsPattern[]
]:=
Module[{tempStyles},
	
	tempStyles = HHPlotStyleTable[OptionValue[PlotStyle], {Length[traces]}];
(*Print[tempStyles];
Print[Dimensions/@traces];
Print[OptionValue[PlotStyle]];*)
	Show[MapThread[
		ListLinePlot[#1,
			Sequence@@HHJoinOptionLists[ ListLinePlot, 
				{PlotRange-> All, PlotStyle -> #2} , {opts}, Options[HHListLinePlotGroups]
			]
		]&, {traces, tempStyles}
	], Sequence@@HHJoinOptionLists[Graphics, {opts}, Options[HHListLinePlotGroups]]]
	
];


HHListLinePlotGroups[
	{},
	increment_/;NumericQ[increment], 
	opts:OptionsPattern[]
]:=
Module[{tempData, stackFactorsCumulated},
	Graphics[ {},
		Sequence@@FilterRules[Join[{opts},Options[HHListLinePlotGroupsStack]], Options[Graphics]] 
	]
];


HHListLinePlotGroups[args___] := Message[HHListLinePlotGroups::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*HHListLinePlotGroupsStack*)


HHListLinePlotGroupsStack[
	{},
	increment_/;NumericQ[increment], 
	opts:OptionsPattern[]
]:=
Module[{tempData, stackFactorsCumulated},
	Graphics[ {},
		Sequence@@FilterRules[Join[{opts},Options[HHListLinePlotGroupsStack]], Options[Graphics]] 
	]
];


HHListLinePlotGroupsStack[
	{{}},
	increment_/;NumericQ[increment], 
	opts:OptionsPattern[]
]:=
Module[{tempData, stackFactorsCumulated},
	Graphics[ {},
		Sequence@@FilterRules[Join[{opts},Options[HHListLinePlotGroupsStack]], Options[Graphics]] 
	]
];


HHListLinePlotGroupsStack[
	traces_/;(HHRaggedArrayDepth[traces] == 3 || Depth[traces] == 4),
	increment_/;NumericQ[increment], 
	opts:OptionsPattern[]
]:=
Module[{tempData, stackFactorsCumulated},
	
	(*tempData = HHStackLists[#, increment, Sequence@@FilterRules[{opts}, Options[HHStackLists]]]& /@ 
		Transpose[traces];*)
	
	stackFactorsCumulated = HHStackLists$StackFactorsCumulated[increment, Length[traces]];
	HHListLinePlotGroups[ 
		MapThread[
			(#1 + #2)&,
			{traces, stackFactorsCumulated}
		],
		Sequence@@FilterRules[Join[{opts},Options[HHListLinePlotGroupsStack]], Options[HHListLinePlotGroups]] 
	]
(*Print[Dimensions /@ traces];
Print[Dimensions[ stackFactorsCumulated]];*)	
];


HHListLinePlotGroupsStack[args___] := Message[HHListLinePlotGroupsStack::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*HHListLinePlotMean*)


HHListLinePlotMean[traces_/;(Length[Dimensions[traces]]==2 && Length[Union[Length/@traces]]==1), opts:OptionsPattern[]]:=
Module[{temp,
		tempMeanData = {}, 
		tempErrorMeanData = {}, tempErrorData1 = {}, tempErrorData2 = {},
		opPlotStyle,
		opMeanPlot, opMeanPlotStyle,
		opErrorPlot, opErrorPlotStyle, opErrorPlotFillingStyle, 
		grMean, grError, grErrorFilling, grMain},

	opPlotStyle=OptionValue[PlotStyle];
	opMeanPlot=OptionValue[HHOptMeanPlot];	opMeanPlotStyle=OptionValue[HHOptMeanPlotStyle];
	
	opErrorPlot=OptionValue[HHOptErrorPlot]; opErrorPlotStyle=OptionValue[HHOptErrorPlotStyle]; 
	opErrorPlotFillingStyle=OptionValue[HHOptErrorPlotFillingStyle];
	If[ MemberQ[{False, None, Null}, opErrorPlotStyle] ==  
		None && MemberQ[{False, None, Null}, opErrorPlotFillingStyle], opErrorPlot = False];
	
	(*==========Process data==========*)
	tempMeanData = Switch[ opMeanPlot,
		x_/;MemberQ[{False, None, Null}, x],    {},
		True,                                    Mean[traces],
		f_/;HHFunctionQ[f],                     opMeanPlot/@Transpose[traces],
		f_/;(Quiet[temp=f[#]&/@Transpose[traces]]; And@@(NumericQ /@ temp) ), 
												temp,
		_, Message[ HHListLinePlotMean::invalidOptionValue, "HHMeanPlot", ToString[opMeanPlot]]; 
												{}(*Table[0, {Length[traces[[1]]]}]*)
	];
	
	Switch[ opErrorPlot ,
		x_/;MemberQ[{False, None, Null}, x], Null,
		x_/;MemberQ[{True, "StandardError"}, x], (
			tempErrorMeanData = Mean[traces];
			tempErrorData1 = StandardDeviation[traces]/Sqrt[Length[traces]];
		),
		x_/;MemberQ[{"StandardDeviation", StandardDeviation}, x], (
			tempErrorMeanData = Mean[traces];
			tempErrorData1 = StandardDeviation[traces];
		),
		x_/;MemberQ[{MedianDeviation, "MedianDeviation"}, x], (
			tempErrorMeanData = Median[traces];
			tempErrorData1 = MedianDeviation[traces];
		),
		x_/;MemberQ[{Quartiles, "Quartiles"}, x], (
			tempErrorData1 = Quartiles[traces];
			tempErrorData2 = tempErrorData1[[All, 1]];
			tempErrorData1 = tempErrorData1[[All, 3]];
		),
		x_/;MemberQ[{MinMax, "MinMax"}, x], (
			tempErrorData1 = MinMax /@ Transpose[traces];
			tempErrorData2 = tempErrorData1[[All, 1]];
			tempErrorData1 = tempErrorData1[[All, 2]];
		),
		_, Message[ HHListLinePlotMean::invalidOptionValue, "HHErrorPlot", ToString[opErrorPlot]]
	];

	If[ opErrorPlot =!= False && tempErrorData2 === {},
		tempErrorData2 = tempErrorMeanData - tempErrorData1;
		tempErrorData1 = tempErrorMeanData + tempErrorData1;
	];


	(*==========Create graphics==========*)
	
		(*==========Mean trace plot==========*)
		grMean=If[ tempMeanData == {}, 
			{},
			ListLinePlot[tempMeanData, 
				Sequence@@HHJoinOptionLists[ListLinePlot, 
				{PlotStyle -> opMeanPlotStyle}, {opts}, Options[HHListLinePlotMean]]
			]
		];

		(*==========Error filling plot==========*)
		grErrorFilling = 
		If[ tempErrorData1 == {} || 
			MemberQ[{False, Null, None, {}, ""}, opErrorPlotFillingStyle], 
			{},
			ListLinePlot[{tempErrorData1, tempErrorData2}, 
				Sequence@@HHJoinOptionLists[ListLinePlot,
					{PlotStyle -> None, Filling->{1 -> {2}}, FillingStyle -> opErrorPlotFillingStyle},
					{opts}, Options[HHListLinePlotMean]
					]
			]
		];

		(*==========Error trace plots==========*)
		grError=If[ tempErrorData1 == {} || MemberQ[{False, Null, None, {}, ""}, opErrorPlotStyle], 
			{},
			ListLinePlot[{tempErrorData1, tempErrorData2}, 
				Sequence@@HHJoinOptionLists[ListLinePlot,
					{PlotStyle -> opErrorPlotStyle},
					{opts}
				]
			]
		];

		(*==========Traces plot==========*)
		grMain = If[ MemberQ[{False, Null, None, {}, ""}, opPlotStyle],
			{},
			ListLinePlot[traces, 
				Sequence@@HHJoinOptionLists[ ListLinePlot, 
					{PlotStyle -> opPlotStyle}, {opts}, Options[HHListLinePlotMean]
				]
			]
		];

	(*==========Combine plots==========*)
	Show@@Flatten[{grErrorFilling, grError, grMean, grMain}]
];


HHListLinePlotMean[args___] := Message[HHListLinePlotMean::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*HHListDensityPlot*)


HHListDensityPlot[ data_/;MatrixQ[data], opts:OptionsPattern[] ]:=
Module[{tempData=data},
	If[ Length[tempData]==1, tempData = Join[tempData, tempData] ];
	If[ Length[tempData[[1]]]==0, 
		tempData = ({0,0})& /@ tempData,
		If[ Length[ tempData[[1]] ] == 1,
			tempData = {#[[1]],#[[1]]}& /@ tempData
		]
	];
	ListDensityPlot[tempData, opts]
];


HHListDensityPlot[args___] := Message[HHListDensityPlot::invalidArgs, {args}];


(* ::Section:: *)
(*HHLineHistogram*)


HHLineHistogram[
	data_/;(HHRaggedArrayDepth[data]==1 || HHRaggedArrayDepth[data]==2), 
	opts:OptionsPattern[] ]:=
HHLineHistogram[data, Automatic, Automatic, opts];


HHLineHistogram[
	data_/;(HHRaggedArrayDepth[data]==1 || HHRaggedArrayDepth[data]==2), 
	bspec_/;(Head[bspec]=!=Rule), 
	opts:OptionsPattern[] ]:=
HHLineHistogram[data, bspec, Automatic, opts];


HHLineHistogram[
	data_/;HHRaggedArrayDepth[data]==1, 
	bspec_/;(Head[bspec]=!=Rule), 
	hspec_/;(Head[hspec]=!=Rule), 
	opts:OptionsPattern[] ]:=
HHLineHistogramImpl[ HistogramList[data,bspec,hspec], opts];


HHLineHistogram[{}, ___ ]:= Graphics[];


HHLineHistogram[
	data_/;( HHRaggedArrayDepth[data] == 2 ), 
	bspec_/;(Head[bspec]=!=Rule), 
	hspec_/;(Head[hspec]=!=Rule), 
	opts:OptionsPattern[] 
]:= HHLineHistogramImpl[ HistogramList[#, bspec, hspec]& /@ data, opts ];


HHLineHistogramImpl[
	data_/;(HHRaggedArrayDepth[data]<=3 && And@@( HHHistogramListQ /@ data ) ),
	opts:OptionsPattern[] 
]:= Module[{realPlotStyleList}, 
	realPlotStyleList = HHPlotStyleTable[ OptionValue[PlotStyle], {Length[data]}];
	Show[MapThread[
		HHLineHistogramImpl[#1, 
			PlotStyle->#2, 
			Sequence@@FilterRules[
				Join[{opts}, Options[HHLineHistogram]], 
				Options[HHLineHistogramImpl]]
		]&, 
		{data, realPlotStyleList}
	], Sequence@@FilterRules[
			Join[{opts}, Options[HHLineHistogram]], 
			Options[Graphics]] 
	]
];


(*HHLineHistogram[data_/;HHRaggedArrayDepth[data]==2, 
	bspec_/;(Head[bspec]=!=Rule), hspec_/;(Head[hspec]=!=Rule), opts:OptionsPattern[] 
]:=
Module[{realPlotStyleList = HHPlotStyleTable[ OptionValue[PlotStyle], {Length[data]}]}, 
	Show[MapThread[
		HHLineHistogramImpl[HistogramList[#1, bspec, hspec], 
			PlotStyle->#2, 
			Sequence@@FilterRules[
				Join[{opts}, Options[HHLineHistogram]], 
				Options[HHLineHistogramImpl]]
		]&, 
		{data, realPlotStyleList}
	], Sequence@@FilterRules[
			Join[{opts}, Options[HHLineHistogram]], 
			Options[Graphics]] 
	]
];*)


HHLineHistogram[args___] := Message[HHLineHistogram::invalidArgs, {args}];


Options[HHLineHistogramImpl] = Options[ListLinePlot];


HHLineHistogramImpl[{{}, {}}, ___ ]:= Graphics[];


HHLineHistogramImpl[histogramList_/;HHRaggedArrayDepth[histogramList]==2, opts:OptionsPattern[] ]:=
Module[{countBorder=Partition[Riffle[Riffle[#1,#1[[2;;]]],Riffle[#2,#2]],2]&@@histogramList},
	ListLinePlot[countBorder, 
		PlotRange->All,
		Sequence@@FilterRules[
			Join[{opts}, Options[HHLineHistogramImpl]],
			Options[ListLinePlot]
		]
	]
];


HHLineHistogramImpl[args___] := Message[HHLineHistogramImpl::invalidArgs, {args}];


(* ::Section::Closed:: *)
(*HHLabelGraphics*)


HHLabelGraphics[ graphics_Graphics, text_String, opts:OptionsPattern[] ]:=
	HHLabelGraphics[ graphics, text, {Right, Bottom}, opts];


HHLabelGraphics[ graphics_Graphics,
	text_String, {alignmentX_:Right, alignmentY_:Bottom}, 
	opts:OptionsPattern[] ]:=
Module[{optLabelStyleSpecifications, tempAbsPlotRange,
		tempX, tempY},
		
	optLabelStyleSpecifications = OptionValue[ HHOptLabelStyleSpecifications ];
	tempAbsPlotRange = AbsoluteOptions[graphics, PlotRange][[1, 2]];
	
	tempX = Switch[ alignmentX, 
		Left, tempAbsPlotRange[[1, 1]],
		Right, tempAbsPlotRange[[1, 2]],
		_,  Message[HHLabelGraphics::invalidAlignmentX, alignmentX];
			tempAbsPlotRange[[1, 2]]
	];
	tempY = Switch[ alignmentY, 
		Top, tempAbsPlotRange[[2, 2]],
		Bottom, tempAbsPlotRange[[2, 1]],
		_,  Message[HHLabelGraphics::invalidAlignmentY, alignmentY];
			tempAbsPlotRange[[2, 1]]
	];
	
	Show[
		graphics,
		Graphics[Text[ Style[ text, Sequence@@optLabelStyleSpecifications ],
			{tempX, tempY}, {alignmentX, alignmentY}
		]]
	]
];


HHLabelGraphics::invalidAlignmentX = "X alignment must be Left or Right, not `1`";
HHLabelGraphics::invalidAlignmentY = "Y alignment must be Top or Bottom, not `1`";


HHLabelGraphics[args___]:=Message[HHLabelGraphics::invalidArgs, {args}];


(* ::Section::Closed:: *)
(*Image Related*)


(* ::Subsubsection::Closed:: *)
(*HHImageMean*)


HHImageMean[x:{__Image}]:= HHImageMean[ImageData /@ x];

HHImageMean[imageDataList_List/;Depth[imageDataList]==5]:=
Block[{},
	(*The following part is repeated with modifications*)
	If[ Length[Union[  Dimensions/@imageDataList ]]!=1,
		Message[HHImageMean::dimensionsMustBeSame];,
		Image[ Mean[imageDataList] ]
	]
];

HHImageMean::dimensionsMustBeSame = "Input list of Image objects must all have the same dimensions and color depths!";

HHImageMean[args___]:=Message[HHImageMean::invalidArgs, {args}];


HHImageSubtract[a_Image, b_Image]:=Image[ ImageData[a] -ImageData[b]];

HHImageSubtract[args___]:=Message[HHImageSubtract::invalidArgs, {args}];


(*HHImageDifference[imageData_List, templateData_List, threshold_]:= 
Block[{tempSelf,tempProd, threshLower, threshUpper},
	tempSelf=MapThread[ Dot, {imageData,imageData},2];
	tempProd=MapThread[ Dot, {imageData+0.0001,templateData+0.0001},2];
	(*Map[ Clip[Norm[#],{1-threshold, 1+ threshold},{0, 0}]&, (imageData-templateData)(*tempSelf/tempProd*), {2}]*)
	threshLower=1-threshold;
	threshUpper=1+threshold;
	MapThread[ 
		Block[{temp},
			temp=#1-#2;
			If[threshLower \[LessEqual] temp &&  temp \[LessEqual] threshUpper, #3, {0.,0.,0.}]
		]&, 
		{tempSelf, tempProd, imageData}, {2}]
];*)


(* ::Subsubsection::Closed:: *)
(*HHImageDifference*)


HHImageDifference[imageData_List/;Depth[imageData]==4, templateData_List, threshold_]:= 
	MapThread[ 
		Block[{temp},
			temp=Norm[#1-#2];
			If[temp <= threshold, {0.,0.,0.}, #1]
		]&, 
		{imageData, templateData}, 2
	];


HHImageDifference[imageData_List/;Depth[imageData]==4, templateData_List, threshold_]:= 
	MapThread[ 
		Block[{temp},
			temp=Norm[Normalize[#1]-Normalize[#2]];
			If[temp <= threshold, {0.,0.,0.}, #1]
		]&, 
		{imageData, templateData}, 2
	];


HHImageDifferenceImpl[imageDataNorm_List, templateData_List, threshold_]:= 
	MapThread[ 
		Block[{temp},
			temp=Norm[#1-#2];
			If[temp <= threshold, {0.,0.,0.}, #1]
		]&, 
		{imageDataNorm, templateData}, 2
	];


(*HHImageDifference[x_Image, commonList_List, threshold_]:= HHImageDifference[{x}, commonList, threshold][[1]];

HHImageDifference[x:{__Image}, commonList_List, threshold_]:=
Block[{commonListDim, threshold2, filterImage},
	(*The following part is repeated with modifications*)
	commonListDim = Dimensions[commonList];
	threshold2 = threshold^2.;

(*	filterImageC= Compile[{{imageDataC, _Real, 3}},
		MapThread[ If[ Plus@@((#1 - #2)^2.) <= threshold2, {0.,0.,0.}, #1  ]&,
			{imageDataC, commonList}, 2    					
		],
		{{threshold2, _Real},{commonList, _Real, 3}}
	];*)
	filterImage[image_Image]:= Block[{imageData},
		imageData=ImageData[image];
		If[ commonListDim != Dimensions[imageData],
			Message[HHImageDifference::commonDimensionMustMatch];,
			(*filterImageC[imageData]*)
			MapThread[ If[ Plus@@((#1 - #2)^2.) <= threshold2, {0.,0.,0.}, #1  ]&,
			{imageData, commonList}, 2    					
			]
		]
	];

	ParallelMap[ filterImage, x ]
];*)

(*filterImage[imageData_List, commonList_List, threshold_]:=
Block[{tempList, threshold2},
	threshold2 = threshold^2.;
	(*tempList=Transpose[{imageData, commonList},{3,1,2}];
	Map[   If[ Plus@@((#[[1]] - #[[2]])^2) <= threshold2,  {0,0,0}, #[[1]]  ]&,   tempList, {2} ]*)
	MapThread[ 
		If[ Plus@@((#1 - #2)^2.) <= threshold2, {0.,0.,0.}, #1  ]&,
		 {imageData, commonList}, 2]
];*)

(*HHImageDifference[x:{__Image}, commonList_List, threshold_]:=
Block[{tempImageData, tempImageDim, func, threshold2, euclideanDistance2Threshold},
	(*The following part is repeated with modifications*)
	tempImageData=ImageData /@ x;
	If[ Length[Union[  Dimensions/@tempImageData ]]!=1,
		Message[HHImageMean::dimensionsMustBeSame];,
			tempImageDim=Dimensions[ImageData[x[[1]]]];
			If[ tempImageDim \[NotEqual] Dimensions[commonList],
				Message[HHImageDifference::commonDimensionMustMatch];,
				threshold2=threshold^2.;
				euclideanDistance2Threshold=Compile[{{xx, _Real, 1}, {yy, _Real, 1}},
					If[ Plus@@((xx - yy)^2.) <= threshold2, {0.,0.,0.}, xx  ]
				];
				func=Compile[{{oneImage, _Real, 3}},
					MapThread[ euclideanDistance2Threshold, {oneImage, commonList}, 2],
					{{commonList, _Real, 3}}
				];
				ParallelMap[ func, tempImageData ]
			]
	]
];*)


(*HHImageDifference[x:{__Image}, {commonList_List, thresholdList_List}]:=
Block[{tempImageData, tempImageDim},
	(*The following part is repeated with modifications*)
	tempImageData=ImageData /@ x;
	If[ Length[Union[  Dimensions/@tempImageData ]]!=1,
		Message[HHImageMean::dimensionsMustBeSame];,
			tempImageDim=Dimensions[ImageData[x[[1]]]];
			If[ tempImageDim \[NotEqual] Dimensions[commonList],
				Message[HHImageDifference::commonDimensionMustMatch];,
				If[ tempImageDim[[1;;2]] \[NotEqual] Dimensions[thresholdList],
					Message[HHImageDifference::thresholdDimensionMustMatch];,
					ParallelMap[ filterImage[#, {commonList, thresholdList}]&, tempImageData ]
				]
			]
	]
];

filterImage[imageData_List, {commonList_, thresholdList_}]:=
Block[{tempList},
	tempList=Transpose[{imageData, commonList, thresholdList},{3,1,2}];
	Map[   If[ Plus@@((#[[1]] - #[[2]])^2) <= #[[3]],  {0,0,0}, #[[1]]  ]&,   tempList, {2} ]
];*)


HHImageDifference[args___]:=Message[HHImageDifference::invalidArgs, {args}];


HHImageDifference::dimensionsMustBeSame = "Input list of Image objects must all have the same dimensions and color depths!";
HHImageDifference::commonDimensionMustMatch = "Input common Lists must have same dimensions as Images";
HHImageDifference::thresholdDimensionMustMatch = "Input threshold List must have same dimensions as Images";


(* ::Subsubsection::Closed:: *)
(*HHImageCommon*)


HHImageCommon[x:{__Image}/;Length[x]>=4]:=
Block[{tempImageData},
	(*The following part is repeated with modifications*)
	tempImageData=ImageData /@ x;
	If[ Length[Union[  Dimensions/@tempImageData ]]!=1,
		Message[HHImageCommon::dimensionsMustBeSame];,
		
		(*DistributeDefinitions["HokahokaW`Graphics`"];
		DistributeDefinitions["HokahokaW`Graphics`Private`"];*)
		ParallelMap[HHImageCommonImpl, Transpose[tempImageData, {3, 1, 2, 4}], {2}]
		(*ParallelMap[medianQuietPixelsShortC, Transpose[tempImageData, {3, 1, 2, 4}], {2}]*)
		(*medianQuietPixelsWholeShortC[ tempImageData ]*)
	]
];
HHImageCommon::dimensionsMustBeSame = "Input list of Image objects must all have the same dimensions and color depths!";

HHImageCommon[args___]:=Message[HHImageCommon::invalidArgs, {args}];


HHImageCommonImpl[{data__List}, sqEucThreshold_]:=
Block[{dataBuildup, temp, tempNewElementList},
	If[ Length[{data}] > 5,
		dataBuildup = {{{data}[[1]] , {{data}[[1]]}}};

		Do[
			(*sort by SquaredEuclideanDistance*)
			temp={SquaredEuclideanDistance[elementC, #[[1]]], #}&/@dataBuildup;
			temp= SortBy[ temp, First]; 
			(* temp = {  { sed1, {mean1, {{x,y,z}, {x,y,z}, ...}}}, { sed2, {mean2, {{x,y,z}, {x,y,z}, ...}}}, ... } *)

			dataBuildup=If[ temp[[1,1]]<= sqEucThreshold,
			tempNewElementList= Append[(temp[[1,2,2]]), elementC];
			ReplacePart[temp[[All, 2]], 1 -> {Mean[tempNewElementList], tempNewElementList}],
			Join[temp[[All , 2]], {{elementC, {elementC}}}]
		],
		{elementC, {data}[[2;;]]}
	];

	temp= SortBy[dataBuildup, Length[#[[2]]]&][[-1]];
	If[ Length[temp[[2]]] <3,{},temp[[1]]],

	Message[ HHImageCommon::notEnoughItems, Length[{data}]]
	]
];



HHImageCommon::notEnoughItems="Not enough items in list (`1`)!";


(*HHImageCommonImpl[{}, {remainingElements__List}, sqEucThreshold_]:=
	If[ Length[{remainingElements}] > 5,
		HHImageCommonImpl[
		{{{remainingElements}[[1]] , {{remainingElements}[[1]]}}},  {remainingElements}[[2 ;;]],
			 sqEucThreshold
	],
	Message[ HHImageCommon::notEnoughItems, Length[{remainingElements}]]
];*)


(*HHImageCommonImpl[{dataElements__List}, {}, sqEucThreshold_]:=
Block[{temp},
	temp= SortBy[{dataElements}, Length[#[[2]]]&][[-1]];
	If[ Length[temp[[2]]] <3,{},temp[[1]]]
];*)


(*HHImageCommonImpl[{dataElements__List}, {remainingElements__List}, sqEucThreshold_]:=
(* {dataElements} = {  {mean1, {{x,y,z}, {x,y,z}, ...}}, {mean2, {{x,y,z}, {x,y,z}, ...}}, ... } *)
(* {remainingElements} = { {x,y,z}, {x,y,z}, ... } *)

With[{elementC = First[{remainingElements}]},
Block[{temp, tempNewElementList},

	(*elementC = First[{remainingElements}];*)

	(*sort by SquaredEuclideanDistance*)
	temp = ( ({SquaredEuclideanDistance[ elementC,  #[[1]]], #}&) /@ {dataElements} );
	temp = SortBy[ temp, First];
	(* temp = {  { sed1, {mean1, {{x,y,z}, {x,y,z}, ...}}}, { sed2, {mean2, {{x,y,z}, {x,y,z}, ...}}}, ... } *)

	HHImageCommonImpl[
		If[ temp[[1,1]]<= sqEucThreshold,
		tempNewElementList= Append[(temp[[1,2,2]]),elementC];
		ReplacePart[temp[[All, 2]], 1 -> {Mean[tempNewElementList], tempNewElementList}],
		Join[temp[[All , 2]], {{elementC, {elementC}}}]
		],
		Rest[{remainingElements}], 
		sqEucThreshold
	]
]];*)


HHImageCommonImpl[list_List]:=
Block[{temp},
	temp = HHImageCommonImpl[list, 0.00025];
	If[temp==={}, temp = HHImageCommonImpl[list, 0.001]];
	If[temp==={}, temp = HHImageCommonImpl[list, 0.004]];
	If[temp==={}, temp = HHImageCommonImpl[list, 0.016]];
	If[temp==={}, temp = HHImageCommonImpl[list, 0.064]];
	If[temp==={}, list[[1]]*0., temp ]
];


(*medianQuietPixelsShortC=
Compile[{{pixelList, _Real, 2}},
	pixelMedian=Median /@ Transpose[pixelList];
	pixelsDist2=(Plus@@((#-pixelMedian)^2))& /@ pixelList;
	quartileEndIndex=Round[Length[pixelList]/4];
	pixelsOrdering = Ordering[pixelsDist2][[1 ;; quartileEndIndex]];
	Mean /@ Transpose[pixelList[[pixelsOrdering]]],
{{pixelMedian, _Real, 1},{pixelsDist2, _Real, 1},
{pixelsOrdering, _Integer, 1},{quartileEndIndex, _Integer}}
];*)


(* ::Subsubsection::Closed:: *)
(*HHImageThresholdNormalize/HHImageThresholdLinearNormalize*)


HHImageThresholdNormalize[imageData_List/;Depth[imageData]==4, threshold_:0.2]:= 
Map[(tempHHITNNorm = Norm[#]; If[ tempHHITNNorm < threshold, {0,0,0}, #/tempHHITNNorm])&, 
	imageData, 
	{2}
];


HHImageThresholdNormalize[imageData_List/;Depth[imageData]==3, threshold_:0.2]:= 
Map[(tempHHITNNorm = Norm[#]; If[ tempHHITNNorm < threshold, {0,0,0}, #/tempHHITNNorm])&, 
	imageData
];


HHImageThresholdNormalize[imageData_List/;Depth[imageData]==2, threshold_:0.2]:= 
(tempHHITNNorm = Norm[imageData]; If[ tempHHITNNorm < threshold, {0,0,0}, imageData/tempHHITNNorm]);


HHImageThresholdNormalize[image_Image, threshold_:0.2]:= 
Image[ HHImageThresholdNormalize[ ImageData[image], threshold ] ];


HHImageThresholdNormalize[args___]:=Message[HHImageThresholdNormalize::invalidArgs, {args}];


HHImageThresholdLinearNormalize[imageData_List/;Depth[imageData]==4, threshold_:0.5]:= 
Block[{sum},
	Map[(sum = Plus @@ #;
		If[ sum < threshold, {0,0,0}, # / sum * 3])&, 
		imageData, 
		{2}]
];


HHImageThresholdLinearNormalize[image_Image, threshold_:0.2]:= 
	Image[ HHImageThresholdLinearNormalize[ ImageData[image], threshold ] ];


HHImageThresholdLinearNormalize[args___]:=Message[HHImageThresholdLinearNormalize::invalidArgs, {args}];


(* ::Subsubsection::Closed:: *)
(*HHImageThreshold/HHImageThresholdLinear*)


HHImageThreshold[imageData_List/;Depth[imageData]==4, color_List/;Length[color]==3, threshold_:0.2]:= 
	Map[If[Norm[# - color] < threshold, {1,1,1}, {0,0,0}]&, 
		imageData, 
		{2}];


HHImageThreshold[image_Image, color_List/;Length[color]==3, threshold_:0.2]:= 
	Image[ HHImageThreshold[ ImageData[image], color, threshold ] ];


HHImageThreshold[args___]:=Message[HHImageThreshold::invalidArgs, {args}];


HHImageThresholdLinear[imageData_List/;Depth[imageData]==4, color_List/;Length[color]==3, threshold_:0.2]:= 
	Map[If[Sum @@ Abs[# - color] < threshold, {1,1,1}, {0,0,0}]&, 
		imageData, 
		{2}];


HHImageThresholdLinear[image_Image, color_List/;Length[color]==3, threshold_:0.2]:= 
	Image[ HHImageThresholdLinear[ ImageData[image], color, threshold ] ];


HHImageThresholdLinear[args___]:=Message[HHImageThresholdLinear::invalidArgs, {args}];


(* ::Section:: *)
(*Plotting Utility Functions*)


HHColorDirectiveQ[directive_RGBColor ]:= True;
HHColorDirectiveQ[directive_Hue ]:= True;
HHColorDirectiveQ[directive_CMYKColor ]:= True;
HHColorDirectiveQ[directive_GrayLevel ]:= True;

HHColorDirectiveQ[directive_LABColor ]:= True;
HHColorDirectiveQ[directive_LCHColor ]:= True;
HHColorDirectiveQ[directive_LUVColor ]:= True;
HHColorDirectiveQ[directive_XYZColor ]:= True;
HHColorDirectiveQ[directive_Opacity/;Length[directive]==2 ]:= True;

HHColorDirectiveQ[directive_ ]:= False;


HHColorDirectiveQ[args___] := Message[HHColorDirectiveQ::invalidArgs, {args}];


HHPlotStyleTable[ plotStyle_List ]:=
	MapThread[ HHPlotStyleTableImpl[#1, #2]&, {plotStyle, Range[Length[plotStyle]]}];

HHPlotStyleTable[plotStyle_List, {count_}]:=
	HHPlotStyleTable[plotStyle[[ ;; Min[Length[plotStyle], count]]]];
	
HHPlotStyleTable[Automatic, {count_} ]:= HHColorData /@ Range[count];
HHPlotStyleTable[plotStyle_, {count_} ]:= HHPlotStyleTableImpl[plotStyle, #]& /@ Range[ count ];


HHPlotStyleTable[args___] := Message[HHPlotStyleTable::invalidArgs, {args}];


HHPlotStyleTableImpl[plotStyle_Directive, number_ ]:=
Block[{tempColor, tempNoncolor},
	tempColor = Select[List@@plotStyle, HHColorDirectiveQ];
	tempNoncolor = Select[List@@plotStyle, !HHColorDirectiveQ[#]&];
	If[ Length[ tempColor ] >0,
		plotStyle,
		Directive@@Flatten[Join[ {HHColorData[number]}, tempNoncolor ]]
	]
];


HHPlotStyleTableImpl[plotStyle_, number_ ]:=
If[HHColorDirectiveQ[plotStyle], 
	plotStyle,
	Directive[ HHColorData[number], plotStyle]
];


HHPlotStyleTableImpl[args___] := Message[HHPlotStyleTableImpl::invalidArgs, {args}];


(* ::Subsection:: *)
(*HHColorData*)


HHColorData[ count_Integer, opts: OptionsPattern[] ]:=
Module[{optColorData},
	optColorData = OptionValue[HHOptColorData];
	If[optColorData == "ColorBlindnessSafe", optColorData=HHColorData["ColorBlindnessSafe"]];
	HHTakeCyclical[ optColorData, count ]
];


HHColorData[ count_Integer, name_String, opts: OptionsPattern[] ]:=
Module[{(*optColorData*)},
	(*optColorData = OptionValue[HHOptColorData];*)
	Switch[name,
		"ColorBlindnessSafe",  HHTakeCyclical[ HHColorData["ColorBlindnessSafe"], count ],
		_, HHTakeCyclical[ HHColorData[], count ]
	]
];


HHColorData[ counts_List, opts: OptionsPattern[] ]:=
	HHColorData[#, opts]& /@ counts;


HHColorData[ counts_List, name_String, opts: OptionsPattern[] ]:=
	HHColorData[#, name, opts]& /@ counts;


HHColorData[ opts: OptionsPattern[] ]:= ColorData[97, "ColorList"];


HHColorData[ "ColorBlindnessSafe" ] := Map[(#/255.)&, {
	RGBColor[0,0,0], RGBColor[0,73,73], RGBColor[0,146,146],
	RGBColor[255,109,182], RGBColor[255,182,119], RGBColor[73,0,146],
	RGBColor[0,109,219], RGBColor[82,109,255], RGBColor[109,182,255],
	RGBColor[182,219,255], RGBColor[146,0,0], RGBColor[146,73,0],
	RGBColor[219,209,0], RGBColor[36,255,36], RGBColor[255*0.5,255*0.5,109*0.5]
}, {2}];


HHColorData[args___] := Message[HHColorData::invalidArgs, {args}];


(* ::Section:: *)
(*Ending*)


End[];

EndPackage[];


(* ::Section::Closed:: *)
(*Bak*)


(*HHImageMeanSubtractedAdjusted[x:{__Image}]:=
Block[{tempImageData,tempMean},
	tempImageData=ImageData /@ x;
	If[ Length[Union[  Dimensions/@tempImageData ]]!=1,
		Message[HHImageMeanSubtractedAdjusted::dimensionsMustBeSame];,
		tempMean=Mean[tempImageData];
		ImageAdjust/@(Image/@((#-tempMean)& /@ tempImageData))
	]
];
HHImageMeanSubtractedAdjusted::dimensionsMustBeSame = "Input list of Image objects must all have the same dimensions and color depths!";

HHImageMeanSubtractedAdjusted[args___]:=Message[HHImageMean::invalidArgs, {args}];*)


(*medianQuietPixelsShort[pixels_List]:=
Block[{pixelMedian, pixelsDist2, pixelsOrdering, pixelsSortedThresholded(*, pixelCutoff*)},
	pixelMedian=Median /@ Transpose[pixels];
	(*pixelsDist=EuclideanDistance[#, pixelMedian]& /@ pixels;*)
	pixelsDist2=(Plus@@((#-pixelMedian)^2.))& /@ pixels;
	pixelsOrdering = Ordering[pixelsDist2][[1 ;; Round[Length[pixels]/4]]];
	Mean /@ Transpose[pixels[[pixelsOrdering]]]

(*	pixelMedian=Mean[pixelsSortedThresholded];
	pixelsDist2=(Plus@@((#-pixelMedian)^2))& /@ pixels;
	pixelsSortedThresholded=SortBy[Transpose[{pixelsDist2, pixels}], First];*)
(*	{Mean[pixelsSortedThresholded[[All, 2]]], pixelsSortedThresholded[[-1, 1]]}*)
];*)
(*simpleDistance[a_,b_]:=Plus@@(Abs /@ (a-b));
simplePixelCluster[pixelList_List/;Length[pixelList]==1, absThreshold_]:= pixelList[[1]];
simplePixelCluster[pixelList_, absThreshold_]:=
	Block[{clusters,clusterCount,innerBreak,innerRet},
		clusters={pixelList[[1]]}; clusterCount={1};
		
	(*For each pixel in the list past index 2*)
	Do[
		innerBreak=False;innerRet=0;

		(*For each cluster index*)
		Do[
			If[ simpleDistance[clusters[[n]],pixel]<=absThreshold,
					clusters=ReplacePart[clusters, n->  Mean[{clusters[[n]],pixel}]];
					clusterCount=ReplacePart[clusterCount,n->  (clusterCount[[n]]+1)];
					Break[],
				AppendTo[clusters,pixel]; AppendTo[clusterCount,1]
			],
		{n,Length[clusters]}
		],

	{pixel,pixelList[[2 ;; ]]}
	];

	SortBy[Transpose[{clusterCount,clusters}], First][[-1,2]]

];*)


(* ::Subsubsection::Closed:: *)
(*BAK: HHListLinePlotStack (Old Signature)*)


(*HHStackLists[traces_ /; Depth[traces]==3, opts:OptionsPattern[]] :=
Block[{tempTraces, temp, 
		opHHOptBaselineCorrection, baselineSubtractFactors, 
		opHHOptStack, stackAddFactors, stackFactorsCumulated},
	
	Message[ HHStackLists::deprecatedSignature ];

	tempTraces = traces;

	opHHOptBaselineCorrection = OptionValue[HHOptBaselineCorrection];

	(*====================*)
	(* Baseline subtraction *)
	(*====================*)
	baselineSubtractFactors = Switch[opHHOptBaselineCorrection,
		None, None,
		f_/;HHFunctionQ[f],    opHHOptBaselineCorrection/@traces, (*This covers specifications such as Mean and First or (#[[1]])& *)
		f_/;(Quiet[temp=f[#]&/@traces]; And@@(NumericQ /@ temp) ), 
					temp, (*This covers specifications such as Mean and First or (#[[1]])& *)
		_, Message[ HHStackLists::invalidOptionValue, "HHOptBaselineCorrection", ToString[opHHOptBaselineCorrection]]; 
		   None
	];
	If[ baselineSubtractFactors =!= None,
		tempTraces = tempTraces - baselineSubtractFactors
	];
	
	(*====================*)
	(* Stack incrementation *)
	(*====================*)
	opHHOptStack = OptionValue[HHOptStack];
	stackAddFactors = Switch[ opHHOptStack,
		None,                    None,
		Automatic,               Table[ Quantile[ (# - Min[#])&[ Flatten[traces] ], 0.95]*1.1, {Length[traces]}], (*- Subtract@@MinMax[ Flatten[traces] ]*)
		x_/;NumericQ[x],         Table[ opHHOptStack, {Length[traces]}],
		f_/;HHFunctionQ[f],      Table[ opHHOptStack[ Flatten[traces] ], {Length[traces]}], 
										(*This covers specifications such as Mean[#]& or (#[[1]])& *)
		f_/;(Quiet[temp=f[ Flatten[traces]]]; And@@(NumericQ /@ temp) ), 
								  temp, (*This covers specifications such as Mean and First *)
		_, Message[ HHStackLists::invalidOptionValue, "HHOptStack", ToString[opHHOptStack]]; Table[0, {Length[traces]}]
	];
	stackFactorsCumulated = FoldList[Plus, 0, stackAddFactors];
	$ActualStackRange = {- stackAddFactors[[1]], stackFactorsCumulated[[ -1 ]]};
	tempTraces + stackFactorsCumulated[[ ;; -2]](*FoldList[Plus, 0, stackAddFactors[[ ;; -2]] ]*)
						(*last stack add factor is not used here... nothing to stack on top*)

   ];*)


(*(*Stack lists of {{t1, x1}, {t2, x2}, ...} pairs in the second dimension*)
HHStackLists[
	traces_ /; (Depth[traces]==4 && Union[(Dimensions /@ traces)[[All, 2]]]=={2}), 
	opts:OptionsPattern[]] :=

Block[{tempTimes, tempTraces},

	Message[ HHStackLists::deprecatedSignature ];

	tempTimes = traces[[All, All, 1]];
	tempTraces = traces[[All, All, 2]];

	tempTraces =  HHStackLists[tempTraces, opts];

	Transpose /@ MapThread[{#1, #2}&, {tempTimes, tempTraces}]
];*)


(*HHListLinePlotStack[
	traces_/;(Depth[traces]==3 || (Depth[traces]==4 && Union[(Dimensions /@ traces)[[All, 2]]]=={2})), 
	opts:OptionsPattern[]
]:=
Block[{tempData,tempPlotRange},

	Message[ HHStackLists::deprecatedSignature ];

	
	tempData = HHStackLists[traces, Sequence@@FilterRules[{opts}, Options[HHStackLists]]];
	tempPlotRange = If[ OptionValue[HHPlotRangeClipping] === Automatic, {PlotRange->{All, $ActualStackRange}},{}];
		

	ListLinePlot[tempData,
		Sequence@@HHJoinOptionLists[ ListLinePlot, {tempPlotRange}, {opts}, HHListLinePlotStack$UniqueOptions ]
	]
];*)


(*HHListLinePlotStack[args___] := Message[HHListLinePlotStack::invalidArgs, {args}];*)
