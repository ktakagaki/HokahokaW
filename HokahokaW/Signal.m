(* ::Package:: *)

(* Wolfram Language package *)


BeginPackage["HokahokaW`Signal`",{"HokahokaW`"}];


HHStandardDeviationMedianEstimate::usage="Makes a standard deviation estimate based on medians (less sensitive to outliers).";


HHThreshold::usage="Takes a trace and returns timepoints at which it crosses threshold.";

HHThresholdLevel::usage="What level to use for threshold.";
HHThresholdDirection::usage="What direction to use for threshold.";

Options[HHThreshold]={HHThresholdLevel->Automatic, HHThresholdDirection->Automatic};


HHPulseTrainDetect::usage="Detect a train.";

HHPulseTrainLengthMinimum::usage="Number of different pulse lengths to accept.";
HHPulseTrainBlackout::usage="Number of samples from beginning (or {beginning, end}) to reject for thresholding.";

Options[HHPulseTrainDetect]={HHPulseTrainLengthMinimum->0, HHPulseTrainBlackout->None};


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*HHStandardDeviationMedianEstimate*)


HHStandardDeviationMedianEstimate[data_List] := MedianDeviation[data]/0.6745;


(* ::Subsection:: *)
(*HHThreshold*)


HHThreshold[data_List, opts:OptionsPattern[]] :=  HHThreshold[data, Automatic, opts];


HHThreshold[data_List, Automatic, opts:OptionsPattern[]] := HHThresholdImplSimpleAutoMedian[data];

HHThreshold[data_List, thresholdValue_, opts:OptionsPattern[]] := HHThresholdImplSimple[data, thresholdValue];

HHThreshold[args___]:=Message[HHThreshold::invalidArgs,{args}];


HHThresholdImplSimpleAutoMedian[data_List]:=
Module[{tempData},
	HHThresholdImplSimple[
		tempData=data-Median[data],
		HHStandardDeviationMedianEstimate[tempData]*4
	]
];


HHThresholdImplSimple[data_List, threshValue_]:=
Module[{tempRes},
	tempRes=FoldList[Plus, 1, Length /@ Split[If[# < threshValue, 0, 1]& /@ data]];
	tempRes=If[ Length[tempRes]>2 && data[[1]]>threshValue, tempRes[[3 ;; ]], tempRes[[2 ;; ]] ];
	tempRes=If[ Length[tempRes]>2 && data[[-1]]>threshValue, tempRes[[ ;; -3]], tempRes[[ ;; -2]] ];
	tempRes	
];


(* ::Subsection:: *)
(*HHPulseTrainDetect*)


HHPulseTrainDetect[data_List, opts:OptionsPattern[]] := HHPulseTrainDetect[data, Automatic, opts]


HHPulseTrainDetect[data_List, thresholdValue_, opts:OptionsPattern[]] := 
Module[{threshed, tempRes,
		optBlackout,optPulseMin},

	threshed=HHThreshold[data, thresholdValue];
	If[ Length[threshed]==0, Message[HHPulseTrainDetect::noThresholdCrosses]];
	If[ !EvenQ[Length[threshed]], Message[HHPulseTrainDetect::oddThresholdCounts]];
	tempRes= (#-{0,1})& /@ Partition[threshed,2];
	
	optBlackout = OptionValue[HHPulseTrainBlackout];
	If[ Head[optBlackout] === List && Length[optBlackout]==2,
		tempRes = Select[ tempRes, (#[[2]] < optBlackout[[1]] ||  optBlackout[[2]] < #[[1]] )&],
		If[ Head[optBlackout]===Integer || Head[optBlackout] ===Real,
			tempRes = Select[ tempRes, (#[[2]]>optBlackout)&]
	]];
	
	optPulseMin = OptionValue[HHPulseTrainLengthMinimum];
	If[ (Head[optPulseMin] === Integer || Head[optPulseMin] === Real),
		If[ optPulseMin >= 1, tempRes = Select[ tempRes, (#[[2]]-#[[1]] >= optPulseMin)& ] ]
	];

	tempRes
	
];

HHPulseTrainDetect::noThresholdCrosses="No threshold crosses detected";
HHPulseTrainDetect::oddThresholdCounts="Some error, odd number of thresholds should not occur with HHThreshold[]";

HHPulseTrainDetect[args___]:=Message[HHPulseTrainDetect::invalidArgs,{args}];



(* ::Section:: *)
(*Ending*)


End[];

EndPackage[];


(* ::Section:: *)
(*Bak*)
