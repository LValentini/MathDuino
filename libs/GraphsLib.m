(* ::Package:: *)

periodSelected[expr_] :=
Module[{
tempValues = "", 
selectedValue =  expr@PropertyValue[{"totalCombo", "selectedItem"}]
},

(*expr@SetPropertyValue[{"tempCombo", "selectedItem"}, selectedValue];
expr@SetPropertyValue[{"lightCombo", "selectedItem"}, selectedValue];
expr@SetPropertyValue[{"humidityCombo", "selectedItem"}, selectedValue];
expr@SetPropertyValue[{"pressureCombo", "selectedItem"}, selectedValue]; *)

(*{"Last 24 Hours", "Last Week", "Last Month", "Last Year"}*)
If[selectedValue == "Last 24 Hours",
setDailyGraph[expr, "humidityHistogram", "umidity"];
setDailyGraph[expr, "pressureHistogram", "pressure"];
setDailyGraph[expr, "tempHistogram","temp"];
setDailyGraph[expr, "lightHistogram", "light"];
];

If[selectedValue == "Last Week",
setWeeklyGraph[expr, "humidityHistogram", "umidity"];
setWeeklyGraph[expr, "pressureHistogram", "pressure"];
setWeeklyGraph[expr, "tempHistogram","temp"];
setWeeklyGraph[expr, "lightHistogram", "light"];
];

If[selectedValue == "Last Month",
setMonthlyGraph[expr, "humidityHistogram", "umidity"];
setMonthlyGraph[expr, "pressureHistogram", "pressure"];
setMonthlyGraph[expr, "tempHistogram","temp"];
setMonthlyGraph[expr, "lightHistogram", "light"];
];

If[selectedValue == "Last Year",
setYearlyGraph[expr, "humidityHistogram", "umidity"];
setYearlyGraph[expr, "pressureHistogram", "pressure"];
setYearlyGraph[expr, "tempHistogram","temp"];
setYearlyGraph[expr, "lightHistogram", "light"];
];

]


getDCNameForGraphLoading[name_] := Module[{retval},
	retval=StringReplace[getDataCollectorMacByName[conn,name],":" -> "-"];
	Return[retval];
]

setDailyGraph[expr_, type_, sens_] := Module[{},
expr@SetPropertyValue[{type, "data"},
ExportString[
Import[FileNameJoin[{nbdir,"tmpgraph", getDCNameForGraphLoading[selectedArduinoName] <> "_" <> sens <> "_" <> "day.png"} ]],"PNG"]
];
]

setWeeklyGraph[expr_, type_, sens_] := Module[{},
expr@SetPropertyValue[{type, "data"}, 
ExportString[
Import[FileNameJoin[{ nbdir,"tmpgraph", getDCNameForGraphLoading[selectedArduinoName] <> "_" <> sens <> "_" <> "week.png" }]], "PNG"]
];
]

setMonthlyGraph[expr_, type_, sens_] := Module[{},
expr@SetPropertyValue[{type, "data"}, 
ExportString[
Import[FileNameJoin[{nbdir, "tmpgraph", getDCNameForGraphLoading[selectedArduinoName] <> "_" <> sens <> "_" <> "month.png" }]], "PNG"]
]
];

setYearlyGraph[expr_, type_, sens_] := Module[{},
expr@SetPropertyValue[{type, "data"}, 
ExportString[
Import[FileNameJoin[{nbdir, "tmpgraph", getDCNameForGraphLoading[selectedArduinoName] <> "_" <> sens <> "_" <> "year.png" }]],"PNG"]
];
]

prepareGraph[timerange_] := Quiet[Module[{
dclist = getCleanDCList[conn],
dcindex=1,
sensors = {"temp", "light", "umidity", "pressure"},
sensindex = 1,
plotData, plotOut,
dcid
},

While[dcindex <= Length[dclist], 
	sensindex=1;
	dcid = StringReplace[getDataCollectorMacByName[conn,dclist[[dcindex]]],{":"->"-"}];
	While [sensindex <= Length[sensors],
		If[Or[timerange == "day", timerange=="all"],
			plotData = getLast24HoursData[conn, sensors[[sensindex]], getDataCollectorMacByName[conn,dclist[[dcindex]]]];
			If[Length[plotData] !=  0, 
				plotOut = DateListPlot[plotData,Joined->True, ImageSize->{335, 200}, Background->RGBColor[{0.9294,0.9294,0.9294}]],
				plotOut = Graphics[Text[Style["Not available", Large]], ImageSize->{335, 200},Background->RGBColor[{0.9294,0.9294,0.9294}]]
			];
			Export[
			FileNameJoin[{NotebookDirectory[], "tmpgraph", dcid <> "_" <> sensors[[sensindex]] <> "_day.png"}],
			plotOut,
			"PNG"
		];];
		If[Or[timerange =="all", timerange =="all_minus_day"],
			plotData = getLastWeekData[conn, sensors[[sensindex]], getDataCollectorMacByName[conn,dclist[[dcindex]]]];
			If[Length[plotData[[1]]] != 0,
				plotOut = DateListPlot[plotData,Joined->True, ImageSize->{335, 200}, Background->RGBColor[{0.9294,0.9294,0.9294}]],
				plotOut = Graphics[Text[Style["Not available", Large]], ImageSize->{335, 200},Background->RGBColor[{0.9294,0.9294,0.9294}]] 	
			];
			Export[
			FileNameJoin[{NotebookDirectory[], "tmpgraph",  dcid <> "_"<> sensors[[sensindex]] <>"_week.png"}],
			plotOut,
			"PNG"
		];];
		If[Or[timerange =="all", timerange =="all_minus_day"],
			plotData = getLastMonthData[conn, sensors[[sensindex]], getDataCollectorMacByName[conn,dclist[[dcindex]]]];
			If[Length[plotData[[1]]] != 0,
				plotOut = DateListPlot[plotData,Joined->True, ImageSize->{335, 200}, Background->RGBColor[{0.9294,0.9294,0.9294}]],
				plotOut = Graphics[Text[Style["Not available", Large]], ImageSize->{335, 200},Background->RGBColor[{0.9294,0.9294,0.9294}]]
			];
			Export[
			FileNameJoin[{NotebookDirectory[], "tmpgraph", dcid <> "_"<> sensors[[sensindex]] <>"_month.png"}],
			plotOut,
			"PNG"
		];];
		If[Or[timerange =="all", timerange =="all_minus_day"],
			plotData = getLastYearData[conn, sensors[[sensindex]], getDataCollectorMacByName[conn,dclist[[dcindex]]]];
			If[Length[plotData[[1]]] != 0,
				plotOut = DateListPlot[plotData,Joined->True, ImageSize->{335, 200}, Background->RGBColor[{0.9294,0.9294,0.9294}]],
				plotOut = Graphics[Text[Style["Not available", Large]], ImageSize->{335, 200},Background->RGBColor[{0.9294,0.9294,0.9294}]]
			];
			Export[
			FileNameJoin[{NotebookDirectory[], "tmpgraph", dcid <> "_"<> sensors[[sensindex]] <>"_year.png"}],
			plotOut,
			"PNG"
		];];
		sensindex++;
	]
	dcindex++;
]
]] 

