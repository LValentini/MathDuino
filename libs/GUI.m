(* ::Package:: *)

expr = 

Widget["Frame", {
"title" -> "MathDuino --- Device Selected: None --- Status: None",
Widget["Panel", {
WidgetFill[],

WidgetGroup[{

(*NON TABBED*)
WidgetGroup[{
	(*Widget["Label", {"text" -> "Selected device: "}, Name->"selectedArduino"], *)
	Widget["Button",{"text"->"Connect", BindEvent["Action", Script[deviceSelected[MathDuino]]]}, Name->"ConnectButton"],
	Widget["Button", {"text"->"Update", BindEvent["Action", Script[setDevicesList[MathDuino]]]}, Name->"UpdateButton"],

	Widget["ScrollPane", {
		"viewportView"->Widget["List", {
		"visibleRowCount"->10, "items"-> getDevicesList[]}, Name->"arduinoList"]
	}, WidgetLayout->{"Alignment"->Center}],
	Widget["Button", {"text"->"Delete", BindEvent["Action", Script[deleteDevice[MathDuino]]]}, Name->"DeleteButton"],
	Widget["Button", {"text"->"Add Device", BindEvent["Action", Script[addDevice[getIPFromGui[MathDuino][[1]], getIPFromGui[MathDuino][[2]]]]]}, Name->"AddButton"],
	WidgetGroup[{
		Widget["Label", {"text"->"  Name: "}],
		Widget["TextField", {"text"->""}, Name->"arduinoName"]
		}, WidgetLayout->Row],
	WidgetGroup[{
		Widget["Label", {"text"->"  IP: "}],
		Widget["ComboBox", {"items"->Script[Range[0, 254]]}, Name->"firstIp"],
		Widget["ComboBox", {"items"->Script[Range[0, 254]]}, Name->"secondIp"],
		Widget["ComboBox", {"items"->Script[Range[0, 254]]}, Name->"thirdIp"],
		Widget["ComboBox", {"items"->Script[Range[0, 254]]}, Name->"fourthIp"]
	}, WidgetLayout->Row]
}, WidgetLayout->Column],

WidgetGroup[{

(*TAB*)
	WidgetGroup[{

(*FIRST TAB*)
		WidgetGroup[{
			Widget["ImageLabel", {"data"->Script[ExportString[Graphics[Text[Style["Not available", Large]], ImageSize->{335, 335}, Background->RGBColor[{0.9294,0.9294,0.9294}]] , "PNG"]]}, WidgetLayout->{"Border"->"Location"}, Name->"largeMap"],
			Widget["ImageLabel", {"data"->Script[ExportString[Graphics[Text[Style["Not available", Large]], ImageSize->{335, 335}, Background->RGBColor[{0.9294,0.9294,0.9294}]] , "PNG"]]}, WidgetLayout->{"Border"->"Detailed location"}, Name->"smallMap"]
		}, WidgetLayout->Row],


(*SECOND TAB*)
		WidgetGroup[{
			WidgetSpace[20],
			WidgetGroup[{
				WidgetSpace[110],
				WidgetGroup[{
					Widget["Label", {"text"->"NA"}, Name->"temp"],
					Widget["ImageLabel", {"data"->Script[ExportString[AngularGauge[-30, {-30, 50}, PerformanceGoal->"Speed", GaugeLabels->Automatic, Background->RGBColor[{0.9294,0.9294,0.9294}]], "PNG"]]
					}, Name->"tempGauge"]
				}, WidgetLayout->{"Border"->"Temperature"}],
				WidgetSpace[80],
			WidgetGroup[{
				Widget["Label", {"text"->"NA"}, Name->"light"],
				Widget["ImageLabel", {"data"->Script[ExportString[AngularGauge[0, {0, 1000}, PerformanceGoal->"Speed", GaugeLabels->Automatic, Background->RGBColor[{0.9294,0.9294,0.9294}]], "PNG"]]
				}, Name->"lightGauge"]
				}, WidgetLayout->{"Border"->"Luminosity"}]
			}, WidgetLayout->{"Alignment"->Center}],
			WidgetSpace[20],

			WidgetGroup[{
				WidgetSpace[110],
				WidgetGroup[{
					Widget["Label", {"text"->"NA"}, Name->"pressure"],
					Widget["ImageLabel", {"data"->Script[ExportString[AngularGauge[950, {950, 1050}, GaugeLabels->Automatic, PerformanceGoal->"Speed", Background->RGBColor[{0.9294,0.9294,0.9294}]], "PNG"]]
					}, Name->"pressureGauge"]
				}, WidgetLayout->{"Border"->"Pressure"}],
				WidgetSpace[80],
				WidgetGroup[{
					Widget["Label", {"text"->"NA"}, Name->"humidity"],
					Widget["ImageLabel", {"data"->Script[ExportString[AngularGauge[0, {0, 100}, GaugeLabels->Automatic, PerformanceGoal->"Speed", Background->RGBColor[{0.9294,0.9294,0.9294}]], "PNG"]]
					}, Name->"humidityGauge"]
				}, WidgetLayout->{"Border"->"Humidity"}]
			}, WidgetLayout->{"Alignment"->Center}]
		}],


(*THIRD TAB*)
		WidgetGroup[{
			WidgetGroup[{
				WidgetGroup[{

					Widget["ImageLabel", {"data"->Script[ExportString[Graphics[Text[Style["Not available", Large]], ImageSize->{335, 200}, Background->RGBColor[{0.9294,0.9294,0.9294}]] , "GIF"]]
					}, Name->"tempHistogram", WidgetLayout->{"Border"->"Temperature"}],
			
					Widget["ImageLabel", {"data"->Script[ExportString[Graphics[Text[Style["Not available", Large]], ImageSize->{335, 200}, Background->RGBColor[{0.9294,0.9294,0.9294}]] , "GIF"]]
					}, Name->"lightHistogram", WidgetLayout->{"Border"->"Luminosity"}]

					}],


				WidgetGroup[{

					Widget["ImageLabel", {"data"->Script[ExportString[Graphics[Text[Style["Not available", Large]], ImageSize->{335, 200}, Background->RGBColor[{0.9294,0.9294,0.9294}]] , "GIF"]]
					}, Name->"pressureHistogram", WidgetLayout->{"Border"->"Pressure"}],


					Widget["ImageLabel", {"data"->Script[ExportString[Graphics[Text[Style["Not available", Large]],ImageSize->{335, 200}, Background->RGBColor[{0.9294,0.9294,0.9294}]] , "GIF"]]
					}, Name->"humidityHistogram", WidgetLayout->{"Border"->"Humidity"}]

				}]
			}],

			Widget["ComboBox", {"items"->Script[{"Last 24 Hours", "Last Week", "Last Month", "Last Year"}],  BindEvent["action", Script[periodSelected[MathDuino]]]
			}, Name->"totalCombo"]
		}]

	}, WidgetLayout->{"Grouping"-> {Tabs, Top, {"Device Location", "Real Time Monitor", "Historical Statistics"}}}]


}, WidgetLayout->Column]
}, WidgetLayout->Row]

}]
}, Name -> "windowFrame"];



