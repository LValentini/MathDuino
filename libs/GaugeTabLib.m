(* ::Package:: *)

(* Real Time Gauge functions: *)

resetGaugeTab[expr_] := Module[{},
		Quiet[RemoveScheduledTask[t]];
		temp = "NA";
		light = "NA";
		humidity = "NA";
		pressure = "NA";
		setTemperatureGauge[expr];
		setLuminosityGauge[expr];
		setHumidityGauge[expr];
		setPressureGauge[expr];
		setTemperatureLabel[expr];
		setLuminosityLabel[expr];
		setHumidityLabel[expr];
		setPressureLabel[expr];
]
	
getValuesScheduled[expr_] := Module[{},
   t = CreateScheduledTask[
     If [pingDataCollector[getDataCollectorIpByName[conn, selectedArduinoName]] == Null, Print[""], Print[""],
	    values = getDataCollectorXML[selectedArduino];
		temp = getSensorValue["temp", getSensorsDataArray[values]];
		light = getSensorValue["light", getSensorsDataArray[values]];
		humidity = getSensorValue["humidity", getSensorsDataArray[values]];
		pressure = getSensorValue["pressure", getSensorsDataArray[values]];
		setTemperatureGauge[expr];
		setLuminosityGauge[expr];
		setHumidityGauge[expr];
		setPressureGauge[expr];
		setTemperatureLabel[expr];
		setLuminosityLabel[expr];
		setHumidityLabel[expr];
		setPressureLabel[expr];
       ], 3
	];
   StartScheduledTask[t];
   ]

setTemperatureGauge[expr_] :=
 Module[{
   localTemperature = temp},
  expr@SetPropertyValue[{"tempGauge", "data"}, ExportString[AngularGauge[ToExpression[localTemperature], {-30, 50}, GaugeLabels->Automatic, PerformanceGoal->"Speed", Background -> RGBColor[{0.9294, 0.9294, 0.9294}]], "PNG"]];
  ]

setLuminosityGauge[expr_] :=
 Module[{
   localLuminosity = light},
  expr@SetPropertyValue[{"lightGauge", "data"}, ExportString[AngularGauge[ToExpression[localLuminosity], {0, 1000}, GaugeLabels->Automatic, PerformanceGoal->"Speed", Background -> RGBColor[{0.9294, 0.9294, 0.9294}]], "PNG"]];
  ]

setHumidityGauge[expr_] :=
 Module[{
   localHumidity = humidity},
  expr@SetPropertyValue[{"humidityGauge", "data"}, ExportString[AngularGauge[ToExpression[localHumidity], {0, 100}, GaugeLabels->Automatic, PerformanceGoal->"Speed", Background -> RGBColor[{0.9294, 0.9294, 0.9294}]], "PNG"]];
  ]

setPressureGauge[expr_] :=
 Module[{
   localPressure = pressure},
  expr@SetPropertyValue[{"pressureGauge", "data"}, ExportString[AngularGauge[ToExpression[localPressure], {950, 1050}, GaugeLabels->Automatic, PerformanceGoal->"Speed", Background -> RGBColor[{0.9294, 0.9294, 0.9294}]], "PNG"]];
  ]

setTemperatureLabel[expr_] :=
 Module[{
   localTemperature = temp},
  expr@SetPropertyValue[{"temp", "text"}, localTemperature]
  ]

setLuminosityLabel[expr_] :=
 Module[{
   localLuminosity = light},
  expr@SetPropertyValue[{"light", "text"}, localLuminosity]
  ]

setHumidityLabel[expr_] :=
 Module[{
   localHumidity = humidity},
  expr@SetPropertyValue[{"humidity", "text"}, localHumidity]
  ]

setPressureLabel[expr_] :=
 Module[{
   localPressure = pressure},
  expr@SetPropertyValue[{"pressure", "text"}, localPressure]
  ]



(* ::Text:: *)
(**)
