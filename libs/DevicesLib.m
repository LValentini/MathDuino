(* ::Package:: *)

(* Prende il nome e l'ip inseriti dall'utente tramite interfaccia grafica e trasforma l'ip in stringa *)

getIPFromGui[expr_] := Module[
{
	first = expr@PropertyValue[{"firstIp", "selectedItem"}],
	second = expr@PropertyValue[{"secondIp", "selectedItem"}],
	third=expr@PropertyValue[{"thirdIp", "selectedItem"}],
	fourth =expr@PropertyValue[{"fourthIp", "selectedItem"}],
	name = expr@PropertyValue[{"arduinoName", "text"}],
	ipString = ""
},
	ipString = StringJoin[Riffle[{ToString[first],ToString[second],ToString[third],ToString[fourth]},"."]];
	If [name == "", name = Null];
	Return[{ipString, name}];
]



(* Controlla se il device \[EGrave] online e raggiungibile e ritorna il suo MAC Address *)

isArduinoOnline[ip_] :=
	Module[{
		isOnline= Null
	},
	isOnline = pingDataCollector[ip];
	Return[isOnline];
]



(* Aggiorna la lista dei device dell'interfaccia grafica *)

setDevicesList[expr_] :=
Module[{
localDevicesList = getDevicesList[]},
expr@SetPropertyValue[{"arduinoList", "items"}, localDevicesList];
]



(* Aggiunge un device al database e aggiorna la lista *)

addDevice[ip_, name_] :=Module[{},
	addDataCollector[conn, pingDataCollector[ip], ip, name];
]




deleteDevice[expr_] := Module[{selectedValue, selectedArduinoName, selectedArduinoMac},
	selectedValue = expr@PropertyValue[{"arduinoList","selectedValues"}];
	selectedArduinoName = StringSplit[selectedValue[[1]], "("][[1]];
	selectedArduinoMac = getDataCollectorMacByName[conn, selectedArduinoName];
	deleteDataCollector[conn, selectedArduinoMac];
	setDevicesList[expr];	
]



(* Richiede la lista dei device al database, controlla se sono raggiungibili e aggiorna il loro stato nel database, prima di ritornare la lista aggiornata *) 

getDevicesList[] :=
Module[{
	arduinoList = getCleanDCList[conn],
	n = 1,
	availability = 0
	},
	While[n <= Length[arduinoList], 
		name = arduinoList[[n]];
		If [ pingDataCollector[getDataCollectorIpByName[conn,name]] == Null, availability=0, availability=1, availability = 1]
			setDCAvailability[conn, name, availability];
	n++];
	Return[getDCList[conn]];
]



(* Lancia le operazioni di monitoraggio per il device selezionato. Aggiorna la status bar, lancia lo scheduled task per il monitoraggio in real time, aggiorna i grafici dello storico e lancia la geolocalizzazione su Google Maps *)

deviceSelected[expr_] :=
	Module[{selectedValue, status},
		selectedValue = expr@PropertyValue[{"arduinoList","selectedValues"}];
		If[Length[selectedValue] == 0, selectedValue = expr@PropertyValue[{"arduinoList", "items"}]];
		selectedArduinoName = StringSplit[selectedValue[[1]], "("][[1]];
		selectedArduino = getDataCollectorIpByName[conn, selectedArduinoName];
		periodSelected[expr];
		status = StringSplit[selectedValue[[1]], "("][[2]];
		status = StringSplit[status, ")"][[1]];
		resetGaugeTab[expr];
		If [status == "Up",
			If [pingDataCollector[selectedArduino] == Null, Print[""], Print[""],
				getValuesScheduled[expr];
				getMap[expr, selectedArduino];
			];
		,
			expr@SetPropertyValue[{"smallMap", "data"},ExportString[Graphics[Text[Style["Not available", Large]], ImageSize->{335, 335}, Background->RGBColor[{0.9294,0.9294,0.9294}]],"PNG"]];
			expr@SetPropertyValue[{"largeMap", "data"},ExportString[Graphics[Text[Style["Not available", Large]], ImageSize->{335, 335}, Background->RGBColor[{0.9294,0.9294,0.9294}]],"PNG"]];
		];
		expr@SetPropertyValue[{"windowFrame", "title"}, "MathDuino --- Selected Device: " <> selectedArduinoName <> " --- Status: " <> status];
	]
		



(* Contatta il device e se \[EGrave] disponibile richiede i valori dei sensori e li scrive sul database *)

queryDataCollector[name_] := Module[{light, temp, pressure, humidity, ip, mac, values}, 
	ip = getDataCollectorIpByName[conn,name];
	mac = pingDataCollector[ip];
	If[mac == Null, 
		Print[""], Print[""], 
		values = getDataCollectorXML[ip];
		light = getSensorValue["light", getSensorsDataArray[values]];
		temp = getSensorValue["temp", getSensorsDataArray[values]];
		pressure = getSensorValue["pressure", getSensorsDataArray[getDataCollectorXML[ip]]];
		humidity = getSensorValue["humidity", getSensorsDataArray[getDataCollectorXML[ip]]];

		addDataCollectorEntry[conn, mac, "light", light];
		addDataCollectorEntry[conn, mac, "temp", temp];
		addDataCollectorEntry[conn, mac, "pressure", pressure];
		addDataCollectorEntry[conn, mac, "umidity", humidity];
	];
];


(* Richiede la posizione al device e lo geolocalizza tramite Google Maps tramite il download delle immagini corrispondenti *)

getMap[expr_, deviceIP_] := Module[{xml, apslist, jsonReq, googleLocQueryOut, latlng, smallMap, largeMap},
	xml = getDataCollectorXML[deviceIP];
	apslist = addApsToList[xml];
	jsonReq = generateJSON[apslist];
	googleLocQueryOut = googleLocQuery[jsonReq, "AIzaSyDxL_zhnKoTUjvFl_iw6LgAaWKFugzPjBA"]; (*aggiungere A *)
	If[googleLocQueryOut == Null, 
		expr@SetPropertyValue[{"smallMap", "data"},ExportString[Graphics[Text[Style["Not available", Large]], ImageSize->{335, 335}, Background->RGBColor[{0.9294,0.9294,0.9294}]],"PNG"]];
		expr@SetPropertyValue[{"largeMap", "data"},ExportString[Graphics[Text[Style["Not available", Large]], ImageSize->{335, 335}, Background->RGBColor[{0.9294,0.9294,0.9294}]],"PNG"]];
		,
		latlng = getLanLngFromGLR[googleLocQueryOut];
		smallMap = getSmallMap[latlng[[1]], latlng[[2]], 18];
		largeMap = getLargeMap[latlng[[1]], latlng[[2]], 16];
		expr@SetPropertyValue[{"smallMap", "data"}, ExportString[ImageResize[smallMap, {335, 335}], "PNG"]];
		expr@SetPropertyValue[{"largeMap", "data"}, ExportString[ImageResize[largeMap, {335, 335}], "PNG"]];
		,
		latlng = getLanLngFromGLR[googleLocQueryOut];
		smallMap = getSmallMap[latlng[[1]], latlng[[2]], 18];
		largeMap = getLargeMap[latlng[[1]], latlng[[2]], 16];
		expr@SetPropertyValue[{"smallMap", "data"}, ExportString[ImageResize[smallMap, {335, 335}], "PNG"]];
		expr@SetPropertyValue[{"largeMap", "data"}, ExportString[ImageResize[largeMap, {335, 335}], "PNG"]];
	]
];
