(* ::Package:: *)

(*
si connette al db mathduino (e solo a questo!)
prende in input una lista di stringhe che rappresentano la path assoluta della cartella in cui \[EGrave] contenuto il db
e.g: {"/","home","user","mathduinodbfolder"}
costruisce automagicamente la stringa in base al sistema operativo utilizzato
*)
connectToDb[dbdir_] := Module[{conn},
conn=OpenSQLConnection[ JDBC["hsqldb",FileNameJoin[Append[dbdir, "mathduino"]]], 
	"Name"->"mathduino","Username"->"SA"];
Return[conn ]
]

(*
crea la tabella necessaria a immagazzinare i dati provenienti dai data collectors
*)
createSensorsValuesTable[conn_] := Module[{},
	SQLCreateTable[conn, SQLTable["SensorsValues"],{
	(*SQLColumn["entryno", "DataTypeName" -> "BIGINT", "Nullable"->False],*)
	SQLColumn["date","DataTypeName"->"DATE","Nullable"->False],
	SQLColumn["timestamp", "DataTypeName"-> "TIMESTAMP","Nullable"->False],
	SQLColumn["datacollectorid","DataTypeName"-> "VARCHAR", "DataLength"-> 17,"Nullable"->False],
	SQLColumn["sensorType", "DataTypeName"-> "VARCHAR", "DataLength"-> 20,"Nullable"->False],
	SQLColumn["value", "DataTypeName"->"REAL","Nullable"->False]
	}
	]
]

(*
crea la tabella necessaria a immagazzinare i data collectors gestibili
*)
createDataCollectorsTable[conn_] := Module[{},
	SQLCreateTable[conn, SQLTable["DataCollectors"], {
	SQLColumn["entryno", "DataTypeName" -> "BIGINT", "Nullable"->False],
	SQLColumn["datacollectorid","DataTypeName"-> "VARCHAR", "DataLength"-> 17,"Nullable"->False],
	SQLColumn["datacollectorip","DataTypeName"-> "VARCHAR", "DataLength"-> 15,"Nullable"->False],
	SQLColumn["datacollectorname","DataTypeName"-> "VARCHAR", "DataLength"-> 17,"Nullable"->False],
	SQLColumn["availability", "DataTypeName"->"INT","Nullable"->False]
	}
	]
]

(*
aggiunge un data collector alla tabella DataCollectors
prende in input:
- la connessione al db
- il mac addr del data collector
- l'ip del data collector
- il nome inserito dall'utente (o il simbolo Null se l'utente non ha inserito alcun nome).
Se il mac addr non \[EGrave] fornito alla funzione questa restituisce Null senza far altro
Se il nome non \[EGrave] stato definito dall'utente viene usato il valore di default che \[EGrave] il mac addr del data collector
Se \[EGrave] gi\[AGrave] presente un data collector con l'id fornito di questo viene aggiornato ip e nome
altrimenti viene creata la relativa entry con i dati forniti
di default l'availability del dc appena inserito \[EGrave] posta a 0 (non raggiungibile)
*)
addDataCollector[conn_, dcid_, dcip_, name_] := Module[{entry,dcname=name},
	If[dcid == Null, Return[]];	
	If[name == Null, dcname = dcid];
	If[Length[SQLExecute[conn, "SELECT entryno FROM DataCollectors WHERE datacollectorid=`1`",{dcid}]]==0,
		entry=SQLExecute[conn, "SELECT MAX(entryno) FROM DataCollectors"][[1,1]];
		If[entry==Null, entry=0];
		entry = entry + 1;
		SQLInsert[conn, SQLTable["DataCollectors"],{"entryno","datacollectorid","datacollectorip","datacollectorname","availability"},{entry,dcid,dcip,dcname,1}],
		SQLUpdate[conn, SQLTable["DataCollectors"],{"datacollectorid","datacollectorip","datacollectorname","availability"},{dcid,dcip,dcname,1}, SQLColumn["datacollectorid"]==dcid]
	]; 	
]

deleteDataCollector[conn_, dcid_] := Module[{},
	SQLDelete[conn, SQLTable["DataCollectors"], SQLColumn["datacollectorid"] == dcid]
]

(*
dato il nome di un data collector ne restituisce l'ultimo ip noto
*)
getDataCollectorIpByName[conn_, name_] := Module[{ip,ret},
	ret = SQLSelect[conn, SQLTable["DataCollectors"], {SQLColumn["datacollectorip"]},SQLColumn["datacollectorname"]==name];
	If[Length[ret]==0, Return[],Return[ret[[1,1]]]]
]

(*
dato il nome di un data collector ne restituisce il mac address
*)
getDataCollectorMacByName[conn_, name_] := Module[{ip,ret},
	ret = SQLSelect[conn, SQLTable["DataCollectors"], {SQLColumn["datacollectorid"]},SQLColumn["datacollectorname"]==name];
	If[Length[ret]==0, Return[],Return[ret[[1,1]]]]
]

(*
dato un mac addr aggiorna il campo availability con il valore fornito in inpu
*)
setDCAvailability[conn_, dcname_, value_] := Module[{},
	SQLUpdate[conn, SQLTable["DataCollectors"], {"availability"}, {value}, SQLColumn["datacollectorname"]==dcname]
]

(*
restituisce la lista dei moduli presenti nella tabella data collectors ordinata per ordine alfabetico
\[EGrave] una lista di stringhe del tipo:
{"dcname1(Up)","dcname2(Down)","dcname(Up)",...}
*)
getDCList[conn_] := Module[{dblist},
	dblist:=SQLSelect[conn, SQLTable["DataCollectors"], {SQLColumn["datacollectorname"],SQLColumn["availability"]}, "SortingColumns"->{SQLColumn["datacollectorname"]->"Ascending"}];
	Return[Map[StringJoin[{#1[[1]],"("<>StringReplace[ToString[#1[[2]]],{"0"->"Down","1"->"Up"}]<>")"}]&,dblist]]
]

getCleanDCList[conn_] := Module[{dblist},
	dblist:=SQLSelect[conn, SQLTable["DataCollectors"], {SQLColumn["datacollectorname"]}, "SortingColumns"->{SQLColumn["datacollectorname"]->"Ascending"}];
	Return[Flatten[dblist]]
]

(*
pulisce la tabella SensorsValues
*)
deleteAllFromSensorsValues[conn_] := Module[{},
	SQLExecute[conn, "DELETE FROM SensorsValues"];
	Return[]
]

(*
pulisce la tabella DataCollectors
*)
deleteAllFromDataCollectors[conn_] := Module[{},
	SQLExecute[conn, "DELETE FROM DataCollectors"];
	Return[]
]

(*
elimina le tabelle dal db (utile se si vuole cambiare formato delle tabelle)
*)
deleteAllTables[conn_] := Module[{},
	SQLExecute[conn, "DROP TABLE SensorsValues"];
	SQLExecute[conn, "DROP TABLE DataCollectors"]
]

(*
funzione di appoggio mia, non te ne fai una fava ma non cancellarla
*)
getDayTemp[day_, datarray_] := Module[{value=Null},
Map[If[
0<=DateDifference[day,#[[1]],"Day"][[1]]< 1,
value = #[[2]]
]&,
datarray];
Return[value];
]

(*
popola il db con dati random (o quasi)
prende in input:
- la connessione al db
- la data di inizio e la data di fine nel formato DateList[{"anno","mese","giorno"}]:
- la lista di data collectors per cui creare dati nella forma: {"mac addr1", "mac addr2",...}
Per ogni data collector nella lista aggiunge per ogni ora di ogni giorno nell'intervallo 4 entry:
- temperatura
- pressione
- umidit\[AGrave]
- luminosit\[AGrave]

Necessita di una connessione a internet.

Prima di chiamare questa funzione pulisci sempre il db per non avere grafici strani.

*)
populateDB[conn_,startDate_, stopDate_, dclist_] := Module[{
drange, 
maxTemps, minTemps,humidity,pressure, 
maxTempsVal, maxTempData,minTempsVal, minTempData,
tempVal,tempData,lightData,
humidityVal,humidityData,
pressureVal,pressureData,
luxval,
finttemp,finthumidity, fintpressure, fintlux,
dcid, dcname, i
},

drange = DateRange[startDate, stopDate, "Hour"];

Do[

dcid = getDataCollectorMacByName[conn, dcname];

maxTemps = WeatherData["Milan","MaxTemperature",{startDate, stopDate ,"Day"}];
minTemps = WeatherData["Milan","MinTemperature",{startDate, stopDate ,"Day"}];
humidity = WeatherData["Milan","Humidity",{startDate, stopDate ,"Day"}];
pressure = WeatherData["Chicago","Pressure",{startDate, stopDate ,"Day"}];

maxTempsVal = Map[{AbsoluteTime[#[[1]]],#[[2]] * RandomReal[{0.9,1.1}]}&,maxTemps]; 
minTempsVal = Map[{AbsoluteTime[#[[1]]],#[[2]]* RandomReal[{0.9,1.1}]}&,minTemps];

tempVal=MapThread[Function[{min,max},{min[[1]], ((min[[2]]+max[[2]])/2)* RandomReal[{0.8,1.2}]}],{minTempsVal,maxTempsVal}];
humidityVal = Map[{AbsoluteTime[#[[1]]],Times[#[[2]],100]* RandomReal[{0.8,1.2}]}&,humidity];
pressureVal = Map[{AbsoluteTime[#[[1]]],#[[2]]+ RandomInteger[{-20,+20}]}&,pressure];
luxval = {{0,0.1},{4,1},{7,10},{10,100},{12,1000},{18,100},{20,10}{22,1},{23,0.1}};

finttemp = Interpolation[tempVal];
finthumidity = Interpolation[humidityVal];
fintpressure = Interpolation[pressureVal];
fintlux = Interpolation[luxval, InterpolationOrder->1];

tempData = Map[{#,finttemp[AbsoluteTime[#]]}&,drange];
humidityData = Map[{#,finthumidity[AbsoluteTime[#]]}&,drange];
pressureData = Map[{#,fintpressure[AbsoluteTime[#]]}&,drange];
lightData = Map[{#, fintlux[#[[4]]*RandomReal[{0.8,1.2}]]}&,drange];

Do[

SQLInsert[conn, "SensorsValues", {"date","timestamp","datacollectorid","sensortype","value"},
			{SQLDateTime[{drange[[i,1]],drange[[i,2]],drange[[i,3]]}],SQLDateTime[drange[[i]]],dcid,"temp",tempData[[i,2]]}];

SQLInsert[conn, "SensorsValues", {"date","timestamp","datacollectorid","sensortype","value"}, 
			{SQLDateTime[{drange[[i,1]],drange[[i,2]],drange[[i,3]]}],SQLDateTime[drange[[i]]],dcid,"umidity",humidityData[[i,2]]}];

SQLInsert[conn, "SensorsValues", {"date","timestamp","datacollectorid","sensortype","value"},
			{SQLDateTime[{drange[[i,1]],drange[[i,2]],drange[[i,3]]}],SQLDateTime[drange[[i]]],dcid,"pressure",pressureData[[i,2]]}];

SQLInsert[conn, "SensorsValues", {"date","timestamp","datacollectorid","sensortype","value"},
			{SQLDateTime[{drange[[i,1]],drange[[i,2]],drange[[i,3]]}],SQLDateTime[drange[[i]]],dcid,"light",lightData[[i,2]]}];


,{i,Range[Length[drange]]}
];

,{dcname,dclist}
];

]

uberPopulateDB[conn_, stopDate_, dclist_] := Module[
{
	startDate, 
	drange,
	refDate,
	dbentries,
	dbentry,
	i
},
	
	startDate = DateList[Take[DatePlus[stopDate, {-7, "Day"}],{1,3}]];
	drange = DateRange[startDate, stopDate, "Hour"];
	Do[
		Do[
			refDate = DatePlus[drange[[i]], {-14 * 24, "Hour"}];
			dbentries = SQLSelect[conn, SQLTable["SensorsValues"], And[SQLColumn["timestamp"] == SQLDateTime[refDate], SQLColumn["datacollectorid"] == getDataCollectorMacByName[conn, dc]]];
			Do[
				newdbentry = {SQLDateTime[DateList[Take[drange[[i]], 3]]],
							 SQLDateTime[drange[[i]]],
							 dbentry[[3]],dbentry[[4]], dbentry[[5]]};
				SQLInsert[conn, SQLTable["SensorsValues"], {"date","timestamp","datacollectorid","sensortype","value"}, newdbentry];
			,{dbentry, dbentries}]
		,{i,Range[Length[drange]]}]
	,{dc,dclist}]
]

(*
Restituisce la lista dei dati per un particolare data collector nelle ultime 24h
Prende in input:
- la connessione al db
- il sensore di interesse
- il mac addr del data collector

L'output \[EGrave] pronto per DateListPlot

*)
getLast24HoursData[conn_, sensname_, dcid_] := Module[{dbret, retval={}},
	dbret=SQLSelect[conn, "SensorsValues", {"timestamp", "value"}, And[SQLDateTime[DatePlus[DateList[AbsoluteTime[]],-1]] < SQLColumn["timestamp"] < SQLDateTime[DateList[AbsoluteTime[]]], SQLColumn["sensorType"]==sensname, SQLColumn["datacollectorid"]==dcid], 
		"SortingColumns"->{SQLColumn["timestamp"]->"Ascending"}];
	retval=Map[Append[retval, {#[[1,1]],#[[2]]}]&,dbret];
	(*Map[Print[{#1[[1,1]],#1[[2]]}]&,dbret];*)
	Return[Flatten[retval,1]];
]

(*
Come per getLast24Hours ma per l'ultima settimana
*)
getLastWeekData[conn_, sensname_, dcid_] := Module[{minimepulite = {},
	massimepulite={}}, 
	minime=SQLExecute[conn, "SELECT date,MIN(value) FROM SensorsValues WHERE sensorType=`1` AND datacollectorid=`4` AND date BETWEEN `2` AND `3` GROUP BY date",
		{sensname,
		SQLDateTime[DatePlus[DateList[DateString[{"Year","Month","Day"}]],{-1,"Week"}]],
		SQLDateTime[DateList[AbsoluteTime[]]],
		dcid}];
	massime=SQLExecute[conn, "SELECT date,MAX(value) FROM SensorsValues WHERE sensorType=`1` AND datacollectorid=`4` AND date BETWEEN `2` AND `3` GROUP BY date",
		{sensname,
		SQLDateTime[DatePlus[DateList[DateString[{"Year","Month","Day"}]],{-1,"Week"}]],
		SQLDateTime[DateList[AbsoluteTime[]]],
		dcid}];
	minimepulite=Map[Append[minimepulite,{#[[1]][[1]],#[[2]]}]&, minime];
	massimepulite=Map[Append[massimepulite,{#[[1]][[1]],#[[2]]}]&, massime];
	Return[{Flatten[minimepulite,1],Flatten[massimepulite,1]}];
]


(*
Come per getLast24Hours ma per l'ultimo mese
*)
getLastMonthData[conn_, sensname_, dcid_] := Module[{minimepulite = {},
	massimepulite={}, startDate = SQLDateTime[DatePlus[DateList[DateString[{"Year","Month","Day"}]],{-1,"Month"}]], stopDate =SQLDateTime[DateList[AbsoluteTime[]]]},
	
	minime=SQLExecute[conn, "SELECT date,MIN(value) FROM SensorsValues WHERE sensorType=`1` AND datacollectorid=`4` AND date BETWEEN `2` AND `3` GROUP BY date",
		{sensname,
		startDate,
		stopDate,
		dcid}];
	massime=SQLExecute[conn, "SELECT date,MAX(value) FROM SensorsValues WHERE sensorType=`1` AND datacollectorid=`4` AND date BETWEEN `2` AND `3` GROUP BY date",
		{sensname,
		startDate,
		stopDate,
		dcid}];

	minimepulite=Map[Append[minimepulite,{#[[1]][[1]],#[[2]]}]&, minime];
	massimepulite=Map[Append[massimepulite,{#[[1]][[1]],#[[2]]}]&, massime];
	Return[{Flatten[minimepulite,1],Flatten[massimepulite,1]}];
]

(*
Come per getLast24Hours ma per l'ultimo anno
*)
getLastYearData[conn_, sensname_, dcid_] := Module[{minimepulite = {},
	massimepulite={}},
	minime=SQLExecute[conn, "SELECT date,MIN(value) FROM SensorsValues WHERE sensorType=`1` AND datacollectorid=`4` AND date BETWEEN `2` AND `3` GROUP BY date",
		{sensname,
		SQLDateTime[DatePlus[DateList[DateString[{"Year","Month","Day"}]],{-1,"Year"}]],
		SQLDateTime[DateList[AbsoluteTime[]]],
		dcid}];
	massime=SQLExecute[conn, "SELECT date,MAX(value) FROM SensorsValues WHERE sensorType=`1` AND datacollectorid=`4` AND date BETWEEN `2` AND `3` GROUP BY date",
		{sensname,
		SQLDateTime[DatePlus[DateList[DateString[{"Year","Month","Day"}]],{-1,"Year"}]],
		SQLDateTime[DateList[AbsoluteTime[]]],
		dcid}];
	minimepulite=Map[Append[minimepulite,{#[[1]][[1]],#[[2]]}]&, minime];
	massimepulite=Map[Append[massimepulite,{#[[1]][[1]],#[[2]]}]&, massime];
	Return[{Flatten[minimepulite,1],Flatten[massimepulite,1]}];
]


(*
aggiunge un dato relativo ad un particolare data collector
prende in input:
- la connessione al db
- il mac addr del data collector
- il tipo di sensore
- il valore
*)
addDataCollectorEntry[conn_, datacollectorid_, sensorType_, value_] := Module[{timestamp=DateList[AbsoluteTime[]]},
	SQLInsert[conn, SQLTable["SensorsValues"], {"date","timestamp","datacollectorid","sensorType","value"},{SQLDateTime[Select[timestamp,#1>0&,3]],SQLDateTime[timestamp],datacollectorid,sensorType,value}];
	Return[]
]
