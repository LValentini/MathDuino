(* ::Package:: *)

(*parsa l'xml e restituisce una lista di triple*)
getApFromXml[xml_] :=Cases[xml, XMLElement["ap",_,_],Infinity]  /. XMLElement[_,_,{val_}] -> val /. XMLElement["ap",_,list_] -> list

(*aggiunge un item json nella lista degli ap che poi verr\[AGrave] usata per la richiesta*)
addApToList[list_, mac_, rssi_] := Append[list, Replace[{"macAddress" -> {"_m_"} , "signalToNoiseRatio"-> {"_r_"}},{{"_m_"} -> mac, {"_r_"} -> rssi},Infinity]] 

(*dato l'xml crea la lista degli ap in un formato pronto per la conversione a json*)
addApsToList[xml_] :=Module[{list={}},Map[addApToList[list, #[[3]], #[[2]] ]&,  getApFromXml[xml]]]

(*data la lista nel formato pronto per json genera la stringa json*)
generateJSON[apslist_] := ExportString[{"wifiAccessPoints" -> Flatten[apslist,1]}, "JSON"]

(* PRENDE IN PASTO L'OUTPUT DI generateJSON! Non c'\[EGrave] bisogno di convertire quella lista in una stringa, lo fa gi\[AGrave] la funzione*)
googleLocQuery[requestRules_, key_]:=Module[{requestJsonString,url,client,method,contType,entity,responseCode,response,responseRules,responseExpression},
Needs["JLink`"];
JavaBlock[

requestJsonString=requestRules;
url="https://www.googleapis.com/geolocation/v1/geolocate?key="<> key;
client=JavaNew["org.apache.commons.httpclient.HttpClient"];
method=JavaNew["org.apache.commons.httpclient.methods.PostMethod",url];
method.setRequestHeader["Content-type","application/json\r\n"];
entity=JavaNew["org.apache.commons.httpclient.methods.StringRequestEntity",requestJsonString, "application/json", Null];
method@setRequestEntity[entity];
responseCode=client@executeMethod[method];
If[responseCode==200,
(*then*)
	response:=method@getResponseBodyAsString[]; 
	responseRules=ImportString[response,"JSON"];
	Return[responseRules]
	,
(*else*)
	Return[Null]]]]

(*prende in input la risposta di google locator (o come si chiama) e restituisce una lista {latitudine, longitudine}*)
getLanLngFromGLR[rep_] := Module[{lat,lng},
lat = Cases[rep, HoldPattern["lat"->value_]->value, Infinity][[1]];
lng =Cases[rep, HoldPattern["lng"->value_]->value, Infinity][[1]];
Return[{lat,lng}];
]

getLargeMap[lat_, long_, zoom_] := Module[{},
	Import["http://maps.google.com/maps/api/staticmap?center="<>ToString[lat]<>","<>ToString[long]<>"&zoom="<>ToString[zoom]<>"&size=500x500&markers=color:blue|label:DC|"<>ToString[lat]<>","<>ToString[long]<>"&sensor=false"]
]

getSmallMap[lat_, long_, zoom_] := Module[{},
	Import["http://maps.google.com/maps/api/staticmap?center="<>ToString[lat]<>","<>ToString[long]<>"&zoom="<>ToString[zoom]<>"&size=500x500&markers=color:blue|label:DC|"<>ToString[lat]<>","<>ToString[long]<>"&sensor=false"]
]
