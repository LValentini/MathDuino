(* ::Package:: *)

(*restituisce l'id del nodo se risponde con pong; altrimenti Null*)
(*restituisce Null anche se si interroga un web server che non restituisce un frammento la cui document root \[EGrave] <pong/>*)
pingDataCollector[ip_] := Module[{ret,pong},
ret:=Quiet[URLFetch["http://"<>ip<>"/ping", "Method"->"GET", "ConnectTimeout"->2]];
If[ret ===$Failed, 
(*then*)
Return[Null], 
(*else*)
pong := Quiet[ImportString[ret, "XML"]];
id :=Cases[pong, XMLElement["pong",_,_],Infinity] /.XMLElement[_,_,{val_}]-> val];
If[Length[id]==0, Return[],Return[id[[1]]]]
]

(*restituisce l'XMLObject del nodo con tutti i dati; altrimenti restituisce Null*)
getDataCollectorXML[ip_] := Module[{raw,ret},
raw:=Quiet[URLFetch["http://"<>ip<>"/data", "Method"-> "GET", "ConnectTimeout"->2]];
If[ret ===$Failed, 
(*then*)
Return[Null], 
(*else*)
Return[Quiet[ImportString[raw,"XML"]]];
]]


(*preso l'output di getDataCollectorXML restituisce un array con i sensori nella forma
{{"metrica",valore},...}*)
getSensorsDataArray[xml_] := Module[{},
Return[Cases[xml, XMLElement["sensor",_,_],Infinity] /.XMLElement[_,_,{val_}]-> val /. XMLElement["sensor",_,list_] -> list]
]

(*Prende l'output di getSensorsDataArray e resituisce il valore del sensore con il nome dato in input, restituisce null se non c'\[EGrave] alcun sensore con quel nome*)
getSensorValue[sensname_, datarray_] := Module[{value=Null},
Map[If[#[[1]]==sensname,value =#[[2]]]&,datarray];
Return[value];
]
