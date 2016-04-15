(* ::Package:: *)

lngToIndex[lng_,zoom_]:=Module[{a,b,alng,blng},
{alng,blng}=First[{a,b}/.Solve[{a*(-180)+b==0,a(180)+b==1},{a,b}]];
Return[Floor[(alng*lng+blng)2^zoom]];
]

mercator[lat_]:=Log[Abs[Sec[lat*Degree]+Tan[lat*Degree]]];

latToIndex[lat_,zoom_]:=Module[{a,b,alat,blat},
{alat,blat}=First[{a,b}/.Solve[{a*mercator[85.0511287798066]+b==0,a*mercator[-85.0511287798066]+b==1},{a,b}]];
Return[Floor[(alat*mercator[lat]+blat)2^zoom]];
]

indicesToTileURL[x_Integer,y_Integer,zoom_Integer]:="http://mt0.google.com/vt/x="<>ToString[x]<>"&y="<>ToString[y]<>"&z="<>ToString[zoom];


(*
getSmallMap[lat_, long_, zoom_] :=  Import[indicesToTileURL[lngToIndex[long, zoom],latToIndex[lat, zoom], zoom]]
getLargeMap[lat_, lng_, z_] := ImageAssemble[With[{x0=lngToIndex[lng,z],y0=latToIndex[lat,z]},{{Import[indicesToTileURL[x0-1,y0-1,z]],Import[indicesToTileURL[x0,y0-1,z]],Import[indicesToTileURL[x0+1,y0-1,z]]},{Import[indicesToTileURL[x0-1,y0,z]],Import[indicesToTileURL[x0,y0,z]],Import[indicesToTileURL[x0+1,y0,z]]},{Import[indicesToTileURL[x0-1,y0+1,z]],Import[indicesToTileURL[x0,y0+1,z]],Import[indicesToTileURL[x0+1,y0+1,z]]}}]]
*)
