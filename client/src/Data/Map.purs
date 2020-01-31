module Rtsv2App.Data.Map where

import Prelude

import Data.Tuple (Tuple(..))
import Foreign.ECharts as EC
import Foreign.Object (fromFoldable)

-------------------------------------------------------------------------------
--  GEO Locations TODO: work out what needs to be done here
-------------------------------------------------------------------------------
data_A :: Array (Array EC.CityName)
data_A =
  [
    [{name:"Los Angeles"}, {name:"Barcelona"}],
    [{name:"Barcelona"}, {name:"London"}],
    [{name:"London"}, {name:"Stockholm"}]

]

geoLocations :: EC.GeoLocations
geoLocations = fromFoldable
  [ (Tuple "Amsterdam" [4.895168,52.370216])
  , (Tuple "Athens" [-83.357567,33.951935])
  , (Tuple "Auckland" [174.763332,-36.84846])
  , (Tuple "Bangkok" [100.501765,13.756331])
  , (Tuple "Barcelona" [2.173404,41.385063])
  , (Tuple "Beijing" [116.407395,39.904211])
  , (Tuple "Berlin" [13.404954,52.520007])
  , (Tuple "Bogotá" [-74.072092,4.710989])
  , (Tuple "Bratislava" [17.107748,48.148596])
  , (Tuple "Brussels" [4.35171,50.85034])
  , (Tuple "Bucharest" [26.102538,44.426767])
  , (Tuple "Budapest" [19.040235,47.497912])
  , (Tuple "Buenos Aire" [-58.381559,-34.603684])
  , (Tuple "Cairo" [31.235712,30.04442])
  , (Tuple "Cape Tow" [18.424055,-33.924870])
  , (Tuple "Caracas" [-66.903606,10.480594])
  , (Tuple "Chicago" [-87.629798,41.878114])
  , (Tuple "Copenhagen" [12.568337,55.676097])
  , (Tuple "Dalas" [-96.796989,32.776665])
  , (Tuple "Delhi" [77.209021,28.613939])
  , (Tuple "Doha" [51.53104,25.285447])
  , (Tuple "Dubai" [55.292679,25.267410])
  , (Tuple "Dublin" [-6.26031,53.349805])
  , (Tuple "Edinburgh" [-3.187550,55.950191])
  , (Tuple "Frankfurt" [8.682127,50.110922])
  , (Tuple "Geneva" [6.143158,46.204391])
  , (Tuple "Helsinki" [24.938379,60.169856])
  , (Tuple "Hong Kon" [114.109497,22.396428])
  , (Tuple "IAD Washingto" [-77.039851, 38.877270])
  , (Tuple "Istanbul" [28.978359,41.008238])
  , (Tuple "Jakarta" [106.845599,-6.208763])
  , (Tuple "Johannesburg" [28.047305,-26.204103])
  , (Tuple "Kiev" [30.5234,50.4501])
  , (Tuple "Kuala Lumpu" [101.686855,3.139003])
  , (Tuple "Lima" [-77.042793,-12.046374])
  , (Tuple "Lisbon" [-9.139337,38.722252])
  , (Tuple "Ljubljana" [14.505751,46.056947])
  , (Tuple "London" [-0.127758,51.507351])
  , (Tuple "Los Angele" [-118.243685,34.052234])
  , (Tuple "Luxembourg" [6.129583,49.815273])
  , (Tuple "Lyon" [4.835659,45.764043])
  , (Tuple "Madrid" [-3.70379,40.416775])
  , (Tuple "Manama" [50.58605,26.228516])
  , (Tuple "Manila" [120.984219,14.599512])
  , (Tuple "Mexico Cit" [-99.133208,19.432608])
  , (Tuple "Miami" [-80.19179,25.76168])
  , (Tuple "Milan" [9.185924,45.465422])
  , (Tuple "Montreal" [-73.567256,45.501689])
  , (Tuple "Moscow" [37.6173,55.755826])
  , (Tuple "Mumbai" [72.877656,19.075984])
  , (Tuple "Munich" [11.581981,48.135125])
  , (Tuple "Nairobi" [36.821946,-1.292066])
  , (Tuple "New Yor" [-74.005941,40.712784])
  , (Tuple "Nicosia" [33.382276,35.185566])
  , (Tuple "Oslo" [10.752245,59.913869])
  , (Tuple "Paris" [2.352222,48.856614])
  , (Tuple "Prague" [14.4378,50.075538])
  , (Tuple "Riga" [24.105186,56.949649])
  , (Tuple "Rio d Janeiro" [-43.172896,-22.906847])
  , (Tuple "Rome" [12.496366,41.902783])
  , (Tuple "San Dieg" [-117.161087,32.715736])
  , (Tuple "Santiago d Chile" [-70.669265,-33.44889])
  , (Tuple "Seoul" [126.977969,37.566535])
  , (Tuple "Shanghai" [121.473701,31.230416])
  , (Tuple "Singapore" [103.819836,1.352083])
  , (Tuple "Sofia" [23.321868,42.697708])
  , (Tuple "Stockholm" [18.068581,59.329323])
  , (Tuple "Sydney" [151.209296,-33.86882])
  , (Tuple "São Paul" [-46.633309,-23.55052])
  , (Tuple "Taipei" [121.565418,25.032969])
  , (Tuple "Tallinn" [24.753575,59.436961])
  , (Tuple "Tel Avi" [34.781768,32.0853])
  , (Tuple "Tokyo" [139.691706,35.689487])
  , (Tuple "Toronto" [-79.383184,43.653226])
  , (Tuple "Vienna" [16.373819,48.208174])
  , (Tuple "Vilnius" [25.279651,54.687156])
  , (Tuple "Warsaw" [21.012229,52.229676])
  , (Tuple "Zurich" [8.541694,47.376887])
  ]

cityNames :: Array EC.CityName
cityNames =
  [ {name:"Amsterdam"}
  , {name:"Athens"}
  , {name:"Auckland"}
  , {name:"Bangkok"}
  , {name:"Bangkok"}
  , {name:"Barcelona"}
  , {name:"Beijing"}
  , {name:"Berlin"}
  , {name:"Bogotá"}
  , {name:"Bratislava"}
  , {name:"Brussels"}
  , {name:"Bucharest"}
  , {name:"Budapest"}
  , {name:"Buenos Aire"}
  , {name:"Cairo"}
  , {name:"Cape Town"}
  , {name:"Caracas"}
  , {name:"Chicago"}
  , {name:"Copenhagen"}
  , {name:"Dalas"}
  , {name:"Delhi"}
  , {name:"Doha"}
  , {name:"Dubai"}
  , {name:"Dublin"}
  , {name:"Edinburgh"}
  , {name:"Frankfurt"}
  , {name:"Geneva"}
  , {name:"Helsinki"}
  , {name:"Hong Kong"}
  , {name:"IAD Washington"}
  , {name:"Istanbul"}
  , {name:"Jakarta"}
  , {name:"Johannesburg"}
  , {name:"Kiev"}
  , {name:"Kuala Lumpur"}
  , {name:"Lima"}
  , {name:"Lisbon"}
  , {name:"Ljubljana"}
  , {name:"London"}
  , {name:"Los Angeles"}
  , {name:"Luxembourg"}
  , {name:"Lyon"}
  , {name:"Madrid"}
  , {name:"Manama"}
  , {name:"Manila"}
  , {name:"Mexico City"}
  , {name:"Miami"}
  , {name:"Milan"}
  , {name:"Montreal"}
  , {name:"Moscow"}
  , {name:"Mumbai"}
  , {name:"Munich"}
  , {name:"Nairobi"}
  , {name:"New York"}
  , {name:"Nicosia"}
  , {name:"Oslo"}
  , {name:"Paris"}
  , {name:"Prague"}
  , {name:"Riga"}
  , {name:"Rio de Janeiro"}
  , {name:"Rome"}
  , {name:"San Diego"}
  , {name:"Santiago de Chile"}
  , {name:"Seoul"}
  , {name:"Shanghai"}
  , {name:"Singapore"}
  , {name:"Sofia"}
  , {name:"Stockholm"}
  , {name:"Sydney"}
  , {name:"São Paulo"}
  , {name:"Taipei"}
  , {name:"Tallinn"}
  , {name:"Tel Aviv"}
  , {name:"Tokyo"}
  , {name:"Toronto"}
  , {name:"Vienna"}
  , {name:"Vilnius"}
  , {name:"Warsaw"}
  , {name:"Zurich"}
  ]
