module Rtsv2App.Data.Map where

-------------------------------------------------------------------------------
--  GEO Locations TODO: work out what needs to be done here
-------------------------------------------------------------------------------
data GeoLocName
  = Dia | Dal | Fra | Lcy | Lax | Lux | Mad | Mcia | Kmia
  | Dme | Bom | Nbo | Nyc | Osl | Cdg | Bma | Syd

-- derive instance genericGeoLocName :: Generic GeoLocName _

-- instance eqGeoLocName :: Eq GeoLocName where
--   eq = genericEq

-- instance compareGeoLocName :: Ord GeoLocName where
--   compare = genericCompare

-- instance showGeoLocName :: Show GeoLocName where
--   show = genericShow

-- instance readForeignGeoLocName :: JSON.ReadForeign GeoLocName where
--   readImpl = sumReadForeign

-- instance writeForeignGeoLocName :: JSON.WriteForeign GeoLocName where
--   writeImpl Dia  = JSON.writeImpl "dia"
--   writeImpl Dal  = JSON.writeImpl "dal"
--   writeImpl Fra  = JSON.writeImpl "fra"
--   writeImpl Lcy  = JSON.writeImpl "lcy"
--   writeImpl Lax  = JSON.writeImpl "lax"
--   writeImpl Lux  = JSON.writeImpl "lux"
--   writeImpl Mad  = JSON.writeImpl "mad"
--   writeImpl Mcia = JSON.writeImpl "mcia"
--   writeImpl Kmia = JSON.writeImpl "kmia"
--   writeImpl Dme  = JSON.writeImpl "dme"
--   writeImpl Bom  = JSON.writeImpl "bom"
--   writeImpl Nbo  = JSON.writeImpl "nbo"
--   writeImpl Nyc  = JSON.writeImpl "nyc"
--   writeImpl Osl  = JSON.writeImpl "osl"
--   writeImpl Cdg  = JSON.writeImpl "cdg"
--   writeImpl Bma  = JSON.writeImpl "bma"
--   writeImpl Syd  = JSON.writeImpl "syd"


-- data_A :: Array (Array EC.CityName)
-- data_A =
--   [
--     [{name:"Los Angeles"}, {name:"Barcelona"}],
--     [{name:"Barcelona"}, {name:"London"}],
--     [{name:"London"}, {name:"Stockholm"}]

-- ]

-- geoLocations :: EC.GeoLocations
-- geoLocations = fromFoldable
--   [ (Dia [-87.629798,41.878114])  -- Chicago
--   , (Dal [-96.796989,32.776665])  -- Dalas
--   , (Fra [8.682127,50.110922])    -- Frankfurt
--   , (Lcy [-0.127758,51.507351])   -- London
--   , (Lax [-118.243685,34.052234]) -- Los Angeles
--   , (Lux [6.129583,49.815273])    -- Luxemberg
--   , (Mad [-3.70379,40.416775])    -- Madrid
--   , (Mcia [-99.133208,19.432608]) -- Mexico City
--   , (Kmia [-80.19179,25.76168])   -- Miami
--   , (Dme [37.6173,55.755826])     -- Moscow
--   , (Bom [72.877656,19.075984])   -- Mumbai
--   , (Nbo [36.821946,-1.292066])   -- Nairobi
--   , (Nyc [-74.005941,40.712784])  -- New York
--   , (Osl [10.752245,59.913869])   -- Oslo
--   , (Cdg [2.352222,48.856614])    -- Paris
--   , (Bma [18.068581,59.329323])   -- Stockholm
--   , (Syd [151.209296,-33.86882])  -- Sydney
--   ]

-- cityNames :: Array EC.CityName
-- cityNames =
--   [ {name:"Amsterdam"}
--   , {name:"Athens"}
--   , {name:"Auckland"}
--   , {name:"Bangkok"}
--   , {name:"Bangkok"}
--   , {name:"Barcelona"}
--   , {name:"Beijing"}
--   , {name:"Berlin"}
--   , {name:"Bogotá"}
--   , {name:"Bratislava"}
--   , {name:"Brussels"}
--   , {name:"Bucharest"}
--   , {name:"Budapest"}
--   , {name:"Buenos Aire"}
--   , {name:"Cairo"}
--   , {name:"Cape Town"}
--   , {name:"Caracas"}
--   , {name:"Chicago"}
--   , {name:"Copenhagen"}
--   , {name:"Dalas"}
--   , {name:"Delhi"}
--   , {name:"Doha"}
--   , {name:"Dubai"}
--   , {name:"Dublin"}
--   , {name:"Edinburgh"}
--   , {name:"Frankfurt"}
--   , {name:"Geneva"}
--   , {name:"Helsinki"}
--   , {name:"Hong Kong"}
--   , {name:"IAD Washington"}
--   , {name:"Istanbul"}
--   , {name:"Jakarta"}
--   , {name:"Johannesburg"}
--   , {name:"Kiev"}
--   , {name:"Kuala Lumpur"}
--   , {name:"Lima"}
--   , {name:"Lisbon"}
--   , {name:"Ljubljana"}
--   , {name:"London"}
--   , {name:"Los Angeles"}
--   , {name:"Luxembourg"}
--   , {name:"Lyon"}
--   , {name:"Madrid"}
--   , {name:"Manama"}
--   , {name:"Manila"}
--   , {name:"Mexico City"}
--   , {name:"Miami"}
--   , {name:"Milan"}
--   , {name:"Montreal"}
--   , {name:"Moscow"}
--   , {name:"Mumbai"}
--   , {name:"Munich"}
--   , {name:"Nairobi"}
--   , {name:"New York"}
--   , {name:"Nicosia"}
--   , {name:"Oslo"}
--   , {name:"Paris"}
--   , {name:"Prague"}
--   , {name:"Riga"}
--   , {name:"Rio de Janeiro"}
--   , {name:"Rome"}
--   , {name:"San Diego"}
--   , {name:"Santiago de Chile"}
--   , {name:"Seoul"}
--   , {name:"Shanghai"}
--   , {name:"Singapore"}
--   , {name:"Sofia"}
--   , {name:"Stockholm"}
--   , {name:"Sydney"}
--   , {name:"São Paulo"}
--   , {name:"Taipei"}
--   , {name:"Tallinn"}
--   , {name:"Tel Aviv"}
--   , {name:"Tokyo"}
--   , {name:"Toronto"}
--   , {name:"Vienna"}
--   , {name:"Vilnius"}
--   , {name:"Warsaw"}
--   , {name:"Zurich"}
--   ]
