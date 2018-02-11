CREATE TABLE 'geo_states' (
  'name' varchar(50) NOT NULL DEFAULT '',
  'abv' char(2) NOT NULL DEFAULT '',
  'country' char(2) NOT NULL,
  'is_state' char(1) DEFAULT NULL,
  'is_lower48' char(1) DEFAULT NULL,
  'slug' varchar(50) NOT NULL,
  'latitude' float(9,6) DEFAULT NULL,
  'longitude' float(9,6) DEFAULT NULL,
  'population' bigint(20) unsigned DEFAULT NULL,
  'area' float(8,2)  DEFAULT NULL,
  PRIMARY KEY ('abv','country')
) ;

INSERT INTO 'geo_states' ('name','abv','country','is_state','is_lower48','slug','latitude','longitude','population','area') VALUES 
('Alabama','AL','US','y','y','alabama',32.806671,-86.791130,4779736,50744.00),
('Alaska','AK','US','y','n','alaska',61.370716,-152.404419,710231,571951.25),
('Arizona','AZ','US','y','y','arizona',33.729759,-111.431221,6392017,113634.57),
('Arkansas','AR','US','y','y','arkansas',34.969704,-92.373123,2915918,52068.17),
('California','CA','US','y','y','california',36.116203,-119.681564,37253956,155939.52),
('Colorado','CO','US','y','y','colorado',39.059811,-105.311104,5029196,103717.53),
('Connecticut','CT','US','y','y','connecticut',41.597782,-72.755371,3574097,4844.80),
('Delaware','DE','US','y','y','delaware',39.318523,-75.507141,897934,1953.56),
('District of Columbia','DC','US','n','n','district-of-columbia',38.897438,-77.026817,601723,68.34),
('Florida','FL','US','y','y','florida',27.766279,-81.686783,18801310,53926.82),
('Georgia','GA','US','y','y','georgia',33.040619,-83.643074,9687653,57906.14),
('Hawaii','HI','US','y','n','hawaii',21.094318,-157.498337,1360301,6422.62),
('Idaho','ID','US','y','y','idaho',44.240459,-114.478828,1567582,82747.21),
('Illinois','IL','US','y','y','illinois',40.349457,-88.986137,12830632,55583.58),
('Indiana','IN','US','y','y','indiana',39.849426,-86.258278,6483802,35866.90),
('Iowa','IA','US','y','y','iowa',42.011539,-93.210526,3046355,55869.36),
('Kansas','KS','US','y','y','kansas',38.526600,-96.726486,2853118,81814.88),
('Kentucky','KY','US','y','y','kentucky',37.668140,-84.670067,4339367,39728.18),
('Louisiana','LA','US','y','y','louisiana',31.169546,-91.867805,4533372,43561.85),
('Maine','ME','US','y','y','maine',44.693947,-69.381927,1328361,30861.55),
('Maryland','MD','US','y','y','maryland',39.063946,-76.802101,5773552,9773.82),
('Massachusetts','MA','US','y','y','massachusetts',42.230171,-71.530106,6547629,7840.02),
('Michigan','MI','US','y','y','michigan',43.326618,-84.536095,9883640,56803.82),
('Minnesota','MN','US','y','y','minnesota',45.694454,-93.900192,5303925,79610.08),
('Mississippi','MS','US','y','y','mississippi',32.741646,-89.678696,2967297,46906.96),
('Missouri','MO','US','y','y','missouri',38.456085,-92.288368,5988927,68885.93),
('Montana','MT','US','y','y','montana',46.921925,-110.454353,989415,145552.44),
('Nebraska','NE','US','y','y','nebraska',41.125370,-98.268082,1826341,76872.41),
('Nevada','NV','US','y','y','nevada',38.313515,-117.055374,2700551,109825.99),
('New Hampshire','NH','US','y','y','new-hampshire',43.452492,-71.563896,1316470,8968.10),
('New Jersey','NJ','US','y','y','new-jersey',40.298904,-74.521011,8791894,7417.34),
('New Mexico','NM','US','y','y','new-mexico',34.840515,-106.248482,2059179,121355.53),
('New York','NY','US','y','y','new-york',42.165726,-74.948051,19378102,47213.79),
('North Carolina','NC','US','y','y','north-carolina',35.630066,-79.806419,9535483,48710.88),
('North Dakota','ND','US','y','y','north-dakota',47.528912,-99.784012,672591,68975.93),
('Ohio','OH','US','y','y','ohio',40.388783,-82.764915,11536504,40948.38),
('Oklahoma','OK','US','y','y','oklahoma',35.565342,-96.928917,3751351,68667.06),
('Oregon','OR','US','y','y','oregon',44.572021,-122.070938,3831074,95996.79),
('Pennsylvania','PA','US','y','y','pennsylvania',40.590752,-77.209755,12702379,44816.61),
('Rhode Island','RI','US','y','y','rhode-island',41.680893,-71.511780,1052567,1044.93),
('South Carolina','SC','US','y','y','south-carolina',33.856892,-80.945007,4625364,30109.47),
('South Dakota','SD','US','y','y','south-dakota',44.299782,-99.438828,814180,75884.64),
('Tennessee','TN','US','y','y','tennessee',35.747845,-86.692345,6346105,41217.12),
('Texas','TX','US','y','y','texas',31.054487,-97.563461,25145561,261797.12),
('Utah','UT','US','y','y','utah',40.150032,-111.862434,2763885,82143.65),
('Vermont','VT','US','y','y','vermont',44.045876,-72.710686,625741,9249.56),
('Virginia','VA','US','y','y','virginia',37.769337,-78.169968,8001024,39594.07),
('Washington','WA','US','y','y','washington',47.400902,-121.490494,6724540,66544.06),
('West Virginia','WV','US','y','y','west-virginia',38.491226,-80.954453,1852994,24077.73),
('Wisconsin','WI','US','y','y','wisconsin',44.268543,-89.616508,5686986,54310.10),
('Wyoming','WY','US','y','y','wyoming',42.755966,-107.302490,563626,97100.40),
('Aguascalientes','AG','MX',NULL,NULL,'aguascalientes',NULL,NULL,NULL,NULL),
('Baja California','BC','MX',NULL,NULL,'baja-california',NULL,NULL,NULL,NULL),
('Baja California Sur','BS','MX',NULL,NULL,'baja-california-sur',NULL,NULL,NULL,NULL),
('Campeche','CM','MX',NULL,NULL,'campeche',NULL,NULL,NULL,NULL),
('Chiapas','CS','MX',NULL,NULL,'chiapas',NULL,NULL,NULL,NULL),
('Chihuahua','CH','MX',NULL,NULL,'chihuahua',NULL,NULL,NULL,NULL),
('Coahuila','CO','MX',NULL,NULL,'coahuila',NULL,NULL,NULL,NULL),
('Colima','CL','MX',NULL,NULL,'colima',NULL,NULL,NULL,NULL),
('Durango','DG','MX',NULL,NULL,'durango',NULL,NULL,NULL,NULL),
('Federal District','DF','MX',NULL,NULL,'federal-district',NULL,NULL,NULL,NULL),
('Guanajuato','GT','MX',NULL,NULL,'guanajuato',NULL,NULL,NULL,NULL),
('Guerrero','GR','MX',NULL,NULL,'guerrero',NULL,NULL,NULL,NULL),
('Hidalgo','HG','MX',NULL,NULL,'hidalgo',NULL,NULL,NULL,NULL),
('Jalisco','JA','MX',NULL,NULL,'jalisco',NULL,NULL,NULL,NULL),
('Mexico State','ME','MX',NULL,NULL,'mexico-state',NULL,NULL,NULL,NULL),
('Michoacán','MI','MX',NULL,NULL,'michoacan',NULL,NULL,NULL,NULL),
('Morelos','MO','MX',NULL,NULL,'morelos',NULL,NULL,NULL,NULL),
('Nayarit','NA','MX',NULL,NULL,'nayarit',NULL,NULL,NULL,NULL),
('Nuevo León','NL','MX',NULL,NULL,'nuevo-leon',NULL,NULL,NULL,NULL),
('Oaxaca','OA','MX',NULL,NULL,'oaxaca',NULL,NULL,NULL,NULL),
('Puebla','PB','MX',NULL,NULL,'puebla',NULL,NULL,NULL,NULL),
('Querétaro','QE','MX',NULL,NULL,'queretaro',NULL,NULL,NULL,NULL),
('Quintana Roo','QR','MX',NULL,NULL,'quintana-roo',NULL,NULL,NULL,NULL),
('San Luis Potosí','SL','MX',NULL,NULL,'san-luis-potosi',NULL,NULL,NULL,NULL),
('Sinaloa','SI','MX',NULL,NULL,'sinaloa',NULL,NULL,NULL,NULL),
('Sonora','SO','MX',NULL,NULL,'sonora',NULL,NULL,NULL,NULL),
('Tabasco','TB','MX',NULL,NULL,'tabasco',NULL,NULL,NULL,NULL),
('Tamaulipas','TM','MX',NULL,NULL,'tamaulipas',NULL,NULL,NULL,NULL),
('Tlaxcala','TL','MX',NULL,NULL,'tlaxcala',NULL,NULL,NULL,NULL),
('Veracruz','VE','MX',NULL,NULL,'veracruz',NULL,NULL,NULL,NULL),
('Yucatán','YU','MX',NULL,NULL,'yucatan',NULL,NULL,NULL,NULL),
('Zacatecas','ZA','MX',NULL,NULL,'zacatecas',NULL,NULL,NULL,NULL),
('Alberta','AB','CA',NULL,NULL,'alberta',NULL,NULL,NULL,NULL),
('British Columbia','BC','CA',NULL,NULL,'british-columbia',NULL,NULL,NULL,NULL),
('Manitoba','MB','CA',NULL,NULL,'manitoba',NULL,NULL,NULL,NULL),
('New Brunswick','NB','CA',NULL,NULL,'new-brunswick',NULL,NULL,NULL,NULL),
('Newfoundland and Labrador','NL','CA',NULL,NULL,'newfoundland-and-labrador',NULL,NULL,NULL,NULL),
('Northwest Territories','NT','CA',NULL,NULL,'northwest-territories',NULL,NULL,NULL,NULL),
('Nova Scotia','NS','CA',NULL,NULL,'nova-scotia',NULL,NULL,NULL,NULL),
('Nunavut','NU','CA',NULL,NULL,'nunavut',NULL,NULL,NULL,NULL),
('Ontario','ON','CA',NULL,NULL,'ontario',NULL,NULL,NULL,NULL),
('Prince Edward Island','PE','CA',NULL,NULL,'prince-edward-island',NULL,NULL,NULL,NULL),
('Quebec','QC','CA',NULL,NULL,'quebec',NULL,NULL,NULL,NULL),
('Saskatchewan','SK','CA',NULL,NULL,'saskatchewan',NULL,NULL,NULL,NULL),
('Yukon','YT','CA',NULL,NULL,'yukon',NULL,NULL,NULL,NULL);