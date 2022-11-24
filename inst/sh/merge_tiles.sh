#!/bin/sh

export BAS=$1
export OUT=$2
## $1 is the raster tile or spatial vector path
## $2 is the output path

for i in "$BAS"; do
	# merge raster file by first creating a vrt and then do the merge
	 gdalbuildvrt basin.vrt $BAS/*.tif
	 rm -f basin.tif
	 gdal_translate -co COMPRESS=DEFLATE -co ZLEVEL=9 basin.vrt $OUT/basin.tif
	 rm -f basin.vrt

	 # merge vector file
	 ogrmerge.py -single -progress -skipfailures -overwrite_ds -f GPKG -o basin.gpkg  $BAS/*.gpkg 
	 rm -f basin_dissolved.gpkg  
	 ogr2ogr  -nlt POLYGON -dialect sqlite -sql "SELECT ST_Union(geom),"ID" FROM merged GROUP BY "ID" " $OUT/basin_dissolved.gpkg basin.gpkg
	 rm -f basin.gpkg 
done