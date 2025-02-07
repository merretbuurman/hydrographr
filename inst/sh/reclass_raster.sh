#!/bin/sh

export RASTER=$1
export RULES=$2
export OUTPUT=$3
export NODATA=$4
export TYPE=$5
export COMPRESSION=$6
export LEVEL=$7
export BTIFF=$8


# Start GRASS GIS session
grass -f --gtext --tmp-location  $RASTER   <<'EOF'

    # Load raster input file
    g.message "Input raster file: $RASTER"
    g.message "Loading input raster file..."
    r.in.gdal --o input=$RASTER  output=raster    --overwrite
    g.message "Loading input raster file... DONE."

    # Reclassify the raster according to the rules
    # Note: This struggles if the reclass rules text file is very big.
    g.message "Reclassifying raster file..."
    g.message "This may use a lot of memory and crash if the input raster is big."
    g.message "If this step crashes, please consider using a smaller input raster and/or input table."
    r.reclass input=raster output=recl_raster rules=$RULES --overwrite
    g.message "Reclassifying raster file... DONE"

    # Export reclassified raster map
    g.message "Exporting reclassified raster to file with nodata as "$NODATA"..."
    r.out.gdal input=recl_raster output=$OUTPUT type=$TYPE  format=GTiff nodata=$NODATA  --o -f -m -c createopt="COMPRESS=$COMPRESSION,ZLEVEL=$LEVEL,BIGTIFF=$BTIFF"
    g.message "Exporting reclassified raster to file... DONE."

    # Note: If nodata is not set at all, the following message is printed by GRASS during r.out.gdal:
    #> Input raster map contains cells with NULL-value (no-data). The value
    #> -2147483648 will be used to represent no-data values in the input map. You
    #> can specify a nodata value with the nodata option.

EOF

exit
