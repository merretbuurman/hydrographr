#! /bin/bash
export DIR=/home/marquez/hydrographr
export DATA=$DIR/spdata_1264942_ids_snap.csv
export LON=lon_snap
export LAT=lat_snap
export BAS=basin_id
export STREAM=$DIR/order_vect_59.gpkg
export BASIN=basin_h18v04.tif
export PAR=1
export OUT=$DIR
export DIST=all

# path - location of input/output data
export DIR=$1

# name of input table. Must be a .csv file
export DATA=$2

# names of lon and lat coordinates and basinID column
export LON=$3
export LAT=$4
   ### from here on all parameters are only nedded for the longitudinal
   ### didatnce calculation
export BAS=$5

# full path to stream network gpkg file
export STREAM=$6

# full path and name of basin .tif file
export BASIN=$7

# number of cores if running in parallel
export PAR=$8

# destination folder path
export OUT=$9

# which distances to provide (eucl "euclidian", long "longitudinal", all)
export DIST=${10}



##  make the file a gpkg
ogr2ogr -f "GPKG" -overwrite -nln ref_points -nlt POINT -a_srs EPSG:4326 \
    $DIR/ref_points.gpkg $DATA -oo X_POSSIBLE_NAMES=$LON \
    -oo Y_POSSIBLE_NAMES=$LAT -oo AUTODETECT_TYPE=YES


# Name of column for unique ID
export SITE=$( awk -F, 'NR==1 {print $1}' $DATA )


if [ "$DIST" = eucl  ] || [ "$DIST" = all  ]

then

    ###  Calculate Euclidean distance between all points
    grass78  -f -text --tmp-location  -c EPSG:4326 <<'EOF'

    #  import points
    v.in.ogr --o input=$DIR/ref_points.gpkg layer=ref_points \
    output=allpoints type=point key=$SITE

    #  Calculate distance, results are given in meters
    v.distance -pa from=allpoints to=allpoints upload=dist separator=comma \
        | sed -re 's/([0-9]+\.[0-9]{3})[0-9]+/\1/g' | \
        awk -F, 'NR > 1 {print $0}' > $OUT/dist_euclidian.csv 

EOF

fi

if [ "$DIST" = long ] || [ "$DIST" = all  ]
then

# all ids from basins in the input data
export basinID=($(awk -F, -v basin_id="$BAS" '
NR == 1 { for (i=1; i<=NF; i++) {f[$i] = i} }
NR > 1 {print $(f["basin_id"])}' $DATA | sort | uniq))

[ ! -d $DIR/distance ] && mkdir $DIR/distance
export OUTDIR=$DIR/distance

[ ! -d $OUTDIR/dist_fly ] && mkdir $OUTDIR/dist_fly
[ ! -d $OUTDIR/dist_fish ] && mkdir $OUTDIR/dist_fish
# function to do the longitudinal distance calculations per basin
# where each basin can be send to a core in parallel

DistCalc(){

# Macro-basin
#export ID=$(echo $basinID | awk -v id=$SLURM_ARRAY_TASK_ID '{print $id}')
export ID=$1


### create table to store output of distance algorithms
echo "from_$SITE,to_$SITE,dist" > $OUTDIR/dist_fly/dist_fly_allp_${ID}.csv
echo "from_$SITE,to_$SITE,dist" > $OUTDIR/dist_fish/dist_fish_allp_${ID}.csv

grass78 -f -text --tmp-location -c $BASIN <<'EOF'

# Points available in each basin
v.in.ogr --o input=$DIR/ref_points.gpkg layer=ref_points \
    output=benthicEU type=point  where="$BAS = ${ID}" key=$SITE  

RANGE=$(v.db.select -c benthicEU col=$SITE)

# calculation for all points using the streams (as the fish swims)

# read the cleaned stream network generated by r.stream.order
v.in.ogr  --o input=$STREAM \
    layer=SELECT output=stre_cl type=line key=stream

# Connect points to streams 
# (threshold does not matter because the snapping was done before)
v.net -s --o input=stre_cl points=benthicEU output=stream_pALL \
    operation=connect threshold=1 arc_layer=1 node_layer=2

# calculate distance in the stream network between all pairs
v.net.allpairs -g --o input=stream_pALL output=dist_all_${ID} \
cats=$(echo $RANGE | awk '{gsub(" ",","); print $0}')

# add results to table
v.report -c map=dist_all_${ID} layer=1 option=length units=kilometers  \
   | awk -F',' 'BEGIN{OFS=",";} {gsub(/[|]/, ","); print $2, $3, $5}' \
   >> $OUTDIR/dist_fish/dist_fish_allp_${ID}.csv

EOF

}

export -f DistCalc
parallel -j $PAR --delay 5 DistCalc ::: ${basinID[@]}

    if [ "${#basinID[@]}" -eq 1 ]
    then

        #mv $OUTDIR/dist_fly/dist_fly_allp_${ID}.csv $OUT/dist_euclidian_basin.csv
        mv $OUTDIR/dist_fish/dist_fish_allp_${ID}.csv $OUT/dist_longitudinal.csv

    else

        #echo "from_$SITE,to_$SITE,dist" > $OUT/dist_euclidian_basin.csv
        #for FILE in $(find $OUTDIR/dist_fly/ -name 'dist_fly_*')
        #do
        #    awk 'FNR > 1' $FILE >> $OUT/dist_euclidian_basin.csv
        #done

        echo "from_$SITE,to_$SITE,dist" > $OUT/dist_longitudinal.csv
        for FILE in $(find $OUTDIR/dist_fish/ -name 'dist_fish_*')
        do
            awk 'FNR > 1' $FILE >> $OUT/dist_longitudinal.csv
        done    

    fi

rm -rf $OUTDIR

fi

rm $DIR/ref_points.gpkg



exit

