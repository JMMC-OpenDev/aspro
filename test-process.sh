#!/bin/bash

PROCESS_ARGS="-v 3 -fast=true -supersampling=5 -math DEFAULT -apodize true -diameter 1.8"

DIR=./src/test/resources/amhra/
INPUT=Achernar_all_Pionier_oiDataCalib_2012-09-17.fits

IMAGE=aeri_rot_1.65um_hr.fits
OUTPUT="$INPUT--$IMAGE"
java -Djava.awt.headless -jar target/aspro2-TRUNK-jar-with-dependencies.jar -process -input $DIR/$INPUT -image $DIR/$IMAGE -output $DIR/$OUTPUT $PROCESS_ARGS

IMAGE=image_disco_amhra.fits
OUTPUT="$INPUT--$IMAGE"
java -Djava.awt.headless -jar target/aspro2-TRUNK-jar-with-dependencies.jar -process -input $DIR/$INPUT -image $DIR/$IMAGE -output $DIR/$OUTPUT $PROCESS_ARGS

#IMAGE=image_disco_amhra_UD2mas.fits
IMAGE=image_disco_amhra_UD5mas.fits
OUTPUT="$INPUT--$IMAGE"
java -Djava.awt.headless -jar target/aspro2-TRUNK-jar-with-dependencies.jar -process -input $DIR/$INPUT -image $DIR/$IMAGE -output $DIR/$OUTPUT $PROCESS_ARGS

# Test scale / rotate:
java -Djava.awt.headless -jar target/aspro2-TRUNK-jar-with-dependencies.jar -process -input $DIR/$INPUT -image $DIR/$IMAGE -output $DIR/$OUTPUT $PROCESS_ARGS -scale 1.02 -rotate 24.5


# Matisse test
INPUT=2018-12-02T062655_HD45677_A0B2J2C1_IR-LM_LOW_Chop_cal_oifits_0.fits
IMAGE=boo2_RADMC3D_model.fits
OUTPUT="$INPUT--$IMAGE"
echo "CMD: java -Djava.awt.headless -jar target/aspro2-TRUNK-jar-with-dependencies.jar -process -input $DIR/$INPUT -image $DIR/$IMAGE -output $DIR/$OUTPUT $PROCESS_ARGS"
java -Djava.awt.headless -jar target/aspro2-TRUNK-jar-with-dependencies.jar -process -input $DIR/$INPUT -image $DIR/$IMAGE -output $DIR/$OUTPUT $PROCESS_ARGS

