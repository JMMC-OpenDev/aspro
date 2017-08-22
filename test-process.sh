#!/bin/bash

DIR=./src/test/resources/amhra/
INPUT=Achernar_all_Pionier_oiDataCalib_2012-09-17.fits

IMAGE=aeri_rot_1.65um_hr.fits
OUTPUT="$INPUT--$IMAGE"

#java -Djava.awt.headless -jar target/aspro2-TRUNK-jar-with-dependencies.jar -process -input $DIR/$INPUT -image $DIR/$IMAGE -output $DIR/$OUTPUT

IMAGE=image_disco_amhra.fits
OUTPUT="$INPUT--$IMAGE"

#java -Djava.awt.headless -jar target/aspro2-TRUNK-jar-with-dependencies.jar -process -input $DIR/$INPUT -image $DIR/$IMAGE -output $DIR/$OUTPUT

IMAGE=image_disco_amhra_UD2mas.fits
OUTPUT="$INPUT--$IMAGE"

java -Djava.awt.headless -jar target/aspro2-TRUNK-jar-with-dependencies.jar -process -input $DIR/$INPUT -image $DIR/$IMAGE -output $DIR/$OUTPUT
