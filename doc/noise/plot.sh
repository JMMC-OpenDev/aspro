#!/bin/bash
stilts plot2plane \
   xpix=1000 ypix=600 \
    ylog=true grid=true \
    xmin=0 xmax=14 ymin=0.099 ymax=200.1 \
    title='GRAVITY_FT SNR' legend=true \
    antialias=true ifmt=CSV x='K (mag)' y=SNR shading=auto size=3 \
    layer_1=Function \
        fexpr_1=1.5 color_1=green \
        leglabel_1='SNR = 1.5' \
    layer_2=Mark \
        in_2=VLTI_UT_GRAVITY_FT.csv \
        leglabel_2='VLTI UT' \
    layer_3=Line \
        in_3=VLTI_UT_GRAVITY_FT.csv \
        thick_3=2 \
        leglabel_3='VLTI UT' \
    layer_4=Mark \
        in_4=VLTI_AT_GRAVITY_FT.csv \
        shape_4=filled_square color_4=blue \
        leglabel_4='VLTI AT' \
    layer_5=Line \
        in_5=VLTI_AT_GRAVITY_FT.csv \
        color_5=blue thick_5=2 \
        leglabel_5='VLTI AT' \
   out=VLTI_UT_GRAVITY_FT-UT_vs_AT.pdf 


stilts plot2plane \
   xpix=1000 ypix=600 \
   ylog=true xlabel='K (mag)' ylabel=SNR grid=true \
   xmin=0 xmax=16 ymin=0.099 ymax=200.1 \
   title='GRAVITY_FT SNR (UT)' legend=true \
   ifmt=CSV shading=auto size=5 antialias=true \
   layer_1=Mark \
      in_1=VLTI_UT_GRAVITY_FT.csv \
      x_1='K (mag)' y_1=SNR \
      leglabel_1=MACAO \
   layer_2=Line \
      in_2=VLTI_UT_GRAVITY_FT.csv \
      x_2='K (mag)' y_2=SNR \
      thick_2=2 \
      leglabel_2=MACAO \
   layer_3=Mark \
      in_3=VLTI_UT_GRAVITY_FT_GPAO-FAINT.csv \
      x_3=K_mag y_3=SNR_FT \
      color_3=blue \
      leglabel_3=GPAO \
   layer_4=Line \
      in_4=VLTI_UT_GRAVITY_FT_GPAO-FAINT.csv \
      x_4=K_mag y_4=SNR_FT \
      color_4=blue thick_4=2 \
      leglabel_4=GPAO \
   layer_5=Function \
      fexpr_5=1.5 color_5=green \
      leglabel_5='SNR = 1.5' \
   out=VLTI_UT_GRAVITY_FT-GPAO-FAINT.pdf

