#!/usr/bin/env python2.7

import numpy as np

# note: A0 corrected by Gilles Duvert (march 2014)

# table from Antoine merand / gilles duvert
# --     station   p        q
layout = {
'A0':(-32.0010, -48.0130),
'A1':(-32.0010, -64.0210),
'B0':(-23.9910, -48.0190),
'B1':(-23.9910, -64.0110),
'B2':(-23.9910, -72.0110),
'B3':(-23.9910, -80.0290),
'B4':(-23.9910, -88.0130),
'B5':(-23.9910, -96.0120),
'C0':(-16.0020, -48.0130),
'C1':(-16.0020, -64.0110),
'C2':(-16.0020, -72.0190),
'C3':(-16.0020, -80.0100),
'D0':(  0.0100, -48.0120),
'D1':(  0.0100, -80.0150),
'D2':(  0.0100, -96.0120),
'E0':( 16.0110, -48.0160),
'G0':( 32.0170, -48.0172),
'G1':( 32.0200, -112.0100),
'G2':( 31.9950, -24.0030),
'H0':( 64.0150, -48.0070),
'I1':( 72.0010, -87.9970),
'J1':( 88.0160, -71.9920),
'J2':( 88.0160, -96.0050),
'J3':( 88.0160,   7.9960),
'J4':( 88.0160,  23.9930),
'J5':( 88.0160,  47.9870),
'J6':( 88.0160,  71.9900),
'K0':( 96.0020, -48.0060),
'L0':(104.0210, -47.9980),
'M0':(112.0130, -48.0000),
'U1':(-16.0000, -16.0000),
'U2':( 24.0000,  24.0000),
'U3':( 64.0013,  47.9725),
'U4':(112.0000,   8.0000)}

# AT at 4.5397m
# UT at 13.044m

layout_orientation = -18.984 # degrees

cosPsi = np.cos(np.radians(layout_orientation))
sinPsi = np.sin(np.radians(layout_orientation))


# -- from FITS header:
vlti_latitude = -24.62743941 # NEW
vlti_longitude = -70.40498688

zAT = 4.5397  # 4.5m for AT

cosLat = np.cos(np.radians(vlti_latitude))
sinLat = np.sin(np.radians(vlti_latitude))

# geocentricLatitude: -0.427291058117218
deltaLat = -0.427291058117218 - np.radians(vlti_latitude)
cosDLat = np.cos(deltaLat)
sinDLat = np.sin(deltaLat)


# inverse longitude rotation for geocentric:
vlti_longitude=-vlti_longitude
cosLon = np.cos(np.radians(vlti_longitude))
sinLon = np.sin(np.radians(vlti_longitude))


for sta in sorted(layout):
   #print "station ",sta
   p = layout[sta][0]
   q = layout[sta][1]
   # elevation over horizon for telescopes:
   z = zAT
   if sta.startswith("U"): z = 13.044 # m (from OI_ARRAY)

   #print "(P,Q,Z): ",p, " : ",q, " : ", z

   e =  cosPsi * p + sinPsi * q
   n = -sinPsi * p + cosPsi * q

   # print "E,N: ",e, " ", n

   xh = -sinLat * n + cosLat * z
   yh = e
   zh =  cosLat * n + sinLat * z

   #print sta, " ",xh, " ",yh," ",zh

   # Equatorial local:
   print("<station>")
   print("    <name>{}</name>".format(sta))
   print("    <relativePosition>")
   print("        <posX>{}</posX>".format(xh))
   print("        <posY>{}</posY>".format(yh))
   print("        <posZ>{}</posZ>".format(zh))
   print("      </relativePosition>")
   print("</station>")

   if (0):
           # convert to geocentric:
           xc = xh
           yc = yh
           zc = zh

           # fix geocentric lat
        #   xc =  cosDLat * xc + sinDLat * zc
        #   zc = -sinDLat * xc + cosDLat * zc

           print "GC lat:",sta, " ",xc, " ",yc," ",zc

           # rotate longitude
           xc =  cosLon * xh + sinLon * yh
           yc = -sinLon * xh + cosLon * yh

           print "GC:",sta, " ",xc, " ",yc," ",zc

#EOF

