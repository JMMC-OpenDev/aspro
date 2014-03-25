#!/usr/bin/env python
""" This script computes all tuples of stations for instruments with 2,3,4 channels """

# CHARA:
#a=["S1","S2","W1","W2","E1","E2"]

# MROI:
#a = ["W0","W1","W2","W3","W4", "W5", "W6", "W7", "W8", "W9",
# "N1","N2","N3","N4", "N5", "N6", "N7", "N8", "N9",
# "S1","S2","S3","S4", "S5", "S6", "S7", "S8", "S9"
# ]

# SUSI:
a=["N1","N3","N4","S1","S2","S3","S4"]

# NPOI:
#a=["E2","E3","E5","E6","AE","AC","AN","AW","W3","W5","W6","W7"]
a=["E6","AE","AC","AN","AW","W7"]
# naming convention: east -> AE -> AC -> AN-> north -> AW -> west
# to avoid too many configuration => only E6 AE AC AN AW W7

#VLTI:
#a=["A0", "A1", "B0", "B1", "B2", "B3", "B4", "B5", "C0", "C1", "C2", "C3", "D0", "D1", "D2", "E0", "G0", "G1", "G2", "H0", "I1", "J1", "J2", "J3", "J4", "J5", "J6", "K0", "L0", "M0", "U1", "U2", "U3", "U4"]

nbStations=len(a)

print("<root>")

# 2 telescopes:
for i in range (0,nbStations):
  for j in range(i+1,nbStations):
    print("<configuration><stations>"+a[i]+" "+a[j]+"</stations></configuration>")

# 3 telescopes:
for i in range (0,nbStations):
  for j in range(i+1,nbStations):
    for k in range(j+1,nbStations):
      print("<configuration><stations>"+a[i]+" "+a[j]+" "+a[k]+"</stations></configuration>")

# 4 telescopes:
for i in range (0,nbStations):
  for j in range(i+1,nbStations):
    for k in range(j+1,nbStations):
      for l in range(k+1,nbStations):
        print("<configuration><stations>"+a[i]+" "+a[j]+" "+a[k]+" "+a[l]+"</stations></configuration>")

# 5 telescopes (MIRC_5T):
for i in range (0,nbStations):
  for j in range(i+1,nbStations):
    for k in range(j+1,nbStations):
      for l in range(k+1,nbStations):
        for n in range(l+1,nbStations):
          print("<configuration><stations>"+a[i]+" "+a[j]+" "+a[k]+" "+a[l]+" "+a[n]+"</stations></configuration>")

print("</root>")
