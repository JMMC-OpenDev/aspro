#!/usr/bin/env python
""" This script computes all tuples of stations for instruments with 2,3,4 channels """

#VLTI:
# mail
# IP1 3 5 7
#- A0/DL6 B2/DL2 D0/DL1 C1/DL5
#- K0/DL4 G2/DL2 D0/DL1 J3/DL3
#- A0/DL6 G1/DL2 J2/DL4 J3/DL3
# output format: vlti_arrayList_....txt
# A1DL5IP1-G1DL6IP3-K0DL4IP5-I1DL3IP7
#a=["A0DL6IP1", "B2DL2IP3", "D0DL1IP5", "C1DL5IP7"]
#a=["K0DL4IP1", "G2DL2IP3", "D0DL1IP5", "J3DL3IP7"]
a=["A0DL6IP1", "G1DL2IP3", "J2DL4IP5", "J3DL3IP7"]

nbStations=len(a)

#n = 0
#n = 5
n = 10

# 2 telescopes:
#for i in range (0,nbStations):
#  for j in range(i+1,nbStations):
#    print("<configuration><stations>"+a[i]+" "+a[j]+"</stations></configuration>")

# 3 telescopes:
for i in range (0,nbStations):
  for j in range(i+1,nbStations):
    for k in range(j+1,nbStations):
      n = n + 1
      print("("+str(n)+") "+a[i]+"-"+a[j]+"-"+a[k])

# 4 telescopes:
for i in range (0,nbStations):
  for j in range(i+1,nbStations):
    for k in range(j+1,nbStations):
      for l in range(k+1,nbStations):
        n = n + 1
        print("("+str(n)+") "+a[i]+"-"+a[j]+"-"+a[k]+"-"+a[l])

#a=["A0", "B2", "D0", "C1"]
#a=["K0", "G2", "D0", "J3"]
a=["A0", "G1", "J2", "J3"]

# XML
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

