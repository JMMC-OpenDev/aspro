#!/usr/bin/env python
""" This script computes all tuples of stations for instruments with 2,3,4 channels """

a=["S1","S2","E1","E2","W1","W2"]
nbStations=len(a)

print("<root>")
for i in range (0,nbStations):
  for j in range(i+1,nbStations):
    print("<configuration><stations>"+a[i]+" "+a[j]+"</stations></configuration>")

for i in range (0,nbStations):
  for j in range(i+1,nbStations):
    for k in range(j+1,nbStations):
      print("<configuration><stations>"+a[i]+" "+a[j]+" "+a[k]+"</stations></configuration>")

for i in range (0,nbStations):
  for j in range(i+1,nbStations):
    for k in range(j+1,nbStations):
      for l in range(k+1,nbStations):
        print("<configuration><stations>"+a[i]+" "+a[j]+" "+a[k]+" "+a[l]+"</stations></configuration>")
print("</root>")
