#!/usr/bin/env python
""" This script computes all tuples of stations for instruments with 2,3,4,5,6 channels """

# all stations generated from:
#   xmlstarlet sel -t -v "a:interferometerSetting/description/station/name" CHARA_7T.xml
# S1 F1 S2 F2 S3 S4 S5 E1 F3 E2 F4 E3 W1 F5 W2 F6 W3 W4 W5 N1 N2

# CHARA 7T:
# use fibered fixed telescopes:
ft=["F1","F2","F3","F4","F5","F6"]
# S1=F1 S2=F2 E1=F3 E2=F4 W1=F5 W2=F6
nFixed=len(ft)

# S3 S4 S5
# E3 
# W3 W4 W5
# N1 N2
mt=["S3","S4","S5","E3","W3","W4","W5","N1","N2"]
nMobile=len(mt)



print("<root>")

# 2 telescopes (1T fixed + 1T mobile):
print("<!-- 2T: 1f + 1m -->");
for i in range (0, nFixed):
  for n in range(0, nMobile):
    print("<configuration><stations>"+ft[i]+" "+mt[n]+"</stations>")
    print("<channels>V1 V2</channels></configuration>")

# 3 telescopes (2T fixed + 1T mobile):
print("<!-- 3T: 2f + 1m -->");
for i in range (0, nFixed):
  for j in range(i+1, nFixed):
    for n in range(0, nMobile):
      print("<configuration><stations>"+ft[i]+" "+ft[j]+" "+mt[n]+"</stations>")
      print("<channels>V1 V2 V3</channels></configuration>")

# 4 telescopes (3T fixed + 1T mobile):
print("<!-- 4T: 3f + 1m -->");
for i in range (0, nFixed):
  for j in range(i+1, nFixed):
    for k in range(j+1, nFixed):
      for n in range(0, nMobile):
        print("<configuration><stations>"+ft[i]+" "+ft[j]+" "+ft[k]+" "+mt[n]+"</stations>")
        print("<channels>V1 V2 V3 V4</channels></configuration>")

# 5 telescopes (4T fixed + 1T mobile):
print("<!-- 5T: 4f + 1m -->");
for i in range (0, nFixed):
  for j in range(i+1, nFixed):
    for k in range(j+1, nFixed):
      for l in range(k+1, nFixed):
        for n in range(0, nMobile):
          print("<configuration><stations>"+ft[i]+" "+ft[j]+" "+ft[k]+" "+ft[l]+" "+mt[n]+"</stations>")
          print("<channels>V1 V2 V3 V4 V5</channels></configuration>")

# 6 telescopes (5T fixed + 1T mobile):
print("<!-- 6T: 5f + 1m -->");
for i in range (0, nFixed):
  for j in range(i+1, nFixed):
    for k in range(j+1, nFixed):
      for l in range(k+1, nFixed):
        for m in range(l+1, nFixed):
          for n in range(0, nMobile):
            print("<configuration><stations>"+ft[i]+" "+ft[j]+" "+ft[k]+" "+ft[l]+" "+ft[m]+" "+mt[n]+"</stations>")
            print("<channels>V1 V2 V3 V4 V5 V6</channels></configuration>")

print("</root>")
