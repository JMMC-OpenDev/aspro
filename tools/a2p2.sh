#!/bin/bash
#FILE=${PWD}/$1

java -classpath /home/gildas/MCS/jmcs/lib/jsamp-1.3-3.jar org.astrogrid.samp.test.MessageSender -mtype "ob.load.data" -param "url" "/home/gildas/GRAVITY/a2p2.xml" -targetname "ESO p2 samp hub" > /tmp/sendtoVOT.log || zenity --error --text="Cannot send to ESO's p2 samp hub, sorry."

