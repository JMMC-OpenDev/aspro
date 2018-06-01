import numpy as np
from astropy import units as u
from astropy.coordinates import SkyCoord
import xml.etree.ElementTree
import p2api

from tkinter import *
from tkinter.messagebox import *
import tkinter.ttk as ttk

import time
import re
import cgi
from astropy.samp import SAMPIntegratedClient
from astropy.table import Table

#help text
HELPTEXT="""
This applet provides the link between ASPRO (that you should have started) and ESO's P2 repository for Observing Blocks (OBs).

Login:
You must log in to the ESO User Portal using your identifiers to access the P2 repository. Please check on the ESO website in case of doubt.

Select Run Id:
After successful login, you are presented with the Runs compatible with Aspro's known instruments. Select the Run, and eventually the subfolder of this Run, where you want to create the OB. Each Run corresponds to a specific instrument. This instrument must be the same as the one selected in ASPRO. 

Send configuration from Aspro:

- In ASPRO, have an object, or an object selected, and check that all important informations (magnitudes, but also Instrument and Fringe Tracker Modes, eventually hour angles), are correctly set.

- In menu "Interop" select "Send Obs. blocks to A2p2"

- Block(s) are created and put in the P2 repository.

- If the source had one or more calibrators, blocks are created for them too.

- For each block submitted, a report is produced. Warnings are usually not significant.

- For more than 1 object sent, a folder containing the two or more blocks is created. In the absence of availability of grouping OBs (like for CAL-SCI-CAL) provided by ESO, this is the closest we can do.

- All the new OBs and folders will be available on https://eso.org/p2
"""
class LoginWindow:
    def __init__(self,root,loginlist,containerInfo,flag):  #passing by pointer on list is a GREAT PAIN IN THE...
        self.loginlist=loginlist
        window=root
        self.containerInfo=containerInfo
        self.flag=flag

        window.title("Connect with ESO DATABASE")


        self.frame = Frame(window)
        self.tree = ttk.Treeview(self.frame,columns=('Project Id', 'instrument', 'folder Id'))
        ysb = ttk.Scrollbar(self.frame, orient='vertical', command=self.tree.yview)
        xsb = ttk.Scrollbar(self.frame, orient='horizontal', command=self.tree.xview)
        self.tree.configure(yscroll=ysb.set, xscroll=xsb.set)
        self.tree.heading('#0', text='Project Id', anchor='w')
        self.tree.heading('#1', text='Instrument', anchor='w')
        self.tree.heading('#2', text='folder Id', anchor='w')
        self.tree.bind('<ButtonRelease-1>', self.on_tree_selection_changed)
        self.tree.grid()
        ysb.grid(row=0, column=1, sticky='ns')
        xsb.grid(row=1, column=0, sticky='ew')
        self.frame.pack()

        self.loginframe  = LabelFrame(window, text="login")

        self.username_label = Label(self.loginframe, text="USERNAME")
        self.username_label.pack()
        self.username = StringVar()
        self.username.set(self.loginlist[0])
        self.username_entry = Entry(self.loginframe,textvariable=self.username)
        self.username_entry.pack()

        self.password_label = Label(self.loginframe, text="PASSWORD")
        self.password_label.pack()
        self.password = StringVar()
        self.password.set(self.loginlist[1])
        self.password_entry = Entry(self.loginframe,textvariable=self.password)
        self.password_entry.pack()

        self.tempo_strval=StringVar()
        self.tempo_strval.set("Please Log In ESO USER PORTAL")
        self.tempolabel = Label(window,textvariable=self.tempo_strval)
        self.tempolabel.pack()

        self.loginframe.pack()
       
        self.progressbar = Scale(window, orient='horizontal', from_=0, to=1, resolution=0.01,showvalue=0,takefocus=0)
        self.progressbar.pack()
        
        self.log_string = StringVar()
        self.log_string.set("log...")
        self.log = Label(window,textvariable=self.log_string)
        self.log.pack()

#
        f1 = Frame(window)
        f1.columnconfigure(3, weight=1)
        f1.rowconfigure(0, weight=1)
        self.buttonok = Button(f1,text="LOG IN",command=self.on_buttonok_clicked)
        self.buttonok.grid(row=0,column=1)
#
        self.buttonabort_strval=StringVar()
        self.buttonabort_strval.set("ABORT")
        self.buttonabort = Button(f1,textvariable=self.buttonabort_strval,command=self.on_buttonabort_clicked)
        self.buttonabort.grid(row=0,column=2)
#        
        self.buttonhelp = Button(f1,text="HELP",command=self.on_buttonhelp_clicked)
        self.buttonhelp.grid(row=0,column=3)
        f1.pack()

#
    def addToLog(self,text):
        self.log_string.set(text)

    def on_buttonok_clicked(self):
        self.loginlist[0]=(self.username.get())
        self.loginlist[1]=(self.password.get())

        if self.loginlist[0] == '52052':
            type='demo'
        else:
            type='production'
        self.api = p2api.ApiConnection(type, self.loginlist[0], self.loginlist[1])
        api=self.api
        runs, _ = self.api.getRuns()
        if len(runs) == 0:
            self.ShowErrorMessage("No Runs defined, impossible to program ESO's P2 interface.")
            self.flag[0]=-1
            return

        self.loginframe.destroy() 
        self.buttonok.destroy()
        self.buttonabort_strval.set("EXIT")
        self.tempo_strval.set("Select the Project Id in the list:")

        supportedInstruments=['GRAVITY','MATISSE','AMBER','PIONIER']

        for i in range(len(runs)):
            if supportedInstruments.count(runs[i]['instrument']) == 1:
                runName=runs[i]['progId']
                instrument=runs[i]['instrument']
                rid=runs[i]['runId']
                cid=runs[i]['containerId']
                self.tree.insert('','end',runName,text=runName,values=(instrument, rid),tags=('run',rid))
                # if folders, add them
                # FIXME: make it recursive!
                folders=api.getFolders(cid)
                for j in range(len(folders)):
                    name=folders[j]['name']
                    contid=folders[j]['containerId']
                    self.tree.insert(runName,'end',contid,text=name,values=(instrument,contid),tags=('folder',rid))
                    folders2=api.getFolders(contid)
                    for k in range(len(folders2)):
                        name2=folders2[k]['name']
                        contid2=folders2[k]['containerId']
                        self.tree.insert(contid,'end',contid2,text=name,values=(instrument,contid2),tags=('subfolder',rid))
 

#    def on_row_expanded(self,view,treeiter,path):
#        index=path[0]
#        id=self.runName[index]
#        if  id != 'Folder:': #get instrument
#            instru=self.instrument[index]
#            self.containerInfo[0]= id #ProjectId
#            self.containerInfo[1]= instru #Instrument
#            runId=self.containerId[index]
#            run, _ = self.api.getRun(runId)
#            containerId = run["containerId"]
#            self.containerInfo[2]=containerId #containerId
#            
    def on_tree_selection_changed(self, selection):
        curItem = self.tree.focus()
        ret=self.tree.item(curItem)
        curinst=ret['values'][0]
        cid=ret['values'][1]
        curname=ret['text']
        tag=ret['tags']
        rid=tag[1]
        entryType=tag[0]
        self.flag[0]=1
        if ( entryType == 'folder' or entryType == 'subfolder' ) : #we have a folder
               new_containerId_same_run=cid
               folderName = curname
               print ("*** Working in Folder",folderName,", containerId: ", new_containerId_same_run, "***")
               win.addToLog('Folder: '+folderName)
               self.containerInfo[2]=new_containerId_same_run
        else:
            instru=curinst
#            if instru != self.containerInfo[1]:
#                self.treeview.collapse_all() #otherwise problems!
            self.containerInfo[0]= curname #ProjectId
            self.containerInfo[1]= instru #Instrument
            run, _ = self.api.getRun(rid)
            containerId = run["containerId"]
            print ("*** Working with ",run["instrument"]," run ",run["progId"],", containerId: ",containerId,"***")
            win.addToLog('Run: '+str(rid))
            self.containerInfo[2]=containerId #containerId

    def on_buttonabort_clicked(self):
        self.flag[0]=-1
        quit()

    def get_api(self):
        return self.api

    def ShowErrorMessage(self,text):
        dialog = showerror("Error",text)

    def ShowWarningMessage(self,text):
        dialog = showwarning("Warning",text)

    def ShowInfoMessage(self,text):
        dialog = showinfo("Info",text)

    def on_buttonhelp_clicked(self):
         self.ShowInfoMessage(HELPTEXT)

def getSkyDiff(ra,dec,ftra,ftdec):
    science=SkyCoord(ra,dec, frame='icrs',unit='deg')
    ft=SkyCoord(ftra, ftdec, frame='icrs',unit='deg')
    ra_offset = (science.ra - ft.ra) * np.cos(ft.dec.to('radian'))
    dec_offset = (science.dec - ft.dec)
    return [ ra_offset.deg*3600*1000, dec_offset.deg*3600*1000] #in mas

def parseXmlMessage(e, api, list, username): #e is parsedTree.

    setProgress(0)
    currentInstrument=list[1]
    containerId=list[2]
    
    try:
        interferometerConfiguration=e.find('interferometerConfiguration')

        interferometer=interferometerConfiguration.find('name').text
        if interferometer != "VLTI":
            print ( "ASPRO not set for VLTI, no action taken.")
            #win.ShowErrorMessage("ASPRO not set for VLTI, no action taken.")
            return

        BASELINE=interferometerConfiguration.find('stations').text

        instrumentConfiguration=e.find('instrumentConfiguration')
        instrument=instrumentConfiguration.find('name').text
        if instrument != currentInstrument:
            #print "ASPRO not set for currently selected instrument: %s" %currentInstrument
            win.ShowErrorMessage("ASPRO not set for currently selected instrument: "+currentInstrument)
            return
        # FIXME: TBD CHANGE TO HAVE OTHER INSTRUMENTS THAN GRAVITY!    
        if instrument != "GRAVITY":
            #print "ASPRO not set for GRAVITY, no action taken."
            win.ShowErrorMessage("ASPRO not set for GRAVITY, no action taken.")
            return

        instrumentMode=instrumentConfiguration.find('instrumentMode').text

        #if we have more than 1 obs, then better put it in a subfolder waiting for the existence of a block sequence not yet implemented in P2
        obsconflist=e.findall('observationConfiguration')
        doFolder=(len(obsconflist) > 1)
        parentContainerId=containerId
        if doFolder:
            folderName=(obsconflist[0].find('SCTarget')).find('name').text
            folderName=re.sub('[^A-Za-z0-9]+', '_', folderName.strip())
            folder, _ = api.createFolder(containerId,folderName)
            containerId=folder['containerId']

        for observationConfiguration in e.findall('observationConfiguration'):
        # science. If calibrator, get also DIAMETER (and compute VIS?)

            TYPE=observationConfiguration.find('type').text
            if TYPE == 'SCIENCE':
                OBJTYPE='SCIENCE'
            else:
                OBJTYPE='CALIBRATOR'

            scienceTarget=observationConfiguration.find('SCTarget')
            NAME=scienceTarget.find('name').text
            SCRA=scienceTarget.find('RA').text
            # RA MUST have 3 digits precision and DEC at least 2!!!
            w=SCRA.rfind('.')
            l=len(SCRA)
            if l-w < 4:
                win.ShowErrorMessage("Object "+NAME+" has a too low precision in RA to be useable by VLTI, please correct.")
                return
                
            if l-w > 4:
                SCRA=SCRA[0:w+4]
            SCDEC=scienceTarget.find('DEC').text
            w=SCDEC.rfind('.')
            l=len(SCDEC)
            if l-w < 3:
                win.ShowErrorMessage("Object "+NAME+" has a too low precision in DEC to be useable by VLTI, please correct.")
                return
            if l-w > 4:
                SCDEC=SCDEC[0:w+4]

            PMRA=0.0
            PMDEC=0.0
            #PMRA and DEC may be null without problem.    
            pmratxt=scienceTarget.find('PMRA')
            if pmratxt != None :
              PMRA=float(pmratxt.text)
            pmdetxt=scienceTarget.find('PMDEC')
            if pmdetxt != None:
              PMDEC=float(pmdetxt.text)

            #but these should be defined.
            COU_GS_MAG=float(scienceTarget.find('FLUX_V').text)
            SEQ_INS_SOBJ_MAG=float(scienceTarget.find('FLUX_K').text)
            SEQ_FI_HMAG=float(scienceTarget.find('FLUX_H').text)
            #setup some default values, to be changed below
            COU_AG_GSSOURCE='SCIENCE' #by default
            GSRA= '00:00:00.000'
            GSDEC= '00:00:00.000'
            COU_AG_PMA=0.0
            COU_AG_PMD=0.0
            dualField=False
            DIAMETER=0.0
            VIS=1.0

            if OBJTYPE=='CALIBRATOR' and scienceTarget.find('DIAMETER') != None :
               DIAMETER=float(scienceTarget.find('DIAMETER').text)
               VIS=1.0 #FIXME

            # initialize FT variables (must exist)
            FTRA=""
            FTDEC=""
            SEQ_FT_ROBJ_NAME=""
            SEQ_FT_ROBJ_MAG=-99.99
            SEQ_FT_ROBJ_DIAMETER=-1.0
            SEQ_FT_ROBJ_VIS=-1.0

            # if FT Target is not ScTarget, we are in dual-field (TBD)
            ftTarget=observationConfiguration.find('FTTarget')
            if ftTarget != None :
                try:
                    SEQ_FT_ROBJ_NAME=ftTarget.find('name').text
                    FTRA=ftTarget.find('RA').text
                    w=FTRA.rfind('.')
                    l=len(FTRA)
                    if l-w < 4:
                        win.ShowErrorMessage("Object "+SEQ_FT_ROBJ_NAME+" has a too low precision in RA to be useable by VLTI, please correct.")
                        return
                    if l-w > 4:
                        FTRA=FTRA[0:w+4]
                    FTDEC=ftTarget.find('DEC').text
                    w=FTDEC.rfind('.')
                    l=len(FTDEC)
                    if l-w < 3:
                        win.ShowErrorMessage("Object "+SEQ_FT_ROBJ_NAME+" has a too low precision in DEC to be useable by VLTI, please correct.")
                        return
                    if l-w > 4:
                        FTDEC=FTDEC[0:w+4]
                    #no PMRA, PMDE for FT !!
                    SEQ_FI_HMAG=float(ftTarget.find('FLUX_H').text)  #just to say we must treat the case there is no FT Target
                    SEQ_FT_ROBJ_MAG=SEQ_FI_HMAG
                    SEQ_FT_ROBJ_DIAMETER=0.0 #FIXME
                    SEQ_FT_ROBJ_VIS=1.0      #FIXME
                    dualField=True
                except:
                    #print "incomplete Fringe Tracker Target definition!"
                    win.ShowErrorMessage("incomplete Fringe Tracker Target definition, OB not set!")

            #AO target
            aoTarget=observationConfiguration.find('AOTarget')
            if aoTarget != None:
                try:
                    AONAME=aoTarget.find('name').text
                    COU_AG_GSSOURCE='SETUPFILE' #since we have an AO
                    AORA=aoTarget.find('RA').text
                    w=AORA.rfind('.')
                    l=len(AORA)
                    if l-w > 4:
                        AORA=AORA[0:w+4]
                    AODEC=aoTarget.find('DEC').text
                    w=AODEC.rfind('.')
                    l=len(AODEC)
                    if l-w > 4:
                        AODEC=AODEC[0:w+4]

                    COU_AG_PMA=0.0
                    COU_AG_PMD=0.0                 
                    #PMRA and DEC may be null without problem.    
                    pmratxt=aoTarget.find('PMRA')
                    if pmratxt != None:
                      COU_AG_PMA=float(pmratxt.text)
                    pmdetxt=aoTarget.find('PMDEC')
                    if pmdetxt != None:
                      COU_AG_PMD=float(pmdetxt.text)

                except:
                    #print "incomplete Adaptive Optics Target definition!"
                    win.ShowErrorMessage("incomplete Adaptive Optics Target definition, OB not set!")
                    
            #Guide Star 
            gsTarget=observationConfiguration.find('GSTarget')
            if gsTarget != None:
                try:
                    GSNAME=gsTarget.find('name').text
                    COU_AG_SOURCE='SETUPFILE' #since we have an GS
                    GSRA=gsTarget.find('RA').text
                    w=GSRA.rfind('.')
                    l=len(GSRA)
                    if l-w > 4:
                        GSRA=GSRA[0:w+4]
                    GSDEC=gsTarget.find('DEC').text
                    w=GSDEC.rfind('.')
                    l=len(GSDEC)
                    if l-w > 4:
                        GSDEC=GSDEC[0:w+4]    
                    COU_GS_MAG=float(gsTarget.find('FLUX_V').text)
                    #no PMRA, PMDE for GS !!

                except:
                    #print "incomplete GuideStar Target definition!"
                    win.ShowErrorMessage("incomplete GuideStar Target definition, OB not set!")
                    
            #LST interval
            try:
                obsConstraint=observationConfiguration.find('observationConstraints')
                LSTINTERVAL=obsConstraint.find('LSTinterval').text
            except:
                pass

            #then call the ob-creation using the API.
            createGravityOB(username, api, containerId, OBJTYPE, NAME, BASELINE, instrumentMode, SCRA, SCDEC, PMRA, PMDEC, SEQ_INS_SOBJ_MAG, SEQ_FI_HMAG, DIAMETER, COU_AG_GSSOURCE, GSRA, GSDEC, COU_GS_MAG, COU_AG_PMA, COU_AG_PMD, dualField, FTRA, FTDEC, SEQ_FT_ROBJ_NAME, SEQ_FT_ROBJ_MAG, SEQ_FT_ROBJ_DIAMETER, SEQ_FT_ROBJ_VIS , LSTINTERVAL)
            win.addToLog("Processed: "+NAME)
        #endfor
        if doFolder:
            containerId=parentContainerId
            doFolder=False
    except:
        #print "General error or Absent Parameter in template (missing magnitude?), OB not set."
        win.ShowErrorMessage("General error or Absent Parameter in template (missing magnitude?), OB not set.")
        setProgress(0)

# here dit must be a string since this is what p2 expects. NOT an integer or real/double.
def getDit(mag,spec,pol,tel,mode):
    string_dit="1"
    
    if mode==1: #Dual
        if tel == 1: 
            mag -= 3.7 #UT, DUAL
        else:
            mag -= 0.7 #AT, DUAL
    elif tel == 1:
        mag -= 3.0 #UT, SINGLE

    if spec == 2: #HR
        if pol == 1 : #SPLIT
            if mag > 1:
                string_dit = "30"
            elif mag > 0:
                string_dit = "10"
            elif mag > -0.5:
                string_dit = "5"
            else:
                string_dit = "3"
        else: #COMB
            if mag > 2:
                string_dit = "30"
            elif mag > 0.5:
                string_dit = "10"
            elif mag > -0.5:
                string_dit = "5"
            else:
                string_dit = "1"
    elif spec == 1: #MR
        if pol == 1 : #SPLIT
            if mag > 4:
                string_dit = "30"
            elif mag > 3:
                string_dit = "10"
            elif mag > 2.5:
                string_dit = "5"
            elif mag > 1.5:
                string_dit = "3"
            elif mag > 0.0:
                string_dit = "1"
            else:
                string_dit = "0.3"
        else: #COMB
            if mag > 5:
                string_dit = "30"
            elif mag > 3.5:
                string_dit = "10"
            elif mag > 3.0:
                string_dit = "5"
            elif mag > 2.5:
                string_dit = "3"
            elif mag > 1.0:
                string_dit = "1"
            else:
                string_dit = "0.3"
    elif spec == 0: #LR FIXME VALUES ARE NOT GIVEN!!!!!!!!
        if pol == 1 : #SPLIT
            if mag > 9:
                string_dit = "30"
            elif mag > 7:
                string_dit = "10"
            elif mag > 6.5:
                string_dit = "5"
            elif mag > 5.5:
                string_dit = "3"
            elif mag > 4.0:
                string_dit = "1"
            else:
                string_dit = "0.3"
        else: #COMB
            if mag > 10:
                string_dit = "30"
            elif mag > 9:
                string_dit = "10"
            elif mag > 7.5:
                string_dit = "5"
            elif mag > 6.5:
                string_dit = "3"
            elif mag > 5.0:
                string_dit = "1"
            else:
                string_dit = "0.3"
    return string_dit

#define function creating the OB:
def createGravityOB(username, api, containerId, OBJTYPE, NAME, BASELINE, instrumentMode, SCRA, SCDEC, PMRA, PMDEC, SEQ_INS_SOBJ_MAG, SEQ_FI_HMAG, DIAMETER, COU_AG_GSSOURCE, GSRA, GSDEC, COU_GS_MAG, COU_AG_PMA, COU_AG_PMD, dualField, FTRA, FTDEC, SEQ_FT_ROBJ_NAME, SEQ_FT_ROBJ_MAG, SEQ_FT_ROBJ_DIAMETER, SEQ_FT_ROBJ_VIS, LSTINTERVAL):

    setProgress(0.1)
    # UT or AT?
    isUT = ( BASELINE[0] == "U" )
    if isUT:
        SCtoREFmaxDist=2000 
        SCtoREFminDist=400
        tel = 1
        if SEQ_INS_SOBJ_MAG  < 5 :
            skyTransparencyConstrainText='Variable, thin cirrus'
        else:
            skyTransparencyConstrainText='Clear'
    else:
        SCtoREFminDist=1500
        SCtoREFmaxDist=4000
        tel = 0
        if SEQ_INS_SOBJ_MAG  < 3 :
            skyTransparencyConstrainText='Variable, thin cirrus'
        else:
            skyTransparencyConstrainText='Clear'
        
    VISIBILITY=1.0
    dualmode=0
    #if Dualfield and offset between fields is bad, complain and do Nothing
    diff=[0,0]
    if dualField:
        dualmode=1
        #compute x,y between science and ref beams:
        diff=getSkyDiff(SCRA,SCDEC,FTRA,FTDEC)
        if np.abs(diff[0]) < SCtoREFminDist :
            win.ShowErrorMessage("Dual-Field distance of two stars is  < "+str(SCtoREFminDist)+" mas, Please Correct.")
            return
        elif  np.abs(diff[0]) > SCtoREFmaxDist :
            win.ShowErrorMessage("Dual-Field distance of two stars is  > "+str(SCtoREFmaxDist)+" mas, Please Correct.")
            return
        
    if instrumentMode == 'LOW-COMBINED':
        INS_SPEC_RES='LOW'
        INS_FT_POL='OUT'
        INS_SPEC_POL='OUT'
        string_dit = getDit(SEQ_INS_SOBJ_MAG,0,0,tel,dualmode)
    elif instrumentMode == 'LOW-SPLIT':
        INS_SPEC_RES='LOW'
        INS_FT_POL='IN'
        INS_SPEC_POL='IN'
        string_dit = getDit(SEQ_INS_SOBJ_MAG,0,1,tel,dualmode)
    elif instrumentMode == 'MEDIUM-COMBINED':
        INS_SPEC_RES='MED'
        INS_FT_POL='OUT'
        INS_SPEC_POL='OUT'
        string_dit = getDit(SEQ_INS_SOBJ_MAG,1,0,tel,dualmode)
    elif instrumentMode == 'MEDIUM-SPLIT':
        INS_SPEC_RES='MED'
        INS_FT_POL='IN'
        INS_SPEC_POL='IN'
        string_dit = getDit(SEQ_INS_SOBJ_MAG,1,1,tel,dualmode)
    elif instrumentMode == 'HIGH-COMBINED':
        INS_SPEC_RES='HIGH'
        INS_FT_POL='OUT'
        INS_SPEC_POL='OUT'
        string_dit = getDit(SEQ_INS_SOBJ_MAG,2,0,tel,dualmode)
    elif instrumentMode == 'HIGH-SPLIT':
        INS_SPEC_RES='HIGH'
        INS_FT_POL='IN'
        INS_SPEC_POL='IN'
        string_dit = getDit(SEQ_INS_SOBJ_MAG,2,1,tel,dualmode)
    else:
        win.ShowErrorMessage("Invalid Instrument Mode, Please Correct.")
        return

    #compute ndit, nexp
    dit=float(string_dit)
    ndit=300/dit
    if ndit < 10:
        ndit = 10
    if ndit > 300:
        ndit = 300
    ndit=int(ndit) #must be integer
    exptime=int(ndit*dit+40) #40 sec overhead by exp
    nexp=(1800-900)/exptime
    nexp=int(nexp)
    win.addToLog('number of exposures to reach 1800 s per OB is '+str(nexp))
    if nexp < 3:
        nexp = 3 #min is O S O
        # recompute ndit
        exptime=(1800-900)/nexp
        ndit=(exptime-40)/dit
        ndit=int(ndit)
        if ndit < 10:
            ndit = 10
            win.addToLog("**Warning**, OB NDIT has been set to min value=10, but OB will take longer than 1800 s")
    nexp %= 40
    sequence='O S O O S O O S O O S O O S O O S O O S O O S O O S O O S O O S O O S O O S O O'
    my_sequence=sequence[0:2*nexp]

    #everything seems OK
    #create new OB in container:
    goodName=re.sub('[^A-Za-z0-9]+', '_', NAME.strip())
    if OBJTYPE == 'SCIENCE':
        isCalib=False
        OBS_DESCR='SCI_'+goodName+'_GRAVITY_'+BASELINE.replace(' ', '')+'_'+instrumentMode
    else:
        isCalib=True
        OBS_DESCR='CAL_'+goodName+'_GRAVITY_'+BASELINE.replace(' ', '')+'_'+instrumentMode
    ob, obVersion = api.createOB(containerId, OBS_DESCR)
    obId = ob['obId']

    #we use obId to populate OB
    ob['obsDescription']['name'] = OBS_DESCR[0:min(len(OBS_DESCR),31)]
    ob['obsDescription']['userComments'] = 'Generated by '+username+' using ASPRO 2 (c) JMMC'
    #ob['obsDescription']['InstrumentComments'] = 'AO-B1-C2-E3' #should be a list of alternative quadruplets!
    
    ob['target']['name'] = NAME.replace(' ', '_')
    ob['target']['ra'] = SCRA
    ob['target']['dec'] = SCDEC
    ob['target']['properMotionRa'] = round(PMRA/1000.0,4)
    ob['target']['properMotionDec'] = round(PMDEC/1000.0,4)
    
    
    ob['constraints']['name']  = 'Aspro-created constraint'
    #FIXME: error (OB): "Phase 2 constraints must closely follow what was requested in the Phase 1 proposal.
    #                    The seeing value allowed for this OB is >= java0x0 arcsec."
    ob['constraints']['seeing'] = 1.0
    ob['constraints']['skyTransparency'] = skyTransparencyConstrainText
    ob['constraints']['baseline']  = BASELINE.replace(' ', '-')
    # FIXME: default values NOT IN ASPRO!
    #ob['constraints']['airmass'] = 5.0
    #ob['constraints']['fli'] = 1
    
    ob, obVersion = api.saveOB(ob, obVersion)
    
    ##LST constraints
    ##by default, above 40 degree. Will generate a WAIVERABLE ERROR if not.
    sidTCs, stcVersion = api.getSiderealTimeConstraints(obId)
    lsts=LSTINTERVAL.split('/')
    lstStartSex=lsts[0]
    lstEndSex=lsts[1]
    ## p2 seems happy with endlst < startlst
    ## a = SkyCoord(lstStartSex+' +0:0:0',unit=(u.hourangle,u.deg))
    ## b = SkyCoord(lstEndSex+' +0:0:0',unit=(u.hourangle,u.deg))
    ## if b.ra.deg < a.ra.deg:
    ## api.saveSiderealTimeConstraints(obId,[ {'from': lstStartSex, 'to': '00:00'},{'from': '00:00','to': lstEndSex}], stcVersion)
    ## else:
    api.saveSiderealTimeConstraints(obId,[ { 'from': lstStartSex,  'to': lstEndSex  } ], stcVersion)

    setProgress(0.2)
    
    
    #then, attach acquisition template(s)
    if dualField:
        tpl, tplVersion = api.createTemplate(obId, 'GRAVITY_dual_acq')
        # put values
        tpl, tplVersion = api.setTemplateParams(obId, tpl, {
        'SEQ.FT.ROBJ.NAME'  : SEQ_FT_ROBJ_NAME,
        'SEQ.FT.ROBJ.MAG'   : round(SEQ_FT_ROBJ_MAG,3),
        'SEQ.FT.ROBJ.DIAMETER' : SEQ_FT_ROBJ_DIAMETER,
        'SEQ.FT.ROBJ.VIS' :  SEQ_FT_ROBJ_VIS,
        'SEQ.FT.MODE'  :      "AUTO",
        'SEQ.INS.SOBJ.NAME'   :   NAME,
        'SEQ.INS.SOBJ.MAG':   round(SEQ_INS_SOBJ_MAG,3),
        'SEQ.INS.SOBJ.DIAMETER'   :   DIAMETER,
        'SEQ.INS.SOBJ.VIS':   VISIBILITY,
        'SEQ.INS.SOBJ.X'  : diff[0],
        'SEQ.INS.SOBJ.Y'  : diff[1],
        'SEQ.FI.HMAG' :   round(SEQ_FI_HMAG,3),
        'TEL.TARG.PARALLAX'   :   0.0,
        'INS.SPEC.RES': INS_SPEC_RES,
        'INS.FT.POL'  : INS_FT_POL,
        'INS.SPEC.POL':  INS_SPEC_POL,
        'COU.AG.GSSOURCE' :   COU_AG_GSSOURCE,
        'COU.AG.ALPHA':   GSRA,
        'COU.AG.DELTA':   GSDEC,
        'COU.GS.MAG'  :  round(COU_GS_MAG,3),
        'COU.AG.PMA'  :  round(COU_AG_PMA/1000,4),
        'COU.AG.PMD'  :  round(COU_AG_PMD/1000,4)
        }, tplVersion)

    else:
        tpl, tplVersion = api.createTemplate(obId, 'GRAVITY_single_acq')
        # put values
        tpl, tplVersion = api.setTemplateParams(obId, tpl, {
        'SEQ.INS.SOBJ.NAME'   :   NAME,
        'SEQ.INS.SOBJ.MAG':   round(SEQ_INS_SOBJ_MAG,3),
        'SEQ.INS.SOBJ.DIAMETER'   :   DIAMETER,
        'SEQ.INS.SOBJ.VIS':   VISIBILITY,
        'COU.AG.GSSOURCE' :   COU_AG_GSSOURCE,
        'COU.AG.ALPHA':   GSRA,
        'COU.AG.DELTA':   GSDEC,
        'COU.GS.MAG'  :  round(COU_GS_MAG,3),
        'COU.AG.PMA'  :  round(COU_AG_PMA/1000,4),
        'COU.AG.PMD'  :  round(COU_AG_PMD/1000,4),
        'SEQ.FI.HMAG' :   round(SEQ_FI_HMAG,3),
        'TEL.TARG.PARALLAX'   :   0.0,
        'INS.SPEC.RES': INS_SPEC_RES,
        'INS.FT.POL'  : INS_FT_POL,
        'INS.SPEC.POL':  INS_SPEC_POL
            }, tplVersion)
    
    templateId = tpl['templateId']
    
    
    setProgress(0.3)

    if isCalib:
        if dualField:
            tpl, tplVersion = api.createTemplate(obId, 'GRAVITY_dual_obs_calibrator')
        else:
            tpl, tplVersion = api.createTemplate(obId, 'GRAVITY_single_obs_calibrator')
    else:
        if dualField:
            tpl, tplVersion = api.createTemplate(obId, 'GRAVITY_dual_obs_exp')
        else:
            tpl, tplVersion = api.createTemplate(obId, 'GRAVITY_single_obs_exp')
    templateId = tpl['templateId']

    setProgress(0.4)
    
    # put values. they are the same except for dual obs science (?)
    if dualField and not isCalib :
        tpl, tplVersion = api.setTemplateParams(obId, tpl, {
        'DET2.DIT' :  string_dit,
        'DET2.NDIT.OBJECT' :  ndit,
        'DET2.NDIT.SKY':  ndit,
        'SEQ.OBSSEQ'   :  my_sequence,
        'SEQ.SKY.X':  2000,
        'SEQ.SKY.Y':  2000
         }, tplVersion)
    else:
        tpl, tplVersion = api.setTemplateParams(obId, tpl, {
        'DET2.DIT' :  string_dit,
        'DET2.NDIT.OBJECT' :  ndit,
        'DET2.NDIT.SKY':  ndit,
        'SEQ.OBSSEQ'   :  my_sequence,
        'SEQ.RELOFF.X' :  "0.0",
        'SEQ.RELOFF.Y' :  "0.0",
        'SEQ.SKY.X':  2000,
        'SEQ.SKY.Y':  2000
         }, tplVersion)

    setProgress(0.5)

    #verify OB online
    response, _ = api.verifyOB(obId, True, obVersion)

    setProgress(1.0)

    if response['observable']:
        win.ShowInfoMessage('OB '+str(obId)+' '+ob['name']+' is OK.')
        win.addToLog('OB: '+str(obId)+' is ok')
    else:
        s=""
        for ss in response['messages']:
            s+=cgi.escape(ss)+'\n'
        win.ShowWarningMessage('OB '+str(obId)+' <b>HAS Warnings</b>. ESO says:\n\n'+s)
        win.addToLog('OB: '+str(obId)+' created with warnings')
 # (NOTE: we need to escape things like <= in returned text)

 #   # fetch OB again to confirm its status change
 #   ob, obVersion = api.getOB(obId)
 #   python3: print('Status of verified OB', obId, 'is now', ob['obStatus'])
    
def setProgress(perc): win.progressbar.set(perc)
  
#------------------------------------------------------------start & main loop-----------------------
# Instantiate the client and connect to the hub
client=SAMPIntegratedClient("ESO p2 samp hub")
connected=0
#try:
#    client.connect()
#except:
#    print "Samp Hub Not found (did you start ASPRO?), will exit."
##    win.ShowErrorMessage("Samp Hub Not found (did you start ASPRO?), will exit.")
#    exit()
    
# Set up a receiver class
class Receiver(object):
    def __init__(self, client):
        self.client = client
        self.received = False
    def receive_call(self, private_key, sender_id, msg_id, mtype, params, extra):
        self.params = params
        self.received = True
        self.client.reply(msg_id, {"samp.status": "samp.ok", "samp.result": {}})
    def receive_notification(self, private_key, sender_id, mtype, params, extra):
        self.params = params
        self.received = True
        

# bool of status change
flag=[0]

import getpass
localuser=getpass.getuser()

username='52052'
password='tutorial'
loginList=[username, password] #apparently strings are immutable, so list of strings is next best.
containerInfo=['0000','0000','0000'] #please make all that less ridiculously written!
api=None

root=Tk()
win=LoginWindow(root,loginList,containerInfo,flag)

shared=[0,api]

def task(shared):
    # We now run the loop to wait for the message in a try/finally block so that if
    # the program is interrupted e.g. by control-C, the client terminates
    # gracefully.
    api=shared[1]
    try:
        if (shared[0] == 0):
            client.connect()
            shared[0] = 1
            # Instantiate the receiver
            r = Receiver(client)
            shared.append(r)
            # Listen for any instructions to load a table
            client.bind_receive_call("ob.load.data", r.receive_call)
            client.bind_receive_notification("ob.load.data", r.receive_notification)
            #        client.notify_all("hello")
    except:
        pass

    if (flag[0] == -1):
        if (shared[0] == 1):
            client.disconnect()
            exit()

    if (shared[0] == 1):
        r=shared[2]
        if r.received:
            if api is None:
                win.ShowErrorMessage('a2p2 is not currently shared with ESO P2 database.')
                r.received=False
            else:
                ob_url=r.params['url']
                if ob_url.startswith("file:") :
                    ob_url=ob_url[5:]
                    e = xml.etree.ElementTree.parse(ob_url)
                    parseXmlMessage(e,api,containerInfo, localuser)
                    r.received=False
    if flag[0]==1:
        shared[1]=api=win.get_api()
        username=loginList[0]
        #            password=loginList[1]
        runId=containerInfo[0]
        runInstrument=containerInfo[1]
        runName=containerInfo[2]
        flag[0]=0

    root.after(50, task,shared)

#sleep some...
root.after(50, task,shared)
root.mainloop();

