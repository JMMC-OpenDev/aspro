import numpy as np
from astropy import units as u
from astropy.coordinates import SkyCoord
import xml.etree.ElementTree
import p2api
import gi
import time
from astropy.vo.samp import SAMPIntegratedClient
from astropy.table import Table

gi.require_version('Gtk', '3.0')
from gi.repository import Gtk, GObject

class LoginWindow(Gtk.Window):
    def __init__(self,loginlist,containerInfo,flag):  #passing by pointer on list is a GREAT PAIN IN THE...
        self.login=loginlist
        self.containerInfo=containerInfo

        self.flag=flag
        Gtk.Window.__init__(self, title="Connect with ESO DATABASE")
        self.set_size_request(200, 100)

        self.timeout_id = None

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=3)
        self.add(vbox)

        self.username_hbox = Gtk.Box(spacing=3)
        vbox.pack_start(self.username_hbox, False, False, 0)
        label = Gtk.Label("USERNAME")
        self.username_hbox.pack_start(label, True, True, 0)
        self.username = Gtk.Entry()
        self.username.set_text(self.login[0])
        self.username_hbox.pack_start(self.username, True, True, 0)

        self.password_hbox = Gtk.Box(spacing=6)
        vbox.pack_start(self.password_hbox, False, False, 0)
        label = Gtk.Label("PASSWORD")
        self.password_hbox.pack_start(label, True, True, 0)
        self.password = Gtk.Entry()
        self.password.set_visibility(False)
        self.password.set_text(self.login[1])
        self.password_hbox.pack_start(self.password, True, True, 0)

        self.tempolabel = Gtk.Label("Please Log In ESO USER PORTAL ")
        vbox.pack_start(self.tempolabel, False, False, 0)

       
        self.scrollable = Gtk.ScrolledWindow()
        self.scrollable.set_vexpand(True)
        vbox.pack_start(self.scrollable,True,True,0)        

        hbox = Gtk.Box(spacing=6)
        vbox.pack_start(hbox, False, False, 0)

        self.buttonok = Gtk.Button(label="LOG IN")
        self.buttonok.connect("clicked", self.on_buttonok_clicked)
        hbox.pack_start(self.buttonok, False, True, 0)

        self.buttonabort = Gtk.Button(label="ABORT")
        self.buttonabort.connect("clicked", self.on_buttonabort_clicked)
        hbox.pack_start(self.buttonabort, False, True, 0)

    def on_buttonok_clicked(self, widget):
        self.login[0]=(self.username.get_text())
        self.login[1]=(self.password.get_text())

        if self.login[0] == '52052':
            type='demo'
        else:
            type='production'
        self.api = p2api.ApiConnection(type, self.login[0], self.login[1])
        api=self.api
        runs, _ = self.api.getRuns()

        self.buttonok.destroy()
        self.password_hbox.destroy()
        self.username_hbox.destroy()
        self.buttonabort.set_label("EXIT")
        self.tempolabel.set_text("Select the Project Id in the list:")

        self.store = Gtk.ListStore(str,str,int)
        self.runName=[]
        self.instrument=[]
        self.containerId=[]
        self.treeiter=[]
        for i in range(len(runs)):
            self.runName.append(runs[i]['progId'])
            self.instrument.append(runs[i]['instrument'])
            self.containerId.append(runs[i]['runId'])
            self.treeiter=self.store.append([self.runName[i],self.instrument[i],self.containerId[i]])
        self.treeview =  Gtk.TreeView(self.store)
        # create a CellRendererText to render the data
        renderer = Gtk.CellRendererText()
        for i, column_title in enumerate(["Project ID", "Instrument", "Run ID"]):
            renderer = Gtk.CellRendererText()
            column = Gtk.TreeViewColumn(column_title, renderer, text=i)
            self.treeview.append_column(column)
        self.scrollable.add(self.treeview)
        self.treeselect = self.treeview.get_selection()
        self.treeselect.connect("changed", self.on_tree_selection_changed)
        self.show_all()
            
    def on_tree_selection_changed(self, selection):
        model, treeiter = selection.get_selected()
        if treeiter != None:
           self.flag[0]=1
           self.containerInfo[0]=(model[treeiter][0]) #ProjectID
           self.containerInfo[1]=(model[treeiter][1]) #Instrument
           runId=model[treeiter][2]
           run, _ = self.api.getRun(runId)
           containerId = run["containerId"]
           print ('*** Working with run', run["progId"], run["instrument"], ', containerId: ', containerId, "***")
           self.containerInfo[2]=containerId #containerID

    def on_buttonabort_clicked(self, widget):
        self.flag[0]=-1

    def language_filter_func(self, model, iter, data):
        """Tests if the language in the row is the one in the filter"""
        if self.current_filter_language is None or self.current_filter_language == "None":
            return True
        else:
            return model[iter][2] == self.current_filter_language
    def get_api(self):
        return self.api


def getSkyDiff(ra,dec,ftra,ftdec):
    science=SkyCoord(ra,dec, frame='icrs',unit='deg')
    ft=SkyCoord(ftra, ftdec, frame='icrs',unit='deg')
    ra_offset = (science.ra - ft.ra) * np.cos(ft.dec.to('radian'))
    dec_offset = (science.dec - ft.dec)
    return [ ra_offset.deg*3600*1000, dec_offset.deg*3600*1000] #in mas

def parseXmlMessage(e, api, list, username): #e is parsedTree.
    currentInstrument=list[1]
    containerId=list[2]

    try:
        interferometerConfiguration=e.find('interferometerConfiguration')

        interferometer=interferometerConfiguration.find('name').text
        if interferometer != "VLTI":
            print("ASPRO not set for VLTI, no action taken.")
            return

        BASELINE=interferometerConfiguration.find('stations').text

        instrumentConfiguration=e.find('instrumentConfiguration')
        instrument=instrumentConfiguration.find('name').text
        if instrument != currentInstrument:
            print("ASPRO not set for currently selected instrument: "+currentInstrument)
            return
        # FIXME: TBD CHANGE TO HAVE OTHER INSTRUMENTS THAN GRAVITY!    
        if instrument != "GRAVITY":
            print("ASPRO not set for GRAVITY, no action taken.")
            return
            
        instrumentMode=instrumentConfiguration.find('instrumentMode').text
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
            w=SCRA.rfind('.')
            l=len(SCRA)
            if l-w > 4:
                SCRA=SCRA[0:w+4]
            SCDEC=scienceTarget.find('DEC').text
            w=SCDEC.rfind('.')
            l=len(SCDEC)
            if l-w > 4:
                SCDEC=SCDEC[0:w+4]    
            PMRA=float(scienceTarget.find('PMRA').text)
            PMDEC=float(scienceTarget.find('PMDEC').text)
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
            if OBJTYPE=='CALIBRATOR':
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
            try:
                ftTarget=observationConfiguration.find('FTTarget')
                SEQ_FT_ROBJ_NAME=ftTarget.find('name').text
                FTRA=ftTarget.find('RA').text
                w=FTRA.rfind('.')
                l=len(FTRA)
                if l-w > 4:
                    FTRA=FTRA[0:w+4]
                FTDEC=ftTarget.find('DEC').text
                w=FTDEC.rfind('.')
                l=len(FTDEC)
                if l-w > 4:
                    FTDEC=FTDEC[0:w+4]    
                FTPMRA=float(ftTarget.find('PMRA').text)
                FTPMDEC=float(ftTarget.find('PMDEC').text)
                SEQ_FI_HMAG=float(ftTarget.find('FLUX_H').text)  #just to say we must treat the case there is no FT Target
                SEQ_FT_ROBJ_MAG=SEQ_FI_HMAG
                SEQ_FT_ROBJ_DIAMETER=0.0 #FIXME
                SEQ_FT_ROBJ_VIS=1.0      #FIXME
                dualField=True
            except:
                print('No FT')

            #AO target
            try:
                aoTarget=observationConfiguration.find('AOTarget')
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
                COU_AG_PMA=float(aoTarget.find('PMRA').text)
                COU_AG_PMD=float(aoTarget.find('PMDEC').text)
            except:
                print('No AO')

            #Guide Star 
            try:
                gsTarget=observationConfiguration.find('GSTarget')
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

            except:
                print('No GS')

            #LST interval
            try:
                obsConstraint=observationConfiguration.find('observationConstraints')
                LSTINTERVAL=obsConstraint.find('LSTinterval').text
            except:
                print('No LST interval defined')

            #then call the ob-creation using the API.
            createGravityOB(username, api, containerId, OBJTYPE, NAME, BASELINE, instrumentMode, SCRA, SCDEC, PMRA, PMDEC, SEQ_INS_SOBJ_MAG, SEQ_FI_HMAG, DIAMETER, COU_AG_GSSOURCE, GSRA, GSDEC, COU_GS_MAG, COU_AG_PMA, COU_AG_PMD, dualField, FTRA, FTDEC, SEQ_FT_ROBJ_NAME, SEQ_FT_ROBJ_MAG, SEQ_FT_ROBJ_DIAMETER, SEQ_FT_ROBJ_VIS , LSTINTERVAL)
            print("Processed: "+NAME)
        #endfor
        #
    except:
        print("Corrupted XML?, no action taken.")

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
            print("Dual-Field distance of two stars is  < "+str(SCtoREFminDist)+" mas, Please Correct.")
            return
        elif  np.abs(diff[0]) > SCtoREFmaxDist :
            print("Dual-Field distance of two stars is  > "+str(SCtoREFmaxDist)+" mas, Please Correct.")
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
        print("Invalid Instrument Mode, Please Correct.")
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
    print('number of exposures to reach 1800 s per OB is '+str(nexp))
    if nexp < 3:
        nexp = 3 #min is O S O
        # recompute ndit
        exptime=(1800-900)/nexp
        ndit=(exptime-40)/dit
        ndit=int(ndit)
        if ndit < 10:
            ndit = 10
            print("**Warning**, setting NDIT to 10 will result in an OB longer than 1800 s")
    nexp %= 40
    sequence='O S O O S O O S O O S O O S O O S O O S O O S O O S O O S O O S O O S O O S O O'
    my_sequence=sequence[0:2*nexp]

    #everything seems OK
    #create new OB in container:
    if OBJTYPE == 'SCIENCE':
        isCalib=False
        OBS_DESCR='SCI_'+NAME.replace(' ,:', '_')+'_GRAVITY_'+BASELINE.replace(' ', '')+'_'+instrumentMode
    else:
        isCalib=True
        OBS_DESCR='CAL_'+NAME.replace(' ,:', '_')+'_GRAVITY_'+BASELINE.replace(' ', '')+'_'+instrumentMode
    ob, obVersion = api.createOB(containerId, OBS_DESCR)
    obId = ob['obId']
#    print ('Created OB', obId)

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
#    print ('Saved OB properties', obId)
    
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
#    print('Saved sidereal time constraints')
    
    
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
#    print('Created acquisition template', templateId)


    #print('Updated parameters for acquisition template',tpl['templateName'])
    
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
    #print('Created acquisition template', templateId)
    
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

    ## print('Updated parameters for acquisition template',tpl['templateName'])
    
    
    #verify OB online
    response, _ = api.verifyOB(obId, True, obVersion)
    if response['observable']:
        print('*** Congratulations. Your OB' , obId, ob['name'], 'is observable!***')
    else:
        print('OB', obId, 'is >>not observable<<. See messages below.')
    
    print(' ', '\n '.join(response['messages']))
    
 #   # fetch OB again to confirm its status change
 #   ob, obVersion = api.getOB(obId)
 #   print('Status of verified OB', obId, 'is now', ob['obStatus'])
    

#------------------------------------------------------------start & main loop-----------------------
# Instantiate the client and connect to the hub
client=SAMPIntegratedClient("ESO p2 samp hub")
try:
    client.connect()
except:
    print("Samp Hub Not found (did you start ASPRO?), will exit.")
    exit()
    
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
        

# Instantiate the receiver
r = Receiver(client)

# Listen for any instructions to load a table
client.bind_receive_call("ob.load.data", r.receive_call)
client.bind_receive_notification("ob.load.data", r.receive_notification)

# bool of status change
flag=[0]

username='52052'
password='tutorial'
loginList=[username, password] #apparently strings are immutable, so list of strings is next best.
containerInfo=['0000','0000','0000'] #please make all that less ridiculously written!
api=None
win = LoginWindow(loginList,containerInfo,flag)
win.connect("delete-event", Gtk.main_quit)
win.show_all()

# We now run the loop to wait for the message in a try/finally block so that if
# the program is interrupted e.g. by control-C, the client terminates
# gracefully.
try:

    # We test every 0.1s to see if the hub has sent a message
    while True:
        while (Gtk.events_pending ()):
            Gtk.main_iteration ();
        if flag[0]==-1:
            client.disconnect()
            exit()
        time.sleep(0.1)
        if r.received:
            if api is None:
                pass
            else:
                ob_url=r.params['url']
                if ob_url.startswith("file:") :
                    ob_url=ob_url[5:]
                print("ob_url: ", ob_url)
                e = xml.etree.ElementTree.parse(ob_url)
                parseXmlMessage(e,api,containerInfo, username)
            r.received=False
        if flag[0]==1:
            api=win.get_api()
            username=loginList[0]
#            password=loginList[1]
            runId=containerInfo[0]
            runInstrument=containerInfo[1]
            runName=containerInfo[2]
            #to show that the last choosen arguments are passed
            print(username, runId, runName, runInstrument)
            flag[0]=0
            

finally:
     client.disconnect()

