#*******************************************************************************
# JMMC project
#
# "@(#) $Id: Makefile,v 1.14 2010-10-07 15:03:36 bourgesl Exp $"
#
# Makefile of aspro module
#
# History
# -------
# $Log: not supported by cvs2svn $
# Revision 1.13  2010/10/06 08:49:58  bourgesl
# try again
#
# Revision 1.12  2010/10/06 08:46:19  bourgesl
# add searchCal_template.xml in jar
#
# Revision 1.11  2010/09/13 14:12:49  bourgesl
# fixed typo
#
# Revision 1.10  2010/09/13 14:10:35  bourgesl
# added edu.dartmouth license
#
# Revision 1.9  2010/06/23 13:08:40  bourgesl
# added xslt files
#
# Revision 1.8  2010/05/17 15:53:20  ccmgr
# change jnlp file
#
# Revision 1.7  2010/04/02 14:38:58  bourgesl
# added OBX files
#
# Revision 1.6  2010/01/13 16:17:29  bourgesl
# added gif icons in the build
#
# Revision 1.5  2009/12/07 15:31:09  bourgesl
# added missing icons
#
# Revision 1.4  2009/11/20 10:22:40  mella
# Add jnlp deployement during installation step
#
# Revision 1.3  2009/11/20 10:17:29  mella
# add jnlp to deploy aspro gui app
#
# Revision 1.2  2009/11/17 09:44:39  mella
# add new util tool
#
# Revision 1.1  2009/09/08 08:15:21  ccmgr
# Fix directory structure and add additional files
#
#*******************************************************************************
# REMARKS
#    Makefile based on the VLT/ALMA Makefile
#
#-------------------------------------------------------------------------------

#
# user definable C-compilation flags
#USER_CFLAGS = 

#
# user definable javac compilation flags
#USER_JFLAGS = 

#
# additional include and library search paths
#USER_INC = 
#USER_LIB = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     =
EXECUTABLES_L   = asproGenInstConfigs

#
# <brief description of xxxxx program>
xxxxx_OBJECTS   =
xxxxx_LDFLAGS   =
xxxxx_LIBS      =

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES        =

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       =
LIBRARIES_L     =

#
# <brief description of lllll library>
lllll_OBJECTS   =

#
# Scripts (public and local)
# --------------------------
SCRIPTS         =
SCRIPTS_L       =

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

#
# <brief description of tttttt tcl-script>
tttttt_OBJECTS  =
tttttt_TCLSH    = 
tttttt_LIBS     = 

#
# TCL libraries (public and local)
# --------------------------------
TCL_LIBRARIES   =
TCL_LIBRARIES_L =

#
# <brief description of tttlll library>
tttlll_OBJECTS  = 

#
# Python scripts (public and local)
# ---------------------------------
PY_SCRIPTS     =
PY_SCRIPTS_L   =

#
# Jarfiles and their directories
# ---------------------------------
JARFILES        = aspro

#
# <brief description of aspro jar files>
aspro_DIRS     = edu \
                 fr

aspro_EXTRAS   = edu/dartmouth/License.txt \
                 fr/jmmc/aspro/*.xml \
                 fr/jmmc/aspro/gui/*.xsl \
                 fr/jmmc/aspro/gui/action/*.xml \
                 fr/jmmc/aspro/gui/icons/*.gif \
                 fr/jmmc/aspro/gui/icons/*.png \
                 fr/jmmc/aspro/model/*.xml \
                 fr/jmmc/aspro/model/searchCal/*.xsl \
                 fr/jmmc/aspro/ob/*.obx

#
# Java programs
# ---------------------------------
JAVA_EXECUTABLES= AsproGUI

#
# <brief description of jjjjjj java program>
AsproGUI_CLASS    = fr.jmmc.aspro.AsproGui

#
# Command Definition Files (CDF)
# ---------------------------------
CDF_FILES       =

#
# man pages to be done
# --------------------
MANSECTIONS =
MAN1 =
MAN3 =
MAN5 =
MAN7 =
MAN8 =

#
# other files to be installed
#----------------------------
INSTALL_FILES =

#
# list of all possible C-sources (used to create automatic dependencies)
# ----------------------------------------------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
#>>>>> END OF standard rules

#
# INCLUDE STANDARDS
# -----------------
MAKEDIR = $(MCSROOT)/include
include $(MAKEDIR)/mkfMakefile

JNLP=Aspro2
#
# TARGETS
# -------
all:	do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo "Deploy $(JNLP).jnlp"
	jmcsDeployJnlp $(JNLP).jnlp

	@echo " . . . installation done"

#___oOo___
