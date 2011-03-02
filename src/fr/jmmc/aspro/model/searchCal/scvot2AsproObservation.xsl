<?xml version="1.0"?>
<!--
********************************************************************************
 JMMC project

 "@(#) $Id: scvot2AsproObservation.xsl,v 1.5 2011-03-02 17:36:03 bourgesl Exp $"

 History
 ~~~~~~~
 $Log: not supported by cvs2svn $
 Revision 1.4  2010/10/22 11:10:20  bourgesl
 use deletedFlag to filter calibrators to mimic SearchCal delete action
 use HD identifier instead of HIP identifier to mimic SearchCal view

 Revision 1.3  2010/10/07 15:06:26  bourgesl
 added searchCal object name (target name in aspro) in observation name

 Revision 1.2  2010/10/07 13:30:39  mella
 Add one comment that explain why we do only consider calibrators with UDD_X diameters

 Revision 1.1  2010/10/07 11:55:10  bourgesl
 xslt to transform searchCal votable to Aspro 2 observation (targets)

********************************************************************************
 NAME
 scvot2AsproObservation.xsl - SearchCal Votable into Aspro2 observation setting

 DESCRIPTION
 This stylesheet transform one SearchCal Votable into one aspro observation
 setting with one target per calibrator that get one diameter.

-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:VOT11="http://www.ivoa.net/xml/VOTable/v1.1" >

  <xsl:output omit-xml-declaration="yes" indent="yes"/>


  <xsl:template match="/">

    <!-- Get column indexes used to build the target list -->
    <xsl:variable name="HIP_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">HIP</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="HD_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">HD</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="twoMASS_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">2MASS</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="DM_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">DM</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="RA_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">RAJ2000</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="DEC_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">DEJ2000</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="PMRA_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">pmRa</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="PMDEC_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">pmDec</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="V_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">V</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="H_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">H</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="I_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">I</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="J_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">J</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="K_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">K</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="N_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">N</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="SpType_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">SpType</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="Parallax_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">plx</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="ParallaxErr_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">e_Plx</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="RadVel_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">RadVel</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <!-- use SearchCal band -->
    <xsl:variable name="diam_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">
          <xsl:value-of select="concat('UD_', /VOT11:VOTABLE/VOT11:RESOURCE/VOT11:TABLE/VOT11:PARAM[@name='band']/@value)"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="UD_U_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">UD_U</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="UD_B_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">UD_B</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="UD_V_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">UD_V</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="UD_R_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">UD_R</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="UD_I_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">UD_I</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="UD_J_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">UD_J</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="UD_H_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">UD_H</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="UD_K_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">UD_K</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="UD_L_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">UD_L</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="UD_N_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">UD_N</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="LD_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">LD</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="deletedFlag_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">deletedFlag</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="EQUINOX" select="translate(/VOT11:VOTABLE/VOT11:COOSYS/@equinox, 'J', '')"/>

    <xsl:variable name="TARGET" select="/VOT11:VOTABLE/VOT11:RESOURCE/VOT11:TABLE/VOT11:PARAM[@name = 'objectName']/@value"/>


<!-- starting output document -->
    <ns2:observationSetting
      xmlns:ns2="http://www.jmmc.fr/aspro-oi/0.1"
      xmlns:ns3="http://www.jmmc.fr/jmcs/models/0.1"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://www.jmmc.fr/aspro-oi/0.1 AsproOIModel.xsd http://www.jmmc.fr/jmcs/models/0.1 targetModel.xsd">

      <xsl:comment>Science Object Name</xsl:comment>

      <name>
        <xsl:value-of select="$TARGET"/>
      </name>


      <xsl:comment>Calibrators</xsl:comment>

        <!-- Build one target element per calibrator -->
      <xsl:for-each select=".//VOT11:TR">

        <xsl:variable name="deletedFlag" select="./VOT11:TD[position()=$deletedFlag_index]"/>

<!--                <xsl:comment>deletedFlag = <xsl:value-of select="$deletedFlag"/></xsl:comment> -->

        <xsl:if test="$deletedFlag = 'false'">

          <xsl:variable name="ID_HD">
            <xsl:choose>
              <xsl:when test="./VOT11:TD[position()=$HD_index]/text()">
                <xsl:value-of select="concat('HD ',./VOT11:TD[position()=$HD_index])"/>
              </xsl:when>
              <xsl:otherwise>UNDEFINED</xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          <xsl:variable name="ID_HIP">
            <xsl:choose>
              <xsl:when test="./VOT11:TD[position()=$HIP_index]/text()">
              <xsl:value-of select="concat('HIP ',./VOT11:TD[position()=$HIP_index])"/>
              </xsl:when>
              <xsl:otherwise>UNDEFINED</xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          <xsl:variable name="ID_twoMASS">
            <xsl:choose>
              <xsl:when test="./VOT11:TD[position()=$twoMASS_index]/text()">
              <xsl:value-of select="concat('2MASS ',./VOT11:TD[position()=$twoMASS_index])"/>
              </xsl:when>
              <xsl:otherwise>UNDEFINED</xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          <xsl:variable name="ID_DM">
            <xsl:choose>
              <xsl:when test="./VOT11:TD[position()=$DM_index]/text()">
              <xsl:value-of select="concat('DM ',./VOT11:TD[position()=$DM_index])"/>
              </xsl:when>
              <xsl:otherwise>UNDEFINED</xsl:otherwise>
            </xsl:choose>
          </xsl:variable>

          <xsl:variable name="name">
            <xsl:choose>
              <xsl:when test="$ID_HD != 'UNDEFINED'">
                <xsl:value-of select="$ID_HD"/>
              </xsl:when>
              <xsl:when test="$ID_HIP != 'UNDEFINED'">
                <xsl:value-of select="$ID_HIP"/>
              </xsl:when>
              <xsl:when test="$ID_twoMASS != 'UNDEFINED'">
                <xsl:value-of select="$ID_twoMASS"/>
              </xsl:when>
              <xsl:when test="$ID_DM != 'UNDEFINED'">
                <xsl:value-of select="$ID_DM"/>
              </xsl:when>
            </xsl:choose>
          </xsl:variable>

          <xsl:variable name="RA"       select="translate(./VOT11:TD[position()=$RA_index], ' ', ':')"/>
          <xsl:variable name="PMRA"     select="./VOT11:TD[position()=$PMRA_index]"/>
          <xsl:variable name="DEC"      select="translate(./VOT11:TD[position()=$DEC_index], ' ', ':')"/>
          <xsl:variable name="PMDEC"    select="./VOT11:TD[position()=$PMDEC_index]"/>
          <xsl:variable name="FLUX_H"   select="./VOT11:TD[position()=$H_index]"/>
          <xsl:variable name="FLUX_I"   select="./VOT11:TD[position()=$I_index]"/>
          <xsl:variable name="FLUX_J"   select="./VOT11:TD[position()=$J_index]"/>
          <xsl:variable name="FLUX_K"   select="./VOT11:TD[position()=$K_index]"/>
          <xsl:variable name="FLUX_N"   select="./VOT11:TD[position()=$N_index]"/>
          <xsl:variable name="FLUX_V"   select="./VOT11:TD[position()=$V_index]"/>
          <xsl:variable name="SPECTYP"  select="./VOT11:TD[position()=$SpType_index]"/>
          <xsl:variable name="PARALLAX" select="./VOT11:TD[position()=$Parallax_index]"/>
          <xsl:variable name="PARA_ERR" select="./VOT11:TD[position()=$ParallaxErr_index]"/>
          <xsl:variable name="SYSVEL"   select="./VOT11:TD[position()=$RadVel_index]"/>
          <xsl:variable name="diam"     select="./VOT11:TD[position()=$diam_index]"/>
          <xsl:variable name="UD_U"     select="./VOT11:TD[position()=$UD_U_index]"/>
          <xsl:variable name="UD_B"     select="./VOT11:TD[position()=$UD_B_index]"/>
          <xsl:variable name="UD_V"     select="./VOT11:TD[position()=$UD_V_index]"/>
          <xsl:variable name="UD_R"     select="./VOT11:TD[position()=$UD_R_index]"/>
          <xsl:variable name="UD_I"     select="./VOT11:TD[position()=$UD_I_index]"/>
          <xsl:variable name="UD_J"     select="./VOT11:TD[position()=$UD_J_index]"/>
          <xsl:variable name="UD_H"     select="./VOT11:TD[position()=$UD_H_index]"/>
          <xsl:variable name="UD_K"     select="./VOT11:TD[position()=$UD_K_index]"/>
          <xsl:variable name="UD_L"     select="./VOT11:TD[position()=$UD_L_index]"/>
          <xsl:variable name="UD_N"     select="./VOT11:TD[position()=$UD_N_index]"/>
          <xsl:variable name="LD"       select="./VOT11:TD[position()=$LD_index]"/>

          <target>
            <!-- no id attribute -->

            <!-- identifier -->
            <name>
              <xsl:value-of select="$name"/>
            </name>

            <!-- position -->
            <RA>
              <xsl:value-of select="$RA"/>
            </RA>
            <DEC>
              <xsl:value-of select="$DEC"/>
            </DEC>
            <EQUINOX>
              <xsl:value-of select="$EQUINOX"/>
            </EQUINOX>

            <!-- radial velocity -->
            <xsl:if test="$SYSVEL/text()">
              <SYSVEL>
                <xsl:value-of select="$SYSVEL"/>
              </SYSVEL>
            </xsl:if>
            <!-- missing in scvot <VELTYP/> -->

            <!-- proper motion -->
            <xsl:if test="$PMRA/text()">
              <PMRA>
                <xsl:value-of select="$PMRA"/>
              </PMRA>
            </xsl:if>
            <xsl:if test="$PMDEC/text()">
              <PMDEC>
                <xsl:value-of select="$PMDEC"/>
              </PMDEC>
            </xsl:if>

            <!-- parallax -->
            <xsl:if test="$PARALLAX/text()">
              <PARALLAX>
                <xsl:value-of select="$PARALLAX"/>
              </PARALLAX>
            </xsl:if>
            <xsl:if test="$PARA_ERR/text()">
              <PARA_ERR>
                <xsl:value-of select="$PARA_ERR"/>
              </PARA_ERR>
            </xsl:if>

            <!-- identifiers (HD, HIP, 2MASS, DM only) -->
            <IDS>
              <xsl:if test="$ID_HD != 'UNDEFINED'"><xsl:value-of select="$ID_HD"/>,</xsl:if>
              <xsl:if test="$ID_HIP != 'UNDEFINED'"><xsl:value-of select="$ID_HIP"/>,</xsl:if>
              <xsl:if test="$ID_twoMASS != 'UNDEFINED'"><xsl:value-of select="$ID_twoMASS"/>,</xsl:if>
              <xsl:if test="$ID_DM != 'UNDEFINED'"><xsl:value-of select="$ID_DM"/>,</xsl:if>
            </IDS>

            <!-- object types -->
            <!-- missing in scvot <OBJTYP> -->

            <!-- spectral types -->
            <xsl:if test="$SPECTYP/text()">
              <SPECTYP>
                <xsl:value-of select="$SPECTYP"/>
              </SPECTYP>
            </xsl:if>

            <!-- magnitudes -->
            <xsl:if test="$FLUX_V/text()">
              <FLUX_V>
                <xsl:value-of select="$FLUX_V"/>
              </FLUX_V>
            </xsl:if>
            <xsl:if test="$FLUX_I/text()">
              <FLUX_I>
                <xsl:value-of select="$FLUX_I"/>
              </FLUX_I>
            </xsl:if>
            <xsl:if test="$FLUX_J/text()">
              <FLUX_J>
                <xsl:value-of select="$FLUX_J"/>
              </FLUX_J>
            </xsl:if>
            <xsl:if test="$FLUX_H/text()">
              <FLUX_H>
                <xsl:value-of select="$FLUX_H"/>
              </FLUX_H>
            </xsl:if>
            <xsl:if test="$FLUX_K/text()">
              <FLUX_K>
                <xsl:value-of select="$FLUX_K"/>
              </FLUX_K>
            </xsl:if>
            <xsl:if test="$FLUX_N/text()">
              <FLUX_N>
                <xsl:value-of select="$FLUX_N"/>
              </FLUX_N>
            </xsl:if>

            <!-- target models = uniform disk model -->
            <ns3:model type="disk" name="disk1">
              <ns3:parameter type="flux_weight" name="flux_weight1">
                <value>1.0</value>
                <minValue>0.0</minValue>
                <hasFixedValue>false</hasFixedValue>
              </ns3:parameter>
              <ns3:parameter type="x" name="x1">
                <units>mas</units>
                <value>0.0</value>
                <hasFixedValue>true</hasFixedValue>
              </ns3:parameter>
              <ns3:parameter type="y" name="y1">
                <units>mas</units>
                <value>0.0</value>
                <hasFixedValue>true</hasFixedValue>
              </ns3:parameter>
              <ns3:parameter type="diameter" name="diameter1">
                <units>mas</units>
                <value>
                  <xsl:choose>
                    <xsl:when test="$diam/text()">
                    <xsl:value-of select="$diam"/>
                    </xsl:when>
                    <xsl:otherwise>0.0</xsl:otherwise>
                  </xsl:choose>
                </value>
                <minValue>0.0</minValue>
                <hasFixedValue>false</hasFixedValue>
              </ns3:parameter>
            </ns3:model>

            <calibratorInfos>

              <xsl:if test="$UD_U/text()">
                <UD_U>
                  <xsl:value-of select="$UD_U"/>
                </UD_U>
              </xsl:if>
              <xsl:if test="$UD_B/text()">
                <UD_B>
                  <xsl:value-of select="$UD_B"/>
                </UD_B>
              </xsl:if>
              <xsl:if test="$UD_V/text()">
                <UD_V>
                  <xsl:value-of select="$UD_V"/>
                </UD_V>
              </xsl:if>
              <xsl:if test="$UD_R/text()">
                <UD_R>
                  <xsl:value-of select="$UD_R"/>
                </UD_R>
              </xsl:if>
              <xsl:if test="$UD_I/text()">
                <UD_I>
                  <xsl:value-of select="$UD_I"/>
                </UD_I>
              </xsl:if>
              <xsl:if test="$UD_J/text()">
                <UD_J>
                  <xsl:value-of select="$UD_J"/>
                </UD_J>
              </xsl:if>
              <xsl:if test="$UD_H/text()">
                <UD_H>
                  <xsl:value-of select="$UD_H"/>
                </UD_H>
              </xsl:if>
              <xsl:if test="$UD_K/text()">
                <UD_K>
                  <xsl:value-of select="$UD_K"/>
                </UD_K>
              </xsl:if>
              <xsl:if test="$UD_L/text()">
                <UD_L>
                  <xsl:value-of select="$UD_L"/>
                </UD_L>
              </xsl:if>
              <xsl:if test="$UD_N/text()">
                <UD_N>
                  <xsl:value-of select="$UD_N"/>
                </UD_N>
              </xsl:if>

              <xsl:if test="$LD/text()">
                <LD>
                  <xsl:value-of select="$LD"/>
                </LD>
              </xsl:if>

            </calibratorInfos>

          </target>

        </xsl:if> <!-- deletedFlag -->

      </xsl:for-each>

    </ns2:observationSetting>

  </xsl:template>


  <xsl:template name="getColumnIndex">
    <xsl:param name="colName"/>
    <xsl:for-each select="/VOT11:VOTABLE/VOT11:RESOURCE/VOT11:TABLE/VOT11:FIELD">
      <xsl:if test="@name=$colName">
        <xsl:value-of select="position()" />
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>
