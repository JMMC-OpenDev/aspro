<?xml version="1.0"?>
<!--
 *******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************

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
    <xsl:apply-templates select="/VOT11:VOTABLE/VOT11:RESOURCE[starts-with(@name,'SearchCal')]" />
  </xsl:template>





  <xsl:template match="/VOT11:VOTABLE/VOT11:RESOURCE">

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
    <xsl:variable name="SBC9_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">SBC9</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="WDS_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">WDS</xsl:with-param>
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

    <xsl:variable name="deletedFlag_index">
      <xsl:call-template name="getColumnIndex">
        <xsl:with-param name="colName">deletedFlag</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="TARGET" select="/VOT11:VOTABLE/VOT11:RESOURCE/VOT11:TABLE/VOT11:PARAM[@name = 'objectName']/@value"/>

    <!-- TODO: check equinox and HMS / DMS are really J2000 -->
    <xsl:variable name="EQUINOX" select="translate(/VOT11:VOTABLE/VOT11:COOSYS/@equinox, 'J', '')"/>


    <!-- SearchCal VOTABLE PARAM elements to copy -->
    <xsl:variable name="COPY_PARAMS"
      select="'band|baseMax|wlen|bright'"/>

    <!-- SearchCal VOTABLE FIELD elements to copy -->
<!--
    <xsl:variable name="COPY_FIELDS"
      select="'|LD|e_LD|UD|e_UD|UDDK|e_UDDK|Dia12|e_dia12|Teff|e_Teff|Teff_SpType|logg_SpType|UD_U|UD_B|UD_V|UD_R|UD_I|UD_J|UD_H|UD_K|UD_L|UD_N|'"/>
  -->

<!-- starting output document -->
    <a:observationSetting
      xmlns:a="http://www.jmmc.fr/aspro-oi/0.1"
      xmlns:tm="http://www.jmmc.fr/jmcs/models/0.1"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://www.jmmc.fr/aspro-oi/0.1 AsproOIModel.xsd http://www.jmmc.fr/jmcs/models/0.1 targetModel.xsd">

      <xsl:comment>Science Object Name</xsl:comment>
      <name>
        <xsl:value-of select="$TARGET"/>
      </name>

      <xsl:comment>Calibrators</xsl:comment>

      <!-- Build one target element per calibrator -->
      <xsl:for-each select="/VOT11:VOTABLE/VOT11:RESOURCE/VOT11:TABLE/VOT11:DATA/VOT11:TABLEDATA/VOT11:TR">

        <xsl:variable name="deletedFlag" select="./VOT11:TD[position()=$deletedFlag_index]"/>

        <xsl:if test="$deletedFlag = 'false'">

          <xsl:variable name="ID_HD">
            <xsl:if test="./VOT11:TD[position()=$HD_index]/text()">
              <xsl:value-of select="concat('HD ',./VOT11:TD[position()=$HD_index])"/>
            </xsl:if>
          </xsl:variable>
          <xsl:variable name="ID_HIP">
            <xsl:if test="./VOT11:TD[position()=$HIP_index]/text()">
              <xsl:value-of select="concat('HIP ',./VOT11:TD[position()=$HIP_index])"/>
            </xsl:if>
          </xsl:variable>
          <xsl:variable name="ID_twoMASS">
            <xsl:if test="./VOT11:TD[position()=$twoMASS_index]/text()">
              <xsl:value-of select="concat('2MASS J',./VOT11:TD[position()=$twoMASS_index])"/>
            </xsl:if>
          </xsl:variable>
          <xsl:variable name="ID_DM">
            <xsl:if test="./VOT11:TD[position()=$DM_index]/text()">
              <xsl:value-of select="concat('DM ',./VOT11:TD[position()=$DM_index])"/>
            </xsl:if>
          </xsl:variable>
          <xsl:variable name="ID_SBC9">
            <xsl:if test="./VOT11:TD[position()=$SBC9_index]/text()">
              <xsl:value-of select="concat('SBC9 ',./VOT11:TD[position()=$SBC9_index])"/>
            </xsl:if>
          </xsl:variable>
          <xsl:variable name="ID_WDS">
            <xsl:if test="./VOT11:TD[position()=$WDS_index]/text()">
              <xsl:value-of select="concat('WDS J',./VOT11:TD[position()=$WDS_index])"/>
            </xsl:if>
          </xsl:variable>

          <xsl:variable name="name">
            <xsl:choose>
              <xsl:when test="$ID_HD != ''">
                <xsl:value-of select="$ID_HD"/>
              </xsl:when>
              <xsl:when test="$ID_HIP != ''">
                <xsl:value-of select="$ID_HIP"/>
              </xsl:when>
              <xsl:when test="$ID_twoMASS != ''">
                <xsl:value-of select="$ID_twoMASS"/>
              </xsl:when>
              <xsl:when test="$ID_DM != ''">
                <xsl:value-of select="$ID_DM"/>
              </xsl:when>
              <xsl:when test="$ID_SBC9 != ''">
                <xsl:value-of select="$ID_SBC9"/>
              </xsl:when>
              <xsl:when test="$ID_WDS != ''">
                <xsl:value-of select="$ID_WDS"/>
              </xsl:when>
            </xsl:choose>
          </xsl:variable>

          <xsl:variable name="RA"       select="translate(./VOT11:TD[position()=$RA_index], ' ', ':')"/>
          <xsl:variable name="PMRA"     select="./VOT11:TD[position()=$PMRA_index]"/>
          <xsl:variable name="DEC"      select="translate(./VOT11:TD[position()=$DEC_index], ' ', ':')"/>
          <xsl:variable name="PMDEC"    select="./VOT11:TD[position()=$PMDEC_index]"/>
          <xsl:variable name="FLUX_V"   select="./VOT11:TD[position()=$V_index]"/>
          <xsl:variable name="FLUX_H"   select="./VOT11:TD[position()=$H_index]"/>
          <xsl:variable name="FLUX_I"   select="./VOT11:TD[position()=$I_index]"/>
          <xsl:variable name="FLUX_J"   select="./VOT11:TD[position()=$J_index]"/>
          <xsl:variable name="FLUX_K"   select="./VOT11:TD[position()=$K_index]"/>
          <xsl:variable name="FLUX_N"   select="./VOT11:TD[position()=$N_index]"/>
          <xsl:variable name="SPECTYP"  select="./VOT11:TD[position()=$SpType_index]"/>
          <xsl:variable name="PARALLAX" select="./VOT11:TD[position()=$Parallax_index]"/>
          <xsl:variable name="PARA_ERR" select="./VOT11:TD[position()=$ParallaxErr_index]"/>
          <xsl:variable name="SYSVEL"   select="./VOT11:TD[position()=$RadVel_index]"/>


          <target>
            <!-- no id attribute : defined by Aspro2 -->

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

            <!-- identifiers (HD, HIP, 2MASS, DM, SBC9, WDS only) -->
            <IDS>
              <xsl:if test="$ID_HD != ''"><xsl:value-of select="$ID_HD"/>,</xsl:if>
              <xsl:if test="$ID_HIP != ''"><xsl:value-of select="$ID_HIP"/>,</xsl:if>
              <xsl:if test="$ID_twoMASS != ''"><xsl:value-of select="$ID_twoMASS"/>,</xsl:if>
              <xsl:if test="$ID_DM != ''"><xsl:value-of select="$ID_DM"/>,</xsl:if>
              <xsl:if test="$ID_SBC9 != ''"><xsl:value-of select="$ID_SBC9"/>,</xsl:if>
              <xsl:if test="$ID_WDS != ''"><xsl:value-of select="$ID_WDS"/>,</xsl:if>
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
            <tm:model type="disk" name="disk1">
              <tm:parameter type="flux_weight" name="flux_weight1">
                <value>1.0</value>
                <minValue>0.0</minValue>
                <hasFixedValue>false</hasFixedValue>
              </tm:parameter>
              <tm:parameter type="x" name="x1">
                <units>mas</units>
                <value>0.0</value>
                <hasFixedValue>true</hasFixedValue>
              </tm:parameter>
              <tm:parameter type="y" name="y1">
                <units>mas</units>
                <value>0.0</value>
                <hasFixedValue>true</hasFixedValue>
              </tm:parameter>
              <tm:parameter type="diameter" name="diameter1">
                <units>mas</units>
                <!-- diameter value is defined by Aspro2 -->
                <value>0.0</value>
                <minValue>0.0</minValue>
                <hasFixedValue>false</hasFixedValue>
              </tm:parameter>
            </tm:model>

            <calibratorInfos>

              <parameter xsi:type="a:StringValue" name="SearchCalServerVersion" value="{/VOT11:VOTABLE/VOT11:RESOURCE/@name}"/>

              <!-- Build one parameter element per VOTable PARAM present in COPY_PARAMS -->
              <xsl:for-each select="/VOT11:VOTABLE/VOT11:RESOURCE/VOT11:TABLE/VOT11:PARAM[contains($COPY_PARAMS, @name) and @value]">
                <parameter>
                  <xsl:attribute name="xsi:type">
                    <xsl:choose>
                      <xsl:when test="@datatype = 'double' or @datatype = 'float'">a:NumberValue</xsl:when>
                      <xsl:when test="@datatype = 'boolean'">a:BooleanValue</xsl:when>
                      <xsl:otherwise>a:StringValue</xsl:otherwise>
                    </xsl:choose>
                  </xsl:attribute>
                  <xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
                  <xsl:attribute name="value"><xsl:value-of select="@value" /></xsl:attribute>
                  <xsl:if test="@unit">
                    <xsl:attribute name="unit"><xsl:value-of select="@unit" /></xsl:attribute>
                  </xsl:if>
                </parameter>
              </xsl:for-each>


              <!-- Build one field element per VOTable FIELD -->
              <xsl:for-each select="VOT11:TD">
                <xsl:variable name="pos" select="position()" />
                <xsl:variable name="field" select="/VOT11:VOTABLE/VOT11:RESOURCE/VOT11:TABLE/VOT11:FIELD[position() = $pos]" />

                <!-- skip blank values and unused columns (origin and confidence and many other data columns ...) -->
<!--
                <xsl:if test="contains($COPY_FIELDS, concat('|', $field/@name, '|')) and text() != '-'">
  -->
                <xsl:if test="text() != '-' and contains($field/@name, '.origin') = false and contains($field/@name, '.confidence') = false">
                  <field>
                    <xsl:attribute name="xsi:type">
                      <xsl:choose>
                        <xsl:when test="$field/@datatype = 'double' or $field/@datatype = 'float'">a:NumberValue</xsl:when>
                        <xsl:when test="$field/@datatype = 'boolean'">a:BooleanValue</xsl:when>
                        <xsl:otherwise>a:StringValue</xsl:otherwise>
                      </xsl:choose>
                    </xsl:attribute>
                    <xsl:attribute name="name"><xsl:value-of select="$field/@name" /></xsl:attribute>
                    <xsl:attribute name="value"><xsl:value-of select="text()" /></xsl:attribute>
                    <xsl:if test="$field/@unit">
                      <xsl:attribute name="unit"><xsl:value-of select="$field/@unit" /></xsl:attribute>
                    </xsl:if>
<!--
                    <xsl:attribute name="description"><xsl:value-of select="$field/VOT11:DESCRIPTION/text()" /></xsl:attribute>
  -->
                  </field>
                </xsl:if>
              </xsl:for-each>

            </calibratorInfos>

          </target>

        </xsl:if> <!-- deletedFlag -->

      </xsl:for-each>

    </a:observationSetting>

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
