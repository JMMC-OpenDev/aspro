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

 TODO: Get SearchCal GUI / Server version as PARAM (not resource name that can be cleared as topcat does)

-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:VOT="http://www.ivoa.net/xml/VOTable/v1.1"
                exclude-result-prefixes="VOT">

    <xsl:output omit-xml-declaration="yes" indent="yes" encoding="UTF-8"/>

    <!-- SearchCal VOTABLE PARAM elements to copy -->
    <xsl:variable name="COPY_PARAMS" select="'band|baseMax|wlen|bright|SearchCalGuiVersion|'"/>

    <!-- TABLE proxy -->
    <xsl:variable name="table" select="/VOT:VOTABLE/VOT:RESOURCE/VOT:TABLE"/>




    <xsl:template match="/">
        <xsl:apply-templates select="/VOT:VOTABLE/VOT:RESOURCE" />
    </xsl:template>





    <xsl:template match="VOT:RESOURCE">

        <!-- debug fields -->
        <!--
        <xsl:for-each select="$table/VOT:FIELD">
            <xsl:message>ColumnIndex('<xsl:value-of select="@name"/>') = position = <xsl:value-of select="position()"/> </xsl:message>
        </xsl:for-each>
        -->

        <!-- Get column indexes used to build the target list -->
        <xsl:variable name="SIMBAD_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">SIMBAD</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="GAIA_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">GAIA</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="HIP_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">HIP</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="TYC1_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">TYC1</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="TYC2_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">TYC2</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="TYC3_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">TYC3</xsl:with-param>
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
        <xsl:variable name="WISE_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">WISE</xsl:with-param>
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

        <xsl:variable name="B_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">B</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="BP_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">Bp</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="V_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">V</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="G_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">G</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="R_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">R</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="RP_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">Rp</xsl:with-param>
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
        <xsl:variable name="H_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">H</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="K_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">K</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="L_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">L</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="M_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">M</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="N_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">N</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="ObjTypes_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="colName">ObjTypes</xsl:with-param>
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

        <xsl:variable name="RESOURCE_NAME" select="@name"/>

        <!-- check FIELD (RA and DEC only) -->
        <xsl:if test="$RA_index != '' and $DEC_index != ''">

            <!-- PARAM information used -->
            <xsl:variable name="SCLSVR_CMD"     select="$table/VOT:PARAM[@name = 'ServerCommand']/@value"/>
            <xsl:variable name="SCLSVR_VERSION" select="$table/VOT:PARAM[@name = 'SearchCalServerVersion']/@value"/>
            <xsl:variable name="TARGET"         select="$table/VOT:PARAM[@name = 'objectName']/@value"/>

            <!-- check PARAM (objectName|ServerCommand|SearchCalServerVersion) -->
            <xsl:if test="$TARGET != '' and $SCLSVR_CMD != '' and $SCLSVR_VERSION != '' ">

                <!-- TODO: check equinox and HMS / DMS are really J2000 -->
                <xsl:variable name="EQUINOX" select="translate(/VOT:VOTABLE/VOT:COOSYS/@equinox, 'J', '')"/>


                <!-- starting output document -->
                <a:observationSetting
                    xmlns:a="http://www.jmmc.fr/aspro-oi/0.1"
                    xmlns:tm="http://www.jmmc.fr/jmcs/models/0.1"
                    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                    xsi:schemaLocation="http://www.jmmc.fr/aspro-oi/0.1 AsproOIModel.xsd http://www.jmmc.fr/jmcs/models/0.1 targetModel.xsd">

                    <schemaVersion>2018.04</schemaVersion>
                    <targetVersion>2024.09</targetVersion>

                    <name>
                        <xsl:value-of select="$SCLSVR_CMD"/>
                    </name>

                    <xsl:variable name="TARGET_RA"      select="translate($table/VOT:PARAM[@name = 'ra']/@value, ' ', ':')"/>
                    <xsl:variable name="TARGET_DEC"     select="translate($table/VOT:PARAM[@name = 'dec']/@value, ' ', ':')"/>

                    <!-- check PARAM (ra/dec) -->
                    <xsl:if test="$TARGET_RA != '' and $TARGET_DEC != '' ">

                        <xsl:comment>Science Object Name</xsl:comment>
                        <target>
                            <!-- no id attribute : defined by Aspro2 -->

                            <!-- identifier -->
                            <name>
                                <xsl:value-of select="$TARGET"/>
                            </name>

                            <!-- position -->
                            <RA>
                                <xsl:value-of select="$TARGET_RA"/>
                            </RA>
                            <DEC>
                                <xsl:value-of select="$TARGET_DEC"/>
                            </DEC>
                            <EQUINOX>
                                <xsl:value-of select="$EQUINOX"/>
                            </EQUINOX>

                            <xsl:variable name="TARGET_MAG"     select="$table/VOT:PARAM[@name = 'mag']/@value"/>
                            <xsl:variable name="BAND"           select="$table/VOT:PARAM[@name = 'band']/@value"/>

                            <xsl:if test="$BAND != '' and $TARGET_MAG != '' ">

                                <!-- magnitudes -->
                                <xsl:if test="$BAND = 'B'">
                                    <FLUX_B>
                                        <xsl:value-of select="$TARGET_MAG"/>
                                    </FLUX_B>
                                </xsl:if>
                                <xsl:if test="$BAND = 'V'">
                                    <FLUX_V>
                                        <xsl:value-of select="$TARGET_MAG"/>
                                    </FLUX_V>
                                </xsl:if>
                                <xsl:if test="$BAND = 'R'">
                                    <FLUX_R>
                                        <xsl:value-of select="$TARGET_MAG"/>
                                    </FLUX_R>
                                </xsl:if>
                                <xsl:if test="$BAND = 'I'">
                                    <FLUX_I>
                                        <xsl:value-of select="$TARGET_MAG"/>
                                    </FLUX_I>
                                </xsl:if>
                                <xsl:if test="$BAND = 'J'">
                                    <FLUX_J>
                                        <xsl:value-of select="$TARGET_MAG"/>
                                    </FLUX_J>
                                </xsl:if>
                                <xsl:if test="$BAND = 'H'">
                                    <FLUX_H>
                                        <xsl:value-of select="$TARGET_MAG"/>
                                    </FLUX_H>
                                </xsl:if>
                                <xsl:if test="$BAND = 'K'">
                                    <FLUX_K>
                                        <xsl:value-of select="$TARGET_MAG"/>
                                    </FLUX_K>
                                </xsl:if>
                                <xsl:if test="$BAND = 'L'">
                                    <FLUX_L>
                                        <xsl:value-of select="$TARGET_MAG"/>
                                    </FLUX_L>
                                </xsl:if>
                                <xsl:if test="$BAND = 'M'">
                                    <FLUX_M>
                                        <xsl:value-of select="$TARGET_MAG"/>
                                    </FLUX_M>
                                </xsl:if>
                                <xsl:if test="$BAND = 'N'">
                                    <FLUX_N>
                                        <xsl:value-of select="$TARGET_MAG"/>
                                    </FLUX_N>
                                </xsl:if>
                            </xsl:if>

                        </target>
                    </xsl:if>


                    <xsl:comment>Calibrators</xsl:comment>

                    <!-- Build one target element per calibrator -->
                    <xsl:for-each select="$table/VOT:DATA/VOT:TABLEDATA/VOT:TR">

                        <xsl:variable name="deletedFlag" select="VOT:TD[number($deletedFlag_index)]/text()"/>

                        <!-- Note: topcat convert boolean to 'F' instead of 'false' -->
                        <xsl:if test="not($deletedFlag) or $deletedFlag = '0' or $deletedFlag = 'false' or $deletedFlag = 'F'">

                            <xsl:variable name="ID_SIMBAD">
                                <xsl:if test="VOT:TD[number($SIMBAD_index)]/text()">
                                    <xsl:value-of select="VOT:TD[number($SIMBAD_index)]"/>
                                </xsl:if>
                            </xsl:variable>
                            <xsl:variable name="ID_GAIA">
                                <xsl:if test="VOT:TD[number($GAIA_index)]/text()">
                                    <xsl:value-of select="concat('GAIA DR3 ',VOT:TD[number($GAIA_index)])"/>
                                </xsl:if>
                            </xsl:variable>
                            <xsl:variable name="ID_HD">
                                <xsl:if test="VOT:TD[number($HD_index)]/text()">
                                    <xsl:value-of select="concat('HD ',VOT:TD[number($HD_index)])"/>
                                </xsl:if>
                            </xsl:variable>
                            <xsl:variable name="ID_HIP">
                                <xsl:if test="VOT:TD[number($HIP_index)]/text()">
                                    <xsl:value-of select="concat('HIP ',VOT:TD[number($HIP_index)])"/>
                                </xsl:if>
                            </xsl:variable>
                            <xsl:variable name="ID_TYCHO">
                                <xsl:if test="VOT:TD[number($TYC1_index)]/text() and VOT:TD[number($TYC2_index)]/text() and VOT:TD[number($TYC3_index)]/text()">
                                    <xsl:value-of select="concat('TYC ',VOT:TD[number($TYC1_index)],'-',VOT:TD[number($TYC2_index)],'-',VOT:TD[number($TYC3_index)])"/>
                                </xsl:if>
                            </xsl:variable>
                            <xsl:variable name="ID_twoMASS">
                                <xsl:if test="VOT:TD[number($twoMASS_index)]/text()">
                                    <xsl:value-of select="concat('2MASS J',VOT:TD[number($twoMASS_index)])"/>
                                </xsl:if>
                            </xsl:variable>
                            <xsl:variable name="ID_WISE">
                                <xsl:if test="VOT:TD[number($WISE_index)]/text()">
                                    <xsl:value-of select="concat('WISEA ',VOT:TD[number($WISE_index)])"/>
                                </xsl:if>
                            </xsl:variable>
                            <xsl:variable name="ID_DM">
                                <xsl:if test="VOT:TD[number($DM_index)]/text()">
                                    <xsl:value-of select="concat('DM ',VOT:TD[number($DM_index)])"/>
                                </xsl:if>
                            </xsl:variable>
                            <xsl:variable name="ID_SBC9">
                                <xsl:if test="VOT:TD[number($SBC9_index)]/text()">
                                    <xsl:value-of select="concat('SBC9 ',VOT:TD[number($SBC9_index)])"/>
                                </xsl:if>
                            </xsl:variable>
                            <xsl:variable name="ID_WDS">
                                <xsl:if test="VOT:TD[number($WDS_index)]/text()">
                                    <xsl:value-of select="concat('WDS J',VOT:TD[number($WDS_index)])"/>
                                </xsl:if>
                            </xsl:variable>

                            <xsl:variable name="RA2000" select="VOT:TD[number($RA_index)]/text()"/>
                            <xsl:variable name="DE2000" select="VOT:TD[number($DEC_index)]/text()"/>

                            <xsl:variable name="name">
                                <xsl:choose>
                                    <xsl:when test="$ID_SIMBAD != ''">
                                        <xsl:value-of select="$ID_SIMBAD"/>
                                    </xsl:when>
                                    <xsl:when test="$ID_GAIA != ''">
                                        <xsl:value-of select="$ID_GAIA"/>
                                    </xsl:when>
                                    <xsl:when test="$ID_HIP != ''">
                                        <xsl:value-of select="$ID_HIP"/>
                                    </xsl:when>
                                    <xsl:when test="$ID_TYCHO != ''">
                                        <xsl:value-of select="$ID_TYCHO"/>
                                    </xsl:when>
                                    <xsl:when test="$ID_twoMASS != ''">
                                        <xsl:value-of select="$ID_twoMASS"/>
                                    </xsl:when>
                                    <xsl:when test="$ID_WISE != ''">
                                        <xsl:value-of select="$ID_WISE"/>
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
                                    <xsl:when test="$ID_HD != ''">
                                        <xsl:value-of select="$ID_HD"/>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <xsl:value-of select="concat($RA2000, ' ', $DE2000)"/>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:variable>

                            <xsl:variable name="RA"       select="translate($RA2000, ' ', ':')"/>
                            <xsl:variable name="DEC"      select="translate($DE2000, ' ', ':')"/>
                            <xsl:variable name="PMRA"     select="VOT:TD[number($PMRA_index)]"/>
                            <xsl:variable name="PMDEC"    select="VOT:TD[number($PMDEC_index)]"/>

                            <xsl:variable name="FLUX_B"   select="VOT:TD[number($B_index)]"/>
                            <xsl:variable name="FLUX_BP"  select="VOT:TD[number($BP_index)]"/>
                            <xsl:variable name="FLUX_V"   select="VOT:TD[number($V_index)]"/>
                            <xsl:variable name="FLUX_G"   select="VOT:TD[number($G_index)]"/>
                            <xsl:variable name="FLUX_R"   select="VOT:TD[number($R_index)]"/>
                            <xsl:variable name="FLUX_RP"  select="VOT:TD[number($RP_index)]"/>
                            <xsl:variable name="FLUX_I"   select="VOT:TD[number($I_index)]"/>
                            <xsl:variable name="FLUX_J"   select="VOT:TD[number($J_index)]"/>
                            <xsl:variable name="FLUX_H"   select="VOT:TD[number($H_index)]"/>
                            <xsl:variable name="FLUX_K"   select="VOT:TD[number($K_index)]"/>
                            <xsl:variable name="FLUX_L"   select="VOT:TD[number($L_index)]"/>
                            <xsl:variable name="FLUX_M"   select="VOT:TD[number($M_index)]"/>
                            <xsl:variable name="FLUX_N"   select="VOT:TD[number($N_index)]"/>

                            <xsl:variable name="OBJTYPES" select="VOT:TD[number($ObjTypes_index)]"/>
                            <xsl:variable name="SPECTYP"  select="VOT:TD[number($SpType_index)]"/>

                            <xsl:variable name="PARALLAX" select="VOT:TD[number($Parallax_index)]"/>
                            <xsl:variable name="PARA_ERR" select="VOT:TD[number($ParallaxErr_index)]"/>
                            <xsl:variable name="SYSVEL"   select="VOT:TD[number($RadVel_index)]"/>

                            <!-- debug fields -->
                            <!--
                            <xsl:for-each select="VOT:TD">
                                <xsl:message>TD[<xsl:value-of select="position()"/> (<xsl:value-of select="$table/VOT:FIELD[number(position())]/@name"/>) ] = '<xsl:value-of select="text()"/>'</xsl:message>
                            </xsl:for-each>
                            -->

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

                                <!-- identifiers -->
                                <IDS>
                                    <xsl:if test="$ID_DM != ''">
                                        <xsl:value-of select="$ID_DM"/>,</xsl:if>
                                    <xsl:if test="$ID_GAIA != ''">
                                        <xsl:value-of select="$ID_GAIA"/>,</xsl:if>
                                    <xsl:if test="$ID_HIP != ''">
                                        <xsl:value-of select="$ID_HIP"/>,</xsl:if>
                                    <xsl:if test="$ID_TYCHO != ''">
                                        <xsl:value-of select="$ID_TYCHO"/>,</xsl:if>
                                    <xsl:if test="$ID_HD != ''">
                                        <xsl:value-of select="$ID_HD"/>,</xsl:if>
                                    <xsl:if test="$ID_twoMASS != ''">
                                        <xsl:value-of select="$ID_twoMASS"/>,</xsl:if>
                                    <xsl:if test="$ID_WISE != ''">
                                        <xsl:value-of select="$ID_WISE"/>,</xsl:if>
                                    <xsl:if test="$ID_SBC9 != ''">
                                        <xsl:value-of select="$ID_SBC9"/>,</xsl:if>
                                    <xsl:if test="$ID_WDS != ''">
                                        <xsl:value-of select="$ID_WDS"/>,</xsl:if>
                                </IDS>

                                <!-- object types -->
                                <xsl:if test="$OBJTYPES/text()">
                                    <OBJTYP>
                                        <xsl:value-of select="$OBJTYPES"/>
                                    </OBJTYP>
                                </xsl:if>

                                <!-- spectral types -->
                                <xsl:if test="$SPECTYP/text()">
                                    <SPECTYP>
                                        <xsl:value-of select="$SPECTYP"/>
                                    </SPECTYP>
                                </xsl:if>

                                <!-- magnitudes -->
                                <xsl:if test="$FLUX_B/text()">
                                    <FLUX_B>
                                        <xsl:value-of select="$FLUX_B"/>
                                    </FLUX_B>
                                </xsl:if>
                                <xsl:if test="$FLUX_BP/text()">
                                    <FLUX_BP>
                                        <xsl:value-of select="$FLUX_BP"/>
                                    </FLUX_BP>
                                </xsl:if>
                                <xsl:if test="$FLUX_V/text()">
                                    <FLUX_V>
                                        <xsl:value-of select="$FLUX_V"/>
                                    </FLUX_V>
                                </xsl:if>
                                <xsl:if test="$FLUX_G/text()">
                                    <FLUX_G>
                                        <xsl:value-of select="$FLUX_G"/>
                                    </FLUX_G>
                                </xsl:if>
                                <xsl:if test="$FLUX_R/text()">
                                    <FLUX_R>
                                        <xsl:value-of select="$FLUX_R"/>
                                    </FLUX_R>
                                </xsl:if>
                                <xsl:if test="$FLUX_RP/text()">
                                    <FLUX_RP>
                                        <xsl:value-of select="$FLUX_RP"/>
                                    </FLUX_RP>
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
                                <xsl:if test="$FLUX_L/text()">
                                    <FLUX_L>
                                        <xsl:value-of select="$FLUX_L"/>
                                    </FLUX_L>
                                </xsl:if>
                                <xsl:if test="$FLUX_M/text()">
                                    <FLUX_M>
                                        <xsl:value-of select="$FLUX_M"/>
                                    </FLUX_M>
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
                                    <parameter xsi:type="a:StringValue" name="SearchCalServerVersion" value="{$SCLSVR_VERSION}"/>

                                    <!-- Build one parameter element per VOTable PARAM present in COPY_PARAMS -->
                                    <xsl:for-each select="$table/VOT:PARAM[contains($COPY_PARAMS, concat(@name,'|')) and @value]">
                                        <parameter>
                                            <xsl:attribute name="xsi:type">
                                                <xsl:choose>
                                                    <xsl:when test="@datatype = 'double' or @datatype = 'float'">a:NumberValue</xsl:when>
                                                    <xsl:when test="@datatype = 'boolean'">a:BooleanValue</xsl:when>
                                                    <xsl:otherwise>a:StringValue</xsl:otherwise>
                                                </xsl:choose>
                                            </xsl:attribute>
                                            <xsl:attribute name="name">
                                                <xsl:value-of select="@name" />
                                            </xsl:attribute>
                                            <xsl:attribute name="value">
                                                <xsl:value-of select="@value" />
                                            </xsl:attribute>
                                            <xsl:if test="@unit">
                                                <xsl:attribute name="unit">
                                                    <xsl:value-of select="@unit" />
                                                </xsl:attribute>
                                            </xsl:if>
                                        </parameter>
                                    </xsl:for-each>


                                    <!-- Build one field element per VOTable FIELD -->
                                    <xsl:for-each select="VOT:TD">
                                        <xsl:variable name="pos" select="position()" />
                                        <xsl:variable name="field" select="$table/VOT:FIELD[$pos]" />
                                        <xsl:variable name="cellValue" select="text()"/>

                                        <!-- skip blank values and unused columns (origin and confidence and many other data columns ...) -->
                                        <xsl:if test="$cellValue and $cellValue != '-' and contains($field/@name, '.origin') = false and contains($field/@name, '.confidence') = false">
                                            <field>
                                                <xsl:attribute name="xsi:type">
                                                    <xsl:choose>
                                                        <xsl:when test="$field/@datatype = 'double' or $field/@datatype = 'float'">a:NumberValue</xsl:when>
                                                        <xsl:when test="$field/@datatype = 'boolean'">a:BooleanValue</xsl:when>
                                                        <xsl:otherwise>a:StringValue</xsl:otherwise>
                                                    </xsl:choose>
                                                </xsl:attribute>
                                                <xsl:attribute name="name">
                                                    <xsl:value-of select="$field/@name" />
                                                </xsl:attribute>
                                                <xsl:attribute name="value">
                                                    <xsl:value-of select="$cellValue" />
                                                </xsl:attribute>
                                                <xsl:if test="$field/@unit">
                                                    <xsl:attribute name="unit">
                                                        <xsl:value-of select="$field/@unit" />
                                                    </xsl:attribute>
                                                </xsl:if>
                                            </field>
                                        </xsl:if>
                                    </xsl:for-each>

                                </calibratorInfos>
                            </target>

                        </xsl:if> <!-- deletedFlag -->
                    </xsl:for-each>

                </a:observationSetting>
            </xsl:if>
        </xsl:if>
    </xsl:template>




    <xsl:template name="getColumnIndex">
        <xsl:param name="colName"/>

        <xsl:for-each select="$table/VOT:FIELD">
            <xsl:if test="@name=$colName">
                <xsl:value-of select="position()" />
                <!-- <xsl:message>getColumnIndex('<xsl:value-of select="$colName"/>'): <xsl:value-of select="position()"/></xsl:message> -->
            </xsl:if>
        </xsl:for-each>
    </xsl:template>

</xsl:stylesheet>
