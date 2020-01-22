<?xml version="1.0"?>
<!--
 *******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************

 NAME
 Obsvot2AsproObservation.xsl - ObsPortal Votable into Aspro2 observation setting

 DESCRIPTION
 This stylesheet transform a specific OBS-PORTAL Votable (any version) into one aspro observation setting.

 Transforms:
         <TABLE>
            <FIELD ID="obs_id" arraysize="*" datatype="unicodeChar" name="obs_id" ucd="meta.id"/>
            <FIELD ID="obs_type" arraysize="*" datatype="unicodeChar" name="obs_type" ucd="meta.id;class"/>
            <FIELD ID="obs_program" arraysize="*" datatype="unicodeChar" name="obs_program" ucd="meta.id"/>
            <FIELD ID="obs_container" arraysize="*" datatype="unicodeChar" name="obs_container" ucd="meta.id"/>
            <FIELD ID="obs_mjd_start" datatype="double" name="obs_mjd_start" ucd="time.start;obs.exposure"/>
            <FIELD ID="obs_mjd_end" datatype="double" name="obs_mjd_end" ucd="time.end;obs.exposure"/>
            <FIELD ID="target_ra" datatype="double" name="target_ra" ucd="pos.eq.ra;meta.main"/>
            <FIELD ID="target_dec" datatype="double" name="target_dec" ucd="pos.eq.dec;meta.main"/>
            <FIELD ID="target_name" arraysize="*" datatype="unicodeChar" name="target_name" ucd="meta.id;src"/>
            <FIELD ID="instrument_name" arraysize="*" datatype="unicodeChar" name="instrument_name" ucd="meta.id;instr"/>
            <FIELD ID="instrument_mode" arraysize="*" datatype="unicodeChar" name="instrument_mode" ucd="meta.id;instr.setup"/>
            <FIELD ID="instrument_submode" arraysize="*" datatype="unicodeChar" name="instrument_submode" ucd="meta.id;instr.setup"/>
            <FIELD ID="interferometer_name" arraysize="*" datatype="unicodeChar" name="interferometer_name" ucd="meta.id"/>
            <FIELD ID="interferometer_stations" arraysize="*" datatype="unicodeChar" name="interferometer_stations"/>
            <FIELD ID="interferometer_version" arraysize="*" datatype="unicodeChar" name="interferometer_version"/>
            <FIELD ID="projected_baselines" arraysize="*" datatype="unicodeChar" name="projected_baselines"/>
            <DATA>
                <TABLEDATA>
                    <TR>
                     <TD>PIONI.2019-12-13T03:24:59.281</TD>
                     <TD>calibrator</TD>
                     <TD>60.A-9004(A)</TD>
                     <TD>2656378</TD>
                     <TD>58830.1423527901</TD>
                     <TD>58830.1429453827</TD>
                     <TD>27.342153</TD>
                     <TD>-27.77426</TD>
                     <TD>HD_11174</TD>
                     <TD>PIONIER</TD>
                     <TD/>
                     <TD/>
                     <TD>VLTI</TD>
                     <TD>A 0   G 1   J 2   J 3</TD>
                     <TD/>
                     <TD>{'A0-G1': {'length': '82.8280', 'angle': '136.3000'}, 'A0-J2': {'length': '111.8865', 'angle': '111.2000'}, 'A0-J3': {'length': '118.3970', 'angle': '57.6805'}, 'G1-J2': {'length': '50.9495', 'angle': '67.5500'}, 'G1-J3': {'length': '130.4260', 'angle': '19.1725'}, 'J2-J3': {'length': '103.8475', 'angle': '357.6440'}}</TD>
                    </TR>
                </TABLEDATA>
            </DATA>
        </TABLE>

 Into:
     <o:observations xmlns:o="http://www.jmmc.fr/aspro-raw-obs/0.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:schemaLocation="http://www.jmmc.fr/aspro-raw-obs/0.1 AsproRawObsModel.xsd">
        <observation>
           <obsId>GRAVI.2017-09-28T01:46:39.919.fits</obsId>
           <type>SCIENCE</type>
           <parentId>200436844</parentId>
           <programId>60.A-9295(A)</programId>
           <interferometerName>VLTI</interferometerName>
           <interferometerVersion>Period 103</interferometerVersion>
           <stations>A0 G1 J2 K0</stations>
           <instrumentName>GRAVITY</instrumentName>
           <instrumentMode>HIGH-COMBINED</instrumentMode>
           <instrumentSubMode>SINGLE</instrumentSubMode>
           <targetName>Fomalhaut</targetName>
           <targetRa>344.411622</targetRa>
           <targetDec>-29.6229</targetDec>
           <mjdStart>58024.07407314</mjdStart>
           <mjdEnd>58024.07778841778</mjdEnd>
           <projectedBaselines></projectedBaselines>
       </observation>
    </o:observations>
-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:exslt="http://exslt.org/common"
                extension-element-prefixes="exslt"
                exclude-result-prefixes="">

    <xsl:output omit-xml-declaration="no" indent="yes"/>




    <xsl:variable name="votableWithoutNS">
        <!-- match any VOTABLE version 1.0 to 1.3 (with or without namespace) -->
        <xsl:for-each select="*[local-name()='VOTABLE']">
            <xsl:call-template name="removeNS"/>
        </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="VOTABLE" select="exslt:node-set($votableWithoutNS)/VOTABLE"/>
    <xsl:variable name="TABLES" select="$VOTABLE/RESOURCE/TABLE"/>




    <xsl:template match="/">
        <xsl:apply-templates select="$VOTABLE/RESOURCE" />
    </xsl:template>


    <!-- This template remove the namespace of current node and associated descendants -->
    <xsl:template name="removeNS">
        <xsl:element name="{local-name()}">
            <xsl:copy-of select="@*|text()"/>
            <xsl:for-each select="child::*">
                <xsl:call-template name="removeNS"/>
            </xsl:for-each>
        </xsl:element>
    </xsl:template>




    <!-- VOTable RESOURCE transform -->
    <xsl:template match="/VOTABLE/RESOURCE">

        <!-- Extract PARAM values (note: topcat/STIL can move PARAMS inside TABLE) -->

        <!-- Query settings : TODO -->
        <xsl:variable name="QUERY"   select=".//PARAM[@name = 'QUERY']/@value"/>


        <!-- Locate fields in the votable (generated by mapping2xslt) -->

        <!-- TODO: fix UCD -->
        <xsl:variable name="OBS_ID_index">
            <xsl:call-template name="getColumnIndex">
                <!--
                    <xsl:with-param name="ucd11">meta.id;meta.main</xsl:with-param>
                    <xsl:with-param name="ucd10">META.MAIN</xsl:with-param>
                -->
                <xsl:with-param name="name">obs_id</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="OBS_TYPE_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">obs_type</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="PARENT_ID_index">
            <xsl:call-template name="getColumnIndex">
                <!--
                    <xsl:with-param name="ucd11">meta.id</xsl:with-param>
                    <xsl:with-param name="ucd10">META.ID</xsl:with-param>
                -->
                <xsl:with-param name="name">obs_container</xsl:with-param> <!-- obs_parent_id -->
            </xsl:call-template>
        </xsl:variable>

        <!-- program -->
        <xsl:variable name="PROG_ID_index">
            <xsl:call-template name="getColumnIndex">
                <!--
                    <xsl:with-param name="ucd11">meta.id</xsl:with-param>
                    <xsl:with-param name="ucd10">META.ID</xsl:with-param>
                -->
                <xsl:with-param name="name">obs_program</xsl:with-param> <!-- obs_prog_id -->
            </xsl:call-template>
        </xsl:variable>

        <!-- interferometer (facility) identification -->
        <xsl:variable name="INTERFEROMETER_NAME_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">interferometer_name</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="INTERFEROMETER_VERSION_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">interferometer_version</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <!-- interferometer (array) setup -->
        <xsl:variable name="STATIONS_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">interferometer_stations</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="POPS_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">interferometer_pops</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="CHANNELS_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">interferometer_channels</xsl:with-param> <!-- TBD -->
            </xsl:call-template>
        </xsl:variable>

        <!-- instrument setup -->
        <xsl:variable name="INSTRUMENT_NAME_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">instrument_name</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="INSTRUMENT_MODE_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">instrument_mode</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="INSTRUMENT_SUB_MODE_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">instrument_submode</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <!-- target -->
        <xsl:variable name="TARGET_NAME_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">target_name</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="TARGET_RA_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">target_ra</xsl:with-param>
                <!--
                                    <xsl:with-param name="ucd11">pos.eq.ra</xsl:with-param>
                                    <xsl:with-param name="ucd10">POS_EQ_RA</xsl:with-param>
                                    <xsl:with-param name="unit">deg</xsl:with-param>
                -->
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="TARGET_DEC_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">target_dec</xsl:with-param>
                <!--
                                    <xsl:with-param name="ucd11">pos.eq.dec</xsl:with-param>
                                    <xsl:with-param name="ucd10">POS_EQ_DEC</xsl:with-param>
                                    <xsl:with-param name="unit">deg</xsl:with-param>
                -->
            </xsl:call-template>
        </xsl:variable>

        <!-- exposure time -->
        <xsl:variable name="MJD_START_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">obs_mjd_start</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="MJD_END_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">obs_mjd_end</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <!-- projected baselines (uv points) -->
        <xsl:variable name="PROJ_BASELINES_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">projected_baselines</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>


        <!-- starting output document -->
        <o:observations xmlns:o="http://www.jmmc.fr/aspro-raw-obs/0.1"
                        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                        xsi:schemaLocation="http://www.jmmc.fr/aspro-raw-obs/0.1 AsproRawObsModel.xsd">

            <xsl:comment>Generated by xslt script from obsportal votable</xsl:comment>

            <!-- skip data if no columns matching NAME/RA/DEC -->
            <xsl:if test="$OBS_ID_index != '' and $TARGET_RA_index != '' and $TARGET_DEC_index != ''">

                <!-- Build one observation element per votable row -->
                <xsl:for-each select="./TABLE/DATA/TABLEDATA/TR">

                    <xsl:variable name="OBS_ID"                       select="./TD[position()=$OBS_ID_index]"/>
                    <xsl:variable name="OBS_TYPE"                     select="./TD[position()=$OBS_TYPE_index]"/>
                    <xsl:variable name="PARENT_ID"                    select="./TD[position()=$PARENT_ID_index]"/>

                    <xsl:variable name="PROG_ID"                      select="./TD[position()=$PROG_ID_index]"/>

                    <xsl:variable name="INTERFEROMETER_NAME"          select="./TD[position()=$INTERFEROMETER_NAME_index]"/>
                    <xsl:variable name="INTERFEROMETER_VERSION"       select="./TD[position()=$INTERFEROMETER_VERSION_index]"/>

                    <xsl:variable name="STATIONS"                     select="./TD[position()=$STATIONS_index]"/>
                    <xsl:variable name="POPS"                         select="./TD[position()=$POPS_index]"/>
                    <xsl:variable name="CHANNELS"                     select="./TD[position()=$CHANNELS_index]"/>

                    <xsl:variable name="INSTRUMENT_NAME"              select="./TD[position()=$INSTRUMENT_NAME_index]"/>
                    <xsl:variable name="INSTRUMENT_MODE"              select="./TD[position()=$INSTRUMENT_MODE_index]"/>
                    <xsl:variable name="INSTRUMENT_SUB_MODE"          select="./TD[position()=$INSTRUMENT_SUB_MODE_index]"/>

                    <xsl:variable name="TARGET_NAME"                  select="./TD[position()=$TARGET_NAME_index]"/>
                    <xsl:variable name="TARGET_RA"                    select="./TD[position()=$TARGET_RA_index]"/>
                    <xsl:variable name="TARGET_DEC"                   select="./TD[position()=$TARGET_DEC_index]"/>

                    <xsl:variable name="MJD_START"                    select="./TD[position()=$MJD_START_index]"/>
                    <xsl:variable name="MJD_END"                      select="./TD[position()=$MJD_END_index]"/>

                    <xsl:variable name="PROJ_BASELINES"               select="./TD[position()=$PROJ_BASELINES_index]"/>


                    <xsl:if test="$OBS_ID/text() and $TARGET_RA/text() and $TARGET_DEC/text()">

                        <observation>
                            <obsId>
                                <xsl:value-of select="$OBS_ID"/>
                            </obsId>
                            <type>
                                <xsl:value-of select="$OBS_TYPE"/>
                            </type>
                            <xsl:if test="$PARENT_ID/text()">
                                <parentId>
                                    <xsl:value-of select="$PARENT_ID"/>
                                </parentId>
                            </xsl:if>
                            <xsl:if test="$PROG_ID/text()">
                                <programId>
                                    <xsl:value-of select="$PROG_ID"/>
                                </programId>
                            </xsl:if>

                            <interferometerName>
                                <xsl:value-of select="$INTERFEROMETER_NAME"/>
                            </interferometerName>
                            <xsl:if test="$INTERFEROMETER_VERSION/text()">
                                <interferometerVersion>
                                    <xsl:value-of select="$INTERFEROMETER_VERSION"/>
                                </interferometerVersion>
                            </xsl:if>

                            <stations>
                                <xsl:value-of select="$STATIONS"/>
                            </stations>
                            <xsl:if test="$POPS/text()">
                                <pops>
                                    <xsl:value-of select="$POPS"/>
                                </pops>
                            </xsl:if>
                            <xsl:if test="$CHANNELS/text()">
                                <channels>
                                    <xsl:value-of select="$CHANNELS"/>
                                </channels>
                            </xsl:if>

                            <instrumentName>
                                <xsl:value-of select="$INSTRUMENT_NAME"/>
                            </instrumentName>
                            <instrumentMode>
                                <xsl:value-of select="$INSTRUMENT_MODE"/>
                            </instrumentMode>
                            <xsl:if test="$INSTRUMENT_SUB_MODE/text()">
                                <instrumentSubMode>
                                    <xsl:value-of select="$INSTRUMENT_SUB_MODE"/>
                                </instrumentSubMode>
                            </xsl:if>

                            <targetName>
                                <xsl:value-of select="$TARGET_NAME"/>
                            </targetName>
                            <targetRa>
                                <xsl:value-of select="$TARGET_RA"/>
                            </targetRa>
                            <targetDec>
                                <xsl:value-of select="$TARGET_DEC"/>
                            </targetDec>

                            <mjdStart>
                                <xsl:value-of select="$MJD_START"/>
                            </mjdStart>
                            <mjdEnd>
                                <xsl:value-of select="$MJD_END"/>
                            </mjdEnd>

                            <projectedBaselines>
                                <xsl:value-of select="$PROJ_BASELINES"/>
                            </projectedBaselines>

                        </observation>

                    </xsl:if>

                </xsl:for-each>

            </xsl:if>

        </o:observations>

    </xsl:template>



    <!-- VOTable Field Index finder -->
    <xsl:template name="getColumnIndex">
        <xsl:param name="name"/>
        <xsl:param name="ucd11"/>
        <xsl:param name="ucd10"/>
        <xsl:param name="unit"/>

        <xsl:variable name="selNodeId">
            <xsl:choose>
                <xsl:when test="$unit">
                    <xsl:value-of select="generate-id($TABLES/FIELD[contains(@unit, $unit) and (contains(@ucd, $ucd11) or contains(@ucd, $ucd10))])" />
                </xsl:when>
                <xsl:when test="$name">
                    <!-- TODO: fix UCD in votable and restore condition: 'and (@ucd = $ucd11 or @ucd = $ucd10)]' -->
                    <xsl:value-of select="generate-id($TABLES/FIELD[@name = $name])" />
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="generate-id($TABLES/FIELD[@ucd = $ucd11 or @ucd = $ucd10])" />
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:if test="$selNodeId != ''">
            <!-- double loop to get position() correspond to the parent table fields -->
            <xsl:for-each select="$TABLES">
                <xsl:for-each select="./FIELD">
                    <xsl:if test="$selNodeId = generate-id(.)">
                        <xsl:value-of select="position()" />
                    </xsl:if>
                </xsl:for-each>
            </xsl:for-each>
        </xsl:if>

    </xsl:template>

</xsl:stylesheet>
