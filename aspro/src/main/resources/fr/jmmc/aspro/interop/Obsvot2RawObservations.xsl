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
    <RESOURCE type="results">
        <PARAM ID="status" arraysize="*" datatype="unicodeChar" name="status" value="OK"/>
        <PARAM ID="last_mod_date" arraysize="*" datatype="unicodeChar" name="last_mod_date" value="2020-03-27 09:26:53+01:00"/>
        <TABLE>
            <FIELD ID="exp_id" arraysize="*" datatype="unicodeChar" name="exp_id" ucd="meta.id"/>
            <FIELD ID="exp_header" arraysize="*" datatype="unicodeChar" name="exp_header" ucd="meta.id"/>
            <FIELD ID="exp_mjd_start" datatype="double" name="exp_mjd_start" ucd="time.start;obs.exposure"/>
            <FIELD ID="exp_mjd_end" datatype="double" name="exp_mjd_end" ucd="time.end;obs.exposure"/>
            <FIELD ID="exp_projected_baselines" arraysize="*" datatype="unicodeChar" name="exp_projected_baselines" ucd="instr.baseline"/>

            <FIELD ID="exp_tau0" datatype="double" name="exp_tau0" ucd="time.processing"/>
            <FIELD ID="exp_temperature" datatype="double" name="exp_temperature" ucd="phys.temperature"/>
            <FIELD ID="exp_seeing" datatype="double" name="exp_seeing" ucd="instr.obsty.seeing"/>

            <FIELD ID="obs_id" arraysize="*" datatype="unicodeChar" name="obs_id" ucd="meta.id"/>
            <FIELD ID="obs_type" arraysize="*" datatype="unicodeChar" name="obs_type" ucd="meta.id;class"/>
            <FIELD ID="obs_program" arraysize="*" datatype="unicodeChar" name="obs_program" ucd="meta.id"/>

            <FIELD ID="interferometer_name" arraysize="*" datatype="unicodeChar" name="interferometer_name" ucd="meta.id"/>
            <FIELD ID="interferometer_stations" arraysize="*" datatype="unicodeChar" name="interferometer_stations"/>

            <FIELD ID="instrument_name" arraysize="*" datatype="unicodeChar" name="instrument_name" ucd="meta.id;instr"/>
            <FIELD ID="instrument_mode" arraysize="*" datatype="unicodeChar" name="instrument_mode" ucd="meta.id;instr.setup"/>
            <FIELD ID="instrument_submode" arraysize="*" datatype="unicodeChar" name="instrument_submode" ucd="meta.id;instr.setup"/>

            <FIELD ID="target_id" arraysize="*" datatype="unicodeChar" name="target_id" ucd="meta.id"/>
            <FIELD ID="target_name" arraysize="*" datatype="unicodeChar" name="target_name" ucd="meta.id;meta.main"/>
            <FIELD ID="target_ra" datatype="double" name="target_ra" ucd="pos.eq.ra;meta.main" unit="deg"/>
            <FIELD ID="target_dec" datatype="double" name="target_dec" ucd="pos.eq.dec;meta.main" unit="deg"/>
            <DATA>
                <TABLEDATA>
                    <TR>
                        <TD>GRAVI.2019-02-16T04:26:53.206_1</TD>
                        <TD>GRAVI.2019-02-16T04:26:53.206</TD>
                        <TD>58530.18533803</TD>
                        <TD>58530.1888102522</TD>
                        <TD>{'U1-U2': {'length': {'start': '50.1590', 'end': '50.0840'}, 'angle': {'start': '14.6000', 'end': '15.4000'}}, 'U1-U3': {'length': {'start': '93.5290', 'end': '93.3650'}, 'angle': {'start': '20.5000', 'end': '21.3000'}}, 'U1-U4': {'length': {'start': '129.2960', 'end': '129.1750'}, 'angle': {'start': '43.4850', 'end': '44.5810'}}, 'U2-U3': {'length': {'start': '43.9410', 'end': '43.8610'}, 'angle': {'start': '27.2000', 'end': '28.2000'}}, 'U2-U4': {'length': {'start': '88.7820', 'end': '88.8800'}, 'angle': {'start': '59.3480', 'end': '60.5360'}}, 'U3-U4': {'length': {'start': '56.6300', 'end': '56.9140'}, 'angle': {'start': '83.7340', 'end': '84.9140'}}}</TD>

                        <TD>0.0049675</TD>
                        <TD>12.3</TD>
                        <TD>0.71</TD>

                        <TD>512</TD>
                        <TD>science</TD>
                        <TD>0102.C-0519(A)</TD>

                        <TD>VLTI</TD>
                        <TD>U1 U2 U3 U4</TD>

                        <TD>GRAVITY</TD>
                        <TD>HIGH-COMBINED</TD>
                        <TD>SINGLE</TD>

                        <TD>466</TD>
                        <TD>IRS9A</TD>
                        <TD>168.790967</TD>
                        <TD>-61.28026</TD>
                    </TR>
                </TABLEDATA>
            </DATA>
        </TABLE>
    </RESOURCE>

 Into:
     <o:observations xmlns:o="http://www.jmmc.fr/aspro-raw-obs/0.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:schemaLocation="http://www.jmmc.fr/aspro-raw-obs/0.1 AsproRawObsModel.xsd">
        <observation>
            <obsId>GRAVI.2019-02-16T04:26:53.206_1</obsId>
            <type>science</type>
            <parentId>512</parentId>
            <programId>0102.C-0519(A)</programId>
            <interferometerName>VLTI</interferometerName>
            <stations>U1 U2 U3 U4</stations>
            <instrumentName>GRAVITY</instrumentName>
            <instrumentMode>HIGH-COMBINED</instrumentMode>
            <instrumentSubMode>SINGLE</instrumentSubMode>
            <targetName>IRS9A</targetName>
            <targetRa>168.790967</targetRa>
            <targetDec>-61.28026</targetDec>
            <mjdStart>58530.18533803</mjdStart>
            <mjdEnd>58530.1888102522</mjdEnd>
            <projectedBaselines>{'U1-U2': {'length': {'start': '50.1590', 'end': '50.0840'}, 'angle': {'start': '14.6000', 'end': '15.4000'}}, 'U1-U3': {'length': {'start': '93.5290', 'end': '93.3650'}, 'angle': {'start': '20.5000', 'end': '21.3000'}}, 'U1-U4': {'length': {'start': '129.2960', 'end': '129.1750'}, 'angle': {'start': '43.4850', 'end': '44.5810'}}, 'U2-U3': {'length': {'start': '43.9410', 'end': '43.8610'}, 'angle': {'start': '27.2000', 'end': '28.2000'}}, 'U2-U4': {'length': {'start': '88.7820', 'end': '88.8800'}, 'angle': {'start': '59.3480', 'end': '60.5360'}}, 'U3-U4': {'length': {'start': '56.6300', 'end': '56.9140'}, 'angle': {'start': '83.7340', 'end': '84.9140'}}}</projectedBaselines>
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
        <xsl:variable name="QUERY"   select="./PARAM[@name = 'QUERY']/@value"/>


        <!-- Locate fields in the votable (generated by mapping2xslt) -->

        <!--
            <FIELD ID="exp_id" arraysize="*" datatype="unicodeChar" name="exp_id" ucd="meta.id"/>
            <FIELD ID="exp_header" arraysize="*" datatype="unicodeChar" name="exp_header" ucd="meta.id"/>
            <FIELD ID="obs_id" arraysize="*" datatype="unicodeChar" name="obs_id" ucd="meta.id"/>
            <FIELD ID="obs_type" arraysize="*" datatype="unicodeChar" name="obs_type" ucd="meta.id;class"/>
            <FIELD ID="obs_program" arraysize="*" datatype="unicodeChar" name="obs_program" ucd="meta.id"/>
        -->

        <!-- TODO: fix UCD -->
        <xsl:variable name="OBS_ID_index">
            <xsl:call-template name="getColumnIndex">
                <!--
                    <xsl:with-param name="ucd11">meta.id;meta.main</xsl:with-param>
                    <xsl:with-param name="ucd10">META.MAIN</xsl:with-param>
                -->
                <xsl:with-param name="name">exp_id</xsl:with-param>
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
                <xsl:with-param name="name">obs_id</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <!-- program -->
        <xsl:variable name="PROG_ID_index">
            <xsl:call-template name="getColumnIndex">
                <!--
                    <xsl:with-param name="ucd11">meta.id</xsl:with-param>
                    <xsl:with-param name="ucd10">META.ID</xsl:with-param>
                -->
                <xsl:with-param name="name">obs_program</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <!-- interferometer (facility) identification -->
        <!--
            <FIELD ID="interferometer_name" arraysize="*" datatype="unicodeChar" name="interferometer_name" ucd="meta.id"/>
            <FIELD ID="interferometer_stations" arraysize="*" datatype="unicodeChar" name="interferometer_stations"/>
        -->
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
        <!--
            <FIELD ID="instrument_name" arraysize="*" datatype="unicodeChar" name="instrument_name" ucd="meta.id;instr"/>
            <FIELD ID="instrument_mode" arraysize="*" datatype="unicodeChar" name="instrument_mode" ucd="meta.id;instr.setup"/>
            <FIELD ID="instrument_submode" arraysize="*" datatype="unicodeChar" name="instrument_submode" ucd="meta.id;instr.setup"/>
        -->
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
        <!-- TODO: keep target_id -->
        <!--
            <FIELD ID="target_id" arraysize="*" datatype="unicodeChar" name="target_id" ucd="meta.id"/>
            <FIELD ID="target_name" arraysize="*" datatype="unicodeChar" name="target_name" ucd="meta.id;meta.main"/>
            <FIELD ID="target_ra" datatype="double" name="target_ra" ucd="pos.eq.ra;meta.main" unit="deg"/>
            <FIELD ID="target_dec" datatype="double" name="target_dec" ucd="pos.eq.dec;meta.main" unit="deg"/>
        -->
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

        <!-- exposure / acquisition -->
        <!--
            <FIELD ID="exp_mjd_start" datatype="double" name="exp_mjd_start" ucd="time.start;obs.exposure"/>
            <FIELD ID="exp_mjd_end" datatype="double" name="exp_mjd_end" ucd="time.end;obs.exposure"/>
            <FIELD ID="exp_projected_baselines" arraysize="*" datatype="unicodeChar" name="exp_projected_baselines" ucd="instr.baseline"/>
        -->

        <!-- exposure time -->
        <xsl:variable name="MJD_START_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">exp_mjd_start</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="MJD_END_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">exp_mjd_end</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <!-- projected baselines (uv points) -->
        <xsl:variable name="PROJ_BASELINES_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">exp_projected_baselines</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <!-- weather conditions -->
        <xsl:variable name="EXP_TAU0_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">exp_tau0</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="EXP_TEMP_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">exp_temperature</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="EXP_SEEING_index">
            <xsl:call-template name="getColumnIndex">
                <xsl:with-param name="name">exp_seeing</xsl:with-param>
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

                    <xsl:variable name="EXP_TAU0"                     select="./TD[position()=$EXP_TAU0_index]"/>
                    <xsl:variable name="EXP_TEMP"                     select="./TD[position()=$EXP_TEMP_index]"/>
                    <xsl:variable name="EXP_SEEING"                   select="./TD[position()=$EXP_SEEING_index]"/>


                    <xsl:if test="$OBS_ID/text() and $TARGET_RA/text() and $TARGET_DEC/text()">
                        <observation>
                            <obsId><xsl:value-of select="$OBS_ID"/></obsId>
                            <type><xsl:value-of select="$OBS_TYPE"/></type>
                            <xsl:if test="$PARENT_ID/text()">
                                <parentId><xsl:value-of select="$PARENT_ID"/></parentId>
                            </xsl:if>
                            <xsl:if test="$PROG_ID/text()">
                                <programId><xsl:value-of select="$PROG_ID"/></programId>
                            </xsl:if>

                            <interferometerName><xsl:value-of select="$INTERFEROMETER_NAME"/></interferometerName>
                            <xsl:if test="$INTERFEROMETER_VERSION/text()">
                                <interferometerVersion><xsl:value-of select="$INTERFEROMETER_VERSION"/></interferometerVersion>
                            </xsl:if>

                            <stations><xsl:value-of select="$STATIONS"/></stations>
                            <xsl:if test="$POPS/text()">
                                <pops><xsl:value-of select="$POPS"/></pops>
                            </xsl:if>
                            <xsl:if test="$CHANNELS/text()">
                                <channels><xsl:value-of select="$CHANNELS"/></channels>
                            </xsl:if>

                            <instrumentName><xsl:value-of select="$INSTRUMENT_NAME"/></instrumentName>
                            <instrumentMode><xsl:value-of select="$INSTRUMENT_MODE"/></instrumentMode>
                            <xsl:if test="$INSTRUMENT_SUB_MODE/text()">
                                <instrumentSubMode><xsl:value-of select="$INSTRUMENT_SUB_MODE"/></instrumentSubMode>
                            </xsl:if>

                            <targetName><xsl:value-of select="$TARGET_NAME"/></targetName>
                            <targetRa><xsl:value-of select="$TARGET_RA"/></targetRa>
                            <targetDec><xsl:value-of select="$TARGET_DEC"/></targetDec>

                            <mjdStart><xsl:value-of select="$MJD_START"/></mjdStart>
                            <mjdEnd><xsl:value-of select="$MJD_END"/></mjdEnd>

                            <projectedBaselines><xsl:value-of select="$PROJ_BASELINES"/></projectedBaselines>

                            <xsl:if test="$EXP_TAU0/text()">
                                <expTau0><xsl:value-of select="$EXP_TAU0"/></expTau0>
                            </xsl:if>
                            <xsl:if test="$EXP_TEMP/text()">
                                <expTemp><xsl:value-of select="$EXP_TEMP"/></expTemp>
                            </xsl:if>
                            <xsl:if test="$EXP_SEEING/text()">
                                <expSeeing><xsl:value-of select="$EXP_SEEING"/></expSeeing>
                            </xsl:if>
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
