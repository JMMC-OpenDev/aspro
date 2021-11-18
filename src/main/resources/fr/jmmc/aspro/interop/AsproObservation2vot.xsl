<?xml version="1.0"?>
<!--
 *******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************

 NAME
 AsproObservation2vot.xsl - Aspro2 observation setting into VOTable

 DESCRIPTION
 This stylesheet transforms one aspro observation setting into one Votable 1.1 (targets).

-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:a="http://www.jmmc.fr/aspro-oi/0.1"
                xmlns:exslt="http://exslt.org/common"
                extension-element-prefixes="exslt"
                exclude-result-prefixes="a">

    <xsl:output omit-xml-declaration="no" indent="yes" />



    <xsl:template match="/">
        <!-- Aspro2 observation transform -->
        <xsl:apply-templates select="/a:observationSetting" />
    </xsl:template>




    <!-- observationSetting transform -->
    <xsl:template match="a:observationSetting">



        <!-- start of votable document -->
        <VOTABLE
            xmlns="http://www.ivoa.net/xml/VOTable/v1.1"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.1"
            xsi:schemaLocation="http://www.ivoa.net/xml/VOTable/v1.1 http://www.ivoa.net/xml/VOTable/VOTable-1.1.xsd">

            <DESCRIPTION>
                Votable exported by Aspro2
            </DESCRIPTION>

            <COOSYS ID="J2000" equinox="2000" epoch="J2000" system="eq_FK5"/>

            <!-- name attribute filled optionally by the name of the client application -->
            <RESOURCE name="aspro2">
                <!-- only export targets -->

                <!-- table containing target(s) : 1..n -->
                <TABLE name="Targets">

                    <!-- NAME field -->
                    <FIELD name="NAME" datatype="char" ucd="meta.id;meta.main" arraysize="*">
                        <DESCRIPTION>Target name (identifier)</DESCRIPTION>
                        <LINK href="http://simbad.u-strasbg.fr/simbad/sim-id?protocol=html&amp;Ident=${NAME}"/>
                    </FIELD>

                    <!-- RA field: J2000 (HMS or degrees) i.e. units contains ("h:m:s" or "hms" or "deg") -->
                    <FIELD name="RA" datatype="char" ucd="pos.eq.ra;meta.main" arraysize="*" unit="h:m:s" ref="J2000">
                        <DESCRIPTION>Right ascension - J2000</DESCRIPTION>
                    </FIELD>
                    <!-- DEC field: J2000 (DMS or degrees) i.e. units contains ("d:m:s" or "dms" or "deg") -->
                    <FIELD name="DEC" datatype="char" ucd="pos.eq.dec;meta.main" arraysize="*" unit="d:m:s" ref="J2000">
                        <DESCRIPTION>Declination - J2000</DESCRIPTION>
                    </FIELD>

                    <!-- RV field -->
                    <FIELD name="RV" datatype="double" ucd="spect.dopplerVeloc.opt" unit="km.s-1">
                        <DESCRIPTION>Radial velocity</DESCRIPTION>
                    </FIELD>
                    <!-- VELTYP : undefined -->


                    <!-- PMRA field -->
                    <FIELD name="PMRA" datatype="double" ucd="pos.pm;pos.eq.ra" unit="mas.yr-1">
                        <DESCRIPTION>Proper motion in RA</DESCRIPTION>
                    </FIELD>
                    <!-- PMDEC field -->
                    <FIELD name="PMDEC" datatype="double" ucd="pos.pm;pos.eq.dec" unit="mas.yr-1">
                        <DESCRIPTION>Proper motion in DEC</DESCRIPTION>
                    </FIELD>

                    <!-- PLX field -->
                    <FIELD name="PLX" datatype="double" ucd="pos.parallax.trig" unit="mas">
                        <DESCRIPTION>Parallax</DESCRIPTION>
                    </FIELD>
                    <!-- e_PLX field -->
                    <FIELD name="e_PLX" datatype="double" ucd="stat.error;pos.parallax.trig" unit="mas">
                        <DESCRIPTION>Parallax error</DESCRIPTION>
                    </FIELD>

                    <!-- IDS in Aspro2 is the concatenation of '<CATALOG_PREFIX> <CATALOG_ID> ...' -->

                    <FIELD ID="ID_HD" name="HD" datatype="char" ucd="meta.id" arraysize="*">
                        <DESCRIPTION>HD identifier (H. Draper: III/135A)</DESCRIPTION>
                        <LINK value="${ID_HD}" href="http://simbad.u-strasbg.fr/simbad/sim-id?Ident=HD${ID_HD}"/>
                    </FIELD>

                    <FIELD ID="ID_HIP" name="HIP" datatype="char" ucd="meta.id" arraysize="*">
                        <DESCRIPTION>HIP identifier (Hipparcos catalogue: I/239)</DESCRIPTION>
                        <LINK value="${ID_HIP}" href="http://simbad.u-strasbg.fr/simbad/sim-id?Ident=HIP${ID_HIP}"/>
                    </FIELD>

                    <FIELD ID="ID_2MASS" name="2MASS" datatype="char" ucd="meta.id" arraysize="*">
                        <DESCRIPTION>2MASS identifier (2Micron All-Sky Survey: II/246)</DESCRIPTION>
                        <LINK value="${ID_2MASS}" href="http://simbad.u-strasbg.fr/simbad/sim-id?Ident=2MASS+J${ID_2MASS}"/>
                    </FIELD>

                    <FIELD ID="ID_GAIA" name="GAIA" datatype="char" ucd="meta.id" arraysize="*">
                        <DESCRIPTION>GAIA DR2 identifier (2Micron All-Sky Survey: II/246)</DESCRIPTION>
                        <LINK value="${ID_GAIA}" href="http://simbad.u-strasbg.fr/simbad/sim-id?Ident=Gaia+DR2+${ID_GAIA}"/>
                    </FIELD>

                    <!-- Object types: comma separated values -->
                    <FIELD name="OTYPES" datatype="char" ucd="src.class" arraysize="*">
                        <DESCRIPTION>Object type(s)</DESCRIPTION>
                        <LINK href="http://simbad.u-strasbg.fr/simbad/sim-display?data=otypes"/>
                    </FIELD>

                    <!-- SP_TYPE field: Spectral types: comma separated values -->
                    <FIELD name="SP_TYPE" datatype="char" ucd="src.spType" arraysize="*">
                        <DESCRIPTION>MK spectral type</DESCRIPTION>
                    </FIELD>

                    <!-- fluxes -->
                    <FIELD name="FLUX_B" datatype="double" ucd="phot.mag;em.opt.B" unit="mag">
                        <DESCRIPTION>Magnitude B</DESCRIPTION>
                    </FIELD>
                    <FIELD name="FLUX_V" datatype="double" ucd="phot.mag;em.opt.V" unit="mag">
                        <DESCRIPTION>Magnitude V</DESCRIPTION>
                    </FIELD>
                    <FIELD name="FLUX_G" datatype="double" ucd="phot.mag;em.opt.G" unit="mag">
                        <DESCRIPTION>Magnitude G</DESCRIPTION>
                    </FIELD>
                    <FIELD name="FLUX_R" datatype="double" ucd="phot.mag;em.opt.R" unit="mag">
                        <DESCRIPTION>Magnitude R</DESCRIPTION>
                    </FIELD>
                    <FIELD name="FLUX_I" datatype="double" ucd="phot.mag;em.opt.I" unit="mag">
                        <DESCRIPTION>Magnitude I</DESCRIPTION>
                    </FIELD>
                    <FIELD name="FLUX_J" datatype="double" ucd="phot.mag;em.IR.J" unit="mag">
                        <DESCRIPTION>Magnitude J</DESCRIPTION>
                    </FIELD>
                    <FIELD name="FLUX_H" datatype="double" ucd="phot.mag;em.IR.H" unit="mag">
                        <DESCRIPTION>Magnitude H</DESCRIPTION>
                    </FIELD>
                    <FIELD name="FLUX_K" datatype="double" ucd="phot.mag;em.IR.K" unit="mag">
                        <DESCRIPTION>Magnitude K</DESCRIPTION>
                    </FIELD>
                    <FIELD name="FLUX_L" datatype="double" ucd="phot.mag;em.IR.3-4um" unit="mag">
                        <DESCRIPTION>Magnitude L</DESCRIPTION>
                    </FIELD>
                    <FIELD name="FLUX_M" datatype="double" ucd="phot.mag;em.IR.4-8um" unit="mag">
                        <DESCRIPTION>Magnitude M</DESCRIPTION>
                    </FIELD>
                    <FIELD name="FLUX_N" datatype="double" ucd="phot.mag;em.IR.8-15um" unit="mag">
                        <DESCRIPTION>Magnitude N</DESCRIPTION>
                    </FIELD>

                    <!-- missing calibrator informations : angular diameter -->
                    <!-- missing model information datatype="char" ucd="char meta.code.class;meta.modelled" arraysize="*" -->

                    <DATA>
                        <TABLEDATA>
                            <xsl:apply-templates select="./target" />
                        </TABLEDATA>
                    </DATA>
                </TABLE>

                <xsl:if test="./targetUserInfos/calibrators/text() != ''">
                    <!-- table containing target-calibrator relation : 1..n -->

                    <TABLE name="TargetsCalibrators">

                        <FIELD name="SCIENCE_TARGET_NAME" datatype="char" ucd="meta.id" arraysize="*">
                            <DESCRIPTION>Target name (identifier) defined in another table describing Targets</DESCRIPTION>
                        </FIELD>
                        <FIELD name="CALIBRATOR_NAME" datatype="char" ucd="meta.id" arraysize="*">
                            <DESCRIPTION>Calibrator name (identifier) defined in another table describing Targets</DESCRIPTION>
                        </FIELD>

                        <DATA>
                            <TABLEDATA>
                                <xsl:apply-templates select="./targetUserInfos" />
                            </TABLEDATA>
                        </DATA>
                    </TABLE>
                </xsl:if>

            </RESOURCE>
        </VOTABLE>
        <!-- end of votable document -->

    </xsl:template>



    <!-- target transform -->
    <xsl:template match="target">

        <!--
        <IDS>AG+28 25,AGKR 186,BD+27 25,GSC 01736-00683,HD 1094,HIC 1234,HIP 1234,PPM 89559,SAO 73846,TD1 103,TYC 1736-683-1,YZ 28 84,uvby98 100001094</IDS>
        -->
        <!-- get each identifier as tokens -->
        <xsl:variable name="splitIDS">
            <xsl:call-template name="str_split">
                <xsl:with-param name="string" select="IDS/text()" />
                <xsl:with-param name="pattern" select="','" />
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="tokenIDS" select="exslt:node-set($splitIDS)/token"/>
        <!--
                <xsl:message>
                    TARGET: <xsl:value-of select="name/text()"/>
                    IDS:
                    <xsl:for-each select="$tokenIDS">
                        <xsl:value-of select="text()"/>
                    </xsl:for-each>
                </xsl:message>
        -->
        <xsl:variable name="ID_HD">
            <xsl:for-each select="$tokenIDS">
                <xsl:if test="starts-with(text(), 'HD ')">
                    <xsl:value-of select="substring(text(), 4)"/>
                </xsl:if>
            </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="ID_HIP">
            <xsl:for-each select="$tokenIDS">
                <xsl:if test="starts-with(text(), 'HIP ')">
                    <xsl:value-of select="substring(text(), 5)"/>
                </xsl:if>
            </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="ID_2MASS">
            <xsl:for-each select="$tokenIDS">
                <xsl:if test="starts-with(text(), '2MASS J')">
                    <xsl:value-of select="substring(text(), 8)"/>
                </xsl:if>
            </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="ID_GAIA">
            <xsl:for-each select="$tokenIDS">
                <xsl:if test="starts-with(text(), 'Gaia DR2 ')">
                    <xsl:value-of select="substring(text(), 10)"/>
                </xsl:if>
            </xsl:for-each>
        </xsl:variable>

        <TR>
            <TD>
                <xsl:value-of select="name/text()"/>
            </TD>
            <!-- RA/DEC -->
            <TD>
                <xsl:value-of select="RA/text()"/>
            </TD>
            <TD>
                <xsl:value-of select="DEC/text()"/>
            </TD>
            <!-- RV -->
            <TD>
                <xsl:value-of select="SYSVEL/text()"/>
            </TD>
            <!-- PMRA/PMDEC -->
            <TD>
                <xsl:value-of select="PMRA/text()"/>
            </TD>
            <TD>
                <xsl:value-of select="PMDEC/text()"/>
            </TD>
            <!-- PLX/e_PLX -->
            <TD>
                <xsl:value-of select="PARALLAX/text()"/>
            </TD>
            <TD>
                <xsl:value-of select="PARA_ERR/text()"/>
            </TD>
            <!-- ID_HD -->
            <TD>
                <xsl:value-of select="$ID_HD"/>
            </TD>
            <!-- ID_HIP -->
            <TD>
                <xsl:value-of select="$ID_HIP"/>
            </TD>
            <!-- ID_2MASS -->
            <TD>
                <xsl:value-of select="$ID_2MASS"/>
            </TD>
            <!-- ID_GAIA -->
            <TD>
                <xsl:value-of select="$ID_GAIA"/>
            </TD>
            <!-- OTYPES -->
            <TD>
                <xsl:value-of select="OBJTYP/text()"/>
            </TD>
            <!-- SP_TYPE -->
            <TD>
                <xsl:value-of select="SPECTYP/text()"/>
            </TD>

            <!-- FLUX_B -->
            <TD>
                <xsl:value-of select="FLUX_B/text()"/>
            </TD>
            <!-- FLUX_V -->
            <TD>
                <xsl:value-of select="FLUX_V/text()"/>
            </TD>
            <!-- FLUX_G -->
            <TD>
                <xsl:value-of select="FLUX_G/text()"/>
            </TD>
            <!-- FLUX_R -->
            <TD>
                <xsl:value-of select="FLUX_R/text()"/>
            </TD>
            <!-- FLUX_I -->
            <TD>
                <xsl:value-of select="FLUX_I/text()"/>
            </TD>
            <!-- FLUX_J -->
            <TD>
                <xsl:value-of select="FLUX_J/text()"/>
            </TD>
            <!-- FLUX_H -->
            <TD>
                <xsl:value-of select="FLUX_H/text()"/>
            </TD>
            <!-- FLUX_K -->
            <TD>
                <xsl:value-of select="FLUX_K/text()"/>
            </TD>
            <!-- FLUX_L -->
            <TD>
                <xsl:value-of select="FLUX_L/text()"/>
            </TD>
            <!-- FLUX_M -->
            <TD>
                <xsl:value-of select="FLUX_M/text()"/>
            </TD>
            <!-- FLUX_N -->
            <TD>
                <xsl:value-of select="FLUX_N/text()"/>
            </TD>
        </TR>
    </xsl:template>




    <!-- targetUserInfos transform -->
    <xsl:template match="targetUserInfos">
        <xsl:variable name="TARGETS" select="../target"/>

        <xsl:for-each select="targetInfo">
            <xsl:variable name="SCI_REF" select="targetRef/text()"/>
            <xsl:variable name="TARGET" select="$TARGETS[@id = $SCI_REF]"/>
            <!--
            <xsl:message>
                SCI_REF: <xsl:value-of select="$SCI_REF"/>
                TARGET: <xsl:value-of select="$TARGET/name/text()"/>
                CALIBS: <xsl:value-of select="./calibrators/text()"/>
            </xsl:message>
            -->
            <xsl:if test="$TARGET">
                <!-- get each calibrator as tokens -->
                <xsl:variable name="splitCALS">
                    <xsl:call-template name="str_split">
                        <xsl:with-param name="string" select="./calibrators/text()" />
                        <xsl:with-param name="pattern" select="' '" />
                    </xsl:call-template>
                </xsl:variable>

                <xsl:for-each select="exslt:node-set($splitCALS)/token">
                    <xsl:variable name="CAL_REF" select="text()"/>
                    <xsl:variable name="CALIB" select="$TARGETS[@id = $CAL_REF]"/>
                    <!--
                    <xsl:message>
                        CAL_REF: <xsl:value-of select="$CAL_REF"/>
                        CALIB: <xsl:value-of select="$CALIB/name/text()"/>
                    </xsl:message>
                    -->
                    <xsl:if test="$CALIB">
                        <TR>
                            <TD>
                                <xsl:value-of select="$TARGET/name/text()"/>
                            </TD>
                            <TD>
                                <xsl:value-of select="$CALIB/name/text()"/>
                            </TD>
                        </TR>
                    </xsl:if>

                </xsl:for-each>
            </xsl:if>
        </xsl:for-each>
    </xsl:template>




    <!-- str:split template from http://exslt.org/strings -->

    <xsl:template name="str_split">
        <xsl:param name="string" select="''" />
        <xsl:param name="pattern" select="' '" />
        <xsl:choose>
            <xsl:when test="not($string)" />
            <xsl:when test="not($pattern)">
                <xsl:call-template name="str__split-characters">
                    <xsl:with-param name="string" select="$string" />
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="str__split-pattern">
                    <xsl:with-param name="string" select="$string" />
                    <xsl:with-param name="pattern" select="$pattern" />
                </xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>

    </xsl:template>

    <xsl:template name="str__split-characters">
        <xsl:param name="string" />
        <xsl:if test="$string">
            <token>
                <xsl:value-of select="substring($string, 1, 1)" />
            </token>

            <xsl:call-template name="str__split-characters">
                <xsl:with-param name="string" select="substring($string, 2)" />
            </xsl:call-template>
        </xsl:if>

    </xsl:template>

    <xsl:template name="str__split-pattern">
        <xsl:param name="string" />
        <xsl:param name="pattern" />
        <xsl:choose>
            <xsl:when test="contains($string, $pattern)">
                <xsl:if test="not(starts-with($string, $pattern))">
                    <token>
                        <xsl:value-of select="substring-before($string, $pattern)" />
                    </token>
                </xsl:if>

                <xsl:call-template name="str__split-pattern">
                    <xsl:with-param name="string" select="substring-after($string, $pattern)" />
                    <xsl:with-param name="pattern" select="$pattern" />
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <token>
                    <xsl:value-of select="$string" />
                </token>
            </xsl:otherwise>
        </xsl:choose>

    </xsl:template>




</xsl:stylesheet>
