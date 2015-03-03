<?xml version="1.0"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:exslt="http://exslt.org/common"
                extension-element-prefixes="exslt"
                exclude-result-prefixes="">

    <xsl:output omit-xml-declaration="no" indent="yes"/>



    <xsl:template match="/">
        <xsl:variable name="votableWithoutNS">
            <!-- match any VOTABLE version 1.0 to 1.3 (with or without namespace) -->
            <xsl:for-each select="*[local-name()='VOTABLE']">
                <xsl:call-template name="removeNS"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:apply-templates select="exslt:node-set($votableWithoutNS)/VOTABLE" />
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


    <!--
    identity copy (test output)
    -->
    <xsl:template match="node()|@*">
        <xsl:copy>
            <xsl:apply-templates select="node()|@*"/>
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>