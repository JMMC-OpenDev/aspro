<?xml version="1.0"?>
<!--
 *******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************

 NAME
 mapping2xslt.xsl - Mapping into xslt script fragments

 DESCRIPTION
 This stylesheet transform one mapping into several script fragments
 setting.

-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xslt="http://www.w3.org/1999/XSL/Transform"
    xmlns:VOT11="http://www.ivoa.net/xml/VOTable/v1.1" >

  <xsl:output omit-xml-declaration="no" indent="yes"/>



  <xsl:template match="/">
    <xsl:element name="xsl:stylesheet">
      <xsl:apply-templates select="/mapping/table" />
    </xsl:element>
  </xsl:template>





  <xsl:template match="/mapping/table">

    <xsl:apply-templates select="./field" />

  </xsl:template>


  <xsl:template match="field">

    <!-- Locate fields in the votable -->
    <xsl:element name="xsl:variable">
      <xsl:attribute name="name"><xsl:value-of select="@name"/>_index</xsl:attribute>

      <xsl:element name="xsl:call-template">
        <xsl:attribute name="name">getColumnIndex</xsl:attribute>

        <xsl:element name="xsl:with-param">
          <xsl:attribute name="name">ucd11</xsl:attribute>
          <xsl:value-of select="@ucd11"/>
        </xsl:element>
        <xsl:element name="xsl:with-param">
          <xsl:attribute name="name">ucd10</xsl:attribute>
          <xsl:value-of select="@ucd1"/>
        </xsl:element>
      </xsl:element>

    </xsl:element>
    
    <!--
      <xslt:call-template name="getColumnIndex">
        <xslt:with-param name="colName">HIP</xslt:with-param>
      </xslt:call-template>
-->    
  </xsl:template>

</xsl:stylesheet>
