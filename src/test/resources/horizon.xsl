<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns='http://www.ivoa.net/xml/VOTable/v1.1'>

  <xsl:output method="xml" encoding="UTF-8" indent="yes"/>




  <!-- root document correspond to the result branch of main document -->
  <xsl:template match="/">

    <VOTABLE version="1.1">
      <RESOURCE name="Horizon">
        <TABLE>
          <FIELD ID="AZ" name="azimuth" datatype="double" />
          <FIELD ID="EL" name="elevation" datatype="double" />
          <DATA>
            <TABLEDATA>
            <xsl:apply-templates select="//station[name/text() = 'A0']"/>
            <!-- [aspro:name/text() = 'A0'] -->
            </TABLEDATA>
          </DATA>
        </TABLE>
      </RESOURCE>
    </VOTABLE>
  </xsl:template>




  <xsl:template match="station">
    <xsl:comment>station <xsl:value-of select="name" /></xsl:comment>

    <xsl:apply-templates select="horizon/point"/>

  </xsl:template>




  <xsl:template match="point">
    <TR>
      <TD><xsl:value-of select="azimuth/text()" /></TD><TD><xsl:value-of select="elevation/text()" /></TD>
    </TR>
  </xsl:template>


</xsl:stylesheet>