
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text" version="1.0" encoding="UTF-8" indent="no" />

  <xsl:strip-space elements="*" />

 <!-- main -->
  <xsl:template match="/">

| Model | Description | Parameters | Function description|
<xsl:apply-templates select="/response/model[@name = 'model_list']/model"/>

  </xsl:template>


  <xsl:template match="model">
| <xsl:value-of select="@name"/> |  | <xsl:apply-templates select="parameter"/> | <xsl:value-of select="./desc/text()"/> |
  </xsl:template>


  <xsl:template match="parameter"><xsl:value-of select="@name"/> <xsl:value-of select="' '"/></xsl:template>
</xsl:stylesheet>