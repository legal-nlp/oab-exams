<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:lexml="http://www.lexml.gov.br/1.0">


  <xsl:template match="*|/">
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="//lexml:Artigo">
    <artigo>
      <xsl:value-of select="."/>
    </artigo>
  </xsl:template>
  
  <xsl:template match="text()|@*"> 
  </xsl:template>

</xsl:stylesheet>

