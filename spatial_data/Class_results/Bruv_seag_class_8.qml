<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.4.14-Madeira" hasScaleBasedVisibilityFlag="0" minScale="1e+08" styleCategories="AllStyleCategories" maxScale="0">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
  </flags>
  <customproperties>
    <property key="identify/format" value="Value"/>
  </customproperties>
  <pipe>
    <rasterrenderer band="1" alphaBand="-1" type="paletted" opacity="1">
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <colorPalette>
        <paletteEntry label="Class Overlap" value="-1000" color="#4d4d4d" alpha="255"/>
        <paletteEntry label="0 - Unclassified" value="0" color="#000000" alpha="255"/>
        <paletteEntry label="1 - 0Sg" value="1" color="#aaaa7f" alpha="255"/>
        <paletteEntry label="2 - 25Sg" value="2" color="#ffff7f" alpha="255"/>
        <paletteEntry label="3 - 50Sg" value="3" color="#aaff00" alpha="255"/>
        <paletteEntry label="4 - 75Sg" value="4" color="#55aa00" alpha="255"/>
        <paletteEntry label="5 - 100Sg" value="5" color="#137302" alpha="255"/>
      </colorPalette>
    </rasterrenderer>
    <brightnesscontrast contrast="0" brightness="0"/>
    <huesaturation colorizeStrength="100" saturation="0" grayscaleMode="0" colorizeRed="255" colorizeGreen="128" colorizeOn="0" colorizeBlue="128"/>
    <rasterresampler maxOversampling="2"/>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
