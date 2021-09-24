# create raster styles for eurac maps

# libs
library(khroma)

# set davos colour scheme and adapt to scd values -----------
davos <- colour("davos")
plot(davos(365)) # ow it will look
plot(davos(4)) # view the breakpoints for a continuous color scheme
davos(4) # get the breakpoints for a continuous color scheme
seq(len = 5, by = 365/5) # get the scd values where to put the breaks

# Updated SLD by hand according to settings above ----------------

# <FeatureTypeStyle>
# <Rule>
# <RasterSymbolizer>
# <ColorMap>
# <ColorMapEntry color="#2C194C" quantity="74" />
# <ColorMapEntry color="#4D7BC3" quantity="147" />
# <ColorMapEntry color="#AEBDA5" quantity="220" />
# <ColorMapEntry color="#FFFFFF" quantity="293" />
# </ColorMap>
# </RasterSymbolizer>
# </Rule>
# </FeatureTypeStyle>
#   
  
# Resources -----------------

# https://docs.geoserver.org/2.16.x/en/user/styling/sld/cookbook/rasters.html#three-color-gradient

# sld example geonode
# <FeatureTypeStyle>
#   <Rule>
#   <RasterSymbolizer>
#   <ColorMap>
#   <ColorMapEntry color="#0000FF" quantity="150" />
#   <ColorMapEntry color="#FFFF00" quantity="200" />
#   <ColorMapEntry color="#FF0000" quantity="250" />
#   </ColorMap>
#   </RasterSymbolizer>
#   </Rule>
#   </FeatureTypeStyle>




