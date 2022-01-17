Outline of methods to create perfectly square veg plot shapefiles, and 5x5 m quads
David Ackerly
6/2/21

# We had three different sources for plot corners
# original points from Trimble at 50 veg plots, at all four corners
# Prahlad redid the superplots, and a few hectares
# Melina got all 4 corners of 4 new 2018 plots

# First check is whether the edges are in fact 20 m long, based on Trimble. Pretty much variation! Prahlad's are closer. But recall the vegplots were put in using magnetic north. So I used Prahlad's to get an average across the superplots of the delta-x and delta-y aligned with magnetic north, and then adjusted these a bit to get exactly 20 m edges. The offsets were 19.5 m x 4.4 m.

# Given Prahlad's corrected values seemed closer, we substituted these in for the super plots, and recalculated all the hectare SW corners based on these. I did not use Prahlad's corrected values measured directly at a subset of hectare plots.

# Prahlad then took these adjusted values for the SW corners of all 54 plots and calculated the locations of NW, NE, and SE incorporating slope (steep slopes occupy less than 20x20 when looking straight down). The hectares are aligned with UTM so are just based on adding 100 m in each dimension, with no slope adjustment as these corners were located using GIS.

# Someday we should go in with high resolution Trimble and get better corners!

# Anytime that the file 'adjusted_four_corners.updated_2021-06-01.csv' is updated, the make_shapefiles script can be run to make new shapefiles, and generate corners and centerpoints for quads and subquads for 20 m and hectare plots