## projections
raster_reproject <- function(ras,crs.new,
	crs.old=NULL,verbose=FALSE,images=FALSE)
{
	require(raster)
	require(rgdal)
	if (!is.null(crs.old)) projection(ras) <- crs.old
	if (verbose) str(ras)
	new = projectExtent(ras,crs.new)
	if (verbose) str(new)
	new.ras = projectRaster(ras,new)
	if (verbose) str(new.ras)
	if (images) image(new.ras,asp=1)
	return(new.ras)
	
}

a2ta_project = function(alb.ras,verbose=FALSE,images=FALSE)
{
    require(raster)
    require(rgdal)
    flint.project = '+proj=aea +datum=NAD83 +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0'
    alb.ras@crs@projargs = flint.project
    if (verbose) str(alb.ras)
    ta.project = '+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'
    new = projectExtent(alb.ras,ta.project)
    if (verbose) str(new)
    ta.ras = projectRaster(alb.ras,new)
    if (verbose) str(ta.ras)
    if (images) image(ta.ras,asp=1)
    return(ta.ras)
}

a2g_project = function(alb.ras,verbose=FALSE,images=FALSE)
{
	require(raster)
	require(rgdal)
	flint.project = '+proj=aea +datum=NAD83 +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0'
	alb.ras@crs@projargs = flint.project
	if (verbose) str(alb.ras)
	prism.project = '+proj=longlat +datum=WGS84'
	new = projectExtent(alb.ras,prism.project)
	if (verbose) str(new)
	wgs.ras = projectRaster(alb.ras,new)
	if (verbose) str(wgs.ras)
	if (images) image(wgs.ras,asp=1)
	return(wgs.ras)
}

g2a_project = function(wgs.ras,verbose=FALSE,images=FALSE)
{
	require(raster)
	require(rgdal)
	prism.project = '+proj=longlat +datum=WGS84'
	flint.project = '+proj=aea +datum=NAD83 +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0'
	wgs.ras@crs@projargs = prism.project
	if (verbose) str(wgs.ras)
	new = projectExtent(wgs.ras,flint.project)
	if (verbose) str(new)
	alb.ras = projectRaster(wgs.ras,new)
	if (verbose) str(alb.ras)
	if (images) image(alb.ras,asp=1)
	return(alb.ras)
}

g2ta_project = function(wgs.ras,verbose=FALSE,images=FALSE)
{
	require(raster)
	require(rgdal)
	prism.project = '+proj=longlat +datum=WGS84'
	cpad.project = '+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'
	wgs.ras@crs@projargs = prism.project
	if (verbose) str(wgs.ras)
	new = projectExtent(wgs.ras,cpad.project)
	if (verbose) str(new)
	alb.ras = projectRaster(wgs.ras,new)
	if (verbose) str(alb.ras)
	if (images) image(alb.ras,asp=1)
	return(alb.ras)
}

ta2g_project = function(alb.ras,verbose=FALSE,images=FALSE)
{
    require(raster)
    require(rgdal)
    flint.project = '+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'
    alb.ras@crs@projargs = flint.project
    if (verbose) str(alb.ras)
    prism.project = '+proj=longlat +datum=WGS84'
    new = projectExtent(alb.ras,prism.project)
    if (verbose) str(new)
    wgs.ras = projectRaster(alb.ras,new)
    if (verbose) str(wgs.ras)
    if (images) image(wgs.ras,asp=1)
    return(wgs.ras)
}


g2u_project = function(ras,utm='10',
    south=FALSE,verbose=FALSE,images=FALSE)
{
	require(raster)
	require(rgdal)
	prism.project = '+proj=longlat +datum=WGS84'
    utm.project = paste('+proj=utm +zone=',utm,sep='')
    if (south) utm.project = paste(utm.project,'+south',sep=' ')
	ras@crs@projargs = prism.project
	if (verbose) str(ras)
	new = projectExtent(ras,utm.project)
	if (verbose) str(new)
	utm.ras = projectRaster(ras,new)
	if (verbose) str(utm.ras)
	if (images) image(utm.ras,asp=1)
	return(utm.ras)
}

u2g_project = function(ras,utm='10',
                       south=FALSE,verbose=FALSE,images=FALSE)
{
    require(raster)
    require(rgdal)
    prism.project = '+proj=longlat +datum=WGS84'
    utm.project = paste('+proj=utm +zone=',utm,sep='')
    if (south) utm.project = paste(utm.project,'+south',sep=' ')
    ras@crs@projargs = utm.project
    if (verbose) str(ras)
    new = projectExtent(ras,prism.project)
    if (verbose) str(new)
    prism.ras = projectRaster(ras,new)
    if (verbose) str(prism.ras)
    if (images) image(prism.ras,asp=1)
    return(prism.ras)
}

a2u_project = function(alb.ras,utm='10S',verbose=FALSE,images=FALSE)
{
	require(raster)
	require(rgdal)
	flint.project = '+proj=aea +datum=NAD83 +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0'
	alb.ras@crs@projargs = flint.project
	if (verbose) str(alb.ras)
	utm.project = paste('+proj=utm +zone=',utm,sep='')
	new = projectExtent(alb.ras,utm.project)
	if (verbose) str(new)
	utm.ras = projectRaster(alb.ras,new)
	if (verbose) str(utm.ras)
	if (images) image(utm.ras,asp=1)
	return(utm.ras)
}
