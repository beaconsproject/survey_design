// Extract and download data from GEE
// PV 2023-01-07

// Wolverine planning region (full extent)
var region = ee.FeatureCollection('projects/ee-vernier/assets/wolverine_grids').geometry();

// NRCan CDEM
var elevation = ee.ImageCollection('NRCan/CDEM').mosaic().clip(region);
Export.image.toDrive({
  image: elevation,
  folder: 'gee_data',
  maxPixels: 1e13,
  crs: 'EPSG:3578',
  scale: 30
});

// CA_FOREST_LC_VLCE2 (1984-2019)
var ca_lc = ee.ImageCollection("projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2");
var lc2019 = ee.Image(ca_lc.sort('system:time_start',false).first()).clip(region);
Export.image.toDrive({
  image: lc2019,
  folder: 'gee_data',
  maxPixels: 1e13,
  crs: 'EPSG:3578',
  scale: 30
});
