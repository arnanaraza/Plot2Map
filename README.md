# PlotToMap
Plot-to-map comparison of aboveground biomass workflow

Here we develop an automated processing chain of AGB plot and map comparison in the context of map validation of AGB map products. The workflow mainly includes preprocessing of forest inventory data and estimating plot-level uncertainties (measurement and allometric model errors, sampling/within-pixel errors, and temporal mismatch with the map year). Preprocessed plot data are also format-ready for calibration/AGB mapping. 

So far, the processing chain can accomodate four kinds of AGB plot data inputs:
1. Plot data (points)
2. Unformatted plot data (default survey format)
3. Polygon data with four corner coordinates
4. Tree-level data 
5. Plot data with nested and irregular sub-plots (special case) 

The most common input is likely #2 and #4

The plot data input will undergo the following preprocessing chain: 
1. Formatting (except for #1)
2. Estimation of SD from Measurement error using BIOMASS package (for #4-5) or using a pre-trained RF model (for #1-3)
3. Estimation of SD from sampling error 
4. Esimation of SD from temporal mismatch error
5. Total of all SD

Map validation is next wherein users should have access of tree cover data (2010) and the AGB map. These inputs are not needed if your purpose is to create calibration-ready plot data.

References:
Araza et al. 2020 (in progress)


