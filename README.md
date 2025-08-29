# fwi25-tools
A set of tools being developed to support the use of the FWI2025 release

## FWI Calculator 
This is an R Shiny app that can be used to input an hourly weather file and calculate FWI2025 outputs, view FWI2025 outputs as tables, download the tabular data, visualize the data with interactive plots and download the plots. It currently functions only on a pre-made hourly file. The user can set all constraints as required. The app also lets you compare the FWI25 to FWI87 for overlapping variables. 
Currently works utilizing the `main` branch from the https://github.com/nrcan-cfs-fire/cffdrs-ng repo ,and uses the FWI87 code from the cffdrs r package package

Set folder structure:
your-project/
├─ app.R                  # (paste the Shiny app below)
└─ ng/
   ├─ NG_FWI.r
   ├─ make_inputs.r
   ├─ util.r
   └─ data/               # optional; copy from the repo if needed
      └─ wx_prf.csv       # example resource used by repo functions

