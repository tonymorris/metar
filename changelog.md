0.0.4

* **BREAKING**: Removed BOM (Bureau of Meteorology) support - NOAA only
* **BREAKING**: Removed functions: `getBOMTAF`, `getAllMETAR`, `getAllTAF`
* **BREAKING**: Removed module: `Data.Aviation.Metar.BOMTAFResult`
* Updated for GHC 9.6.7 compatibility
* Relaxed dependency bounds

0.0.3

* tgftp.nws.noaa.gov moved to forcing https, so use wreq library
* add metar version when given no arguments

0.0.2

* update nix
* add screenrecorder

0.0.1

* Initial release
