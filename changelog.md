0.0.5

* Restore BOM (Bureau of Meteorology) support via HTML scraping of the METAR/SPECI page
* New function `getBOMMETAR`; `getMETAR` tries BOM first (for `Y*` codes) then falls back to NOAA
* Cache ICAO → state mapping in `$XDG_CACHE_HOME/metar/icao-states.txt` so most Australian lookups are a single request
* Classify `HttpException` so 404s render as "No METAR for X" and other network errors get a short label
* New module `Data.Aviation.Metar.METARError` with `METARError` sum type (`ConnErrorAt`, `ParseErrorAt`); replaces the old `ConnErrorResult`/`ParseErrorResult` constructors on `METARResult`
* `METARResult` failure now carries a `NonEmpty METARError` so errors accumulate across sources (BOM states + NOAA)
* New module `Data.Aviation.Metar.Cache` (exports `readCache`, `mergeCache`)
* Drop `NoImplicitPrelude`; use standard `Prelude`
* Add `tagsoup`, `directory`, `filepath`, `http-types` dependencies; add upper bounds throughout
* Remove empty `tests` test-suite stanza
* Doctest coverage across all library modules

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
