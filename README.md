# metar

Fetch aviation weather products from the Australian Bureau of Meteorology
(BOM), with NOAA fallback for METARs at non-Australian aerodromes.

## What's here

- **METAR observations** — Australian aerodromes (ICAO codes beginning with
  `Y`) are scraped from
  <https://www.bom.gov.au/aviation/observations/metar-speci/>; other codes
  fall back to <http://tgftp.nws.noaa.gov/data/observations/metar/stations/>.
- **Graphical Area Forecasts (GAFs)** — the current or next PNG chart for one
  of the ten BOM forecast areas
  (`WA-N`, `WA-S`, `NT`, `QLD-N`, `QLD-S`, `SA`, `NSW-W`, `NSW-E`, `VIC`,
  `TAS`), scraped from
  <https://www.bom.gov.au/aviation/gaf/gaf.shtml>. Which of the four rotating
  products counts as "current" or "next" is derived from the UTC hour.
- **Grid Point Wind & Temperature forecasts (GPWTs)** — the PNG chart for a
  given flight-level band (`low`, `mid`, `high`), area (e.g. `AUS`, `NSW`,
  `QLD-N`, `VIC/TAS`, `TIMS`) and three-hourly UTC time-slot (`00Z`, `03Z`
  … `21Z`), scraped from
  <https://www.bom.gov.au/aviation/charts/grid-point-forecasts/>.

## Library modules

| Module                    | Entry point                                                                       |
|---------------------------|-----------------------------------------------------------------------------------|
| `Data.Aviation.Metar`     | `getMETAR :: String -> METARResultT IO String`                                    |
| `Data.Aviation.GAF`       | `getGAF   :: String -> GAFPeriod -> IO (Either GAFError GAFImage)`                |
| `Data.Aviation.GPWT`      | `getGPWT  :: GPWTLevel -> String -> String -> IO (Either GPWTError GPWTImage)`    |

Area codes for `getGPWT` are matched case-insensitively with punctuation
ignored, so `VIC/TAS`, `vic-tas` and `VICTAS` are all equivalent.

## Command line

The bundled `metar` executable is a thin wrapper around `getMETAR`:

```
$ metar YSSY
METAR YSSY 231000Z AUTO 04007KT 9999 // NCD 18/13 Q1026 RMK RF00.0/000.0
$ metar KJFK
KJFK 231005Z 11009KT 10SM SCT009 BKN016 BKN042 BKN140 22/21 A2979 RMK …
```

The GAF and GPWT modules are library-only in this package; an HTTP surface
lives in the sibling [`metar-http`](https://github.com/tonymorris/metar-http)
project.

## Development

```
bin/lint.sh          hlint + fourmolu (check)
bin/lint.sh --fix    apply fixes
bin/test.sh          run doctests across the library
```

----

![metar](https://i.imgur.com/VGTogB8.gif)
