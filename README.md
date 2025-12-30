# geoManipulatoR

`geoManipulatoR` is an R package for working with UK geographic data, with a focus on
**Index of Multiple Deprivation (IMD)** analysis and **postcode-based geographic lookups**.

It provides tools to:

- Aggregate IMD scores from **LSOA level to higher geographies** using population weighting
- Calculate **national ranks and deciles** at each geography level
- Download **postcode-level lookups** from the ONS Postcode Directory (ONSPD)
- Enrich postcodes with **LSOA, MSOA, ward, constituency, and local authority codes**
- Optionally attach **IMD scores and population data** to postcode-level data

The package is designed for **public health**, **local government**, and **health
intelligence** workflows, where reproducible and consistent geography handling
is essential.

---

## Installation

### From GitHub

```r
# install.packages("devtools")

devtools::install_github("BCC-PHM/geoManipulatoR")
```

---

## Data sources

### ONS Postcode Directory (ONSPD)

Postcode-level data are accessed via the **ONS Postcode Directory ArcGIS REST API**
to provide up-to-date postcode lookups.

### Packaged IMD lookup dataset

The package includes a bundled lookup dataset containing:

- LSOA-level IMD scores 2025
- 2024 Population predictions from NOMIS
- Higher geography codes and names from data.gov.uk

Where LSOA do not perfectly fit into higher geographies best-fit lookups were used.

---

## Core functions

### get_imd()

Aggregates IMD scores to a chosen geography level, calculates **national ranks and
deciles**, and optionally filters to one or more Local Authorities.

```r
imd_lsoa <- get_imd()
imd_bham <- get_imd("Birmingham")
imd_bham_msoa <- get_imd("Birmingham", level = "MSOA21CD")
```

```r
imd_bs <- get_imd(c("Birmingham", "Solihull"), level = "MSOA21CD")
```

Supported geography levels:

- LSOA21CD (default)
- MSOA21CD
- WD25CD (wards)
- PCON24CD (parliamentary constituencies)
- LA24CD (local authorities)

---

### get_postcode_lookup()

Downloads postcode-level data from the ONS Postcode Directory and optionally
enriches it with higher geographies and IMD/Population information from its LSOA. Also has the functionality to return the LAT and LONG co-ordinates of the postcode centroids.

```r
pc_lsoa <- get_postcode_lookup("Birmingham")
pc_geogs <- get_postcode_lookup("Birmingham", add_geogs = TRUE)
pc_imd <- get_postcode_lookup("Birmingham", add_geogs = TRUE, add_imd = TRUE)
pc_latlon<- get_postcode_lookup("Birmingham", add_latlon = TRUE)

```

---

## Handling area names and codes

Both core functions accept:

- Local Authority names (e.g. "Birmingham", "Solihull")
- Local Authority codes (e.g. "E08000025")

Name matching uses **fuzzy matching** (Jaro–Winkler distance).

---

## Author

Daniel Slater  
Analysis & Data Visualisations Officer (Public Health)

---

## License

MIT License
