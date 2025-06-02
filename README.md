# vigilaPeru <img src="man/figures/logo.png" align="right" height="120">

<!-- badges: start -->
[![R-CMD-check](https://github.com/your-org/vigilaPeru/workflows/R-CMD-check/badge.svg)](https://github.com/your-org/vigilaPeru/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

R tools for fast, reproducible access to Peru's national **epidemiological surveillance datasets** (Ministry of Health / CDC-Perú) published on the [Plataforma Nacional de Datos Abiertos](https://datosabiertos.gob.pe).

## Overview

`vigilaPeru` provides a unified interface to access, download, and analyze epidemiological surveillance data from Peru's open data portal. The package handles the complexities of the CKAN API, implements smart caching, and provides lightning-fast aggregation capabilities using `data.table`.

### Key Features

- 📊 **Live data access** - Direct connection to 7+ surveillance datasets including malaria, dengue, leishmaniasis, IRA, EDA, and more
- ⚡ **Lightning-fast aggregation** - Powered by `data.table` for efficient processing of millions of records
- 💾 **Smart caching** - Automatic local caching with incremental updates
- 🌐 **Bilingual** - Full documentation in Spanish and English
- 📦 **Interoperability** - Works seamlessly with tidyverse, Arrow, and GIS tools
- 🔄 **Reproducible** - Consistent data access for reproducible research

## Installation

```r
# Install development version from GitHub
remotes::install_github("your-org/vigilaPeru")
```

## Quick Start

```r
library(vigilaPeru)

# Download malaria surveillance data
malaria <- vp_download("malaria")

# Aggregate cases by year and department
malaria_summary <- vp_aggregate(
  malaria,
  by = c("ano", "departamento"),
  summarize = list(
    casos_total = sum,
    casos_promedio = mean
  )
)

# View available datasets
vp_datasets()
#> # A tibble: 7 × 8
#>   nombre        dataset_id                    titulo                     recursos
#>   <chr>         <chr>                         <chr>                         <int>
#> 1 malaria       vigilancia-epidemiológica-... Vigilancia epidemiológ...         2
#> 2 dengue        vigilancia-epidemiológica-... Vigilancia Epidemiológ...         1
#> 3 leishmaniasis vigilancia-epidemiológica-... vigilancia epidemiológ...         1
#> ...
```

## Core Functions

| Function | Purpose | Example |
|----------|---------|---------|
| `vp_datasets()` | List available surveillance datasets | `vp_datasets()` |
| `vp_download()` | Download and cache dataset | `vp_download("dengue")` |
| `vp_aggregate()` | Fast grouped summaries | `vp_aggregate(data, by = c("year", "week"))` |
| `vp_metadata()` | Get DCAT/DCAP-AP metadata | `vp_metadata("malaria")` |
| `vp_cache_dir()` | Manage local cache | `vp_cache_dir()` |

## Available Datasets

The package currently provides access to:

- **Malaria** - Historical data from 2000-2023
- **Dengue** - Complete surveillance records  
- **Leishmaniasis** - Cutaneous and mucocutaneous cases
- **IRA** - Acute Respiratory Infections
- **EDA** - Acute Diarrheal Diseases
- **Zoonosis** - Zoonotic disease surveillance
- **Carrión Disease** - Bartonellosis cases

## Examples

### Geographic Aggregation

```r
# Aggregate by department (state) level
dengue_dept <- vp_aggregate_geo(
  dengue_data,
  level = "departamento",
  include_totals = TRUE
)

# Aggregate by province
dengue_prov <- vp_aggregate_geo(
  dengue_data,
  level = "provincia"
)
```

### Time Series Analysis

```r
# Create monthly time series
monthly_cases <- vp_aggregate_time(
  malaria_data,
  period = "mes",
  by = "sexo"  # Additional grouping by sex
)

# Quarterly aggregation
quarterly <- vp_aggregate_time(
  data,
  period = "trimestre"
)
```

### Custom Aggregations

```r
# Multiple statistics by group
summary_stats <- vp_aggregate(
  data,
  by = c("ano", "departamento", "sexo"),
  summarize = list(
    total = sum,
    promedio = mean,
    maximo = max,
    casos_confirmados = function(x) sum(data$tipo_dx == "C")
  )
)
```

## Performance

The package is optimized for speed using `data.table`:

- Process 150,000+ records in < 1 second
- Aggregate millions of rows with minimal memory usage
- Smart caching reduces download time to zero for repeat analyses

## Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## Citation

If you use this package in academic work, please cite:

```
@software{vigilaperu2025,
  author = {Author Name},
  title = {vigilaPeru: Access to Peru's Epidemiological Surveillance Data},
  year = {2025},
  url = {https://github.com/your-org/vigilaPeru}
}
```

And cite the original data sources:

> CDC-Perú. Vigilancia epidemiológica. Plataforma Nacional de Datos Abiertos.
> https://datosabiertos.gob.pe

## License

This package is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

The surveillance data is provided by Peru's Ministry of Health under [Creative Commons BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## Acknowledgments

- Centro Nacional de Epidemiología, Prevención y Control de Enfermedades (CDC-Perú)
- Ministerio de Salud del Perú (MINSA)
- Red Nacional de Epidemiología (RENACE)

---

<p align="center">
Made with ❤️ for public health in Peru 🇵🇪
</p>