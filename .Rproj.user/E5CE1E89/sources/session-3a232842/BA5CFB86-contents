# Setting up GitHub and GitHub Pages for vigilaPeru

## 1. Create GitHub Repository

1. Go to [GitHub.com](https://github.com) and sign in
2. Click the "+" icon in the top right → "New repository"
3. Repository settings:
   - Name: `vigilaPeru`
   - Description: "Fast access to Peru's epidemiological surveillance datasets"
   - Public repository (required for GitHub Pages)
   - DO NOT initialize with README (you already have one)
   - DO NOT add .gitignore or license (you already have them)

## 2. Push Your Local Repository

```bash
# Navigate to your package directory
cd /Users/hchacont/Repositories/peru_surveillance/vigilaPeru

# Initialize git if not already done
git init

# Add all files
git add .

# Initial commit
git commit -m "Initial commit: vigilaPeru package"

# Add GitHub remote (replace YOUR_USERNAME with your GitHub username)
git remote add origin https://github.com/YOUR_USERNAME/vigilaPeru.git

# Push to GitHub
git push -u origin main
```

## 3. Set Up GitHub Pages with pkgdown

### Install pkgdown
```r
install.packages("pkgdown")
```

### Configure pkgdown
Create `_pkgdown.yml` in your package root:

```yaml
url: https://YOUR_USERNAME.github.io/vigilaPeru/

template:
  bootstrap: 5
  bootswatch: cosmo
  bslib:
    primary: "#0054AD"
    border-radius: 0.5rem
    btn-border-radius: 0.25rem

home:
  title: "vigilaPeru - Acceso a Datos de Vigilancia Epidemiológica del Perú"
  description: "Acceso rápido y reproducible a los datasets de vigilancia epidemiológica del Perú"

navbar:
  title: "vigilaPeru"
  left:
    - icon: fa-home
      href: index.html
    - text: "Inicio/Start"
      href: articles/index.html
    - text: "Referencia/Reference"
      href: reference/index.html
    - text: "Artículos/Articles"
      menu:
        - text: "Demo Malaria y Dengue"
          href: articles/malaria-dengue-demo.html
        - text: "Extender a Otras Enfermedades"
          href: articles/extending-to-other-diseases.html
  right:
    - icon: fa-github
      href: https://github.com/YOUR_USERNAME/vigilaPeru

reference:
- title: "Funciones Principales / Main Functions"
  desc: "Funciones centrales para acceder a datos / Core functions for data access"
  contents:
  - vp_download
  - vp_datasets
  - vp_metadata
  
- title: "Agregación de Datos / Data Aggregation"
  desc: "Funciones para agregación rápida / Functions for fast aggregation"
  contents:
  - vp_aggregate
  - vp_aggregate_geo
  - vp_aggregate_time
  
- title: "Gestión de Caché / Cache Management"
  desc: "Control del caché local / Local cache control"
  contents:
  - vp_cache_dir
  - vp_cache_clear
  - vp_cache_info
  
- title: "Funciones UBIGEO / UBIGEO Functions"
  desc: "Utilidades para códigos geográficos / Geographic code utilities"
  contents:
  - starts_with("ubigeo_")

footer:
  structure:
    left: developed_by
    right: built_with
```

### Build the website locally
```r
# Build the pkgdown site
pkgdown::build_site()

# Preview locally
pkgdown::preview_site()
```

## 4. Set Up GitHub Actions for Automatic Website Deployment

Create `.github/workflows/pkgdown.yaml`:

```yaml
# Workflow derived from https://github.com/r-lib/actions/tree/v2/examples
# Need help debugging build failures? Start at https://github.com/r-lib/actions#where-to-find-help
on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]
  release:
    types: [published]
  workflow_dispatch:

name: pkgdown

jobs:
  pkgdown:
    runs-on: ubuntu-latest
    # Only restrict concurrency for non-PR jobs
    concurrency:
      group: pkgdown-${{ github.event_name != 'pull_request' || github.run_id }}
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
    permissions:
      contents: write
    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-pandoc@v2

      - uses: r-lib/actions/setup-r@v2
        with:
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::pkgdown, local::.
          needs: website

      - name: Build site
        run: pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
        shell: Rscript {0}

      - name: Deploy to GitHub pages 🚀
        if: github.event_name != 'pull_request'
        uses: JamesIves/github-pages-deploy-action@v4.5.0
        with:
          clean: false
          branch: gh-pages
          folder: docs
```

## 5. Enable GitHub Pages

1. Push all changes to GitHub:
   ```bash
   git add .
   git commit -m "Add pkgdown configuration"
   git push
   ```

2. Go to your repository on GitHub
3. Click on "Settings" tab
4. Scroll down to "Pages" section
5. Under "Source", select:
   - Source: Deploy from a branch
   - Branch: `gh-pages`
   - Folder: `/ (root)`
6. Click "Save"

## 6. Add Badges to README

Update your README.md to include badges:

```markdown
# vigilaPeru <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/YOUR_USERNAME/vigilaPeru/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YOUR_USERNAME/vigilaPeru/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/YOUR_USERNAME/vigilaPeru/branch/main/graph/badge.svg)](https://app.codecov.io/gh/YOUR_USERNAME/vigilaPeru?branch=main)
[![pkgdown](https://github.com/YOUR_USERNAME/vigilaPeru/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/YOUR_USERNAME/vigilaPeru/actions/workflows/pkgdown.yaml)
<!-- badges: end -->
```

## 7. Optional: Create a Package Logo

```r
# Install hexSticker package
install.packages("hexSticker")

# Create a simple logo
library(hexSticker)
library(ggplot2)

# Create a plot representing Peru/epidemiology
p <- ggplot() + 
  theme_void() + 
  annotate("text", x = 0, y = 0, label = "vP", 
           size = 30, fontface = "bold", color = "#0054AD")

# Create hex sticker
sticker(p, 
        package = "vigilaPeru", 
        p_size = 8, 
        p_color = "#0054AD",
        h_fill = "#FFFFFF", 
        h_color = "#0054AD",
        filename = "man/figures/logo.png")
```

## 8. Final Steps

After the GitHub Actions run:
1. Your package website will be available at: `https://YOUR_USERNAME.github.io/vigilaPeru/`
2. The website will automatically update whenever you push changes to the main branch
3. Test coverage reports will be available at Codecov (you may need to sign up at codecov.io with your GitHub account)

## Troubleshooting

- If GitHub Pages doesn't work immediately, wait a few minutes for DNS propagation
- Check the Actions tab in your GitHub repository to see if workflows are running successfully
- Make sure your repository is public (required for GitHub Pages on free accounts)
- If pkgdown build fails, check that all your documentation is properly formatted

## Additional Resources

- [pkgdown documentation](https://pkgdown.r-lib.org/)
- [GitHub Pages documentation](https://docs.github.com/en/pages)
- [R-hub blog on pkgdown](https://blog.r-hub.io/2019/01/09/pkgdown/)