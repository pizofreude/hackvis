# HackVis

![GitHub language count](https://img.shields.io/github/languages/count/pizofreude/hackvis)
![GitHub top language](https://img.shields.io/github/languages/top/pizofreude/hackvis)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

HackVis is an R-based Shiny application designed for visualizing and analyzing hackathon datasets. It provides an interactive interface for exploring hackathon data, enabling event organizers, participants, and researchers to derive meaningful insights.

## Repository Structure

```
hackvis/
├── data/           # Hackathon datasets
├── renv/           # R environment management for dependencies
├── scripts/        # Data utility scripts for the Shiny application
├── .Rprofile       # R profile configuration
├── .gitignore      # Git ignore file (excludes Shiny apps credentials)
├── LICENSE         # Apache 2.0 license
├── app.R           # Main Shiny application
├── hackvis.Rproj   # R project file
└── renv.lock       # R environment lock file with dependencies
```

## Features

- **Interactive Data Visualization**: Explore hackathon datasets through dynamic, interactive visualizations
- **Data Analysis Tools**: Analyze participant demographics, project categories, and other hackathon metrics
- **User-Friendly Interface**: Intuitive Shiny application design for users of all technical backgrounds

## Getting Started

### Prerequisites

- R >= 4.0.0
- RStudio (recommended)
- Required packages (automatically managed by renv):
  - shiny
  - tidyverse
  - [other dependencies in renv.lock]

### Installation

1. Clone the repository:
```bash
git clone https://github.com/pizofreude/hackvis.git
cd hackvis
```

2. Open the project in RStudio:
```bash
rstudio hackvis.Rproj
```

3. Install dependencies using renv:
```r
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()
```

### Running the Application

To run the Shiny application locally:

```r
shiny::runApp()
```

The application should open in your default web browser.

## Usage

1. Load hackathon data using the provided interface
2. Explore visualizations through the interactive dashboard
3. Filter and analyze data based on various parameters
4. Export or share insights as needed

## Data Utilities

The `scripts/` directory contains utility functions for:
- Data preprocessing and cleaning
- Feature extraction from hackathon datasets
- Helper functions for visualization components

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.

## Acknowledgements

- Major League Hacking for organizing the [Global Hack Week](https://ghw.mlh.io/)
- The R and Shiny communities for their excellent tools and documentation
- Contributors to the tidyverse and other dependencies
- Hackathon organizers and participants who provided feedback
