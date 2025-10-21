# metadataTools

`metadataTools` is an R package designed to simplify the process of documenting datasets with structured metadata. It provides tools to:

- Annotate data frames with variable-level descriptions and units  
- Generate human-readable README files for data repositories (e.g., DataverseNO)  
- Document and validate naming conventions for datasets

## Features

### 📝 Add Metadata to Columns
- `add_desc()` – Adds a `"description"` attribute to columns. If the description contains square brackets (e.g., `"Length [cm]"`), the content inside the brackets is stored as a `"unit"` attribute.
- `add_unit()` – Adds a `"unit"` attribute to columns directly.
- Use `attr(df, "description")` to describe the dataset as a whole.

### 📄 Generate README Files
- `create_readme()` produces a structured `README.txt` file with:
  - Dataset title, DOI, contact info
  - Dataset description and methodology
  - File- and variable-level metadata
- `set_metadata_defaults()` lets you store default metadata values (e.g., name, institution, email, ORCID) used by `create_readme()` to streamline README generation.
- Compatible with the DataverseNO metadata schema.

### 📊 Create Metadata Tables
- `create_metadata_table()` builds a formatted `flextable` summarizing variable names, types, units, and descriptions.

### ✅ Validate Projects' Naming Conventions
- `validate_naming_convention()` checks whether a dataset contains the mandatory variables and only valid entries as defined in a project's naming convention (e.g., `EcoForest_naming`). It also adds standardized variable descriptions to the dataset.
- `view_naming_convention()` prints a human-readable overview of the naming convention.


## 🔧 Installation

You can install the development version of `metadataTools` directly from GitHub using the `remotes` package:

```r
# Install remotes if not already installed
install.packages("remotes")

# Install metadataTools from GitHub
remotes::install_github("asplundj/metadataTools")
