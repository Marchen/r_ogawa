# ogawa: an R package for management of the Ogawa forest plot data

This package provides utility and graphic functions to manage the Ogawa forest
plot data.

## Installation

```R
# Install the 'devtools' package if you don't have it.
install.packages("devtools")

# Install the package
devtools::install_github("Marchen/r_ogawa")
```

## Usage

```R
library(ogawa)

#==============================================================================
#	Utility functions
#==============================================================================

# Validate quadrat codes.
is.valid.q(c("A1a", "Z2a"))

# Standardize quadrat codes.
standardize.q("A1a")

# Split quadrat codes.
q.to.elements(c("A1a1", "G2a1"))

# Construct standardized quadrat codes.
construct.q("A", 1, "a", 1)

# Check the quadrats are in core plot or not?
is.core(c("A1a1", "G2a1"))

# Convert quadrat codes to coordinates.
q.to.x(c("A1a1", "G2a1"))
q.to.y(c("A1a1", "G2a1"))
q.to.rect(c("A1a1", "G2a1"))


#==============================================================================
#	Plotting
#==============================================================================

# Create plot.
x <- create.ogawa.plot()

# Fill grid.
fill.q("G3a1", col = "green")

# Draw grid lines.
add.grid(x)

# At once.
create.quadrat.map("G3a1", col = "pink")

```
