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

#------------------------------------------------------------------------------
#   Utility functions
#------------------------------------------------------------------------------

# Validate quadrat codes.
is.valid.q(c("A1a", "Z2a"))

# Standardize quadrat codes.
standardize.q("A1a")

# Split quadrat codes.
q.to.elements(c("A1a1", "G2a1"))

# Construct standardized quadrat codes.
construct.q("A", 1, "a", 1)

# Check the quadrats are in the 1.2ha core plot.
is.core(c("A1a1", "G2a1"))

# Convert quadrat codes to coordinates.
q.to.x(c("A1a1", "G2a1"))
q.to.y(c("A1a1", "G2a1"))
q.to.rect(c("A1a1", "G2a1"))

# Check if the plot pairs are adjacent.
q1 <- c("A1a2", "A1a2", "A1a1")
q2 <- c("A1a4", "B1a2", "A1a3")
is.adjacent(q1, q2)

# Check if the quadrats are in the specified region.
is.in("A1a1", 0, 100, 0, 200)
is.in("A1a1", q.from="B01a1", q.to = "B3c2")


#------------------------------------------------------------------------------
#   Plotting
#------------------------------------------------------------------------------

# Create plot.
x <- create.ogawa.plot()

# Fill grid.
fill.q("G3a1", col = "green")

# Draw grid lines.
add.grid(x)

# At once.
create.quadrat.map("G3a1", col = "pink")

```
