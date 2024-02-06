# ogawa: an R package for management of the Ogawa forest plot data

This package provides utility and graphic functions to manage the Ogawa forest
plot data.

## Installation

```R
# Install the 'devtools' package if you don't have it.
install.packages("devtools")

# Install the package.
devtools::install_github("Marchen/r_ogawa")
```

## Usage

```R
library(ogawa)

#------------------------------------------------------------------------------
#   Utility functions
#------------------------------------------------------------------------------

# Validate quadrat codes.
is_valid_q(c("A1a", "Z2a"))

# Standardize quadrat codes.
standardize_q("A1a")

# Split quadrat codes.
q_to_elements(c("A1a1", "G2a1"))

# Construct standardized quadrat codes.
construct_q("A", 1, "a", 1)

# Check the quadrats are in the 1.2ha core plot.
is_core(c("A1a1", "G2a1"))

# Convert quadrat codes to coordinates.
q_to_x(c("A1a1", "G2a1"))
q_to_y(c("A1a1", "G2a1"))
q_to_point(c("A1a1", "G2a1"), pos = "center")
q_to_rect(c("A1a1", "G2a1"))

# Check if the plot pairs touch each other.
q1 <- c("A1a2", "A1a2", "A1a1")
q2 <- c("A1a4", "B1a2", "A1a3")
touches(q1, q2)

# Check if the quadrats are in the specified region.
is_in("A1a1", 0, 100, 0, 200)
is_in("A1a1", q.from = "B01a1", q.to = "B3c2")

# Validate 1cm tag format.
is_valid_1cm_tag("A0000111")
is_valid_1cm_tag("A00111")

# Validate coordinate.
# -> Valid
invalid_xy(sq2 = 1, x = 3, y = 7)
# -> Invalid
invalid_xy(sq2 = 2, x = 3, y = 7)

#------------------------------------------------------------------------------
#   Plotting
#------------------------------------------------------------------------------

# Create plot.
x <- create_ogawa_plot()

# Fill grid.
fill_q("G3a1", col = "green")

# Draw grid lines.
add_grid(x)

# At once.
create_quadrat_map("G3a1", col = "pink")


#------------------------------------------------------------------------------
#   Data validation
#------------------------------------------------------------------------------

# Find unknown records with measurements.
find_unknown_with_measurement(census_data)

# Find resurrection (i.e., L -> U or D -> L)
find_resurrection(census_data)

# Find multiple measurement for ids or tag numbers.
find_multiple_measurements(census_data)

# Find tags on different stems.
find_tags_on_different_stems(census_data)

# Find unused IDs.
find_missing_ids(stem_data)

```
