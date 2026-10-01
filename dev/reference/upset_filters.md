# UpSet chart filter helper functions

These functions create filters for the `annotation_upset_chart` class to
control which intersections are displayed in UpSet plots. Each function
returns a filter function that can be used with the `filter` parameter.

## Usage

``` r
upset_min_size(min_size)

upset_min_groups(min_groups)

upset_max_groups(max_groups)

upset_intersections(combinations)
```

## Arguments

- min_size:

  `numeric` The minimum number of items in an intersection

- min_groups:

  `numeric` The minimum number of groups in an intersection

- max_groups:

  `numeric` The maximum number of groups in an intersection

- combinations:

  `character` Vector of specific intersection combinations to include
  (e.g., c("A/B", "B/C"))

## Value

A function that takes `region_data` as input and returns a logical
vector indicating which intersections to keep.

## Details

These filter functions work by analyzing the region data from the Venn
diagram to determine which intersections meet the specified criteria:

- `upset_min_size()`: Filters intersections based on the number of items

- `upset_min_groups()`: Filters intersections based on the minimum
  number of groups involved

- `upset_max_groups()`: Filters intersections based on the maximum
  number of groups involved

- `upset_intersections()`: Filters to show only specific intersection
  combinations (e.g., "A/B", "B/C", "A/B/C")

For complex filtering logic, create custom filter functions:

    # Single filter
    filter = upset_min_size(5)

    # Specific combinations only
    filter = upset_intersections(c("A/B", "B/C"))

    # Custom filter function with AND logic
    custom_filter <- function(region_data) {
      region_data$count >= 3 & region_data$count <= 10 & grepl("A", region_data$name)
    }

    # Custom filter function with OR logic
    or_filter <- function(region_data) {
      region_data$count >= 5 | grepl("B/C", region_data$name)
    }

## Examples

``` r
# create a filter function that keeps intersections with 5+ items
f <- upset_min_size(5)
is.function(f)
#> [1] TRUE

# \donttest{
# Filter to show only intersections with 5+ items
C <- annotation_upset_chart(factor_name = "V1", filter = upset_min_size(5))

# Filter to show only intersections involving 3+ groups
C <- annotation_upset_chart(factor_name = "V1", filter = upset_min_groups(3))

# Filter to show only intersections involving 2-4 groups (custom function)
group_range_filter <- function(region_data) {
  group_counts <- sapply(region_data$name, function(x) {
    if (x == "") return(0)
    groups <- strsplit(x, "/")[[1]]
    length(groups)
  })
  group_counts >= 2 & group_counts <= 4
}
C <- annotation_upset_chart(factor_name = "V1", filter = group_range_filter)

# Filter to show only specific combinations
C <- annotation_upset_chart(factor_name = "V1", 
                          filter = upset_intersections(c("A/B", "B/C")))

# Custom filter combining size and group criteria
size_and_group_filter <- function(region_data) {
  region_data$count >= 3 & sapply(region_data$name, function(x) {
    if (x == "") return(FALSE)
    groups <- strsplit(x, "/")[[1]]
    length(groups) >= 2
  })
}
C <- annotation_upset_chart(factor_name = "V1", filter = size_and_group_filter)
# }
```
