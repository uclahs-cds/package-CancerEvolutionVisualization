# CancerEvolutionVisualization 3.0.0 (2025-01-06)

## Added
* Dendrogram mode
* Optional "spread" column to control node/branch spacing
* Plotting functions to visualize the distribution of clones across the genome.
* Documentation for heatmaps and clone-genome distribution plot
* Option to disable node drawing with node-by-node control
* Node-by-node control of node size
* Aesthetic changes for heatmap and clone-genome distribution plot
* Add parameters to specify polygon shape, width, color, and transparency (alpha).
* Add option to use scale bars instead of y-axes.
* Wrapper function for `SRCgrob` to automatically save plots to file
* Add option to annotate the CCF summary heatmap with the cell values.
* Add support for 1xn and 1x1 heatmaps.

## Update
* Fixed angle calculation bug where child angles do not follow
  their parent angle, instead moving "downward" at 0 degrees.
* Updated package metadata and README
* Split angle handling for `radial` and `dendrogram` modes to optimized each
* Fixed bug where horizontal tree segments rendered as zero-length segments
* Updated changelog format to NEWS.md Markdown format
* Refactored use of plyr/dplyr and stringr functions to remove dependencies
* Set default parameters for heatmaps, defaulting to BPG defaults unless necessary
* Improved default node style
* Define color scheme with a single value, automatically generating other shades.
* Report the number of SNVs per clone in the legend for the clone-genome distribution plot.
* Updated the user guide to reflect new features.
* Fix bug where the x-axis only renders when y-axis is also rendered.

## Bug
* Resolved issue where the spread parameter was not applied in dendrogram mode.
* Resolved issue for simple dendrogram trees ( < 6 nodes or binary tree), where node angles were not calculated correctly.

# CancerEvolutionVisualization 2.0.1 (2023-11-17)

## Added
* GitHub links for code and bug reports

## Bug
* Fixed S3 naming conflict in heatmap functions, using safe "create." prefix
* Fixed error when input is monoclonal


# CancerEvolutionVisualization 2.0.0 (2023-11-16)

## Added
* Option to specify edge colour with "edge.col.1" and "edge.col.2" 
  columns in tree input dataframe
* Option to specify edge width using "edge.width.1" and "edge.width.2" 
  columns in tree input dataframe
* Option to specify edge linetype with "edge.type.1" and "edge.type.2" 
  columns in tree input dataframe
* Support for specifying tree angles in either radians or degrees using
  an optional "angle" column
* Generic functions to generate accompanying heatmaps
* Option to specify tree node colours with "node.col" column
* Option to specify tree node border colour, width, and line-type with 
  "border.col", "border.width", and "border.type" columns
* Option ot specify tree node label colour with "node.label.col" column

## Update
* Reimplemented tree angle calculations
* Fixed lopsided radial tree bug

## Removed
* "seg1.col" and "seg2.col" parameters (replaced by tree input columns).
* "node.col" parameter to SRCGrob. (Node colour only customizable through tree input data.frame.)


# CancerEvolutionVisualization 1.0.1 (2022-10-03)

## Update
* Package title change for CRAN submission


# CancerEvolutionVisualization 1.0.0 (2022-09-28)

## Added
* Documentation for default colour scheme
* Checks for valid tree structure
  - Valid root node
  - Circular node references

## Update
* Changed gene input to a generic node text input, where
  style and colour are specified directly (not through
  SNV or CNA values).
* Fixed discrepancies between documentation and code
* Fixed bug related to referencing an uninitialized variable
* Fixed y-axis positioning bug
* Updated packaging/development dependencies
* Update User Guide
* Remove README from build


# CancerEvolutionVisualization 0.10.0 (2022-08-01)

## Added
* Optional SNV column in gene input data to italicize gene text
* Validates gene.line dist input value

## Update
* Changed gene input "gene" column to more generic "name"

## Removed
* Removed extra.len parameter to allow the value to be inferred by the
  presence/absence of CP values.
* Removed spread parameter until radial nodes are supported.
* Removed wid parameter, as it is no longer needed with horizontal.padding
* Removed cluster.list parameter until (pie nodes are implemented)


# CancerEvolutionVisualization 0.9.0 (2022-07-20)

## Removed
* Removed ylimits and yaxis.interval parameters.
  (yat can be used instead.)
* Removed filename parameter from SRCGrob to follow grid patterns.


# CancerEvolutionVisualization 0.8.0 (2022-07-14)

## Update
* Added "smart" branch length scaling based on the branch lengths and
  tree depth.
* The user can still scale the lengths proportionally with the scale1
  and scale2 arguments.


# CancerEvolutionVisualization 0.7.0 (2022-06-28)

## Update
* Added yat parameter to allow specific Y axis tick values

## Removed
* Removed yaxis.interval parameters (replaced with yat) 


# CancerEvolutionVisualization 0.6.0 (2022-06-24)

## Update
* Changed output format to only return the tree grob itself
  (no longer including intermediate values)


# CancerEvolutionVisualization 0.5.0 (2022-06-24)

## Update
* Automatically sets the branch angle to pi / 6
* Infers whether to draw polygons based on existence and changed
  parameter to optionally disable polygons
* Infers whether to add gene text based on presence of gene dataframe
* Changed "title" parameter names to "main"
* Separated axis parameters by X/Y axis instead of accepting a list
* Renamed "rad" parameter to "node.radius"
* Made tree input required
* Changed title unit type to a generic "unit.type" parameter to apply
  to any other position parameters that might be added
* Infers Y axis position based on the labels that are provided
* Changed "nodes" parameter to boolean "draw.nodes" to enable/disable
  node circles

## Removed
* Removed the fixed_angle argument (to be replaced by an angle column
  in the tree input data.frame)
* Removed add.genes parameter
* Removed unused sig.curve parameter
* Disconnected pie node functionality
* Removed y.axis.position parameter
* Disabled genes.on.nodes mode


# CancerEvolutionVisualization 0.4.1 (2022-05-05)

## Update
* Automatically adjusts node size, shape, and text size based on the
  length of the label text


# CancerEvolutionVisualization 0.4.0 (2022-05-03)

## Update
* Combined gene input dataframes into one

## Bug
* Fixed issue when trunk node is not positioned first in the input tree


# CancerEvolutionVisualization 0.3.0 (2022-04-29)

## Update
* Allow node labels to be specified separately

## Bug
* Fixed issue when creating ellipse nodes for longer labels


# CancerEvolutionVisualization 0.2.1 (2022-04-14)

## Update
* Allow tree input without CP and validate CP input values


# CancerEvolutionVisualization 0.2.0 (2022-04-12)

## Update
* Include tree preparation in SRCGrob to simplify use and improve
  consistency. 


# CancerEvolutionVisualization 0.1.1 (2022-02-23)

## Update
* Remove input sampling (assume that data has been prepared properly)


# CancerEvolutionVisualization 0.1.0 (2021-11-08)

## Update
* Add functions for deep comparison of plot Grobs.
* Update tests to use new comparisons and add test cases.


# CancerEvolutionVisualization 0.0.0 (2021-09-13)

INITIAL FEATURES
* Tree, CNA, and SNV input sampling
