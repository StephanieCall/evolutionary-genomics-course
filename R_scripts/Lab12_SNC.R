## Lab 12 - Network Analysis using Cytoscape and RCy3

# All the Cytoscape work is shown on in the RMarkdwon file

if(!"RCy3" %in% installed.packages()) {
  install.packages("BiocManager")
  BiocManager::install("RCy3")
}
library(RCy3)

## Getting started 

# First, launch Cytoscape and keep it running while using RCy3. Call for Cytoscape to use 
# it. To check that everything is installed and running properly, also call the version info
cytoscapePing()
cytoscapeVersionInfo()

## My first network
nodes <- data.frame(id=c('node 0', 'node 1', 'node 2', 'node 3'),
                   group = c('A','A','B','B'), # Categorical strings for the node grouping
                   score = as.integer(c(20, 10, 15, 5)), # Integers for the node values
                   stringsAsFactors = FALSE)
edges <- data.frame(source=c('node 0', 'node 0', 'node 0', 'node 2'), # Start of edge
                    target = c('node 1', 'node 2', 'node 3', 'node 3'), # End of edge
                    interaction = c('inhibits', 'interacts', 'activates', 'interacts'), # Optional
                    weight = c(5.1, 3.0, 5.2, 9.9), # Numeric for line weights
                    stringsAsFactors = FALSE)

createNetworkFromDataFrames(nodes, edges, title = 'My first network',
                            collection = 'DataFrame Example')


## Switch styles
# Change to a different premade style 
setVisualStyle('Marquee')

# Create and apply a custom style
style.name <- 'myStyle'
defaults <- list(NODE_SHAPE = 'diamond',
                 NODE_SIZE = 30,
                 EDGE_TRANSPARENCY = 120,
                 NODE_LABEL_POSITION = 'W,E,c,0.00,0.00')
nodeLabels <- mapVisualProperty('node label', 'id', 'p')
nodeFills <- mapVisualProperty('node fill color', 'group', 'd', c('A', 'B'), 
                               c('#FF9900', '#66AAAA'))
arrowShapes <- mapVisualProperty('Edge Target Arrow Shape', 'interaction', 'd', 
                                 c('activates', 'inhibits', 'interacts'),
                                 c('Arrow', 'T', 'None'))
edgeWidth <- mapVisualProperty('edge width', 'weight', 'p')

createVisualStyle(style.name, defaults, list(nodeLabels, nodeFills, arrowShapes, edgeWidth))
setVisualStyle(style.name)
# Unlock node dimensions in case you want to change height and width independently
lockNodeDimensions(FALSE, style.name)


## Bioconductor graph example 

# Start with a simple 4-node network
g <- new('graphNEL', edgemode = 'directed')
g <- graph::addNode('A', g)
g <- graph::addNode('D', g)
g <- graph::addNode('C', g, edges = list('D'))
g <- graph::addNode('B', g, edges = list(c('A', 'D', 'C')))
createNetworkFromGraph(g, title = "Simple network", collection = 'GraphNEL Example')

# Add node attributes 
# The simplest way to add node attributes is by using a data.frame object, but there 
# are other ways, such as adding the attributes when creating the network. See other 
# vignettes for the RCy3 package for details
df <- data.frame(moleculeType = c('kinase', 'TF', 'cytokine', 'cytokine'),
                 log2fc = c(1.8, 3.0, -1.2, -2.5),
                 row.names = c('A', 'B', 'C', 'D'), # row names = node names
                 stringsAsFactors = FALSE)
loadTableData(df)


## 5.2 - Modifying the distplay: defaults and mappings

# Change the defaults of the visuals using a series of set[Property]Default() functions
setNodeShapeDefault('OCTAGON')
setNodeColorDefault('#AAFF88')
setNodeSizeDefault(60)
setNodeFontSizeDefault(30)

# Map a property to a visual by specifying the property and mapping values in a 
# set[Property]Mapping() function
# First, determine the shapes available to map node shapes to
getNodeShapes() # Many options, including diamond, triangle, rectangle, ellipse, etc.
column <- 'moleculeType'
values <- c('kinase', 'TF', 'cytokine')
shapes <- c('DIAMOND', 'TRIANGLE', 'RECTANGLE')
setNodeShapeMapping(column, values, shapes)

# The node shapes was an example of mapping a discrete variable. Now, map a continuous
# variable (log2fc for this example) using a similar format but mapping to a few select
# control points rather than all possible points
column <- 'log2fc'
control.points <- c(-3.0, 0, 3.0)
colors <- c('#5588DD', '#FFFFFF', '#DD8855')
setNodeColorMapping(column, control.points, colors)

# You can specify two additional colors for extreme values that fall outside of the set 
# point limits.
control.points <- c(-2.0, 0, 2.0)
colors <- c('#2255CC', '#5588DD', '#FFFFFF', '#DD8855','#CC5522')
setNodeColorMapping (column, control.points, colors)

# Now, map the node sizes to the log2fc
control.points <- c(-3.0, 2.0, 3.0) # Note that the set points don't need to be symmetric
sizes <- c(20, 80, 90) # Nor do the values for the mappings
setNodeSizeMapping(column, control.points, sizes)


## 5.3 - Selecting Nodes

# Nodes can be selected using the seletcNodes() function and specifying how the nodes are 
# selected 
selectNodes('C', 'name')
getSelectedNodes()

# Select the first neighbors
selectFirstNeighbors()
getSelectedNodes()
# Return the node names in the selected list by assigning the output of getSelectedNodes() 
# to an object
node.names <- getSelectedNodes()
node.names

# Deselect the nodes with clearSelection()
clearSelection()
getSelectedNodes()
?clearSelection()


## 5.4 - Saving and export

# Save an entire session (all data, manipulations to network, etc. in a .cys file)
saveSession('data/vignette_session') # Saves as a .cys file


## 5.4.1 - Saving high resolution image files
# Many different image files can be save. See documentation for details
full.path <- paste(getwd(), 'data/vignette_image', sep = '/')
exportImage(full.path, 'PNG', zoom = 200) # Save a .png scaled by 200%
exportImage(full.path, 'PDF') # Save a .pdf image
?exportImage()

## 6 Browse available functions, commands, and arguments
# Explore the entire package using help()
help(package = RCy3)

# Open swagger docs to explore the API between R and Cytoscape
cyrestAPI()
commandsAPI()

# View lists of commands and arguements in RCy3 using commandsHelp()
commandsHelp('help') # For top-level commands
commandsHelp('help network') # For network commands.
commandsHelp('help network select')