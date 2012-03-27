This is a basic decription of how the visualization application operates and how to use it.

--- Visualizing Firewall Policies ---
Note: The application is not currently done (8/13/10), so this is a bit theoretical. If you actually follow these steps, you'll
get a visualization that doesn't convey all the information. 

1. Create Margrave vocabulary and policy files which describe your policy.

2. Describe your topology as a graph with nodes and edges according to the topology file format. This is a simple format, just look at
topotest.rkt to get an idea of how this is done.

3. Look at example.rkt to see how to set up your program. Basically:
 - Start Margrave
 - Load your policies
 - Specify your keywords (ipsrc, ipdest, etc.)
 - Specify your query (see step 4)
 - Call visualize
 
4. Your query must be formed in a way the application can understand. It should be a tupling query which has IDBOUTPUT for each possible policy decision.

5. Run the application, and use the controls (or, currently, the space bar) to advance through the models. 


--- Application Design ---
The main file is visualize.rkt, which has the visualize function. The program creates a model object, which contains the response xml from Margrave and a
few functions which are used to derive graph information from the xml data. For example, there's a function which determines if a specific named entity
is the ipsrc or ipdest in the model. The function apply-model (in apply-model.rkt) takes a model object and a graph and annotates the graph with the information
from the model. 

There are two graphs, the netgraph and the modelgraph. A netgraph is the basic topology, without annotations. A modelgraph is a transformed netgraph with the
information from the model. So, when using the program, only one netgraph will be created (from the topology file) but many modelgraphs will be created
(one for each returned model from Margrave.) To make extending the application easier, netgraph/modelgraph nodes can be given position information using a mixin. My
implementation of the visualization uses these pos-modelgraph-nodes and the positions are user-specified, but a different implementation may be able to figure
out the position algorithmically. 

Some custom GUI controls were made, to support drawing nodes and edges; these are in controls.rkt. 

load-netgraph.rkt includes a macro for loading a topology from a file in a nice format. 