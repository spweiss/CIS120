type 'a node  = { name: 'a;
                  edges: 'a list ref }
    
type 'a graph = 'a node list ref
    
val g : string graph
val valid : 'a graph -> bool
val invalid_graph : unit -> string graph
val remove : 'a -> 'a graph -> unit
val dfs : 'a graph -> 'a list
