require(checkmate)
require(igraph)
require(tibble)

#' Simulate from a DAG specified by an igraph object
#'
#' @param model_graph A DAG specified as an igraph object.
#' @param simulators A named list containing simulation functions for each
#' vertex of the DAG. Each simulation function must take 3 arguments: the sample
#' size "n", a list containing the simulated values from its parents
#' "parents", and any model parameters "parameters".
#' @param n Number of samples to draw.
#'
#' @returns A tibble containing samples from the DAG model.
sim_dag <- function(model_graph, simulators, parameters, n) {
    # Argument checks
    assert_class(model_graph, "igraph")
    assert_list(simulators)
    assert_set_equal(names(V(model_graph)), names(simulators))
    for (simulator in simulators) {
        assert_function(simulator, args = c("n", "parents", "parameters"))
    }
    assert_count(n)

    # Initialise container
    result <- tibble(id = 1:n)

    # Sort variables
    node_names <- names(topo_sort(model_graph))

    # Simulate each variable
    for (node_name in node_names) {
        # Simulation function
        node_simulator <- simulators[[node_name]]

        # Parents
        node_parent_names <- names(
            neighbors(model_graph, node_name, mode = "in")
        )
        node_parents <- as.list(result[node_parent_names])

        # Parameters
        node_parameters <- parameters[[node_name]]

        # Simulate
        result[node_name] <- node_simulator(
            n = n, parents = node_parents, parameters = node_parameters
        )
    }

    result
}
