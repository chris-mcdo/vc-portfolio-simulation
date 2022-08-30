require(checkmate)
require(tibble)
require(dplyr)

#' Random draws from the pareto distribution.
#'
#' @param n Number of draws to take.
#' @param shape Shape parameter; controls the thickness of the tail.
#' @param scale Scale parameter; the minimum possible value of the variable.
#'
#' @returns A sample of size n drawn from the Pareto(shape, scale) distribution.
rpareto <- function(n, shape, scale) {
    scale * runif(n = n, min = 0, max = 1)^(-1 / shape)
}

#' Random draws from the pareto distribution.
#'
#' @param n Number of draws to take.
#' @param shape Shape parameter; controls the thickness of the tail.
#' @param scale1 Scale parameter; the minimum possible value of the variable.
#' @param scale2 Scale parameter; the maximum possible value of the variable.
#'
#' @returns A sample of size n drawn from the Pareto(shape, scale) distribution.
rpareto_truncated <- function(n, shape, scale1, scale2) {
    numerator <- 1 - runif(n) * (1 - (scale1 / scale2)^shape)
    denominator <- scale1^shape
    (numerator / denominator)^(-1 / shape)
}

#' Simulate whether a single firm is successful (has multiplier > 1) or not.
#'
#' A given firm is "successful" with probability p
success_simulator <- function(n, parents, parameters) {
    check_count(n)
    check_list(parents)

    # Success probability
    p <- parameters$success_probability
    check_number(p, lower = 0, upper = 1)

    sample(c(FALSE, TRUE), size = n, replace = TRUE, prob = c(1 - p, p))
}

#' Simulate the return on investment of a single firm, given that it succeeded
#' or failed.
#'
#' Uses a Pareto distribution.
multiplier_simulator <- function(n, parents, parameters) {
    check_count(n)
    check_list(parents)
    check_logical(parents$success)
    check_int(parameters$exit_year, lower = 1, upper = 8)
    check_number(parameters$shape1, lower = 0)
    check_number(parameters$shape2, lower = 0)

    # Success distribution
    shape <- parameters$power_law_parameter[parameters$exit_year]
    success_draws <- rpareto(n = n, shape = shape, scale = 1)

    # Failure distribution
    failure_draws <- rbeta(
        n = n, shape1 = parameters$shape1, shape2 = parameters$shape2
    )

    # Construct final result
    result <- failure_draws
    result[parents$success] <- success_draws[parents$success]

    result
}

#' Simulate the return on investment of a single firm, given that it succeeded
#' or failed.
#'
#' Uses a truncated Pareto distribution.
truncated_multiplier_simulator <- function(n, parents, parameters) {
    check_count(n)
    check_list(parents)
    check_logical(parents$success)
    check_int(parameters$exit_year, lower = 1, upper = 8)
    check_number(parameters$scale, lower = 0)
    check_number(parameters$shape1, lower = 0)
    check_number(parameters$shape2, lower = 0)

    # Success distribution
    shape <- parameters$power_law_parameter[parameters$exit_year]
    success_draws <- rpareto_truncated(
        n = n, shape = shape, scale1 = 1, scale2 = parameters$scale
    )

    # Failure distribution
    failure_draws <- rbeta(
        n = n, shape1 = parameters$shape1, shape2 = parameters$shape2
    )

    # Construct final result
    result <- failure_draws
    result[parents$success] <- success_draws[parents$success]

    result
}

#' Simulate the return of an equally-distributed investment among a portfolio of
#' k identical firms, via Monte Carlo simulation
#'
#' @param n Number of Monte Carlo samples to draw
#' @param k Number of firms in the portfolio
#' @param model_function A function which returns draws from the distribution of
#' multipliers of individual investments. The function should take the sample
#' size "n" as an argument.
portfolio_simulator <- function(n, k, model_function) {
    model_function(n * k) %>%
        add_column(group_id = rep(1:n, each = k)) %>%
        group_by(group_id) %>%
        summarise(
            net_multiplier = mean(multiplier), success_fraction = mean(success)
        )
}
