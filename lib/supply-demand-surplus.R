demand <- function(Q) 20 - 0.5 * Q
demand_new <- function(Q) demand(Q) + 5
supply <- function(Q) 2 + 0.25 * Q
supply_new <- function(Q) supply(Q) + 5

supply_tax <- function(Q) supply(Q) + 5

demand_elastic <- function(Q) 10 - 0.05 * Q
demand_inelastic <- function(Q) 20 - 2 * Q

supply_elastic <- function(Q) 2 + 0.05 * Q
supply_elastic_tax <- function(Q) supply_elastic(Q) + 5
supply_inelastic <- function(Q) 2 + 1.5 * Q
supply_inelastic_tax <- function(Q) supply_inelastic(Q) + 5


midpoint <- function(ymin, ymax) {
  ymax + (ymin - ymax) / 2
}

tax_graph <- function(demand_fun, supply_fun, supply_tax, title, shaded = FALSE) {
  equilibrium <- uniroot(function(x) supply_fun(x) - demand_fun(x), c(0, 45))$root
  equilibrium_tax <- uniroot(function(x) supply_tax(x) - demand_fun(x), c(0, 45))$root
  
  x_q_tax <- seq(0, equilibrium_tax, 0.1)
  x_q_dwl <- seq(equilibrium_tax, equilibrium, 0.1)
  
  surplus_labels <- tribble(
    ~x, ~y, ~text, ~fill,
    1, midpoint(demand_fun(equilibrium_tax), max(demand_fun(x_q_tax))), 
    "Consumer surplus", nord_green,
    1, midpoint(min(supply_fun(x_q_tax)), supply_fun(equilibrium_tax)), 
    "Producer surplus", nord_lt_blue,
    equilibrium_tax + 1, midpoint(min(supply_fun(x_q_dwl)), max(demand_fun(x_q_dwl))), 
    "DWL", nord_purple,
    1, midpoint(demand_fun(equilibrium), demand_fun(equilibrium_tax)), 
    "Consumer tax burden", nord_yellow,
    1, midpoint(supply_fun(equilibrium), supply_fun(equilibrium_tax)), 
    "Producer tax burden", nord_yellow
  )
  
  if (shaded) {
    base_plot <- ggplot(mapping = aes(x = 0:45)) +
      geom_ribbon(aes(x = x_q_tax, 
                      ymin = demand_fun(equilibrium_tax), ymax = demand_fun(x_q_tax)),
                  alpha = 0.3, fill = nord_green) +
      geom_ribbon(aes(x = x_q_tax, 
                      ymin = supply_fun(x_q_tax), ymax = supply_fun(equilibrium_tax)),
                  alpha = 0.3, fill = nord_lt_blue) +
      geom_ribbon(aes(x = x_q_dwl, 
                      ymin = supply_fun(x_q_dwl), ymax = demand_fun(x_q_dwl)),
                  alpha = 0.3, fill = nord_purple) +
      geom_ribbon(aes(x = x_q_tax, 
                      ymin = demand_fun(equilibrium), ymax = demand_fun(equilibrium_tax)),
                  alpha = 0.3, fill = nord_yellow) +
      geom_ribbon(aes(x = x_q_tax, 
                      ymin = supply_fun(equilibrium), ymax = supply_fun(equilibrium_tax)),
                  alpha = 0.3, fill = nord_yellow)
  } else {
    base_plot <- ggplot(mapping = aes(x = 0:45))
  }
  
  full_plot <- base_plot +
    geom_segment(aes(x = equilibrium, xend = equilibrium, 
                     y = -Inf, yend = supply_fun(equilibrium)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    geom_segment(aes(x = -Inf, xend = equilibrium,
                     y = supply_fun(equilibrium), yend = supply_fun(equilibrium)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    geom_segment(aes(x = equilibrium_tax, xend = equilibrium_tax, 
                     y = -Inf, yend = supply_tax(equilibrium_tax)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    geom_segment(aes(x = -Inf, xend = equilibrium_tax,
                     y = supply_tax(equilibrium_tax), yend = supply_tax(equilibrium_tax)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    geom_segment(aes(x = -Inf, xend = equilibrium_tax,
                     y = supply_fun(equilibrium_tax), yend = supply_fun(equilibrium_tax)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    stat_function(fun = supply_fun, size = 1.5, color = nord_red) +
    stat_function(fun = supply_tax, size = 1.5, color = nord_orange) +
    stat_function(fun = demand_fun, size = 1.5, color = nord_dk_blue) +
    annotate(geom = "label", x = 38, y = supply_fun(38), label = "S", 
             size = 4, fill = nord_red, color = "white") +
    annotate(geom = "label", x = 38, y = supply_tax(38), label = "S[tax]", 
             size = 4, fill = nord_orange, color = "white", parse = TRUE) +
    annotate(geom = "label", x = 38, y = demand_fun(38), label = "D", 
             size = 4, fill = nord_dk_blue, color = "white") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    coord_cartesian(xlim = c(0, 45), ylim = c(0, 20)) +
    labs(x = "Product (Q)", y = "Price (P)", title = title) +
    theme_econ(13, axis_line = TRUE) +
    theme(panel.grid = element_blank())
  
  if (shaded) {
    full_plot + 
      geom_label(data = surplus_labels, aes(x = x, y = y, label = text, fill = fill), 
                 hjust = "left", size = 4, color = "white") +
      scale_fill_identity()
  } else {
    full_plot
  }
}
