DiagrammeR::render_graph(define_graph(
  define_comp_layout(qmd_info = examples$threecomp, scaling = TRUE, color_scaling = 'dqmd', comp_scale_fun = function(x) { x/20 })
  ))

qmd(qmd_info = examples$threecomp, scaling = TRUE, color_scaling = 'dqmd', comp_scale_fun = function(x) { x/20 }, unscaled_color = 'grey30')

