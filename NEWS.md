
# processmapR 0.5.2 

## Features
* Functions `render_map()` and `export_map()` added, so using `DiagrammeR` functions is not longer needed.
* `dotted_chart()` and `lined_chart()` now have an argument `scale_color` to specify the colour scale for the dots.
Defaults to `bupaR::scale_color_discrete_bupaR`. Reverts to `ggplot2::scale_color_discrete` when more than 26 activities are present. 
* `trace_explorer()` now has an argument `scale_fill` to specify the colour scale for the fill.
Defaults to `bupaR::scale_fill_discrete_bupaR`. Reverts to `ggplot2::scale_fill_discrete` when more than 26 activities are present. 
* `dotted_chart()`, `lined_chart()`, and `trace_explorer()` now have a `plotly` argument to specify
that a `plotly` object should be returned, instead of a `ggplot` object.

## Bug Fixes
* Fixed bug when using `process_map()` on a `grouped_log`.
* Added required dependencies `rsvg` and `DiagrammeRsvg` for `export_map` function.

## Deprecations
* `idotted_chart()`, `iplotly_dotted_chart()`, `ilined_chart()`, `iplotly_lined_chart()` have been deprecated.
* Argument `.abbreviate` of `trace_explorer()` has been deprecated in favour of `abbreviate`.
* `plotly_dotted_chart()`, `plotly_lined_chart()`, and `plotly_trace_explorer()` have been deprecated in favour
of `plotly_dotted_chart(..., plotly = TRUE)`, `plotly_lined_chart(..., plotly = TRUE)`,
and `plotly_trace_explorer(..., plotly = TRUE)`, respectively.

## Other
* Added a `NEWS.md` file to track changes to the package.
* Color scales have been moved to `bupaR` (0.5.1).
* Updated documentation of `dotted_chart()`, `lined_chart()`, and `trace_explorer()`.
