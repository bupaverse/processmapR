
# processmapR 0.5.2 (dev)

## Features
* Functions `render_map()` and `export_map()` added, so using `DiagrammeR` functions is not longer needed.
* `dotted_chart()` now has an argument `scale_color` to specify the colour scale for the dots.
Defaults to `bupaR::scale_color_discrete_bupaR`.
* `trace_explorer()` now has an argument `scale_fill` to specify the colour scale for the fill.
Defaults to `bupaR::scale_fill_discrete_bupaR`.

## Bug Fixes
* Fixed bug when using `process_map()` on a `grouped_log`.
* Added required dependencies `rsvg` and `DiagrammeRsvg` for `export_map` function. 

## Deprecations
* Argument `.abbreviate` of `trace_explorer()` has been deprecated in favour of `abbreviate`.

## Other
* Added a `NEWS.md` file to track changes to the package.
* Color scales have been moved to `bupaR` (0.5.1).
* Updated documentation of `dotted_chart()` and `trace_explorer()`.
