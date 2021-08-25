# sch2tikz
schematic to tikzpicture converter for Cadence Virtuoso

## Description

Cadence SKILL function, which can be used to create
a *tikzpicture* of a *schematic*.

## Installation

The function can be used by loading the SKILL
file *sch2tikz.il* in Cadence Virtuoso:

``` scheme
(load "sch2tikz.il")
```
A more convenient way might be to add this load command in the `.cdsinit`.

## Usage

``` scheme
EDsch2tikz( 
  cv
  mag
  file
)=>path/nil
```
### Arguments

`cv`

Schematic to be be exported. 
Provide `(geGetWindowCellView)` to reference schematic in foreground.

`mag`

Magnification. Float in (0,inf).

`file`

Path to file where *tikzpicture* is created.

## Limitations

- Schematic is monochrome
- Labels are not exported

## Dependencies

Needed LaTeX usepackages:

+ [tikz](https://www.ctan.org/pkg/pgf)