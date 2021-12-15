# SudokuHelper

SudokuHelper is an application that acts like an electronic Sudoku grid.
It supports 9x9, 12x12, and 16x16 Sudokus, both in the classic and Gosu variant,
where cells can be marked to only accept even numbers.
The application neither creates Sudokus itself nor provides a solver for them;
it is just a more convenient way to solve a Sudoku from a magazine or other external source than doing it on paper,
using pencil and eraser.

The source code demonstrates how to design a completely uncoupled code base using interfaces in Embarcadero Delphi.

---

About forked repository:

## SH01

Original source code files have been moved into subdirectories of folder SH01 and renamed!

The reason for this big unit name refactoring is that I wanted to add a second project in folder SH02,
which uses FMX framework to build the UI, instead of VCL.

## SH02

Folder SH02 holds the experimental FMX application, which is work in progress.

The motivation for starting an FMX alternative was to demonstrate Federgraph's *button frame component*.
And the plan is to replace the UI wholesale, while keeping the *business logic* almost unchanged.

It should be easy to compare folders SH01 and SH02 to see what has changed.
Obviously there needed to be some changes to the base classes and interfaces,
in order to remove the dependency on the VCL framework.

How to explore:

1. Find out which files are in both directories, SH01 and SH02.
1. Notice the changes in those files.
1. Test-Run on Win32 platform (only).
1. Find out about the button frame component.
1. Find out how I have replaced the TDrawGrid.
1. See how two application building flavors can coexist.
1. Notice how SH02 adapts to different form factors!
1. Notice that the button frame solution is platform independent already.
1. Think briefly about how and where data should be stored in the future.

Use SH01 ( or original SudokuHelper application) if you want the finished application!