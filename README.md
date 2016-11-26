Mathematica CustomTicks Package
===============================

CustomTicks package for Mathematica from Mark Caprio. 
See for original code:

http://scidraw.nd.edu/

http://library.wolfram.com/infocenter/MathSource/5599/

## Install

Downland the `CustomTicks.m` and import package or run:

`Get["https://raw.githubusercontent.com/mark-caprio/CustomTicks/master/CustomTicks.m"]`


## Versions

 - V1.0. March 11, 2005.  MathSource No. 5599.
 - V1.1. March 18, 2005.  Documentation update.
 - V1.2. September 17, 2005.  Simplified LogTicks syntax.
 - V1.3. November 29, 2005.  Documentation update.
 - V1.4. July 18, 2007.  FixedPointForm, ExtraTicks option.
 - V1.5. August 2, 2007.  Mathematica 6 compatibility update.
 - V1.6. July 10, 2008.  Overhaul of FixedPointForm to string output.  Separate options for LogTicks.  Updates for use of LinTicks and LogTicks as automatic tick functions.
 - V1.61. July 23, 2008. Workaround for unexpected behavior of RealDigits.
 - V1.62. September 30, 2008. Rounding rather than truncation on digits to right of decimal point.
 - V1.63. December 29, 2009. TickTest and TickLabelTest.  TickLengthScale and TickReverse.  LimitTickLabelRange.
 - V1.64. October 27, 2010. Context changed to LevelScheme`*.
 - V1.70. June 3, 2011. Restructured with init.m file.  Added option LogPlot.  Replaced StyleForm with Style (Mathematica 6) in natural log plot.
 - V1.80. September 30, 2011. TickDirection.
 - V1.81. March 24, 2012. Add pass-through of nonnumeric values for FixedPointForm.
 - V1.82. July 15, 2012. Patch to allow negative tick range in range tests, for use with reversed-axis plots.
 - V1.90. October 6, 2014. Pass through of options to FixedPointForm from LinTicks, to allow easier control over sign formatting in ticks.
 - V2.0.0. March 1, 2015. Add support for minor tick labels: option ShowMinorTickLabels, MinorTickLabelStyle.
 - V2.1.0. March 12, 2016. Convert to standalone .m file (as opposed to autogeneration from notebook) and move text comments into .m file. 
