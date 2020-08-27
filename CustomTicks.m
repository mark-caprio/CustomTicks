(* ::Package:: *)

(* :Title: CustomTicks *)
(* :Context: CustomTicks` *)
(* :Author: Mark A. Caprio, Department of Physics, University of Notre Dame *)
(* :Summary: Custom tick mark generation for linear, log, and general nonlinear axes. *)
(* :Copyright: Copyright 2020, Mark A. Caprio *)
(* :Package Version: 2.1.1 *)
(* :Mathematica Version: 12.0 *)
(* :History:
  MCAxes package, January 10, 2003.
  MCAxes and then MCTicks packages distributed as part of LevelScheme, 2004.
  V1.0. March 11, 2005.  MathSource No. 5599.
  V1.1. March 18, 2005.  Documentation update.
  V1.2. September 17, 2005.  Simplified LogTicks syntax.
  V1.3. November 29, 2005.  Documentation update.
  V1.4. July 18, 2007.  FixedPointForm, ExtraTicks option.
  V1.5. August 2, 2007.  Mathematica 6 compatibility update.
  V1.6. July 10, 2008.  Overhaul of FixedPointForm to string output.  Separate options for LogTicks.  Updates for use of LinTicks and LogTicks as automatic tick functions.
  V1.61. July 23, 2008. Workaround for unexpected behavior of RealDigits.
  V1.62. September 30, 2008. Rounding rather than truncation on digits to right of decimal point.
  V1.63. December 29, 2009. TickTest and TickLabelTest.  TickLengthScale and TickReverse.  LimitTickLabelRange.
  V1.64. October 27, 2010. Context changed to LevelScheme`*.
  V1.70. June 3, 2011. Restructured with init.m file.  Added option LogPlot.  Replaced StyleForm with Style (Mathematica 6) in natural log plot.
  V1.80. September 30, 2011. TickDirection.
  V1.81. March 24, 2012. Add pass-through of nonnumeric values for FixedPointForm.
  V1.82. July 15, 2012. Patch to allow negative tick range in range tests, for use with reversed-axis plots.
  V1.90. October 6, 2014. Pass through of options to FixedPointForm from LinTicks, to allow easier control over sign formatting in ticks.
  V2.0.0. March 1, 2015. Add support for minor tick labels: option ShowMinorTickLabels, MinorTickLabelStyle.
  V2.1.0. March 12, 2016. Convert to standalone .m file (as opposed to autogeneration from notebook) and move text comments into .m file.
  V2.1.1. August 26, 2020. Documentation update (FrameTicks syntax in examples).
 *)
(* :Notes:

  Reference:

    M. A. Caprio, Comput. Phys. Commun. 171, 107 (2005).

  CustomTicks may be found at:

    http://scidraw.nd.edu  

    https://library.wolfram.com/infocenter/MathSource/5599/

    https://github.com/mark-caprio/CustomTicks

  
  TODO:
   Revise MajorTickX/MinorTickX options to TickX/MinorTickX with suitable fall-through for minor ticks.
     => Implement TickLabelStyle (after pattern of MinorTickLabelStyle) with suitable fall-through for minor ticks.
   Provide option-based syntax for specifying minor tick subdivisions (Karolis Misiunas 140905).
   Consider changing default tick length.
   Add examples for minor tick labels.

  Acknowledgements for bug reports and suggestions: Johannes Grosse, Will Robertson, Robert Collyer, Karolis Misiunas, Sebastian Schott, Qian Li, Gabriel Landi, Nate Bode.

 *)

(****************************************************************
 * begin package
 ****************************************************************)


BeginPackage["CustomTicks`"];


Unprotect[Evaluate[$Context<>"*"]];


(****************************************************************
 * messages
 ****************************************************************)

(* usage messages *)


CustomTicks::usage="Reserved symbol for CustomTicks package.";


LinTicks::usage="LinTicks[x1,x2,spacing,subdivisions] or LinTicks[x1,x2] or LinTicks[majorlist,minorlist] produces linear or custom rescaled tick marks.";
LogTicks::usage="LogTicks[power1,power2] or LogTicks[base,power1,power2] produces logarithmic tick marks.";

ExtraTicks::usage="Option for LinTicks.";
TickPreTransformation::usage="Option for LinTicks.  Mapping from given coordinate value to value used for range tests labeling.";
TickPostTransformation::usage="Option for LinTicks.  Mapping from tick values to actual coordinates used for positioning tick mark, applied after all range tests and labeling.";
TickRange::usage="Option for LinTicks.";
TickTest::usage="Option for LinTicks.";
TickLabelTest::usage="Option for LinTicks.";

ShowTickLabels::usage="Option for LinTicks.";
ShowMinorTickLabels::usage="Option for LinTicks.";
TickLabelRange::usage="Option for LinTicks.";
ShowFirst::usage="Option for LinTicks.";
ShowLast::usage="Option for LinTicks.";
ShowMinorTicks::usage="Option for LinTicks.";
TickLabelStart::usage="Option for LinTicks.";
TickLabelStep::usage="Option for LinTicks.";
TickLabelFunction::usage="Option for LinTicks.";
DecimalDigits::usage="Option for LinTicks.";

MajorTickLength::usage="Option for LinTicks.";
MinorTickLength::usage="Option for LinTicks.";
TickLengthScale::usage="Option for LinTicks.";
TickDirection::usage="Option for LinTicks.";
TickReverse::usage="Legacy option for LinTicks (v1.63).  Use TickDirection (v1.80) instead.";
MajorTickStyle::usage="Option for LinTicks.";
MinorTickStyle::usage="Option for LinTicks.";
MinorTickIndexRange::usage="Option for LinTicks.";
MinorTickIndexTransformation::usage="Option for LinTicks.";

TickLabelStyle::usage="Option for LinTicks.  (NOT IMPLEMENTED)";
MinorTickLabelStyle::usage="Option for LinTicks.";

FixedPointForm::usage="FixedPointForm[x,r] formats x with r digits to the right of the decimal point.  It allows as many digits as necessary to the left of the decimal point, thereby avoiding the rounding problem associated with PaddedForm[x,{n,f}] when n is specified too small (PaddedForm zeros out some of the rightmost digits of the number).  It also suppresses any trailing decimal point when r=0.  FixedPointForm[x,{l,r}] formats x as a fixed-point number with l digits (or spaces) to the left and r to the right of the decimal point.  FixedPointForm respects the standard Mathematica numerical formatting options NumberSigns, NumberPoint, and SignPadding.  By default, for positive numbers a blank padding space appears at left (where a minus sign would be for negative numbers), but with NumberSigns->Automatic this space is suppressed.";
FractionDigits::usage="FractionDigits[x] returns the number of digits to the right of the point in the decimal representation of x.  It will return large values, determined by Precision, for some numbers, e.g., non-terminating rationals.";
FractionDigitsBase::usage="Option for FractionDigits.";

LimitTickRange::usage="LimitTickRange[{x1,x2},ticks] selects those ticks with coordinates in the range x1...x2.  (The range check on the coordinate allows a tolerance, of 10^-10, so that legitimate ticks at the edge of the range are not cut off due to roundoff error.)";
LimitTickLabelRange::usage="LimitTickLabelRange[{x1,x2},ticks] retains the labels on those ticks with coordinates approximately in the range x1...x2, stripping the labels from all others.  (The range check on the coordinate allows a tolerance, of 10^-10, so that legitimate ticks at the edge of the range are not cut off due to roundoff error.)";
StripTickLabels::usage="StripTickLabels[ticks] removes any text labels from ticks.";
TransformTicks::usage="StripTickLabels[positionfcn,lengthfcn,ticks] transforms the positions and lengths of all tick marks in a list.  Tick marks must be specified in full form, or at least with an explicit pair of in and out lengths.";


AugmentTicks::usage="AugmentTicks[labelfunction,lengthlist,stylelist,ticks] augments any ticks in ticklist to full form.";
AugmentAxisTickOptions::usage="AugmentAxisTickOptions[numaxes,list] replaces any None entries with null lists and appends additional null lists as needed to make numaxes entries.  AugmentAxisTickOptions[numaxes,None] returns a list of null lists.  Note that this differs from the behavior of the Mathematica plotting functions with FrameTicks, for which an unspecified upper or right axis duplicates the given lower or left axis.";
TickQ::usage="TickQ[x] returns True if x is a valid tick specification." ;
TickListQ::usage="TickListQ[x] returns True if x is a list of valid tick specifications.  This is not a general test for a valid axis tick option specification, as None, Automatic, and a function can also be valid axis tick option specifications."; 


(****************************************************************
 * begin private context
 ****************************************************************)


Begin["`Private`"];


(****************************************************************
 * arithmetic utility functions
 ****************************************************************)


(* range testing and manipulation utilities, from package MCGraphics *)


InRange[{x1_,x2_},x_]:=(x1<=x)&&(x<=x2);


InRange[{x1_,x2_},x_]:=(x1<=x)&&(x<=x2);
InRangeProper[{x1_,x2_},x_]:=(x1<x)&&(x<x2);


InRegion[{{x1_,x2_},{y1_,y2_}},{x_,y_}]:=InRange[{x1,x2},x]&&InRange[{y1,y2},y];
InRegion[{x1_,y1_},{x2_,y2_},{x_,y_}]:=InRange[{x1,x2},x]&&InRange[{y1,y2},y];


ExtendRange[PRange:{x1_,x2_},PFrac:{fx1_,fx2_}]:=PRange+PFrac*{-1,+1}*-Subtract@@PRange;
ExtendRegion[PRange:{{x1_,x2_},{y1_,y2_}},PFrac:{{fx1_,fx2_},{fy1_,fy2_}}]:=PRange+PFrac*{{-1,+1},{-1,+1}}*-Subtract@@@PRange;


(* approximate equality testing utility, from package MCArithmetic *)
(* patched to accept reversed range limits*)


Options[ApproxEqual]={Chop->1*^-10};
ApproxEqual[x_?NumericQ,y_?NumericQ,Opts___?OptionQ]:=Module[
  {FullOpts=Flatten[{Opts,Options[ApproxEqual]}]},
  Chop[x-y,Chop/.FullOpts]==0
						      ];
ApproxEqual[x_?NumericQ,_DirectedInfinity]:=False;
ApproxEqual[_DirectedInfinity,x_?NumericQ]:=False;
ApproxInRange[{x1p_,x2p_},x_,Opts___?OptionQ]:=With[
  {x1=Min[x1p,x2p],x2=Max[x1p,x2p]},
  ApproxEqual[x,x1,Opts]||((x1<=x)&&(x<=x2))||ApproxEqual[x,x2,Opts]
					       ];
ApproxIntegerQ[x_?NumericQ,Opts___?OptionQ]:=ApproxEqual[Round[x],x,Opts];


(****************************************************************
 * tick label formatting utility functions
 ****************************************************************)


(* digit counting *)

(* FractionDigits -- returns number of digits to right of point in arbitrary-base representation of x, up to specified limit
Not based on RealDigits since too easily fooled by 0.9999... roundoff.
Effective limit in some cases is given implicity by chop value.  Chop value is chosen coarser than usual default 10^-10 to avoid occasionally failing to recognize terminating decimal.

Algorithm:
	Make sure x is floating point.
	Keep shifting x left until it is approximately an integer.
	The number of digits by which x had to be shifted is the number of digits originally to the left of the decimal point. *)


Options[FractionDigits]={FractionDigitsBase->10,Limit->Infinity,Chop->1*^-8};


FractionDigits[x_?NumericQ,Opts___?OptionQ]:=
  Module[
    {
      FullOpts=Flatten[{Opts,Options[FractionDigits]}],
      Value,NumToRight,OptFractionDigitsBase,OptLimit
    },
    Value=N[x];
    OptFractionDigitsBase=FractionDigitsBase/.FullOpts;
    OptLimit=Limit/.FullOpts;
    NumToRight=0;
    While[!ApproxIntegerQ[Value,Chop->(Chop/.FullOpts)]&&(NumToRight<OptLimit),
	  Value*=OptFractionDigitsBase;
	  NumToRight++
    ];
    NumToRight
  ];


(* fixed-point decimal notation *)


FixedPointForm::leftdigits="Insufficient left digits (`1`) specified for formatting of number (`2`).  Consider using automatic version FixedPointForm[x,r].";FixedPointForm::numbersigns="Unsupported form of NumberSigns.";
Options[FixedPointForm]={NumberSigns->{"-",""},NumberPoint->".",SignPadding->False,Debug->False};
(* TODO: NumberForm takes NumberPadding -> {"0", ""} default {"","0"} , but we want to zero pad when SignPadding->True
 or perhaps NumberPadding->True*)
FixedPointForm[x_?NumberQ,{l_Integer?NonNegative,r_Integer?NonNegative},Opts___?OptionQ]:=
  Module[
    {
      FullOpts=Flatten[{Opts,Options[FixedPointForm]}],
      sx,mx,SignString,LeftDigits,RightDigits,SeparatorChars,SignChars,PaddingChars,
      NaturalLeftDigits,
      PartialSigns,FullSigns,
      AutoOpts
    },

    (* option-based setup *)
    (* deduce character lists for -/+ signs, space padded to same length *)
    FullSigns=Switch[
      (NumberSigns/.FullOpts),
      Automatic,{{"-"},{}},
      {_String,_String},
      PartialSigns=Characters/@(NumberSigns/.FullOpts);
      PadLeft[#,Max[Length/@PartialSigns]," "]&/@PartialSigns,
      _,Message[FixedPointForm::numbersigns];{{"-"},{" "}}
	      ];

    (* extract basic information on natural representation of number *)
    sx=Sign[x];
    mx=Abs[N[x]];
    (*LeftDigits=IntegerDigits[IntegerPart[mx]];*)
    (*RightDigits=First[RealDigits[mx,10,r,-1]];*)
    (*RightDigits=First[RealDigits[FractionalPart[mx],10,r,-1]];*)
    (*RightDigits=IntegerDigits[Round[FractionalPart[mx]*10^r],10,r];*) 
    (* round rather than truncate, to avoid, e.g., 2.3999999999 formatting as 2.3 *)
    LeftDigits=IntegerDigits[IntegerPart[Round[mx*10^r]/10^r]];
    RightDigits=IntegerDigits[Round[mx*10^r],10,r];
    If[
      (Debug/.FullOpts),
      Print[{x,l,r},": ",{sx,mx,LeftDigits,RightDigits}]
    ];
    NaturalLeftDigits=Length[LeftDigits];
    If[l<NaturalLeftDigits,
       Message[FixedPointForm::leftdigits,l,x]
    ];

    (* construct character representation *)
    SeparatorChars=If[r==0,{},{(NumberPoint/.FullOpts)}];
    SignChars=Switch[
      sx,
      -1,First[FullSigns],
      +1|0,Last[FullSigns]
	      ];
    PaddingChars=Switch[
      SignPadding/.FullOpts,
      False,PadLeft[SignChars,Max[l+Length[SignChars]-NaturalLeftDigits,Length[SignChars]]," "],
      True,PadRight[SignChars,Max[l+Length[SignChars]-NaturalLeftDigits,Length[SignChars]]," "]
		 ];
    StringJoin@@Join[PaddingChars,ToString/@LeftDigits,SeparatorChars,ToString/@RightDigits]
  ];
FixedPointForm[x_?NumberQ,RightDigits_Integer?NonNegative,Opts___?OptionQ]:=
  FixedPointForm[x,{Length[IntegerDigits[IntegerPart[N[x]]]],RightDigits},Opts];
FixedPointForm[x:Except[_?NumberQ],{l_Integer?NonNegative,r_Integer?NonNegative}|(RightDigits_Integer?NonNegative),Opts___?OptionQ]:=
  x;


(* label formatting function *)

(* Always pad to left by largest abs coord.
Choose decimal digits:
	If Automatic, use max needed for specified major ticks 
	If specified, use value specified
Right padding:
	If decimal digits 0,  truncate to integer
	If not, pad to right by decimal digits specified. 
Allow in all calculations for empty major coordinate list.

Pass through options to FixedPointForm.  For positive ticks, generally want space for sign suppressed in padding (by use of FixedPointForm with NumberSigns->Automatic). *)


FixedTickFunction[ValueList_List,DecimalDigits:(Automatic|(_Integer)),Opts___?OptionQ]:=
  Module[
    {
      LeftDigits,RightDigits,x
    },
    LeftDigits=If[
      ValueList==={},
      0,
      Length[IntegerDigits[IntegerPart[Max[Abs[ValueList]]]]]
	       ];
    RightDigits=If[
      DecimalDigits===Automatic,
      If[
	ValueList==={},
	0,
	Max[FractionDigits/@ValueList]
      ],
      DecimalDigits
		];
    With[
      {ld=LeftDigits,rd=RightDigits},
      Function[x,FixedPointForm[x,{ld,rd},Opts,NumberSigns->Automatic]]
    ]
  ];


(****************************************************************
 * tick length utility
 ****************************************************************)

(* notes on tick lengths:
	Mathematica 4.1 default lengths are 0.00625 and 0.00375
	MCTicks default lengths were 0.00750 and 0.00375
	CustomTicks v1.0 default lengths are 0.010 and 0.005 
 *)


(* Derive tick {in,out} length from given options.

given tick length option may be either a single number or an {in,out} pair
 *)


ResolveTickLength[d_?NumericQ,TickLengthScale_?NumericQ,TickDirection:(In|Out|All),TickReverse:(True|False)]:=
  Module[
    {LengthPair},

    (* upgrade single length to {in,out} pair *)
    LengthPair=Switch[TickDirection,
		      In,{d,0},
		      Out,{0,d},
		      All,{d,d}
	       ];

    (* implement legacy direction reversing option *)
    LengthPair=If[TickReverse,Reverse,Identity]@LengthPair;

    (* scale length *)
    TickLengthScale*LengthPair
  ];


ResolveTickLength[{d1_?NumericQ,d2_?NumericQ},TickLengthScale_?NumericQ,TickDirection:(In|Out|All),TickReverse:(True|False)]:=
  Module[
    {LengthPair},

    (* ignore TickDirection if given {in,out} pair *)
    LengthPair={d1,d2};

    (* implement legacy direction reversing option *)
    LengthPair=If[TickReverse,Reverse,Identity]@LengthPair;

    (* scale length *)
    TickLengthScale*LengthPair
  ];


(****************************************************************
 * Linear ticks
 ****************************************************************)

(* Option MinorTickIndexRange is only respected by the iterating form of LinTicks. *)


Options[LinTicks]={
  (* tick options *)
  ExtraTicks->{},
  TickPreTransformation->Identity,TickPostTransformation->Identity,
  ShowFirst->True,ShowLast->True,ShowTickLabels->True,ShowMinorTickLabels->False,ShowMinorTicks->True,
  TickLabelStart->0,TickLabelStep->1,TickRange->{-Infinity,Infinity},TickLabelRange->{-Infinity,Infinity},TickLabelFunction->Automatic,
  DecimalDigits->Automatic,
  MajorTickLength->0.010,MinorTickLength->0.005,MinorTickLabelStyle->None,TickLengthScale->1,TickDirection->In,TickReverse->False,
  MajorTickStyle->{},MinorTickStyle->{},MinorTickIndexRange->{1,Infinity},MinorTickIndexTransformation->Identity,
  TickTest->(True&),TickLabelTest->(True&),

  (* pass through to FixedPointForm *)
  NumberSigns->Automatic,  (* suppress space before positive numbers *)
  NumberPoint->".",(* SignPadding is pretty irrelevant to ticks, so omit... *)

  (* debugging *)
  Debug->False
};


LinTicks[RawMajorCoordList_List,RawMinorCoordList_List,Opts___]:=
  Module[
    {
      FullOpts= Flatten[{Opts,Options[LinTicks]}],
      MajorCoordList,
      LabeledCoordList,
      MinorCoordList,
      MajorTickList,
      MinorTickList,
      UsedTickLabelFunction,
      DefaultTickLabelFunction,
      TickValue,
      TickPosition,
      TickLabel,
      TickLength,
      TickStyle,
      TickLabelStyleWrapper,
      i
    },

    (* make major ticks *)
    MajorCoordList=Select[
      (TickPreTransformation/.FullOpts)/@Union[RawMajorCoordList,ExtraTicks/.FullOpts],
      (ApproxInRange[(TickRange/.FullOpts),#]&&(TickTest/.FullOpts)[#])&
		   ];
    LabeledCoordList=Flatten[Table[
      TickValue=MajorCoordList[[i]];
      If[
	(ShowTickLabels/.FullOpts)
	&&ApproxInRange[(TickLabelRange/.FullOpts),TickValue]
	&&(Mod[i-1,(TickLabelStep/.FullOpts)]==(TickLabelStart/.FullOpts))
	&&((i!=1)||(ShowFirst/.FullOpts))
	&&((i!=Length[MajorCoordList])||(ShowLast/.FullOpts))
	&&(TickLabelTest/.FullOpts)[TickValue],
	TickValue,
	{}
      ],
      {i,1,Length[MajorCoordList]}
		     ]];
    DefaultTickLabelFunction=FixedTickFunction[
      LabeledCoordList,
      DecimalDigits/.FullOpts,
      FilterRules[FullOpts,Options[FixedPointForm]]
			     ];
    UsedTickLabelFunction=Switch[
      (TickLabelFunction/.FullOpts),
      Automatic,(#2&),
      _,(TickLabelFunction/.FullOpts)
			  ];
    TickLength=ResolveTickLength[
      (MajorTickLength/.FullOpts),(TickLengthScale/.FullOpts),
      (TickDirection/.FullOpts),(TickReverse/.FullOpts)
	       ];
    TickStyle=(MajorTickStyle/.FullOpts);
    MajorTickList=Table[

      (* calculate tick value *)
      TickValue=MajorCoordList[[i]];

      (* calculate coordinate for drawing tick *)
      TickPosition=(TickPostTransformation/.FullOpts)[TickValue];

      (* construct label, or null string if it should be suppressed -- if: tick labels shown,  tick is in TickLabelRange, in designated modular cycle if only a cycle of major ticks are to be labeled, not explicitly suppressed as first or last label; will only then be used if tick is also in TickRange *)
      TickLabel=If[
	(ShowTickLabels/.FullOpts)
	&&ApproxInRange[(TickLabelRange/.FullOpts),TickValue]
	&&(Mod[i-1,(TickLabelStep/.FullOpts)]==(TickLabelStart/.FullOpts))
	&&((i!=1)||(ShowFirst/.FullOpts))
	&&((i!=Length[MajorCoordList])||(ShowLast/.FullOpts)),
	UsedTickLabelFunction[TickValue,DefaultTickLabelFunction[TickValue]],
	""
		];

      (* make tick *)
      {TickPosition,TickLabel,TickLength,TickStyle},

      {i,1,Length[MajorCoordList]}
		  ];

    (* make minor ticks *)
    MinorCoordList=Select[
      (TickPreTransformation/.FullOpts)/@RawMinorCoordList,
      (ApproxInRange[(TickRange/.FullOpts),#]&&(TickTest/.FullOpts)[#])&
		   ];
    TickLength=ResolveTickLength[
      (MinorTickLength/.FullOpts),(TickLengthScale/.FullOpts),
      (TickDirection/.FullOpts),(TickReverse/.FullOpts)
	       ];
    TickStyle=(MinorTickStyle/.FullOpts);
    TickLabelStyleWrapper=With[
      {UsedMinorTickLabelStyle=(MinorTickLabelStyle/.FullOpts)},
      If[
	UsedMinorTickLabelStyle=!=None,
	(Style[#,UsedMinorTickLabelStyle]&),
	Identity
      ]
			  ];
    MinorTickList=If[(ShowMinorTicks/.FullOpts),
		     Table[

		       (* calculate tick value *)
		       TickValue=MinorCoordList[[i]];

		       (* calculate coordinate for drawing tick *)
		       TickPosition=(TickPostTransformation/.FullOpts)[TickValue];

		       (* construct label, or null string if it should be suppressed -- if: tick labels shown, *minor* tick labels shown, tick is in TickLabelRange; will only then be used if tick is also in TickRange *)
		       TickLabel=If[
			 (ShowTickLabels/.FullOpts)&&(ShowMinorTickLabels/.FullOpts)
			 &&ApproxInRange[(TickLabelRange/.FullOpts),TickValue],
			 TickLabelStyleWrapper[UsedTickLabelFunction[TickValue,DefaultTickLabelFunction[TickValue]]],
			 ""
				 ];

		       (* make tick *)
		       {TickPosition,TickLabel,TickLength,TickStyle},

		       {i,1,Length[MinorCoordList]}
		     ],
		     {}
		  ];

    (* combine tick lists*)
    If[
      (Debug/.FullOpts),
      Print[RawMajorCoordList];
      Print[RawMinorCoordList];
      Print[Join[MajorTickList,MinorTickList]]
    ];

    Join[MajorTickList,MinorTickList]

  ];


LinTicks[x1_?NumericQ,x2_?NumericQ,Opts___?OptionQ]:=Module[
  {UsedRange,DummyGraphics,TickList,MajorCoordList,MinorCoordList,x},

  (* extend any round-number range by a tiny amount *)
  (* this seems to make Mathematica 4.1 give a much cleaner, sparser set of ticks *)
  UsedRange=If[
    And@@ApproxIntegerQ/@{x1,x2},
    ExtendRange[{x1,x2},{1*^-5,1*^-5}],
    {x1,x2}
	    ]; 

  (* extract raw tick coordinates from Mathematica *)
  (* Line[{}] primative since Mathematica 6 requires nonnull primative list in Graphics for suitable ticks to be produced -- reported by J. Grosse *)
  DummyGraphics=Show[Graphics[{Line[{}]}],PlotRange->{UsedRange,Automatic},DisplayFunction->Identity];
  TickList=First[Ticks/.AbsoluteOptions[DummyGraphics,Ticks]];
  MajorCoordList=Cases[TickList,{x_,_Real,___}:>x];
  MinorCoordList=Cases[TickList,{x_,"",___}:>x];

  (* generate formatted tick mark specifications *)
  LinTicks[MajorCoordList,MinorCoordList,Opts]
						     ]


LinTicks[x1_?NumericQ,x2_?NumericQ,Spacing_?NumericQ,MinorSubdivs:_Integer,Opts___]:=
  Module[
    {
      FullOpts= Flatten[{Opts,Options[LinTicks]}],
      MaxMajorIndex,
      MajorCoordList,MinorCoordList
    },

    (* preliminary calculations  *)
    MaxMajorIndex=Round[(x2-x1)/Spacing];

    (* construct table of ticks --indexed by MajorIndex=0,1,...,MaxMajorTick and MinorIndex=0,...,MinorSubdivs-1, where MinorIndex=0 gives the major tick, 
except no minor ticks after last major tick *)
    MajorCoordList=Flatten[Table[
      N[x1+MajorIndex*Spacing+MinorIndex*Spacing/MinorSubdivs],
      {MajorIndex,0,MaxMajorIndex},{MinorIndex,0,0}
		   ]];
    MinorCoordList=Flatten[Table[
      If[
	InRange[MinorTickIndexRange/.FullOpts,MinorIndex],
	N[x1+MajorIndex*Spacing+((MinorTickIndexTransformation/.FullOpts)@MinorIndex)*Spacing/MinorSubdivs],
	{}
      ],
      {MajorIndex,0,MaxMajorIndex},{MinorIndex,1,MinorSubdivs-1}
		   ]];

    (* there are usually ticks to be suppressed at the upper end, since the major tick index rounds up to the next major tick (for safety in borderline cases where truncation might fail), and the loop minor tick index iterates for a full series of minor ticks even after the last major tick *)
    MajorCoordList=Select[MajorCoordList,ApproxInRange[{x1,x2},#]&];
    MinorCoordList=Select[MinorCoordList,ApproxInRange[{x1,x2},#]&];

    LinTicks[MajorCoordList,MinorCoordList,Opts]

  ];


(****************************************************************
Log ticks
 ****************************************************************)

(*
Minor tick calculation
	s is number of subdivisions given by user
	s' is number of subdivisions given to LinTicks (cancels out from calculations, but serves as internal iteration max+1 for k)
	k is untransformed index (k = 1,...,s-1)
	i is transformed index
	p is preceding major tick's power
	x is plot coordinate (so b^x is true exponentiated coordinate)
	
	i = Log[b,k+1]*s'
	b^x = b^(p+i/s') = b^(i/s')*b^p = b^(Log[b,k+1]*s'/s')*b^p = (k+1)*b^p
	
	Therefore, any value of (k+1) from 2 to strictly less than b makes sense.
	We thus bound k+1 by \[LeftCeiling]b\[RightCeiling]-1, so this is the bound on s.  We arbitrarily choose s'=s, saving the need for an explicit MinorTickIndexRange.
	
	General numeric values are allowed for the lower and upper exponents, to support arbitrary argments which arise when used as automatic tick function in Ticks or FrameTicks option.
 *)


Options[LogTicks]={
  ExtraTicks->{},TickPreTransformation->Identity,TickPostTransformation->Identity,
  ShowFirst->True,ShowLast->True,ShowTickLabels->True,ShowMinorTicks->True,
  TickLabelStart->0,TickLabelStep->1,TickRange->{-Infinity,Infinity},TickLabelRange->{-Infinity,Infinity},
  DecimalDigits->Automatic,
  MajorTickLength->0.010,MinorTickLength->0.005,TickLengthScale->1,TickDirection->In,TickReverse->False,
  MajorTickStyle->{},MinorTickStyle->{},MinorTickIndexRange->{1,Infinity},LogPlot->False
};


LogTicks::oldsyntax="The number of minor subdivisions no longer needs to be specified for LogTicks (see CustomTicks manual for new syntax).";LogTicks::minorsubdivs="Number of minor subdivisions `1` specified for LogTicks is not 1 or \[LeftCeiling]base\[RightCeiling]-1 (i.e., \[LeftCeiling]base\[RightCeiling]-2 tick marks) and so is being ignored.";
LogTicks[Base:(_?NumericQ):10,p1Raw_?NumericQ,p2Raw_?NumericQ,Opts___?OptionQ]:=
  Module[
    {
      FullOpts= Flatten[{Opts,Options[LogTicks]}],
      p1,p2,BaseSymbol,MinorSubdivs,
      UsedArgumentTransform,UsedPostTransformation
    },

    (* for use with LogPlot: scale input arguments from true values down to log *)
    UsedArgumentTransform=Switch[
      (LogPlot/.FullOpts),
      True,
      Log[Base, #]&,
	 E|False,
      Identity
			  ];
    (* for use with LogPlot: scale coordinate back up from log to true value *)
    UsedPostTransformation =Switch[
      (LogPlot/.FullOpts),
      (* direct use with LogPlot -- will be rescaled by LogPlot *)
      True, Composition[(TickPostTransformation/.FullOpts),(Base^#&)],
      (* use with Graphics results of LogPlot -- done base E *)
      (* more generally, would multiply by Log[plotbase,labelbase] *)
      E,
      Composition[(TickPostTransformation/.FullOpts),(Log[10]*#&)],
      (* standard use on plot in which coordinates are logs base 10 *)
      False,
      (TickPostTransformation/.FullOpts)
			    ];

    p1=Floor[UsedArgumentTransform@p1Raw];
    p2=Ceiling[UsedArgumentTransform@p2Raw];

    BaseSymbol=If[Base===E,Style["e",FontFamily->"Italic"],Base];
    MinorSubdivs=Ceiling[Base]-1; (* one more than minor ticks *)
    MinorSubdivs=Max[MinorSubdivs,1]; (* prevent underflow from bases less than 2 *)
    LinTicks[p1,p2,1,MinorSubdivs,
	     TickPostTransformation->UsedPostTransformation,
	     TickLabelFunction->(DisplayForm[SuperscriptBox[BaseSymbol,IntegerPart[#]]]&),
	     MinorTickIndexTransformation->(Log[Base,#+1]*MinorSubdivs&),
	     FullOpts (* pass through to LinTicks*)
    ]

  ];


(* syntax traps for old syntax -- but will not catch usual situation in which base was unspecified but subdivs was *)
LogTicks[Base_?NumericQ,p1_Integer,p2_Integer,MinorSubdivs_Integer,Opts___?OptionQ]/;(MinorSubdivs==Max[Ceiling[Base]-1,1]):=
  (Message[LogTicks::oldsyntax];LogTicks[Base,p1,p2,ShowMinorTicks->True,Opts]);
LogTicks[Base_?NumericQ,p1_Integer,p2_Integer,MinorSubdivs_Integer,Opts___?OptionQ]/;(MinorSubdivs==1):=
  (Message[LogTicks::oldsyntax];LogTicks[Base,p1,p2,ShowMinorTicks->False,Opts]);
LogTicks[Base_?NumericQ,p1_Integer,p2_Integer,MinorSubdivs_Integer,Opts___?OptionQ]/;((MinorSubdivs!=Max[Ceiling[Base]-1,1])&&(MinorSubdivs!=1)):=
  (Message[LogTicks::oldsyntax];Message[LogTicks::minorsubdivs,MinorSubdivs];LogTicks[Base,p1,p2,ShowMinorTicks->True,Opts]);


(****************************************************************
augmentation of tick specifications
 *)


(* Plain coordinate is upgraded to coordinate in list *)


AugmentTick[LabelFunction_,DefaultLength_List,DefaultStyle_List,x_?NumericQ]:=
  AugmentTick[LabelFunction,DefaultLength,DefaultStyle,{x}];


(* Wrapped coordinate is given label *)


AugmentTick[LabelFunction_,DefaultLength_List,DefaultStyle_List,{x_?NumericQ}]:=
  AugmentTick[LabelFunction,DefaultLength,DefaultStyle,{x,LabelFunction@x}];


(* Coordinate + label is given length list *)


AugmentTick[LabelFunction_,DefaultLength_List,DefaultStyle_List,{x_?NumericQ,LabelText_}]:=
  AugmentTick[LabelFunction,DefaultLength,DefaultStyle,{x,LabelText,DefaultLength}];


(* Any tick with just plength is upgraded to having length list *)


AugmentTick[LabelFunction_,DefaultLength_List,DefaultStyle_List,{x_?NumericQ,LabelText_,PLen_?NumericQ,RestSeq___}]:=
  AugmentTick[LabelFunction,DefaultLength,DefaultStyle,{x,LabelText,{PLen,0},RestSeq}];


(* Coordinate + label + length list is given style *)


AugmentTick[LabelFunction_,DefaultLength_List,DefaultStyle_List,{x_?NumericQ,LabelText_,LengthList_List}]:=
  AugmentTick[LabelFunction,DefaultLength,DefaultStyle,{x,LabelText,LengthList,DefaultStyle}];


(* Release a complete tick *)


AugmentTick[LabelFunction_,DefaultLength_List,DefaultStyle_List,TheTick:{x_?NumericQ,LabelText_,LengthList_List,Style_List}]:=
  TheTick;


(* Process full list *)


AugmentTicks[DefaultLength_List,DefaultStyle_List,TickList_List]:=
  AugmentTick[""&,DefaultLength,DefaultStyle,#]&/@TickList;


(****************************************************************
Augment tick axis lists
 ****************************************************************)


(*
Converts plain None to list of empty lists
Augments list of specifications to NumAxes with empty lists, replacing None with empty list
Does not handle plain Automatic
 *)


AugmentAxisTickOptions::numaxes="Tick lists specified for more than `1` axes.";
AugmentAxisTickOptions[NumAxes_Integer,TickLists:None]:=
  Table[{},{NumAxes}];
AugmentAxisTickOptions[NumAxes_Integer,TickLists_List]:=
  Module[
    {},
    If[
      NumAxes<Length[TickLists],
      Message[AugmentAxisTickOptions::numaxes,NumAxes]
    ];
    Join[
      Replace[TickLists,{None->{}},{1}],
      Table[{},{NumAxes-Length[TickLists]}]
  ]];


(****************************************************************
Tick testing
 ****************************************************************)


(*
Valid tick is coordinate or {coordinate,label,length/pair,style}, with all contents after first optional, where coordinate and length or the components of pair must be numeric and style must be a list.
Ambiguity arises in that, e.g., {1,2,3} could either be a single tick mark or a list of very simple tick marks.  Thus, it produces True as an argument to either TickQ or TickListQ.
 *)

(*
Test code:
>>> TickQ/@{1,{1},{1,"a"},{1,"a",2},{1,"a",{2,3}},{1,"a",{2,3},{Red}}}
>>> TickQ/@{x,{1,"a",{2,"a"}},{1,"a",{2,3},Red}}

{True,True,True,True,True,True}

>>> TickListQ/@{{},LinTicks[1,5,1,2]}
>>> TickListQ/@{None}

{True,True}
{False}
 *)


TickPattern=(
  (_?NumericQ)
|{_?NumericQ}|{_?NumericQ,_}|{_?NumericQ,_,(_?NumericQ)|{_?NumericQ,_?NumericQ}}|{_?NumericQ,_,(_?NumericQ)|{_?NumericQ,_?NumericQ},_List}
 );
 

 TickQ[x_]:=MatchQ[x,TickPattern];
 TickListQ[x_]:=MatchQ[x,{}|{TickPattern..}];


 (****************************************************************
  * Tick list manipulation
  ****************************************************************)


 LimitTickRange[Range:{x1_,x2_},TickList_List]:=Cases[TickList,((x_?NumericQ)|{x_,___})/;ApproxInRange[{x1,x2},x]];


 LimitTickLabelRange[Range:{x1_,x2_},TickList_List]:=
  Union[
    Cases[TickList,x_?NumericQ],  (* bare number *)
    Cases[TickList,{x_}],  (* no label given *)
    Cases[TickList,{x_,l_,r___}/;ApproxInRange[{x1,x2},x]], (* label -- retain *)
    Cases[TickList,{x_,l_,r___}/;!ApproxInRange[{x1,x2},x]:>{x,"",r}
    ]  (* label -- strip *)
  ];


 StripTickLabels[TickList_List]:=
  Replace[TickList,{x_,l_,r___}:>{x,"",r},{1}];
 StripTickLabels[f_Symbol]:=
  Composition[StripTickLabels,f];


 TransformTicks[PosnTransformation_,LengthTransformation_,TickList_List]:=
  Replace[TickList,{x_,t_,l:{_,_},RestSeq___}:>{PosnTransformation@x,t,LengthTransformation/@l,RestSeq},{1}];


 (****************************************************************
  * End private context
  ****************************************************************)


 End[];


 (****************************************************************
  * End package
  ****************************************************************)


 Protect[Evaluate[$Context<>"*"]];
 EndPackage[];
