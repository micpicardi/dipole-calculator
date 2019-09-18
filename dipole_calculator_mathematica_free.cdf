(* Content-type: application/vnd.wolfram.cdf.text *)

(*** Wolfram CDF File ***)
(* http://www.wolfram.com/cdf *)

(* CreatedBy='Mathematica 11.0' *)

(*************************************************************************)
(*                                                                       *)
(*  The Mathematica License under which this file was created prohibits  *)
(*  restricting third parties in receipt of this file from republishing  *)
(*  or redistributing it by any means, including but not limited to      *)
(*  rights management or terms of use, without the express consent of    *)
(*  Wolfram Research, Inc. For additional information concerning CDF     *)
(*  licensing and redistribution see:                                    *)
(*                                                                       *)
(*        www.wolfram.com/cdf/adopting-cdf/licensing-options.html        *)
(*                                                                       *)
(*************************************************************************)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[      1064,         20]
NotebookDataLength[    173867,       4118]
NotebookOptionsPosition[    173412,       4082]
NotebookOutlinePosition[    173899,       4102]
CellTagsIndexPosition[    173856,       4099]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{

Cell[CellGroupData[{
Cell[BoxData[{
 RowBox[{"Clear", "[", 
  RowBox[{"Evaluate", "[", 
   RowBox[{
    RowBox[{"Context", "[", "]"}], "<>", "\"\<*\>\""}], "]"}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Kpm", " ", ":=", " ", 
   RowBox[{"{", 
    RowBox[{"kx", ",", " ", "ky", ",", " ", 
     RowBox[{"\[PlusMinus]", "kz1"}]}], "}"}]}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Kp", " ", ":=", " ", 
   RowBox[{"{", 
    RowBox[{"kx", ",", " ", "ky", ",", " ", "kz1"}], "}"}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Km", " ", ":=", " ", 
   RowBox[{"{", 
    RowBox[{"kx", ",", " ", "ky", ",", " ", 
     RowBox[{"-", "kz1"}]}], "}"}]}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Z", " ", ":=", " ", 
   RowBox[{"{", 
    RowBox[{"0", ",", "0", ",", "1"}], "}"}]}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"p", " ", ":=", " ", 
   RowBox[{"{", 
    RowBox[{"px", ",", "py", ",", "pz"}], "}"}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"m", ":=", 
    RowBox[{"{", 
     RowBox[{"mx", ",", "my", ",", "mz"}], "}"}]}], ";"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{"Assumptions", ":", " ", 
     RowBox[{"kx", " ", "and", " ", "ky", " ", "are", " ", "real"}]}], ",", 
    " ", 
    RowBox[{
    "while", " ", "kt", " ", "and", " ", "k", " ", "are", " ", "real", " ", 
     "and", " ", "greater", " ", "than", " ", "zero"}]}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"$Assumptions", " ", "=", " ", 
    RowBox[{
     RowBox[{"Element", "[", 
      RowBox[{
       RowBox[{
       "kx", "|", "ky", " ", "|", " ", "x", " ", "|", " ", "y", " ", "|", " ",
         "z", " ", "|", " ", "\[Theta]"}], ",", "Reals"}], "]"}], " ", "&&", 
     " ", 
     RowBox[{"c", ">", "0"}], " ", "&&", 
     RowBox[{"kt", ">", "0"}], " ", "&&", " ", 
     RowBox[{"k1", ">", "0"}], " ", "&&", " ", 
     RowBox[{"k2", ">", "0"}], " ", "&&", 
     RowBox[{"\[Rho]", ">", "0"}], " ", "&&", " ", 
     RowBox[{"\[Epsilon]", ">", "0"}], " ", "&&", " ", 
     RowBox[{"dkx", ">", "0"}], " ", "&&", " ", 
     RowBox[{"dky", ">", "0"}], " ", "&&", " ", 
     RowBox[{"dkt", ">", "0"}], " ", "&&", " ", 
     RowBox[{"d\[Alpha]", ">", "0"}], " ", "&&", " ", 
     RowBox[{"\[Mu]2", ">", "0"}], " ", "&&", " ", 
     RowBox[{"\[Mu]1", ">", "0"}]}]}], ";"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{"'", 
    RowBox[{"Unit", "'"}], " ", "Vectors"}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"es", " ", "=", " ", 
   RowBox[{
    RowBox[{"Cross", "[", 
     RowBox[{"Z", ",", "Kp"}], "]"}], "/", 
    RowBox[{"(", "kt", ")"}]}]}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"epp", "=", " ", 
   RowBox[{
    RowBox[{"Cross", "[", 
     RowBox[{
      RowBox[{"Cross", "[", 
       RowBox[{"Z", ",", "Kp"}], "]"}], ",", "Kp"}], "]"}], "/", 
    RowBox[{"(", 
     RowBox[{"k1", " ", "kt"}], ")"}]}]}], " ", ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"epm", "=", " ", 
   RowBox[{
    RowBox[{"Cross", "[", 
     RowBox[{
      RowBox[{"Cross", "[", 
       RowBox[{"Z", ",", "Km"}], "]"}], ",", "Km"}], "]"}], "/", 
    RowBox[{"(", 
     RowBox[{"k1", " ", "kt"}], ")"}]}]}], " ", ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"EpEpp", "=", 
   RowBox[{
    FractionBox["\[ImaginaryI]", 
     RowBox[{"2", "\[Pi]"}]], 
    FractionBox["1", 
     RowBox[{"4", "\[Pi]", " "}]], 
    FractionBox[
     RowBox[{"k1", "^", "2"}], 
     RowBox[{"kz1", "  "}]], 
    RowBox[{"(", 
     RowBox[{
      RowBox[{"Dot", "[", 
       RowBox[{"epp", ",", " ", "p"}], "]"}], " ", "+", 
      RowBox[{
       FractionBox["1", "c"], 
       RowBox[{"(", 
        RowBox[{"Dot", "[", 
         RowBox[{"es", ",", "m"}], "]"}], ")"}]}]}], ")"}]}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"EpEpm", "=", 
   RowBox[{
    FractionBox["\[ImaginaryI]", 
     RowBox[{"2", "\[Pi]"}]], 
    FractionBox["1", 
     RowBox[{"4", "\[Pi]", " "}]], 
    FractionBox[
     RowBox[{"k1", "^", "2"}], 
     RowBox[{"kz1", "  "}]], 
    RowBox[{"(", 
     RowBox[{
      RowBox[{"Dot", "[", 
       RowBox[{"epm", ",", " ", "p"}], "]"}], " ", "+", 
      RowBox[{
       FractionBox["1", "c"], 
       RowBox[{"(", 
        RowBox[{"Dot", "[", 
         RowBox[{"es", ",", "m"}], "]"}], ")"}]}]}], ")"}]}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"EsEpp", "=", 
   RowBox[{
    FractionBox["\[ImaginaryI]", 
     RowBox[{"2", "\[Pi]"}]], 
    FractionBox["1", 
     RowBox[{"4", "\[Pi]", " "}]], 
    FractionBox[
     RowBox[{"k1", "^", "2"}], 
     RowBox[{"kz1", "  "}]], 
    RowBox[{"(", 
     RowBox[{
      RowBox[{"Dot", "[", 
       RowBox[{"es", ",", "p"}], "]"}], "-", 
      RowBox[{
       FractionBox["1", "c"], 
       RowBox[{"(", 
        RowBox[{"Dot", "[", 
         RowBox[{"epp", ",", "m"}], "]"}], ")"}]}]}], ")"}]}]}], ";"}], "\n", 
 RowBox[{
  RowBox[{"EsEpm", "=", 
   RowBox[{
    FractionBox["\[ImaginaryI]", 
     RowBox[{"2", "\[Pi]"}]], 
    FractionBox["1", 
     RowBox[{"4", "\[Pi]", " "}]], 
    FractionBox[
     RowBox[{"k1", "^", "2"}], 
     RowBox[{"kz1", "  "}]], 
    RowBox[{"(", 
     RowBox[{
      RowBox[{"Dot", "[", 
       RowBox[{"es", ",", "p"}], "]"}], "-", 
      RowBox[{
       FractionBox["1", "c"], 
       RowBox[{"(", 
        RowBox[{"Dot", "[", 
         RowBox[{"epm", ",", "m"}], "]"}], ")"}]}]}], ")"}]}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"kz1", " ", "=", " ", 
   SqrtBox[
    RowBox[{
     SuperscriptBox["k1", "2"], "-", 
     SuperscriptBox["kt", "2"]}]]}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"kt", " ", "=", " ", 
   SqrtBox[
    RowBox[{
     SuperscriptBox["kx", "2"], "+", 
     SuperscriptBox["ky", "2"]}]]}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"k1", " ", "=", " ", "1"}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"pol", "=", 
   RowBox[{"{", 
    RowBox[{
     RowBox[{"\"\<spol\>\"", "->", "\"\<s\>\""}], ",", 
     RowBox[{"\"\<ppol\>\"", "->", "\"\<p\>\""}]}], "}"}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"plane", "=", 
   RowBox[{"{", 
    RowBox[{
     RowBox[{"\"\<zplus\>\"", "->", "\"\<z>0\>\""}], ",", 
     RowBox[{"\"\<zmin\>\"", "->", "\"\<z<0\>\""}]}], "}"}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"Which", "[", 
    RowBox[{
     RowBox[{
      RowBox[{"Length", "[", "nulsol", "]"}], "\[Equal]", "0"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"coef", "=", "\"\< \>\""}], ";", "\[IndentingNewLine]", 
      RowBox[{"plus", " ", "=", " ", "\"\< \>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"with", " ", "=", " ", "\"\< \>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"compl", " ", "=", " ", "\"\< \>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"ToPrint", "=", "\"\< \>\""}]}], ",", "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"Length", "[", "nulsol", "]"}], "==", "1"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"coef", "=", 
       RowBox[{"{", "\[Alpha]", "}"}]}], ";", "\[IndentingNewLine]", 
      RowBox[{"plus", " ", "=", " ", "\"\<+\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"with", " ", "=", " ", "\"\<With\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{
      "compl", " ", "=", " ", "\"\<\[Element]\[DoubleStruckCapitalC]\>\""}], 
      ";", "\[IndentingNewLine]", 
      RowBox[{"ToPrint", " ", "=", " ", 
       RowBox[{
        RowBox[{"MatrixForm", "[", 
         RowBox[{"coef", "[", 
          RowBox[{"[", "1", "]"}], "]"}], "]"}], 
        RowBox[{"MatrixForm", "[", 
         RowBox[{"nulsol", "[", 
          RowBox[{"[", "1", "]"}], "]"}], "]"}]}]}]}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"Length", "[", "nulsol", "]"}], "==", "2"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"coef", "=", 
       RowBox[{"{", 
        RowBox[{"\[Alpha]", ",", "\[Beta]"}], "}"}]}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"plus", " ", "=", " ", "\"\<+\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"with", " ", "=", " ", "\"\<With\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{
      "compl", " ", "=", " ", "\"\<\[Element]\[DoubleStruckCapitalC]\>\""}], 
      ";", "\[IndentingNewLine]", 
      RowBox[{"ToPrint", " ", "=", " ", 
       RowBox[{
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "2", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}]}]}]}], ",", "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"Length", "[", "nulsol", "]"}], "==", "3"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"coef", "=", 
       RowBox[{"{", 
        RowBox[{"\[Alpha]", ",", "\[Beta]", ",", "\[Gamma]"}], "}"}]}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"plus", " ", "=", " ", "\"\<+\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"with", " ", "=", " ", "\"\<With\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{
      "compl", " ", "=", " ", "\"\<\[Element]\[DoubleStruckCapitalC]\>\""}], 
      ";", "\[IndentingNewLine]", 
      RowBox[{"ToPrint", " ", "=", " ", 
       RowBox[{
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "2", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "3", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "3", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}]}]}]}], ",", "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"Length", "[", "nulsol", "]"}], "==", "4"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"coef", "=", 
       RowBox[{"{", 
        RowBox[{
        "\[Alpha]", ",", "\[Beta]", ",", "\[Gamma]", ",", "\[Delta]"}], 
        "}"}]}], ";", "\[IndentingNewLine]", 
      RowBox[{"plus", " ", "=", " ", "\"\<+\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"with", " ", "=", " ", "\"\<With\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{
      "compl", " ", "=", " ", "\"\<\[Element]\[DoubleStruckCapitalC]\>\""}], 
      ";", "\[IndentingNewLine]", 
      RowBox[{"ToPrint", " ", "=", " ", 
       RowBox[{
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "2", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "3", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "3", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "4", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "4", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}]}]}]}], ",", "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"Length", "[", "nulsol", "]"}], "==", "5"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"coef", "=", 
       RowBox[{"{", 
        RowBox[{
        "\[Alpha]", ",", "\[Beta]", ",", "\[Gamma]", ",", "\[Delta]", ",", 
         "\[Epsilon]"}], "}"}]}], ";", "\[IndentingNewLine]", 
      RowBox[{"plus", " ", "=", " ", "\"\<+\>\""}], ";", " ", 
      "\[IndentingNewLine]", 
      RowBox[{"with", " ", "=", " ", "\"\<With\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{
      "compl", " ", "=", " ", "\"\<\[Element]\[DoubleStruckCapitalC]\>\""}], 
      ";", "\[IndentingNewLine]", 
      RowBox[{"ToPrint", " ", "=", " ", 
       RowBox[{
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "2", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "3", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "3", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "4", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "4", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "5", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"NumberForm", "[", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"nulsol", "[", 
              RowBox[{"[", "5", "]"}], "]"}], "]"}], ",", "2"}], "]"}], 
          "]"}]}]}]}]}]}], "]"}], ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Dynamic", "[", 
   RowBox[{"point1", "=", "False"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"point2", "=", "True"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"point3", "=", "True"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"point4", "=", "True"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"point5", "=", "True"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"point6", "=", "True"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"A1", "=", "0"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"imA1", "=", "0"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"reA1", "=", "0"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"imA2", "=", "0"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"reA2", "=", "0"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"imA3", "=", "0"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"reA3", "=", "0"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"imA4", "=", "0"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"reA4", "=", "0"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"imA5", "=", "0"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"reA5", "=", "0"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"imA6", "=", "0"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"reA6", "=", "0"}], "]"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Dynamic", "[", 
   RowBox[{"If", "[", 
    RowBox[{
     RowBox[{
      RowBox[{"imA1", "\[NotEqual]", "0"}], "||", 
      RowBox[{"reA1", "\[NotEqual]", "0"}]}], ",", 
     RowBox[{"Dynamic", "[", 
      RowBox[{"point1", "=", "False"}], "]"}]}], "]"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"If", "[", 
    RowBox[{
     RowBox[{
      RowBox[{"imA2", "\[NotEqual]", "0"}], "||", 
      RowBox[{"reA2", "\[NotEqual]", "0"}]}], ",", 
     RowBox[{"Dynamic", "[", 
      RowBox[{"point2", "=", "False"}], "]"}]}], "]"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"If", "[", 
    RowBox[{
     RowBox[{
      RowBox[{"imA3", "\[NotEqual]", "0"}], "||", 
      RowBox[{"reA3", "\[NotEqual]", "0"}]}], ",", 
     RowBox[{"Dynamic", "[", 
      RowBox[{"point3", "=", "False"}], "]"}]}], "]"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"If", "[", 
    RowBox[{
     RowBox[{
      RowBox[{"imA4", "\[NotEqual]", "0"}], "||", 
      RowBox[{"reA4", "\[NotEqual]", "0"}]}], ",", 
     RowBox[{"Dynamic", "[", 
      RowBox[{"point4", "=", "False"}], "]"}]}], "]"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"If", "[", 
    RowBox[{
     RowBox[{
      RowBox[{"imA5", "\[NotEqual]", "0"}], "||", 
      RowBox[{"reA5", "\[NotEqual]", "0"}]}], ",", 
     RowBox[{"Dynamic", "[", 
      RowBox[{"point5", "=", "False"}], "]"}]}], "]"}], "]"}], 
  RowBox[{"Dynamic", "[", 
   RowBox[{"If", "[", 
    RowBox[{
     RowBox[{
      RowBox[{"imA6", "\[NotEqual]", "0"}], "||", 
      RowBox[{"reA6", "\[NotEqual]", "0"}]}], ",", 
     RowBox[{"Dynamic", "[", 
      RowBox[{"point6", "=", "False"}], "]"}]}], "]"}], 
   "]"}]}], "\[IndentingNewLine]", 
 RowBox[{"Style", "[", 
  RowBox[{
  "\"\<Engineering far and near-field directionality: full amplitude and \
phase control of guided mode excitation from a single dipole source\>\"", ",", 
   RowBox[{"FontSize", "\[Rule]", "24"}], ",", 
   RowBox[{"FontWeight", "\[Rule]", "Bold"}], ",", 
   RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", "Black"}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{"Framed", "[", "\[IndentingNewLine]", "\[IndentingNewLine]", 
  RowBox[{
   RowBox[{"Framed", "[", 
    RowBox[{
     RowBox[{"Style", "[", 
      RowBox[{
      "\"\<Insert the normalised wavevectors for each of the points you want \
to fix in the electric field spectra. For each of the points, specify the \
polarisation and half-space, together with the complex amplitude of the \
spectrum.\\nThe code returns the values for the 6 dipole moments components: \
\!\(\*SubscriptBox[\(p\), \(x\)]\), \!\(\*SubscriptBox[\(p\), \(y\)]\), \
\!\(\*SubscriptBox[\(p\), \(z\)]\), \!\(\*SubscriptBox[\(m\), \(x\)]\), \
\!\(\*SubscriptBox[\(m\), \(y\)]\), \!\(\*SubscriptBox[\(m\), \(z\)]\) as a \
particular solution + the associated homogeneous system's solutions, each \
multiplied by an arbitrary complex coefficient.\\nThe code is solving a \
linear system of equations of the type: E(\!\(\*SubscriptBox[\(k\), \(x\)]\),\
\!\(\*SubscriptBox[\(k\), \(y\)]\))=\!\(\*FractionBox[\(A + \[ImaginaryI]B\), \
\(\*SubscriptBox[\(k\), \(0\)] \*SubscriptBox[\(k\), \(z\)]\)]\), with A and \
B specified by the user. For the singular point \!\(\*SubscriptBox[\(k\), \(x\
\)]\)=\!\(\*SubscriptBox[\(k\), \(y\)]\)=0, where the polarisations are not \
well-defined, \!\(\*SubscriptBox[OverscriptBox[\(e\), \(^\)], \(s\)]\) and \!\
\(\*SubscriptBox[OverscriptBox[\(e\), \(^\)], \(p\)]\) are taken as \
\!\(\*OverscriptBox[\(x\), \(^\)]\) and \!\(\*OverscriptBox[\(y\), \(^\)]\), \
respectively.\>\"", ",", 
       RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", "Blue", 
       ",", 
       RowBox[{"FontSize", "\[Rule]", "14"}], ",", 
       RowBox[{"LineSpacing", "\[Rule]", 
        RowBox[{"{", 
         RowBox[{"1.5", ",", " ", "0"}], "}"}]}]}], "]"}], ",", 
     RowBox[{"Background", "\[Rule]", "LightGray"}]}], "]"}], 
   "\[IndentingNewLine]", 
   RowBox[{"Grid", "[", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"\"\< \>\"", ",", "\"\< \>\"", ",", " ", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<Normalised \!\(\*SubscriptBox[\(k\), \(x\)]\) \
(\!\(\*SubscriptBox[\(k\), \(x\)]\)/\!\(\*SubscriptBox[\(k\), \(0\)]\))\>\"", 
           ",", 
           RowBox[{"FontWeight", "\[Rule]", "Bold"}], ",", "Larger", ",", 
           RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], 
         ",", " ", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<Normalised \!\(\*SubscriptBox[\(k\), \
\(y\)]\)(\!\(\*SubscriptBox[\(k\), \(y\)]\)/\!\(\*SubscriptBox[\(k\), \
\(0\)]\))\>\"", ",", 
           RowBox[{"FontWeight", "\[Rule]", "Bold"}], ",", "Larger", ",", 
           RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], 
         ",", " ", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<Select polarisation\>\"", ",", 
           RowBox[{"FontWeight", "\[Rule]", "Bold"}], ",", "Larger", ",", 
           RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], 
         ",", " ", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<Select half-space\>\"", ",", 
           RowBox[{"FontWeight", "\[Rule]", "Bold"}], ",", "Larger", ",", 
           RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], 
         ",", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<Complex amplitude (A+iB)\>\"", ",", 
           RowBox[{"FontWeight", "\[Rule]", "Bold"}], ",", "Larger", ",", 
           RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], 
         ",", "SpanFromLeft", ",", "SpanFromLeft"}], "}"}], ",", 
       "\[IndentingNewLine]", 
       RowBox[{"{", 
        RowBox[{
         RowBox[{"Checkbox", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "point1", "]"}], ",", 
           RowBox[{"{", 
            RowBox[{"True", ",", "False"}], "}"}]}], "]"}], ",", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<1st point\>\"", ",", 
           RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], 
         ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "kx1", "]"}], ",", "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", " ", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<kx1/k0\>\""}]}], "]"}], ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "ky1", "]"}], ",", "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<ky1/k0\>\""}]}], "]"}], ",", 
         RowBox[{"RadioButtonBar", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"pol1", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "pol"}], "]"}], ",", 
         RowBox[{"RadioButtonBar", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"plane1", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "plane"}], "]"}], ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"reA1", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<Re[A1]\>\""}]}], "]"}], ",", 
         "\"\<+\[ImaginaryI]\>\"", ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"imA1", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<Im[A1]\>\""}]}], "]"}]}], 
        "}"}], ",", "\[IndentingNewLine]", 
       RowBox[{"{", 
        RowBox[{
         RowBox[{"Checkbox", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "point2", "]"}], ",", 
           RowBox[{"{", 
            RowBox[{"True", ",", "False"}], "}"}]}], "]"}], ",", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<2nd point\>\"", ",", 
           RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], 
         ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "kx2", "]"}], ",", "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<kx2/k0\>\""}]}], "]"}], ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "ky2", "]"}], ",", "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<ky2/k0\>\""}]}], "]"}], ",", 
         RowBox[{"RadioButtonBar", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"pol2", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "pol"}], "]"}], ",", 
         RowBox[{"RadioButtonBar", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"plane2", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "plane"}], "]"}], ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"reA2", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<Re[A2]\>\""}]}], "]"}], ",", 
         "\"\<+\[ImaginaryI]\>\"", ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"imA2", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<Im[A2]\>\""}]}], "]"}]}], 
        "}"}], ",", "\[IndentingNewLine]", 
       RowBox[{"{", 
        RowBox[{
         RowBox[{"Checkbox", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "point3", "]"}], ",", 
           RowBox[{"{", 
            RowBox[{"True", ",", "False"}], "}"}]}], "]"}], ",", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<3rd point\>\"", ",", 
           RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], 
         ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "kx3", "]"}], ",", "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<kx3/k0\>\""}]}], "]"}], ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "ky3", "]"}], ",", "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<ky3/k0\>\""}]}], "]"}], ",", 
         RowBox[{"RadioButtonBar", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "pol3", "]"}], ",", "pol"}], "]"}], ",", 
         RowBox[{"RadioButtonBar", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "plane3", "]"}], ",", "plane"}], "]"}], 
         ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"reA3", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<Re[A3]\>\""}]}], "]"}], ",", 
         "\"\<+\[ImaginaryI]\>\"", ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"imA3", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<Im[A3]\>\""}]}], "]"}]}], 
        "}"}], ",", "\[IndentingNewLine]", 
       RowBox[{"{", 
        RowBox[{
         RowBox[{"Checkbox", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "point4", "]"}], ",", 
           RowBox[{"{", 
            RowBox[{"True", ",", "False"}], "}"}]}], "]"}], ",", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<4th point\>\"", ",", 
           RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], 
         ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "kx4", "]"}], ",", "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<kx4/k0\>\""}]}], "]"}], ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "ky4", "]"}], ",", "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<ky4/k0\>\""}]}], "]"}], ",", 
         RowBox[{"RadioButtonBar", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "pol4", "]"}], ",", "pol"}], "]"}], ",", 
         RowBox[{"RadioButtonBar", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "plane4", "]"}], ",", "plane"}], "]"}], 
         ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"reA4", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<Re[A4]\>\""}]}], "]"}], ",", 
         "\"\<+\[ImaginaryI]\>\"", ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"imA4", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<Im[A4]\>\""}]}], "]"}]}], 
        "}"}], ",", "\[IndentingNewLine]", 
       RowBox[{"{", 
        RowBox[{
         RowBox[{"Checkbox", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "point5", "]"}], ",", 
           RowBox[{"{", 
            RowBox[{"True", ",", "False"}], "}"}]}], "]"}], ",", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<5th point\>\"", ",", 
           RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], 
         ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "kx5", "]"}], ",", "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<kx5/k0\>\""}]}], "]"}], ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "ky5", "]"}], ",", "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<ky5/k0\>\""}]}], "]"}], ",", 
         RowBox[{"RadioButtonBar", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "pol5", "]"}], ",", "pol"}], "]"}], ",", 
         RowBox[{"RadioButtonBar", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "plane5", "]"}], ",", "plane"}], "]"}], 
         ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"reA5", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<Re[A5]\>\""}]}], "]"}], ",", 
         "\"\<+\[ImaginaryI]\>\"", ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"imA5", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<Im[A5]\>\""}]}], "]"}]}], 
        "}"}], ",", "\[IndentingNewLine]", 
       RowBox[{"{", 
        RowBox[{
         RowBox[{"Checkbox", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "point6", "]"}], ",", 
           RowBox[{"{", 
            RowBox[{"True", ",", "False"}], "}"}]}], "]"}], ",", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<6th point\>\"", ",", 
           RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], 
         ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "kx6", "]"}], ",", "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<kx6/k0\>\""}]}], "]"}], ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "ky6", "]"}], ",", "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<ky6/k0\>\""}]}], "]"}], ",", 
         RowBox[{"RadioButtonBar", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "pol6", "]"}], ",", "pol"}], "]"}], ",", 
         RowBox[{"RadioButtonBar", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", "plane6", "]"}], ",", "plane"}], "]"}], 
         ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"reA6", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<Re[A6]\>\""}]}], "]"}], ",", 
         "\"\<+\[ImaginaryI]\>\"", ",", 
         RowBox[{"InputField", "[", 
          RowBox[{
           RowBox[{"Dynamic", "[", 
            RowBox[{"imA6", ",", 
             RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
           "Number", ",", 
           RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
           RowBox[{"FieldHint", "\[Rule]", "\"\<Im[A6]\>\""}]}], "]"}]}], 
        "}"}]}], "}"}], "\[IndentingNewLine]", ",", 
     RowBox[{"Frame", "\[Rule]", "All"}]}], "]"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Print", "[", 
   RowBox[{"Dynamic", "[", "result", "]"}], "]"}], "\[IndentingNewLine]", 
  "\[IndentingNewLine]"}], "\[IndentingNewLine]", 
 RowBox[{"Framed", "[", 
  RowBox[{
   RowBox[{"Row", "[", 
    RowBox[{"{", 
     RowBox[{
      RowBox[{"Style", "[", 
       RowBox[{"\"\<Read more at:\\n\>\"", ",", 
        RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
        RowBox[{"FontSlant", "\[Rule]", "Italic"}], ",", "Gray"}], "]"}], 
      ",", "\"\<\>\"", ",", 
      RowBox[{"Style", "[", 
       RowBox[{"Button", "[", 
        RowBox[{
         RowBox[{"Mouseover", "[", 
          RowBox[{
           RowBox[{"Style", "[", 
            RowBox[{
            "\"\<M. F. Picardi, A. Zayats and F. Rodr\[IAcute]guez-Fortu\
\[NTilde]o, Amplitude and phase control of guided modes excitation from a \
single dipole source: engineering far- and near-field directionality, \
arXiv:1907.06573\>\"", ",", 
             RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
             RowBox[{"FontSlant", "\[Rule]", "Italic"}], ",", "Gray"}], "]"}],
            ",", 
           RowBox[{"Style", "[", 
            RowBox[{
            "\"\<M. F. Picardi, A. Zayats and F. Rodr\[IAcute]guez-Fortu\
\[NTilde]o, Amplitude and phase control of guided modes excitation from a \
single dipole source: engineering far- and near-field directionality, \
arXiv:1907.06573\>\"", ",", 
             RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
             RowBox[{"FontSlant", "\[Rule]", "Italic"}], ",", "Black"}], 
            "]"}]}], "]"}], ",", 
         RowBox[{"NotebookLocate", "[", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{
            "URL", "[", "\"\<https://arxiv.org/abs/1907.06573\>\"", "]"}], 
            ",", "None"}], "}"}], "]"}], ",", 
         RowBox[{"Appearance", "\[Rule]", "None"}]}], "]"}], "]"}]}], "}"}], 
    "]"}], ",", 
   RowBox[{"Background", "\[Rule]", 
    RowBox[{"RGBColor", "[", 
     RowBox[{"0", ",", "1", ",", "0", ",", ".2"}], "]"}]}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"A1", "=", 
    RowBox[{"(", 
     RowBox[{"reA1", "+", 
      RowBox[{"\[ImaginaryI]", " ", "imA1"}]}], ")"}]}], ";", 
   "\[IndentingNewLine]", 
   RowBox[{"A2", "=", 
    RowBox[{"(", 
     RowBox[{"reA2", "+", 
      RowBox[{"\[ImaginaryI]", " ", "imA2"}]}], ")"}]}], ";", 
   "\[IndentingNewLine]", 
   RowBox[{"A3", "=", 
    RowBox[{"(", 
     RowBox[{"reA3", "+", 
      RowBox[{"\[ImaginaryI]", " ", "imA3"}]}], ")"}]}], ";", 
   "\[IndentingNewLine]", 
   RowBox[{"A4", "=", 
    RowBox[{"(", 
     RowBox[{"reA4", "+", 
      RowBox[{"\[ImaginaryI]", " ", "imA4"}]}], ")"}]}], ";", 
   "\[IndentingNewLine]", 
   RowBox[{"A5", "=", 
    RowBox[{"(", 
     RowBox[{"reA5", "+", 
      RowBox[{"\[ImaginaryI]", " ", "imA5"}]}], ")"}]}], ";", 
   "\[IndentingNewLine]", 
   RowBox[{"A6", "=", 
    RowBox[{"(", 
     RowBox[{"reA6", "+", 
      RowBox[{"\[ImaginaryI]", " ", "imA6"}]}], ")"}]}], ";"}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Dynamic", "[", 
   RowBox[{
    RowBox[{"If", " ", "[", 
     RowBox[{
      RowBox[{"point1", "\[Equal]", "False"}], ",", "\[IndentingNewLine]", 
      RowBox[{"Dynamic", "[", 
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{
          RowBox[{"kx1", "\[Equal]", "0"}], "&&", 
          RowBox[{"ky1", "\[Equal]", "0"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{"Dynamic", "[", 
          RowBox[{
           RowBox[{"Which", "[", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{
              RowBox[{"pol1", "\[Equal]", "\"\<spol\>\""}], "&&", 
              RowBox[{"plane1", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M1", "=", 
              RowBox[{"{", 
               RowBox[{
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0", ",", 
                "0", ",", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", "c", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0"}], "}"}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol1", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
              RowBox[{"plane1", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M1", "=", 
              RowBox[{"{", 
               RowBox[{
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0", ",", 
                "0", ",", 
                RowBox[{"-", 
                 FractionBox["\[ImaginaryI]", 
                  RowBox[{"8", " ", "c", " ", 
                   SuperscriptBox["\[Pi]", "2"]}]]}], ",", "0"}], "}"}]}], 
             ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol1", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
              RowBox[{"plane1", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M1", "=", 
              RowBox[{"{", 
               RowBox[{"0", ",", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", 
                RowBox[{"-", 
                 FractionBox["\[ImaginaryI]", 
                  RowBox[{"8", " ", "c", " ", 
                   SuperscriptBox["\[Pi]", "2"]}]]}], ",", "0", ",", "0"}], 
               "}"}]}], ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol1", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
              RowBox[{"plane1", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M1", "=", 
              RowBox[{"{", 
               RowBox[{"0", ",", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", "c", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0"}], 
               "}"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"Dynamic", "[", 
          RowBox[{
           RowBox[{"Which", "[", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{
              RowBox[{"pol1", "\[Equal]", "\"\<spol\>\""}], "&&", 
              RowBox[{"plane1", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M1", "=", 
              RowBox[{
               RowBox[{"Limit", "[", 
                RowBox[{
                 RowBox[{"Limit", "[", 
                  RowBox[{
                   RowBox[{"Coefficient", "[", 
                    RowBox[{
                    RowBox[{"EsEpp", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], " ", ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                   RowBox[{"kx", "\[Rule]", "kx1"}]}], "]"}], ",", 
                 RowBox[{"ky", "\[Rule]", "ky1"}]}], "]"}], "//", 
               "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol1", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
              RowBox[{"plane1", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M1", "=", 
              RowBox[{
               RowBox[{"Limit", "[", 
                RowBox[{
                 RowBox[{"Limit", "[", 
                  RowBox[{
                   RowBox[{"Coefficient", "[", 
                    RowBox[{
                    RowBox[{"EsEpm", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], " ", ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                   RowBox[{"kx", "\[Rule]", "kx1"}]}], "]"}], ",", 
                 RowBox[{"ky", "\[Rule]", "ky1"}]}], "]"}], "//", 
               "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol1", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
              RowBox[{"plane1", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M1", "=", 
              RowBox[{
               RowBox[{"Limit", "[", 
                RowBox[{
                 RowBox[{"Limit", "[", 
                  RowBox[{
                   RowBox[{"Coefficient", "[", 
                    RowBox[{
                    RowBox[{"EpEpp", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], " ", ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                   RowBox[{"kx", "\[Rule]", "kx1"}]}], "]"}], ",", 
                 RowBox[{"ky", "\[Rule]", "ky1"}]}], "]"}], "//", 
               "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol1", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
              RowBox[{"plane1", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M1", "=", 
              RowBox[{
               RowBox[{"Limit", "[", 
                RowBox[{
                 RowBox[{"Limit", "[", 
                  RowBox[{
                   RowBox[{"Coefficient", "[", 
                    RowBox[{
                    RowBox[{"EpEpm", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], " ", ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                   RowBox[{"kx", "\[Rule]", "kx1"}]}], "]"}], ",", 
                 RowBox[{"ky", "\[Rule]", "ky1"}]}], "]"}], "//", 
               "FullSimplify"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}]}], "]"}], 
       "]"}], "\[IndentingNewLine]", ",", 
      RowBox[{
       RowBox[{"M1", "=", 
        RowBox[{"{", 
         RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
         "}"}]}], ";", 
       RowBox[{"A1", "=", "0"}]}]}], "]"}], ",", 
    RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], 
  "\[IndentingNewLine]"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Dynamic", "[", 
   RowBox[{
    RowBox[{"If", " ", "[", 
     RowBox[{
      RowBox[{"point2", "\[Equal]", "False"}], ",", "\[IndentingNewLine]", 
      RowBox[{"Dynamic", "[", 
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{
          RowBox[{"kx2", "\[Equal]", "0"}], "&&", 
          RowBox[{"ky2", "\[Equal]", "0"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{"Dynamic", "[", 
          RowBox[{
           RowBox[{"Which", "[", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{
              RowBox[{"pol2", "\[Equal]", "\"\<spol\>\""}], "&&", 
              RowBox[{"plane2", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M2", "=", 
              RowBox[{"{", 
               RowBox[{
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0", ",", 
                "0", ",", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", "c", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0"}], "}"}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol2", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
              RowBox[{"plane2", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M2", "=", 
              RowBox[{"{", 
               RowBox[{
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0", ",", 
                "0", ",", 
                RowBox[{"-", 
                 FractionBox["\[ImaginaryI]", 
                  RowBox[{"8", " ", "c", " ", 
                   SuperscriptBox["\[Pi]", "2"]}]]}], ",", "0"}], "}"}]}], 
             ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol2", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
              RowBox[{"plane2", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M2", "=", 
              RowBox[{"{", 
               RowBox[{"0", ",", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", 
                RowBox[{"-", 
                 FractionBox["\[ImaginaryI]", 
                  RowBox[{"8", " ", "c", " ", 
                   SuperscriptBox["\[Pi]", "2"]}]]}], ",", "0", ",", "0"}], 
               "}"}]}], ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol2", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
              RowBox[{"plane2", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M2", "=", 
              RowBox[{"{", 
               RowBox[{"0", ",", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", "c", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0"}], 
               "}"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"Dynamic", "[", 
          RowBox[{
           RowBox[{"Which", "[", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{
              RowBox[{"pol2", "\[Equal]", "\"\<spol\>\""}], "&&", 
              RowBox[{"plane2", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M2", "=", 
              RowBox[{
               RowBox[{"Limit", "[", 
                RowBox[{
                 RowBox[{"Limit", "[", 
                  RowBox[{
                   RowBox[{"Coefficient", "[", 
                    RowBox[{
                    RowBox[{"EsEpp", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], " ", ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                   RowBox[{"kx", "\[Rule]", "kx2"}]}], "]"}], ",", 
                 RowBox[{"ky", "\[Rule]", "ky2"}]}], "]"}], "//", 
               "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol2", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
              RowBox[{"plane2", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M2", "=", 
              RowBox[{
               RowBox[{"Limit", "[", 
                RowBox[{
                 RowBox[{"Limit", "[", 
                  RowBox[{
                   RowBox[{"Coefficient", "[", 
                    RowBox[{
                    RowBox[{"EsEpm", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], " ", ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                   RowBox[{"kx", "\[Rule]", "kx2"}]}], "]"}], ",", 
                 RowBox[{"ky", "\[Rule]", "ky2"}]}], "]"}], "//", 
               "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol2", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
              RowBox[{"plane2", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M2", "=", 
              RowBox[{
               RowBox[{"Limit", "[", 
                RowBox[{
                 RowBox[{"Limit", "[", 
                  RowBox[{
                   RowBox[{"Coefficient", "[", 
                    RowBox[{
                    RowBox[{"EpEpp", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], " ", ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                   RowBox[{"kx", "\[Rule]", "kx2"}]}], "]"}], ",", 
                 RowBox[{"ky", "\[Rule]", "ky2"}]}], "]"}], "//", 
               "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol2", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
              RowBox[{"plane2", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M2", "=", 
              RowBox[{
               RowBox[{"Limit", "[", 
                RowBox[{
                 RowBox[{"Limit", "[", 
                  RowBox[{
                   RowBox[{"Coefficient", "[", 
                    RowBox[{
                    RowBox[{"EpEpm", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], " ", ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                   RowBox[{"kx", "\[Rule]", "kx2"}]}], "]"}], ",", 
                 RowBox[{"ky", "\[Rule]", "ky2"}]}], "]"}], "//", 
               "FullSimplify"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}]}], "]"}], 
       "]"}], "\[IndentingNewLine]", ",", 
      RowBox[{
       RowBox[{"M2", "=", 
        RowBox[{"{", 
         RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
         "}"}]}], ";", 
       RowBox[{"A2", "=", "0"}]}]}], "]"}], ",", 
    RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], 
  "\[IndentingNewLine]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"If", " ", "[", 
    RowBox[{
     RowBox[{"point3", "\[Equal]", "False"}], ",", "\[IndentingNewLine]", 
     RowBox[{"Dynamic", "[", 
      RowBox[{"If", "[", 
       RowBox[{
        RowBox[{
         RowBox[{"kx3", "\[Equal]", "0"}], "&&", 
         RowBox[{"ky3", "\[Equal]", "0"}]}], ",", "\[IndentingNewLine]", 
        RowBox[{"Dynamic", "[", 
         RowBox[{
          RowBox[{"Which", "[", "\[IndentingNewLine]", 
           RowBox[{
            RowBox[{
             RowBox[{"pol3", "\[Equal]", "\"\<spol\>\""}], "&&", 
             RowBox[{"plane3", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M3", "=", 
             RowBox[{"{", 
              RowBox[{
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0", ",", 
               "0", ",", 
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", "c", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0"}], "}"}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol3", "\[Equal]", "\"\<spol\>\""}], "&&", 
             RowBox[{"plane3", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M3", "=", 
             RowBox[{"{", 
              RowBox[{
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0", ",", 
               "0", ",", 
               RowBox[{"-", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", "c", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]]}], ",", "0"}], "}"}]}], ",",
             "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol3", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
             RowBox[{"plane3", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M3", "=", 
             RowBox[{"{", 
              RowBox[{"0", ",", 
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", 
               RowBox[{"-", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", "c", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]]}], ",", "0", ",", "0"}], 
              "}"}]}], ",", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol3", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
             RowBox[{"plane3", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M3", "=", 
             RowBox[{"{", 
              RowBox[{"0", ",", 
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", 
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", "c", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0"}], 
              "}"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
          RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
        "\[IndentingNewLine]", 
        RowBox[{"Dynamic", "[", 
         RowBox[{
          RowBox[{"Which", "[", "\[IndentingNewLine]", 
           RowBox[{
            RowBox[{
             RowBox[{"pol3", "\[Equal]", "\"\<spol\>\""}], "&&", 
             RowBox[{"plane3", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M3", "=", 
             RowBox[{
              RowBox[{"Limit", "[", 
               RowBox[{
                RowBox[{"Limit", "[", 
                 RowBox[{
                  RowBox[{"Coefficient", "[", 
                   RowBox[{
                    RowBox[{"EsEpp", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], " ", ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                  RowBox[{"kx", "\[Rule]", "kx3"}]}], "]"}], ",", 
                RowBox[{"ky", "\[Rule]", "ky3"}]}], "]"}], "//", 
              "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol3", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
             RowBox[{"plane3", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M3", "=", 
             RowBox[{
              RowBox[{"Limit", "[", 
               RowBox[{
                RowBox[{"Limit", "[", 
                 RowBox[{
                  RowBox[{"Coefficient", "[", 
                   RowBox[{
                    RowBox[{"EsEpm", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                  RowBox[{"kx", "\[Rule]", "kx3"}]}], "]"}], ",", 
                RowBox[{"ky", "\[Rule]", "ky3"}]}], "]"}], "//", 
              "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol3", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
             RowBox[{"plane3", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M3", "=", 
             RowBox[{
              RowBox[{"Limit", "[", 
               RowBox[{
                RowBox[{"Limit", "[", 
                 RowBox[{
                  RowBox[{"Coefficient", "[", 
                   RowBox[{
                    RowBox[{"EpEpp", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                  RowBox[{"kx", "\[Rule]", "kx3"}]}], "]"}], ",", 
                RowBox[{"ky", "\[Rule]", "ky3"}]}], "]"}], "//", 
              "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol3", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
             RowBox[{"plane3", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M3", "=", 
             RowBox[{
              RowBox[{"Limit", "[", 
               RowBox[{
                RowBox[{"Limit", "[", 
                 RowBox[{
                  RowBox[{"Coefficient", "[", 
                   RowBox[{
                    RowBox[{"EpEpm", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                  RowBox[{"kx", "\[Rule]", "kx3"}]}], "]"}], ",", 
                RowBox[{"ky", "\[Rule]", "ky3"}]}], "]"}], "//", 
              "FullSimplify"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
          RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}]}], "]"}], 
      "]"}], "\[IndentingNewLine]", ",", 
     RowBox[{
      RowBox[{"M3", "=", 
       RowBox[{"{", 
        RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
        "}"}]}], ";", 
      RowBox[{"A3", "=", "0"}]}]}], "]"}], ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Dynamic", "[", 
   RowBox[{
    RowBox[{"If", " ", "[", 
     RowBox[{
      RowBox[{"point4", "\[Equal]", "False"}], ",", "\[IndentingNewLine]", 
      RowBox[{"Dynamic", "[", 
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{
          RowBox[{"kx4", "\[Equal]", "0"}], "&&", 
          RowBox[{"ky4", "\[Equal]", "0"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{"Dynamic", "[", 
          RowBox[{
           RowBox[{"Which", "[", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{
              RowBox[{"pol4", "\[Equal]", "\"\<spol\>\""}], "&&", 
              RowBox[{"plane4", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M4", "=", 
              RowBox[{"{", 
               RowBox[{
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0", ",", 
                "0", ",", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", "c", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0"}], "}"}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol4", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
              RowBox[{"plane4", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M4", "=", 
              RowBox[{"{", 
               RowBox[{
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0", ",", 
                "0", ",", 
                RowBox[{"-", 
                 FractionBox["\[ImaginaryI]", 
                  RowBox[{"8", " ", "c", " ", 
                   SuperscriptBox["\[Pi]", "2"]}]]}], ",", "0"}], "}"}]}], 
             ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol4", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
              RowBox[{"plane4", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M4", "=", 
              RowBox[{"{", 
               RowBox[{"0", ",", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", 
                RowBox[{"-", 
                 FractionBox["\[ImaginaryI]", 
                  RowBox[{"8", " ", "c", " ", 
                   SuperscriptBox["\[Pi]", "2"]}]]}], ",", "0", ",", "0"}], 
               "}"}]}], ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol4", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
              RowBox[{"plane4", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M4", "=", 
              RowBox[{"{", 
               RowBox[{"0", ",", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", "c", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0"}], 
               "}"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"Dynamic", "[", 
          RowBox[{
           RowBox[{"Which", "[", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{
              RowBox[{"pol4", "\[Equal]", "\"\<spol\>\""}], "&&", 
              RowBox[{"plane4", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M4", "=", 
              RowBox[{
               RowBox[{"Limit", "[", 
                RowBox[{
                 RowBox[{"Limit", "[", 
                  RowBox[{
                   RowBox[{"Coefficient", "[", 
                    RowBox[{
                    RowBox[{"EsEpp", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                   RowBox[{"kx", "\[Rule]", "kx4"}]}], "]"}], ",", 
                 RowBox[{"ky", "\[Rule]", "ky4"}]}], "]"}], "//", 
               "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol4", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
              RowBox[{"plane4", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M4", "=", 
              RowBox[{
               RowBox[{"Limit", "[", 
                RowBox[{
                 RowBox[{"Limit", "[", 
                  RowBox[{
                   RowBox[{"Coefficient", "[", 
                    RowBox[{
                    RowBox[{"EsEpm", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                   RowBox[{"kx", "\[Rule]", "kx4"}]}], "]"}], ",", 
                 RowBox[{"ky", "\[Rule]", "ky4"}]}], "]"}], "//", 
               "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol4", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
              RowBox[{"plane4", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M4", "=", 
              RowBox[{
               RowBox[{"Limit", "[", 
                RowBox[{
                 RowBox[{"Limit", "[", 
                  RowBox[{
                   RowBox[{"Coefficient", "[", 
                    RowBox[{
                    RowBox[{"EpEpp", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                   RowBox[{"kx", "\[Rule]", "kx4"}]}], "]"}], ",", 
                 RowBox[{"ky", "\[Rule]", "ky4"}]}], "]"}], "//", 
               "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
             RowBox[{
              RowBox[{"pol4", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
              RowBox[{"plane4", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
             "\[IndentingNewLine]", 
             RowBox[{"M4", "=", 
              RowBox[{
               RowBox[{"Limit", "[", 
                RowBox[{
                 RowBox[{"Limit", "[", 
                  RowBox[{
                   RowBox[{"Coefficient", "[", 
                    RowBox[{
                    RowBox[{"EpEpm", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                   RowBox[{"kx", "\[Rule]", "kx4"}]}], "]"}], ",", 
                 RowBox[{"ky", "\[Rule]", "ky4"}]}], "]"}], "//", 
               "FullSimplify"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}]}], "]"}], 
       "]"}], "\[IndentingNewLine]", ",", 
      RowBox[{
       RowBox[{"M4", "=", 
        RowBox[{"{", 
         RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
         "}"}]}], ";", 
       RowBox[{"A4", "=", "0"}]}]}], "]"}], ",", 
    RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], 
  "\[IndentingNewLine]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"If", " ", "[", 
    RowBox[{
     RowBox[{"point5", "\[Equal]", "False"}], ",", "\[IndentingNewLine]", 
     RowBox[{"Dynamic", "[", 
      RowBox[{"If", "[", 
       RowBox[{
        RowBox[{
         RowBox[{"kx5", "\[Equal]", "0"}], "&&", 
         RowBox[{"ky5", "\[Equal]", "0"}]}], ",", "\[IndentingNewLine]", 
        RowBox[{"Dynamic", "[", 
         RowBox[{
          RowBox[{"Which", "[", "\[IndentingNewLine]", 
           RowBox[{
            RowBox[{
             RowBox[{"pol5", "\[Equal]", "\"\<spol\>\""}], "&&", 
             RowBox[{"plane5", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M5", "=", 
             RowBox[{"{", 
              RowBox[{
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0", ",", 
               "0", ",", 
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", "c", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0"}], "}"}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol5", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
             RowBox[{"plane5", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M5", "=", 
             RowBox[{"{", 
              RowBox[{
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0", ",", 
               "0", ",", 
               RowBox[{"-", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", "c", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]]}], ",", "0"}], "}"}]}], ",",
             "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol5", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
             RowBox[{"plane5", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M5", "=", 
             RowBox[{"{", 
              RowBox[{"0", ",", 
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", 
               RowBox[{"-", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", "c", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]]}], ",", "0", ",", "0"}], 
              "}"}]}], ",", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol5", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
             RowBox[{"plane5", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M5", "=", 
             RowBox[{"{", 
              RowBox[{"0", ",", 
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", 
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", "c", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0"}], 
              "}"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
          RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
        "\[IndentingNewLine]", 
        RowBox[{"Dynamic", "[", 
         RowBox[{
          RowBox[{"Which", "[", "\[IndentingNewLine]", 
           RowBox[{
            RowBox[{
             RowBox[{"pol5", "\[Equal]", "\"\<spol\>\""}], "&&", 
             RowBox[{"plane5", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M5", "=", 
             RowBox[{
              RowBox[{"Limit", "[", 
               RowBox[{
                RowBox[{"Limit", "[", 
                 RowBox[{
                  RowBox[{"Coefficient", "[", 
                   RowBox[{
                    RowBox[{"EsEpp", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                  RowBox[{"kx", "\[Rule]", "kx5"}]}], "]"}], ",", 
                RowBox[{"ky", "\[Rule]", "ky5"}]}], "]"}], "//", 
              "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol5", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
             RowBox[{"plane5", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M5", "=", 
             RowBox[{
              RowBox[{"Limit", "[", 
               RowBox[{
                RowBox[{"Limit", "[", 
                 RowBox[{
                  RowBox[{"Coefficient", "[", 
                   RowBox[{
                    RowBox[{"EsEpm", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                  RowBox[{"kx", "\[Rule]", "kx5"}]}], "]"}], ",", 
                RowBox[{"ky", "\[Rule]", "ky5"}]}], "]"}], "//", 
              "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol5", "\[Equal]", "\"\<ppol\>\""}], "&&", 
             RowBox[{"plane5", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M5", "=", 
             RowBox[{
              RowBox[{"Limit", "[", 
               RowBox[{
                RowBox[{"Limit", "[", 
                 RowBox[{
                  RowBox[{"Coefficient", "[", 
                   RowBox[{
                    RowBox[{"EpEpp", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                  RowBox[{"kx", "\[Rule]", "kx5"}]}], "]"}], ",", 
                RowBox[{"ky", "\[Rule]", "ky5"}]}], "]"}], "//", 
              "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol5", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
             RowBox[{"plane5", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M5", "=", 
             RowBox[{
              RowBox[{"Limit", "[", 
               RowBox[{
                RowBox[{"Limit", "[", 
                 RowBox[{
                  RowBox[{"Coefficient", "[", 
                   RowBox[{
                    RowBox[{"EpEpm", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                  RowBox[{"kx", "\[Rule]", "kx5"}]}], "]"}], ",", 
                RowBox[{"ky", "\[Rule]", "ky5"}]}], "]"}], "//", 
              "FullSimplify"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
          RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}]}], "]"}], 
      "]"}], "\[IndentingNewLine]", ",", 
     RowBox[{
      RowBox[{"M5", "=", 
       RowBox[{"{", 
        RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
        "}"}]}], ";", 
      RowBox[{"A5", "=", "0"}]}]}], "]"}], ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"If", " ", "[", 
    RowBox[{
     RowBox[{"point6", "\[Equal]", "False"}], ",", "\[IndentingNewLine]", 
     RowBox[{"Dynamic", "[", 
      RowBox[{"If", "[", 
       RowBox[{
        RowBox[{
         RowBox[{"kx6", "\[Equal]", "0"}], "&&", 
         RowBox[{"ky6", "\[Equal]", "0"}]}], ",", "\[IndentingNewLine]", 
        RowBox[{"Dynamic", "[", 
         RowBox[{
          RowBox[{"Which", "[", "\[IndentingNewLine]", 
           RowBox[{
            RowBox[{
             RowBox[{"pol6", "\[Equal]", "\"\<spol\>\""}], "&&", 
             RowBox[{"plane6", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M6", "=", 
             RowBox[{"{", 
              RowBox[{
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0", ",", 
               "0", ",", 
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", "c", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0"}], "}"}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol6", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
             RowBox[{"plane6", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M6", "=", 
             RowBox[{"{", 
              RowBox[{
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0", ",", 
               "0", ",", 
               RowBox[{"-", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", "c", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]]}], ",", "0"}], "}"}]}], ",",
             "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol6", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
             RowBox[{"plane6", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M6", "=", 
             RowBox[{"{", 
              RowBox[{"0", ",", 
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", 
               RowBox[{"-", 
                FractionBox["\[ImaginaryI]", 
                 RowBox[{"8", " ", "c", " ", 
                  SuperscriptBox["\[Pi]", "2"]}]]}], ",", "0", ",", "0"}], 
              "}"}]}], ",", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol6", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
             RowBox[{"plane6", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M6", "=", 
             RowBox[{"{", 
              RowBox[{"0", ",", 
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", 
               FractionBox["\[ImaginaryI]", 
                RowBox[{"8", " ", "c", " ", 
                 SuperscriptBox["\[Pi]", "2"]}]], ",", "0", ",", "0"}], 
              "}"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
          RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
        "\[IndentingNewLine]", 
        RowBox[{"Dynamic", "[", 
         RowBox[{
          RowBox[{"Which", "[", "\[IndentingNewLine]", 
           RowBox[{
            RowBox[{
             RowBox[{"pol6", "\[Equal]", "\"\<spol\>\""}], "&&", 
             RowBox[{"plane6", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M6", "=", 
             RowBox[{
              RowBox[{"Limit", "[", 
               RowBox[{
                RowBox[{"Limit", "[", 
                 RowBox[{
                  RowBox[{"Coefficient", "[", 
                   RowBox[{
                    RowBox[{"EsEpp", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                  RowBox[{"kx", "\[Rule]", "kx6"}]}], "]"}], ",", 
                RowBox[{"ky", "\[Rule]", "ky6"}]}], "]"}], "//", 
              "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol6", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
             RowBox[{"plane6", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M6", "=", 
             RowBox[{
              RowBox[{"Limit", "[", 
               RowBox[{
                RowBox[{"Limit", "[", 
                 RowBox[{
                  RowBox[{"Coefficient", "[", 
                   RowBox[{
                    RowBox[{"EsEpm", " ", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                  RowBox[{"kx", "\[Rule]", "kx6"}]}], "]"}], ",", 
                RowBox[{"ky", "\[Rule]", "ky6"}]}], "]"}], "//", 
              "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol6", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
             RowBox[{"plane6", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M6", "=", 
             RowBox[{
              RowBox[{"Limit", "[", 
               RowBox[{
                RowBox[{"Limit", "[", 
                 RowBox[{
                  RowBox[{"Coefficient", "[", 
                   RowBox[{
                    RowBox[{"EpEpp", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                  RowBox[{"kx", "\[Rule]", "kx6"}]}], "]"}], ",", 
                RowBox[{"ky", "\[Rule]", "ky6"}]}], "]"}], "//", 
              "FullSimplify"}]}], ",", "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"pol6", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
             RowBox[{"plane6", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
            "\[IndentingNewLine]", 
            RowBox[{"M6", "=", 
             RowBox[{
              RowBox[{"Limit", "[", 
               RowBox[{
                RowBox[{"Limit", "[", 
                 RowBox[{
                  RowBox[{"Coefficient", "[", 
                   RowBox[{
                    RowBox[{"EpEpm", 
                    SqrtBox[
                    RowBox[{
                    SuperscriptBox["k1", "2"], "-", 
                    RowBox[{"(", 
                    RowBox[{
                    SuperscriptBox["kx", "2"], "+", 
                    SuperscriptBox["ky", "2"]}], ")"}]}]]}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", 
                    "mz"}], "}"}]}], "]"}], ",", 
                  RowBox[{"kx", "\[Rule]", "kx6"}]}], "]"}], ",", 
                RowBox[{"ky", "\[Rule]", "ky6"}]}], "]"}], "//", 
              "FullSimplify"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
          RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}]}], "]"}], 
      "]"}], "\[IndentingNewLine]", ",", 
     RowBox[{
      RowBox[{"M6", "=", 
       RowBox[{"{", 
        RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
        "}"}]}], ";", 
      RowBox[{"A6", "=", "0"}]}]}], "]"}], ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{"nulsol", " ", "=", " ", 
   RowBox[{"NullSpace", "[", 
    RowBox[{"{", 
     RowBox[{"M1", ",", "M2", ",", "M3", ",", "M4", ",", "M5", ",", "M6"}], 
     "}"}], "]"}]}], "]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{"result", "=", 
   RowBox[{"Check", "[", 
    RowBox[{
     RowBox[{"Row", "[", 
      RowBox[{"{", 
       RowBox[{
        RowBox[{"Style", "[", 
         RowBox[{
          RowBox[{"MatrixForm", "[", 
           RowBox[{"{", 
            RowBox[{
            "\"\<\!\(\*SubscriptBox[\(p\), \(x\)]\)\>\"", ",", 
             "\"\<\!\(\*SubscriptBox[\(p\), \(y\)]\)\>\"", ",", 
             "\"\<\!\(\*SubscriptBox[\(p\), \(z\)]\)\>\"", ",", 
             "\"\<\!\(\*SubscriptBox[\(m\), \(x\)]\)\>\"", ",", 
             "\"\<\!\(\*SubscriptBox[\(m\), \(y\)]\)\>\"", ",", 
             "\"\<\!\(\*SubscriptBox[\(m\), \(z\)]\)\>\""}], "}"}], "]"}], 
          ",", 
          RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
          RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
        RowBox[{"Style", "[", 
         RowBox[{"\"\<=\>\"", ",", 
          RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
          RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", " ", 
        RowBox[{"Style", "[", 
         RowBox[{
          RowBox[{"MatrixForm", "[", 
           RowBox[{"LinearSolve", "[", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{
              "M1", ",", "M2", ",", "M3", ",", "M4", ",", "M5", ",", "M6"}], 
              "}"}], ",", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{
               "A1", ",", "A2", ",", "A3", ",", "A4", ",", "A5", ",", "A6"}], 
               "}"}], "//", "FullSimplify"}]}], "]"}], "]"}], ",", 
          RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
          RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
        RowBox[{"Style", "[", 
         RowBox[{
          RowBox[{"Dynamic", "[", 
           RowBox[{"plus", ",", 
            RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
          RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
          RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
        RowBox[{"Style", "[", 
         RowBox[{
          RowBox[{"Dynamic", "[", 
           RowBox[{
            RowBox[{"ToPrint", "//", "Simplify"}], ",", 
            RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
          RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
          RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
        RowBox[{"Style", "[", 
         RowBox[{"\"\< \>\"", ",", 
          RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
          RowBox[{"FontSize", "\[Rule]", "64"}]}], "]"}], ",", 
        RowBox[{"Style", "[", 
         RowBox[{
          RowBox[{"Dynamic", "[", 
           RowBox[{"with", ",", 
            RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
          RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
          RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
        RowBox[{"Style", "[", 
         RowBox[{"\"\< \>\"", ",", 
          RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
          RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
        RowBox[{"Style", "[", 
         RowBox[{
          RowBox[{"Dynamic", "[", 
           RowBox[{"coef", ",", 
            RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
          RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
          RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
        RowBox[{"Style", "[", 
         RowBox[{"\"\< \>\"", ",", 
          RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
          RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
        RowBox[{"Style", "[", 
         RowBox[{
          RowBox[{"Dynamic", "[", 
           RowBox[{"compl", ",", 
            RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
          RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
          RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}]}], "}"}], "]"}], ",", 
     RowBox[{"Style", "[", 
      RowBox[{
      "\"\<The system has no solution. Check that all the inputs are \
compatible.\>\"", ",", 
       RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
       RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}]}], "]"}]}], 
  "]"}], "\[IndentingNewLine]"}], "Input",
 Editable->False,
 CellOpen->False,
 FontSize->12,
 ExpressionUUID -> "60847027-2c3e-465f-99c2-d5fe2190a4df"],

Cell[BoxData[
 DynamicBox[ToBoxes[
   Which[Length[$CellContext`nulsol] == 
    0, $CellContext`coef = " "; $CellContext`plus = " "; $CellContext`with = 
     " "; $CellContext`compl = " "; $CellContext`ToPrint = " ", 
    Length[$CellContext`nulsol] == 
    1, $CellContext`coef = {$CellContext`\[Alpha]}; $CellContext`plus = 
     "+"; $CellContext`with = "With"; $CellContext`compl = 
     "\[Element]\[DoubleStruckCapitalC]"; $CellContext`ToPrint = MatrixForm[
        Part[$CellContext`coef, 1]] MatrixForm[
        Part[$CellContext`nulsol, 1]], Length[$CellContext`nulsol] == 
    2, $CellContext`coef = {$CellContext`\[Alpha], $CellContext`\[Beta]}; \
$CellContext`plus = "+"; $CellContext`with = "With"; $CellContext`compl = 
     "\[Element]\[DoubleStruckCapitalC]"; $CellContext`ToPrint = Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 1]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 1]], 2]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 2]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 2]], 2]], Length[$CellContext`nulsol] == 
    3, $CellContext`coef = {$CellContext`\[Alpha], $CellContext`\[Beta], \
$CellContext`\[Gamma]}; $CellContext`plus = "+"; $CellContext`with = 
     "With"; $CellContext`compl = 
     "\[Element]\[DoubleStruckCapitalC]"; $CellContext`ToPrint = Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 1]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 1]], 2]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 2]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 2]], 2]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 3]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 3]], 2]], Length[$CellContext`nulsol] == 
    4, $CellContext`coef = {$CellContext`\[Alpha], $CellContext`\[Beta], \
$CellContext`\[Gamma], $CellContext`\[Delta]}; $CellContext`plus = 
     "+"; $CellContext`with = "With"; $CellContext`compl = 
     "\[Element]\[DoubleStruckCapitalC]"; $CellContext`ToPrint = Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 1]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 1]], 2]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 2]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 2]], 2]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 3]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 3]], 2]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 4]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 4]], 2]], Length[$CellContext`nulsol] == 
    5, $CellContext`coef = {$CellContext`\[Alpha], $CellContext`\[Beta], \
$CellContext`\[Gamma], $CellContext`\[Delta], $CellContext`\[Epsilon]}; \
$CellContext`plus = "+"; $CellContext`with = "With"; $CellContext`compl = 
     "\[Element]\[DoubleStruckCapitalC]"; $CellContext`ToPrint = Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 1]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 1]], 2]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 2]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 2]], 2]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 3]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 3]], 2]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 4]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 4]], 2]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 5]]] Dynamic[
         NumberForm[
          MatrixForm[
           Part[$CellContext`nulsol, 5]], 2]]], StandardForm],
  ImageSizeCache->{622., {58.5, 65.5}}]], "Output",
 Editable->False,
 CellOpen->False,
 FontSize->12],

Cell[BoxData[
 RowBox[{
  DynamicBox[ToBoxes[$CellContext`A1 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`imA1 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`imA2 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`imA3 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`imA4 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`imA5 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`imA6 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`point1 = False, StandardForm],
   ImageSizeCache->{35., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`point2 = True, StandardForm],
   ImageSizeCache->{28., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`point3 = True, StandardForm],
   ImageSizeCache->{28., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`point4 = True, StandardForm],
   ImageSizeCache->{28., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`point5 = True, StandardForm],
   ImageSizeCache->{28., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`point6 = True, StandardForm],
   ImageSizeCache->{28., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`reA1 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`reA2 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`reA3 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`reA4 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`reA5 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[$CellContext`reA6 = 0, StandardForm],
   ImageSizeCache->{7., {0., 8.}}]}]], "Output",
 Editable->False,
 CellOpen->False,
 FontSize->12],

Cell[BoxData[
 RowBox[{
  DynamicBox[ToBoxes[
    If[
     Or[$CellContext`imA1 != 0, $CellContext`reA1 != 0], 
     Dynamic[$CellContext`point1 = False]], StandardForm],
   ImageSizeCache->{28., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[
    If[
     Or[$CellContext`imA2 != 0, $CellContext`reA2 != 0], 
     Dynamic[$CellContext`point2 = False]], StandardForm],
   ImageSizeCache->{28., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[
    If[
     Or[$CellContext`imA3 != 0, $CellContext`reA3 != 0], 
     Dynamic[$CellContext`point3 = False]], StandardForm],
   ImageSizeCache->{28., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[
    If[
     Or[$CellContext`imA4 != 0, $CellContext`reA4 != 0], 
     Dynamic[$CellContext`point4 = False]], StandardForm],
   ImageSizeCache->{28., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[
    If[
     Or[$CellContext`imA5 != 0, $CellContext`reA5 != 0], 
     Dynamic[$CellContext`point5 = False]], StandardForm],
   ImageSizeCache->{28., {0., 8.}}], " ", 
  DynamicBox[ToBoxes[
    If[
     Or[$CellContext`imA6 != 0, $CellContext`reA6 != 0], 
     Dynamic[$CellContext`point6 = False]], StandardForm],
   ImageSizeCache->{28., {0., 8.}}]}]], "Output",
 Editable->False,
 CellOpen->False,
 FontSize->12],

Cell[BoxData[
 StyleBox["\<\"Engineering far and near-field directionality: full amplitude \
and phase control of guided mode excitation from a single dipole source\"\>",
  StripOnInput->False,
  LineColor->GrayLevel[0],
  FrontFaceColor->GrayLevel[0],
  BackFaceColor->GrayLevel[0],
  GraphicsColor->GrayLevel[0],
  FontFamily->"Helvetica",
  FontSize->24,
  FontWeight->Bold,
  FontColor->GrayLevel[0]]], "Output",
 FontSize->12],

Cell[BoxData[
 FrameBox[
  RowBox[{
   FrameBox[
    StyleBox["\<\"Insert the normalised wavevectors for each of the points \
you want to fix in the electric field spectra. For each of the points, \
specify the polarisation and half-space, together with the complex amplitude \
of the spectrum.\\nThe code returns the values for the 6 dipole moments \
components: \\!\\(\\*SubscriptBox[\\(p\\), \\(x\\)]\\), \
\\!\\(\\*SubscriptBox[\\(p\\), \\(y\\)]\\), \\!\\(\\*SubscriptBox[\\(p\\), \
\\(z\\)]\\), \\!\\(\\*SubscriptBox[\\(m\\), \\(x\\)]\\), \
\\!\\(\\*SubscriptBox[\\(m\\), \\(y\\)]\\), \\!\\(\\*SubscriptBox[\\(m\\), \
\\(z\\)]\\) as a particular solution + the associated homogeneous system's \
solutions, each multiplied by an arbitrary complex coefficient.\\nThe code is \
solving a linear system of equations of the type: \
E(\\!\\(\\*SubscriptBox[\\(k\\), \\(x\\)]\\),\\!\\(\\*SubscriptBox[\\(k\\), \
\\(y\\)]\\))=\\!\\(\\*FractionBox[\\(A + \[ImaginaryI]B\\), \
\\(\\*SubscriptBox[\\(k\\), \\(0\\)] \\*SubscriptBox[\\(k\\), \
\\(z\\)]\\)]\\), with A and B specified by the user. For the singular point \
\\!\\(\\*SubscriptBox[\\(k\\), \\(x\\)]\\)=\\!\\(\\*SubscriptBox[\\(k\\), \
\\(y\\)]\\)=0, where the polarisations are not well-defined, \
\\!\\(\\*SubscriptBox[OverscriptBox[\\(e\\), \\(^\\)], \\(s\\)]\\) and \\!\\(\
\\*SubscriptBox[OverscriptBox[\\(e\\), \\(^\\)], \\(p\\)]\\) are taken as \\!\
\\(\\*OverscriptBox[\\(x\\), \\(^\\)]\\) and \\!\\(\\*OverscriptBox[\\(y\\), \
\\(^\\)]\\), respectively.\"\>",
     StripOnInput->False,
     LineSpacing->{1.5, 0},
     LineColor->RGBColor[0, 0, 1],
     FrontFaceColor->RGBColor[0, 0, 1],
     BackFaceColor->RGBColor[0, 0, 1],
     GraphicsColor->RGBColor[0, 0, 1],
     FontFamily->"Helvetica",
     FontSize->14,
     FontColor->RGBColor[0, 0, 1]],
    Background->GrayLevel[0.85],
    StripOnInput->False], " ", 
   TagBox[GridBox[{
      {"\<\" \"\>", "\<\" \"\>", 
       StyleBox["\<\"Normalised \\!\\(\\*SubscriptBox[\\(k\\), \\(x\\)]\\) \
(\\!\\(\\*SubscriptBox[\\(k\\), \\(x\\)]\\)/\\!\\(\\*SubscriptBox[\\(k\\), \
\\(0\\)]\\))\"\>",
        StripOnInput->False,
        FontFamily->"Helvetica",
        FontSize->Larger,
        FontWeight->Bold], 
       StyleBox["\<\"Normalised \\!\\(\\*SubscriptBox[\\(k\\), \
\\(y\\)]\\)(\\!\\(\\*SubscriptBox[\\(k\\), \\(y\\)]\\)/\\!\\(\\*SubscriptBox[\
\\(k\\), \\(0\\)]\\))\"\>",
        StripOnInput->False,
        FontFamily->"Helvetica",
        FontSize->Larger,
        FontWeight->Bold], 
       StyleBox["\<\"Select polarisation\"\>",
        StripOnInput->False,
        FontFamily->"Helvetica",
        FontSize->Larger,
        FontWeight->Bold], 
       StyleBox["\<\"Select half-space\"\>",
        StripOnInput->False,
        FontFamily->"Helvetica",
        FontSize->Larger,
        FontWeight->Bold], 
       StyleBox["\<\"Complex amplitude (A+iB)\"\>",
        StripOnInput->False,
        FontFamily->"Helvetica",
        FontSize->Larger,
        FontWeight->Bold], "\[SpanFromLeft]", "\[SpanFromLeft]"},
      {
       CheckboxBox[Dynamic[$CellContext`point1], {True, False}], 
       StyleBox["\<\"1st point\"\>",
        StripOnInput->False,
        FontFamily->"Helvetica"], 
       InputFieldBox[Dynamic[$CellContext`kx1], Number,
        FieldHint->"kx1/k0",
        FieldSize->Tiny], 
       InputFieldBox[Dynamic[$CellContext`ky1], Number,
        FieldHint->"ky1/k0",
        FieldSize->Tiny], 
       InterpretationBox[
        StyleBox[
         RowBox[{GridBox[{
            {
             
             RadioButtonBox[
              Dynamic[$CellContext`pol1, SaveDefinitions -> True], {"spol"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"s\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
            {
             
             RadioButtonBox[
              Dynamic[$CellContext`pol1, SaveDefinitions -> True], {"ppol"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"p\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
        RadioButtonBar[
         Dynamic[$CellContext`pol1, SaveDefinitions -> True], {
         "spol" -> "s", "ppol" -> "p"}]], 
       InterpretationBox[
        StyleBox[
         RowBox[{GridBox[{
            {
             
             RadioButtonBox[
              Dynamic[$CellContext`plane1, SaveDefinitions -> True], {"zplus"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"z>0\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
            {
             
             RadioButtonBox[
              Dynamic[$CellContext`plane1, SaveDefinitions -> True], {"zmin"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"z<0\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
        RadioButtonBar[
         Dynamic[$CellContext`plane1, SaveDefinitions -> True], {
         "zplus" -> "z>0", "zmin" -> "z<0"}]], 
       InputFieldBox[Dynamic[$CellContext`reA1, SaveDefinitions -> True], 
        Number,
        FieldHint->"Re[A1]",
        FieldSize->Tiny], "\<\"+\[ImaginaryI]\"\>", 
       InputFieldBox[Dynamic[$CellContext`imA1, SaveDefinitions -> True], 
        Number,
        FieldHint->"Im[A1]",
        FieldSize->Tiny]},
      {
       CheckboxBox[Dynamic[$CellContext`point2], {True, False}], 
       StyleBox["\<\"2nd point\"\>",
        StripOnInput->False,
        FontFamily->"Helvetica"], 
       InputFieldBox[Dynamic[$CellContext`kx2], Number,
        FieldHint->"kx2/k0",
        FieldSize->Tiny], 
       InputFieldBox[Dynamic[$CellContext`ky2], Number,
        FieldHint->"ky2/k0",
        FieldSize->Tiny], 
       InterpretationBox[
        StyleBox[
         RowBox[{GridBox[{
            {
             
             RadioButtonBox[
              Dynamic[$CellContext`pol2, SaveDefinitions -> True], {"spol"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"s\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
            {
             
             RadioButtonBox[
              Dynamic[$CellContext`pol2, SaveDefinitions -> True], {"ppol"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"p\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
        RadioButtonBar[
         Dynamic[$CellContext`pol2, SaveDefinitions -> True], {
         "spol" -> "s", "ppol" -> "p"}]], 
       InterpretationBox[
        StyleBox[
         RowBox[{GridBox[{
            {
             
             RadioButtonBox[
              Dynamic[$CellContext`plane2, SaveDefinitions -> True], {"zplus"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"z>0\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
            {
             
             RadioButtonBox[
              Dynamic[$CellContext`plane2, SaveDefinitions -> True], {"zmin"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"z<0\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
        RadioButtonBar[
         Dynamic[$CellContext`plane2, SaveDefinitions -> True], {
         "zplus" -> "z>0", "zmin" -> "z<0"}]], 
       InputFieldBox[Dynamic[$CellContext`reA2, SaveDefinitions -> True], 
        Number,
        FieldHint->"Re[A2]",
        FieldSize->Tiny], "\<\"+\[ImaginaryI]\"\>", 
       InputFieldBox[Dynamic[$CellContext`imA2, SaveDefinitions -> True], 
        Number,
        FieldHint->"Im[A2]",
        FieldSize->Tiny]},
      {
       CheckboxBox[Dynamic[$CellContext`point3], {True, False}], 
       StyleBox["\<\"3rd point\"\>",
        StripOnInput->False,
        FontFamily->"Helvetica"], 
       InputFieldBox[Dynamic[$CellContext`kx3], Number,
        FieldHint->"kx3/k0",
        FieldSize->Tiny], 
       InputFieldBox[Dynamic[$CellContext`ky3], Number,
        FieldHint->"ky3/k0",
        FieldSize->Tiny], 
       InterpretationBox[
        StyleBox[
         RowBox[{GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`pol3], {"spol"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"s\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`pol3], {"ppol"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"p\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
        RadioButtonBar[
         Dynamic[$CellContext`pol3], {"spol" -> "s", "ppol" -> "p"}]], 
       InterpretationBox[
        StyleBox[
         RowBox[{GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`plane3], {"zplus"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"z>0\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`plane3], {"zmin"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"z<0\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
        RadioButtonBar[
         Dynamic[$CellContext`plane3], {"zplus" -> "z>0", "zmin" -> "z<0"}]], 
       InputFieldBox[Dynamic[$CellContext`reA3, SaveDefinitions -> True], 
        Number,
        FieldHint->"Re[A3]",
        FieldSize->Tiny], "\<\"+\[ImaginaryI]\"\>", 
       InputFieldBox[Dynamic[$CellContext`imA3, SaveDefinitions -> True], 
        Number,
        FieldHint->"Im[A3]",
        FieldSize->Tiny]},
      {
       CheckboxBox[Dynamic[$CellContext`point4], {True, False}], 
       StyleBox["\<\"4th point\"\>",
        StripOnInput->False,
        FontFamily->"Helvetica"], 
       InputFieldBox[Dynamic[$CellContext`kx4], Number,
        FieldHint->"kx4/k0",
        FieldSize->Tiny], 
       InputFieldBox[Dynamic[$CellContext`ky4], Number,
        FieldHint->"ky4/k0",
        FieldSize->Tiny], 
       InterpretationBox[
        StyleBox[
         RowBox[{GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`pol4], {"spol"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"s\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`pol4], {"ppol"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"p\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
        RadioButtonBar[
         Dynamic[$CellContext`pol4], {"spol" -> "s", "ppol" -> "p"}]], 
       InterpretationBox[
        StyleBox[
         RowBox[{GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`plane4], {"zplus"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"z>0\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`plane4], {"zmin"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"z<0\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
        RadioButtonBar[
         Dynamic[$CellContext`plane4], {"zplus" -> "z>0", "zmin" -> "z<0"}]], 
       InputFieldBox[Dynamic[$CellContext`reA4, SaveDefinitions -> True], 
        Number,
        FieldHint->"Re[A4]",
        FieldSize->Tiny], "\<\"+\[ImaginaryI]\"\>", 
       InputFieldBox[Dynamic[$CellContext`imA4, SaveDefinitions -> True], 
        Number,
        FieldHint->"Im[A4]",
        FieldSize->Tiny]},
      {
       CheckboxBox[Dynamic[$CellContext`point5], {True, False}], 
       StyleBox["\<\"5th point\"\>",
        StripOnInput->False,
        FontFamily->"Helvetica"], 
       InputFieldBox[Dynamic[$CellContext`kx5], Number,
        FieldHint->"kx5/k0",
        FieldSize->Tiny], 
       InputFieldBox[Dynamic[$CellContext`ky5], Number,
        FieldHint->"ky5/k0",
        FieldSize->Tiny], 
       InterpretationBox[
        StyleBox[
         RowBox[{GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`pol5], {"spol"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"s\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`pol5], {"ppol"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"p\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
        RadioButtonBar[
         Dynamic[$CellContext`pol5], {"spol" -> "s", "ppol" -> "p"}]], 
       InterpretationBox[
        StyleBox[
         RowBox[{GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`plane5], {"zplus"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"z>0\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`plane5], {"zmin"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"z<0\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
        RadioButtonBar[
         Dynamic[$CellContext`plane5], {"zplus" -> "z>0", "zmin" -> "z<0"}]], 
       InputFieldBox[Dynamic[$CellContext`reA5, SaveDefinitions -> True], 
        Number,
        FieldHint->"Re[A5]",
        FieldSize->Tiny], "\<\"+\[ImaginaryI]\"\>", 
       InputFieldBox[Dynamic[$CellContext`imA5, SaveDefinitions -> True], 
        Number,
        FieldHint->"Im[A5]",
        FieldSize->Tiny]},
      {
       CheckboxBox[Dynamic[$CellContext`point6], {True, False}], 
       StyleBox["\<\"6th point\"\>",
        StripOnInput->False,
        FontFamily->"Helvetica"], 
       InputFieldBox[Dynamic[$CellContext`kx6], Number,
        FieldHint->"kx6/k0",
        FieldSize->Tiny], 
       InputFieldBox[Dynamic[$CellContext`ky6], Number,
        FieldHint->"ky6/k0",
        FieldSize->Tiny], 
       InterpretationBox[
        StyleBox[
         RowBox[{GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`pol6], {"spol"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"s\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`pol6], {"ppol"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"p\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
        RadioButtonBar[
         Dynamic[$CellContext`pol6], {"spol" -> "s", "ppol" -> "p"}]], 
       InterpretationBox[
        StyleBox[
         RowBox[{GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`plane6], {"zplus"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"z>0\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
            {
             RadioButtonBox[Dynamic[$CellContext`plane6], {"zmin"},
              DefaultBaseStyle->"RadioButtonBar"], 
             StyleBox["\<\"z<0\"\>", "RadioButtonBarLabel",
              StripOnInput->False]}
           },
           AutoDelete->False,
           
           GridBoxAlignment->{
            "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
             "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
           
           GridBoxItemSize->{
            "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
           GridBoxSpacings->{"Columns" -> {
               Offset[0.27999999999999997`], {
                Offset[0.21]}, 
               Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
               Offset[0.2], {
                Offset[0.4]}, 
               Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
        RadioButtonBar[
         Dynamic[$CellContext`plane6], {"zplus" -> "z>0", "zmin" -> "z<0"}]], 
       InputFieldBox[Dynamic[$CellContext`reA6, SaveDefinitions -> True], 
        Number,
        FieldHint->"Re[A6]",
        FieldSize->Tiny], "\<\"+\[ImaginaryI]\"\>", 
       InputFieldBox[Dynamic[$CellContext`imA6, SaveDefinitions -> True], 
        Number,
        FieldHint->"Im[A6]",
        FieldSize->Tiny]}
     },
     AutoDelete->False,
     GridBoxFrame->{"Columns" -> {{True}}, "Rows" -> {{True}}},
     GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}}],
    "Grid"]}],
  StripOnInput->False]], "Output",
 FontSize->12],

Cell[BoxData[
 DynamicBox[ToBoxes[$CellContext`result, StandardForm],
  ImageSizeCache->{836., {71., 79.}}]], "Print"],

Cell[BoxData[
 FrameBox[
  TemplateBox[{
   StyleBox["\"Read more at:\\n\"", FontFamily -> "Helvetica", FontSlant -> 
     Italic, 
     GrayLevel[0.5], StripOnInput -> False],"\"\"",StyleBox[
     ButtonBox[
      PaneSelectorBox[{
       False -> StyleBox[
         "\"M. F. Picardi, A. Zayats and F. \
Rodr\[IAcute]guez-Fortu\[NTilde]o, Amplitude and phase control of guided \
modes excitation from a single dipole source: engineering far- and near-field \
directionality, arXiv:1907.06573\"", FontFamily -> "Helvetica", FontSlant -> 
          Italic, 
          GrayLevel[0.5], StripOnInput -> False], True -> 
        StyleBox[
         "\"M. F. Picardi, A. Zayats and F. \
Rodr\[IAcute]guez-Fortu\[NTilde]o, Amplitude and phase control of guided \
modes excitation from a single dipole source: engineering far- and near-field \
directionality, arXiv:1907.06573\"", FontFamily -> "Helvetica", FontSlant -> 
          Italic, 
          GrayLevel[0], StripOnInput -> False]}, 
       Dynamic[
        CurrentValue["MouseOver"]], ImageSize -> Automatic, FrameMargins -> 
       0], ButtonFunction :> NotebookLocate[{
         URL["https://arxiv.org/abs/1907.06573"], None}], Appearance -> None, 
      Evaluator -> Automatic, Method -> "Preemptive"], StripOnInput -> False]},
   "RowDefault"],
  Background->RGBColor[0, 1, 0, 0.2],
  StripOnInput->False]], "Output",
 FontSize->12],

Cell[BoxData[
 DynamicBox[
  ToBoxes[$CellContext`A1 = $CellContext`reA1 + 
     I $CellContext`imA1; $CellContext`A2 = $CellContext`reA2 + 
     I $CellContext`imA2; $CellContext`A3 = $CellContext`reA3 + 
     I $CellContext`imA3; $CellContext`A4 = $CellContext`reA4 + 
     I $CellContext`imA4; $CellContext`A5 = $CellContext`reA5 + 
     I $CellContext`imA5; $CellContext`A6 = $CellContext`reA6 + 
     I $CellContext`imA6; Null, StandardForm],
  ImageSizeCache->{28., {0., 8.}}]], "Output",
 Editable->False,
 CellOpen->False,
 FontSize->12],

Cell[BoxData[
 DynamicBox[ToBoxes[
   If[$CellContext`point1 == False, 
    Dynamic[
     If[
      And[$CellContext`kx1 == 0, $CellContext`ky1 == 0], 
      Dynamic[
       Which[
        And[$CellContext`pol1 == "spol", $CellContext`plane1 == 
         "zplus"], $CellContext`M1 = {
         I/(8 Pi^2), 0, 0, 0, I/(8 $CellContext`c Pi^2), 0}, 
        And[$CellContext`pol1 == "spol", $CellContext`plane1 == 
         "zmin"], $CellContext`M1 = {
         I/(8 Pi^2), 0, 0, 0, -(I/(8 $CellContext`c Pi^2)), 0}, 
        And[$CellContext`pol1 == "ppol", $CellContext`plane1 == 
         "zplus"], $CellContext`M1 = {
         0, I/(8 Pi^2), 0, -(I/(8 $CellContext`c Pi^2)), 0, 0}, 
        And[$CellContext`pol1 == "ppol", $CellContext`plane1 == 
         "zmin"], $CellContext`M1 = {
         0, I/(8 Pi^2), 0, I/(8 $CellContext`c Pi^2), 0, 0}], SaveDefinitions -> 
       True], 
      Dynamic[
       Which[
        And[$CellContext`pol1 == "spol", $CellContext`plane1 == 
         "zplus"], $CellContext`M1 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EsEpp 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx1], $CellContext`ky -> $CellContext`ky1]], 
        And[$CellContext`pol1 == "spol", $CellContext`plane1 == 
         "zmin"], $CellContext`M1 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EsEpm 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx1], $CellContext`ky -> $CellContext`ky1]], 
        And[$CellContext`pol1 == "ppol", $CellContext`plane1 == 
         "zplus"], $CellContext`M1 = FullSimplify[
          Limit[
           Limit[
            Coefficient[$CellContext`EpEpp 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx1], $CellContext`ky -> $CellContext`ky1]], 
        And[$CellContext`pol1 == "ppol", $CellContext`plane1 == 
         "zmin"], $CellContext`M1 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EpEpm 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx1], $CellContext`ky -> $CellContext`ky1]]], SaveDefinitions -> 
       True]]], $CellContext`M1 = {0, 0, 0, 0, 0, 0}; $CellContext`A1 = 0], 
   StandardForm],
  ImageSizeCache->{1021.4, {22., 46.}},
  Initialization:>{$CellContext`point1 = 
    False, $CellContext`EsEpp = ((I/
       8) $CellContext`k1^2 (-((($CellContext`kx $CellContext`kz1 \
$CellContext`mx)/($CellContext`k1 $CellContext`kt) + ($CellContext`ky \
$CellContext`kz1 $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 Pi^2), $CellContext`k1 = 
    1, $CellContext`kz1 = 
    Sqrt[$CellContext`k1^2 - $CellContext`kt^2], $CellContext`kt = 
    Sqrt[$CellContext`kx^2 + $CellContext`ky^2], $CellContext`EsEpm = ((I/
       8) $CellContext`k1^2 (-((-(($CellContext`kx $CellContext`kz1 \
$CellContext`mx)/($CellContext`k1 $CellContext`kt)) - ($CellContext`ky \
$CellContext`kz1 $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpp = ((I/
       8) $CellContext`k1^2 ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c + ($CellContext`kx \
$CellContext`kz1 $CellContext`px)/($CellContext`k1 $CellContext`kt) + \
($CellContext`ky $CellContext`kz1 $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpm = ((I/
       8) $CellContext`k1^2 ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c - ($CellContext`kx \
$CellContext`kz1 $CellContext`px)/($CellContext`k1 $CellContext`kt) - \
($CellContext`ky $CellContext`kz1 $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2), $CellContext`A1 = 0}]], "Output",
 Editable->False,
 CellOpen->False,
 FontSize->12],

Cell[BoxData[
 DynamicBox[ToBoxes[
   If[$CellContext`point2 == False, 
    Dynamic[
     If[
      And[$CellContext`kx2 == 0, $CellContext`ky2 == 0], 
      Dynamic[
       Which[
        And[$CellContext`pol2 == "spol", $CellContext`plane2 == 
         "zplus"], $CellContext`M2 = {
         I/(8 Pi^2), 0, 0, 0, I/(8 $CellContext`c Pi^2), 0}, 
        And[$CellContext`pol2 == "spol", $CellContext`plane2 == 
         "zmin"], $CellContext`M2 = {
         I/(8 Pi^2), 0, 0, 0, -(I/(8 $CellContext`c Pi^2)), 0}, 
        And[$CellContext`pol2 == "ppol", $CellContext`plane2 == 
         "zplus"], $CellContext`M2 = {
         0, I/(8 Pi^2), 0, -(I/(8 $CellContext`c Pi^2)), 0, 0}, 
        And[$CellContext`pol2 == "ppol", $CellContext`plane2 == 
         "zmin"], $CellContext`M2 = {
         0, I/(8 Pi^2), 0, I/(8 $CellContext`c Pi^2), 0, 0}], SaveDefinitions -> 
       True], 
      Dynamic[
       Which[
        And[$CellContext`pol2 == "spol", $CellContext`plane2 == 
         "zplus"], $CellContext`M2 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EsEpp 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx2], $CellContext`ky -> $CellContext`ky2]], 
        And[$CellContext`pol2 == "spol", $CellContext`plane2 == 
         "zmin"], $CellContext`M2 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EsEpm 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx2], $CellContext`ky -> $CellContext`ky2]], 
        And[$CellContext`pol2 == "ppol", $CellContext`plane2 == 
         "zplus"], $CellContext`M2 = FullSimplify[
          Limit[
           Limit[
            Coefficient[$CellContext`EpEpp 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx2], $CellContext`ky -> $CellContext`ky2]], 
        And[$CellContext`pol2 == "ppol", $CellContext`plane2 == 
         "zmin"], $CellContext`M2 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EpEpm 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx2], $CellContext`ky -> $CellContext`ky2]]], SaveDefinitions -> 
       True]]], $CellContext`M2 = {0, 0, 0, 0, 0, 0}; $CellContext`A2 = 0], 
   StandardForm],
  ImageSizeCache->{7., {0., 8.}},
  Initialization:>{$CellContext`point2 = 
    True, $CellContext`EsEpp = ((I/
       8) $CellContext`k1^2 (-((($CellContext`kx $CellContext`kz1 \
$CellContext`mx)/($CellContext`k1 $CellContext`kt) + ($CellContext`ky \
$CellContext`kz1 $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 Pi^2), $CellContext`k1 = 
    1, $CellContext`kz1 = 
    Sqrt[$CellContext`k1^2 - $CellContext`kt^2], $CellContext`kt = 
    Sqrt[$CellContext`kx^2 + $CellContext`ky^2], $CellContext`EsEpm = ((I/
       8) $CellContext`k1^2 (-((-(($CellContext`kx $CellContext`kz1 \
$CellContext`mx)/($CellContext`k1 $CellContext`kt)) - ($CellContext`ky \
$CellContext`kz1 $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpp = ((I/
       8) $CellContext`k1^2 ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c + ($CellContext`kx \
$CellContext`kz1 $CellContext`px)/($CellContext`k1 $CellContext`kt) + \
($CellContext`ky $CellContext`kz1 $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpm = ((I/
       8) $CellContext`k1^2 ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c - ($CellContext`kx \
$CellContext`kz1 $CellContext`px)/($CellContext`k1 $CellContext`kt) - \
($CellContext`ky $CellContext`kz1 $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2)}]], "Output",
 Editable->False,
 CellOpen->False,
 FontSize->12],

Cell[BoxData[
 DynamicBox[ToBoxes[
   If[$CellContext`point3 == False, 
    Dynamic[
     If[
      And[$CellContext`kx3 == 0, $CellContext`ky3 == 0], 
      Dynamic[
       Which[
        And[$CellContext`pol3 == "spol", $CellContext`plane3 == 
         "zplus"], $CellContext`M3 = {
         I/(8 Pi^2), 0, 0, 0, I/(8 $CellContext`c Pi^2), 0}, 
        And[$CellContext`pol3 == "spol", $CellContext`plane3 == 
         "zmin"], $CellContext`M3 = {
         I/(8 Pi^2), 0, 0, 0, -(I/(8 $CellContext`c Pi^2)), 0}, 
        And[$CellContext`pol3 == "ppol", $CellContext`plane3 == 
         "zplus"], $CellContext`M3 = {
         0, I/(8 Pi^2), 0, -(I/(8 $CellContext`c Pi^2)), 0, 0}, 
        And[$CellContext`pol3 == "ppol", $CellContext`plane3 == 
         "zmin"], $CellContext`M3 = {
         0, I/(8 Pi^2), 0, I/(8 $CellContext`c Pi^2), 0, 0}], SaveDefinitions -> 
       True], 
      Dynamic[
       Which[
        And[$CellContext`pol3 == "spol", $CellContext`plane3 == 
         "zplus"], $CellContext`M3 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EsEpp 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx3], $CellContext`ky -> $CellContext`ky3]], 
        And[$CellContext`pol3 == "spol", $CellContext`plane3 == 
         "zmin"], $CellContext`M3 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EsEpm 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx3], $CellContext`ky -> $CellContext`ky3]], 
        And[$CellContext`pol3 == "ppol", $CellContext`plane3 == 
         "zplus"], $CellContext`M3 = FullSimplify[
          Limit[
           Limit[
            Coefficient[$CellContext`EpEpp 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx3], $CellContext`ky -> $CellContext`ky3]], 
        And[$CellContext`pol3 == "ppol", $CellContext`plane3 == 
         "zmin"], $CellContext`M3 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EpEpm 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx3], $CellContext`ky -> $CellContext`ky3]]], SaveDefinitions -> 
       True]]], $CellContext`M3 = {0, 0, 0, 0, 0, 0}; $CellContext`A3 = 0], 
   StandardForm],
  ImageSizeCache->{7., {0., 8.}},
  Initialization:>{$CellContext`point3 = 
    True, $CellContext`EsEpp = ((I/
       8) $CellContext`k1^2 (-((($CellContext`kx $CellContext`kz1 \
$CellContext`mx)/($CellContext`k1 $CellContext`kt) + ($CellContext`ky \
$CellContext`kz1 $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 Pi^2), $CellContext`k1 = 
    1, $CellContext`kz1 = 
    Sqrt[$CellContext`k1^2 - $CellContext`kt^2], $CellContext`kt = 
    Sqrt[$CellContext`kx^2 + $CellContext`ky^2], $CellContext`EsEpm = ((I/
       8) $CellContext`k1^2 (-((-(($CellContext`kx $CellContext`kz1 \
$CellContext`mx)/($CellContext`k1 $CellContext`kt)) - ($CellContext`ky \
$CellContext`kz1 $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpp = ((I/
       8) $CellContext`k1^2 ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c + ($CellContext`kx \
$CellContext`kz1 $CellContext`px)/($CellContext`k1 $CellContext`kt) + \
($CellContext`ky $CellContext`kz1 $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpm = ((I/
       8) $CellContext`k1^2 ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c - ($CellContext`kx \
$CellContext`kz1 $CellContext`px)/($CellContext`k1 $CellContext`kt) - \
($CellContext`ky $CellContext`kz1 $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2)}]], "Output",
 Editable->False,
 CellOpen->False,
 FontSize->12],

Cell[BoxData[
 DynamicBox[ToBoxes[
   If[$CellContext`point4 == False, 
    Dynamic[
     If[
      And[$CellContext`kx4 == 0, $CellContext`ky4 == 0], 
      Dynamic[
       Which[
        And[$CellContext`pol4 == "spol", $CellContext`plane4 == 
         "zplus"], $CellContext`M4 = {
         I/(8 Pi^2), 0, 0, 0, I/(8 $CellContext`c Pi^2), 0}, 
        And[$CellContext`pol4 == "spol", $CellContext`plane4 == 
         "zmin"], $CellContext`M4 = {
         I/(8 Pi^2), 0, 0, 0, -(I/(8 $CellContext`c Pi^2)), 0}, 
        And[$CellContext`pol4 == "ppol", $CellContext`plane4 == 
         "zplus"], $CellContext`M4 = {
         0, I/(8 Pi^2), 0, -(I/(8 $CellContext`c Pi^2)), 0, 0}, 
        And[$CellContext`pol4 == "ppol", $CellContext`plane4 == 
         "zmin"], $CellContext`M4 = {
         0, I/(8 Pi^2), 0, I/(8 $CellContext`c Pi^2), 0, 0}], SaveDefinitions -> 
       True], 
      Dynamic[
       Which[
        And[$CellContext`pol4 == "spol", $CellContext`plane4 == 
         "zplus"], $CellContext`M4 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EsEpp 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx4], $CellContext`ky -> $CellContext`ky4]], 
        And[$CellContext`pol4 == "spol", $CellContext`plane4 == 
         "zmin"], $CellContext`M4 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EsEpm 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx4], $CellContext`ky -> $CellContext`ky4]], 
        And[$CellContext`pol4 == "ppol", $CellContext`plane4 == 
         "zplus"], $CellContext`M4 = FullSimplify[
          Limit[
           Limit[
            Coefficient[$CellContext`EpEpp 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx4], $CellContext`ky -> $CellContext`ky4]], 
        And[$CellContext`pol4 == "ppol", $CellContext`plane4 == 
         "zmin"], $CellContext`M4 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EpEpm 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx4], $CellContext`ky -> $CellContext`ky4]]], SaveDefinitions -> 
       True]]], $CellContext`M4 = {0, 0, 0, 0, 0, 0}; $CellContext`A4 = 0], 
   StandardForm],
  ImageSizeCache->{7., {0., 8.}},
  Initialization:>{$CellContext`point4 = True, $CellContext`pol4 = 
    "spol", $CellContext`plane4 = 
    "zplus", $CellContext`EsEpp = ((I/
       8) $CellContext`k1^2 (-((($CellContext`kx $CellContext`kz1 \
$CellContext`mx)/($CellContext`k1 $CellContext`kt) + ($CellContext`ky \
$CellContext`kz1 $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 Pi^2), $CellContext`k1 = 
    1, $CellContext`kz1 = 
    Sqrt[$CellContext`k1^2 - $CellContext`kt^2], $CellContext`kt = 
    Sqrt[$CellContext`kx^2 + $CellContext`ky^2], $CellContext`EsEpm = ((I/
       8) $CellContext`k1^2 (-((-(($CellContext`kx $CellContext`kz1 \
$CellContext`mx)/($CellContext`k1 $CellContext`kt)) - ($CellContext`ky \
$CellContext`kz1 $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpp = ((I/
       8) $CellContext`k1^2 ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c + ($CellContext`kx \
$CellContext`kz1 $CellContext`px)/($CellContext`k1 $CellContext`kt) + \
($CellContext`ky $CellContext`kz1 $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpm = ((I/
       8) $CellContext`k1^2 ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c - ($CellContext`kx \
$CellContext`kz1 $CellContext`px)/($CellContext`k1 $CellContext`kt) - \
($CellContext`ky $CellContext`kz1 $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2)}]], "Output",
 Editable->False,
 CellOpen->False,
 FontSize->12],

Cell[BoxData[
 DynamicBox[ToBoxes[
   If[$CellContext`point5 == False, 
    Dynamic[
     If[
      And[$CellContext`kx5 == 0, $CellContext`ky5 == 0], 
      Dynamic[
       Which[
        And[$CellContext`pol5 == "spol", $CellContext`plane5 == 
         "zplus"], $CellContext`M5 = {
         I/(8 Pi^2), 0, 0, 0, I/(8 $CellContext`c Pi^2), 0}, 
        And[$CellContext`pol5 == "spol", $CellContext`plane5 == 
         "zmin"], $CellContext`M5 = {
         I/(8 Pi^2), 0, 0, 0, -(I/(8 $CellContext`c Pi^2)), 0}, 
        And[$CellContext`pol5 == "ppol", $CellContext`plane5 == 
         "zplus"], $CellContext`M5 = {
         0, I/(8 Pi^2), 0, -(I/(8 $CellContext`c Pi^2)), 0, 0}, 
        And[$CellContext`pol5 == "ppol", $CellContext`plane5 == 
         "zmin"], $CellContext`M5 = {
         0, I/(8 Pi^2), 0, I/(8 $CellContext`c Pi^2), 0, 0}], SaveDefinitions -> 
       True], 
      Dynamic[
       Which[
        And[$CellContext`pol5 == "spol", $CellContext`plane5 == 
         "zplus"], $CellContext`M5 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EsEpp 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx5], $CellContext`ky -> $CellContext`ky5]], 
        And[$CellContext`pol5 == "spol", $CellContext`plane5 == 
         "zmin"], $CellContext`M5 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EsEpm 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx5], $CellContext`ky -> $CellContext`ky5]], 
        And[$CellContext`pol5 == "ppol", $CellContext`plane5 == 
         "zplus"], $CellContext`M5 = FullSimplify[
          Limit[
           Limit[
            Coefficient[$CellContext`EpEpp 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx5], $CellContext`ky -> $CellContext`ky5]], 
        And[$CellContext`pol5 == "ppol", $CellContext`plane5 == 
         "zmin"], $CellContext`M5 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EpEpm 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx5], $CellContext`ky -> $CellContext`ky5]]], SaveDefinitions -> 
       True]]], $CellContext`M5 = {0, 0, 0, 0, 0, 0}; $CellContext`A5 = 0], 
   StandardForm],
  ImageSizeCache->{7., {0., 8.}},
  Initialization:>{$CellContext`point5 = True, $CellContext`pol5 = 
    "spol", $CellContext`plane5 = 
    "zplus", $CellContext`EsEpp = ((I/
       8) $CellContext`k1^2 (-((($CellContext`kx $CellContext`kz1 \
$CellContext`mx)/($CellContext`k1 $CellContext`kt) + ($CellContext`ky \
$CellContext`kz1 $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 Pi^2), $CellContext`k1 = 
    1, $CellContext`kz1 = 
    Sqrt[$CellContext`k1^2 - $CellContext`kt^2], $CellContext`kt = 
    Sqrt[$CellContext`kx^2 + $CellContext`ky^2], $CellContext`EsEpm = ((I/
       8) $CellContext`k1^2 (-((-(($CellContext`kx $CellContext`kz1 \
$CellContext`mx)/($CellContext`k1 $CellContext`kt)) - ($CellContext`ky \
$CellContext`kz1 $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpp = ((I/
       8) $CellContext`k1^2 ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c + ($CellContext`kx \
$CellContext`kz1 $CellContext`px)/($CellContext`k1 $CellContext`kt) + \
($CellContext`ky $CellContext`kz1 $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpm = ((I/
       8) $CellContext`k1^2 ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c - ($CellContext`kx \
$CellContext`kz1 $CellContext`px)/($CellContext`k1 $CellContext`kt) - \
($CellContext`ky $CellContext`kz1 $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2)}]], "Output",
 Editable->False,
 CellOpen->False,
 FontSize->12],

Cell[BoxData[
 DynamicBox[ToBoxes[
   If[$CellContext`point6 == False, 
    Dynamic[
     If[
      And[$CellContext`kx6 == 0, $CellContext`ky6 == 0], 
      Dynamic[
       Which[
        And[$CellContext`pol6 == "spol", $CellContext`plane6 == 
         "zplus"], $CellContext`M6 = {
         I/(8 Pi^2), 0, 0, 0, I/(8 $CellContext`c Pi^2), 0}, 
        And[$CellContext`pol6 == "spol", $CellContext`plane6 == 
         "zmin"], $CellContext`M6 = {
         I/(8 Pi^2), 0, 0, 0, -(I/(8 $CellContext`c Pi^2)), 0}, 
        And[$CellContext`pol6 == "ppol", $CellContext`plane6 == 
         "zplus"], $CellContext`M6 = {
         0, I/(8 Pi^2), 0, -(I/(8 $CellContext`c Pi^2)), 0, 0}, 
        And[$CellContext`pol6 == "ppol", $CellContext`plane6 == 
         "zmin"], $CellContext`M6 = {
         0, I/(8 Pi^2), 0, I/(8 $CellContext`c Pi^2), 0, 0}], SaveDefinitions -> 
       True], 
      Dynamic[
       Which[
        And[$CellContext`pol6 == "spol", $CellContext`plane6 == 
         "zplus"], $CellContext`M6 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EsEpp 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx6], $CellContext`ky -> $CellContext`ky6]], 
        And[$CellContext`pol6 == "spol", $CellContext`plane6 == 
         "zmin"], $CellContext`M6 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EsEpm 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx6], $CellContext`ky -> $CellContext`ky6]], 
        And[$CellContext`pol6 == "ppol", $CellContext`plane6 == 
         "zplus"], $CellContext`M6 = FullSimplify[
          Limit[
           Limit[
            Coefficient[$CellContext`EpEpp 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx6], $CellContext`ky -> $CellContext`ky6]], 
        And[$CellContext`pol6 == "ppol", $CellContext`plane6 == 
         "zmin"], $CellContext`M6 = FullSimplify[
          Limit[
           Limit[
            
            Coefficient[$CellContext`EpEpm 
             Sqrt[$CellContext`k1^2 - ($CellContext`kx^2 + \
$CellContext`ky^2)], {$CellContext`px, $CellContext`py, $CellContext`pz, \
$CellContext`mx, $CellContext`my, $CellContext`mz}], $CellContext`kx -> \
$CellContext`kx6], $CellContext`ky -> $CellContext`ky6]]], SaveDefinitions -> 
       True]]], $CellContext`M6 = {0, 0, 0, 0, 0, 0}; $CellContext`A6 = 0], 
   StandardForm],
  ImageSizeCache->{7., {0., 8.}},
  Initialization:>{$CellContext`point6 = True, $CellContext`pol6 = 
    "spol", $CellContext`plane6 = 
    "zplus", $CellContext`EsEpp = ((I/
       8) (-((($CellContext`kx 
            Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`ky 
            Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`my)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`mz)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c) - \
($CellContext`ky $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`kx \
$CellContext`py)/Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] Pi^2), $CellContext`k1 = 
    1, $CellContext`EsEpm = ((I/
       8) (-((-(($CellContext`kx 
             Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`mx)/
            Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) - ($CellContext`ky 
           Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`my)/
          
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`mz)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c) - \
($CellContext`ky $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`kx \
$CellContext`py)/Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EpEpp = ((I/
       8) ((-(($CellContext`ky $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) + ($CellContext`kx \
$CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c + \
($CellContext`kx 
         Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`px)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`ky 
         Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`py)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`pz)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EpEpm = ((I/
       8) ((-(($CellContext`ky $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) + ($CellContext`kx \
$CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c - \
($CellContext`kx 
        Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] - ($CellContext`ky 
        Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`py)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`pz)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] Pi^2), $CellContext`A6 = 
    0}]], "Output",
 Editable->False,
 CellOpen->False,
 FontSize->12],

Cell[BoxData[
 DynamicBox[
  ToBoxes[$CellContext`nulsol = 
   NullSpace[{$CellContext`M1, $CellContext`M2, $CellContext`M3, \
$CellContext`M4, $CellContext`M5, $CellContext`M6}], StandardForm],
  ImageSizeCache->{1111.8, {12.1, 46.}}]], "Output",
 Editable->False,
 CellOpen->False,
 FontSize->12],

Cell[BoxData[
 DynamicBox[ToBoxes[$CellContext`result = Check[
     Row[{
       Style[
        MatrixForm[{
         "\!\(\*SubscriptBox[\(p\), \(x\)]\)", 
          "\!\(\*SubscriptBox[\(p\), \(y\)]\)", 
          "\!\(\*SubscriptBox[\(p\), \(z\)]\)", 
          "\!\(\*SubscriptBox[\(m\), \(x\)]\)", 
          "\!\(\*SubscriptBox[\(m\), \(y\)]\)", 
          "\!\(\*SubscriptBox[\(m\), \(z\)]\)"}], FontFamily -> "Helvetica", 
        FontSize -> 14], 
       Style["=", FontFamily -> "Helvetica", FontSize -> 14], 
       Style[
        MatrixForm[
         LinearSolve[{$CellContext`M1, $CellContext`M2, $CellContext`M3, \
$CellContext`M4, $CellContext`M5, $CellContext`M6}, 
          
          FullSimplify[{$CellContext`A1, $CellContext`A2, $CellContext`A3, \
$CellContext`A4, $CellContext`A5, $CellContext`A6}]]], FontFamily -> 
        "Helvetica", FontSize -> 14], 
       Style[
        Dynamic[$CellContext`plus, SaveDefinitions -> True], FontFamily -> 
        "Helvetica", FontSize -> 14], 
       Style[
        Dynamic[
         Simplify[$CellContext`ToPrint], SaveDefinitions -> True], FontFamily -> 
        "Helvetica", FontSize -> 14], 
       Style[" ", FontFamily -> "Helvetica", FontSize -> 64], 
       Style[
        Dynamic[$CellContext`with, SaveDefinitions -> True], FontFamily -> 
        "Helvetica", FontSize -> 14], 
       Style[" ", FontFamily -> "Helvetica", FontSize -> 14], 
       Style[
        Dynamic[$CellContext`coef, SaveDefinitions -> True], FontFamily -> 
        "Helvetica", FontSize -> 14], 
       Style[" ", FontFamily -> "Helvetica", FontSize -> 14], 
       Style[
        Dynamic[$CellContext`compl, SaveDefinitions -> True], FontFamily -> 
        "Helvetica", FontSize -> 14]}], 
     Style[
     "The system has no solution. Check that all the inputs are compatible.", 
      FontFamily -> "Helvetica", FontSize -> 14]], StandardForm],
  ImageSizeCache->{836., {71., 79.}}]], "Output",
 Editable->False,
 CellOpen->False,
 FontSize->12]
}, Open  ]]
},
WindowSize->{1920, 1117},
WindowMargins->{{-8, Automatic}, {Automatic, -8}},
Visible->True,
ScrollingOptions->{"VerticalScrollRange"->Fit},
ShowCellBracket->Automatic,
CellContext->Notebook,
TrackCellChangeTimes->False,
FrontEndVersion->"11.0 for Microsoft Windows (64-bit) (July 28, 2016)",
StyleDefinitions->"Default.nb"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[CellGroupData[{
Cell[1486, 35, 96253, 2318, 19, "Input",
 CellOpen->False],
Cell[97742, 2355, 4178, 99, 19, "Output",
 CellOpen->False],
Cell[101923, 2456, 2035, 42, 19, "Output",
 CellOpen->False],
Cell[103961, 2500, 1221, 34, 19, "Output",
 CellOpen->False],
Cell[105185, 2536, 431, 12, 44, "Output"],
Cell[105619, 2550, 31204, 771, 348, "Output"],
Cell[136826, 3323, 118, 2, 160, "Print"],
Cell[136947, 3327, 1385, 30, 60, "Output"],
Cell[138335, 3359, 545, 12, 19, "Output",
 CellOpen->False],
Cell[138883, 3373, 5222, 103, 19, "Output",
 CellOpen->False],
Cell[144108, 3478, 5194, 103, 19, "Output",
 CellOpen->False],
Cell[149305, 3583, 5194, 103, 19, "Output",
 CellOpen->False],
Cell[154502, 3688, 5258, 104, 19, "Output",
 CellOpen->False],
Cell[159763, 3794, 5258, 104, 19, "Output",
 CellOpen->False],
Cell[165024, 3900, 6072, 121, 19, "Output",
 CellOpen->False],
Cell[171099, 4023, 298, 8, 19, "Output",
 CellOpen->False],
Cell[171400, 4033, 1996, 46, 19, "Output",
 CellOpen->False]
}, Open  ]]
}
]
*)

(* NotebookSignature 0wDVDenAMjFkRBKDmjS2Q#Sn *)
