/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    Definition file                                  //
//                                                     //
/////////////////////////////////////////////////////////

{$B-} {- Complete Boolean Evaluation }
{$R-} {- Range-Checking }
{$V-} {- Var-String Checking }
{$T-} {- Typed @ operator }
{$X+} {- Extended syntax }
{$P+} {- Open string params }
{$J+} {- Writeable structured consts }
{$H+} {- Use long strings by default }

{$DEFINE FG_LIB_170}

{$IFDEF VER120} { Borland DELPHI 4.0 }
  {$DEFINE FG_DELPHI}
  {$DEFINE FG_D4_VER}
  {$DEFINE FG_D4}
{$ENDIF}

{$IFDEF VER125} { Borland C++Builder 4.0 }
  {$DEFINE FG_CBUILDER}
  {$DEFINE FG_C4_VER}
  {$DEFINE FG_C4}
{$ENDIF}

{$IFDEF VER130} { Borland DELPHI/C++Builder 5.0 }
  {$DEFINE FG_D5}
  {$DEFINE FG_D4}
  {$DEFINE FG_C4}
  {$IFDEF BCB}
    {$DEFINE FG_CBUILDER}
    {$DEFINE FG_C5_VER}
    {$DEFINE FG_C5}
  {$ELSE}
    {$DEFINE FG_DELPHI}
    {$DEFINE FG_D5_VER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140} { Borland DELPHI/C++Builder 6.0 }
  {$DEFINE FG_D6}
  {$DEFINE FG_D5}
  {$DEFINE FG_D4}
  {$DEFINE FG_C5}
  {$DEFINE FG_C4}
  {$IFDEF BCB}
    {$DEFINE FG_CBUILDER}
    {$DEFINE FG_C6_VER}
    {$DEFINE FG_C6}
  {$ELSE}
    {$DEFINE FG_DELPHI}
    {$DEFINE FG_D6_VER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER150} { Borland DELPHI/C++Builder 7.0 }
  {$DEFINE FG_D7}
  {$DEFINE FG_D6}
  {$DEFINE FG_D5}
  {$DEFINE FG_D4}
  {$DEFINE FG_C6}
  {$DEFINE FG_C5}
  {$DEFINE FG_C4}
  {$IFDEF BCB}
    {$DEFINE FG_CBUILDER}
    {$DEFINE FG_C7_VER}
    {$DEFINE FG_C7}
  {$ELSE}
    {$DEFINE FG_DELPHI}
    {$DEFINE FG_D7_VER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER160} { Borland DELPHI/C++Builder 8.0 }

  Version not supported

{$ENDIF}

{$IFDEF VER170} { Borland DELPHI/C++Builder 9.0 (Studio 2005) }
  {$DEFINE FG_D9}
  {$DEFINE FG_D7}
  {$DEFINE FG_D6}
  {$DEFINE FG_D5}
  {$DEFINE FG_D4}
  {$DEFINE FG_C7}
  {$DEFINE FG_C6}
  {$DEFINE FG_C5}
  {$DEFINE FG_C4}
  {$IFDEF BCB}
    {$DEFINE FG_CBUILDER}
    {$DEFINE FG_C9_VER}
    {$DEFINE FG_C9}
  {$ELSE}
    {$DEFINE FG_DELPHI}
    {$DEFINE FG_D9_VER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER180} { Borland DELPHI/C++Builder 10.0 (Studio 2006) }
  {$DEFINE FG_D10}
  {$DEFINE FG_D9}
  {$DEFINE FG_D7}
  {$DEFINE FG_D6}
  {$DEFINE FG_D5}
  {$DEFINE FG_D4}
  {$DEFINE FG_C9}
  {$DEFINE FG_C7}
  {$DEFINE FG_C6}
  {$DEFINE FG_C5}
  {$DEFINE FG_C4}
  {$IFDEF BCB}
    {$DEFINE FG_CBUILDER}
    {$DEFINE FG_C10_VER}
    {$DEFINE FG_C10}
  {$ELSE}
    {$DEFINE FG_DELPHI}
    {$DEFINE FG_D10_VER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER200} { Borland DELPHI/C++Builder 12.0 (Studio 2009) }
  {$DEFINE FG_D12}
  {$DEFINE FG_D10}
  {$DEFINE FG_D9}
  {$DEFINE FG_D7}
  {$DEFINE FG_D6}
  {$DEFINE FG_D5}
  {$DEFINE FG_D4}
  {$DEFINE FG_C10}
  {$DEFINE FG_C9}
  {$DEFINE FG_C7}
  {$DEFINE FG_C6}
  {$DEFINE FG_C5}
  {$DEFINE FG_C4}
  {$IFDEF BCB}
    {$DEFINE FG_CBUILDER}
    {$DEFINE FG_C12_VER}
    {$DEFINE FG_C12}
  {$ELSE}
    {$DEFINE FG_DELPHI}
    {$DEFINE FG_D12_VER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER210} { Borland DELPHI/C++Builder 14.0 (Studio 2010) }
  {$DEFINE FG_D14}
  {$DEFINE FG_D12}
  {$DEFINE FG_D10}
  {$DEFINE FG_D9}
  {$DEFINE FG_D7}
  {$DEFINE FG_D6}
  {$DEFINE FG_D5}
  {$DEFINE FG_D4}
  {$DEFINE FG_C10}
  {$DEFINE FG_C9}
  {$DEFINE FG_C7}
  {$DEFINE FG_C6}
  {$DEFINE FG_C5}
  {$DEFINE FG_C4}
  {$IFDEF BCB}
    {$DEFINE FG_CBUILDER}
    {$DEFINE FG_C14_VER}
    {$DEFINE FG_C14}
  {$ELSE}
    {$DEFINE FG_DELPHI}
    {$DEFINE FG_D14_VER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER230}
  {$DEFINE VER220}
{$ENDIF} 

{$IFDEF VER230}
  {$DEFINE VER220}
{$ENDIF} 

{$IFDEF VER240}
  {$DEFINE VER220}
{$ENDIF} 

{$IFDEF VER250}
  {$DEFINE VER220}
{$ENDIF} 

{$IFDEF VER260}
  {$DEFINE VER220}
{$ENDIF} 

{$IFDEF VER270}
  {$DEFINE VER220}
{$ENDIF} 

{$IFDEF VER280}
  {$DEFINE VER220}
{$ENDIF} 

{$IFDEF VER330}
  {$DEFINE VER220}
  {$DEFINE VER330}
{$ENDIF}

{$IFDEF VER220} { Borland DELPHI/C++Builder 15.0 (Studio XE) }
  {$DEFINE FG_D15}
  {$DEFINE FG_D14}
  {$DEFINE FG_D12}
  {$DEFINE FG_D10}
  {$DEFINE FG_D9}
  {$DEFINE FG_D7}
  {$DEFINE FG_D6}
  {$DEFINE FG_D5}
  {$DEFINE FG_D4}
  {$DEFINE FG_C10}
  {$DEFINE FG_C9}
  {$DEFINE FG_C7}
  {$DEFINE FG_C6}
  {$DEFINE FG_C5}
  {$DEFINE FG_C4}
  {$IFDEF BCB}
    {$DEFINE FG_CBUILDER}
    {$DEFINE FG_C15_VER}
    {$DEFINE FG_C15}
  {$ELSE}
    {$DEFINE FG_DELPHI}
    {$DEFINE FG_D15_VER}
  {$ENDIF}
{$ENDIF}

{$IFNDEF FG_DELPHI}
 {$IFNDEF FG_CBUILDER}
  {$IFNDEF VER80}            { DELPHI 1.0     }
   {$IFNDEF VER90}           { DELPHI 2.0     }
    {$IFNDEF VER93}          { C++Builder 1.0 }
     {$IFNDEF VER100}        { DELPHI 3.0     }
      {$IFNDEF VER110}       { C++Builder 3.0 }
       { Borland DELPHI/C++Builder higher 6.0 }
       {$DEFINE FG_UNKNOWN_VER}
       {$DEFINE FG_D7} 	
       {$DEFINE FG_D6}
       {$DEFINE FG_D5}
       {$DEFINE FG_D4}
       {$DEFINE FG_C6}
       {$DEFINE FG_C5}
       {$DEFINE FG_C4}
      {$ENDIF}
     {$ENDIF}
    {$ENDIF}
   {$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF FG_CBUILDER}
// {$ObjExportAll On}  { <- needed for compatibility with run-time packages in C++Builder 3+ }
{$ENDIF}


{ // Directive STDFLEXCTRLS turn on/off standard flex controls ////////////// }
{ // in FlexControls unit /////////////////////////////////////////////////// }
{$DEFINE STDFLEXCTRLS}

{ // Directive STDFILEFORMATS includes/excludes additional file formats //// }
{ // for import/export operations (resides in FileFormats subfolder //////// } 
{$DEFINE STDFILEFORMATS}

{ // Directive USE_SYS_GRADIENT turn on GDI gradient painting ////////////// }
{ // (need Win98 or higher) //////////////////////////////////////////////// }
{$DEFINE USE_SYS_GRADIENT}
