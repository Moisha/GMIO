{
  Библиотека дополнительных компонентов

  Директивы компилятора, определение версии Delphi

  Роман М. Мочалов
  E-mail: roman@sar.nnov.ru

  Исправлено: Компанец Илья, iluha@convex.ru
}

{$BOOLEVAL OFF}       // Complete Boolean expression evaluation
{$EXTENDEDSYNTAX ON}  // Delphi's extended syntax
{$LONGSTRINGS ON}     // Long strings
{$OPENSTRINGS ON}     // Open strings parameters
{$IMPORTEDDATA ON}    // iluha

  {$DEFINE EX_D2006}    // DELPHI/C++Builder 10.0 (Studio 2006)
  {$DEFINE EX_D2006_UP}
  {$DEFINE EX_D9_UP}
  {$DEFINE EX_D7_UP}
  {$DEFINE EX_D6_UP}
  {$DEFINE EX_D5_UP}
  {$DEFINE EX_D4_UP}
  {$DEFINE EX_D3_UP}
  {$DEFINE EX_D2_UP}


{$IFDEF VER180}
  {$DEFINE EX_D2006}    // DELPHI/C++Builder 10.0 (Studio 2006)
  {$DEFINE EX_D2006_UP}
  {$DEFINE EX_D9_UP}
  {$DEFINE EX_D7_UP}
  {$DEFINE EX_D6_UP}
  {$DEFINE EX_D5_UP}
  {$DEFINE EX_D4_UP}
  {$DEFINE EX_D3_UP}
  {$DEFINE EX_D2_UP}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE EX_D9}    // DELPHI/C++Builder 9.0 (Studio 2005)
  {$DEFINE EX_D9_UP}
  {$DEFINE EX_D7_UP}
  {$DEFINE EX_D6_UP}
  {$DEFINE EX_D5_UP}
  {$DEFINE EX_D4_UP}
  {$DEFINE EX_D3_UP}
  {$DEFINE EX_D2_UP}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE EX_D7}    // DELPHI/C++Builder 7.0
  {$DEFINE EX_D7_UP}
  {$DEFINE EX_D6_UP}
  {$DEFINE EX_D5_UP}
  {$DEFINE EX_D4_UP}
  {$DEFINE EX_D3_UP}
  {$DEFINE EX_D2_UP}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE EX_D6_UP}
  {$DEFINE EX_D5_UP}
  {$DEFINE EX_D4_UP}
  {$DEFINE EX_D3_UP}
  {$DEFINE EX_D2_UP}
  {$IFDEF BCB}
    {$DEFINE EX_CB6}  // C++ Builder 6.0
    {$DEFINE EX_CB6_UP}
    {$DEFINE EX_CB5_UP}
    {$DEFINE EX_CB4_UP}
    {$DEFINE EX_CB3_UP}
    {$DEFINE EX_CB1_UP}
    {$ObjExportAll On}  { <- needed for compatibility with run-time packages in C++Builder 3+ }
  {$ELSE}
    {$DEFINE EX_D6}    // Delphi 6.0
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE EX_D5_UP}
  {$DEFINE EX_D4_UP}
  {$DEFINE EX_D3_UP}
  {$DEFINE EX_D2_UP}
  {$IFDEF BCB}
    {$DEFINE EX_CB5}  // C++ Builder 5.0
    {$DEFINE EX_CB5_UP}
    {$DEFINE EX_CB4_UP}
    {$DEFINE EX_CB3_UP}
    {$DEFINE EX_CB1_UP}
  {$ELSE}
    {$DEFINE EX_D5}     // Delphi 5.0
  {$ENDIF}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE EX_D4}     // Delphi 4.0
  {$DEFINE EX_D4_UP}
  {$DEFINE EX_D3_UP}
  {$DEFINE EX_D2_UP}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE EX_D3}     // Delphi 3.0
  {$DEFINE EX_D3_UP}
  {$DEFINE EX_D2_UP}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE EX_D2}     // Delphi 2.0
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE EX_CB4}    // C++ Builder 4.0
  {$DEFINE EX_CB4_UP}
  {$DEFINE EX_CB3_UP}
  {$DEFINE EX_CB1_UP}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE EX_CB3}    // C++ Builder 3.0
  {$DEFINE EX_CB3_UP}
  {$DEFINE EX_CB1_UP}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE EX_CB1}    // C++ Builder 1.0
  {$DEFINE EX_CB1_UP}
{$ENDIF}

{$IFDEF EX_D6_UP}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFDEF EX_D7_UP}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}
