//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("FlexC5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\FlexUtils.pas");
USEUNIT("..\FlexControls.pas");
USEUNIT("..\FlexLibs.pas");
USEUNIT("..\FlexProps.pas");
USEUNIT("..\FlexReg.pas");
USERES("..\FlexReg.dcr");
USEUNIT("..\FlexBase.pas");
USEPACKAGE("vcljpg50.bpi");
USEUNIT("..\FlexPath.pas");
USEUNIT("..\FlexAlpha.pas");
USEUNIT("..\FlexHistory.pas");
USEUNIT("..\FlexActions.pas");
USEUNIT("..\FlexFileFormats.pas");
USEUNIT("..\FormatMetaFile.pas");
USEUNIT("..\FormatStdFiles.pas");
USEUNIT("..\FormatSvgFile.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
