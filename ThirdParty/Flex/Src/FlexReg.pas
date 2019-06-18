/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    Components registration unit                     //
//                                                     //
/////////////////////////////////////////////////////////

unit FlexReg;

{$I FlexDefs.inc}

interface

procedure Register;

implementation

uses
  Classes,
  FlexBase, FlexProps, FlexControls, FlexLibs, FlexUtils;

procedure Register;
begin
 RegisterComponents('Flex', [TFlexPanel]);
end;

end.
