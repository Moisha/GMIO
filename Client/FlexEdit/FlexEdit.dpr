program FlexEdit;

uses
  Forms,
  FlexEdit.Main in 'FlexEdit.Main.pas' {EditMainForm},
  ACCtrls in 'ACControls\ACCtrls.pas',
  fChild in 'fChild.pas' {FlexChildForm},
  fOptions in 'fOptions.pas' {fmOptions},
  ToolMngr in 'ToolMngr.pas' {ToolContainer},
  fInspector in 'fInspector.pas' {fmInspector},
  fLibrary in 'fLibrary.pas' {fmLibrary},
  fLibProps in 'fLibProps.pas' {fmLibProps},
  fLibItemProps in 'fLibItemProps.pas' {fmLibItemProps},
  fPreview in 'fPreview.pas' {fmPreview},
  fDocProps in 'fDocProps.pas' {fmDocProps},
  UserDataFrm in 'PropForms\UserDataFrm.pas' {UserDataForm},
  PenFrm in 'PropForms\PenFrm.pas' {PenPropForm},
  PictureFrm in 'PropForms\PictureFrm.pas' {PicturePropForm},
  StrListFrm in 'PropForms\StrListFrm.pas' {StrListPropForm},
  BrushFrm in 'PropForms\BrushFrm.pas' {BrushPropForm},
  LineCapFrm in 'PropForms\LineCapFrm.pas' {LineCapPropForm},
  BackgroundFrm in 'PropForms\BackgroundFrm.pas' {BackgroundOptionsForm},
  fUserData in 'fUserData.pas' {fmUserData},
  fAboutPrg in 'fAboutPrg.pas' {formAbout},
  fLayers in 'fLayers.pas' {fmLayers},
  fChannelsProps in 'fChannelsProps.pas' {ChannelsPropForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'FlexGraphics editor';
  Application.CreateForm(TEditMainForm, EditMainForm);
  Application.CreateForm(TfmInspector, fmInspector);
  Application.CreateForm(TfmLibrary, fmLibrary);
  Application.CreateForm(TfmUserData, fmUserData);
  Application.CreateForm(TfmLayers, fmLayers);
  Application.Run;
end.
