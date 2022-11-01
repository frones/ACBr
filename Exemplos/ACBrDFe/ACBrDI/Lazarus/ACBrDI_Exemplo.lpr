program ACBrDI_Exemplo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Frm_ACBrDI
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:= True;
  Application.Scaled:= True;
  Application.Initialize;
  Application.CreateForm(TfrmACBrDI, frmACBrDI);
  Application.Run;
end.

