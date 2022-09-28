program ACBrPagFor_Exemplo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Frm_ACBrPagFor_Exemplo in 'Frm_ACBrPagFor_Exemplo.pas' {frmACBrPagFor_Exemplo};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBrPagFor_Exemplo, frmACBrPagFor_Exemplo);
  Application.Run;
end.
