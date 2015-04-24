program Demo_ACBrNFSe;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Frm_Demo_ACBrNFSe in 'Frm_Demo_ACBrNFSe.pas' {frmDemo_ACBrNFSe};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo_ACBrNFSe, frmDemo_ACBrNFSe);
  Application.Run;
end.
