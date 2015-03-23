program TrocoTeste;

uses
  Forms,
  TrocoTeste1 in 'TrocoTeste1.pas' {FrmTroco};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmTroco, FrmTroco);
  Application.Run;
end.
