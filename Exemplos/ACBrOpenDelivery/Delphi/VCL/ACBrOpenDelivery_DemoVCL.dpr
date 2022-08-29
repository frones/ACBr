program ACBrOpenDelivery_DemoVCL;

uses
  Forms,
  UntMain in 'UntMain.pas' {FMain},
  UntDM in 'UntDM.pas' {DM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
