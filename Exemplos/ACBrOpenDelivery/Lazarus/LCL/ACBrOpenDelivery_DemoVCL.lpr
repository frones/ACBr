program ACBrOpenDelivery_DemoVCL;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UntMain in 'UntMain.pas' {FMain},
  UntDM in 'UntDM.pas' {DM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
