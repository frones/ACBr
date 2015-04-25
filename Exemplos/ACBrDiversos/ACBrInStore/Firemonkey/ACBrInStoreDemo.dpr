program ACBrInStoreDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UFormMaster in 'UFormMaster.pas' {FormMaster};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMaster, FormMaster);
  Application.Run;
end.
