program AcbrBoletoDemo;

uses
  Forms,
  uDemo in 'uDemo.pas' {frmDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo, frmDemo);
  Application.Run;
end.
