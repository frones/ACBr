program ACBrPixCDShipay;

uses
  Forms,
  PixCDShipay in 'PixCDShipay.pas' {frPixCDShipay};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrPixCDShipay, frPixCDShipay);
  Application.Run;
end.
