program picpay_vcl;

uses
  Forms,
  Principal.View in 'Principal.View.pas' {PrincipalView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPrincipalView, PrincipalView);
  Application.Run;
end.
