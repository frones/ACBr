program picpay_vcl;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Principal.View in 'Principal.View.pas' {PrincipalView};

{.$R *.res}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPrincipalView, PrincipalView);
  Application.Run;
end.
