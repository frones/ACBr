program ConsultaCNPJ;

{$MODE Delphi}

uses
  Forms, Interfaces,
  U_Principal in 'U_Principal.pas' {F_Principal};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TF_Principal, F_Principal);
  Application.Run;
end.
