program ConsultaCNPJ;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces,
  U_Principal in 'U_Principal.pas' {F_Principal};


begin
  Application.Initialize;
  Application.CreateForm(TF_Principal, F_Principal);
  Application.Run;
end.
