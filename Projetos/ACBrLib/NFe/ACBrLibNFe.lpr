library ACBrLibNFe;

{$mode delphi}

uses
  Interfaces, Classes, Forms, ACBrLibNFeClass, ACBrLibConfig, ACBrLibComum,
  ACBrLibConsts, ACBrLibNFeConfig, ACBrLibResposta, ACBrNFeRespostas;

{$R *.res}
begin
  pLibClass := TACBrLibNFe; // Ajusta a classe a ser criada
  Application.Initialize;
end.


