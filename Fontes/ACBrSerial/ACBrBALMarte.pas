unit ACBrBALMarte;

interface

uses
  Classes,
  ACBrBALClass
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type
  { ACBrBALMarte }

  TACBrBALMarte = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    procedure SolicitarPeso; override;
  end;

implementation

{ ACBrBALMarte }

constructor TACBrBALMarte.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Marte';
end;

procedure TACBrBALMarte.SolicitarPeso;
begin
  inherited;
  fpDevice.Limpar;
  fpDevice.EnviaString(#68#48#53); //COMANDO D05 - LEITURA SIMPLES
end;

end.
