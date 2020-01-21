
{$I ACBr.inc}

unit pcnRetEnvCIOT;

interface
 uses
  SysUtils, Classes,
  pcnAuxiliar, pcnConversao, pcnLeitor, pcnCIOT, pcnConversaoCIOT;

type
  TRetornoEnvio = class;

{ TRetornoEnvio }

 TRetornoEnvio = class(TPersistent)
  private
    FLeitor: TLeitor;
    FRetEnvio: TRetEnvio;
    FIntegradora: TCIOTIntegradora;
  public
    constructor Create;
    destructor Destroy; override;

    function LerRetorno_eFrete  : Boolean;
    function LerRetorno_Repom   : Boolean;
    function LerRetorno_Pamcard : Boolean;

    function LerXml: Boolean;
  published
    property Integradora: TCIOTIntegradora read FIntegradora write FIntegradora;
    property Leitor: TLeitor     read FLeitor   write FLeitor;
    property RetEnvio: TRetEnvio read FRetEnvio write FRetEnvio;
  end;

implementation

{ TRetornoEnvio }

constructor TRetornoEnvio.Create;
begin
  FLeitor   := TLeitor.Create;
  FRetEnvio := TRetEnvio.Create;
end;

destructor TRetornoEnvio.Destroy;
begin
  FLeitor.Free;
  FRetEnvio.Free;

  inherited;
end;

function TRetornoEnvio.LerRetorno_eFrete: Boolean;
var
  ok: Boolean;
  i: Integer;
begin
  Result := False;
  try
    if leitor.rExtrai(1, 'AdicionarOperacaoTransporteResponse') <> '' then
    begin
      if leitor.rExtrai(2, 'AdicionarOperacaoTransporteResult') <> '' then
      begin
        with RetEnvio do
        begin
          Versao := leitor.rCampo(tcStr, 'Versao');
          Sucesso := leitor.rCampo(tcStr, 'Sucesso');
          ProtocoloServico := leitor.rCampo(tcStr, 'ProtocoloServico');

          if leitor.rExtrai(3, 'Excecao') <> '' then
          begin
            Mensagem := leitor.rCampo(tcStr, 'Mensagem');
            Codigo := leitor.rCampo(tcStr, 'Codigo');
          end;
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

function TRetornoEnvio.LerRetorno_Pamcard: Boolean;
begin
   Result := True;
  try

    //.................. Implementar

  except
    Result := False;
  end;
end;

function TRetornoEnvio.LerRetorno_Repom: Boolean;
begin
  Result := True;
  try

    //.................. Implementar

  except
    Result := False;
  end;
end;

function TRetornoEnvio.LerXml: Boolean;
begin
  Leitor.Grupo := Leitor.Arquivo;

  case Integradora of
    ieFrete  :  Result := LerRetorno_eFrete;
    iRepom   :  Result := LerRetorno_Repom;
    iPamcard :  Result := LerRetorno_Pamcard;
  else
    Result := False;
  end;
end;

end.

