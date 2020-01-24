
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

    function LerRetorno_eFrete: Boolean;
    function LerRetorno_Repom: Boolean;
    function LerRetorno_Pamcard: Boolean;

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
    if (leitor.rExtrai(1, 'AdicionarOperacaoTransporteResponse') <> '') or
       (leitor.rExtrai(1, 'RetificarOperacaoTransporteResponse') <> '') or
       (leitor.rExtrai(1, 'CancelarOperacaoTransporteResponse') <> '') or
       (leitor.rExtrai(1, 'ObterOperacaoTransportePdfResponse') <> '') or
       (leitor.rExtrai(1, 'AdicionarViagemResponse') <> '') or
       (leitor.rExtrai(1, 'AdicionarPagamentoResponse') <> '') or
       (leitor.rExtrai(1, 'CancelarPagamentoResponse') <> '') or
       (leitor.rExtrai(1, 'EncerrarOperacaoTransporteResponse') <> '') then
    begin
      if (leitor.rExtrai(2, 'AdicionarOperacaoTransporteResult') <> '') or
         (leitor.rExtrai(2, 'RetificarOperacaoTransporteResult') <> '') or
         (leitor.rExtrai(2, 'CancelarOperacaoTransporteResult') <> '') or
         (leitor.rExtrai(2, 'ObterOperacaoTransportePdfResult') <> '') or
         (leitor.rExtrai(2, 'AdicionarViagemResult') <> '') or
         (leitor.rExtrai(2, 'AdicionarPagamentoResult') <> '') or
         (leitor.rExtrai(2, 'CancelarPagamentoResult') <> '') or
         (leitor.rExtrai(2, 'EncerrarOperacaoTransporteResult') <> '') then
      begin
        with RetEnvio do
        begin
          Versao           := leitor.rCampo(tcStr, 'Versao');
          Sucesso          := leitor.rCampo(tcStr, 'Sucesso');
          ProtocoloServico := leitor.rCampo(tcStr, 'ProtocoloServico');

          PDF                         := leitor.rCampo(tcStr, 'Pdf');
          CodigoIdentificacaoOperacao := leitor.rCampo(tcStr, 'CodigoIdentificacaoOperacao');
          Data                        := leitor.rCampo(tcDatHor, 'Data');
          Protocolo                   := leitor.rCampo(tcStr, 'Protocolo');
          DataRetificacao             := leitor.rCampo(tcDatHor, 'DataRetificacao');
          QuantidadeViagens           := leitor.rCampo(tcInt, 'QuantidadeViagens');
          QuantidadePagamentos        := leitor.rCampo(tcInt, 'QuantidadePagamentos');
          IdPagamentoCliente          := leitor.rCampo(tcStr, 'IdPagamentoCliente');

          if leitor.rExtrai(3, 'DocumentoViagem') <> '' then
          begin
            i := 0;
            while Leitor.rExtrai(4, 'string', '', i + 1) <> '' do
            begin
              with DocumentoViagem.New do
              begin
                Mensagem := Leitor.rCampo(tcStr, 'string');
              end;
              inc(i);
            end;
          end;

          if leitor.rExtrai(3, 'DocumentoPagamento') <> '' then
          begin
            i := 0;
            while Leitor.rExtrai(4, 'string', '', i + 1) <> '' do
            begin
              with DocumentoPagamento.New do
              begin
                Mensagem := Leitor.rCampo(tcStr, 'string');
              end;
              inc(i);
            end;
          end;

          if leitor.rExtrai(3, 'Excecao') <> '' then
          begin
            Mensagem := leitor.rCampo(tcStr, 'Mensagem');
            Codigo   := leitor.rCampo(tcStr, 'Codigo');
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
  Result := False;

  try

    //.................. Implementar

  except
    Result := False;
  end;
end;

function TRetornoEnvio.LerRetorno_Repom: Boolean;
begin
  Result := False;

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
    ieFrete:  Result := LerRetorno_eFrete;
    iRepom:   Result := LerRetorno_Repom;
    iPamcard: Result := LerRetorno_Pamcard;
  else
    Result := False;
  end;
end;

end.

