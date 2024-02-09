{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit pcnRetEnvCIOT;

interface
 uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor, pcnCIOT, pcnConversaoCIOT, synacode;

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
    if (leitor.rExtrai(1, 'LoginResponse') <> '') or
       (leitor.rExtrai(1, 'LogoutResponse') <> '') or

       (leitor.rExtrai(1, 'GravarResponse') <> '') or

       (leitor.rExtrai(1, 'AdicionarOperacaoTransporteResponse') <> '') or
       (leitor.rExtrai(1, 'RetificarOperacaoTransporteResponse') <> '') or
       (leitor.rExtrai(1, 'CancelarOperacaoTransporteResponse') <> '') or
       (leitor.rExtrai(1, 'ObterCodigoIdentificacaoOperacaoTransportePorIdOperacaoClienteResponse') <> '') or
       (leitor.rExtrai(1, 'ObterOperacaoTransportePdfResponse') <> '') or
       (leitor.rExtrai(1, 'AdicionarViagemResponse') <> '') or
       (leitor.rExtrai(1, 'AdicionarPagamentoResponse') <> '') or
       (leitor.rExtrai(1, 'CancelarPagamentoResponse') <> '') or
       (leitor.rExtrai(1, 'EncerrarOperacaoTransporteResponse') <> '') or
       (leitor.rExtrai(1, 'ConsultarTipoCargaResponse') <> '') or
       (leitor.rExtrai(1, 'AlterarDataLiberacaoPagamentoResponse') <> '') then
    begin
      if (leitor.rExtrai(2, 'LoginResult') <> '') or
         (leitor.rExtrai(2, 'LogoutResult') <> '') or

         (leitor.rExtrai(2, 'GravarResult') <> '') or

         (leitor.rExtrai(2, 'AdicionarOperacaoTransporteResult') <> '') or
         (leitor.rExtrai(2, 'RetificarOperacaoTransporteResult') <> '') or
         (leitor.rExtrai(2, 'CancelarOperacaoTransporteResult') <> '') or
         (leitor.rExtrai(2, 'ObterCodigoIdentificacaoOperacaoTransportePorIdOperacaoClienteResult') <> '') or
         (leitor.rExtrai(2, 'ObterOperacaoTransportePdfResult') <> '') or
         (leitor.rExtrai(2, 'AdicionarViagemResult') <> '') or
         (leitor.rExtrai(2, 'AdicionarPagamentoResult') <> '') or
         (leitor.rExtrai(2, 'CancelarPagamentoResult') <> '') or
         (leitor.rExtrai(2, 'EncerrarOperacaoTransporteResult') <> '') or
         (leitor.rExtrai(2, 'ConsultarTipoCargaResult') <> '') or
         (leitor.rExtrai(2, 'AlterarDataLiberacaoPagamentoResult') <> '') then
      begin
        with RetEnvio do
        begin
          Versao           := leitor.rCampo(tcStr, 'Versao');
          Sucesso          := leitor.rCampo(tcStr, 'Sucesso');
          ProtocoloServico := leitor.rCampo(tcStr, 'ProtocoloServico');

          Token := leitor.rCampo(tcStr, 'Token');

          PDF := leitor.rCampo(tcEsp, 'Pdf');

          if PDF <> '' then
            PDF := DecodeBase64(PDF);
          PDFNomeArquivo              := '';
          CodigoIdentificacaoOperacao := leitor.rCampo(tcStr, 'CodigoIdentificacaoOperacao');
          Data                        := leitor.rCampo(tcDatHor, 'Data');
          Protocolo                   := leitor.rCampo(tcStr, 'Protocolo');
          DataRetificacao             := leitor.rCampo(tcDatHor, 'DataRetificacao');
          QuantidadeViagens           := leitor.rCampo(tcInt, 'QuantidadeViagens');
          QuantidadePagamentos        := leitor.rCampo(tcInt, 'QuantidadePagamentos');
          IdPagamentoCliente          := leitor.rCampo(tcStr, 'IdPagamentoCliente');
          EstadoCiot                  := StrToEstadoCIOT(ok, leitor.rCampo(tcStr, 'EstadoCiot'));

          if (leitor.rExtrai(3, 'Proprietario') <> '') then
          begin
            With Proprietario do
            begin
              CNPJ              := leitor.rCampo(tcStr, 'CNPJ');
              TipoPessoa        := StrToTipoPessoa(ok, leitor.rCampo(tcStr, 'TipoPessoa'));
              RazaoSocial       := leitor.rCampo(tcStr, 'RazaoSocial');
              RNTRC             := leitor.rCampo(tcStr, 'RNTRC');
              Tipo              := StrToTipoProprietario(ok, leitor.rCampo(tcStr, 'Tipo'));
              TACouEquiparado   := StrToBool(leitor.rCampo(tcStr, 'TACouEquiparado'));
              DataValidadeRNTRC := leitor.rCampo(tcDat, 'DataValidadeRNTRC');
              RNTRCAtivo        := StrToBool(leitor.rCampo(tcStr, 'RNTRCAtivo'));

              if (leitor.rExtrai(4, 'Endereco') <> '') then
              begin
                with Endereco do
                begin
                  Bairro          := leitor.rCampo(tcStr, 'Bairro');
                  Rua             := leitor.rCampo(tcStr, 'Rua');
                  Numero          := leitor.rCampo(tcStr, 'Numero');
                  Complemento     := leitor.rCampo(tcStr, 'Complemento');
                  CEP             := leitor.rCampo(tcStr, 'CEP');
                  CodigoMunicipio := leitor.rCampo(tcInt, 'CodigoMunicipio');
                end;
              end;

              if (leitor.rExtrai(4, 'Telefones') <> '') then
              begin
                with Telefones do
                begin
                  if (leitor.rExtrai(5, 'Celular') <> '') then
                  begin
                    Celular.DDD    := leitor.rCampo(tcInt, 'DDD');
                    Celular.Numero := leitor.rCampo(tcInt, 'Numero');
                  end;

                  if (leitor.rExtrai(5, 'Fixo') <> '') then
                  begin
                    Fixo.DDD    := leitor.rCampo(tcInt, 'DDD');
                    Fixo.Numero := leitor.rCampo(tcInt, 'Numero');
                  end;

                  if (leitor.rExtrai(5, 'Fax') <> '') then
                  begin
                    Fax.DDD    := leitor.rCampo(tcInt, 'DDD');
                    Fax.Numero := leitor.rCampo(tcInt, 'Numero');
                  end;
                end;
              end;
            end;
          end;

          if (leitor.rExtrai(3, 'Veiculo') <> '') then
          begin
            With Veiculo do
            begin
              Placa           := leitor.rCampo(tcStr, 'Placa');
              Renavam         := leitor.rCampo(tcStr, 'Renavam');
              Chassi          := leitor.rCampo(tcStr, 'Chassi');
              RNTRC           := leitor.rCampo(tcStr, 'RNTRC');
              NumeroDeEixos   := leitor.rCampo(tcInt, 'NumeroDeEixos');
              CodigoMunicipio := leitor.rCampo(tcInt, 'CodigoMunicipio');
              Marca           := leitor.rCampo(tcStr, 'Marca');
              Modelo          := leitor.rCampo(tcStr, 'Modelo');
              AnoFabricacao   := leitor.rCampo(tcInt, 'AnoFabricacao');
              AnoModelo       := leitor.rCampo(tcInt, 'AnoModelo');
              Cor             := leitor.rCampo(tcStr, 'Cor');
              Tara            := leitor.rCampo(tcInt, 'Tara');
              CapacidadeKg    := leitor.rCampo(tcInt, 'CapacidadeKg');
              CapacidadeM3    := leitor.rCampo(tcInt, 'CapacidadeM3');
              TipoRodado      := StrToTipoRodado(ok, leitor.rCampo(tcStr, 'TipoRodado'));
              TipoCarroceria  := StrToTipoCarroceria(ok, leitor.rCampo(tcStr, 'TipoCarroceria'));
            end;
          end;

          if (leitor.rExtrai(3, 'Motorista') <> '') then
          begin
            With Motorista do
            begin
              CPF                 := leitor.rCampo(tcStr, 'CPF');
              Nome                := leitor.rCampo(tcStr, 'Nome');
              CNH                 := leitor.rCampo(tcStr, 'CNH');
              DataNascimento      := leitor.rCampo(tcDat, 'DataNascimento');
              NomeDeSolteiraDaMae := leitor.rCampo(tcStr, 'NomeDeSolteiraDaMae');

              if (leitor.rExtrai(4, 'Endereco') <> '') then
              begin
                with Endereco do
                begin
                  Bairro          := leitor.rCampo(tcStr, 'Bairro');
                  Rua             := leitor.rCampo(tcStr, 'Rua');
                  Numero          := leitor.rCampo(tcStr, 'Numero');
                  Complemento     := leitor.rCampo(tcStr, 'Complemento');
                  CEP             := leitor.rCampo(tcStr, 'CEP');
                  CodigoMunicipio := leitor.rCampo(tcInt, 'CodigoMunicipio');
                end;
              end;

              if (leitor.rExtrai(4, 'Telefones') <> '') then
              begin
                with Telefones do
                begin
                  if (leitor.rExtrai(5, 'Celular') <> '') then
                  begin
                    Celular.DDD    := leitor.rCampo(tcInt, 'DDD');
                    Celular.Numero := leitor.rCampo(tcInt, 'Numero');
                  end;

                  if (leitor.rExtrai(5, 'Fixo') <> '') then
                  begin
                    Fixo.DDD    := leitor.rCampo(tcInt, 'DDD');
                    Fixo.Numero := leitor.rCampo(tcInt, 'Numero');
                  end;

                  if (leitor.rExtrai(5, 'Fax') <> '') then
                  begin
                    Fax.DDD    := leitor.rCampo(tcInt, 'DDD');
                    Fax.Numero := leitor.rCampo(tcInt, 'Numero');
                  end;
                end;
              end;
            end;
          end;

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

          if leitor.rExtrai(3, 'TipoCargas') <> '' then
          begin
            i := 0;
            while Leitor.rExtrai(4, 'TipoCarga', '', i + 1) <> '' do
            begin
              with TipoCarga.New do
              begin
                Codigo    := Leitor.rCampo(tcStr, 'CodigoTipoCarga');
                Descricao := StrToTipoCarga(ok, leitor.rCampo(tcStr, 'DescricaoTipoCarga'));
              end;
              inc(i);
            end;
          end;

          if leitor.rExtrai(3, 'AlterarDataLiberacaoPagamentoResult') <> '' then
          begin
            With AlterarDataLiberacaoPagamento do
            begin
              CodigoIdentificacaoOperacao := leitor.rCampo(tcStr, 'CodigoIdentificacaoOperacao');
              IdPagamentoCliente          := leitor.rCampo(tcStr, 'IdPagamentoCliente');
              DataDeLiberacao             := leitor.rCampo(tcDat, 'DataLiberacao');
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

