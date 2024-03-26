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
  pcnConversao,
  pcnLeitor, pcnCIOT, ACBrCIOTConversao, synacode;

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
  i: Integer;
  sAux: string;
  ItemCarga: TConsultaTipoCargaCollectionItem;
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

          sAux := leitor.rCampo(tcStr, 'EstadoCiot');
          EstadoCiot := ecEmViagem;
          if sAux <> '' then
            EstadoCiot := StrToEstadoCIOT(sAux);

          if (leitor.rExtrai(3, 'Proprietario') <> '') then
          begin
            Proprietario.CNPJ := leitor.rCampo(tcStr, 'CNPJ');

            sAux := leitor.rCampo(tcStr, 'TipoPessoa');
            Proprietario.TipoPessoa := tpIndefinido;
            if sAux <> '' then
              Proprietario.TipoPessoa := StrToTipoPessoa(sAux);

            Proprietario.RazaoSocial       := leitor.rCampo(tcStr, 'RazaoSocial');
            Proprietario.RNTRC             := leitor.rCampo(tcStr, 'RNTRC');

            sAux := leitor.rCampo(tcStr, 'Tipo');
            Proprietario.Tipo := tpTAC;
            if sAux <> '' then
              Proprietario.Tipo := StrToTipoProprietario(sAux);

            Proprietario.TACouEquiparado   := StrToBool(leitor.rCampo(tcStr, 'TACouEquiparado'));
            Proprietario.DataValidadeRNTRC := leitor.rCampo(tcDat, 'DataValidadeRNTRC');
            Proprietario.RNTRCAtivo        := StrToBool(leitor.rCampo(tcStr, 'RNTRCAtivo'));

            if (leitor.rExtrai(4, 'Endereco') <> '') then
            begin
              Proprietario.Endereco.Bairro          := leitor.rCampo(tcStr, 'Bairro');
              Proprietario.Endereco.Rua             := leitor.rCampo(tcStr, 'Rua');
              Proprietario.Endereco.Numero          := leitor.rCampo(tcStr, 'Numero');
              Proprietario.Endereco.Complemento     := leitor.rCampo(tcStr, 'Complemento');
              Proprietario.Endereco.CEP             := leitor.rCampo(tcStr, 'CEP');
              Proprietario.Endereco.CodigoMunicipio := leitor.rCampo(tcInt, 'CodigoMunicipio');
            end;

            if (leitor.rExtrai(4, 'Telefones') <> '') then
            begin
              if (leitor.rExtrai(5, 'Celular') <> '') then
              begin
                Proprietario.Telefones.Celular.DDD    := leitor.rCampo(tcInt, 'DDD');
                Proprietario.Telefones.Celular.Numero := leitor.rCampo(tcInt, 'Numero');
              end;

              if (leitor.rExtrai(5, 'Fixo') <> '') then
              begin
                Proprietario.Telefones.Fixo.DDD    := leitor.rCampo(tcInt, 'DDD');
                Proprietario.Telefones.Fixo.Numero := leitor.rCampo(tcInt, 'Numero');
              end;

              if (leitor.rExtrai(5, 'Fax') <> '') then
              begin
                Proprietario.Telefones.Fax.DDD    := leitor.rCampo(tcInt, 'DDD');
                Proprietario.Telefones.Fax.Numero := leitor.rCampo(tcInt, 'Numero');
              end;
            end;
          end;

          if (leitor.rExtrai(3, 'Veiculo') <> '') then
          begin
            Veiculo.Placa           := leitor.rCampo(tcStr, 'Placa');
            Veiculo.Renavam         := leitor.rCampo(tcStr, 'Renavam');
            Veiculo.Chassi          := leitor.rCampo(tcStr, 'Chassi');
            Veiculo.RNTRC           := leitor.rCampo(tcStr, 'RNTRC');
            Veiculo.NumeroDeEixos   := leitor.rCampo(tcInt, 'NumeroDeEixos');
            Veiculo.CodigoMunicipio := leitor.rCampo(tcInt, 'CodigoMunicipio');
            Veiculo.Marca           := leitor.rCampo(tcStr, 'Marca');
            Veiculo.Modelo          := leitor.rCampo(tcStr, 'Modelo');
            Veiculo.AnoFabricacao   := leitor.rCampo(tcInt, 'AnoFabricacao');
            Veiculo.AnoModelo       := leitor.rCampo(tcInt, 'AnoModelo');
            Veiculo.Cor             := leitor.rCampo(tcStr, 'Cor');
            Veiculo.Tara            := leitor.rCampo(tcInt, 'Tara');
            Veiculo.CapacidadeKg    := leitor.rCampo(tcInt, 'CapacidadeKg');
            Veiculo.CapacidadeM3    := leitor.rCampo(tcInt, 'CapacidadeM3');

            sAux := leitor.rCampo(tcStr, 'TipoRodado');
            Veiculo.TipoRodado := trNaoAplicavel;
            if sAux <> '' then
              Veiculo.TipoRodado := StrToTipoRodado(sAux);

            sAux := leitor.rCampo(tcStr, 'TipoCarroceria');
            Veiculo.TipoCarroceria := tcNaoAplicavel;
            if sAux <> '' then
              Veiculo.TipoCarroceria := StrToTipoCarroceria(sAux);
          end;

          if (leitor.rExtrai(3, 'Motorista') <> '') then
          begin
            Motorista.CPF                 := leitor.rCampo(tcStr, 'CPF');
            Motorista.Nome                := leitor.rCampo(tcStr, 'Nome');
            Motorista.CNH                 := leitor.rCampo(tcStr, 'CNH');
            Motorista.DataNascimento      := leitor.rCampo(tcDat, 'DataNascimento');
            Motorista.NomeDeSolteiraDaMae := leitor.rCampo(tcStr, 'NomeDeSolteiraDaMae');

            if (leitor.rExtrai(4, 'Endereco') <> '') then
            begin
              Motorista.Endereco.Bairro          := leitor.rCampo(tcStr, 'Bairro');
              Motorista.Endereco.Rua             := leitor.rCampo(tcStr, 'Rua');
              Motorista.Endereco.Numero          := leitor.rCampo(tcStr, 'Numero');
              Motorista.Endereco.Complemento     := leitor.rCampo(tcStr, 'Complemento');
              Motorista.Endereco.CEP             := leitor.rCampo(tcStr, 'CEP');
              Motorista.Endereco.CodigoMunicipio := leitor.rCampo(tcInt, 'CodigoMunicipio');
            end;

            if (leitor.rExtrai(4, 'Telefones') <> '') then
            begin
              if (leitor.rExtrai(5, 'Celular') <> '') then
              begin
                Motorista.Telefones.Celular.DDD    := leitor.rCampo(tcInt, 'DDD');
                Motorista.Telefones.Celular.Numero := leitor.rCampo(tcInt, 'Numero');
              end;

              if (leitor.rExtrai(5, 'Fixo') <> '') then
              begin
                Motorista.Telefones.Fixo.DDD    := leitor.rCampo(tcInt, 'DDD');
                Motorista.Telefones.Fixo.Numero := leitor.rCampo(tcInt, 'Numero');
              end;

              if (leitor.rExtrai(5, 'Fax') <> '') then
              begin
                Motorista.Telefones.Fax.DDD    := leitor.rCampo(tcInt, 'DDD');
                Motorista.Telefones.Fax.Numero := leitor.rCampo(tcInt, 'Numero');
              end;
            end;
          end;

          if leitor.rExtrai(3, 'DocumentoViagem') <> '' then
          begin
            i := 0;
            while Leitor.rExtrai(4, 'string', '', i + 1) <> '' do
            begin
              DocumentoViagem.New.Mensagem := Leitor.rCampo(tcStr, 'string');
              inc(i);
            end;
          end;

          if leitor.rExtrai(3, 'DocumentoPagamento') <> '' then
          begin
            i := 0;
            while Leitor.rExtrai(4, 'string', '', i + 1) <> '' do
            begin
              DocumentoPagamento.New.Mensagem := Leitor.rCampo(tcStr, 'string');
              inc(i);
            end;
          end;

          if leitor.rExtrai(3, 'TipoCargas') <> '' then
          begin
            i := 0;
            while Leitor.rExtrai(4, 'TipoCarga', '', i + 1) <> '' do
            begin
              ItemCarga := TipoCarga.New;

              ItemCarga.Codigo := Leitor.rCampo(tcStr, 'CodigoTipoCarga');

              sAux := leitor.rCampo(tcStr, 'DescricaoTipoCarga');
              ItemCarga.Descricao := tpNaoAplicavel;
              if sAux <> '' then
                ItemCarga.Descricao := StrToTipoCarga(sAux);

              inc(i);
            end;
          end;

          if leitor.rExtrai(3, 'AlterarDataLiberacaoPagamentoResult') <> '' then
          begin
            AlterarDataLiberacaoPagamento.CodigoIdentificacaoOperacao := leitor.rCampo(tcStr, 'CodigoIdentificacaoOperacao');
            AlterarDataLiberacaoPagamento.IdPagamentoCliente          := leitor.rCampo(tcStr, 'IdPagamentoCliente');
            AlterarDataLiberacaoPagamento.DataDeLiberacao             := leitor.rCampo(tcDat, 'DataLiberacao');
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

