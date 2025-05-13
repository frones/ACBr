{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrNFSeXLerXml_ABRASFv1;

interface

uses
  SysUtils, Classes, StrUtils, IniFiles,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXLerXml;

type
  { TNFSeR_ABRASFv1 }

  TNFSeR_ABRASFv1 = class(TNFSeRClass)
  private

  protected
    procedure Configuracao; override;

    //======Arquivo XML==========================================
    function LerDataHoraCancelamento(const ANode: TACBrXmlNode): TDateTime; virtual;
    function LerDataHora(const ANode: TACBrXmlNode): TDateTime; virtual;
    function LerDataEmissao(const ANode: TACBrXmlNode): TDateTime; virtual;
    function LerDataEmissaoRps(const ANode: TACBrXmlNode): TDateTime; virtual;
    function LerCompetencia(const ANode: TACBrXmlNode): TDateTime; virtual;

    procedure LerInfNfse(const ANode: TACBrXmlNode); virtual;
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);
    procedure LerServico(const ANode: TACBrXmlNode);
    procedure LerItensServico(const ANode: TACBrXmlNode);
    procedure LerValores(const ANode: TACBrXmlNode);

    procedure LerPrestadorServico(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestadorServico(const ANode: TACBrXmlNode; const aTag: string);
    procedure LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
    procedure LerContatoPrestador(const ANode: TACBrXmlNode);

    procedure LerTomadorServico(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerContatoTomador(const ANode: TACBrXmlNode);

    procedure LerIntermediarioServico(const ANode: TACBrXmlNode);

    procedure LerOrgaoGerador(const ANode: TACBrXmlNode);
    procedure LerConstrucaoCivil(const ANode: TACBrXmlNode); virtual;

    procedure LerNfseCancelamento(const ANode: TACBrXmlNode);
    procedure LerConfirmacao(const ANode: TACBrXmlNode);
    procedure LerPedido(const ANode: TACBrXmlNode);
    procedure LerInfConfirmacaoCancelamento(const ANode: TACBrXmlNode);
    procedure LerInfPedidoCancelamento(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoNfse(const ANode: TACBrXmlNode);

    procedure LerNfseSubstituicao(const ANode: TACBrXmlNode);
    procedure LerSubstituicaoNfse(const ANode: TACBrXmlNode);

    procedure LerRpsSubstituido(const ANode: TACBrXmlNode);
    procedure LerPrestador(const ANode: TACBrXmlNode);

    //======Arquivo INI===========================================
    procedure LerINISecaoIdentificacaoNFSe(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoIdentificacaoRps(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoRpsSubstituido(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoNFSeSubstituicao(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoNFSeCancelamento(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoPrestador(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoTomador(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoIntermediario(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoTransportadora(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoConstrucaoCivil(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoServico(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoDeducoes(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoQuartos(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoEmail(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoGenericos(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoDespesas(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoItens(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoDadosDeducao(const AINIRec: TMemIniFile; const AIndice: Integer); virtual;
    procedure LerINISecaoDadosProssionalParceiro(const AINIRec: TMemIniFile; const AIndice: Integer); virtual;
    procedure LerINISecaoComercioExterior(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoLocacaoSubLocacao(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoEvento(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoRodoviaria(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoInformacoesComplementares(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoValores(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoValoresNFSe(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoCondicaoPagamento(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoOrgaoGerador(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoParcelas(const AINIRec: TMemIniFile); virtual;
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean; virtual;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
    function LerIni: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrConsts,
  ACBrNFSeXConversao;

//==============================================================================
// Essa unit tem por finalidade exclusiva de Ler o XML da NFS-e e RPS dos provedores
//     que seguem a versão 1.xx do layout da ABRASF
//==============================================================================

{ TNFSeR_ABRASFv1 }

procedure TNFSeR_ABRASFv1.Configuracao;
begin
  // Executa a Configuração Padrão
  inherited Configuracao;

end;

function TNFSeR_ABRASFv1.LerCompetencia(const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('Competencia'), tcDat);
end;

function TNFSeR_ABRASFv1.LerDataEmissao(const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDatHor);
end;

function TNFSeR_ABRASFv1.LerDataEmissaoRps(
  const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissaoRps'), tcDatHor);
end;

function TNFSeR_ABRASFv1.LerDataHora(const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('DataHora'), tcDatHor);
end;

function TNFSeR_ABRASFv1.LerDataHoraCancelamento(
  const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('DataHoraCancelamento'), tcDatHor);

  if Result = 0 then
    Result := ObterConteudo(ANode.Childrens.FindAnyNs('DataHora'), tcDatHor);

  if Result = 0 then
    Result := ObterConteudo(ANode.Childrens.FindAnyNs('Datahora'), tcDatHor);
end;

procedure TNFSeR_ABRASFv1.LerConfirmacao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Confirmacao');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('ConfirmacaoCancelamento');

  if AuxNode <> nil then
  begin
    LerPedido(AuxNode);
    LerInfConfirmacaoCancelamento(AuxNode);

    with NFSe.NfseCancelamento do
    begin
      if DataHora = 0 then
      begin
        DataHora := LerDataHoraCancelamento(AuxNode);

        if DataHora > 0 then
          NFSe.SituacaoNfse := snCancelado;
      end;
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerConstrucaoCivil(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('ConstrucaoCivil');

  if AuxNode <> nil then
  begin
    with NFSe.ConstrucaoCivil do
    begin
      CodigoObra := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoObra'), tcStr);
      Art        := ObterConteudo(AuxNode.Childrens.FindAnyNs('Art'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerContatoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Contato');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Contato do
    begin
      Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('Telefone'), tcStr);
      Email    := ObterConteudo(AuxNode.Childrens.FindAnyNs('Email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerContatoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Contato');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Contato do
    begin
      Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('Telefone'), tcStr);
      Email    := ObterConteudo(AuxNode.Childrens.FindAnyNs('Email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerEnderecoPrestadorServico(const ANode: TACBrXmlNode;
  const aTag: string);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs(aTag);

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      Endereco        := ObterConteudo(AuxNode.Childrens.FindAnyNs('Endereco'), tcStr);

      if Endereco = '' then
        Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('EnderecoDescricao'), tcStr);

      Numero          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento     := ObterConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);

      if CodigoMunicipio = '' then
        CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);

      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);

      if UF = '' then
        UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('Estado'), tcStr);

      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;

      CodigoPais := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoPais'), tcInt);
      CEP        := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Endereco        := ObterConteudo(AuxNode.Childrens.FindAnyNs('Endereco'), tcStr);

      if Endereco = '' then
        Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('EnderecoDescricao'), tcStr);

      Numero          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento     := ObterConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);

      if CodigoMunicipio = '' then
        CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);

      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);

      if UF = '' then
        UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('Estado'), tcStr);

      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;

      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerIdentificacaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoNfse');

  if AuxNode <> nil then
  begin
    with NFSe.NfseCancelamento.Pedido.IdentificacaoNfse do
    begin
      Numero             := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Cnpj               := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);
      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
      CodigoMunicipio    := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoPrestador');

  if AuxNode = nil then
    AuxNode := ANode;

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.IdentificacaoPrestador do
    begin
      CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);

      if CpfCnpj = '' then
      begin
        AuxNodeCpfCnpj := AuxNode.Childrens.FindAnyNs('CpfCnpj');

        if AuxNodeCpfCnpj <> nil then
        begin
          CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cpf'), tcStr);

          if CpfCnpj = '' then
            CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cnpj'), tcStr);
        end;
      end;

      if Length(CpfCnpj) > 11 then
        CpfCnpj := Poem_Zeros(CpfCnpj, 14);

      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerIdentificacaoRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoRps');

  if AuxNode <> nil then
  begin
    with NFSe.IdentificacaoRps do
    begin
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Serie  := ObterConteudo(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);
      Tipo   := FpAOwner.StrToTipoRPS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Tipo'), tcStr));
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerIdentificacaoTomador(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoTomador');

  if AuxNode <> nil then
  begin
    AuxNodeCpfCnpj := AuxNode.Childrens.FindAnyNs('CpfCnpj');

    with NFSe.Tomador.IdentificacaoTomador do
    begin
      if AuxNodeCpfCnpj <> nil then
      begin
        CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cpf'), tcStr);

        if CpfCnpj = '' then
          CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cnpj'), tcStr);
      end;

      if Length(CpfCnpj) > 11 then
        CpfCnpj := Poem_Zeros(CpfCnpj, 14);

      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
      InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoEstadual'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerInfConfirmacaoCancelamento(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfConfirmacaoCancelamento');

  NFSe.SituacaoNfse := snNormal;

  if AuxNode <> nil then
  begin
    with NFSe.NfseCancelamento do
    begin
      Sucesso  := ObterConteudo(AuxNode.Childrens.FindAnyNs('Sucesso'), tcBool);
      DataHora := LerDataHora(AuxNode);

      if DataHora > 0 then
        NFSe.SituacaoNfse := snCancelado;
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerInfNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  IdAttr: string;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfNfse');

  if AuxNode <> nil then
  begin
    IdAttr := FpAOwner.ConfigGeral.Identificador;
    NFSe.infNFSe.ID := ObterConteudoTag(AuxNode.Attributes.Items[IdAttr]);

    NFSe.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
    NFSe.Link := ObterConteudo(AuxNode.Childrens.FindAnyNs('LinkVisualizacaoNfse'), tcStr);

    if NFSe.Link = '' then
      NFSe.Link := ObterConteudo(AuxNode.Childrens.FindAnyNs('LinkPdf'), tcStr);

    NFSe.Link := StringReplace(NFSe.Link, '&amp;', '&', [rfReplaceAll]);
    NFSe.CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
    NFSe.DataEmissao := LerDataEmissao(AuxNode);
    NFSe.NfseSubstituida := ObterConteudo(AuxNode.Childrens.FindAnyNs('NfseSubstituida'), tcStr);

    LerIdentificacaoRps(AuxNode);

    NFSe.DataEmissaoRps := LerDataEmissaoRps(AuxNode);
    NFSe.NaturezaOperacao := StrToNaturezaOperacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('NaturezaOperacao'), tcStr));
    NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RegimeEspecialTributacao'), tcStr));
    NFSe.OptanteSimplesNacional := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr));
    NFSe.IncentivadorCultural := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('IncentivadorCultural'), tcStr));
    NFSe.Competencia := LerCompetencia(AuxNode);
    NFSe.NfseSubstituida := ObterConteudo(AuxNode.Childrens.FindAnyNs('NfseSubstituida'), tcStr);
    NFSe.OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('OutrasInformacoes'), tcStr);
    NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);

    LerServico(AuxNode);

    NFSe.ValorCredito := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCredito'), tcDe2);

    LerPrestadorServico(AuxNode);
    LerTomadorServico(AuxNode);
    LerIntermediarioServico(AuxNode);
    LerOrgaoGerador(AuxNode);
    LerConstrucaoCivil(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerNfseCancelamento(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('NfseCancelamento');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('CancelamentoNfse');

  LerConfirmacao(AuxNode);
end;

procedure TNFSeR_ABRASFv1.LerNfseSubstituicao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('NfseSubstituicao');

  LerSubstituicaoNfse(AuxNode);
end;

procedure TNFSeR_ABRASFv1.LerInfPedidoCancelamento(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfPedidoCancelamento');

  if AuxNode <> nil then
  begin
    LerIdentificacaoNfse(AuxNode);

    with NFSe.NfseCancelamento.Pedido do
      CodigoCancelamento := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoCancelamento'), tcStr);
  end;
end;

procedure TNFSeR_ABRASFv1.LerIntermediarioServico(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IntermediarioServico');

  if AuxNode <> nil then
  begin
    NFSe.Intermediario.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);

    AuxNodeCpfCnpj := AuxNode.Childrens.FindAnyNs('CpfCnpj');

    with NFSe.Intermediario.Identificacao do
    begin
      if AuxNodeCpfCnpj <> nil then
      begin
        CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cpf'), tcStr);

        if CpfCnpj = '' then
          CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cnpj'), tcStr);
      end;

      if Length(CpfCnpj) > 11 then
        CpfCnpj := Poem_Zeros(CpfCnpj, 14);

      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerOrgaoGerador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('OrgaoGerador');

  if AuxNode <> nil then
  begin
    with NFSe.OrgaoGerador do
    begin
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      Uf              := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerPedido(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Pedido');

  if AuxNode <> nil then
  begin
    LerInfPedidoCancelamento(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Prestador');

  if AuxNode <> nil then
  begin
    LerIdentificacaoPrestador(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerPrestadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('PrestadorServico');

  if AuxNode <> nil then
  begin
    LerIdentificacaoPrestador(AuxNode);

    with NFSe.Prestador do
    begin
      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
      RazaoSocial := StringReplace(RazaoSocial, '&amp;', '&', [rfReplaceAll]);

      NomeFantasia := ObterConteudo(AuxNode.Childrens.FindAnyNs('NomeFantasia'), tcStr);
    end;

    LerEnderecoPrestadorServico(AuxNode, 'Endereco');
    LerContatoPrestador(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerRpsSubstituido(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('RpsSubstituido');

  if AuxNode <> nil then
  begin
    with NFSe.RpsSubstituido do
    begin
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Serie  := ObterConteudo(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);
      Tipo   := FpAOwner.StrToTipoRPS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Tipo'), tcStr));
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  CodigoItemServico, xUF: string;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Servico');

  if AuxNode <> nil then
  begin
    LerValores(AuxNode);

    CodigoItemServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('ItemListaServico'), tcStr);

    with NFSe.Servico do
    begin
      ItemListaServico          := NormatizarItemListaServico(CodigoItemServico);
      xItemListaServico         := ItemListaServicoDescricao(ItemListaServico);
      CodigoCnae                := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoCnae'), tcStr);
      CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoTributacaoMunicipio'), tcStr);
      Discriminacao             := ObterConteudo(AuxNode.Childrens.FindAnyNs('Discriminacao'), tcStr);
      Discriminacao := StringReplace(Discriminacao, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);

      VerificarSeConteudoEhLista(Discriminacao);

      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);

      if CodigoMunicipio = '' then
        CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('MunicipioPrestacaoServico'), tcStr);

      MunicipioIncidencia := StrToIntDef(CodigoMunicipio, 0);
      MunicipioPrestacaoServico := '';
      xMunicipioIncidencia := '';

      if MunicipioIncidencia > 0 then
      begin
        MunicipioPrestacaoServico := ObterNomeMunicipioUF(MunicipioIncidencia, xUF);
        MunicipioPrestacaoServico := MunicipioPrestacaoServico + '/' + xUF;

        xMunicipioIncidencia := MunicipioPrestacaoServico;
      end;
    end;

    NFSe.InformacoesComplementares := ObterConteudo(AuxNode.Childrens.FindAnyNs('InformacoesComplementares'), tcStr);
    NFSe.InformacoesComplementares := StringReplace(NFSe.InformacoesComplementares,
                                   FpQuebradeLinha, sLineBreak, [rfReplaceAll]);

    LerItensServico(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerItensServico(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: integer;
begin
  if not Assigned(ANode) then Exit;

  ANodes := ANode.Childrens.FindAllAnyNs('ItensServico');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.Servico.ItemServico.New;

    with NFSe.Servico.ItemServico[i] do
    begin
      Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Descricao'), tcStr);
      Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Quantidade'), tcDe2);
      ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorUnitario'), tcDe4);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerSubstituicaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('SubstituicaoNfse');

  if AuxNode <> nil then
  begin
    NFSe.NfseSubstituidora := ObterConteudo(AuxNode.Childrens.FindAnyNs('NfseSubstituidora'), tcStr);
  end;
end;

procedure TNFSeR_ABRASFv1.LerTomadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('TomadorServico');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('Tomador');

  if AuxNode <> nil then
  begin
    LerIdentificacaoTomador(AuxNode);

    NFSe.Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
    NFSe.Tomador.RazaoSocial := StringReplace(NFSe.Tomador.RazaoSocial, '&amp;', '&', [rfReplaceAll]);

    LerEnderecoTomador(AuxNode);
    LerContatoTomador(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerValores(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
  ValorLiq: Double;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Valores');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorServicos := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
      ValorDeducoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorDeducoes'), tcDe2);
      ValorPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorPis'), tcDe2);
      ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
      ValorInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorInss'), tcDe2);
      ValorIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIr'), tcDe2);
      ValorCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);
      IssRetido := FpAOwner.StrToSituacaoTributaria(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('IssRetido'), tcStr));
      ValorIss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIss'), tcDe2);
      OutrasRetencoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('OutrasRetencoes'), tcDe2);
      BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('BaseCalculo'), tcDe2);
      Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe4);
      Aliquota := NormatizarAliquota(Aliquota);
      ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorLiquidoNfse'), tcDe2);
      ValorIssRetido := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIssRetido'), tcDe2);
      DescontoCondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescontoCondicionado'), tcDe2);
      DescontoIncondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescontoIncondicionado'), tcDe2);

      RetencoesFederais := ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll;

      if ValorIssRetido = 0 then
      begin
        if IssRetido = stRetencao then
          ValorIssRetido := ValorIss
        else
          ValorIssRetido := 0;
      end;

      ValorLiq := ValorServicos - RetencoesFederais - OutrasRetencoes -
                  ValorIssRetido - DescontoIncondicionado - DescontoCondicionado;

      if (ValorLiquidoNfse = 0) or (ValorLiquidoNfse <> ValorLiq) then
        ValorLiquidoNfse := ValorLiq;

      ValorTotalNotaFiscal := ValorServicos - DescontoCondicionado -
                              DescontoIncondicionado;
    end;

    NFSe.TipoRecolhimento := FpAOwner.SituacaoTributariaDescricao(NFSe.Servico.Valores.IssRetido);
  end;
end;

function TNFSeR_ABRASFv1.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  LerParamsTabIni(True);

  Arquivo := NormatizarXml(Arquivo);

  tpXML := TipodeXMLLeitura(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  NFSe.Clear;
  NFSe.tpXML := tpXml;

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);

  FreeAndNil(FDocument);
end;

function TNFSeR_ABRASFv1.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode1, AuxNode2: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  AuxNode1 := ANode.Childrens.FindAnyNs('tcCompNfse');

  if AuxNode1 = nil then
  begin
    AuxNode1 := ANode;
    AuxNode2 := ANode.Childrens.FindAnyNs('Nfse')
  end
  else
    AuxNode2 := AuxNode1.Childrens.FindAnyNs('Nfse');

  if AuxNode2 = nil then
    AuxNode2 := ANode;

  LerInfNfse(AuxNode2);

  LerNfseCancelamento(AuxNode1);
  LerNfseSubstituicao(AuxNode1);

  LerCampoLink;
end;

function TNFSeR_ABRASFv1.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfRps');

  if AuxNode <> nil then
  begin
    LerIdentificacaoRps(AuxNode);

    with NFSe do
    begin
      DataEmissao := LerDataEmissao(AuxNode);
      NaturezaOperacao := StrToNaturezaOperacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('NaturezaOperacao'), tcStr));
      RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RegimeEspecialTributacao'), tcStr));
      OptanteSimplesNacional := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr));
      IncentivadorCultural := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('IncentivadorCultural'), tcStr));
      StatusRps := FpAOwner.StrToStatusRPS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Status'), tcStr));
    end;

    LerRpsSubstituido(AuxNode);
    LerServico(AuxNode);
    LerPrestador(AuxNode);
    LerTomadorServico(AuxNode);
    LerIntermediarioServico(AuxNode);
    LerConstrucaoCivil(AuxNode);
  end;
end;

function TNFSeR_ABRASFv1.LerIni: Boolean;
var
  LIniRec: TMemIniFile;
begin
  LIniRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(Arquivo, LIniRec);
    LerINISecaoIdentificacaoNFSe(LINIRec);
    LerINISecaoIdentificacaoRps(LINIRec);
    LerINISecaoRpsSubstituido(LINIRec);
    LerINISecaoNFSeSubstituicao(LINIRec);
    LerINISecaoNFSeCancelamento(LINIRec);
    LerINISecaoPrestador(LINIRec);
    LerINISecaoTomador(LINIRec);
    LerINISecaoIntermediario(LINIRec);
    LerINISecaoTransportadora(LINIRec);
    LerINISecaoConstrucaoCivil(LINIRec);
    LerINISecaoServico(LINIRec);
    LerINISecaoDeducoes(LINIRec);
    LerINISecaoQuartos(LINIRec);
    LerINISecaoEmail(LINIRec);
    LerINISecaoGenericos(LINIRec);
    LerINISecaoDespesas(LINIRec);
    LerINISecaoItens(LINIRec);
    LerINISecaoComercioExterior(LINIRec);
    LerINISecaoLocacaoSubLocacao(LINIRec);
    LerINISecaoEvento(LINIRec);
    LerINISecaoRodoviaria(LINIRec);
    LerINISecaoInformacoesComplementares(LINIRec);
    LerINISecaoValores(LINIRec);
    LerINISecaoValoresNFSe(LINIRec);
    LerINISecaoCondicaoPagamento(LINIRec);
    LerINISecaoOrgaoGerador(LINIRec);
    LerINISecaoParcelas(LINIRec);
    Result := True;
  finally
    LIniRec.Free;
  end;
end;

procedure TNFSeR_ABRASFv1.LerINISecaoIdentificacaoNFSe(const AINIRec: TMemIniFile);
var
  LSecao, LAux: String;
  OK: Boolean;
begin
  LSecao:= 'IdentificacaoNFSe';
  if AIniRec.SectionExists(LSecao) then
  begin
    LAux := AINIRec.ReadString(LSecao, 'TipoXML', '');
    if LAux = 'NFSe' then
    begin
      NFSe.tpXMl := txmlNFSe;

      NFSe.Numero := AINIRec.ReadString(LSecao, 'Numero', '');
      NFSe.SituacaoNfse := StrToStatusNFSe(Ok, AINIRec.ReadString(LSecao, 'StatusNFSe', ''));
      NFSe.CodigoVerificacao := AINIRec.ReadString(LSecao, 'CodigoVerificacao', '');
      NFSe.InfNFSe.Id := AINIRec.ReadString(LSecao, 'ID', NFSe.InfNFSe.ID);
      NFSe.NfseSubstituida := AINIRec.ReadString(LSecao, 'NfseSubstituida', '');
      NFSe.NfseSubstituidora := AINIRec.ReadString(LSecao, 'NfseSubstituidora', '');
      NFSe.ValorCredito := AINIRec.ReadFloat(LSecao, 'ValorCredito', 0);
      NFSe.Link := AINIRec.ReadString(LSecao, 'Link', NFSe.Link);
    end
    else
      NFSe.tpXML := txmlRPS;
  end;
end;

procedure TNFSeR_ABRASFv1.LerINISecaoIdentificacaoRps(const AINIRec: TMemIniFile);
var
  LSecao: String;
  Ok: Boolean;
begin
  LSecao:= 'IdentificacaoRps';

  NFSe.IdentificacaoRps.Numero := AINIRec.ReadString(LSecao, 'Numero', '');
  NFSe.IdentificacaoRps.Serie := AINIRec.ReadString(LSecao, 'Serie', '');
  NFSe.IdentificacaoRps.Tipo := FPAOwner.StrToTipoRPS(Ok, AINIRec.ReadString(LSecao, 'Tipo', ''));

  NFSe.DataEmissao := AINIRec.ReadDateTime(LSecao, 'DataEmissao', 0);
  NFSe.DataEmissaoRps := AINIRec.ReadDateTime(LSecao, 'DataEmissaoRps', 0);
  NFSe.NaturezaOperacao := StrToNaturezaOperacao(Ok, AINIRec.ReadString(LSecao, 'NaturezaOperacao', ''));

  NFSe.StatusRps := FpAOwner.StrToStatusRPS(Ok, AINIRec.ReadString(LSecao, 'Status', ''));
  NFSe.OutrasInformacoes := StringReplace(AINIRec.ReadString(LSecao, 'OutrasInformacoes', ''), FpAOwner.ConfigGeral.QuebradeLinha, sLineBreak, [rfReplaceAll]) ;

  NFSe.TipoRecolhimento := AINIRec.ReadString(LSecao, 'TipoRecolhimento', '');
  NFSe.Competencia := AINIRec.ReadDate(LSecao, 'Competencia', 0);
  NFSe.InformacoesComplementares := AINIRec.ReadString(LSecao, 'InformacoesComplementares', '');
end;

procedure TNFSeR_ABRASFv1.LerINISecaoRpsSubstituido(const AINIRec: TMemIniFile);
var
  LSecao: String;
  Ok: Boolean;
begin
  LSecao := 'RpsSubstituido';
  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.RpsSubstituido.Numero := AINIRec.ReadString(LSecao, 'Numero', '');
    NFSe.RpsSubstituido.Serie := AINIRec.ReadString(LSecao, 'Serie', '');
    NFSe.RpsSubstituido.Tipo := FpAOwner.StrToTipoRPS(Ok, AINIRec.ReadString(LSecao, 'Tipo', ''));
  end;
end;

procedure TNFSeR_ABRASFv1.LerINISecaoNFSeSubstituicao(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoNFSeCancelamento(const AINIRec: TMemIniFile);
var
  LSecao: String;
begin
  LSecao := 'NFSeCancelamento';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.NFSeCancelamento.Pedido.IdentificacaoNfse.Numero := AINIRec.ReadString(LSecao, 'NumeroNFSe', '');
    NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.Cnpj := AINIRec.ReadString(LSecao, 'CNPJ', '');
    NFSe.NFSeCancelamento.Pedido.IdentificacaoNfse.InscricaoMunicipal := AINIRec.ReadString(LSecao, 'InscricaoMunicipal', '');
    NFSe.NFSeCancelamento.Pedido.IdentificacaoNfse.CodigoMunicipio := AINIRec.ReadString(LSecao, 'CodigoMunicipio', '');
    NFSe.NfseCancelamento.Pedido.CodigoCancelamento := AINIRec.ReadString(LSecao, 'CodCancel', '');
    NFSe.NfSeCancelamento.DataHora := AINIRec.ReadDateTime(LSecao, 'DataHora', 0);
  end;
end;

procedure TNFSeR_ABRASFv1.LerINISecaoPrestador(const AINIRec: TMemIniFile);
var
  LSecao: String;
  Ok: Boolean;
begin
  LSecao := 'Prestador';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, AINIRec.ReadString(LSecao, 'Regime', ''));
    NFSe.OptanteSimplesNacional := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(LSecao, 'OptanteSN', ''));
    NFSe.IncentivadorCultural := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(LSecao, 'IncentivadorCultural', ''));

    NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := AINIRec.ReadString(LSecao, 'CNPJ', '');
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := AINIRec.ReadString(LSecao, 'InscricaoMunicipal', '');
    {
    NFSe.Prestador.RazaoSocial := AINIRec.ReadString(LSecao, 'RazaoSocial', '');
    NFSe.Prestador.NomeFantasia := AINIRec.ReadString(LSecao, 'NomeFantasia', '');

    NFSe.Prestador.Endereco.Endereco := AINIRec.ReadString(LSecao, 'Logradouro', '');
    NFSe.Prestador.Endereco.Numero := AINIRec.ReadString(LSecao, 'Numero', '');
    NFSe.Prestador.Endereco.Complemento := AINIRec.ReadString(LSecao, 'Complemento', '');
    NFSe.Prestador.Endereco.Bairro := AINIRec.ReadString(LSecao, 'Bairro', '');
    NFSe.Prestador.Endereco.CodigoMunicipio := AINIRec.ReadString(LSecao, 'CodigoMunicipio', '');
    NFSe.Prestador.Endereco.xMunicipio := AINIRec.ReadString(LSecao, 'xMunicipio', '');
    NFSe.Prestador.Endereco.UF := AINIRec.ReadString(LSecao, 'UF', '');
    NFSe.Prestador.Endereco.CodigoPais := AINIRec.ReadInteger(LSecao, 'CodigoPais', 0);
    NFSe.Prestador.Endereco.CEP := AINIRec.ReadString(LSecao, 'CEP', '');

    NFSe.Prestador.Contato.Telefone := AINIRec.ReadString(LSecao, 'Telefone', '');
    NFSe.Prestador.Contato.Email := AINIRec.ReadString(LSecao, 'Email', '');
    }
  end;
end;

procedure TNFSeR_ABRASFv1.LerINISecaoTomador(const AINIRec: TMemIniFile);
var
  LSecao: String;
begin
  LSecao := 'Tomador';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := AINIRec.ReadString(LSecao, 'CNPJCPF', '');
    NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := AINIRec.ReadString(LSecao, 'InscricaoMunicipal', '');
    NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual := AINIRec.ReadString(LSecao, 'InscricaoEstadual', '');
    NFSe.Tomador.RazaoSocial := AINIRec.ReadString(LSecao, 'RazaoSocial', '');

    NFSe.Tomador.Endereco.Endereco := AINIRec.ReadString(LSecao, 'Logradouro', '');
    NFSe.Tomador.Endereco.Numero := AINIRec.ReadString(LSecao, 'Numero', '');
    NFSe.Tomador.Endereco.Complemento := AINIRec.ReadString(LSecao, 'Complemento', '');
    NFSe.Tomador.Endereco.Bairro := AINIRec.ReadString(LSecao, 'Bairro', '');
    NFSe.Tomador.Endereco.CodigoMunicipio := AINIRec.ReadString(LSecao, 'CodigoMunicipio', '');
    NFSe.Tomador.Endereco.xMunicipio := AINIRec.ReadString(LSecao, 'xMunicipio', '');
    NFSe.Tomador.Endereco.UF := AINIRec.ReadString(LSecao, 'UF', '');
    NFSe.Tomador.Endereco.CEP := AINIRec.ReadString(LSecao, 'CEP', '');
//    NFSe.Tomador.Endereco.CodigoPais := AINIRec.ReadInteger(LSecao, 'CodigoPais', 0);

    NFSe.Tomador.Contato.Telefone := AINIRec.ReadString(LSecao, 'Telefone', '');
    NFSe.Tomador.Contato.Email := AINIRec.ReadString(LSecao, 'Email', '');
  end;
end;

procedure TNFSeR_ABRASFv1.LerINISecaoIntermediario(const AINIRec: TMemIniFile);
var
  LSecao: String;
begin
  LSecao := 'Intermediario';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.Intermediario.Identificacao.CpfCnpj := AINIRec.ReadString(LSecao, 'CNPJCPF', '');
    NFSe.Intermediario.Identificacao.InscricaoMunicipal := AINIRec.ReadString(LSecao, 'InscricaoMunicipal', '');
    NFSe.Intermediario.RazaoSocial := AINIRec.ReadString(LSecao, 'RazaoSocial', '');
//    NFSe.Intermediario.CodigoMunicipio := AINIRec.ReadString(LSecao, 'CodigoMunicipio', '');
  end;
end;

procedure TNFSeR_ABRASFv1.LerINISecaoTransportadora(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoConstrucaoCivil(const AINIRec: TMemIniFile);
var
  LSecao: String;
begin
  LSecao := 'ConstrucaoCivil';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.ConstrucaoCivil.CodigoObra := AINIRec.ReadString(LSecao, 'CodigoObra', '');
    NFSe.ConstrucaoCivil.Art := AINIRec.ReadString(LSecao, 'Art', '');
  end;
end;

procedure TNFSeR_ABRASFv1.LerINISecaoServico(const AINIRec: TMemIniFile);
var
  LSecao: String;
begin
  LSecao := 'Servico';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.Servico.ItemListaServico := AINIRec.ReadString(LSecao, 'ItemListaServico', '');
    NFSe.Servico.xItemListaServico := AINIRec.ReadString(LSecao, 'xItemListaServico', '');
    NFSe.Servico.CodigoCnae := AINIRec.ReadString(LSecao, 'CodigoCnae', '');
    NFSe.Servico.CodigoTributacaoMunicipio := AINIRec.ReadString(LSecao, 'CodigoTributacaoMunicipio', '');
    NFSe.Servico.Discriminacao := ChangeLineBreak(AINIRec.ReadString(LSecao, 'Discriminacao', ''), FpAOwner.ConfigGeral.QuebradeLinha);
    NFSe.Servico.CodigoMunicipio := AINIRec.ReadString(LSecao, 'CodigoMunicipio', '');
    {
    NFSe.Servico.MunicipioIncidencia := AINIRec.ReadInteger(LSecao, 'MunicipioIncidencia', 0);
    NFSe.Servico.xMunicipioIncidencia := AINIRec.ReadString(LSecao, 'xMunicipioIncidencia', '');
    NFSe.Servico.MunicipioPrestacaoServico := AINIRec.ReadString(LSecao, 'MunicipioPrestacaoServico', '');
    NFSe.Servico.ValorTotalRecebido := AINIRec.ReadFloat(LSecao, 'ValorTotalRecebido', 0.0);
    }
  end;
end;

procedure TNFSeR_ABRASFv1.LerINISecaoDeducoes(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoQuartos(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoEmail(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoGenericos(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoDespesas(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoItens(const AINIRec: TMemIniFile);
var
  I: Integer;
  LSecao: String;
begin
  I := 0;
  while True do
  begin
    LSecao:= 'Itens' + IntToStrZero(I + 1, 3);
    if not AINIRec.SectionExists(LSecao) then
      break;

    NFSe.Servico.ItemServico.New;
    NFSe.Servico.ItemServico.Items[I].Descricao := ChangeLineBreak(AINIRec.ReadString(LSecao, 'Descricao', ''), FpAOwner.ConfigGeral.QuebradeLinha);
    NFSe.Servico.ItemServico.Items[I].ItemListaServico := AINIRec.ReadString(LSecao, 'ItemListaServico', '');
    NFSe.Servico.ItemServico.Items[I].xItemListaServico := AINIRec.ReadString(LSecao, 'xItemListaServico', '');
    NFSe.Servico.ItemServico.Items[I].Quantidade := AINIRec.ReadFloat(LSecao, 'Quantidade', 0);
    NFSe.Servico.ItemServico.Items[I].ValorUnitario := AINIRec.ReadFloat(LSecao, 'ValorUnitario', 0);
    NFSe.Servico.ItemServico.Items[I].ValorISS := AINIRec.ReadFloat(LSecao, 'ValorIss', 0);
    NFSe.Servico.ItemServico.Items[I].Aliquota := AINIRec.ReadFloat(LSecao, 'Aliquota', 0);
    NFSe.Servico.ItemServico.Items[I].BaseCalculo := AINIRec.ReadFloat(LSecao, 'BaseCalculo', 0);
    NFSe.Servico.ItemServico.Items[I].ValorTotal := AINIRec.ReadFloat(LSecao, 'ValorTotal', 0);

    LerINISecaoDadosDeducao(AIniRec, I);
    LerINISecaoDadosProssionalParceiro(AIniRec, I);

    Inc(I);
  end;
end;

procedure TNFSeR_ABRASFv1.LerINISecaoDadosDeducao(const AINIRec: TMemIniFile;
  const AIndice: Integer);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoDadosProssionalParceiro(
  const AINIRec: TMemIniFile; const AIndice: Integer);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoComercioExterior(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoLocacaoSubLocacao(
  const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoEvento(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoRodoviaria(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoInformacoesComplementares(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoValores(const AINIRec: TMemIniFile);
var
  LSecao: String;
  Ok: Boolean;
begin
  LSecao := 'Valores';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.Servico.Valores.ValorServicos := AINIRec.ReadFloat(LSecao, 'ValorServicos', 0.0);
    NFSe.Servico.Valores.ValorDeducoes := AINIRec.ReadFloat(LSecao, 'ValorDeducoes', 0.0);
    NFSe.Servico.Valores.ValorTotalRecebido := AINIRec.ReadFloat(LSecao, 'ValorTotalRecebido', 0.0);
    NFSe.Servico.Valores.ValorPis := AINIRec.ReadFloat(LSecao, 'ValorPis', 0.0);
    NFSe.Servico.Valores.ValorCofins := AINIRec.ReadFloat(LSecao, 'ValorCofins', 0.0);
    NFSe.Servico.Valores.ValorInss := AINIRec.ReadFloat(LSecao, 'ValorInss', 0.0);
    NFSe.Servico.Valores.ValorIr := AINIRec.ReadFloat(LSecao, 'ValorIr', 0.0);
    NFSe.Servico.Valores.ValorCsll := AINIRec.ReadFloat(LSecao, 'ValorCsll', 0.0);
    NFSe.Servico.Valores.IssRetido := FpAOwner.StrToSituacaoTributaria(Ok, AINIRec.ReadString(LSecao, 'ISSRetido', ''));
    NFSe.Servico.Valores.ValorIss := AINIRec.ReadFloat(LSecao, 'ValorIss', 0.0);
    NFSe.Servico.Valores.ValorIssRetido := AINIRec.ReadFloat(LSecao, 'ValorIssRetido', 0.0);
    NFSe.Servico.Valores.OutrasRetencoes := AINIRec.ReadFloat(LSecao, 'OutrasRetencoes', 0.0);
    NFSe.Servico.Valores.BaseCalculo := AINIRec.ReadFloat(LSecao, 'BaseCalculo', 0.0);
    NFSe.Servico.Valores.Aliquota := AINIRec.ReadFloat(LSecao, 'Aliquota', 0.0);
    NFSe.Servico.Valores.ValorLiquidoNfse := AINIRec.ReadFloat(LSecao, 'ValorLiquidoNfse', 0.0);
    NFSe.Servico.Valores.DescontoIncondicionado := StringToFloatDef(AINIRec.ReadString(LSecao, 'DescontoIncondicionado', ''), 0);
    NFSe.Servico.Valores.DescontoCondicionado := StringToFloatDef(AINIRec.ReadString(LSecao, 'DescontoCondicionado', ''), 0);
    {
    NFSe.Servico.Valores.ValorTotalNotaFiscal := AINIRec.ReadFloat(LSecao, 'ValorTotalNotaFiscal', 0.0);
    NFSe.Servico.Valores.ValorTotalTributos := AINIRec.ReadFloat(LSecao, 'ValorTotalTributos', 0.0);
    NFSe.Servico.Valores.RetencoesFederais := AINIRec.ReadFloat(LSecao, 'RetencoesFederais', 0.0);
    }
  end;
end;

procedure TNFSeR_ABRASFv1.LerINISecaoValoresNFSe(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoCondicaoPagamento(
  const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv1.LerINISecaoOrgaoGerador(const AINIRec: TMemIniFile);
var
  LSecao: String;
begin
  LSecao := 'OrgaoGerador';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.OrgaoGerador.CodigoMunicipio := AINIRec.ReadString(LSecao, 'CodigoMunicipio', '');
    NFSe.OrgaoGerador.Uf := AINIRec.ReadString(LSecao, 'UF', '');
  end;
end;

procedure TNFSeR_ABRASFv1.LerINISecaoParcelas(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

end.
