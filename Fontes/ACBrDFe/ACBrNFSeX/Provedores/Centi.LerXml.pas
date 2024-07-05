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

unit Centi.LerXml;

interface

uses
//  ACBrNFSeXLerXml_ABRASFv2;
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXLerXml;

type
  { TNFSeR_Centi202 }

  TNFSeR_Centi202 = class(TNFSeRClass)
  protected
    function LerCompetencia(const ANode: TACBrXmlNode): TDateTime; virtual;
    function LerDataEmissaoRps(const ANode: TACBrXmlNode): TDateTime; virtual;
    function LerDataEmissao(const ANode: TACBrXmlNode): TDateTime; virtual;

    procedure LerInfDeclaracaoPrestacaoServico(const ANode: TACBrXmlNode);
    procedure LerRps(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);

    procedure LerRpsSubstituido(const ANode: TACBrXmlNode);

    procedure LerServico(const ANode: TACBrXmlNode);
    procedure LerValores(const ANode: TACBrXmlNode);

    procedure LerPrestador(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestador(const ANode: TACBrXmlNode; const aTag: string);
    procedure LerContatoPrestador(const ANode: TACBrXmlNode);

    procedure LerTomadorServico(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerContatoTomador(const ANode: TACBrXmlNode);

    procedure LerIntermediarioServico(const ANode: TACBrXmlNode);

    procedure LerConstrucaoCivil(const ANode: TACBrXmlNode);

    procedure LerInfNfse(const ANode: TACBrXmlNode);

    procedure LerValoresNfse(const ANode: TACBrXmlNode);

    procedure LerDeclaracaoPrestacaoServico(const ANode: TACBrXmlNode);
    {
    function LerDataHoraCancelamento(const ANode: TACBrXmlNode): TDateTime; virtual;
    function LerDataHora(const ANode: TACBrXmlNode): TDateTime; virtual;

    procedure LerPrestadorServico(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestadorServico(const ANode: TACBrXmlNode; aTag: string);
    procedure LerContatoPrestadorServico(const ANode: TACBrXmlNode);
    procedure LerContatoPrestador(const ANode: TACBrXmlNode);

    procedure LerOrgaoGerador(const ANode: TACBrXmlNode);

    procedure LerListaServicos(const ANode: TACBrXmlNode);
    procedure LerServicos(const ANode: TACBrXmlNode);


    procedure LerNfseCancelamento(const ANode: TACBrXmlNode);
    procedure LerConfirmacao(const ANode: TACBrXmlNode);
    procedure LerPedido(const ANode: TACBrXmlNode);
    procedure LerInfConfirmacaoCancelamento(const ANode: TACBrXmlNode);
    procedure LerInfPedidoCancelamento(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoNfse(const ANode: TACBrXmlNode);

    procedure LerNfseSubstituicao(const ANode: TACBrXmlNode);
    procedure LerSubstituicaoNfse(const ANode: TACBrXmlNode);

    procedure LerDadosExtras(const ANode: TACBrXmlNode);
    }
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings,
  ACBrConsts,
  ACBrNFSeXConversao;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     Centi
//==============================================================================

{ TNFSeR_Centi202 }

function TNFSeR_Centi202.LerCompetencia(const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('Competencia'), tcDat);
end;

procedure TNFSeR_Centi202.LerConstrucaoCivil(const ANode: TACBrXmlNode);
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

procedure TNFSeR_Centi202.LerContatoPrestador(const ANode: TACBrXmlNode);
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
      Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('Email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Centi202.LerContatoTomador(const ANode: TACBrXmlNode);
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

function TNFSeR_Centi202.LerDataEmissao(const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDatHor);
end;

function TNFSeR_Centi202.LerDataEmissaoRps(
  const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDatHor);
end;

procedure TNFSeR_Centi202.LerDeclaracaoPrestacaoServico(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');

  if AuxNode <> nil then
    LerInfDeclaracaoPrestacaoServico(AuxNode);
end;

procedure TNFSeR_Centi202.LerEnderecoPrestador(const ANode: TACBrXmlNode;
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
      Numero          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento     := ObterConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      UF              := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
      CodigoPais      := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoPais'), tcInt);
      CEP             := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
      xMunicipio      := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

procedure TNFSeR_Centi202.LerEnderecoTomador(const ANode: TACBrXmlNode);
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
      Numero          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento     := ObterConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      UF              := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
      CEP             := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
      xMunicipio      := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

procedure TNFSeR_Centi202.LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoPrestador');

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

      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
    end;
  end
  else
  begin
    with NFSe.Prestador.IdentificacaoPrestador do
    begin
      CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('Cnpj'), tcStr);

      if CpfCnpj = '' then
      begin
        AuxNodeCpfCnpj := ANode.Childrens.FindAnyNs('CpfCnpj');

        if AuxNodeCpfCnpj <> nil then
        begin
          CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cpf'), tcStr);

          if CpfCnpj = '' then
            CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cnpj'), tcStr);
        end;
      end;

      InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
    end;

    if NFSe.Prestador.RazaoSocial = '' then
    begin
      NFSe.Prestador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
      NFSe.Prestador.RazaoSocial := StringReplace(NFSe.Prestador.RazaoSocial, '&amp;', '&', [rfReplaceAll]);
    end;
  end;
end;

procedure TNFSeR_Centi202.LerIdentificacaoRps(const ANode: TACBrXmlNode);
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

procedure TNFSeR_Centi202.LerIdentificacaoTomador(const ANode: TACBrXmlNode);
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

      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
      InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoEstadual'), tcStr);
    end;

    if NFSe.Tomador.RazaoSocial = '' then
    begin
      NFSe.Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
      NFSe.Tomador.RazaoSocial := StringReplace(NFSe.Tomador.RazaoSocial, '&amp;', '&', [rfReplaceAll]);
    end;

    LerEnderecoTomador(AuxNode);
    LerContatoTomador(AuxNode);
  end;
end;

procedure TNFSeR_Centi202.LerInfDeclaracaoPrestacaoServico(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');

  if AuxNode <> nil then
  begin
    LerRps(AuxNode);

    NFSe.Competencia := LerCompetencia(AuxNode);

//    LerListaServicos(AuxNode);
    LerServico(AuxNode);
    LerPrestador(AuxNode);
    LerTomadorServico(AuxNode);
    LerIntermediarioServico(AuxNode);
    LerConstrucaoCivil(AuxNode);

    NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RegimeEspecialTributacao'), tcStr));
    NFSe.OptanteSimplesNacional   := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr));
    NFSe.IncentivadorCultural     := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('IncentivoFiscal'), tcStr));

    if NFSe.InformacoesComplementares = '' then
    begin
      NFSe.InformacoesComplementares := ObterConteudo(AuxNode.Childrens.FindAnyNs('InformacoesComplementares'), tcStr);
      NFSe.InformacoesComplementares := StringReplace(NFSe.InformacoesComplementares, '&lt;br&gt;', ';', [rfReplaceAll]);
    end;
  end;
end;

procedure TNFSeR_Centi202.LerInfNfse(const ANode: TACBrXmlNode);
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
    NFSe.InfID.ID := ObterConteudoTag(AuxNode.Attributes.Items[IdAttr]);

    NFSe.Numero            := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
    NFSe.CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
    NFSe.DataEmissao       := LerDataEmissao(AuxNode);
    NFSe.NfseSubstituida   := ObterConteudo(AuxNode.Childrens.FindAnyNs('NfseSubstituida'), tcStr);
    NFSe.IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('IdentificacaoRps'), tcStr);
    NFSe.OptanteSimplesNacional := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr));
    NFSe.OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('OutrasInformacoes'), tcStr);
    NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
    NFSe.StatusRps := FpAOwner.StrToStatusRPS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Status'), tcStr));
    NFSe.InformacoesComplementares := ObterConteudo(AuxNode.Childrens.FindAnyNs('Observacao'), tcStr);
    NFSe.Servico.Valores.IssRetido := FpAOwner.StrToSituacaoTributaria(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('IssRetido'), tcStr));

    LerValoresNfse(AuxNode);
    LerDeclaracaoPrestacaoServico(AuxNode);
    LerConstrucaoCivil(AuxNode);
    {
    NFSe.ValorCredito := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCredito'), tcDe2);

    LerServico(AuxNode);
    LerTomadorServico(AuxNode);
    LerPrestadorServico(AuxNode);
    LerEnderecoPrestadorServico(AuxNode, 'EnderecoPrestadorServico');
    LerOrgaoGerador(AuxNode);
    }
//    LerDadosExtras(AuxNode);
  end;
end;

procedure TNFSeR_Centi202.LerIntermediarioServico(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Intermediario');

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

      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Centi202.LerPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Prestador');

  if AuxNode <> nil then
  begin
    LerIdentificacaoPrestador(AuxNode);
    LerEnderecoPrestador(AuxNode, 'Endereco');
    LerContatoPrestador(AuxNode);
  end;
end;

procedure TNFSeR_Centi202.LerRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Rps');

  if AuxNode = nil then
    AuxNode := ANode;

  if AuxNode <> nil then
  begin
    LerIdentificacaoRps(AuxNode);

    NFSe.DataEmissaoRps := LerDataEmissaoRps(AuxNode);

    NFSe.StatusRps := FpAOwner.StrToStatusRPS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Status'), tcStr));

    if NFSe.StatusRps = srCancelado then
      NFSe.SituacaoNfse := snCancelado;

    LerRpsSubstituido(AuxNode);
  end;
end;

procedure TNFSeR_Centi202.LerRpsSubstituido(const ANode: TACBrXmlNode);
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

procedure TNFSeR_Centi202.LerServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
  CodigoItemServico, Responsavel: string;
  ValorLiq: Double;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Servico');

  if AuxNode <> nil then
  begin
    LerValores(AuxNode);

    CodigoItemServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('ItemListaServico'), tcStr);

    with NFSe.Servico do
    begin
      Responsavel := ObterConteudo(AuxNode.Childrens.FindAnyNs('ResponsavelRetencao'), tcStr);

      if Responsavel = '' then
        ResponsavelRetencao := rtNenhum
      else
        ResponsavelRetencao := FpAOwner.StrToResponsavelRetencao(Ok, Responsavel);

      ItemListaServico          := NormatizarItemListaServico(CodigoItemServico);

      CodigoItemServico := Copy(ItemListaServico, 1, 5);

      xItemListaServico         := ItemListaServicoDescricao(CodigoItemServico);
      CodigoCnae                := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoCnae'), tcStr);
      CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoTributacaoMunicipio'), tcStr);
      Discriminacao             := ObterConteudo(AuxNode.Childrens.FindAnyNs('Discriminacao'), tcStr);
      Discriminacao := StringReplace(Discriminacao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

      VerificarSeConteudoEhLista(Discriminacao);

      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);

      CodigoPais          := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoPais'), tcInt);
      ExigibilidadeISS    := FpAOwner.StrToExigibilidadeISS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('ExigibilidadeISS'), tcStr));
      MunicipioIncidencia := ObterConteudo(AuxNode.Childrens.FindAnyNs('MunicipioIncidencia'), tcInt);
      NumeroProcesso      := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroProcesso'), tcStr);

      if Valores.IssRetido = stRetencao then
        Valores.ValorIssRetido := Valores.ValorIss
      else
        Valores.ValorIssRetido := 0;

      ValorLiq := Valores.ValorServicos - Valores.RetencoesFederais -
                  Valores.OutrasRetencoes - Valores.ValorIssRetido -
                  Valores.DescontoIncondicionado - Valores.DescontoCondicionado;

      if (Valores.ValorLiquidoNfse = 0) or (Valores.ValorLiquidoNfse <> ValorLiq) then
        Valores.ValorLiquidoNfse := ValorLiq;
    end;
  end;
end;

procedure TNFSeR_Centi202.LerTomadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('TomadorServico');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('Tomador');

  if AuxNode <> nil then
  begin
    NFSe.Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
    NFSe.Tomador.RazaoSocial := StringReplace(NFSe.Tomador.RazaoSocial, '&amp;', '&', [rfReplaceAll]);

    LerIdentificacaoTomador(AuxNode);

    LerEnderecoTomador(AuxNode);
    LerContatoTomador(AuxNode);
  end;
end;

procedure TNFSeR_Centi202.LerValores(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Valores');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorServicos   := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);

      if ValorServicos = 0 then
        ValorServicos := ValorLiquidoNfse;

      ValorDeducoes   := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorDeducoes'), tcDe2);

      if AliquotaPis = 0 then
        AliquotaPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaPis'), tcDe2);

      RetidoPis := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RetidoPis'), tcStr));

      if ValorPis = 0 then
        ValorPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorPis'), tcDe2);

      if AliquotaCofins = 0 then
        AliquotaCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaCofins'), tcDe2);

      RetidoCofins := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RetidoCofins'), tcStr));

      if ValorCofins = 0 then
        ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);

      if AliquotaInss = 0 then
        AliquotaInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaInss'), tcDe2);

      RetidoInss := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RetidoInss'), tcStr));

      if ValorInss = 0 then
        ValorInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorInss'), tcDe2);

      if AliquotaIr = 0 then
        AliquotaIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaIr'), tcDe2);

      RetidoIr := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RetidoIr'), tcStr));

      if ValorIr = 0 then
        ValorIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIr'), tcDe2);

      if AliquotaCsll = 0 then
        AliquotaCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaCsll'), tcDe2);

      RetidoCsll := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RetidoCsll'), tcStr));

      if ValorCsll = 0 then
        ValorCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);

      if ValorIss = 0 then
        ValorIss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIss'), tcDe2);

      RetencoesFederais := ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll;

      OutrasRetencoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('OutrasRetencoes'), tcDe2);

      if NFSe.ValoresNfse.BaseCalculo = 0 then
      begin
        BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('BaseCalculo'), tcDe2);
        NFSe.ValoresNfse.BaseCalculo := BaseCalculo;
      end;

      if NFSe.ValoresNfse.ValorIss = 0 then
      begin
        ValorIss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIss'), tcDe2);
        NFSe.ValoresNfse.ValorIss := ValorIss;
      end;

      if Aliquota = 0 then
        Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe4);

      if NFSe.ValoresNfse.ValorLiquidoNfse = 0 then
      begin
        ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorLiquidoNfse'), tcDe2);
        NFSe.ValoresNfse.ValorLiquidoNfse := ValorLiquidoNfse;
      end;

      DescontoCondicionado   := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescontoCondicionado'), tcDe2);
      DescontoIncondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescontoIncondicionado'), tcDe2);

      ValorTotalNotaFiscal := ValorServicos - DescontoCondicionado -
                              DescontoIncondicionado;
    end;
  end;
end;

procedure TNFSeR_Centi202.LerValoresNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('ValoresNfse');

  if AuxNode <> nil then
  begin
    with NFSe.ValoresNfse do
    begin
      BaseCalculo      := ObterConteudo(AuxNode.Childrens.FindAnyNs('BaseCalculo'), tcDe2);
      Aliquota         := ObterConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe4);
      ValorIss         := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIss'), tcDe2);
      ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorLiquidoNfse'), tcDe2);
    end;

    with NFSe.Servico.Valores do
    begin
      AliquotaIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaIr'), tcDe2);
      AliquotaInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaInss'), tcDe2);
      AliquotaPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaPis'), tcDe2);
      AliquotaCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaCofins'), tcDe2);
      AliquotaCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaCsll'), tcDe2);

      ValorIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIr'), tcDe2);
      ValorInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorInss'), tcDe2);
      ValorPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorPis'), tcDe2);
      ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
      ValorCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);
      ValorServicos := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
      DescontoIncondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescontoIncondicionado'), tcDe2);
      DescontoCondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescontoCondicionado'), tcDe2);

      BaseCalculo := NFSe.ValoresNfse.BaseCalculo;
      Aliquota := NFSe.ValoresNfse.Aliquota;
      ValorIss := NFSe.ValoresNfse.ValorIss;
      ValorLiquidoNfse := NFSe.ValoresNfse.ValorLiquidoNfse;

      if (ValorLiquidoNfse > 0) and (ValorServicos <= 0) then
         ValorServicos := ValorLiquidoNfse;

      RetencoesFederais := ValorIr + ValorInss + ValorPis + ValorCofins + ValorCsll;

      ValorTotalNotaFiscal := ValorServicos - DescontoCondicionado - DescontoIncondicionado;
    end;
  end;
end;

function TNFSeR_Centi202.LerXml: Boolean;
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

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);

  if NFSe.Tomador.RazaoSocial = '' then
    NFSe.Tomador.RazaoSocial := 'Tomador Não Identificado';

  FreeAndNil(FDocument);
end;

function TNFSeR_Centi202.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Nfse');

  if AuxNode = nil then
    AuxNode := ANode;

  LerInfNfse(AuxNode);

//  LerNfseCancelamento(ANode);
//  LerNfseSubstituicao(ANode);

  LerCampoLink;
end;

function TNFSeR_Centi202.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  LerInfDeclaracaoPrestacaoServico(ANode);
end;

end.
