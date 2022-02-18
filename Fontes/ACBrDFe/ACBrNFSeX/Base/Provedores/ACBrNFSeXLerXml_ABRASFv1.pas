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
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXLerXml;

type
  { TNFSeR_ABRASFv1 }

  TNFSeR_ABRASFv1 = class(TNFSeRClass)
  private

  protected
    procedure LerInfNfse(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);
    procedure LerServico(const ANode: TACBrXmlNode);
    procedure LerValores(const ANode: TACBrXmlNode);

    procedure LerPrestadorServico(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestadorServico(const ANode: TACBrXmlNode; aTag: string);
    procedure LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
    procedure LerContatoPrestador(const ANode: TACBrXmlNode);

    procedure LerTomadorServico(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerContatoTomador(const ANode: TACBrXmlNode);

    procedure LerIntermediarioServico(const ANode: TACBrXmlNode);

    procedure LerOrgaoGerador(const ANode: TACBrXmlNode);
    procedure LerConstrucaoCivil(const ANode: TACBrXmlNode);

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
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;

  end;

implementation

uses
  ACBrUtil, ACBrConsts,
  ACBrNFSeXConversao;

//==============================================================================
// Essa unit tem por finalidade exclusiva de Ler o XML da NFS-e e RPS dos provedores
//     que seguem a versão 1.xx do layout da ABRASF
//==============================================================================

{ TNFSeR_ABRASFv1 }

procedure TNFSeR_ABRASFv1.LerConfirmacao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Confirmacao');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('ConfirmacaoCancelamento');

  if AuxNode <> nil then
  begin
    LerPedido(AuxNode);
    LerInfConfirmacaoCancelamento(AuxNode);

    with NFSe.NfseCancelamento do
    begin
      DataHora := ObterConteudo(AuxNode.Childrens.FindAnyNs('DataHoraCancelamento'), tcDat);

      if DataHora > 0 then
        NFSe.SituacaoNfse := snCancelado;
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerConstrucaoCivil(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  aTag: string);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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

      if CodigoMunicipio = '' then
        CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);

      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);

      if UF = '' then
        UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('Estado'), tcStr);

      CodigoPais := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoPais'), tcInt);
      CEP        := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
      xMunicipio := CodIBGEToCidade(StrToIntDef(CodigoMunicipio, 0));
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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

      if CodigoMunicipio = '' then
        CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);

      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);

      if UF = '' then
        UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('Estado'), tcStr);

      CEP        := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
      xMunicipio := CodIBGEToCidade(StrToIntDef(CodigoMunicipio, 0));
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerIdentificacaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  end;
end;

procedure TNFSeR_ABRASFv1.LerIdentificacaoRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoRps');

  if AuxNode <> nil then
  begin
    with NFSe.IdentificacaoRps do
    begin
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Serie  := ObterConteudo(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);
      Tipo   := StrToTipoRPS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Tipo'), tcStr));
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerIdentificacaoTomador(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerInfConfirmacaoCancelamento(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfConfirmacaoCancelamento');

  NFSe.SituacaoNfse := snNormal;

  if AuxNode <> nil then
  begin
    with NFSe.NfseCancelamento do
    begin
      Sucesso  := ObterConteudo(AuxNode.Childrens.FindAnyNs('Sucesso'), tcBool);
      DataHora := ObterConteudo(AuxNode.Childrens.FindAnyNs('DataHora'), tcDat);

      if DataHora > 0 then
        NFSe.SituacaoNfse := snCancelado;
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerInfNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfNfse');

  if AuxNode <> nil then
  begin
    NFSe.Numero            := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
    NFSe.CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
    NFSe.DataEmissao       := ObterConteudo(AuxNode.Childrens.FindAnyNs('DataEmissao'), tcDat);
    NFSe.NfseSubstituida   := ObterConteudo(AuxNode.Childrens.FindAnyNs('NfseSubstituida'), tcStr);

    LerIdentificacaoRps(AuxNode);

    NFSe.DataEmissaoRps           := ObterConteudo(AuxNode.Childrens.FindAnyNs('DataEmissaoRps'), tcDat);
    NFSe.NaturezaOperacao         := StrToNaturezaOperacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('NaturezaOperacao'), tcStr));
    NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RegimeEspecialTributacao'), tcStr));
    NFSe.OptanteSimplesNacional   := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr));
    NFSe.IncentivadorCultural     := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('IncentivadorCultural'), tcStr));
    NFSe.Competencia              := ObterConteudo(AuxNode.Childrens.FindAnyNs('Competencia'), tcDat);
    NFSe.NfseSubstituida          := ObterConteudo(AuxNode.Childrens.FindAnyNs('NfseSubstituida'), tcStr);
    NFSe.OutrasInformacoes        := ObterConteudo(AuxNode.Childrens.FindAnyNs('OutrasInformacoes'), tcStr);

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
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('NfseCancelamento');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('CancelamentoNfse');

  LerConfirmacao(AuxNode);
end;

procedure TNFSeR_ABRASFv1.LerNfseSubstituicao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('NfseSubstituicao');

  LerSubstituicaoNfse(AuxNode);
end;

procedure TNFSeR_ABRASFv1.LerInfPedidoCancelamento(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IntermediarioServico');

  if AuxNode <> nil then
  begin
    NFSe.IntermediarioServico.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);

    AuxNodeCpfCnpj := AuxNode.Childrens.FindAnyNs('CpfCnpj');

    with NFSe.IntermediarioServico do
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

procedure TNFSeR_ABRASFv1.LerOrgaoGerador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('RpsSubstituido');

  if AuxNode <> nil then
  begin
    with NFSe.RpsSubstituido do
    begin
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Serie  := ObterConteudo(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);
      Tipo   := StrToTipoRPS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Tipo'), tcStr));
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  CodigoItemServico: string;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
      CodigoMunicipio           := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);

      if CodigoMunicipio = '' then
        CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('MunicipioPrestacaoServico'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerSubstituicaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
      ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorLiquidoNfse'), tcDe2);
      ValorIssRetido := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIssRetido'), tcDe2);
      DescontoCondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescontoCondicionado'), tcDe2);
      DescontoIncondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescontoIncondicionado'), tcDe2);

      ValorLiq := ValorServicos - ValorPis - ValorCofins - ValorInss -
                  ValorIr - ValorCsll - OutrasRetencoes - ValorIssRetido -
                  DescontoIncondicionado - DescontoCondicionado;

      if (ValorLiquidoNfse = 0) or (ValorLiquidoNfse <> ValorLiq) then
        ValorLiquidoNfse := ValorLiq;
    end;
  end;
end;

function TNFSeR_ABRASFv1.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

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

  FreeAndNil(FDocument);
end;

function TNFSeR_ABRASFv1.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode1, AuxNode2: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

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
end;

function TNFSeR_ABRASFv1.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfRps');

  if AuxNode <> nil then
  begin
    LerIdentificacaoRps(AuxNode);

    with NFSe do
    begin
      DataEmissao := ObterConteudo(AuxNode.Childrens.FindAnyNs('DataEmissao'), tcDat);
      NaturezaOperacao := StrToNaturezaOperacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('NaturezaOperacao'), tcStr));
      RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RegimeEspecialTributacao'), tcStr));
      OptanteSimplesNacional := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr));
      IncentivadorCultural := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('IncentivadorCultural'), tcStr));
      StatusRps := StrToStatusRPS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Status'), tcStr));
    end;

    LerRpsSubstituido(AuxNode);
    LerServico(AuxNode);
    LerPrestador(AuxNode);
    LerTomadorServico(AuxNode);
    LerIntermediarioServico(AuxNode);
    LerConstrucaoCivil(AuxNode);
  end;
end;

end.
