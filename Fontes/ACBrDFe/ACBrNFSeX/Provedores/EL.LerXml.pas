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

unit EL.LerXml;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils, Math,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml,
  ACBrNFSeXLerXml_ABRASFv2;

type
  { Provedor com layout próprio }
  { TNFSeR_EL }

  TNFSeR_EL = class(TNFSeRClass)
  protected

    procedure LerIdNota(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoNfse(const ANode: TACBrXmlNode);
    procedure LerDadosPrestador(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestador(const ANode: TACBrXmlNode);
    procedure LerContatoPrestador(const ANode: TACBrXmlNode);
    procedure LerDadosTomador(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerContatoTomador(const ANode: TACBrXmlNode);
    procedure LerServicos(const ANode: TACBrXmlNode);
    procedure LerValores(const ANode: TACBrXmlNode);
    procedure LerObservacao(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

  { TNFSeR_EL204 }

  TNFSeR_EL204 = class(TNFSeR_ABRASFv2)
  protected

  public

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     EL
//==============================================================================

{ TNFSeR_EL }

procedure TNFSeR_EL.LerContatoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Contato');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Contato do
    begin
      Telefone := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Telefone'), tcStr);
      Email    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_EL.LerContatoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Contato');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Contato do
    begin
      Telefone := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Telefone'), tcStr);
      Email    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_EL.LerDadosPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DadosPrestador');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      NaturezaOperacao         := StrToNaturezaOperacao(Ok, ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NaturezaOperacao'), tcStr));
      RegimeEspecialTributacao := StrToRegimeEspecialTributacao(Ok, ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RegimeEspecialTributacao'), tcStr));
      OptanteSimplesNacional   := StrToSimNao(Ok, ProcessarConteudo(AuxNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr));
      IncentivadorCultural     := StrToSimNao(Ok, ProcessarConteudo(AuxNode.Childrens.FindAnyNs('IncentivadorCultural'), tcStr));

      with Prestador do
      begin
        RazaoSocial  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
        NomeFantasia := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NomeFantasia'), tcStr);
      end;
    end;

    LerIdentificacaoPrestador(AuxNode);
    LerEnderecoPrestador(AuxNode);
    LerContatoPrestador(AuxNode);
  end;
end;

procedure TNFSeR_EL.LerDadosTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DadosTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      RazaoSocial  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
      NomeFantasia := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NomeFantasia'), tcStr);
    end;

    LerIdentificacaoTomador(AuxNode);
    LerEnderecoTomador(AuxNode);
    LerContatoTomador(AuxNode);
  end;
end;

procedure TNFSeR_EL.LerEnderecoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      Endereco        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('LogradouroNumero'), tcStr);
      Complemento     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('LogradouroComplemento'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
      xMunicipio      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Municipio'), tcStr);
    end;
  end;
end;

procedure TNFSeR_EL.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Endereco        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('LogradouroNumero'), tcStr);
      Complemento     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('LogradouroComplemento'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
      xMunicipio      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Municipio'), tcStr);
    end;
  end;
end;

procedure TNFSeR_EL.LerIdentificacaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoNfse');

  if AuxNode <> nil then
  begin
    with NFSe.IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Serie  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);
      Tipo   := StrToTipoRPS(Ok, ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Tipo'), tcStr));

      NFSe.InfID.ID := Numero;
    end;
  end;
end;

procedure TNFSeR_EL.LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoPrestador');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.IdentificacaoPrestador do
    begin
      Cnpj               := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CpfCnpj'), tcStr);
      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
      InscricaoEstadual  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('InscricaoEstadual'), tcStr);
    end;
  end;
end;

procedure TNFSeR_EL.LerIdentificacaoRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoRps');

  if AuxNode <> nil then
  begin
    with NFSe.IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Serie  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);
      Tipo   := StrToTipoRPS(Ok, ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Tipo'), tcStr));
    end;
  end;
end;

procedure TNFSeR_EL.LerIdentificacaoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.IdentificacaoTomador do
    begin
      CpfCnpj            := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CpfCnpj'), tcStr);
      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
      InscricaoEstadual  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('InscricaoEstadual'), tcStr);
    end;
  end;
end;

procedure TNFSeR_EL.LerIdNota(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('idNota');

  if AuxNode <> nil then
  begin
    with NFSe do
      CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('idNota'), tcStr);
  end;
end;

procedure TNFSeR_EL.LerObservacao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Observacao');

  if AuxNode <> nil then
    NFSe.OutrasInformacoes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Observacao'), tcStr);
end;

procedure TNFSeR_EL.LerServicos(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: integer;
  aValorTotal: Double;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Servicos');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('Servico');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemListaServico := OnlyNumber(ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('CodigoServico116'), tcStr));
      NFSe.Servico.CodigoCnae := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('CodigoCnae'), tcStr);

      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        CodLCServ     := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('CodigoServico116'), tcStr);
        CodServ       := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('CodigoServicoMunicipal'), tcStr);
        Quantidade    := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('Quantidade'), tcDe4);
        Unidade       := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('Unidade'), tcStr);
        ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('ValorServico'), tcDe2);
        Descricao     := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('Descricao'), tcStr);
        Aliquota      := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('Aliquota'), tcDe2);
        ValorISS      := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('ValorIssqn'), tcDe4);

        aValorTotal := Quantidade * ValorUnitario;

        ValorTotal := RoundTo(aValorTotal, - 2);
      end;
    end;
  end;
end;

procedure TNFSeR_EL.LerValores(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Valores');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorServicos    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
      ValorIss         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorIss'), tcDe2);
      ValorLiquidoNfse := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorLiquidoNfse'), tcDe2);
      ValorDeducoes    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorDeducoes'), tcDe2);
      ValorPis         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorPis'), tcDe2);
      ValorCofins      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
      ValorInss        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorInss'), tcDe2);
      ValorIr          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorIr'), tcDe2);
      ValorCsll        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);
      OutrasRetencoes  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('OutrasRetencoes'), tcDe2);
      ValorIssRetido   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorIssRetido'), tcDe2);
      OutrosDescontos  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('OutrosDescontos'), tcDe2);
      BaseCalculo      := ValorServicos - ValorDeducoes;
    end;
  end;
end;

function TNFSeR_EL.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
  xRetorno: string;
begin
  xRetorno := TratarXmlRetorno(Arquivo);

  if EstaVazio(xRetorno) then
    raise Exception.Create('Arquivo xml não carregado.');

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(xRetorno);

//  if (Pos('notasFiscais', xRetorno) > 0) then
  if (Pos('Nfse', xRetorno) > 0) then
    tpXML := txmlNFSe
  else
    tpXML := txmlRPS;

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);

  FreeAndNil(FDocument);
end;

function TNFSeR_EL.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  Ok: Boolean;
begin
  Result := True;
  NFSe.SituacaoNfse := snNormal;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('notasFiscais');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('nfeRpsNotaFiscal');

  if AuxNode <> nil then
  begin
    NFSe.Numero            := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('numero'), tcStr);
    NFSe.CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('idRps'), tcStr);
    NFSe.DataEmissao       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dataProcessamento'), tcDatHor);

    NFSe.IdentificacaoRps.Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('rpsNumero'), tcStr);

    aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('situacao'), tcStr);

    if aValor <> 'A' then
      NFSe.SituacaoNfse := snCancelado;

    LerIdNota(AuxNode);
  end;

  AuxNode := ANode.Childrens.FindAnyNs('Nfse');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      CodigoVerificacao := ProcessarConteudo(ANode.Childrens.FindAnyNs('Id'), tcStr);
      Link              := CodigoVerificacao;
      DataEmissao       := ProcessarConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDatHor);
      OutrasInformacoes := ProcessarConteudo(ANode.Childrens.FindAnyNs('Observacao'), tcStr);
      Status            := StrToStatusRPS(Ok, ProcessarConteudo(ANode.Childrens.FindAnyNs('Status'), tcStr));

      Servico.Valores.IssRetido := StrToSituacaoTributaria(Ok, ProcessarConteudo(ANode.Childrens.FindAnyNs('IssRetido'), tcStr));
    end;

    LerIdentificacaoNfse(AuxNode);
    LerDadosPrestador(AuxNode);
    LerDadosTomador(AuxNode);
    LerServicos(AuxNode);
    LerValores(AuxNode);
    LerObservacao(AuxNode);
  end;
end;

function TNFSeR_EL.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  Ok: Boolean;
begin
  Result := True;

  with NFSe do
  begin
    CodigoVerificacao := ProcessarConteudo(ANode.Childrens.FindAnyNs('Id'), tcStr);
    DataEmissao       := ProcessarConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDatHor);
    OutrasInformacoes := ProcessarConteudo(ANode.Childrens.FindAnyNs('Observacao'), tcStr);
    Status            := StrToStatusRPS(Ok, ProcessarConteudo(ANode.Childrens.FindAnyNs('Status'), tcStr));

    Servico.Valores.IssRetido := StrToSituacaoTributaria(Ok, ProcessarConteudo(ANode.Childrens.FindAnyNs('IssRetido'), tcStr));
  end;

  LerIdentificacaoRps(ANode);
  LerDadosPrestador(ANode);
  LerDadosTomador(ANode);
  LerServicos(ANode);
  LerValores(ANode);
end;

end.
