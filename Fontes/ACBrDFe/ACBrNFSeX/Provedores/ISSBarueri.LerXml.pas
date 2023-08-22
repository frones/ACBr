{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ISSBarueri.LerXml;

interface

uses
  SysUtils, Classes, StrUtils, MaskUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXConversao, ACBrNFSeXClass, ACBrNFSeXLerXml;

type
  { Provedor com layout próprio }

  { TNFSeR_ISSBarueri }

  TNFSeR_ISSBarueri = class(TNFSeRClass)
  private
    function StrToSituacaoTributaria(const AIssRetido: String): TnfseSituacaoTributaria;
    function StrToStatusNFSe(const ASituacao: String): TStatusNFSe;

    procedure LerValoresNfe(const ANode: TACBrXmlNode);
    procedure LerPrestadorServico(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestador(const ANode: TACBrXmlNode);
    procedure LerDeclaracaoServicoPrestado(const ANode: TACBrXmlNode);
    procedure LerRps(const ANode: TACBrXmlNode);
    procedure LerServicoPrestado(const ANode: TACBrXmlNode);
    procedure LerValoresServicoPrestado(const ANode: TACBrXmlNode);
    procedure LerFatura(const ANode: TACBrXmlNode);
    procedure LerTomadorServico(const ANode: TACBrXmlNode);
    procedure LerTomadorServicoContato(const ANode: TACBrXmlNode);
    procedure LerTomadorServicoEndereco(const ANode: TACBrXmlNode);
    procedure LerTomadorServicoIdentificacao(const ANode: TACBrXmlNode);
    procedure LerCartaCorrecao(const ANode: TACBrXmlNode);
    procedure LerCancelamentoNFe(const ANode: TACBrXmlNode);

    procedure LerRegistroTipo2(const ALinha: String);
    procedure LerRegistroTipo3(const ALinha: String);
    procedure LerRegistroTipo4(const ALinha: String);

  protected
    function TipodeXMLLeitura(const aArquivo: string): TtpXML; override;
    function IsXML(const aArquivo: String): Boolean;

  public
    function LerXml: Boolean; override;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;

  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do RPS da prefeitura de:
//     ISSBarueri
//==============================================================================

{ TNFSeR_ISSBarueri }

function TNFSeR_ISSBarueri.StrToSituacaoTributaria(const AIssRetido: String): TnfseSituacaoTributaria;
begin
  Result := stNormal;

  if (AIssRetido = 'S') then
    Result := stRetencao;
end;

function TNFSeR_ISSBarueri.StrToStatusNFSe(const ASituacao: String): TStatusNFSe;
begin
  Result := snNormal;

  if (ASituacao = 'C') then
    Result := snCancelado;
end;

procedure TNFSeR_ISSBarueri.LerValoresNfe(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ValoresNfe');

  NFSe.Servico.Valores.BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('BaseCalculo'), tcDe2);
  NFSe.Servico.Valores.Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe2);
  NFSe.Servico.Valores.ValorIss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIss'), tcDe2);
  NFSe.Servico.Valores.ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorLiquidoNfe'), tcDe2);
end;

procedure TNFSeR_ISSBarueri.LerPrestadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('PrestadorServico');

  NFSe.Prestador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);

  LerIdentificacaoPrestador(AuxNode);
  LerEnderecoPrestador(AuxNode);
end;

procedure TNFSeR_ISSBarueri.LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode, CpfCnpjNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoPrestador');

  NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
  CpfCnpjNode := AuxNode.Childrens.FindAnyNs('CpfCnpj');

  if (CpfCnpjNode <> Nil) then
    NFSe.Prestador.IdentificacaoPrestador.Cnpj := ObterConteudo(CpfCnpjNode.Childrens.FindAnyNs('Cnpj'), tcStr);
end;

procedure TNFSeR_ISSBarueri.LerEnderecoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  NFSe.Prestador.Endereco.Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('Endereco'), tcStr);
  NFSe.Prestador.Endereco.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroEndereco'), tcStr);
  NFSe.Prestador.Endereco.Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
  NFSe.Prestador.Endereco.CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
  NFSe.Prestador.Endereco.UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
  NFSe.Prestador.Endereco.xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);
end;

procedure TNFSeR_ISSBarueri.LerDeclaracaoServicoPrestado(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoServicoPrestado');

  if AuxNode <> Nil then
    AuxNode := AuxNode.Childrens.FindAnyNs('InfDeclaracaoServicoPrestado')
  else
  begin
    AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
    AuxNode := AuxNode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
  end;

  NFSe.Competencia := Iso8601ToDateTime(ObterConteudo(AuxNode.Childrens.FindAnyNs('Competencia'), tcStr));

  LerRps(AuxNode);
  LerServicoPrestado(AuxNode);
  LerTomadorServico(AuxNode);

  NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RegimeEspecialTributacao'), tcStr));
end;

procedure TNFSeR_ISSBarueri.LerRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Rps');

  NFSe.IdentificacaoRps.Tipo := trRPS;
  NFSe.DataEmissaoRps := Iso8601ToDateTime(ObterConteudo(AuxNode.Childrens.FindAnyNs('DataEmissao'), tcStr));

  AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');

  NFSe.IdentificacaoRps.Numero := Trim(String(ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr)));
  NFSe.IdentificacaoRps.Serie := Trim(String(ObterConteudo(AuxNode.Childrens.FindAnyNs('Serie'), tcStr)));
end;

procedure TNFSeR_ISSBarueri.LerServicoPrestado(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  OK: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ServicoPrestado');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('Servico');

  NFSe.Servico.CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoServico'), tcStr);
  NFSe.Servico.Descricao := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescricaoServico'), tcStr);
  NFSe.Servico.Descricao := StringReplace(NFSe.Servico.Descricao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  NFSe.Servico.Discriminacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('Discriminacao'), tcStr);
  NFSe.Servico.Discriminacao := StringReplace(NFSe.Servico.Discriminacao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  NFSe.InformacoesComplementares := ObterConteudo(AuxNode.Childrens.FindAnyNs('ObservacaoLocalTributado'), tcStr);

  NFSe.Servico.Valores.IssRetido := FpAOwner.StrToSituacaoTributaria(OK, ObterConteudo(AuxNode.Childrens.FindAnyNs('IssRetido'), tcInt));

  LerValoresServicoPrestado(AuxNode);
  LerFatura(AuxNode);
end;

procedure TNFSeR_ISSBarueri.LerValoresServicoPrestado(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  //Item: TItemServicoCollectionItem;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ValoresServicoPrestado');

  if AuxNode = nil then
    AuxNode := ANode;

  NFSe.Servico.Valores.ValorIss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIss'), tcDe2);
  NFSe.Servico.Valores.Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe2);

  NFSe.Servico.Valores.ValorServicos := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
  NFSe.Servico.Valores.ValorPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorPis'), tcDe2);
  NFSe.Servico.Valores.ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
  NFSe.Servico.Valores.ValorIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIr'), tcDe2);
  NFSe.Servico.Valores.ValorCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);

  NFSe.Servico.Valores.ValorTotalTributos := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValTotTributos'), tcDe2);

  //Item := NFSe.Servico.ItemServico.Add;

  //Item.Quantidade := ObterConteudo(AuxNode.Childrens.FindAnyNs('QuantidadeServico'), tcDe2);
  //Item.ValorUnitario := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorUnitarioServico'), tcDe2);
  //Item.ValorTotal := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
  //Item.ValorPIS := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorPis'), tcDe2);
  //Item.ValorCOFINS := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
  //Item.ValorIRRF := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIr'), tcDe2);
  //Item.ValorCSLL := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);
  //Item.ValorISS := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIss'), tcDe2);
  //Item.Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe2);
  //Item.BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorNaoBaseCalculo'), tcDe2);
end;

procedure TNFSeR_ISSBarueri.LerFatura(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Pag: TParcelasCollectionItem;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Fatura');

  if AuxNode = nil then
    Exit;

  NFSe.FormaPagamento := StrToIntDef(ObterConteudo(AuxNode.Childrens.FindAnyNs('FormaPagamentoFatura'), tcInt), 0);

  Pag := NFSe.CondicaoPagamento.Parcelas.New;
  Pag.Parcela := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroFatura'), tcStr);
  Pag.Valor := StrToFloatDef(ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorFatura'), tcDe2), 0);
  //ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorExtensoFatura'), tcStr);
end;

procedure TNFSeR_ISSBarueri.LerTomadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('TomadorServico');

  NFSe.Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);

  LerTomadorServicoIdentificacao(AuxNode);
  LerTomadorServicoContato(AuxNode);
  LerTomadorServicoEndereco(AuxNode);
end;

procedure TNFSeR_ISSBarueri.LerTomadorServicoContato(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Contato');
  NFSe.Tomador.Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('Email'), tcStr);
end;

procedure TNFSeR_ISSBarueri.LerTomadorServicoEndereco(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  NFSe.Tomador.Endereco.Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('Endereco'), tcStr);
  NFSe.Tomador.Endereco.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroEndereco'), tcStr);
  NFSe.Tomador.Endereco.Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('ComplementoEndereco'), tcStr);
  NFSe.Tomador.Endereco.Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
  NFSe.Tomador.Endereco.CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
  NFSe.Tomador.Endereco.UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
  NFSe.Tomador.Endereco.xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);
end;

procedure TNFSeR_ISSBarueri.LerTomadorServicoIdentificacao(const ANode: TACBrXmlNode);
var
  AuxNode, CpfCnpjNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoTomador');
  NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
  CpfCnpjNode := AuxNode.Childrens.FindAnyNs('CpfCnpj');

  if (CpfCnpjNode <> Nil) then
  begin
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(CpfCnpjNode.Childrens.FindAnyNs('Cnpj'), tcStr);

    if NFSe.Tomador.IdentificacaoTomador.CpfCnpj = '' then
      NFSe.Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(CpfCnpjNode.Childrens.FindAnyNs('Cpf'), tcStr);
  end;
end;

procedure TNFSeR_ISSBarueri.LerCartaCorrecao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('CartaCorrecao');

  if AuxNode <> nil then
  begin
    // Falta implementar
  end;
end;

procedure TNFSeR_ISSBarueri.LerCancelamentoNFe(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  DataHoraStr: String;
begin
  AuxNode := ANode.Childrens.FindAnyNs('CancelamentoNFe');

  if (AuxNode <> Nil) then
  begin
    NFSe.NfseCancelamento.Sucesso := False;
    DataHoraStr := ObterConteudo(AuxNode.Childrens.FindAnyNs('DataCancelamento'), tcStr);
    if NaoEstaVazio(DataHoraStr) then
    begin
      NFSe.NfseCancelamento.Sucesso := True;
      NFSe.NfseCancelamento.DataHora := Iso8601ToDateTime(DataHoraStr);
      NFSe.MotivoCancelamento := ObterConteudo(AuxNode.Childrens.FindAnyNs('MotivoCancelamento'), tcStr);
      NFSe.SituacaoNfse := snCancelado;
    end;
  end;
end;

procedure TNFSeR_ISSBarueri.LerRegistroTipo2(const ALinha: String);
var
  Ok: Boolean;
begin
  NFSe.Numero := Trim(Copy(ALinha, 7, 6));

  if NaoEstaVazio(Trim(Copy(ALinha, 22, 6))) then
  begin
    NFSe.DataEmissao := EncodeDataHora(Trim(Copy(ALinha, 13, 8)), 'YYYYMMDD');
    NFSe.DataEmissao := NFSe.DataEmissao + StrToTime(Format('%S:%S:%S', [Trim(Copy(ALinha, 21, 2)), Trim(Copy(ALinha, 23, 2)), Trim(Copy(ALinha, 25, 2))]));
  end
  else
    NFSe.DataEmissao := EncodeDataHora(Trim(Copy(ALinha, 13, 8)), 'YYYYMMDD');

  NFSe.CodigoVerificacao := Trim(Copy(ALinha, 27, 24));
  NFSe.IdentificacaoRps.Serie := Trim(Copy(ALinha, 51, 4));
  NFSe.IdentificacaoRps.Numero := Trim(Copy(ALinha, 55, 10));
  NFSe.TipoTributacaoRPS := FpAOwner.StrToTipoTributacaoRPS(Ok, Trim(Copy(ALinha, 65, 1)));
  NFSe.Servico.Valores.IssRetido := Self.StrToSituacaoTributaria(Trim(Copy(ALinha, 66, 1)));
  NFSe.SituacaoNfse := Self.StrToStatusNFSe(Trim(Copy(ALinha, 67, 1)));

  if (NaoEstaVazio(Trim(Copy(ALinha, 68, 8)))) then
  begin
    NFSe.NfseCancelamento.Sucesso := True;
    NFSe.NfseCancelamento.DataHora := EncodeDataHora(Trim(Copy(ALinha, 68, 8)), 'YYYYMMDD');
  end;

  NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Trim(Copy(ALinha, 94, 14));
  NFSe.Tomador.RazaoSocial := Trim(Copy(ALinha, 108, 100));
  NFSe.Tomador.Endereco.Endereco := Trim(Copy(ALinha, 208, 100));
  NFSe.Tomador.Endereco.Numero := Trim(Copy(ALinha, 308, 9));
  NFSe.Tomador.Endereco.Complemento := Trim(Copy(ALinha, 317, 20));
  NFSe.Tomador.Endereco.Bairro := Trim(Copy(ALinha, 337, 40));
  NFSe.Tomador.Endereco.xMunicipio := Trim(Copy(ALinha, 377, 40));
  NFSe.Tomador.Endereco.UF := Trim(Copy(ALinha, 417, 2));
  NFSe.Tomador.Endereco.CEP := Trim(Copy(ALinha, 419, 8));
  NFSe.Tomador.Endereco.xPais := Trim(Copy(ALinha, 427, 50));
  NFSe.Tomador.Contato.Email := Trim(Copy(ALinha, 477, 152));
  NFSe.Servico.Discriminacao := StringReplace(Trim(Copy(ALinha, 629, 1000)), '|', CRLF, [rfReplaceAll]);
end;

procedure TNFSeR_ISSBarueri.LerRegistroTipo3(const ALinha: String);
var
  Item: TItemServicoCollectionItem;
begin
  Item := NFSe.Servico.ItemServico.New;
  Item.Quantidade := StrToFloatDef(Trim(Copy(ALinha, 2, 6)), 1.00);
  Item.Descricao := Trim(Copy(ALinha, 8, 60));
  Item.Descricao := StringReplace(Item.Descricao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  Item.CodServ := Trim(Copy(ALinha, 68, 9));
  Item.ValorUnitario := StrToFloatDef(Trim(Copy(ALinha, 77, 15)), 0.00) / 100;
  //Item.ValorISS := StrToFloatDef(Trim(Copy(ALinha, 77, 15)), 0.00) / 100;
  Item.Aliquota := StrToFloatDef(Trim(Copy(ALinha, 92, 4)), 0.00) / 100;

  NFSe.Servico.Descricao := Trim(Copy(ALinha, 8, 60));
  NFSe.Servico.CodigoMunicipio := Trim(Copy(ALinha, 68, 9));
  NFSe.Servico.Valores.Aliquota := StrToFloatDef(Trim(Copy(ALinha, 92, 4)), 0.00) / 100;

  if (NFSe.Servico.Valores.IssRetido = stRetencao) then
  begin
    NFSe.Servico.Valores.ValorIssRetido := Item.ValorISS;
  end;
end;

procedure TNFSeR_ISSBarueri.LerRegistroTipo4(const ALinha: String);
var
  CodigoOutrosValores: String;
begin
  CodigoOutrosValores := Trim(Copy(ALinha, 2, 2));

  if (CodigoOutrosValores = '01') then
  begin
    NFSe.Servico.Valores.ValorIr := StrToFloatDef(Trim(Copy(ALinha, 4, 15)), 0.00) / 100;
    NFSe.Servico.ItemServico.Items[0].ValorIRRF := StrToFloatDef(Trim(Copy(ALinha, 4, 15)), 0.00) / 100;
  end
  else if (CodigoOutrosValores = '02') then
  begin
    NFSe.Servico.Valores.ValorPis := StrToFloatDef(Trim(Copy(ALinha, 4, 15)), 0.00) / 100;
    NFSe.Servico.ItemServico.Items[0].ValorPIS := StrToFloatDef(Trim(Copy(ALinha, 4, 15)), 0.00) / 100;
  end
  else if (CodigoOutrosValores = '03') then
  begin
    NFSe.Servico.Valores.ValorCofins := StrToFloatDef(Trim(Copy(ALinha, 4, 15)), 0.00) / 100;
    NFSe.Servico.ItemServico.Items[0].ValorCOFINS := StrToFloatDef(Trim(Copy(ALinha, 4, 15)), 0.00) / 100;
  end
  else if (CodigoOutrosValores = '04') then
  begin
    NFSe.Servico.Valores.ValorCsll := StrToFloatDef(Trim(Copy(ALinha, 4, 15)), 0.00) / 100;
    NFSe.Servico.ItemServico.Items[0].ValorCSLL := StrToFloatDef(Trim(Copy(ALinha, 4, 15)), 0.00) / 100;
  end
  else
  begin
    NFSe.Servico.ItemServico.Items[0].ValorTotal := StrToFloatDef(Trim(Copy(ALinha, 4, 15)), 0.00) / 100;
  end;
end;

function TNFSeR_ISSBarueri.TipodeXMLLeitura(const aArquivo: string): TtpXML;
begin
  Result := txmlNFSe;
end;

function TNFSeR_ISSBarueri.IsXML(const aArquivo: String): Boolean;
begin
  Result := (Pos('<ConsultarNfeServPrestadoResposta', aArquivo) > 0) or
            (Pos('<CompNfe>', aArquivo) > 0);
end;

function TNFSeR_ISSBarueri.LerXml: Boolean;
var
  Linha: String;
  XmlNode, AuxNode: TACBrXmlNode;
  DadosTxt: TStringList;
  I: Integer;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo XML não carregado.');

  LerParamsTabIni(True);

  Arquivo := NormatizarXml(Arquivo);

  if (FDocument = Nil) then
    FDocument := TACBrXmlDocument.Create();

  DadosTxt := TStringList.Create;

  try
    {$IFDEF FPC}
    DadosTxt.LineBreak := CRLF;
    {$ELSE}
      {$IFDEF DELPHI2006_UP}
      DadosTxt.LineBreak := CRLF;
      {$ENDIF}
    {$ENDIF}

    NFSe.Clear;

    tpXML := TipodeXMLLeitura(Arquivo);

    if (not IsXML(Arquivo)) then
    begin
      DadosTxt.Text := Arquivo;

      for I := 0 to Pred(DadosTxt.Count) do
      begin
        Linha := DadosTxt[I];

        if (Linha[1] = '1') then
        begin
          NFSe.IdentificacaoRps.Numero := Trim(Copy(Linha, Pos('PMB002', Linha), Length(Linha)));
          NFSe.IdentificacaoRps.Numero := StringReplace(NFSe.IdentificacaoRps.Numero, 'PMB002', '', [rfReplaceAll]);
          NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := Trim(Copy(Linha, 2, 7));
        end else if (Linha[1] = '2') then
        begin
          LerRegistroTipo2(Linha);
        end else if (Linha[1] = '3') then
        begin
          LerRegistroTipo3(Linha);
        end else if (Linha[1] = '4') then
        begin
          LerRegistroTipo4(Linha);
        end;
      end;

      Result := True;
    end
    else
    begin
      Document.Clear();
      Document.LoadFromXml(Arquivo);

      XmlNode := Document.Root;

      if (XmlNode = Nil) then
        raise Exception.Create('Arquivo XML vazio.');

      AuxNode := XmlNode.Childrens.FindAnyNs('ListaNfeServPrestado');

      if AuxNode <> nil then
      begin
        AuxNode := AuxNode.Childrens.FindAnyNs('CompNfeServPrestado');
        AuxNode := AuxNode.Childrens.FindAnyNs('NfeServPrestado');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfeServPrestado');
        Result := LerXmlNfse(AuxNode);
      end
      else
      begin
        AuxNode := XmlNode.Childrens.FindAnyNs('Nfe');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNFe');
        Result := LerXmlNfse(AuxNode);
      end;
    end;
  finally
    FreeAndNil(FDocument);
    FreeAndNil(DadosTxt);
  end;
end;

function TNFSeR_ISSBarueri.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
begin
  NFSe.SituacaoNfse := snNormal;
  NFSe.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroNfe'), tcStr);
  NFSe.SeriePrestacao := ObterConteudo(ANode.Childrens.FindAnyNs('SerieNfe'), tcStr);
  //NFSe.DescricaoNfe := ObterConteudo(ANode.Childrens.FindAnyNs('DescricaoNfe'), tcStr);
  NFSe.CodigoVerificacao := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
  NFSe.DataEmissao := Iso8601ToDateTime(ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcStr));

  LerValoresNfe(ANode);
  LerPrestadorServico(ANode);
  LerDeclaracaoServicoPrestado(ANode);
  LerCartaCorrecao(ANode);
  LerCancelamentoNFe(ANode);

  LerCampoLink;

  Result := True;
end;

end.
