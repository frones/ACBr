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

unit SigISS.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_SigISS }

  TNFSeR_SigISS = class(TNFSeRClass)
  protected
    procedure LerDadosPrestador(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
    function LerXmlEspelho(const ANode: TACBrXmlNode): Boolean;
  end;

  { TNFSeR_SigISS101 }

  TNFSeR_SigISS101 = class(TNFSeR_SigISS)
  protected

  public

  end;

  { TNFSeR_SigISS103 }

  TNFSeR_SigISS103 = class(TNFSeR_SigISS)
  protected

  public

  end;

implementation

uses
  synautil,
  ACBrUtil.Base;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     SigISS
//==============================================================================

{ TNFSeR_SigISS }

procedure TNFSeR_SigISS.LerDadosPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DadosPrestador');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      Prestador.crc := ObterConteudo(AuxNode.Childrens.FindAnyNs('crc'), tcStr);
      Prestador.crc_estado := ObterConteudo(AuxNode.Childrens.FindAnyNs('crc_estado'), tcStr);

      Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('cnpj'), tcStr);

      Servico.Valores.AliquotaSN := ObterConteudo(AuxNode.Childrens.FindAnyNs('aliquota_simples'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_SigISS.LerIdentificacaoRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Dia, Mes, Ano, xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DescricaoRps');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      Prestador.crc := ObterConteudo(AuxNode.Childrens.FindAnyNs('crc'), tcStr);
      Prestador.crc_estado := ObterConteudo(AuxNode.Childrens.FindAnyNs('crc_estado'), tcStr);

      Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('cnpj'), tcStr);

      Servico.Valores.AliquotaSN := ObterConteudo(AuxNode.Childrens.FindAnyNs('aliquota_simples'), tcDe2);

      id_sis_legado := ObterConteudo(AuxNode.Childrens.FindAnyNs('id_sis_legado'), tcStr);
      SituacaoTrib := ObterConteudo(AuxNode.Childrens.FindAnyNs('situacao'), tcStr);

      Servico.Discriminacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('descricaoNF'), tcStr);
      Servico.Discriminacao := StringReplace(Servico.Discriminacao, FpQuebradeLinha,
                                    sLineBreak, [rfReplaceAll, rfIgnoreCase]);

      VerificarSeConteudoEhLista(Servico.Discriminacao);

      Servico.CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('servico'), tcStr);
      Servico.MunicipioIncidencia := ObterConteudo(AuxNode.Childrens.FindAnyNs('codigo_cidade_local_servico'), tcInt);

      Servico.Valores.ValorServicos := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor'), tcDe4);
      Servico.Valores.BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('base'), tcDe4);
      Servico.Valores.ValorInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_inss'), tcDe4);
      Servico.Valores.ValorIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_ir'), tcDe4);
      Servico.Valores.ValorPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_pis'), tcDe4);
      Servico.Valores.ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_cofins'), tcDe4);
      Servico.Valores.ValorCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_csll'), tcDe4);

      Tomador.IdentificacaoTomador.Tipo := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_tipo'), tcStr);
      Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_cnpj'), tcStr);
      Tomador.IdentificacaoTomador.InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_ie'), tcStr);
      Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_im'), tcStr);

      Tomador.Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_email'), tcStr);
      Tomador.Contato.Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_fone'), tcStr);

      Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_razao'), tcStr);
      Tomador.NomeFantasia := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_fantasia'), tcStr);

      Tomador.Endereco.Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_endereco'), tcStr);
      Tomador.Endereco.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_numero'), tcStr);
      Tomador.Endereco.Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_complemento'), tcStr);
      Tomador.Endereco.Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_bairro'), tcStr);
      Tomador.Endereco.CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_CEP'), tcStr);
      Tomador.Endereco.CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_cod_cidade'), tcStr);
      Tomador.Endereco.xMunicipio := ObterNomeMunicipioUF(StrToIntDef(Tomador.Endereco.CodigoMunicipio, 0), xUF);

      if Tomador.Endereco.UF = '' then
        Tomador.Endereco.UF := xUF;

      IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('rps_num'), tcStr);
      IdentificacaoRps.Serie := ObterConteudo(AuxNode.Childrens.FindAnyNs('rps_serie'), tcStr);

      Dia := ObterConteudo(AuxNode.Childrens.FindAnyNs('rps_dia'), tcStr);
      Mes := ObterConteudo(AuxNode.Childrens.FindAnyNs('rps_mes'), tcStr);
      Ano := ObterConteudo(AuxNode.Childrens.FindAnyNs('rps_ano'), tcStr);

      DataEmissaoRps := StrToDateDef(Dia + '/' + Mes + '/' + Ano, Date);
    end;
  end;
end;

function TNFSeR_SigISS.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  LerParamsTabIni(True);

  Arquivo := NormatizarXml(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('Nota', Arquivo) > 0) then
    tpXML := txmlNFSe
  else
    if (Pos('EspelhoNfse', Arquivo) > 0) then
      tpXML := txmlEspelho
    else
      tpXML := txmlRPS;

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    if tpXML = txmlEspelho then
      Result := LerXmlEspelho(XmlNode)
    else
      Result := LerXmlRps(XmlNode);

  FreeAndNil(FDocument);
end;

function TNFSeR_SigISS.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('DadosNota');

  if AuxNode = nil then
    AuxNode := ANode;

  with NFSe do
  begin
    SituacaoNfse := snNormal;
    dhRecebimento := Now;
    id_sis_legado := ObterConteudo(AuxNode.Childrens.FindAnyNs('id_sis_legado'), tcInt);
    CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('autenticidade'), tcStr);
    Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nota'), tcStr);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('dt_conversao'), tcStr);
    aValor := Copy(aValor, 1, 11);
    DataEmissao := DecodeRfcDateTime(aValor);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('emissao_rps'), tcStr);
    aValor := Copy(aValor, 1, 11);
    DataEmissaoRps := DecodeRfcDateTime(aValor);

    Link := ObterConteudo(AuxNode.Childrens.FindAnyNs('LinkImpressao'), tcStr);
    Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('StatusNFe'), tcStr);

    if (aValor = 'Cancelada') or (aValor = '2') then
      SituacaoNfse := snCancelado;

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('OpcaoSimples'), tcStr);

    if aValor = 'NAO' then
      OptanteSimplesNacional := snNao
    else
      OptanteSimplesNacional := snSim;

    ValoresNfse.ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor'), tcDe2);
    ValoresNfse.BaseCalculo      := ObterConteudo(AuxNode.Childrens.FindAnyNs('base'), tcDe2);
    ValoresNfse.Aliquota         := ObterConteudo(AuxNode.Childrens.FindAnyNs('aliquota_atividade'), tcDe2);
    ValoresNfse.ValorIss         := ObterConteudo(AuxNode.Childrens.FindAnyNs('iss'), tcDe2);

    Servico.ItemListaServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('servico'), tcStr);
    Servico.Discriminacao    := ObterConteudo(AuxNode.Childrens.FindAnyNs('descricao'), tcStr);
    Servico.Discriminacao := StringReplace(Servico.Discriminacao, FpQuebradeLinha,
                                    sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    VerificarSeConteudoEhLista(Servico.Discriminacao);

    Servico.Valores.ValorServicos := NFSe.ValoresNfse.ValorLiquidoNfse;
    Servico.Valores.BaseCalculo   := Servico.Valores.ValorServicos;
    Servico.Valores.Aliquota      := NFSe.ValoresNfse.Aliquota;
    Servico.Valores.ValorIss      := NFSe.ValoresNfse.ValorIss;

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('ISSRetido'), tcStr);

    if aValor = 'NAO' then
       Servico.Valores.IssRetido := stNormal
    else
    begin
      Servico.Valores.IssRetido := stRetencao;
      Servico.Valores.ValorIssRetido := StrToFloatDef(ObterConteudo(AuxNode.Childrens.FindAnyNs('ISSRetido'), tcStr), 0);
    end;

    Servico.Valores.RetencoesFederais := Servico.Valores.ValorPis +
      Servico.Valores.ValorCofins + Servico.Valores.ValorInss +
      Servico.Valores.ValorIr + Servico.Valores.ValorCsll;

    Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos -
     (Servico.Valores.RetencoesFederais + Servico.Valores.ValorDeducoes +
      Servico.Valores.ValorIssRetido + Servico.Valores.DescontoCondicionado +
      Servico.Valores.DescontoIncondicionado);

    Servico.Valores.ValorTotalNotaFiscal := Servico.Valores.ValorServicos -
      Servico.Valores.DescontoCondicionado - Servico.Valores.DescontoIncondicionado;

    Prestador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_razao'), tcStr);

    Prestador.Endereco.Endereco    := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_endereco'), tcStr);
    Prestador.Endereco.Numero      := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_numero'), tcStr);
    Prestador.Endereco.Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_complemento'), tcStr);
    Prestador.Endereco.Bairro      := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_bairro'), tcStr);
    Prestador.Endereco.xMunicipio  := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_cidade'), tcStr);
    Prestador.Endereco.UF          := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_estado'), tcStr);
    Prestador.Endereco.CEP         := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_cep'), tcStr);

    Prestador.Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_email'), tcStr);

    Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('razao_tomador'), tcStr);

    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('cnpj_tomador'), tcStr);

    Tomador.Endereco.Endereco    := ObterConteudo(AuxNode.Childrens.FindAnyNs('endereco_tomador'), tcStr);
    Tomador.Endereco.Numero      := ObterConteudo(AuxNode.Childrens.FindAnyNs('numero_tomador'), tcStr);
    Tomador.Endereco.Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('complemento_tomador'), tcStr);
    Tomador.Endereco.Bairro      := ObterConteudo(AuxNode.Childrens.FindAnyNs('bairro_tomador'), tcStr);
    Tomador.Endereco.xMunicipio  := ObterConteudo(AuxNode.Childrens.FindAnyNs('cidade_tomador'), tcStr);
    Tomador.Endereco.UF          := ObterConteudo(AuxNode.Childrens.FindAnyNs('estado_tomador'), tcStr);
    Tomador.Endereco.CEP         := ObterConteudo(AuxNode.Childrens.FindAnyNs('cep_tomador'), tcStr);

    Tomador.Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email_tomador'), tcStr);
  end;
end;

function TNFSeR_SigISS.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  with NFSe do
  begin
    LerDadosPrestador(ANode);
    LerIdentificacaoRps(ANode);
  end;
end;

function TNFSeR_SigISS.LerXmlEspelho(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode, IdentificacaoNfseNode, DadosNfseNode: TACBrXmlNode;
  aValor, xUF: string;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Nfse');

  if AuxNode = nil then Exit;

  IdentificacaoNfseNode := AuxNode.Childrens.FindAnyNs('IdentificacaoNfse');
  DadosNfseNode := AuxNode.Childrens.FindAnyNs('DadosNfse');

  with NFSe do
  begin
    dhRecebimento := ObterConteudo(IdentificacaoNfseNode.Childrens.FindAnyNs('DataEmissao'), tcDat);
    Numero := ObterConteudo(IdentificacaoNfseNode.Childrens.FindAnyNs('Numero'), tcStr);
    CodigoVerificacao := ObterConteudo(IdentificacaoNfseNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
    Link := ObterConteudo(IdentificacaoNfseNode.Childrens.FindAnyNs('LinkImpressao'), tcStr);
    Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
    DataEmissao := ObterConteudo(IdentificacaoNfseNode.Childrens.FindAnyNs('DataEmissao'), tcDat);

    aValor := ObterConteudo(IdentificacaoNfseNode.Childrens.FindAnyNs('StatusNfse'), tcStr);

    if aValor = '1' then
      SituacaoNfse := snNormal
    else
      SituacaoNfse := snCancelado;

    Prestador.RazaoSocial  := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorRazaoSocial'), tcStr);
    Prestador.NomeFantasia := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorNomeFantasia'), tcStr);

    Prestador.IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorInscricaoMunicipal'), tcStr);
    Prestador.IdentificacaoPrestador.CpfCnpj            := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorCnpj'), tcStr);
    Prestador.IdentificacaoPrestador.InscricaoEstadual  := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorInscricaoEstadual'), tcStr);

    Prestador.Endereco.Endereco        := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorEndereco'), tcStr);
    Prestador.Endereco.Numero          := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorNumero'), tcStr);
    Prestador.Endereco.Complemento     := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorComplemento'), tcStr);
    Prestador.Endereco.Bairro          := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorBairro'), tcStr);
    Prestador.Endereco.CodigoMunicipio := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorCodigoMunicipio'), tcStr);
    Prestador.Endereco.UF              := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorUf'), tcStr);
    Prestador.Endereco.CEP             := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorCep'), tcStr);
    Prestador.Endereco.xMunicipio      := ObterNomeMunicipioUF(StrToIntDef(Prestador.Endereco.CodigoMunicipio, 0), xUF);

    if Prestador.Endereco.UF = '' then
      Prestador.Endereco.UF := xUF;

    ValoresNfse.ValorLiquidoNfse := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorLiquidoNfse'), tcDe2);
    ValoresNfse.BaseCalculo      := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('BaseCalculo'), tcDe2);
    ValoresNfse.Aliquota         := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('Aliquota'), tcDe2);
    ValoresNfse.ValorIss         := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorIss'), tcDe2);

    Servico.Discriminacao    := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('Discriminacao'), tcStr);
    Servico.Discriminacao := StringReplace(Servico.Discriminacao, FpQuebradeLinha,
                                    sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    VerificarSeConteudoEhLista(Servico.Discriminacao);

    Servico.Valores.ValorServicos := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
    Servico.Valores.BaseCalculo   := NFSe.ValoresNfse.BaseCalculo;
    Servico.Valores.Aliquota      := NFSe.ValoresNfse.Aliquota;
    Servico.Valores.ValorIss      := NFSe.ValoresNfse.ValorIss;

    Servico.Valores.ValorIr     := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorIr'), tcDe2);
    Servico.Valores.ValorPis    := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorPis'), tcDe2);
    Servico.Valores.ValorCofins := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
    Servico.Valores.ValorCsll   := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);
    Servico.Valores.ValorInss   := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorInss'), tcDe2);

    aValor := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('IssRetido'), tcStr);

    if aValor = 'false' then
      Servico.Valores.IssRetido := stNormal
    else
      Servico.Valores.IssRetido := stRetencao;

    Servico.Valores.ValorLiquidoNfse := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorLiquidoNfse'), tcDe2);

    Servico.Valores.ValorTotalNotaFiscal := Servico.Valores.ValorServicos -
      Servico.Valores.DescontoCondicionado - Servico.Valores.DescontoIncondicionado;

    Tomador.RazaoSocial  := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorRazaoSocial'), tcStr);
    Tomador.NomeFantasia := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorRazaoSocial'), tcStr);

    Tomador.IdentificacaoTomador.CpfCnpj            := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorCpfCnpj'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoEstadual  := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorInscricaoEstadual'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorInscricaoMunicipal'), tcStr);

    Tomador.Endereco.Endereco        := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorEndereco'), tcStr);
    Tomador.Endereco.Numero          := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorNumero'), tcStr);
    Tomador.Endereco.Complemento     := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorComplemento'), tcStr);
    Tomador.Endereco.Bairro          := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorBairro'), tcStr);
    Tomador.Endereco.CEP             := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorCep'), tcStr);
    Tomador.Endereco.CodigoMunicipio := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorCodigoMunicipio'), tcStr);
    Tomador.Endereco.UF              := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorUf'), tcStr);
    Tomador.Endereco.xMunicipio      := ObterNomeMunicipioUF(StrToIntDef(Tomador.Endereco.CodigoMunicipio, 0), xUF);

    if Tomador.Endereco.UF = '' then
      Tomador.Endereco.UF := xUF;

    OutrasInformacoes := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('Observacoes'), tcStr);
    OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    aValor := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr);

    if aValor = 'true' then
      OptanteSimplesNacional := snSim
    else
      OptanteSimplesNacional := snNao;
  end;
end;


end.
