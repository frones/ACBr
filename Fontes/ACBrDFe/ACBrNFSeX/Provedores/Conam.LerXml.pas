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

unit Conam.LerXml;

interface

uses
  SysUtils, Classes, StrUtils, IniFiles,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_Conam }

  TNFSeR_Conam = class(TNFSeRClass)
  protected
    FpTipoXML: string;

    // Ler o arquivo XML
    procedure LerReg30(const ANode: TACBrXmlNode);

    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;

    //====== Ler o Arquivo INI===========================================
    procedure LerINIIdentificacaoNFSe(AINIRec: TMemIniFile);
    procedure LerINIIdentificacaoRps(AINIRec: TMemIniFile);
    procedure LerINIDadosServico(AINIRec: TMemIniFile);
    procedure LerINIDadosValores(AINIRec: TMemIniFile);
    procedure LerINIDadosTomador(AINIRec: TMemIniFile);
    procedure LerINIIdentificacaoPrestador(AINIRec: TMemIniFile);
    procedure LerINIListaEmail(AINIRec: TMemIniFile);
    procedure LerINIValoresNFSe(AINIRec: TMemIniFile);

    procedure LerIniRps(AINIRec: TMemIniFile);
    procedure LerIniNfse(AINIRec: TMemIniFile);
  public
    function LerXml: Boolean; override;
    function LerIni: Boolean; override;
  end;

implementation

uses
  ACBrUtil.FilesIO,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     Conam
//==============================================================================

{ TNFSeR_Conam }

procedure TNFSeR_Conam.LerReg30(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Reg30');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('Reg30Item');

    for i := 0 to Length(ANodes) - 1 do
    begin
      aValor := ObterConteudo(ANodes[i].Childrens.FindAnyNs('TributoSigla'), tcStr);

      with NFSe.Servico.Valores do
      begin
        if aValor = 'IR' then
        begin
          AliquotaIr := ObterConteudo(ANodes[i].Childrens.FindAnyNs('TributoAliquota'), tcDe2);
          ValorIr    := ObterConteudo(ANodes[i].Childrens.FindAnyNs('TributoValor'), tcDe2);
        end;

        if aValor = 'PIS' then
        begin
          AliquotaPis := ObterConteudo(ANodes[i].Childrens.FindAnyNs('TributoAliquota'), tcDe2);
          ValorPis    := ObterConteudo(ANodes[i].Childrens.FindAnyNs('TributoValor'), tcDe2);
        end;

        if aValor = 'COFINS' then
        begin
          AliquotaCofins := ObterConteudo(ANodes[i].Childrens.FindAnyNs('TributoAliquota'), tcDe2);
          ValorCofins    := ObterConteudo(ANodes[i].Childrens.FindAnyNs('TributoValor'), tcDe2);
        end;

        if aValor = 'CSLL' then
        begin
          AliquotaCsll := ObterConteudo(ANodes[i].Childrens.FindAnyNs('TributoAliquota'), tcDe2);
          ValorCsll    := ObterConteudo(ANodes[i].Childrens.FindAnyNs('TributoValor'), tcDe2);
        end;

        if aValor = 'INSS' then
        begin
          AliquotaInss := ObterConteudo(ANodes[i].Childrens.FindAnyNs('TributoAliquota'), tcDe2);
          ValorInss    := ObterConteudo(ANodes[i].Childrens.FindAnyNs('TributoValor'), tcDe2);
        end;

        RetencoesFederais := ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll;
      end;
    end;
  end;
end;

function TNFSeR_Conam.LerXml: Boolean;
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

  if (Pos('Reg20Item', Arquivo) > 0) then
    tpXML := txmlNFSe
  else
    tpXML := txmlRPS;

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  NFSe.tpXML := tpXml;

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);

  FreeAndNil(FDocument);
end;

function TNFSeR_Conam.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  ValorIssRet: Double;
begin
  Result := True;
  NFSe.SituacaoNfse := snNormal;

  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Reg20Item');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('CompNfse');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumNf'), tcStr);
    SeriePrestacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('SerNf'), tcStr);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('SitNf'), tcStr);

    if aValor = '2' then
      NFSe.SituacaoNfse := snCancelado;

    MotivoCancelamento := ObterConteudo(AuxNode.Childrens.FindAnyNs('MotivoCncNf'), tcStr);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('DtEmi'), tcStr);

    if aValor = '' then
      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('DtEmiNf'), tcStr);

    if aValor <> '' then
    begin
      DataEmissao := StrToDate(aValor);
      Competencia := DataEmissao;
    end;

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('DtEmiRps'), tcStr);

    DataEmissaoRps := StrToDate(aValor);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('DtHrGerNf'), tcStr);

    dhRecebimento := StrToDateTimeDef(aValor, Now);

    IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumRps'), tcStr);
    IdentificacaoRps.Serie  := ObterConteudo(AuxNode.Childrens.FindAnyNs('SerRps'), tcStr);

    InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;

    CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodVernf'), tcStr);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('TipoTribPre'), tcStr);

    if aValor = 'SN' then
      OptanteSimplesNacional := snSim
    else
      OptanteSimplesNacional := snNao;

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('DtAdeSN'), tcStr);

    if (aValor <> '') and (aValor <> '/  /') then
      DataOptanteSimplesNacional := StrToDate(aValor);

    Prestador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazSocPre'), tcStr);

    Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CpfCnpjPre'), tcStr);
    Prestador.IdentificacaoPrestador.InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('IEPr'), tcStr);
    Prestador.IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodCadBic'), tcStr);

    Prestador.Endereco.Endereco    := ObterConteudo(AuxNode.Childrens.FindAnyNs('LogPre'), tcStr);
    Prestador.Endereco.Numero      := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumEndPre'), tcStr);
    Prestador.Endereco.Bairro      := ObterConteudo(AuxNode.Childrens.FindAnyNs('BairroPre'), tcStr);
    Prestador.Endereco.Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('ComplEndPre'), tcStr);
    Prestador.Endereco.xMunicipio  := ObterConteudo(AuxNode.Childrens.FindAnyNs('MunPre'), tcStr);
    Prestador.Endereco.CEP         := ObterConteudo(AuxNode.Childrens.FindAnyNs('CepPre'), tcStr);
    Prestador.Endereco.UF          := ObterConteudo(AuxNode.Childrens.FindAnyNs('SiglaUFPre'), tcStr);

    Servico.Discriminacao    := ObterConteudo(AuxNode.Childrens.FindAnyNs('DiscrSrv'), tcStr);
    Servico.Discriminacao := StringReplace(Servico.Discriminacao, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);

    VerificarSeConteudoEhLista(Servico.Discriminacao);

    Servico.ItemListaServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodSrv'), tcStr);

    Servico.xItemListaServico := ItemListaServicoDescricao(Servico.ItemListaServico);

    ValorIssRet := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlIssRet'), tcDe2);

    ValoresNfse.ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlNFS'), tcDe2);

    if ValorIssRet > 0 then
      ValoresNfse.ValorLiquidoNfse := ValoresNfse.ValorLiquidoNfse - ValorIssRet;

    ValoresNfse.BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlBasCalc'), tcDe2);
    ValoresNfse.Aliquota    := ObterConteudo(AuxNode.Childrens.FindAnyNs('AlqIss'), tcDe2);
    ValoresNfse.ValorIss    := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlIss'), tcDe2);

    Servico.Valores.ValorServicos  := ValoresNfse.ValorLiquidoNfse;
    Servico.Valores.BaseCalculo    := ValoresNfse.BaseCalculo;
    Servico.Valores.Aliquota       := ValoresNfse.Aliquota;
    Servico.Valores.ValorIssRetido := ValorIssRet;

    if ValorIssRet > 0 then
    begin
      Servico.Valores.IssRetido := stRetencao;
      Servico.Valores.ValorIss  := 0;
    end
    else
    begin
      Servico.Valores.IssRetido := stNormal;
      Servico.Valores.ValorIss  := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlIss'), tcDe2);
    end;

    Servico.Valores.ValorDeducoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlDed'), tcDe2);

    Servico.Valores.JustificativaDeducao := ObterConteudo(AuxNode.Childrens.FindAnyNs('DiscrDed'), tcStr);

    Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazSocTom'), tcStr);

    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CpfCnpjTom'), tcStr);

    Tomador.Endereco.Endereco    := ObterConteudo(AuxNode.Childrens.FindAnyNs('LogTom'), tcStr);
    Tomador.Endereco.Numero      := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumEndTom'), tcStr);
    Tomador.Endereco.Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('ComplEndTom'), tcStr);
    Tomador.Endereco.Bairro      := ObterConteudo(AuxNode.Childrens.FindAnyNs('BairroTom'), tcStr);
    Tomador.Endereco.xMunicipio  := ObterConteudo(AuxNode.Childrens.FindAnyNs('MunTom'), tcStr);
    Tomador.Endereco.UF          := ObterConteudo(AuxNode.Childrens.FindAnyNs('SiglaUFTom'), tcStr);
    Tomador.Endereco.CEP         := ObterConteudo(AuxNode.Childrens.FindAnyNs('CepTom'), tcStr);

    NFSe.Servico.Valores.ValorTotalNotaFiscal := NFSe.Servico.Valores.ValorServicos -
      NFSe.Servico.Valores.DescontoCondicionado -
      NFSe.Servico.Valores.DescontoIncondicionado;

    //valores dos tributos
    LerReg30(AuxNode);
  end;

  LerCampoLink;
end;

function TNFSeR_Conam.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  i: Integer;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Reg20Item');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('TipoNFS'), tcStr);

    if aValor = 'RPS' then
      IdentificacaoRps.Tipo := trRPS
    else
      IdentificacaoRps.Tipo := trNone;

    IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumRps'), tcStr);
    IdentificacaoRps.Serie := ObterConteudo(AuxNode.Childrens.FindAnyNs('SerRps'), tcStr);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('DtEmi'), tcStr);

    if aValor <> '' then
    begin
      DataEmissaoRps := StrToDate(aValor);
      Competencia := DataEmissao;
    end;

    Servico.ItemListaServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodSrv'), tcStr);
    Servico.Discriminacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('DiscrSrv'), tcStr);
    Servico.Discriminacao := StringReplace(Servico.Discriminacao, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);

    VerificarSeConteudoEhLista(Servico.Discriminacao);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('RetFonte'), tcStr);

    if aValor <> 'NAO' then
      Servico.Valores.IssRetido := stNormal
    else
      Servico.Valores.IssRetido := stRetencao;

    Servico.Valores.ValorServicos := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlNFS'), tcDe2);
    Servico.Valores.ValorDeducoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlDed'), tcDe2);
    Servico.Valores.JustificativaDeducao := ObterConteudo(AuxNode.Childrens.FindAnyNs('DiscrDed'), tcStr);
    Servico.Valores.BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlBasCalc'), tcDe2);
    Servico.Valores.Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('AlqIss'), tcDe2);
    Servico.Valores.ValorIss := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlIss'), tcDe2);
    Servico.Valores.ValorIssRetido := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlIssRet'), tcDe2);

    Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazSocTom'), tcStr);

    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CpfCnpTom'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);

    Tomador.Endereco.TipoLogradouro := ObterConteudo(AuxNode.Childrens.FindAnyNs('TipoLogtom'), tcStr);
    Tomador.Endereco.Endereco    := ObterConteudo(AuxNode.Childrens.FindAnyNs('LogTom'), tcStr);
    Tomador.Endereco.Numero      := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumEndTom'), tcStr);
    Tomador.Endereco.Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('ComplEndTom'), tcStr);
    Tomador.Endereco.Bairro      := ObterConteudo(AuxNode.Childrens.FindAnyNs('BairroTom'), tcStr);
    Tomador.Endereco.xMunicipio  := ObterConteudo(AuxNode.Childrens.FindAnyNs('MunTom'), tcStr);
    Tomador.Endereco.UF          := ObterConteudo(AuxNode.Childrens.FindAnyNs('SiglaUFTom'), tcStr);
    Tomador.Endereco.CEP         := ObterConteudo(AuxNode.Childrens.FindAnyNs('CepTom'), tcStr);

    Tomador.Contato.Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('Telefone'), tcStr);
    Tomador.Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('Email1'), tcStr);

    Prestador.Endereco.Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('LogLocPre'), tcStr);

    if Prestador.Endereco.Endereco = Tomador.Endereco.Endereco then
      LogradouLocalPrestacaoServico := llpTomador
    else
      LogradouLocalPrestacaoServico := llpPrestador;

    Prestador.Endereco.TipoLogradouro := ObterConteudo(AuxNode.Childrens.FindAnyNs('TipoLogLocPre'), tcStr);
    Prestador.Endereco.Numero      := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumEndLocPre'), tcStr);
    Prestador.Endereco.Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('ComplEndLocPre'), tcStr);
    Prestador.Endereco.Bairro      := ObterConteudo(AuxNode.Childrens.FindAnyNs('BairroLocPre'), tcStr);
    Prestador.Endereco.xMunicipio  := ObterConteudo(AuxNode.Childrens.FindAnyNs('MunLocPre'), tcStr);
    Prestador.Endereco.UF          := ObterConteudo(AuxNode.Childrens.FindAnyNs('SiglaUFLocpre'), tcStr);
    Prestador.Endereco.CEP         := ObterConteudo(AuxNode.Childrens.FindAnyNs('CepLocPre'), tcStr);

    i:= 0;

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('Email2'), tcStr);

    if aValor <> '' then
    begin
      email.New;
      email[i].emailCC := aValor;
      Inc(i);
    end;

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('Email3'), tcStr);

    if aValor <> '' then
    begin
      email.New;
      email[i].emailCC := aValor;
//      Inc(i);
    end;

    NFSe.Servico.Valores.ValorTotalNotaFiscal := NFSe.Servico.Valores.ValorServicos -
      NFSe.Servico.Valores.DescontoCondicionado -
      NFSe.Servico.Valores.DescontoIncondicionado;

    //valores dos tributos
    LerReg30(AuxNode);
  end;
end;

//====== Ler o Arquivo INI===========================================
function TNFSeR_Conam.LerIni: Boolean;
var
  INIRec: TMemIniFile;
begin
  INIRec := TMemIniFile.Create('');

  // Usar o FpAOwner em vez de  FProvider

  try
    LerIniArquivoOuString(Arquivo, INIRec);

    FpTipoXML := INIRec.ReadString('IdentificacaoNFSe', 'TipoXML', '');

    if FpTipoXML = 'NFSE' then
      LerIniNfse(INIRec)
    else
      LerIniRps(INIRec);

  finally
    INIRec.Free;
  end;

  Result := True;
end;

procedure TNFSeR_Conam.LerIniNfse(AINIRec: TMemIniFile);
begin
  NFSe.tpXML := txmlNFSe;

  LerINIIdentificacaoNFSe(AINIRec);
  LerINIIdentificacaoRps(AINIRec);
  LerINIDadosServico(AINIRec);
  LerINIDadosValores(AINIRec);
  LerINIDadosTomador(AINIRec);
  LerINIIdentificacaoPrestador(AINIRec);
  LerINIListaEmail(AINIRec);
  LerINIValoresNFSe(AINIRec);

  with NFSe.Servico do
  begin
    Valores.RetencoesFederais := Valores.ValorPis + Valores.ValorCofins +
      Valores.ValorInss + Valores.ValorIr + Valores.ValorCsll;

    case Valores.IssRetido of
      stRetencao: Valores.ValorIssRetido := Valores.ValorIss;
      stNormal: Valores.ValorIssRetido := 0;
    end;

    Valores.ValorTotalNotaFiscal := Valores.ValorServicos -
      Valores.DescontoCondicionado - Valores.DescontoIncondicionado;
  end;
end;

procedure TNFSeR_Conam.LerIniRps(AINIRec: TMemIniFile);
begin
  NFSe.tpXML := txmlRPS;

  LerINIIdentificacaoNFSe(AINIRec);
  LerINIIdentificacaoRps(AINIRec);
  LerINIDadosServico(AINIRec);
  LerINIDadosValores(AINIRec);
  LerINIDadosTomador(AINIRec);
  LerINIIdentificacaoPrestador(AINIRec);
  LerINIListaEmail(AINIRec);
end;

procedure TNFSeR_Conam.LerINIIdentificacaoNFSe(AINIRec: TMemIniFile);
var
  sSecao, sData: string;
  Ok: Boolean;
begin
  sSecao := 'IdentificacaoNFSe';

  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.LogradouLocalPrestacaoServico := StrToLogradouroLocalPrestacaoServico(AINIRec.ReadString(sSecao, 'LogradouLocalPrestacaoServico', '1'));

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      NFSe.SeriePrestacao := AINIRec.ReadString(sSecao, 'SeriePrestacao', '');
      NFSe.SituacaoNFSe := StrToStatusNFSe(Ok, AINIRec.ReadString(sSecao, 'StatusNFSe', ''));
      NFSe.MotivoCancelamento := AINIRec.ReadString(sSecao, 'MotivoCancelamento', '');

      sData := AINIRec.ReadString(sSecao, 'DataEmissao', '');
      if sData <> '' then
        NFSe.DataEmissao := StringToDateTimeDef(sData, 0);

      sData := AINIRec.ReadString(sSecao, 'Competencia', '');
      if sData <> '' then
        NFSe.Competencia := StringToDateTimeDef(sData, 0);

      sData := AINIRec.ReadString(sSecao, 'dhRecebimento', '');
      if sData <> '' then
        NFSe.dhRecebimento := StringToDateTimeDef(sData, 0);

      NFSe.CodigoVerificacao := AINIRec.ReadString(sSecao, 'CodigoVerificacao', '');
    end;
  end;
end;

procedure TNFSeR_Conam.LerINIIdentificacaoRps(AINIRec: TMemIniFile);
var
  sSecao, sData: string;
begin
  sSecao := 'IdentificacaoRps';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.IdentificacaoRps.Numero := AINIRec.ReadString(sSecao, 'Numero', '0');
    NFSe.IdentificacaoRps.Serie := AINIRec.ReadString(sSecao, 'Serie', '0');

    sData := AINIRec.ReadString(sSecao, 'DataEmissao', '');
    if sData <> '' then
      NFSe.DataEmissao := StringToDateTimeDef(sData, 0);

    sData := AINIRec.ReadString(sSecao, 'DataEmissaoRps', '');
    if sData <> '' then
      NFSe.DataEmissaoRps := StringToDateTimeDef(sData, 0)
    else
      NFSe.DataEmissaoRPS := NFSe.DataEmissao;

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
    end;
  end;
end;

procedure TNFSeR_Conam.LerINIDadosServico(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Servico';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.ItemListaServico := AINIRec.ReadString(sSecao, 'ItemListaServico', '');
    NFSe.Servico.Discriminacao := AINIRec.ReadString(sSecao, 'Discriminacao', '');
    NFSe.Servico.Discriminacao := StringReplace(NFSe.Servico.Discriminacao,
                FpAOwner.ConfigGeral.QuebradeLinha, sLineBreak, [rfReplaceAll]);

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.Servico.xItemListaServico := AINIRec.ReadString(sSecao, 'xItemListaServico', '');
    end;
  end;
end;

procedure TNFSeR_Conam.LerINIDadosValores(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Valores';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.Valores.ValorServicos := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorServicos', ''), 0);
    NFSe.Servico.Valores.ValorDeducoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorDeducoes', ''), 0);
    NFSe.Servico.Valores.JustificativaDeducao := AINIRec.ReadString(sSecao, 'JustificativaDeducao', '');
    NFSe.Servico.Valores.BaseCalculo := StringToFloatDef(AINIRec.ReadString(sSecao, 'BaseCalculo', ''), 0);
    NFSe.Servico.Valores.Aliquota := StringToFloatDef(AINIRec.ReadString(sSecao, 'Aliquota', ''), 0);
    NFSe.Servico.Valores.ValorIss := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIss', ''), 0);
    NFSe.Servico.Valores.ValorIssRetido := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIssRetido', ''), 0);
    NFSe.Servico.Valores.ValorPis := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorPis', ''), 0);
    NFSe.Servico.Valores.AliquotaPis := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaPis', ''), 0);
    NFSe.Servico.Valores.ValorCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCofins', ''), 0);
    NFSe.Servico.Valores.AliquotaCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaCofins', ''), 0);
    NFSe.Servico.Valores.ValorCsll := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCsll', ''), 0);
    NFSe.Servico.Valores.AliquotaCsll := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaCsll', ''), 0);
    NFSe.Servico.Valores.ValorInss := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorInss', ''), 0);
    NFSe.Servico.Valores.AliquotaInss := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaInss', ''), 0);
    NFSe.Servico.Valores.ValorIr := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIr', ''), 0);
    NFSe.Servico.Valores.AliquotaIr := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaIr', ''), 0);

    if NFSe.tpXML = txmlNFSe then
    begin
      if NFSe.Servico.Valores.ValorIssRetido > 0 then
      begin
        NFSe.Servico.Valores.IssRetido := stRetencao;
        NFSe.Servico.Valores.ValorIss  := 0;
      end
      else
      begin
        NFSe.Servico.Valores.IssRetido := stNormal;
      end;

      NFSe.Servico.Valores.ValorTotalNotaFiscal := NFSe.Servico.Valores.ValorServicos -
        NFSe.Servico.Valores.DescontoCondicionado -
        NFSe.Servico.Valores.DescontoIncondicionado;
    end;
  end;
end;

procedure TNFSeR_Conam.LerINIDadosTomador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Tomador';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
    NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual := AINIRec.ReadString(sSecao, 'InscricaoEstadual', '');

    NFSe.Tomador.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');

    NFSe.Tomador.Endereco.TipoLogradouro := AINIRec.ReadString(sSecao, 'TipoLogradouro', '');
    NFSe.Tomador.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.Tomador.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.Tomador.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.Tomador.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');
    NFSe.Tomador.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.Tomador.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
    NFSe.Tomador.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
    NFSe.Tomador.Endereco.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
    NFSe.Tomador.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
    NFSe.Tomador.Endereco.xPais := AINIRec.ReadString(sSecao, 'xPais', '');

    NFSe.Tomador.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
    NFSe.Tomador.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');
  end;
end;

procedure TNFSeR_Conam.LerINIIdentificacaoPrestador(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Prestador';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Prestador.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
    NFSe.Prestador.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
    NFSe.Prestador.Endereco.TipoLogradouro := AINIRec.ReadString(sSecao, 'TipoLogradouro', '');
    NFSe.Prestador.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.Prestador.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.Prestador.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.Prestador.Endereco.Bairro := UTF8ToNativeString(AINIRec.ReadString(sSecao, 'Bairro', ''));
    NFSe.Prestador.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.Prestador.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');

      NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJ', '');
      NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
      NFSe.Prestador.IdentificacaoPrestador.InscricaoEstadual := AINIRec.ReadString(sSecao, 'InscricaoEstadual', '');

      NFSe.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      NFSe.CodigoVerificacao := AINIRec.ReadString(sSecao, 'CodigoVerificacao', '');
      NFSe.SituacaoNFSe := StrToStatusNFSe(Ok, AINIRec.ReadString(sSecao, 'StatusNFSe', ''));
      NFSe.MotivoCancelamento := AINIRec.ReadString(sSecao, 'MotivoCancelamento', '');

      NFSe.OptanteSimplesNacional := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(sSecao, 'OptanteSN', '1'));
      NFSe.DataOptanteSimplesNacional := AINIRec.ReadDateTime(sSecao, 'DataOptanteSimplesNacional', 0);
    end;
  end;
end;

procedure TNFSeR_Conam.LerINIListaEmail(AINIRec: TMemIniFile);
var
  i: Integer;
  sSecao, sFim: string;
begin
  i := 1;
  while true do
  begin
    sSecao := 'Email' + IntToStrZero(I + 1, 1);
    sFim := AINIRec.ReadString(sSecao, 'emailCC', 'FIM');

    if (Length(sFim) <= 0) or (sFim = 'FIM') then
      break;

    with NFSe.email.New do
      emailCC := sFim;

    Inc(i);
  end;
end;

procedure TNFSeR_Conam.LerINIValoresNFSe(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'ValoresNFSe';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.ValoresNfse.ValorLiquidoNfse := StrToFloatDef(AINIRec.ReadString(sSecao, 'ValorLiquidoNfse', ''), 0);

    if NFSe.Servico.Valores.ValorIssRetido > 0 then
      NFSe.ValoresNfse.ValorLiquidoNfse := NFSe.ValoresNfse.ValorLiquidoNfse -
                                           NFSe.Servico.Valores.ValorIssRetido;

    NFSe.ValoresNfse.BaseCalculo := StrToFloatDef(AINIRec.ReadString(sSecao, 'BaseCalculo', ''), 0);
    NFSe.ValoresNfse.Aliquota := StrToFloatDef(AINIRec.ReadString(sSecao, 'Aliquota', ''), 0);
    NFSe.ValoresNfse.ValorIss := StrToFloatDef(AINIRec.ReadString(sSecao, 'ValorIss', ''), 0);

    NFSe.Servico.Valores.ValorServicos := NFSe.ValoresNfse.ValorLiquidoNfse;
    NFSe.Servico.Valores.BaseCalculo := NFSe.ValoresNfse.BaseCalculo;
    NFSe.Servico.Valores.Aliquota := NFSe.ValoresNfse.Aliquota;
  end;
end;

end.
