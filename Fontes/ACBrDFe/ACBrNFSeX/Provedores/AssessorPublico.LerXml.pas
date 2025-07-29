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

unit AssessorPublico.LerXml;

interface

uses
  SysUtils, Classes, StrUtils, IniFiles,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_AssessorPublico }

  TNFSeR_AssessorPublico = class(TNFSeRClass)
  protected
    FpTipoXML: string;

    procedure LerInformacoesCancelamento(const ANode: TACBrXmlNode);

    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;

    //====== Ler o Arquivo INI===========================================
    procedure LerINIIdentificacaoNFSe(AINIRec: TMemIniFile);
    procedure LerINIIdentificacaoRps(AINIRec: TMemIniFile);
    procedure LerINIDadosServico(AINIRec: TMemIniFile);
    procedure LerINIDadosValores(AINIRec: TMemIniFile);
    procedure LerINIDadosTomador(AINIRec: TMemIniFile);
    procedure LerINIListaServico(AINIRec: TMemIniFile);

    procedure LerINIInformacoesCancelamento(AINIRec: TMemIniFile);
    procedure LerINIDadosPrestador(AINIRec: TMemIniFile);

    procedure LerIniRps(AINIRec: TMemIniFile);
    procedure LerIniNfse(AINIRec: TMemIniFile);
  public
    function LerXml: Boolean; override;
    function LerIni: Boolean; override;
  end;

implementation

uses
  ACBrDFe.Conversao,
  ACBrUtil.Base,
  ACBrUtil.XMLHTML,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrNFSeXClass;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     AssessorPublico
//==============================================================================

{ TNFSeR_AssessorPublico }

procedure TNFSeR_AssessorPublico.LerInformacoesCancelamento(const ANode: TACBrXmlNode);
var
  DataCancel, HoraCancel: string;
begin
  if not Assigned(ANode) then exit;

  DataCancel := ObterConteudo(ANode.Childrens.FindAnyNs('DATACANCEL'), tcStr);
  HoraCancel := ObterConteudo(ANode.Childrens.FindAnyNs('HORACANCEL'), tcStr);

  if OnlyNumber(HoraCancel) <> '' then
    DataCancel := DataCancel + ' ' + HoraCancel;

  if OnlyNumber(DataCancel) <> '' then
    NFSe.NfseCancelamento.DataHora := StringToDateTime(DataCancel, 'DD/MM/YYYY hh:nn:ss');

  NFSe.MotivoCancelamento := ObterConteudo(ANode.Childrens.FindAnyNs('MOTIVOCANCEL'), tcStr);
  NFSe.JustificativaCancelamento := ObterConteudo(ANode.Childrens.FindAnyNs('JUSTCANCEL'), tcStr);

  NFSe.NfseCancelamento.Sucesso := (NFSe.NFSeCancelamento.DataHora > 0) or
                                   (Trim(NFSe.MotivoCancelamento) <> '')or
                                   (Trim(NFSe.JustificativaCancelamento) <> '');

  if NFSe.NfseCancelamento.Sucesso then
    NFSe.SituacaoNfse := snCancelado;
end;

function TNFSeR_AssessorPublico.LerXml: Boolean;
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

  if (Pos('RPS', Arquivo) > 0) or (Pos('PRESTCPFCNPJ', Arquivo) > 0) then
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

function TNFSeR_AssessorPublico.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor, aHora: string;
  ANodes: TACBrXmlNodeArray;
  i, mes, ano: integer;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('NOTA');

  if AuxNode = nil then
    AuxNode := ANode;

  LerInformacoesCancelamento(AuxNode);

  NFSe.Link       := ObterConteudo(AuxNode.Childrens.FindAnyNs('LINK'), tcStr);
  NFSe.Link       := StringReplace(NFSe.Link, '&amp;', '&', [rfReplaceAll]);
  NFSe.NumeroLote := ObterConteudo(AuxNode.Childrens.FindAnyNs('LOTE'), tcStr);
  NFSe.Numero     := ObterConteudo(AuxNode.Childrens.FindAnyNs('COD'), tcStr);
  NFSe.Situacao   := ObterConteudo(AuxNode.Childrens.FindAnyNs('SITCOD'), tcInt);

  NFSe.InfID.ID := NFSe.Numero;

  NFSe.IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('SEQUENCIA'), tcStr);
  NFSe.CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('RPS'), tcStr);

  mes := ObterConteudo(AuxNode.Childrens.FindAnyNs('MESCOMP'), tcInt);
  ano := ObterConteudo(AuxNode.Childrens.FindAnyNs('ANOCOMP'), tcInt);

  if (ano > 0) and (mes > 0) then
    NFSe.Competencia := EncodeDataHora(IntToStr(Ano)+ '/' + Poem_Zeros(mes, 2));

  aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('DATA'), tcStr);
  aHora := ObterConteudo(AuxNode.Childrens.FindAnyNs('HORA'), tcStr);

  if OnlyNumber(aHora) <> '' then
    aValor := aValor + ' ' + aHora;

  if OnlyNumber(aValor) <> '' then
    NFSe.DataEmissao := StringToDateTime(aValor, 'DD/MM/YYYY hh:nn:ss');

  NFSe.OptanteSimplesNacional := snNao;

  aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTSUPERSIMP'), tcStr);

  if aValor = 'S' then
    NFSe.OptanteSimplesNacional := snSim;

  NFSe.OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('OBSSERVICO'), tcStr);
  NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);

  with NFSe.Servico do
  begin
//    Discriminacao     := ObterConteudo(AuxNode.Childrens.FindAnyNs('OBSSERVICO'), tcStr);
    ItemListaServico  := ObterConteudo(AuxNode.Childrens.FindAnyNs('ATIVCOD'), tcStr);
    xItemListaServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('ATIVDESC'), tcStr);
    CodigoMunicipio   := ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMMUNICIPIOCOD'), tcStr);
    MunicipioIncidencia := ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMMUNICIPIOCOD'), tcInt);
  end;

  with NFSe.Servico.Valores do
  begin
    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('RETIDO'), tcStr);

    if aValor = 'N' then
      IssRetido := stNormal;
    if aValor = 'S' then
      IssRetido := stRetencao;

    BaseCalculo   := ObterConteudo(AuxNode.Childrens.FindAnyNs('BASECALC'), tcDe2);
    ValorServicos := ObterConteudo(AuxNode.Childrens.FindAnyNs('VALORTOTALSERVICOS'), tcDe2);
    ValorIss      := ObterConteudo(AuxNode.Childrens.FindAnyNs('IMPOSTO'), tcDe2);
    ValorDeducoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('DEDUCAO'), tcDe2);
    ValorOutrasRetencoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('RETENCAO'), tcDe2);
    ValorPis      := ObterConteudo(AuxNode.Childrens.FindAnyNs('PIS'), tcDe2);
    ValorCofins   := ObterConteudo(AuxNode.Childrens.FindAnyNs('COFINS'), tcDe2);
    ValorInss     := ObterConteudo(AuxNode.Childrens.FindAnyNs('INSS'), tcDe2);
    ValorIr       := ObterConteudo(AuxNode.Childrens.FindAnyNs('IR'), tcDe2);
    ValorCsll     := ObterConteudo(AuxNode.Childrens.FindAnyNs('CSLL'), tcDe2);
    ValorIssRetido  := ObterConteudo(AuxNode.Childrens.FindAnyNs('RETENCAO'), tcDe2);

    RetencoesFederais := ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll;

    OutrasRetencoes := OutrasRetencoes +
           ObterConteudo(AuxNode.Childrens.FindAnyNs('ICMS'), tcDe2) +
           ObterConteudo(AuxNode.Childrens.FindAnyNs('IOF'), tcDe2) +
           ObterConteudo(AuxNode.Childrens.FindAnyNs('CIDE'), tcDe2) +
           ObterConteudo(AuxNode.Childrens.FindAnyNs('OUTROSTRIBUTOS'), tcDe2) +
           ObterConteudo(AuxNode.Childrens.FindAnyNs('OUTRASRETENCOES'), tcDe2) +
           ObterConteudo(AuxNode.Childrens.FindAnyNs('IPI'), tcDe2);

    Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('ALIQUOTA'), tcDe4);
  end;

  with NFSe.Prestador do
  begin
    RazaoSocial  := ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTNOMERAZAO'), tcStr);
    NomeFantasia := ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTNOMERAZAO'), tcStr);
  end;

  with NFSe.Prestador.IdentificacaoPrestador do
  begin
    CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTCPFCNPJ'), tcStr);
    InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTINSCRICAOMUN'), tcStr);
  end;

  with NFSe.Prestador.Endereco do
  begin
    Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTPREFIXODESC'), tcStr) +
         ' ' + ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTLOGDESC'), tcStr);
    Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTNUMERO'), tcStr);

    Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTBAIRRODESC'), tcStr);
    CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTMUNICIPIOCOD'), tcStr);
    xMunicipio      := ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTMUNICIPIODESC'), tcStr);
    UF              := ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTMUNICIPIOUF'), tcStr);
    CEP             := ObterConteudo(AuxNode.Childrens.FindAnyNs('PRESTCEP'), tcStr);
  end;

  with NFSe.Tomador do
  begin
    RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMNOMERAZAO'), tcStr);
  end;

  with NFSe.Tomador.IdentificacaoTomador do
  begin
    CpfCnpj            := ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMCPFCNPJ'), tcStr);
    InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMINSCRICAOMUN'), tcStr);
  end;

  with NFSe.Tomador.Endereco do
  begin
    Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMPREFIXODESC'), tcStr) +
           ' ' + ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMLOGDESC'), tcStr);
    Numero   := ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMNUMERO'), tcStr);

    Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMBAIRRODESC'), tcStr);
    CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMMUNICIPIOCOD'), tcStr);
    xMunicipio      := ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMMUNICIPIODESC'), tcStr);
    UF              := ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMMUNICIPIOUF'), tcStr);
    CEP             := ObterConteudo(AuxNode.Childrens.FindAnyNs('TOMCEP'), tcStr);
  end;

  AuxNode := AuxNode.Childrens.FindAnyNs('SERVICOS');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('SERVICO');

    NFSe.Servico.ItemServico.Clear;

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        CodServ       := ObterConteudo(ANodes[i].Childrens.FindAnyNs('CODIGO'), tcStr);
        Descricao     := ObterConteudo(ANodes[i].Childrens.FindAnyNs('DESCRICAO'), tcStr);
        Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);
        Quantidade    := ObterConteudo(ANodes[i].Childrens.FindAnyNs('QUANTIDADE'), tcDe2);
        ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('VALOR'), tcDe2);

        DescontoIncondicionado := ObterConteudo(ANodes[i].Childrens.FindAnyNs('DESCONTO'), tcDe2);

        ValorTotal := Quantidade * ValorUnitario;
        Tributavel := snSim;
      end;
    end;
  end;

  with NFSe.Servico.Valores do
  begin
    ValorLiquidoNfse := ValorServicos -
        (ValorDeducoes + DescontoCondicionado +
         DescontoIncondicionado + ValorIssRetido);

    ValorTotalNotaFiscal := ValorServicos - DescontoCondicionado -
                            DescontoIncondicionado;
  end;
end;

function TNFSeR_AssessorPublico.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('NOTAS');

  if Assigned(AuxNode) then
    AuxNode := AuxNode.Childrens.FindAnyNs('NOTA');

  if AuxNode = nil then
    AuxNode := ANode;

  with NFSe do
  begin
    NumeroLote := ObterConteudo(AuxNode.Childrens.FindAnyNs('LOTE'), tcStr);

    IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('SEQUENCIA'), tcStr);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('DATAEMISSAO'), tcStr);
    aValor := aValor + ' ' +
              ObterConteudo(AuxNode.Childrens.FindAnyNs('HORAEMISSAO'), tcStr);

    DataEmissao := StrToDateTimeDef(aValor, 0);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('LOCAL'), tcStr);

    if aValor = 'D' then
      NFSe.Servico.LocalPrestacao := lpMunicipio
    else
      NFSe.Servico.LocalPrestacao := lpForaMunicipio;

    NFSe.Servico.UFPrestacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('UFFORA'), tcStr);
    NFSe.Servico.MunicipioIncidencia := ObterConteudo(AuxNode.Childrens.FindAnyNs('MUNICIPIOFORA'), tcInt);

    Situacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('SITUACAO'), tcInt);

    Servico.CodigoCnae := ObterConteudo(AuxNode.Childrens.FindAnyNs('ATIVIDADE'), tcStr);

    Servico.Discriminacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('OBSERVACAO'), tcStr);
    Servico.Discriminacao := StringReplace(Servico.Discriminacao, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('RETIDO'), tcStr);

    if aValor = 'S' then
      Servico.Valores.IssRetido := stRetencao
    else
      Servico.Valores.IssRetido := stNormal;

    Servico.Valores.Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('ALIQUOTAAPLICADA'), tcDe2);

    Servico.Valores.ValorDeducoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('DEDUCAO'), tcDe2);

    Servico.Valores.valorOutrasRetencoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('IMPOSTO'), tcDe2);

    Servico.Valores.ValorIssRetido := ObterConteudo(AuxNode.Childrens.FindAnyNs('RETENCAO'), tcDe2);

    Servico.Valores.ValorPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('PIS'), tcDe2);

    Servico.Valores.ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('COFINS'), tcDe2);

    Servico.Valores.ValorInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('INSS'), tcDe2);

    Servico.Valores.ValorIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('IR'), tcDe2);

    Servico.Valores.ValorCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('CSLL'), tcDe2);

    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CPFCNPJ'), tcStr);

    Tomador.IdentificacaoTomador.InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('RGIE'), tcStr);

    Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('NOMERAZAO'), tcStr);

    Tomador.NomeFantasia := ObterConteudo(AuxNode.Childrens.FindAnyNs('NOMEFANTASIA'), tcStr);

    Tomador.Endereco.CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('MUNICIPIO'), tcStr);

    Tomador.Endereco.Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('BAIRRO'), tcStr);

    Tomador.Endereco.CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);

    Tomador.Endereco.TipoLogradouro := ObterConteudo(AuxNode.Childrens.FindAnyNs('PREFIXO'), tcStr);

    Tomador.Endereco.Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('LOGRADOURO'), tcStr);

    Tomador.Endereco.Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('COMPLEMENTO'), tcStr);

    Tomador.Endereco.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NUMERO'), tcStr);

    Tomador.Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('EMAIL'), tcStr);

    NFSe.Vencimento := ObterConteudo(AuxNode.Childrens.FindAnyNs('DATAVENCIMENTO'), tcDatVcto);

    AuxNode := AuxNode.Childrens.FindAnyNs('SERVICOS');

    if AuxNode <> nil then
    begin
      ANodes := AuxNode.Childrens.FindAllAnyNs('SERVICO');

      Servico.ItemServico.Clear;

      for i := 0 to Length(ANodes) - 1 do
      begin
        Servico.ItemServico.New;

        NFSe.Servico.ItemServico[i].Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('DESCRICAO'), tcStr);
        NFSe.Servico.ItemServico[i].Descricao := StringReplace(NFSe.Servico.ItemServico[i].Descricao, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);

        NFSe.Servico.ItemServico[i].ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('VALORUNIT'), tcDe2);

        NFSe.Servico.ItemServico[i].Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('QUANTIDADE'), tcDe4);

        NFSe.Servico.ItemServico[i].ValorTotal := NFSe.Servico.ItemServico[i].Quantidade *
          NFSe.Servico.ItemServico[i].ValorUnitario;

        NFSe.Servico.ItemServico[i].DescontoIncondicionado := ObterConteudo(ANodes[i].Childrens.FindAnyNs('DESCONTO'), tcDe2);
      end;
    end;
  end;
end;

//====== Ler o Arquivo INI===========================================
function TNFSeR_AssessorPublico.LerIni: Boolean;
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

procedure TNFSeR_AssessorPublico.LerIniNfse(AINIRec: TMemIniFile);
begin
  NFSe.tpXML := txmlNFSe;

  LerINIInformacoesCancelamento(AINIRec);

  LerINIIdentificacaoNFSe(AINIRec);
  LerINIIdentificacaoRps(AINIRec);
  LerINIDadosPrestador(AINIRec);
  LerINIDadosTomador(AINIRec);
  LerINIDadosServico(AINIRec);
  LerINIDadosValores(AINIRec);
  LerINIListaServico(AINIRec);

  case NFSe.Servico.Valores.IssRetido of
    stRetencao: NFSe.Servico.Valores.ValorIssRetido := NFSe.Servico.Valores.ValorIss;
    stNormal: NFSe.Servico.Valores.ValorIssRetido := 0;
  end;

  NFSe.Servico.Valores.ValorTotalNotaFiscal := NFSe.Servico.Valores.ValorServicos -
                                               NFSe.Servico.Valores.DescontoCondicionado -
                                               NFSe.Servico.Valores.DescontoIncondicionado;

  NFSe.Servico.Valores.ValorLiquidoNfse := NFSe.Servico.Valores.ValorServicos -
                                           (NFSe.Servico.Valores.ValorDeducoes +
                                            NFSe.Servico.Valores.DescontoCondicionado +
                                            NFSe.Servico.Valores.DescontoIncondicionado +
                                            NFSe.Servico.Valores.ValorIssRetido);
end;

procedure TNFSeR_AssessorPublico.LerIniRps(AINIRec: TMemIniFile);
begin
  NFSe.tpXML := txmlRPS;

  LerINIIdentificacaoNFSe(AINIRec);
  LerINIIdentificacaoRps(AINIRec);
  LerINIDadosServico(AINIRec);
  LerINIDadosValores(AINIRec);
  LerINIDadosTomador(AINIRec);
  LerINIListaServico(AINIRec);
end;

procedure TNFSeR_AssessorPublico.LerINIIdentificacaoNFSe(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'IdentificacaoNFSe';

  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.NumeroLote := AINIRec.ReadString(sSecao, 'NumeroLote', '');

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.Link := AINIRec.ReadString(sSecao, 'Link', '');
      NFSe.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      NFSe.SituacaoNFSe := StrToStatusNFSe(Ok, AINIRec.ReadString(sSecao, 'StatusNFSe', ''));
      NFSe.CodigoVerificacao := AINIRec.ReadString(sSecao, 'CodigoVerificacao', '');

      NFSe.InfID.ID := NFSe.Numero;
      {
      NFSe.MotivoCancelamento := AINIRec.ReadString(sSecao, 'MotivoCancelamento', '');
      }
    end;
  end;
end;

procedure TNFSeR_AssessorPublico.LerINIIdentificacaoRps(AINIRec: TMemIniFile);
var
  sSecao, sData: string;
begin
  sSecao := 'IdentificacaoRps';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.IdentificacaoRps.Numero := AINIRec.ReadString(sSecao, 'Numero', '0');

    sData := AINIRec.ReadString(sSecao, 'DataEmissao', '');
    if sData <> '' then
      NFSe.DataEmissao := StringToDateTimeDef(sData, 0);

    NFSe.Situacao := AINIRec.ReadInteger(sSecao, 'Situacao', 0);

    sData := AINIRec.ReadString(sSecao, 'Vencimento', '');
    if sData <> '' then
      NFSe.Vencimento := StringToDateTimeDef(sData, 0);

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.OutrasInformacoes := AINIRec.ReadString(sSecao, 'OutrasInformacoes', '');
      NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpAOwner.ConfigGeral.QuebradeLinha, sLineBreak, [rfReplaceAll]);

      sData := AINIRec.ReadString(sSecao, 'Competencia', '');
      if sData <> '' then
        NFSe.Competencia := StringToDateTimeDef(sData, 0);
    end;

    {
    NFSe.IdentificacaoRps.Serie := AINIRec.ReadString(sSecao, 'Serie', '0');
    NFSe.IdentificacaoRps.Tipo := FpAOwner.StrToTipoRPS(Ok, AINIRec.ReadString(sSecao, 'Tipo', '1'));
    }
  end;
end;

procedure TNFSeR_AssessorPublico.LerINIDadosServico(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Servico';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.Servico.MunicipioIncidencia := AINIRec.ReadInteger(sSecao, 'MunicipioIncidencia', 0);
    NFSe.Servico.LocalPrestacao := StrToLocalPrestacao(Ok, AINIRec.ReadString(sSecao, 'LocalPrestacao', '1'));
    NFSe.Servico.UFPrestacao := AINIRec.ReadString(sSecao, 'UFPrestacao', '');
    NFSe.Servico.CodigoCnae := AINIRec.ReadString(sSecao, 'CodigoCnae', '');
    NFSe.Servico.Discriminacao := AINIRec.ReadString(sSecao, 'Discriminacao', '');
    NFSe.Servico.Discriminacao := StringReplace(NFSe.Servico.Discriminacao, FpAOwner.ConfigGeral.QuebradeLinha, sLineBreak, [rfReplaceAll]);

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.Servico.ItemListaServico := AINIRec.ReadString(sSecao, 'ItemListaServico', '');
      NFSe.Servico.xItemListaServico := AINIRec.ReadString(sSecao, 'xItemListaServico', '');
    end;

    {
    NFSe.Servico.CodigoTributacaoMunicipio := AINIRec.ReadString(sSecao, 'CodigoTributacaoMunicipio', '');
    NFSe.Servico.ExigibilidadeISS := FpAOwner.StrToExigibilidadeISS(Ok, AINIRec.ReadString(sSecao, 'ExigibilidadeISS', '1'));
    NFSe.Servico.NumeroProcesso := AINIRec.ReadString(sSecao, 'NumeroProcesso', '');
    NFSe.Servico.ResponsavelRetencao := FpAOwner.StrToResponsavelRetencao(Ok, AINIRec.ReadString(sSecao, 'ResponsavelRetencao', ''));

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.Servico.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
    end;
    }
  end;
end;

procedure TNFSeR_AssessorPublico.LerINIDadosValores(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Valores';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.Valores.ISSRetido := FpAOwner.StrToSituacaoTributaria(Ok, AINIRec.ReadString(sSecao, 'ISSRetido', '0'));
    NFSe.Servico.Valores.Aliquota := StringToFloatDef(AINIRec.ReadString(sSecao, 'Aliquota', ''), 0);
    NFSe.Servico.Valores.ValorDeducoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorDeducoes', ''), 0);
    NFSe.Servico.Valores.valorOutrasRetencoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'valorOutrasRetencoes', ''), 0);
    NFSe.Servico.Valores.ValorIssRetido := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIssRetido', ''), 0);
    NFSe.Servico.Valores.ValorPis := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorPis', ''), 0);
    NFSe.Servico.Valores.ValorCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCofins', ''), 0);
    NFSe.Servico.Valores.ValorInss := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorInss', ''), 0);
    NFSe.Servico.Valores.ValorIr := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIr', ''), 0);
    NFSe.Servico.Valores.ValorCsll := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCsll', ''), 0);
    {
    NFSe.Servico.Valores.DescontoIncondicionado := StringToFloatDef(AINIRec.ReadString(sSecao, 'DescontoIncondicionado', ''), 0);
    NFSe.Servico.Valores.ValorLiquidoNfse := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorLiquidoNfse', ''), 0);
    }

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.Servico.Valores.BaseCalculo := StringToFloatDef(AINIRec.ReadString(sSecao, 'BaseCalculo', ''), 0);
      NFSe.Servico.Valores.ValorServicos := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorServicos', ''), 0);
      NFSe.Servico.Valores.ValorIss := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIss', ''), 0);
      NFSe.Servico.Valores.OutrasRetencoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'OutrasRetencoes', ''), 0);

      NFSe.Servico.Valores.RetencoesFederais := NFSe.Servico.Valores.ValorPis +
                                                NFSe.Servico.Valores.ValorCofins +
                                                NFSe.Servico.Valores.ValorInss +
                                                NFSe.Servico.Valores.ValorIr +
                                                NFSe.Servico.Valores.ValorCsll;
    end;
  end;
end;

procedure TNFSeR_AssessorPublico.LerINIDadosTomador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Tomador';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual := AINIRec.ReadString(sSecao, 'InscricaoEstadual', '');

    NFSe.Tomador.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');
    NFSe.Tomador.NomeFantasia := AINIRec.ReadString(sSecao, 'NomeFantasia', '');

    NFSe.Tomador.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.Tomador.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');
    NFSe.Tomador.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
    NFSe.Tomador.Endereco.TipoLogradouro := AINIRec.ReadString(sSecao, 'TipoLogradouro', '');
    NFSe.Tomador.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.Tomador.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.Tomador.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.Tomador.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');

    if NFSe.tpXML = txmlNFSe then
    begin
      NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
      NFSe.Tomador.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
      NFSe.Tomador.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
    end;

    {
    NFSe.Tomador.Endereco.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
    NFSe.Tomador.Endereco.xPais := AINIRec.ReadString(sSecao, 'xPais', '');

    NFSe.Tomador.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
    }
  end;
end;

procedure TNFSeR_AssessorPublico.LerINIListaServico(AINIRec: TMemIniFile);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TItemServicoCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'Itens' + IntToStrZero(i, 3);
    sFim := AINIRec.ReadString(sSecao, 'Descricao'  ,'FIM');

    if (sFim = 'FIM') then
      break;

    Item := NFSe.Servico.ItemServico.New;

    Item.Descricao := StringReplace(sFim, FpAOwner.ConfigGeral.QuebradeLinha, sLineBreak, [rfReplaceAll]);
    Item.ValorUnitario := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorUnitario', ''), 0);
    Item.Quantidade := StringToFloatDef(AINIRec.ReadString(sSecao, 'Quantidade', ''), 0);
    Item.DescontoIncondicionado := StringToFloatDef(AINIRec.ReadString(sSecao, 'DescontoIncondicionado', ''), 0);

    if NFSe.tpXML = txmlNFSe then
    begin
      Item.CodServ := AINIRec.ReadString(sSecao, 'CodServico', '');
      Item.ValorTotal := Item.Quantidade * Item.ValorUnitario;
      Item.Tributavel := snSim;
    end;

    Inc(i);
  end;
end;

procedure TNFSeR_AssessorPublico.LerINIInformacoesCancelamento(
  AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'NFSeCancelamento';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.NfSeCancelamento.DataHora := AINIRec.ReadDateTime(sSecao, 'DataHora', 0);
    NFSe.MotivoCancelamento := AINIRec.ReadString(sSecao, 'MotivoCancelamento', '');
    NFSe.JustificativaCancelamento := AINIRec.ReadString(sSecao, 'JustificativaCancelamento', '');

    NFSe.NfseCancelamento.Sucesso := (NFSe.NFSeCancelamento.DataHora > 0) or
                                     (Trim(NFSe.MotivoCancelamento) <> '')or
                                     (Trim(NFSe.JustificativaCancelamento) <> '');

    if NFSe.NfseCancelamento.Sucesso then
      NFSe.SituacaoNfse := snCancelado;

    {
    NFSe.CodigoCancelamento := AINIRec.ReadString(sSecao, 'CodigoCancelamento', '');

    NFSe.NFSeCancelamento.Pedido.IdentificacaoNfse.Numero := AINIRec.ReadString(sSecao, 'NumeroNFSe', '');
    NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.Cnpj := AINIRec.ReadString(sSecao, 'CNPJ', '');
    NFSe.NFSeCancelamento.Pedido.IdentificacaoNfse.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
    NFSe.NFSeCancelamento.Pedido.IdentificacaoNfse.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.NfseCancelamento.Pedido.CodigoCancelamento := AINIRec.ReadString(sSecao, 'CodCancel', '');
    NFSe.NFSeCancelamento.Sucesso := AINIRec.ReadBool(sSecao, 'Sucesso', True);
    }
  end;
end;

procedure TNFSeR_AssessorPublico.LerINIDadosPrestador(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'DadosPrestador';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.OptanteSimplesNacional := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(sSecao, 'OptanteSN', '1'));

    NFSe.Prestador.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');
    NFSe.Prestador.NomeFantasia := AINIRec.ReadString(sSecao, 'NomeFantasia', '');

    NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJ', '');
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');

    NFSe.Prestador.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.Prestador.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.Prestador.Endereco.Bairro := UTF8ToNativeString(AINIRec.ReadString(sSecao, 'Bairro', ''));
    NFSe.Prestador.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.Prestador.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
    NFSe.Prestador.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
    NFSe.Prestador.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');

    {
    NFSe.Prestador.Endereco.TipoLogradouro := AINIRec.ReadString(sSecao, 'TipoLogradouro', '');
    NFSe.Prestador.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.Prestador.Endereco.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
    NFSe.Prestador.Endereco.xPais := AINIRec.ReadString(sSecao, 'xPais', '');
    NFSe.Prestador.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
    NFSe.Prestador.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');

    NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, AINIRec.ReadString(sSecao, 'Regime', '0'));
    NFSe.OptanteMEISimei := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(sSecao, 'OptanteMEISimei', ''));
    }
  end;
end;

end.
