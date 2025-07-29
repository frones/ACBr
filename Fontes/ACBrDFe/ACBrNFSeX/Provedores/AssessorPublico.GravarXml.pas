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

unit AssessorPublico.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils, IniFiles,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml;

type
  { Provedor com layout próprio }
  { TNFSeW_AssessorPublico }

  TNFSeW_AssessorPublico = class(TNFSeWClass)
  protected
    function GerarServicos: TACBrXmlNode;
    function GerarServico: TACBrXmlNodeArray;

    // Gerar o arquivo INI
    procedure GerarINIIdentificacaoNFSe(AINIRec: TMemIniFile);
    procedure GerarINIIdentificacaoRps(AINIRec: TMemIniFile);
    procedure GerarINIDadosServico(AINIRec: TMemIniFile);
    procedure GerarINIDadosValores(AINIRec: TMemIniFile);
    procedure GerarINIDadosTomador(AINIRec: TMemIniFile);
    procedure GerarINIListaServico(AINIRec: TMemIniFile);
    procedure GerarINIInformacoesCancelamento(AINIRec: TMemIniFile);
    procedure GerarINIDadosPrestador(AINIRec: TMemIniFile);

    procedure GerarIniRps(AINIRec: TMemIniFile);
    procedure GerarIniNfse(AINIRec: TMemIniFile);
  public
    function GerarXml: Boolean; override;

    function GerarIni: string; override;
  end;

implementation

uses
  ACBrDFe.Conversao,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrNFSeXConsts,
  ACBrNFSeXConversao;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     AssessorPublico
//==============================================================================

{ TNFSeW_AssessorPublico }

function TNFSeW_AssessorPublico.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('NOTA');

  FDocument.Root := NFSeNode;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'LOTE', 1, 15, 1,
                                                          NFSe.NumeroLote, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SEQUENCIA', 1, 15, 1,
                                             NFSe.IdentificacaoRps.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DATAEMISSAO', 1, 10, 1,
                           FormatDateTime('dd/MM/yyyy', NFSe.DataEmissao), ''));

  NFSeNode.AppendChild(AddNode(tcHor, '#1', 'HORAEMISSAO', 1, 10, 1,
                                                         NFSe.DataEmissao, ''));

  if (NFSe.Servico.CodigoMunicipio = IntToStr(NFSe.Servico.MunicipioIncidencia)) or
     (NFSe.Servico.LocalPrestacao = lpMunicipio) then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'LOCAL', 1, 1, 1, 'D', ''))
  else
  begin
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'LOCAL', 1, 1, 1, 'F', ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'UFFORA', 1, 1, 1,
                                                 NFSe.Servico.UFPrestacao, ''));
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MUNICIPIOFORA', 1, 1, 1,
                               IntToStr(NFSe.Servico.MunicipioIncidencia), ''));

//    Gerador.wCampo(tcStr, '', 'PAISFORA', 1, 1, 1, '1', '');
  end;

  NFSeNode.AppendChild(AddNode(tcInt, '#1', 'SITUACAO', 1, 4, 1,
                                                            NFSe.Situacao, ''));

  if NFSe.Servico.Valores.IssRetido = stRetencao then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETIDO', 1, 1, 1, 'S', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETIDO', 1, 1, 1, 'N', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ATIVIDADE', 1, 10, 1,
                                                  NFSe.Servico.CodigoCnae, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ALIQUOTAAPLICADA', 1, 5, 1,
                                            NFSe.Servico.Valores.Aliquota, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'DEDUCAO', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'IMPOSTO', 1, 15, 1,
                                NFSe.Servico.Valores.valorOutrasRetencoes, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'RETENCAO', 1, 15, 1,
                                      NFSe.Servico.Valores.ValorIssRetido, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'OBSERVACAO', 1, 1000, 1,
                                               NFSe.Servico.Discriminacao, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CPFCNPJ', 1, 14, 1,
                                NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RGIE', 1, 14, 1,
                      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NOMERAZAO', 1, 60, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NOMEFANTASIA', 1, 60, 1,
                                                 NFSe.Tomador.NomeFantasia, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MUNICIPIO', 1, 7, 1,
                                    NFSe.Tomador.Endereco.CodigoMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'BAIRRO', 1, 60, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CEP', 1, 10, 1,
                                                NFSe.Tomador.Endereco.CEP, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'PREFIXO', 1, 10, 1,
                                     NFSe.Tomador.Endereco.TipoLogradouro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'LOGRADOURO', 1, 60, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'COMPLEMENTO', 1, 60, 1,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NUMERO', 1, 10, 1,
                                             NFSe.Tomador.Endereco.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'EMAIL', 1, 100, 1,
                                             NFSe.Tomador.Contato.Email, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DENTROPAIS', 1, 1, 1, 'S', ''));

  NFSeNode.AppendChild(AddNode(tcDatVcto, '#1', 'DATAVENCIMENTO', 1, 10, 0,
                                                          NFSe.Vencimento, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'PIS', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  if NFSe.Servico.Valores.ValorPis > 0 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETPIS', 1, 1, 1, 'S', ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'COFINS', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  if NFSe.Servico.Valores.ValorCofins > 0 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETCOFINS', 1, 1, 1, 'S', ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'INSS', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

  if NFSe.Servico.Valores.ValorInss > 0 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETINSS', 1, 1, 1, 'S', ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'IR', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  if NFSe.Servico.Valores.ValorIr > 0 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETIR', 1, 1, 1, 'S', ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'CSLL', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  if NFSe.Servico.Valores.ValorCsll > 0 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETCSLL', 1, 1, 1, 'S', ''));

  xmlNode := GerarServicos;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

function TNFSeW_AssessorPublico.GerarServico: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('SERVICO');

    Result[i].AppendChild(AddNode(tcStr, '#1', 'DESCRICAO', 1, 60, 1,
                              NFSe.Servico.ItemServico.Items[i].Descricao, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'VALORUNIT', 1, 15, 1,
                          NFSe.Servico.ItemServico.Items[i].ValorUnitario, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'QUANTIDADE', 1, 10, 1,
                             NFSe.Servico.ItemServico.Items[i].Quantidade, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'DESCONTO', 1, 10, 1,
                 NFSe.Servico.ItemServico.Items[i].DescontoIncondicionado, ''));
  end;

  if NFSe.Servico.ItemServico.Count > 10 then
    wAlerta('#54', 'SERVICO', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFSeW_AssessorPublico.GerarServicos: TACBrXmlNode;
var
  i : integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := CreateElement('SERVICOS');

  nodeArray := GerarServico;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

//====== Gerar o Arquivo INI===========================================
function TNFSeW_AssessorPublico.GerarIni: string;
var
  INIRec: TMemIniFile;
  IniNFSe: TStringList;
begin
  Result:= '';
// Usar FpAOwner no lugar de FProvider

  INIRec := TMemIniFile.Create('');
  try
    if NFSe.tpXML = txmlRPS then
      GerarIniRps(INIRec)
    else
      GerarIniNfse(INIRec);
  finally
    IniNFSe := TStringList.Create;
    try
      INIRec.GetStrings(IniNFSe);
      INIRec.Free;

      Result := StringReplace(IniNFSe.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniNFSe.Free;
    end;
  end;
end;

procedure TNFSeW_AssessorPublico.GerarIniNfse(AINIRec: TMemIniFile);
begin
  GerarINIIdentificacaoNFSe(AINIRec);
  GerarINIIdentificacaoRps(AINIRec);
  GerarINIDadosServico(AINIRec);
  GerarINIDadosValores(AINIRec);
  GerarINIDadosTomador(AINIRec);
  GerarINIListaServico(AINIRec);
  GerarINIInformacoesCancelamento(AINIRec);
  GerarINIDadosPrestador(AINIRec);
end;

procedure TNFSeW_AssessorPublico.GerarIniRps(AINIRec: TMemIniFile);
begin
  GerarINIIdentificacaoNFSe(AINIRec);
  GerarINIIdentificacaoRps(AINIRec);
  GerarINIDadosServico(AINIRec);
  GerarINIDadosValores(AINIRec);
  GerarINIDadosTomador(AINIRec);
  GerarINIListaServico(AINIRec);
end;

procedure TNFSeW_AssessorPublico.GerarINIIdentificacaoNFSe(
  AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao:= 'IdentificacaoNFSe';

  if NFSe.tpXML = txmlRPS then
  begin
    AINIRec.WriteString(sSecao, 'TipoXML', 'RPS');
    AINIRec.WriteString(sSecao, 'NumeroLote', NFSe.NumeroLote);
  end
  else
  begin
    AINIRec.WriteString(sSecao, 'TipoXML', 'NFSE');
    AINIRec.WriteString(sSecao, 'NumeroLote', NFSe.NumeroLote);
    AINIRec.WriteString(sSecao, 'Link', NFSe.Link);
    AINIRec.WriteString(sSecao, 'Numero', NFSe.Numero);
    AINIRec.WriteString(sSecao, 'StatusNFSe', StatusNFSeToStr(NFSe.SituacaoNfse));
    AINIRec.WriteString(sSecao, 'CodigoVerificacao', NFSe.CodigoVerificacao);
  end;
end;

procedure TNFSeW_AssessorPublico.GerarINIIdentificacaoRps(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao:= 'IdentificacaoRps';

  AINIRec.WriteString(sSecao, 'Numero', NFSe.IdentificacaoRps.Numero);

  if NFSe.DataEmissao > 0 then
    AINIRec.WriteDateTime(sSecao, 'DataEmissao', NFSe.DataEmissao)
  else
    AINIRec.WriteDateTime(sSecao, 'DataEmissao', Now);

  AINIRec.WriteInteger(sSecao, 'Situacao', NFSe.Situacao);

  if NFSe.Vencimento > 0 then
    AINIRec.WriteDate(sSecao, 'Vencimento', NFSe.Vencimento)
  else
    AINIRec.WriteDate(sSecao, 'Vencimento', Now);

  if NFSe.tpXML = txmlNFSe then
  begin
    AINIRec.WriteString(sSecao, 'OutrasInformacoes',
      StringReplace(NFSe.OutrasInformacoes, sLineBreak,
                    FpAOwner.ConfigGeral.QuebradeLinha, [rfReplaceAll]));

    if NFSe.Competencia > 0 then
      AINIRec.WriteDate(sSecao, 'Competencia', NFSe.Competencia)
    else
      AIniRec.WriteDate(sSecao, 'Competencia', Now);
  end;
end;

procedure TNFSeW_AssessorPublico.GerarINIDadosServico(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao:= 'Servico';

  AINIRec.WriteString(sSecao, 'CodigoMunicipio', NFSe.Servico.CodigoMunicipio);
  AINIRec.WriteInteger(sSecao, 'MunicipioIncidencia', NFSe.Servico.MunicipioIncidencia);
  AINIRec.WriteString(sSecao,'LocalPrestacao', LocalPrestacaoToStr(NFSe.Servico.LocalPrestacao));
  AINIRec.WriteString(sSecao, 'UFPrestacao', NFSe.Servico.UFPrestacao);
  AINIRec.WriteString(sSecao, 'CodigoCnae', NFSe.Servico.CodigoCnae);
  AINIRec.WriteString(sSecao, 'Discriminacao', ChangeLineBreak(NFSe.Servico.Discriminacao, FpAOwner.ConfigGeral.QuebradeLinha));

  if NFSe.tpXML = txmlNFSe then
  begin
    AINIRec.WriteString(sSecao, 'ItemListaServico', NFSe.Servico.ItemListaServico);
    AINIRec.WriteString(sSecao, 'xItemListaServico', NFSe.Servico.xItemListaServico);
  end;
end;

procedure TNFSeW_AssessorPublico.GerarINIDadosValores(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao:= 'Valores';

  AINIRec.WriteString(sSecao, 'ISSRetido', FpAOwner.SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido));
  AINIRec.WriteFloat(sSecao, 'Aliquota', NFSe.Servico.Valores.Aliquota);
  AINIRec.WriteFloat(sSecao, 'ValorDeducoes', NFSe.Servico.Valores.ValorDeducoes);
  AINIRec.WriteFloat(sSecao, 'valorOutrasRetencoes', NFSe.Servico.Valores.valorOutrasRetencoes);
  AINIRec.WriteFloat(sSecao, 'ValorIssRetido', NFSe.Servico.Valores.ValorIssRetido);
  AINIRec.WriteFloat(sSecao, 'ValorPis', NFSe.Servico.Valores.ValorPis);
  AINIRec.WriteFloat(sSecao, 'ValorCofins', NFSe.Servico.Valores.ValorCofins);
  AINIRec.WriteFloat(sSecao, 'ValorInss', NFSe.Servico.Valores.ValorInss);
  AINIRec.WriteFloat(sSecao, 'ValorIr', NFSe.Servico.Valores.ValorIr);
  AINIRec.WriteFloat(sSecao, 'ValorCsll', NFSe.Servico.Valores.ValorCsll);

  if NFSe.tpXML = txmlNFSe then
  begin
    AINIRec.WriteFloat(sSecao, 'BaseCalculo', NFSe.Servico.Valores.BaseCalculo);
    AINIRec.WriteFloat(sSecao, 'ValorServicos', NFSe.Servico.Valores.ValorServicos);
    AINIRec.WriteFloat(sSecao, 'ValorIss', NFSe.Servico.Valores.ValorIss);
    AINIRec.WriteFloat(sSecao, 'OutrasRetencoes', NFSe.Servico.Valores.OutrasRetencoes);
  end;
end;

procedure TNFSeW_AssessorPublico.GerarINIDadosTomador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao:= 'Tomador';

  AINIRec.WriteString(sSecao, 'CNPJCPF', NFSe.Tomador.IdentificacaoTomador.CpfCnpj);
  AINIRec.WriteString(sSecao, 'InscricaoEstadual', NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual);

  AINIRec.WriteString(sSecao, 'RazaoSocial', NFSe.Tomador.RazaoSocial);
  AINIRec.WriteString(sSecao, 'NomeFantasia', NFSe.Tomador.NomeFantasia);

  AINIRec.WriteString(sSecao, 'CodigoMunicipio', NFSe.Tomador.Endereco.CodigoMunicipio);
  AINIRec.WriteString(sSecao, 'Bairro', NFSe.Tomador.Endereco.Bairro);
  AINIRec.WriteString(sSecao, 'CEP', NFSe.Tomador.Endereco.CEP);
  AINIRec.WriteString(sSecao, 'TipoLogradouro', NFSe.Tomador.Endereco.TipoLogradouro);
  AINIRec.WriteString(sSecao, 'Logradouro', NFSe.Tomador.Endereco.Endereco);
  AINIRec.WriteString(sSecao, 'Complemento', NFSe.Tomador.Endereco.Complemento);
  AINIRec.WriteString(sSecao, 'Numero', NFSe.Tomador.Endereco.Numero);
  AINIRec.WriteString(sSecao, 'Email', NFSe.Tomador.Contato.Email);

  if NFSe.tpXML = txmlNFSe then
  begin
    AINIRec.WriteString(sSecao, 'InscricaoMunicipal', NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal);
    AINIRec.WriteString(sSecao, 'xMunicipio', NFSe.Tomador.Endereco.xMunicipio);
    AINIRec.WriteString(sSecao, 'UF', NFSe.Tomador.Endereco.UF);
  end;
end;

procedure TNFSeW_AssessorPublico.GerarINIListaServico(AINIRec: TMemIniFile);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    sSecao:= 'Itens' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'Descricao', ChangeLineBreak(NFSe.Servico.ItemServico.Items[I].Descricao, FpAOwner.ConfigGeral.QuebradeLinha));
    AINIRec.WriteFloat(sSecao, 'ValorUnitario', NFSe.Servico.ItemServico.Items[I].ValorUnitario);
    AINIRec.WriteFloat(sSecao, 'Quantidade', NFSe.Servico.ItemServico.Items[I].Quantidade);
    AINIRec.WriteFloat(sSecao, 'DescontoIncondicionado', NFSe.Servico.ItemServico.Items[I].DescontoIncondicionado);

    if NFSe.tpXML = txmlNFSe then
    begin
      AINIRec.WriteString(sSecao, 'CodServico', NFSe.Servico.ItemServico.Items[I].CodServ);
    end;
  end;
end;

procedure TNFSeW_AssessorPublico.GerarINIInformacoesCancelamento(
  AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  if (NFSe.NfseCancelamento.DataHora > 0) or (Trim(NFSe.MotivoCancelamento) <> '')then
  begin
    sSecao := 'NFSeCancelamento';

    AINIRec.WriteDateTime(sSecao, 'DataHora', NFSe.NfSeCancelamento.DataHora);
    AINIRec.WriteString(sSecao, 'MotivoCancelamento', NFSe.MotivoCancelamento);
    AINIRec.WriteString(sSecao, 'JustificativaCancelamento', NFSe.JustificativaCancelamento);
  end;
end;

procedure TNFSeW_AssessorPublico.GerarINIDadosPrestador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao:= 'DadosPrestador';

  AINIRec.WriteString(sSecao, 'OptanteSN', FpAOwner.SimNaoToStr(NFSe.OptanteSimplesNacional));

  AINIRec.WriteString(sSecao, 'RazaoSocial', NFSe.Prestador.RazaoSocial);
  AINIRec.WriteString(sSecao, 'NomeFantasia', NFSe.Prestador.NomeFantasia);
  AINIRec.WriteString(sSecao, 'CNPJ', NFSe.Prestador.IdentificacaoPrestador.CpfCnpj);
  AINIRec.WriteString(sSecao, 'InscricaoMunicipal', NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal);
  AINIRec.WriteString(sSecao, 'Logradouro', NFSe.Prestador.Endereco.Endereco);
  AINIRec.WriteString(sSecao, 'Numero', NFSe.Prestador.Endereco.Numero);
  AINIRec.WriteString(sSecao, 'Bairro', NFSe.Prestador.Endereco.Bairro);
  AINIRec.WriteString(sSecao, 'CodigoMunicipio', NFSe.Prestador.Endereco.CodigoMunicipio);
  AINIRec.WriteString(sSecao, 'xMunicipio', NFSe.Prestador.Endereco.xMunicipio);
  AINIRec.WriteString(sSecao, 'UF',  NFSe.Prestador.Endereco.UF);
  AINIRec.WriteString(sSecao, 'CEP', NFSe.Prestador.Endereco.CEP);
end;

end.
