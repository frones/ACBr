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

unit Infisc.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXGravarXml_ABRASFv2,
  ACBrNFSeXConversao;

type
  { TNFSeW_Infisc }

  TNFSeW_Infisc = class(TNFSeWClass)
  private
    FPdTotBCISS: Double;
    FPdTotISS: Double;
    FPVersao: TVersaoNFSe;

  protected
    procedure Configuracao; override;

    function GerarinfNFSe: TACBrXmlNode;
    function GerarID: TACBrXmlNode;
    function GerarEmitente: TACBrXmlNode;
    function GerarEnderecoEmitente: TACBrXmlNode;
    function GerarTomador: TACBrXmlNode;
    function GerarEnderecoTomador: TACBrXmlNode;
    function GerarDadosdaObra: TACBrXmlNode;
    function GerarTransportadora: TACBrXmlNode;
    function GerarDetalhamento: TACBrXmlNodeArray;
    function GerarServico(Item: Integer): TACBrXmlNode;
    function GerarISSST(Item: Integer): TACBrXmlNode;
    function GerarTotal: TACBrXmlNode;
    function GerarRetencao: TACBrXmlNode;
    function GerarISS: TACBrXmlNode;
    function GerarCobrancav100: TACBrXmlNode;
    function GerarDuplicatasv100: TACBrXmlNodeArray;
    function GerarCobrancav110: TACBrXmlNode;
    function GerarDuplicatasv110: TACBrXmlNodeArray;
    function GerarDespesa: TACBrXmlNode;
    function GerarDespesas: TACBrXmlNodeArray;

    function GerarObservacoes: TACBrXmlNode;
  public
    function GerarXml: Boolean; override;

  end;

  { TNFSeW_Infisc101 }

  TNFSeW_Infisc101 = class(TNFSeW_Infisc)
  protected
    procedure Configuracao; override;

  end;

  { TNFSeW_Infisc201 }

  TNFSeW_Infisc201 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

    procedure DefinirIDRps; override;

  end;

  { TNFSeW_Infisc203 }

  TNFSeW_Infisc203 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

  end;

implementation

uses
  ACBrUtil.Strings, ACBrDFeUtil,
  ACBrNFSeXConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Infisc
//==============================================================================

{ TNFSeW_Infisc }

function TNFSeW_Infisc.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('NFS-e');

  FDocument.Root := NFSeNode;

  NFSe.InfID.ID  := NFSe.Numero;

  xmlNode := GerarinfNFSe;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

procedure TNFSeW_Infisc.Configuracao;
begin
  inherited Configuracao;

  FPVersao := ve100;
end;

function TNFSeW_Infisc.GerarCobrancav100: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('cobr');

  nodeArray := GerarDuplicatasv100;

  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_Infisc.GerarCobrancav110: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('faturas');

  nodeArray := GerarDuplicatasv110;

  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_Infisc.GerarDadosdaObra: TACBrXmlNode;
begin
  Result := CreateElement('dadosDaObra');

  Result.AppendChild(AddNode(tcStr, '#1', 'xLogObra', 1, 100, 1,
                                   NFSe.ConstrucaoCivil.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xComplObra', 1, 100, 1,
                                NFSe.ConstrucaoCivil.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'vNumeroObra', 1, 6, 1,
                                     NFSe.ConstrucaoCivil.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xBairroObra', 1, 100, 1,
                                     NFSe.ConstrucaoCivil.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xCepObra', 1, 8, 1,
                                        NFSe.ConstrucaoCivil.Endereco.CEP, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'cCidadeObra', 1, 7, 1,
                            NFSe.ConstrucaoCivil.Endereco.CodigoMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xCidadeObra', 1, 60, 1,
                                 NFSe.ConstrucaoCivil.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xUfObra', 1, 2, 1,
                                         NFSe.ConstrucaoCivil.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcInt, '#1', 'cPaisObra', 1, 10, 1,
                                 NFSe.ConstrucaoCivil.Endereco.CodigoPais, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xPaisObra', 1, 100, 1,
                                      NFSe.ConstrucaoCivil.Endereco.xPais, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'numeroArt', 1, 12, 0,
                                                 NFSe.ConstrucaoCivil.Art, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'numeroCei', 1, 12, 0,
                                                NFSe.ConstrucaoCivil.nCei, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'numeroProj', 1, 15, 0,
                                               NFSe.ConstrucaoCivil.nProj, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'numeroMatri', 1, 15, 0,
                                              NFSe.ConstrucaoCivil.nMatri, ''));
end;

function TNFSeW_Infisc.GerarDespesa: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('despesas');

  nodeArray := GerarDespesas;

  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_Infisc.GerarDespesas: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, NFSe.Despesa.Count);

  for i := 0 to NFSe.Despesa.Count - 1 do
  begin
    Result[i] := CreateElement('despesa');

    Result[i].AppendChild(AddNode(tcStr, '#1', 'nItemDesp', 1, 100, 1,
                                          NFSe.Despesa.Items[i].nItemDesp, ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'xDesp', 1, 100, 1,
                                              NFSe.Despesa.Items[i].xDesp, ''));

    Result[i].AppendChild(AddNode(tcDat, '#1', 'dDesp', 1, 15, 1,
                                              NFSe.Despesa.Items[i].dDesp, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'vDesp', 1, 15, 1,
                                              NFSe.Despesa.Items[i].vDesp, ''));
  end;

  if NFSe.Servico.ItemServico.Count > 999 then
    wAlerta('#54', 'despesa', '', ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TNFSeW_Infisc.GerarDetalhamento: TACBrXmlNodeArray;
var
  i: Integer;
  xmlNode: TACBrXmlNode;
begin
  FPdTotBCISS := 0;
  FPdTotISS   := 0;

  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('det');

    Result[i].AppendChild(AddNode(tcStr, '#1', 'nItem', 1, 2, 1,
                                                          IntToStr(i + 1), ''));

    xmlNode := GerarServico(i);
    Result[i].AppendChild(xmlNode);

    // Retenção ISSQN
    if NFSe.Servico.ItemServico[i].ValorISSST > 0 then
    begin
      xmlNode := GerarISSST(i);
      Result[i].AppendChild(xmlNode);
    end;
  end;

  if NFSe.Servico.ItemServico.Count > 999 then
    wAlerta('#54', 'det', '', ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TNFSeW_Infisc.GerarDuplicatasv100: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, NFSe.CondicaoPagamento.Parcelas.Count);

  for i := 0 to NFSe.CondicaoPagamento.Parcelas.Count - 1 do
  begin
    Result[i] := CreateElement('dup');

    Result[i].AppendChild(AddNode(tcStr, '#1', 'nDup', 1, 9, 1,
                               NFSe.CondicaoPagamento.Parcelas[i].Parcela, ''));

    Result[i].AppendChild(AddNode(tcDat, '#1', 'dVenc', 1, 10, 1,
                        NFSe.CondicaoPagamento.Parcelas[i].DataVencimento, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'vDup', 1, 15, 1,
                                 NFSe.CondicaoPagamento.Parcelas[i].Valor, ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'bBol', 1, 1, 1, '2', ''));
  end;

  if NFSe.CondicaoPagamento.Parcelas.Count > 999 then
    wAlerta('#54', 'fat', '', ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TNFSeW_Infisc.GerarDuplicatasv110: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, NFSe.CondicaoPagamento.Parcelas.Count);

  for i := 0 to NFSe.CondicaoPagamento.Parcelas.Count - 1 do
  begin
    Result[i] := CreateElement('fat');

    Result[i].AppendChild(AddNode(tcStr, '#1', 'nItem', 1, 9, 1, (i + 1), ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'nFat', 1, 9, 1,
                               NFSe.CondicaoPagamento.Parcelas[i].Parcela, ''));

    Result[i].AppendChild(AddNode(tcDat, '#1', 'dVenc', 1, 10, 1,
                        NFSe.CondicaoPagamento.Parcelas[i].DataVencimento, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'vFat', 1, 15, 1,
                                 NFSe.CondicaoPagamento.Parcelas[i].Valor, ''));
  end;

  if NFSe.CondicaoPagamento.Parcelas.Count > 999 then
    wAlerta('#54', 'fat', '', ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TNFSeW_Infisc.GerarEmitente: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  if  FPVersao = ve100 then
    Result := CreateElement('emit')
  else
    Result := CreateElement('prest');

  Result.AppendChild(AddNode(tcStr, '#1', 'CNPJ', 1, 14, 1,
                            NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xNome', 1, 100, 1,
                                               NFSe.Prestador.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xFant', 1, 60, 0,
                                              NFSe.Prestador.NomeFantasia, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'IM', 1, 15, 1,
                 NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, ''));

  if FPVersao = ve101 then
  begin
    Result.AppendChild(AddNode(tcStr, '#1', 'xEmail', 1, 50, 0,
                                             NFSe.Prestador.Contato.Email, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'xSite', 1, 50, 0,
                                             NFSe.Prestador.Contato.xSite, ''));
  end;

  xmlNode := GerarEnderecoEmitente;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'fone', 1, 100, 1,
                                          NFSe.Prestador.Contato.Telefone, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'IE', 1, 15, 0,
                   NFSe.Prestador.IdentificacaoPrestador.InscricaoEstadual, ''));

  case Nfse.RegimeEspecialTributacao of
    retSimplesNacional:
      Result.AppendChild(AddNode(tcStr, '#1', 'regimeTrib', 1, 1, 1, '1', ''));

    retMicroempresarioEmpresaPP:
      Result.AppendChild(AddNode(tcStr, '#1', 'regimeTrib', 1, 1, 1, '2', ''));

    retLucroReal,
    retLucroPresumido,
    retNenhum:
      Result.AppendChild(AddNode(tcStr, '#1', 'regimeTrib', 1, 1, 1, '3', ''));
  else
    Result.AppendChild(AddNode(tcStr, '#1', 'regimeTrib', 1, 1, 1, '1', ''));
  end;
end;

function TNFSeW_Infisc.GerarEnderecoEmitente: TACBrXmlNode;
begin
  Result := CreateElement('end');

  Result.AppendChild(AddNode(tcStr, '#1', 'xLgr', 1, 100, 1,
                                         NFSe.Prestador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'nro', 1, 15, 1,
                                           NFSe.Prestador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xCpl', 1, 100, 0,
                                      NFSe.Prestador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xBairro', 1, 100, 1,
                                           NFSe.Prestador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'cMun', 1, 7, 1,
                                  NFSe.Prestador.Endereco.CodigoMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xMun', 1, 60, 1,
                                       NFSe.Prestador.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'UF', 1, 2, 1,
                                               NFSe.Prestador.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'CEP', 1, 8, 1,
                                              NFSe.Prestador.Endereco.CEP, ''));

  Result.AppendChild(AddNode(tcInt, '#1', 'cPais', 1, 10, 1,
                                       NFSe.Prestador.Endereco.CodigoPais, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xPais', 1, 100, 1,
                                            NFSe.Prestador.Endereco.xPais, ''));
end;

function TNFSeW_Infisc.GerarEnderecoTomador: TACBrXmlNode;
begin
  Result := CreateElement('ender');

  Result.AppendChild(AddNode(tcStr, '#1', 'xLgr', 1, 100, 0,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'nro', 1, 15, 0,
                                             NFSe.Tomador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xCpl', 1, 100, 0,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xBairro', 1, 100, 0,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'cMun', 1, 7, 0,
                                    NFSe.Tomador.Endereco.CodigoMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xMun', 1, 60, 0,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'UF', 1, 2, 0,
                                                 NFSe.Tomador.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'CEP', 1, 8, 0,
                                                NFSe.Tomador.Endereco.CEP, ''));

  Result.AppendChild(AddNode(tcInt, '#1', 'cPais', 1, 10, 0, '1058', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xPais', 1, 100, 0, 'Brasil', ''));
end;

function TNFSeW_Infisc.GerarID: TACBrXmlNode;
var
  cUF, CNPJ, Modelo, aSerie, Numero, Codigo, sChave: string;

const
  DFeUF: array[0..26] of String =
  ('AC','AL','AP','AM','BA','CE','DF','ES','GO','MA','MT','MS','MG','PA',
   'PB','PR','PE','PI','RJ','RN','RS','RO','RR','SC','SP','SE','TO');

  DFeUFCodigo: array[0..26] of Integer =
  (12,27,16,13,29,23,53,32,52,21,51,50,31,15,25,41,26,22,33,24,43,11,14,42,35,28,17);

function UFtoCUF(const UF: String): Integer;
var
  i: Integer;
begin
  Result := -1 ;
  for i:= Low(DFeUF) to High(DFeUF) do
  begin
    if DFeUF[I] = UF then
    begin
      Result := DFeUFCodigo[I];
      exit;
    end;
  end;
end;

begin
  {
    A Chave é composta por:
     2 | N |Código IBGE para UF do prestador
    14 | N |CNPJ do prestador
     2 | N |Modelo da nota (valor 98 por padrão)
     3 | C |Série da nota (em maiúsculas, com zeros à direita)
     9 | N |Número da nota (com zeros à esquerda)
     9 | N |Código numérico aleatório
  }
  cUF := IntToStr(UFtoCUF(NFSe.Prestador.Endereco.UF));
  CNPJ := Poem_Zeros(OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj), 14);
  Modelo := NFSe.ModeloNFSe;
  aSerie := Poem_Zeros(UpperCase(NFSE.SeriePrestacao), 3);
  Numero := Poem_Zeros(NFSe.Numero, 9);
  Codigo := Poem_Zeros(NFSe.cNFSe, 9);

  sChave := cUF + CNPJ + Modelo + aSerie + Numero + Codigo;

  NFSe.refNF := sChave;

  Result := CreateElement('Id');

  Result.AppendChild(AddNode(tcInt, '#1', 'cNFS-e', 1, 9, 1, NFSe.cNFSe, ''));

  if FPVersao = ve100 then
    Result.AppendChild(AddNode(tcStr, '#1', 'natOp', 1, 50, 1,
                             NaturezaOperacaoToStr(NFSe.NaturezaOperacao), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'mod', 1, 2, 1, NFSe.ModeloNFSe, ''));

  // tem que ser S - ver como está sendo passado a variável "serie"
  Result.AppendChild(AddNode(tcStr, '#1', 'serie', 1, 2, 1,
                                                      NFSE.SeriePrestacao, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'nNFS-e', 1, 9, 1, NFSe.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'dEmi', 1, 10, 1,
                            FormatDateTime('yyyy-mm-dd',NFSe.DataEmissao), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'hEmi', 1, 10, 1,
                                FormatDateTime('hh:mm', NFSe.DataEmissao), ''));

  // 0- Entrada 1- Saída
  Result.AppendChild(AddNode(tcStr, '#1', 'tpNF', 1, 1, 1, '1', ''));

  if FPVersao = ve100 then
    Result.AppendChild(AddNode(tcStr, '#1', 'cMunFG', 1, 7, 1,
                                             NFSe.Servico.CodigoMunicipio, ''));

  // chave de acesso 39 caracteres
  Result.AppendChild(AddNode(tcStr, '#1', 'refNF', 1, 39, 1, NFSe.refNF, ''));

  // N- Normal C- Contigencia
  Result.AppendChild(AddNode(tcStr, '#1', 'tpEmis', 1, 1, 1,
                                       TipoEmissaoToStr(NFSe.TipoEmissao), ''));

  if FPVersao = ve100 then
    Result.AppendChild(AddNode(tcStr, '#1', 'anulada', 1, 1, 1, 'N', ''));

  if FPVersao = ve101 then
  begin
    if NFSe.StatusRps = srCancelado then
      Result.AppendChild(AddNode(tcStr, '#1', 'cancelada', 1, 1, 1, 'S', ''))
    else
      Result.AppendChild(AddNode(tcStr, '#1', 'cancelada', 1, 1, 1, 'N', ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'canhoto', 1, 1, 1,
                                               CanhotoToStr(NFSe.Canhoto), ''));

    if NFSe.Producao = snSim then
      Result.AppendChild(AddNode(tcStr, '#1', 'ambienteEmi', 1, 1, 1, '1', ''))
    else
      Result.AppendChild(AddNode(tcStr, '#1', 'ambienteEmi', 1, 1, 1, '2', ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'formaEmi', 1, 1, 1, '2', ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'empreitadaGlobal', 1, 1, 1,
                             EmpreitadaGlobalToStr(NFSe.EmpreitadaGlobal), ''));
  end;
end;

function TNFSeW_Infisc.GerarinfNFSe: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
  nodeArray: TACBrXmlNodeArray;
  i, lIndex, lLimiteLinha, lDeOndeIniciaCopia, lNumeroCaracteres: Integer;
  lTexto: String;
  lResultadoDivisao, lResultadoSobra: Word;
begin
  Result := CreateElement('infNFSe');

  Result.SetAttribute('versao', FpAOwner.ConfigWebServices.VersaoDados);

  xmlNode := GerarID;
  Result.AppendChild(xmlNode);

  xmlNode := GerarEmitente;
  Result.AppendChild(xmlNode);

  xmlNode := GerarTomador;
  Result.AppendChild(xmlNode);

  if EmpreitadaGlobalToStr(NFSe.EmpreitadaGlobal) = '1' then
  begin
    xmlNode := GerarDadosdaObra;
    Result.AppendChild(xmlNode);
  end;

  if NFSe.Transportadora.xCpfCnpjTrans <> '' then
  begin
    xmlNode := GerarTransportadora;
    Result.AppendChild(xmlNode);
  end;

  nodeArray := GerarDetalhamento;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;

  {
    Usado para inclusão de serviços que não são tributaveis e
    agregam valor no total da nota
  }
  if (NFSe.Despesa.Count > 0) and (FPVersao = ve101) then
  begin
    xmlNode := GerarDespesa;

    Result.AppendChild(xmlNode);
  end;

  xmlNode := GerarTotal;
  Result.AppendChild(xmlNode);

  if NFSe.CondicaoPagamento.Parcelas.Count > 0 then
  begin
    if FPVersao = ve100 then
      xmlNode := GerarCobrancav100
    else
      xmlNode := GerarCobrancav110;

    Result.AppendChild(xmlNode);
  end;

  if (Trim(NFSe.OutrasInformacoes) <> '') and (FPVersao = ve100) then
  begin
    xmlNode := GerarObservacoes;
    Result.AppendChild(xmlNode);
  end;

  if FPVersao = ve101 then
  begin
    {
      Quando é tributado fora do municipio temos que ter o IBGE o mesmo do
      localdeTributação do Serviço
    }
    Result.AppendChild(AddNode(tcStr, '#1', 'infAdicLT', 1, 100, 1,
                                         NFSe.Servico.MunicipioIncidencia, ''));

    lIndex := 0;
    lLimiteLinha := 250;
    lDeOndeIniciaCopia := 1;
    lTexto := NFSe.OutrasInformacoes;
    lNumeroCaracteres := Length(lTexto);

    lResultadoDivisao := lNumeroCaracteres div lLimiteLinha;
    lResultadoSobra := lNumeroCaracteres mod lLimiteLinha;

    if (lResultadoDivisao > 0) then
    begin
      repeat
        lDeOndeIniciaCopia := lIndex * lLimiteLinha;

        Result.AppendChild(AddNode(tcStr, '#1', 'infAdic', 1, 100, 1,
                           Copy(lTexto, lDeOndeIniciaCopia, lLimiteLinha), ''));

        inc(lIndex);
      until lIndex > (lResultadoDivisao -1);

      lDeOndeIniciaCopia := (lIndex * lLimiteLinha);
    end;

    if (lResultadoSobra > 0) and (lDeOndeIniciaCopia > 0) then
      Result.AppendChild(AddNode(tcStr, '#1', 'infAdic', 1, 100, 1,
                        Copy(lTexto, lIndex * lLimiteLinha, lLimiteLinha), ''));
  end;
end;

function TNFSeW_Infisc.GerarISS: TACBrXmlNode;
begin
  Result := CreateElement('ISS');

  if NFSe.Servico.Valores.IssRetido = stNormal then
  begin  // 2 - stNormal
    Result.AppendChild(AddNode(tcDe2, '#1', 'vBCISS', 1, 15, 0,
                                                              FPdTotBCISS, ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'vISS', 1, 15, 0, FPdTotISS, ''));
  end;

  if NFSe.Servico.Valores.IssRetido = stRetencao then
  begin  // 1 - stRetencao
    Result.AppendChild(AddNode(tcDe2, '#1', 'vBCSTISS', 1, 15, 0,
                                       NFSe.Servico.Valores.ValorServicos, ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'vSTISS', 1, 15, 0,
                                      NFSe.Servico.Valores.ValorIssRetido, ''));
  end;
end;

function TNFSeW_Infisc.GerarISSST(Item: Integer): TACBrXmlNode;
begin
  Result := CreateElement('ISSST');

  if FPVersao = ve101 then
  begin
    Result.AppendChild(AddNode(tcDe2, '#1', 'vRedBCST', 1, 15, 1, 0, ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'vBCST', 1, 15, 0,
                         NFSe.Servico.ItemServico[Item].BaseCalculo, ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'pISSST', 1, 5, 0,
                        NFSe.Servico.ItemServico[Item].AliqISSST, ''));
  end;

  Result.AppendChild(AddNode(tcDe2, '#1', 'vISSST', 1, 15, 0,
                          NFSe.Servico.ItemServico[Item].ValorISSST, ''));
end;

function TNFSeW_Infisc.GerarObservacoes: TACBrXmlNode;
begin
  Result := CreateElement('Observacoes');

  Result.AppendChild(AddNode(tcStr, '#1', 'xinf', 1, 100, 1,
                                     Copy(NFSe.OutrasInformacoes, 1, 100), ''));
end;

function TNFSeW_Infisc.GerarRetencao: TACBrXmlNode;
begin
  Result := CreateElement('Ret');

  if NFSe.Servico.Valores.ValorIr > 0 then
  begin
    if FPVersao = ve100 then
      Result.AppendChild(AddNode(tcStr, '#1', 'xRetIRF', 1, 100, 1,
                                                          'Retenção IRRF', ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'vRetIR', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));
  end;

  if NFSe.Servico.Valores.ValorPis > 0 then
  begin
    if FPVersao = ve100 then
      Result.AppendChild(AddNode(tcStr, '#1', 'xRetLei10833-PIS-PASEP', 1, 100, 1,
                                                           'Retenção PIS', ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'vRetPISPASEP', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));
  end;

  if NFSe.Servico.Valores.ValorCofins > 0 then
  begin
    if FPVersao = ve100 then
      Result.AppendChild(AddNode(tcStr, '#1', 'xRetLei10833-COFINS', 1, 100, 1,
                                                        'Retenção COFINS', ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'vRetCOFINS', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));
  end;

  if NFSe.Servico.Valores.ValorCsll > 0 then
  begin
    if FPVersao = ve100 then
      Result.AppendChild(AddNode(tcStr, '#1', 'xRetLei10833-CSLL', 1, 100, 1,
                                                          'Retenção CSLL', ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'vRetCSLL', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));
  end;

  if NFSe.Servico.Valores.ValorInss > 0 then
  begin
    if FPVersao = ve100 then
      Result.AppendChild(AddNode(tcStr, '#1', 'xRetINSS', 1, 100, 1,
                                                          'Retenção INSS', ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'vRetINSS', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));
  end;
end;

function TNFSeW_Infisc.GerarServico(Item: Integer): TACBrXmlNode;
var
  GeraTag: Integer;
  xServ: String;
begin
  Result := CreateElement('serv');

  xServ := NFSe.Servico.ItemServico[Item].Descricao;

  if NFSe.Servico.ItemServico[Item].ItemListaServico <> '' then
    xServ := xServ + ' (Class.: ' + NFSe.Servico.ItemServico[Item].ItemListaServico + ')';

  // CodServ = Código do Municipio
  Result.AppendChild(AddNode(tcStr, '#1', 'cServ', 1, 2, 1,
                             NFSe.Servico.ItemServico[Item].codServ, ''));

  if FPVersao = ve101 then
    Result.AppendChild(AddNode(tcStr, '#1', 'cLCServ', 1, 2, 1,
                           NFSe.Servico.ItemServico[Item].codLCServ, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xServ', 1, 12, 1, xServ, ''));

  if FPVersao = ve101 then
  begin
    Result.AppendChild(AddNode(tcStr, '#1', 'localTributacao', 1, 4, 1,
                               IntToStr(NFSe.Servico.MunicipioIncidencia), ''));

    // Local da verificacao: 1 - Brasil 2 - Exterior
    Result.AppendChild(AddNode(tcStr, '#1', 'localVerifResServ', 1, 4, 1, '1', ''));
  end;

  Result.AppendChild(AddNode(tcStr, '#1', 'uTrib', 1, 6, 1,
                             NFSe.Servico.ItemServico[Item].Unidade, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'qTrib', 1, 15, 1,
                          NFSe.Servico.ItemServico[Item].Quantidade, ''));

  if FPVersao = ve100 then
    Result.AppendChild(AddNode(tcDe3, '#1', 'vUnit', 1, 15, 1,
                       NFSe.Servico.ItemServico[Item].ValorUnitario, ''))
  else
    Result.AppendChild(AddNode(tcDe4, '#1', 'vUnit', 1, 15, 1,
                       NFSe.Servico.ItemServico[Item].ValorUnitario, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vServ', 1, 15, 0,
                          NFSe.Servico.ItemServico[Item].ValorTotal, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vDesc', 1, 15, 0,
              NFSe.Servico.ItemServico[Item].DescontoIncondicionado, ''));

  if FPVersao = ve101 then
    Result.AppendChild(AddNode(tcDe2, '#1', 'vDed', 1, 15, 0,
                       NFSe.Servico.ItemServico[Item].ValorDeducoes, ''));

  if (Nfse.RegimeEspecialTributacao = retSimplesNacional) or
     (Nfse.OptanteSimplesNacional = snSim) then
    GeraTag := 1
  else
    GeraTag := 0;

  if NFSe.Servico.ItemServico[Item].ValorISSST = 0 then
    Result.AppendChild(AddNode(tcDe2, '#1', 'vBCISS', 1, 15, GeraTag,
                         NFSe.Servico.ItemServico[Item].BaseCalculo, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'pISS', 1, 15, GeraTag,
                            NFSe.Servico.ItemServico[Item].Aliquota, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vISS', 1, 15, GeraTag,
                            NFSe.Servico.ItemServico[Item].ValorISS, ''));

  FPdTotBCISS := FPdTotBCISS + NFSe.Servico.ItemServico[Item].BaseCalculo;
  FPdTotISS   := FPdTotISS   + NFSe.Servico.ItemServico[Item].ValorISS;

  Result.AppendChild(AddNode(tcDe2, '#1', 'pRed', 1, 15, 0,
                         NFSe.Servico.ItemServico[Item].AliqReducao, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vRed', 1, 15, 0,
                        NFSe.Servico.ItemServico[Item].ValorReducao, ''));

  // Retenção INSS
  if NFSe.Servico.ItemServico[item].ValorINSS > 0 then
  begin
    if FPVersao = ve101 then
    begin
      Result.AppendChild(AddNode(tcDe2, '#1', 'vBCINSS', 1, 15, 1,
                         NFSe.Servico.ItemServico[Item].ValorBCINSS, ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'pRetINSS', 1, 15, 1,
                         NFSe.Servico.ItemServico[Item].AliqRetINSS, ''));
    end;

    Result.AppendChild(AddNode(tcDe2, '#1', 'vRetINSS', 1, 15, 1,
                           NFSe.Servico.ItemServico[Item].ValorINSS, ''));
  end;

  // Retenção IRRF
  if NFSe.Servico.ItemServico[Item].ValorIRRF > 0 then
  begin
    if FPVersao = ve100 then
    begin
      Result.AppendChild(AddNode(tcStr, '#1', 'xRetIRF', 1, 100, 1,
                                                          'Retenção IRRF', ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'vRetIRF', 1, 15, 1,
                           NFSe.Servico.ItemServico[Item].ValorIRRF, ''));
    end
    else
    begin
      Result.AppendChild(AddNode(tcDe2, '#1', 'vBCRetIR', 1, 15, 1,
                      NFSe.Servico.ItemServico[Item].ValorBCRetIRRF, ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'pRetIR', 1, 15, 1,
                         NFSe.Servico.ItemServico[Item].AliqRetIRRF, ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'vRetIR', 1, 15, 1,
                           NFSe.Servico.ItemServico[Item].ValorIRRF, ''));
    end;
  end;

  if FPVersao = ve101 then
  begin
    // Retenção COFINS
    if NFSe.Servico.ItemServico[Item].ValorCOFINS > 0 then
    begin
      Result.AppendChild(AddNode(tcDe2, '#1', 'vBCCOFINS', 1, 15, 1,
                       NFSe.Servico.ItemServico[Item].ValorBCCOFINS, ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'pRetCOFINS', 1, 15, 1,
                       NFSe.Servico.ItemServico[Item].AliqRetCOFINS, ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'vRetCOFINS', 1, 15, 1,
                         NFSe.Servico.ItemServico[Item].ValorCOFINS, ''));
    end;

    // Retenção CSLL
    if NFSe.Servico.ItemServico[Item].ValorCSLL > 0 then
    begin
      Result.AppendChild(AddNode(tcDe2, '#1', 'vBCCSLL', 1, 15, 1,
                         NFSe.Servico.ItemServico[Item].ValorBCCSLL, ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'pRetCSLL', 1, 15, 1,
                         NFSe.Servico.ItemServico[Item].AliqRetCSLL, ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'vRetCSLL', 1, 15, 1,
                           NFSe.Servico.ItemServico[Item].ValorCSLL, ''));
    end;
  end;

  // Retenção PIS
  if NFSe.Servico.ItemServico[Item].ValorPIS > 0 then
  begin
    if FPVersao = ve100 then
    begin
      Result.AppendChild(AddNode(tcStr, '#1', 'xRetLei10833-PIS-PASEP', 1, 100, 1,
                                                           'Retenção PIS', ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'vRetLei10833-PIS-PASEP', 1, 15, 1,
                            NFSe.Servico.ItemServico[Item].ValorPIS, ''));
    end
    else
    begin
      Result.AppendChild(AddNode(tcDe2, '#1', 'vBCPISPASEP', 1, 15, 1,
                          NFSe.Servico.ItemServico[Item].ValorBCPIS, ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'pRetPISPASEP', 1, 15, 1,
                          NFSe.Servico.ItemServico[Item].AliqRetPIS, ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'vRetPISPASEP', 1, 15, 1,
                            NFSe.Servico.ItemServico[Item].ValorPIS, ''));
    end;
  end;

  if FPVersao = ve100 then
  begin
    // Retenção COFINS
    if NFSe.Servico.ItemServico[Item].ValorCOFINS > 0 then
    begin
      Result.AppendChild(AddNode(tcStr, '#1', 'xRetLei10833-COFINS', 1, 100, 1,
                                                        'Retenção COFINS', ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'vRetLei10833-COFINS', 1, 15, 1,
                         NFSe.Servico.ItemServico[Item].ValorCOFINS, ''));
    end;

    // Retenção CSLL
    if NFSe.Servico.ItemServico[Item].ValorCSLL > 0 then
    begin
      Result.AppendChild(AddNode(tcStr, '#1', 'xRetLei10833-CSLL', 1, 100, 1,
                                                          'Retenção CSLL', ''));

      Result.AppendChild(AddNode(tcDe2, '#1', 'vRetLei10833-CSLL', 1, 15, 1,
                           NFSe.Servico.ItemServico[Item].ValorCSLL, ''));
    end;
  end;
end;

function TNFSeW_Infisc.GerarTomador: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
  xCidade, xUF: string;
begin
  Result := CreateElement('TomS');

  if Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj) = 11 then
    Result.AppendChild(AddNode(tcStr, '#1', 'CPF', 1, 11, 1,
                                 NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''))
  else
    Result.AppendChild(AddNode(tcStr, '#1', 'CNPJ', 1, 14, 1,
                                NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xNome', 1, 100, 0,
                                                 NFSe.Tomador.RazaoSocial, ''));

  if NFSe.Tomador.Endereco.Endereco <> '' then
  begin
    xmlNode := GerarEnderecoTomador;
    Result.AppendChild(xmlNode);
  end;

  Result.AppendChild(AddNode(tcStr, '#1', 'xEmail', 1, 100, 0,
                                               NFSe.Tomador.Contato.Email, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'IE', 1, 15, 0,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'IM', 1, 15, 0,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'fone', 1, 100, 0,
                                            NFSe.Tomador.Contato.Telefone, ''));

  if (FPVersao = ve100) and (NFSe.Servico.MunicipioIncidencia <> 0) then
  begin
    xUF := '';
    xCidade := ObterNomeMunicipioUF(NFSe.Servico.MunicipioIncidencia, xUF);

    Result.AppendChild(AddNode(tcStr, '#1', 'Praca', 1, 60, 1,
                                                      xCidade + '-' + xUF, ''));
  end;
end;

function TNFSeW_Infisc.GerarTotal: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('total');

  if FPVersao = ve100 then
    Result.AppendChild(AddNode(tcDe2, '#1', 'vReemb', 1, 15, 1, 0, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vServ', 1, 15, 0,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vDesc', 1, 15, 0,
                              NFSe.Servico.Valores.DescontoIncondicionado, ''));

  if FPVersao = ve100 then
    Result.AppendChild(AddNode(tcDe2, '#1', 'vOutro', 1, 15, 0,
                                     NFSe.Servico.Valores.OutrosDescontos, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vtNF', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vtLiq', 1, 15, 1,
                                    NFSe.Servico.Valores.ValorLiquidoNfse, ''));

  if FPVersao = ve100 then
    Result.AppendChild(AddNode(tcDe2, '#1', 'totalAproxTrib', 1, 15, 0,
                                                                        0, ''));

  if (NFSe.Servico.Valores.ValorIr + NFSe.Servico.Valores.ValorPis +
      NFSe.Servico.Valores.ValorCofins + NFSe.Servico.Valores.ValorCsll +
      NFSe.Servico.Valores.ValorInss) > 0 then
  begin
    xmlNode := GerarRetencao;
    Result.AppendChild(xmlNode);
  end;

  // incluir tag 'fat' aqui!

  if FPVersao = ve101 then
    Result.AppendChild(AddNode(tcDe2, '#1', 'vtLiqFaturas', 1, 15, 0,
                                    NFSe.Servico.Valores.ValorLiquidoNfse, ''));

  // Total Retenção ISSQN
  xmlNode := GerarISS;
  Result.AppendChild(xmlNode);
end;

function TNFSeW_Infisc.GerarTransportadora: TACBrXmlNode;
begin
  Result := CreateElement('transportadora');

  Result.AppendChild(AddNode(tcStr, '#1', 'xNomeTrans', 1, 100, 1,
                                           NFSe.Transportadora.xNomeTrans, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xCpfCnpjTrans', 1, 14, 1,
                                        NFSe.Transportadora.xCpfCnpjTrans, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xInscEstTrans', 1, 15, 1,
                                        NFSe.Transportadora.xInscEstTrans, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xPlacaTrans', 1, 7, 1,
                                          NFSe.Transportadora.xPlacaTrans, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xEndTrans', 1, 100, 1,
                                            NFSe.Transportadora.xEndTrans, ''));

  Result.AppendChild(AddNode(tcInt, '#1', 'cMunTrans', 1, 7, 1,
                                            NFSe.Transportadora.cMunTrans, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xMunTrans', 1, 60, 1,
                                            NFSe.Transportadora.xMunTrans, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xUfTrans', 1, 2, 1,
                                             NFSe.Transportadora.xUFTrans, ''));

  Result.AppendChild(AddNode(tcInt, '#1', 'cPaisTrans', 1, 4, 1,
                                           NFSe.Transportadora.cPaisTrans, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'xPaisTrans', 1, 100, 1,
                                           NFSe.Transportadora.xPaisTrans, ''));

  Result.AppendChild(AddNode(tcInt, '#1', 'vTipoFreteTrans', 1, 1, 1,
                      TipoFreteToStr(NFSe.Transportadora.vTipoFreteTrans), ''));
end;

{ TNFSeW_Infisc101 }

procedure TNFSeW_Infisc101.Configuracao;
begin
  inherited Configuracao;

  FPVersao := ve101;
end;

{ TNFSeW_Infisc201 }

procedure TNFSeW_Infisc201.Configuracao;
begin
  inherited Configuracao;

  NrOcorrCodigoPaisTomador := 1;

  GerarNSRps := False;
end;

procedure TNFSeW_Infisc201.DefinirIDRps;
begin
  NFSe.InfID.ID := 'rps' + NFSe.IdentificacaoRps.Numero +
                   NFSe.IdentificacaoRps.Serie;
end;

{ TNFSeW_Infisc203 }

procedure TNFSeW_Infisc203.Configuracao;
begin
  inherited Configuracao;

  GerarNSRps := True;
end;

end.
