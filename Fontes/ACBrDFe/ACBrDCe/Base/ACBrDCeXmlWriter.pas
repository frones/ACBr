{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrDCeXmlWriter;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrXmlWriter,
  ACBrDCeClass;

type

  TDCeXmlWriterOptions = class(TACBrXmlWriterOptions)
  private
    FAjustarTagNro: boolean;
    FGerarTagIPIparaNaoTributado: boolean;
    FNormatizarMunicipios: boolean;
    FGerarTagAssinatura: TACBrTagAssinatura;
    FPathArquivoMunicipios: string;
    FValidarInscricoes: boolean;
    FValidarListaServicos: boolean;

  published
    property AjustarTagNro: boolean read FAjustarTagNro write FAjustarTagNro;
    property GerarTagIPIparaNaoTributado: boolean read FGerarTagIPIparaNaoTributado write FGerarTagIPIparaNaoTributado;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios write FNormatizarMunicipios;
    property GerarTagAssinatura: TACBrTagAssinatura read FGerarTagAssinatura write FGerarTagAssinatura;
    property PathArquivoMunicipios: string read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: boolean read FValidarInscricoes write FValidarInscricoes;
    property ValidarListaServicos: boolean read FValidarListaServicos write FValidarListaServicos;

  end;

  TDCeXmlWriter = class(TACBrXmlWriter)
  private
    FDCe: TDCe;

    Versao: string;
    ChaveDCe: string;

    function GerarInfDCe: TACBrXmlNode;
    function GerarIde: TACBrXmlNode;
    function GerarEmit: TACBrXmlNode;
    function GerarEnderEmit: TACBrXmlNode;
    function GerarFisco: TACBrXmlNode;
    function GerarMarketplace: TACBrXmlNode;
    function GerarTransportadora: TACBrXmlNode;
    function GerarEmpEmisProp: TACBrXmlNode;
    function GerarDest: TACBrXmlNode;
    function GerarEnderDest: TACBrXmlNode;
    function GerarautXML: TACBrXmlNodeArray;
    function GerarDet: TACBrXmlNodeArray;
    function GerarDetProd(const i: Integer): TACBrXmlNode;
    function GerarTotal: TACBrXmlNode;
    function GerarTransp: TACBrXmlNode;
    function GerarInfAdic: TACBrXmlNode;
    function GerarInfAdicObsCont: TACBrXmlNodeArray;
    function GerarInfAdicObsMarketplace: TACBrXmlNodeArray;
    function GerarInfDec: TACBrXmlNode;
    {
    function GerarProtDCe: TACBrXmlNode;
    }
    function GetOpcoes: TDCeXmlWriterOptions;
    procedure SetOpcoes(AValue: TDCeXmlWriterOptions);

    procedure AjustarMunicipioUF(out xUF: string; out xMun: string;
      out cMun: integer; cPais: integer; const vxUF, vxMun: string; vcMun: integer);

    function GerarChaveAcesso(AUF: Integer; ADataEmissao: TDateTime;
      const ACNPJ:String; ASerie, ANumero, AtpEmi, ATipoEmit, AnSiteAut,
      ACodigo: Integer; AModelo: Integer = 99): String;
  protected
    function CreateOptions: TACBrXmlWriterOptions; override;

  public
    constructor Create(AOwner: TDCe); reintroduce;
    destructor Destroy; override;

    function GerarXml: boolean; override;
    function ObterNomeArquivo: string; override;

    property Opcoes: TDCeXmlWriterOptions read GetOpcoes write SetOpcoes;
    property DCe: TDCe read FDCe write FDCe;
  end;

implementation

uses
  pcnAuxiliar,
  ACBrValidador,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrDFeUtil,
  ACBrDFeConsts,
  ACBrDCe,
  ACBrDCeConversao,
  ACBrDCeConsts;

{ TDCeXmlWriter }

constructor TDCeXmlWriter.Create(AOwner: TDCe);
begin
  inherited Create;

  Opcoes.AjustarTagNro := True;
  Opcoes.GerarTagIPIparaNaoTributado := True;
  Opcoes.NormatizarMunicipios := False;
  Opcoes.PathArquivoMunicipios := '';
  Opcoes.GerarTagAssinatura := taSomenteSeAssinada;
  Opcoes.ValidarInscricoes := False;
  Opcoes.ValidarListaServicos := False;

  FDCe := AOwner;
end;

function TDCeXmlWriter.CreateOptions: TACBrXmlWriterOptions;
begin
  Result := TDCeXmlWriterOptions.Create();
end;

destructor TDCeXmlWriter.Destroy;
begin
  inherited Destroy;
end;

function TDCeXmlWriter.GetOpcoes: TDCeXmlWriterOptions;
begin
  Result := TDCeXmlWriterOptions(FOpcoes);
end;

function TDCeXmlWriter.ObterNomeArquivo: string;
begin
  Result := OnlyNumber(FDCe.infDCe.ID) + '-dce.xml';
end;

procedure TDCeXmlWriter.SetOpcoes(AValue: TDCeXmlWriterOptions);
begin
  FOpcoes := AValue;
end;

procedure TDCeXmlWriter.AjustarMunicipioUF(out xUF: string; out xMun: string;
  out cMun: integer; cPais: integer; const vxUF, vxMun: string; vcMun: integer);
var
  PaisBrasil: boolean;
begin
  PaisBrasil := cPais = CODIGO_BRASIL;

  cMun := IIf(PaisBrasil, vcMun, CMUN_EXTERIOR);
  xMun := IIf(PaisBrasil, vxMun, XMUN_EXTERIOR);
  xUF  := IIf(PaisBrasil, vxUF, UF_EXTERIOR);

  if Opcoes.NormatizarMunicipios then
    if ((EstaZerado(cMun)) and (xMun <> XMUN_EXTERIOR)) then
      cMun := ObterCodigoMunicipio(xMun, xUF, Opcoes.FPathArquivoMunicipios)
    else if ( ( EstaVazio(xMun)) and (cMun <> CMUN_EXTERIOR) ) then
      xMun := ObterNomeMunicipio(cMun, xUF, Opcoes.FPathArquivoMunicipios);
end;

function TDCeXmlWriter.GerarChaveAcesso(AUF: Integer; ADataEmissao: TDateTime;
  const ACNPJ: String; ASerie, ANumero, AtpEmi, ATipoEmit, AnSiteAut, ACodigo,
  AModelo: Integer): String;
var
  vUF, vDataEmissao, vSerie, vNumero, vCodigo, vModelo, vCNPJ, vtpEmi,
  vTipoEmit, vnSiteAut: String;
begin
  // Se o usuario informar 0 ou -1; o código numerico sera gerado de maneira aleatória //
  if ACodigo = -1 then
    ACodigo := 0;

  if ACodigo = 0 then
    ACodigo := GerarCodigoDFe(ANumero);

  // Se o usuario informar um código inferior ou igual a -2 a chave será gerada
  // com o código igual a zero, mas poderá não ser autorizada pela SEFAZ.
  if ACodigo <= -2 then
    ACodigo := 0;

  vUF          := Poem_Zeros(AUF, 2);
  vDataEmissao := FormatDateTime('YYMM', ADataEmissao);
  vCNPJ        := PadLeft(OnlyNumber(ACNPJ), 14, '0');
  vModelo      := Poem_Zeros(AModelo, 2);
  vSerie       := Poem_Zeros(ASerie, 3);
  vNumero      := Poem_Zeros(ANumero, 9);
  vtpEmi       := Poem_Zeros(AtpEmi, 1);
  vTipoEmit    := Poem_Zeros(ATipoEmit, 1);
  vnSiteAut    := Poem_Zeros(AnSiteAut, 1);
  vCodigo      := Poem_Zeros(ACodigo, 6);

  Result := vUF + vDataEmissao + vCNPJ + vModelo + vSerie + vNumero + vtpEmi +
            vTipoEmit + vnSiteAut + vCodigo;
  Result := Result + Modulo11(Result);
end;

function TDCeXmlWriter.GerarInfDCe: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := FDocument.CreateElement('infDCe');

  Result.SetAttribute('Id', DCe.infDCe.ID);
  Result.SetAttribute('versao', FloatToString(DCe.infDCe.Versao, '.', '#0.00'));

  Result.AppendChild(GerarIde);
  Result.AppendChild(GerarEmit);
  Result.AppendChild(GerarFisco);
  Result.AppendChild(GerarMarketplace);
  Result.AppendChild(GerarTransportadora);
  Result.AppendChild(GerarEmpEmisProp);
  Result.AppendChild(GerarDest);

  nodeArray := GerarautXML;

  for i := 0 to DCe.autXML.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  nodeArray := GerarDet;

  for i := 0 to DCe.Det.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(GerarTotal);
  Result.AppendChild(GerarTransp);
  Result.AppendChild(GerarInfAdic);
  Result.AppendChild(GerarInfDec);
end;

function TDCeXmlWriter.GerarIde: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ide');

  Result.AppendChild(AddNode(tcInt, 'B02', 'cUF', 2, 2, 1, DCe.ide.cUF, DSC_CUF));

  if not ValidarCodigoUF(DCe.ide.cUF) then
    wAlerta('B02', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, 'B03', 'cDC', 6, 6, 1,
            IntToStrZero(ExtrairCodigoChaveAcesso(DCe.infDCe.ID), 8), DSC_CDF));

  Result.AppendChild(AddNode(tcInt, 'B04', 'mod', 2, 2, 1, DCe.Ide.modelo, ''));

  Result.AppendChild(AddNode(tcInt, 'B05', 'serie', 1, 3, 1,
                                                     DCe.ide.serie, DSC_SERIE));

  Result.AppendChild(AddNode(tcInt, 'B06', 'nDC', 1, 9, 1, DCe.ide.nDC, DSC_NDF));

  Result.AppendChild(AddNode(tcStr, 'B07', 'dhEmi', 25, 25, 1,
      DateTimeTodh(DCe.ide.dhEmi) +
      GetUTC(CodigoParaUF(DCe.ide.cUF), DCe.ide.dhEmi), DSC_DEMI));

  Result.AppendChild(AddNode(tcStr, 'B08', 'tpEmis', 1, 1, 1,
                                 TipoEmissaoToStr(DCe.Ide.tpEmis), DSC_TPEMIS));

  Result.AppendChild(AddNode(tcStr, 'B09', 'tpEmit', 1, 1, 1,
                                 EmitenteDCeToStr(DCe.Ide.tpEmit), DSC_TPEMIS));

  Result.AppendChild(AddNode(tcInt, 'B10', 'nSiteAutoriz', 1, 1, 1,
                                                DCe.ide.nSiteAutoriz, DSC_NDF));

  Result.AppendChild(AddNode(tcInt, 'B11', 'cDV', 1, 1, 1, DCe.Ide.cDV, DSC_CDV));

  Result.AppendChild(AddNode(tcStr, 'B12', 'tpAmb', 1, 1, 1,
                                  TipoAmbienteToStr(DCe.Ide.tpAmb), DSC_TPAMB));

  Result.AppendChild(AddNode(tcStr, 'B13', 'verProc', 1, 20, 1,
                                                 DCe.Ide.verProc, DSC_VERPROC));
end;

function TDCeXmlWriter.GerarEmit: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('emit');

  if DCe.emit.idOutros = '' then
    Result.AppendChild(AddNodeCNPJCPF('C02', 'C02a', DCe.Emit.CNPJCPF))
  else
    Result.AppendChild(AddNode(tcStr, 'C02b', 'idOutros', 2, 60, 1,
                                                 DCe.Emit.idOutros, DSC_XNOME));

  Result.AppendChild(AddNode(tcStr, 'C03', 'xNome', 2, 60, 1,
                                                    DCe.Emit.xNome, DSC_XNOME));

  Result.AppendChild(GerarEnderEmit);
end;

function TDCeXmlWriter.GerarEnderEmit: TACBrXmlNode;
var
  cMun: integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, DCe.Emit.enderEmit.UF,
    DCe.Emit.enderEmit.xMun, DCe.Emit.EnderEmit.cMun);

  Result := FDocument.CreateElement('enderEmit');

  Result.AppendChild(AddNode(tcStr, 'C05', 'xLgr', 2, 60, 1,
                                            DCe.Emit.enderEmit.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, 'C06', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, DCe.Emit.enderEmit.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, 'C07', 'xCpl', 1, 60, 0,
                                            DCe.Emit.enderEmit.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, 'C08', 'xBairro', 2, 60, 1,
                                      DCe.Emit.enderEmit.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, 'C09', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('C09', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, 'C10', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcStr, 'C11', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not pcnAuxiliar.ValidarUF(xUF) then
    wAlerta('C11', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcInt, 'C12', 'CEP', 8, 8, 1,
                                              DCe.Emit.enderEmit.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcInt, 'C13', 'cPais', 4, 4, 0,
                                                     CODIGO_BRASIL, DSC_CPAIS));

  Result.AppendChild(AddNode(tcStr, 'C14', 'xPais', 1, 60, 1,
                                          DCe.Emit.enderEmit.xPais, DSC_XPAIS));

  Result.AppendChild(AddNode(tcStr, 'C15', 'fone', 6, 14, 0,
                                OnlyNumber(DCe.Emit.enderEmit.fone), DSC_FONE));
end;

function TDCeXmlWriter.GerarFisco: TACBrXmlNode;
begin
  Result := nil;

  if DCe.Fisco.CNPJ <> '' then
  begin
    Result := FDocument.CreateElement('Fisco');

    Result.AppendChild(AddNode(tcStr, 'D02', 'CNPJ', 14, 14, 1,
                                                     DCe.Fisco.CNPJ, DSC_CNPJ));

    Result.AppendChild(AddNode(tcStr, 'D03', 'xOrgao', 1, 60, 1,
                                                  DCe.Fisco.xOrgao, DSC_XNOME));

    Result.AppendChild(AddNode(tcStr, 'D07', 'UF', 2, 2, 1,
                                                         DCe.Fisco.UF, DSC_UF));
  end;
end;

function TDCeXmlWriter.GerarMarketplace: TACBrXmlNode;
begin
  Result := nil;

  if DCe.Marketplace.CNPJ <> '' then
  begin
    Result := FDocument.CreateElement('Marketplace');

    Result.AppendChild(AddNode(tcStr, 'D09', 'CNPJ', 14, 14, 1,
                                               DCe.Marketplace.CNPJ, DSC_CNPJ));

    Result.AppendChild(AddNode(tcStr, 'D10', 'xNome', 1, 60, 1,
                                             DCe.Marketplace.xNome, DSC_XNOME));

    Result.AppendChild(AddNode(tcStr, 'D11', 'Site', 1, 120, 1,
                                                 DCe.Marketplace.Site, DSC_UF));
  end;
end;

function TDCeXmlWriter.GerarTransportadora: TACBrXmlNode;
begin
  Result := nil;

  if DCe.Transportadora.CNPJ <> '' then
  begin
    Result := FDocument.CreateElement('Transportadora');

    Result.AppendChild(AddNode(tcStr, 'D13', 'CNPJ', 14, 14, 1,
                                               DCe.Transportadora.CNPJ, DSC_CNPJ));

    Result.AppendChild(AddNode(tcStr, 'D14', 'xNome', 1, 60, 1,
                                             DCe.Transportadora.xNome, DSC_XNOME));
  end;
end;

function TDCeXmlWriter.GerarEmpEmisProp: TACBrXmlNode;
begin
  Result := nil;

  if DCe.EmpEmisProp.CNPJ <> '' then
  begin
    Result := FDocument.CreateElement('EmpEmisProp');

    Result.AppendChild(AddNode(tcStr, 'D16', 'CNPJ', 14, 14, 1,
                                               DCe.EmpEmisProp.CNPJ, DSC_CNPJ));

    Result.AppendChild(AddNode(tcStr, 'D17', 'xNome', 1, 60, 1,
                                             DCe.EmpEmisProp.xNome, DSC_XNOME));
  end;
end;

function TDCeXmlWriter.GerarDest: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('dest');

  if DCe.dest.idOutros = '' then
    Result.AppendChild(AddNodeCNPJCPF('E02', 'E03', DCe.dest.CNPJCPF))
  else
    Result.AppendChild(AddNode(tcStr, 'E03a', 'idOutros', 2, 60, 1,
                                                 DCe.dest.idOutros, DSC_XNOME));

  Result.AppendChild(AddNode(tcStr, 'E04', 'xNome', 2, 60, 1,
                                                    DCe.dest.xNome, DSC_XNOME));

  Result.AppendChild(GerarEnderDest);
end;

function TDCeXmlWriter.GerarEnderDest: TACBrXmlNode;
var
  cMun: integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, DCe.dest.enderDest.UF,
    DCe.dest.enderDest.xMun, DCe.dest.enderDest.cMun);

  Result := FDocument.CreateElement('enderEmit');

  Result.AppendChild(AddNode(tcStr, 'E06', 'xLgr', 2, 60, 1,
                                            DCe.dest.enderDest.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, 'E07', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, DCe.dest.enderDest.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, 'E08', 'xCpl', 1, 60, 0,
                                            DCe.dest.enderDest.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, 'E09', 'xBairro', 2, 60, 1,
                                      DCe.dest.enderDest.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, 'E10', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('E10', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, 'E11', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcStr, 'E12', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not pcnAuxiliar.ValidarUF(xUF) then
    wAlerta('E12', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcInt, 'E13', 'CEP', 8, 8, 1,
                                              DCe.dest.enderDest.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcInt, 'E14', 'cPais', 4, 4, 0,
                                                     CODIGO_BRASIL, DSC_CPAIS));

  Result.AppendChild(AddNode(tcStr, 'E15', 'xPais', 1, 60, 1,
                                          DCe.dest.enderDest.xPais, DSC_XPAIS));

  Result.AppendChild(AddNode(tcStr, 'E16', 'fone', 6, 14, 0,
                                OnlyNumber(DCe.dest.enderDest.fone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, 'E19', 'email', 1, 60, 0,
                              OnlyNumber(DCe.dest.enderDest.email), DSC_EMAIL));
end;

function TDCeXmlWriter.GerarautXML: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, DCe.autXML.Count);

  for i := 0 to DCe.autXML.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('autXML');
    Result[i].AppendChild(AddNodeCNPJCPF('F02', 'F03', DCe.autXML[i].CNPJCPF));
  end;

  if DCe.autXML.Count > 10 then
    wAlerta('F01', 'autXML', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TDCeXmlWriter.GerarDet: TACBrXmlNodeArray;
var
  i: integer;
begin
  SetLength(Result, DCe.Det.Count);

  for i := 0 to DCe.Det.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('det');
    Result[i].SetAttribute('nItem', IntToStr(DCe.Det[i].Prod.nItem));

    Result[i].AppendChild(GerarDetProd(i));

    Result[i].AppendChild(AddNode(tcStr, 'V01', 'infAdProd', 01, 500, 0,
                                          DCe.Det[i].infAdProd, DSC_INFADPROD));
  end;

  if DCe.Det.Count > 999 then
    wAlerta('H02', 'nItem', DSC_NITEM, ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TDCeXmlWriter.GerarDetProd(const i: Integer): TACBrXmlNode;
const
  HOM_XPROD = 'DECLARACAO EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
begin
  Result := FDocument.CreateElement('prod');

  if (DCe.Det[i].Prod.nItem = 1) and (DCe.Ide.tpAmb = TACBrTipoAmbiente.taHomologacao) then
    Result.AppendChild(AddNode(tcStr, 'I02', 'xProd', 1, 120, 1,
                                                          HOM_XPROD, DSC_XPROD))
  else
    Result.AppendChild(AddNode(tcStr, 'I02', 'xProd', 1, 120, 1,
                                             DCe.Det[i].Prod.xProd, DSC_XPROD));

  Result.AppendChild(AddNode(tcStr, 'I03', 'NCM', 2, 8, 0,
                                                 DCe.Det[i].Prod.NCM, DSC_NCM));

  Result.AppendChild(AddNode(tcDe4, 'I04', 'qCom', 0, 15, 1,
                                               DCe.Det[i].Prod.qCom, DSC_QCOM));

  Result.AppendChild(AddNode(tcDe10, 'I05', 'vUnCom', 0, 21, 1,
                                           DCe.Det[i].Prod.vUnCom, DSC_VUNCOM));

  Result.AppendChild(AddNode(tcDe2, 'I06 ', 'vProd', 0, 15, 1,
                                             DCe.Det[i].Prod.vProd, DSC_VPROD));
end;

function TDCeXmlWriter.GerarTotal: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('total');

  Result.AppendChild(AddNode(tcDe2, 'W16', 'vDC', 1, 15, 1,
                                               DCe.Total.vDC, DSC_VDF));
end;

function TDCeXmlWriter.GerarTransp: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('transp');

  Result.AppendChild(AddNode(tcStr, 'X02', 'modTrans', 1, 1, 1,
                                       ModTransToStr(DCe.Transp.modTrans), ''));

  Result.AppendChild(AddNode(tcStr, 'X03','CNPJTransp', 14, 14, 0,
                                               DCe.Transp.CNPJTrans, DSC_CNPJ));
end;

function TDCeXmlWriter.GerarInfAdic: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  if (trim(DCe.InfAdic.infAdFisco) <> '') or
     (trim(DCe.InfAdic.infCpl) <> '') or
     (trim(DCe.InfAdic.infadMarketplace) <> '') or
     (trim(DCe.InfAdic.infadTransp) <> '') or
     (DCe.InfAdic.obsCont.Count > 0) or
     (DCe.InfAdic.obsMarketplace.Count > 0)  then
  begin
    Result := FDocument.CreateElement('infAdic');

    Result.AppendChild(AddNode(tcStr, 'Z02', 'infAdFisco', 01, 5000, 0,
                                       DCe.InfAdic.infAdFisco, DSC_INFADFISCO));

    Result.AppendChild(AddNode(tcStr, 'Z03', 'infCpl', 01, 5000, 0,
                                               DCe.InfAdic.infCpl, DSC_INFCPL));

    Result.AppendChild(AddNode(tcStr, 'Z04', 'infadMarketplace', 01, 5000, 0,
                                     DCe.InfAdic.infadMarketplace, DSC_INFCPL));

    Result.AppendChild(AddNode(tcStr, 'Z04a', 'infadTransp', 01, 5000, 0,
                                          DCe.InfAdic.infadTransp, DSC_INFCPL));

    nodeArray := GerarInfAdicObsCont;

    for i := 0 to DCe.InfAdic.obsCont.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;

    nodeArray := GerarInfAdicObsMarketplace;
    for i := 0 to DCe.InfAdic.obsMarketplace.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TDCeXmlWriter.GerarInfAdicObsCont: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, DCe.InfAdic.obsCont.Count);

  for i := 0 to DCe.InfAdic.obsCont.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('obsCont');

    Result[i].SetAttribute('xCampo', DCe.InfAdic.obsCont[i].xCampo);

    if length(trim(DCe.InfAdic.obsCont[i].xCampo)) > 20 then
      wAlerta('Z06', 'xCampo', DSC_XCAMPO, ERR_MSG_MAIOR);

    if length(trim(DCe.InfAdic.obsCont[i].xCampo)) = 0 then
      wAlerta('Z06', 'xCampo', DSC_XCAMPO, ERR_MSG_VAZIO);

    Result[i].AppendChild(AddNode(tcStr, 'Z06', 'xTexto', 01, 60, 1,
                                    DCe.InfAdic.obsCont[i].xTexto, DSC_XTEXTO));
  end;

  if DCe.InfAdic.obsCont.Count > 10 then
    wAlerta('Z05', 'obsCont', DSC_OBSCONT, ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TDCeXmlWriter.GerarInfAdicObsMarketplace: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, DCe.InfAdic.obsMarketplace.Count);

  for i := 0 to DCe.InfAdic.obsMarketplace.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('obsMarketplace');

    Result[i].SetAttribute('xCampo', DCe.InfAdic.obsMarketplace[i].xCampo);

    if length(trim(DCe.InfAdic.obsMarketplace[i].xCampo)) > 20 then
      wAlerta('Z08', 'xCampo', DSC_XCAMPO, ERR_MSG_MAIOR);

    if length(trim(DCe.InfAdic.obsMarketplace[i].xCampo)) = 0 then
      wAlerta('Z08', 'xCampo', DSC_XCAMPO, ERR_MSG_VAZIO);

    Result[i].AppendChild(AddNode(tcStr, 'Z09', 'xTexto', 01, 60, 1,
                                    DCe.InfAdic.obsMarketplace[i].xTexto, DSC_XTEXTO));
  end;

  if DCe.InfAdic.obsMarketplace.Count > 10 then
    wAlerta('Z07', 'obsMarketplace', DSC_OBSCONT, ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TDCeXmlWriter.GerarInfDec: TACBrXmlNode;
var
  xObs1, xObs2: string;
begin
  xObs1 := 'Deverá conter o texto fixo "É contribuinte de ICMS qualquer pessoa ' +
           'física ou jurídica, que realize, com habitualidade ou em volume ' +
           'que caracterize intuito comercial, operações de circulação de ' +
           'mercadoria ou prestações de serviços de transportes interestadual e ' +
           'intermunicipal e de comunicação, ainda que as operações e ' +
           'prestações de iniciem no exterior (Lei Complementar n. 87/96, Art. 4º)"';

  xObs2 := 'Deverá conter o texto fixo "Constitui crime contra a ordem ' +
           'tributária suprimir ou reduzir tributo, ou contribuição social ' +
           'e qualquer acessório: quando negar ou deixar de fornecer, ' +
           'quando obrigatório, nota fiscal ou documento equivalente, ' +
           'relativa a venda de mercadoria ou prestação de serviço, ' +
           'efetivamente realizada ou fornece-la em desacordo com a ' +
           'legislação. Sob pena de reclusão de 2 (dois) e 5 (cinco) anos, e ' +
           'multa (Lei 8.137/90, Art 1a, V)"';

  Result := FDocument.CreateElement('infDec');

  Result.AppendChild(AddNode(tcStr, 'ZA02', 'xObs1', 1, 2000, 1, xObs1, ''));

  Result.AppendChild(AddNode(tcStr, 'ZA03', 'xObs2', 1, 2000, 1, xObs2, ''));
end;

function TDCeXmlWriter.GerarXml: boolean;
var
  Gerar: boolean;
  xCNPJCPF: string;
  DCeNode, xmlNode: TACBrXmlNode;
begin
  Result := False;

  ListaDeAlertas.Clear;

  Versao := Copy(DCe.infDCe.VersaoStr, 9, 4);

  case DCe.Ide.tpEmit of
    teFisco:
      xCNPJCPF := DCe.Fisco.CNPJ;

    teMarketplace:
      xCNPJCPF := DCe.Marketplace.CNPJ;

    teEmissorProprio:
      xCNPJCPF := DCe.EmpEmisProp.CNPJ;

  else
    xCNPJCPF := DCe.Transportadora.CNPJ;
  end;

  DCe.Ide.modelo := 99;

  ChaveDCe := GerarChaveAcesso(DCe.ide.cUF, DCe.ide.dhEmi, xCNPJCPF,
      DCe.ide.serie, DCe.ide.nDC, StrToInt(TipoEmissaoToStr(DCe.ide.tpEmis)),
      StrToInt(EmitenteDCeToStr(DCe.Ide.tpEmit)), DCe.Ide.nSiteAutoriz,
      DCe.ide.cDC, DCe.Ide.modelo);

  DCe.infDCe.ID := 'DCe' + ChaveDCe;
  DCe.ide.cDV := ExtrairDigitoChaveAcesso(DCe.infDCe.ID);

  FDocument.Clear();
  DCeNode := FDocument.CreateElement('DCe', ACBRDCE_NAMESPACE);

  if DCe.procDCe.nProt <> '' then
  begin
    xmlNode := FDocument.CreateElement('DCeProc', ACBRDCE_NAMESPACE);
    xmlNode.SetAttribute('versao', FloatToString(DCe.infDCe.Versao, '.', '#0.00'));
    xmlNode.AppendChild(DCeNode);
    FDocument.Root := xmlNode;
  end
  else
    FDocument.Root := DCeNode;

  xmlNode := GerarInfDCe();
  DCeNode.AppendChild(xmlNode);

  if DCe.infDCeSupl.qrCode <> '' then
  begin
    xmlNode := DCeNode.AddChild('infDCeSupl');
    xmlNode.AppendChild(AddNode(tcStr, 'ZX02', 'qrCode', 100, 600, 1,
                       '<![CDATA[' + DCe.infDCeSupl.qrCode + ']]>', '', False));

    xmlNode.AppendChild(AddNode(tcStr, 'ZX03', 'urlChave', 21,85, 1,
                                           DCe.infDCeSupl.urlChave, '', False));
  end;

  Gerar := (Opcoes.GerarTagAssinatura = taSempre) or
    (
      (Opcoes.GerarTagAssinatura = taSomenteSeAssinada) and
        (DCe.signature.DigestValue <> '') and
        (DCe.signature.SignatureValue <> '') and
        (DCe.signature.X509Certificate <> '')
    ) or
    (
      (Opcoes.GerarTagAssinatura = taSomenteParaNaoAssinada) and
        (DCe.signature.DigestValue = '') and
        (DCe.signature.SignatureValue = '') and
        (DCe.signature.X509Certificate = '')
    );

  if Gerar then
  begin
    FDCe.signature.URI := '#DCe' + OnlyNumber(DCe.infDCe.ID);
    xmlNode := GerarSignature(FDCe.signature);
    DCeNode.AppendChild(xmlNode);
  end;

  if DCe.procDCe.nProt <> '' then
  begin
//    xmlNode := GerarProtDCe;
    FDocument.Root.AppendChild(xmlNode);
  end;
end;

end.
