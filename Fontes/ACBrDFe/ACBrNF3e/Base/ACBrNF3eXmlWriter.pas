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

unit ACBrNF3eXmlWriter;

interface

uses
  Classes, SysUtils,
  ACBrXmlBase, ACBrXmlDocument, ACBrXmlWriter,
  ACBrNF3eClass,
  ACBrNF3eConversao;

type
  TNF3eXmlWriterOptions = class(TACBrXmlWriterOptions)
  private
    FAjustarTagNro: boolean;
    FGerarTagIPIparaNaoTributado: boolean;
    FNormatizarMunicipios: boolean;
    FGerarTagAssinatura: TACBrTagAssinatura;
    FPathArquivoMunicipios: string;
    FValidarInscricoes: boolean;
    FValidarListaServicos: boolean;
    FCamposFatObrigatorios: boolean;

  public
    property AjustarTagNro: boolean read FAjustarTagNro write FAjustarTagNro;
    property GerarTagIPIparaNaoTributado: boolean read FGerarTagIPIparaNaoTributado write FGerarTagIPIparaNaoTributado;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios write FNormatizarMunicipios;
    property GerarTagAssinatura: TACBrTagAssinatura read FGerarTagAssinatura write FGerarTagAssinatura;
    property PathArquivoMunicipios: string read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: boolean read FValidarInscricoes write FValidarInscricoes;
    property ValidarListaServicos: boolean read FValidarListaServicos write FValidarListaServicos;
    property CamposFatObrigatorios: boolean read FCamposFatObrigatorios write FCamposFatObrigatorios;

  end;

  TNF3eXmlWriter = class(TACBrXmlWriter)
  private
    FNF3e: TNF3e;

    FChaveNF3e: string;

    FVersaoDF: TVersaoNF3e;
    FModeloDF: Integer;
    FtpAmb: TACBrTipoAmbiente;
    FtpEmis: TACBrTipoEmissao;
    FIdCSRT: integer;
    FCSRT: string;

    function Gerar_InfNF3e: TACBrXmlNode;
    function Gerar_Ide: TACBrXmlNode;

    function Gerar_Emit: TACBrXmlNode;
    function Gerar_EmitEnderEmit: TACBrXmlNode;

    function Gerar_Dest: TACBrXmlNode;
    function Gerar_DestEnderDest(var UF: string): TACBrXmlNode;

    function Gerar_Acessante: TACBrXmlNode;
    function Gerar_gNF: TACBrXmlNode;
    function Gerar_gSub: TACBrXmlNode;
    function Gerar_gJudic: TACBrXmlNode;
    function Gerar_gGrContrat: TACBrXmlNodeArray;
    function Gerar_gMed: TACBrXmlNodeArray;

    function Gerar_gSCEE: TACBrXmlNode;
    function Gerar_gSCEE_gConsumidor: TACBrXmlNodeArray;
    function Gerar_gSCEE_gSaldoCred: TACBrXmlNodeArray;
    function Gerar_gSCEE_gTipoSaldo: TACBrXmlNodeArray;

    function Gerar_NFdet: TACBrXmlNodeArray;
    function Gerar_NFdet_det(aNFdet: Integer): TACBrXmlNodeArray;
    function Gerar_NFdet_det_gAjusteNF3eAnt(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItemAnt(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItemAnt_retTrib(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItem(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItem_gTarif(aNFdet, aDet: Integer): TACBrXmlNodeArray;
    function Gerar_NFdet_det_DetItem_gAdBand(aNFdet, aDet: Integer): TACBrXmlNodeArray;
    function Gerar_NFdet_det_DetItem_Prod(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItem_Prod_gMedicao(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItem_Prod_gMedicao_gMedida(aNFdet, aDet: Integer): TACBrXmlNode;

    function Gerar_NFdet_det_DetItem_Imposto(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItem_Imposto_ICMS(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItem_Imposto_PIS(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItem_Imposto_PISEfet(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItem_Imposto_COFINS(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItem_Imposto_COFINSEfet(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItem_Imposto_RetTrib(aNFdet, aDet: Integer): TACBrXmlNode;

    function Gerar_NFdet_det_DetItem_gProcRef(aNFdet, aDet: Integer): TACBrXmlNode;
    function Gerar_NFdet_det_DetItem_gProcRef_gProc(aNFdet, aDet: Integer): TACBrXmlNodeArray;
    function Gerar_NFdet_det_DetItem_gContab(aNFdet, aDet: Integer): TACBrXmlNodeArray;

    function Gerar_Total: TACBrXmlNode;
    function Gerar_TotalICMSTotal: TACBrXmlNode;
    function Gerar_TotalretTrib: TACBrXmlNode;

    function Gerar_gFat: TACBrXmlNode;
    function Gerar_gFatEnderCorresp: TACBrXmlNode;
    function Gerar_gFatgPix: TACBrXmlNode;

    function Gerar_gANEEL: TACBrXmlNode;
    function Gerar_gANEEL_gHistFat: TACBrXmlNodeArray;
    function Gerar_gANEEL_gHistFat_gGrandFat(agHistFat: Integer): TACBrXmlNodeArray;

    function Gerar_autXML: TACBrXmlNodeArray;
    function Gerar_InfAdic: TACBrXmlNode;
    function Gerar_gRespTec: TACBrXmlNode;
    function Gerar_ProtNF3e: TACBrXmlNode;

    function GetOpcoes: TNF3eXmlWriterOptions;
    procedure SetOpcoes(AValue: TNF3eXmlWriterOptions);

    procedure AjustarMunicipioUF(out xUF: string; out xMun: string;
      out cMun: integer; cPais: integer; const vxUF, vxMun: string; vcMun: integer);
  protected
    function CreateOptions: TACBrXmlWriterOptions; override;

  public
    constructor Create(AOwner: TNF3e); reintroduce;
    destructor Destroy; override;

    function GerarXml: boolean; override;
    function ObterNomeArquivo: string; overload;

    property Opcoes: TNF3eXmlWriterOptions read GetOpcoes write SetOpcoes;
    property NF3e: TNF3e read FNF3e write FNF3e;

    property VersaoDF: TVersaoNF3e read FVersaoDF write FVersaoDF;
    property ModeloDF: Integer read FModeloDF write FModeloDF;
    property tpAmb: TACBrTipoAmbiente read FtpAmb write FtpAmb;
    property tpEmis: TACBrTipoEmissao read FtpEmis write FtpEmis;
    property IdCSRT: integer read FIdCSRT write FIdCSRT;
    property CSRT: string read FCSRT write FCSRT;

  end;

implementation

uses
  variants,
  dateutils,
  StrUtils,
  Math,
  ACBrDFeConsts,
  ACBrNF3eConsts,
  ACBrValidador,
  ACBrDFeUtil,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.Strings;

constructor TNF3eXmlWriter.Create(AOwner: TNF3e);
begin
  inherited Create;

  TNF3eXmlWriterOptions(Opcoes).AjustarTagNro := True;
  TNF3eXmlWriterOptions(Opcoes).GerarTagIPIparaNaoTributado := True;
  TNF3eXmlWriterOptions(Opcoes).NormatizarMunicipios := False;
  TNF3eXmlWriterOptions(Opcoes).PathArquivoMunicipios := '';
  TNF3eXmlWriterOptions(Opcoes).GerarTagAssinatura := taSomenteSeAssinada;
  TNF3eXmlWriterOptions(Opcoes).ValidarInscricoes := False;
  TNF3eXmlWriterOptions(Opcoes).ValidarListaServicos := False;
  TNF3eXmlWriterOptions(Opcoes).CamposFatObrigatorios := True;

  FNF3e := AOwner;
end;

destructor TNF3eXmlWriter.Destroy;
begin
  inherited Destroy;
end;

function TNF3eXmlWriter.CreateOptions: TACBrXmlWriterOptions;
begin
  Result := TNF3eXmlWriterOptions.Create();
end;

function TNF3eXmlWriter.GetOpcoes: TNF3eXmlWriterOptions;
begin
  Result := TNF3eXmlWriterOptions(FOpcoes);
end;

procedure TNF3eXmlWriter.SetOpcoes(AValue: TNF3eXmlWriterOptions);
begin
  FOpcoes := AValue;
end;

procedure TNF3eXmlWriter.AjustarMunicipioUF(out xUF: string; out xMun: string;
  out cMun: integer; cPais: integer; const vxUF, vxMun: string; vcMun: integer);
var
  PaisBrasil: boolean;
begin
  PaisBrasil := cPais = CODIGO_BRASIL;
  cMun := IfThen(PaisBrasil, vcMun, CMUN_EXTERIOR);
  xMun := IfThen(PaisBrasil, vxMun, XMUN_EXTERIOR);
  xUF := IfThen(PaisBrasil, vxUF, UF_EXTERIOR);

  if Opcoes.NormatizarMunicipios then
    if ((EstaZerado(cMun)) and (xMun <> XMUN_EXTERIOR)) then
      cMun := ObterCodigoMunicipio(xMun, xUF, Opcoes.FPathArquivoMunicipios)
    else if ((EstaVazio(xMun)) and (cMun <> CMUN_EXTERIOR)) then
      xMun := ObterNomeMunicipio(cMun, xUF, Opcoes.FPathArquivoMunicipios);

end;

function TNF3eXmlWriter.ObterNomeArquivo: string;
begin
  Result := OnlyNumber(FNF3e.infNF3e.ID) + '-NF3e.xml';
end;

function TNF3eXmlWriter.GerarXml: boolean;
var
  Gerar: boolean;
  NF3eNode, xmlNode: TACBrXmlNode;
begin
  Result := False;

  ListaDeAlertas.Clear;

  {
    Os campos abaixo tem que ser os mesmos da configuração
  }
{
  NF3e.infNF3e.Versao := VersaoNF3eToDbl(VersaoDF);
  NF3e.Ide.modelo := ModeloDF;
  NF3e.Ide.tpAmb := tpAmb;
  NF3e.ide.tpEmis := tpEmis;
}
  FChaveNF3e := GerarChaveAcesso(NF3e.ide.cUF, NF3e.ide.dhEmi, NF3e.emit.CNPJ,
      NF3e.ide.serie, NF3e.ide.nNF, StrToInt(TipoEmissaoToStr(NF3e.ide.tpEmis)),
      NF3e.ide.cNF, NF3e.ide.modelo,
      StrToInt(SiteAutorizadorToStr(NF3e.Ide.nSiteAutoriz)));

  NF3e.infNF3e.ID := 'NF3e' + FChaveNF3e;
  NF3e.ide.cDV := ExtrairDigitoChaveAcesso(NF3e.infNF3e.ID);
  NF3e.Ide.cNF := ExtrairCodigoChaveAcesso(NF3e.infNF3e.ID);

  FDocument.Clear();
  NF3eNode := FDocument.CreateElement('NF3e', 'http://www.portalfiscal.inf.br/nf3e');

  if NF3e.procNF3e.nProt <> '' then
  begin
    xmlNode := FDocument.CreateElement('NF3eProc', 'http://www.portalfiscal.inf.br/nf3e');
    xmlNode.SetAttribute('versao', FloatToString(NF3e.infNF3e.Versao, '.', '#0.00'));
    xmlNode.AppendChild(NF3eNode);
    FDocument.Root := xmlNode;
  end
  else
  begin
    FDocument.Root := NF3eNode;
  end;

  xmlNode := Gerar_InfNF3e();
  NF3eNode.AppendChild(xmlNode);

  if NF3e.infNF3eSupl.qrCodNF3e <> '' then
  begin
    xmlNode := NF3eNode.AddChild('infNF3eSupl');
    xmlNode.AppendChild(AddNode(tcStr, '#318', 'qrCodNF3e', 50, 1000, 1,
       '<![CDATA[' + NF3e.infNF3eSupl.qrCodNF3e + ']]>', DSC_INFQRCODE, False));
  end;

  if Opcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := True;
    if Opcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((NF3e.signature.DigestValue <> '') and
                (NF3e.signature.SignatureValue <> '') and
                (NF3e.signature.X509Certificate <> ''));

    if Opcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
       Gerar := ((NF3e.signature.DigestValue = '') and
                 (NF3e.signature.SignatureValue = '') and
                 (NF3e.signature.X509Certificate = ''));
    if Gerar then
    begin
      FNF3e.signature.URI := '#NF3e' + OnlyNumber(NF3e.infNF3e.ID);
      xmlNode := GerarSignature(FNF3e.signature);
      NF3eNode.AppendChild(xmlNode);
    end;
  end;

  if NF3e.procNF3e.nProt <> '' then
  begin
    xmlNode := Gerar_ProtNF3e;
    FDocument.Root.AppendChild(xmlNode);
  end;
end;

function TNF3eXmlWriter.Gerar_InfNF3e: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := FDocument.CreateElement('infNF3e');
  Result.SetAttribute('Id', 'NF3e' + NF3e.infNF3e.ID);
  Result.SetAttribute('versao', FloatToString(NF3e.infNF3e.Versao, '.', '#0.00'));

  Result.AppendChild(Gerar_Ide);
  Result.AppendChild(Gerar_Emit);
  Result.AppendChild(Gerar_Dest);
  Result.AppendChild(Gerar_Acessante);
  Result.AppendChild(Gerar_gSub);
  Result.AppendChild(Gerar_gJudic);

  nodeArray := Gerar_gGrContrat;
  for i := 0 to NF3e.gGrContrat.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  nodeArray := Gerar_gMed;
  for i := 0 to NF3e.gMed.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(Gerar_gSCEE);

  nodeArray := Gerar_NFDet;
  for i := 0 to NF3e.NFDet.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(Gerar_Total);
  Result.AppendChild(Gerar_gFat);
  Result.AppendChild(Gerar_gANEEL);

  nodeArray := Gerar_autXML;
  for i := 0 to NF3e.autXML.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(Gerar_InfAdic);
  Result.AppendChild(Gerar_gRespTec);
end;

function TNF3eXmlWriter.Gerar_Ide: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ide');
  Result.AppendChild(AddNode(tcInt, '#5', 'cUF', 2, 2, 1, NF3e.ide.cUF, DSC_CUF));

  if not ValidarCodigoUF(NF3e.ide.cUF) then
    wAlerta('#5', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#6', 'tpAmb  ', 1, 1, 1,
    TipoAmbienteToStr(NF3e.Ide.tpAmb), DSC_TPAMB));

  Result.AppendChild(AddNode(tcInt, '#7', 'mod', 2, 2, 1,
    NF3e.ide.modelo, DSC_MOD));

  Result.AppendChild(AddNode(tcInt, '#8', 'serie', 1, 3, 1,
    NF3e.ide.serie, DSC_SERIE));

  Result.AppendChild(AddNode(tcInt, '#9', 'nNF', 1, 9, 1, NF3e.ide.nNF, DSC_NDF));

  Result.AppendChild(AddNode(tcInt, '#10', 'cNF', 7, 7, 1,
                                                        NF3e.Ide.cNF, DSC_CDF));

  Result.AppendChild(AddNode(tcInt, '#11', 'cDV', 1, 1, 1, NF3e.Ide.cDV, DSC_CDV));

  Result.AppendChild(AddNode(tcStr, '#12', 'dhEmi', 25, 25, 1,
    DateTimeTodh(NF3e.ide.dhEmi) + GetUTC(CodigoUFparaUF(NF3e.ide.cUF), NF3e.ide.dhEmi),
    DSC_DEMI));

  Result.AppendChild(AddNode(tcStr, '#13', 'tpEmis', 1, 1, 1,
    TipoEmissaoToStr(NF3e.Ide.tpEmis), DSC_TPEMIS));

  Result.AppendChild(AddNode(tcInt, '#13a', 'nSiteAutoriz', 1, 1, 1,
    NF3e.Ide.nSiteAutoriz, DSC_NSITEAUTORIZ));

  Result.AppendChild(AddNode(tcInt, '#14', 'cMunFG ', 7, 7, 1,
    NF3e.ide.cMunFG, DSC_CMUNFG));

  if not ValidarMunicipio(NF3e.ide.cMunFG) then
    wAlerta('#14', 'cMunFG', DSC_CMUNFG, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#15', 'finNF3e', 1, 1, 1,
    finNF3eToStr(NF3e.Ide.finNF3e), DSC_FINNF3e));

  Result.AppendChild(AddNode(tcStr, '#16', 'verProc', 1, 20, 1,
    NF3e.Ide.verProc, DSC_VERPROC));

  if (NF3e.Ide.dhCont > 0) or (NF3e.Ide.xJust <> '') then
  begin
    Result.AppendChild(AddNode(tcStr, '#17', 'dhCont', 25, 25,
      1, DateTimeTodh(NF3e.ide.dhCont) + GetUTC(CodigoUFparaUF(NF3e.ide.cUF),
      NF3e.ide.dhCont), DSC_DHCONT));

    Result.AppendChild(AddNode(tcStr, '#18', 'xJust', 15, 256, 1,
      NF3e.ide.xJust, DSC_XJUSTCONT));
  end;
end;

function TNF3eXmlWriter.Gerar_Emit: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('emit');

  Result.AppendChild(AddNode(tcStr, '#20', 'CNPJ', 14, 14, 1,
    NF3e.Emit.CNPJ, DSC_CNPJ));

  if NF3e.Emit.IE = 'ISENTO' then
    Result.AppendChild(AddNode(tcStr, '#21', 'IE', 2, 14, 1, NF3e.Emit.IE, DSC_IE))
  else
    Result.AppendChild(AddNode(tcStr, '#21', 'IE', 2, 14, 1,
      OnlyNumber(NF3e.Emit.IE), DSC_IE));

  if (Opcoes.ValidarInscricoes) then
  begin
    if Length(NF3e.Emit.IE) = 0 then
      wAlerta('#21', 'IE', DSC_IE, ERR_MSG_VAZIO)
    else
    begin
      if not ValidarIE(NF3e.Emit.IE, CodigoUFparaUF(NF3e.Ide.cUF)) then
        wAlerta('#21', 'IE', DSC_IE, ERR_MSG_INVALIDO);
    end;
  end;

  Result.AppendChild(AddNode(tcStr, '#22', 'xNome', 2, 60, 1,
    NF3e.Emit.xNome, DSC_XNOME));

  Result.AppendChild(AddNode(tcStr, '#23', 'xFant', 1, 60, 0,
    NF3e.Emit.xFant, DSC_XFANT));

  Result.AppendChild(Gerar_EmitEnderEmit);
end;

function TNF3eXmlWriter.Gerar_EmitEnderEmit: TACBrXmlNode;
var
  cMun: integer;
  xMun, xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, NF3e.Emit.enderEmit.UF,
    NF3e.Emit.enderEmit.xMun, NF3e.Emit.EnderEmit.cMun);

  Result := FDocument.CreateElement('enderEmit');

  Result.AppendChild(AddNode(tcStr, '#25', 'xLgr', 2, 60, 1,
    NF3e.Emit.enderEmit.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#26', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, NF3e.Emit.enderEmit.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#27', 'xCpl', 1, 60, 0,
    NF3e.Emit.enderEmit.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#28', 'xBairro', 2, 60, 1,
    NF3e.Emit.enderEmit.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, '#29', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('#29', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#30', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, '#31', 'CEP', 8, 8, 1,
    NF3e.Emit.enderEmit.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, '#32', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('#32', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#33', 'fone', 7, 12, 0,
    OnlyNumber(NF3e.Emit.enderEmit.fone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, '#34', 'email', 1, 60, 0,
    NF3e.Emit.enderEmit.email, DSC_EMAIL));
end;

function TNF3eXmlWriter.Gerar_Dest: TACBrXmlNode;
var
  UF: string;
const
  HOM_NOME_DEST = 'NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
begin
  UF := '';
  Result := FDocument.CreateElement('dest');

  if NF3e.Ide.tpAmb = taProducao then
    Result.AppendChild(AddNode(tcStr, '#36', 'xNome', 2, 60, 1,
      NF3e.Dest.xNome, DSC_XNOME))
  else
    Result.AppendChild(AddNode(tcStr, '#36', 'xNome', 2, 60, 1,
      HOM_NOME_DEST, DSC_XNOME));


  if NF3e.Dest.idOutros <> '' then
    Result.AppendChild(AddNode(tcStr, '#39', 'idOutros', 2, 20, 1,
      NF3e.Dest.idOutros, DSC_IDESTR))
  else
    Result.AppendChild(AddNodeCNPJCPF('#37', '#38', NF3e.Dest.CNPJCPF));

  Result.AppendChild(AddNode(tcStr, '#40', 'indIEDest', 1, 1, 1,
    indIEDestToStr(NF3e.Dest.indIEDest), DSC_INDIEDEST));

  if NF3e.Dest.indIEDest <> inIsento then
  begin
    if (NF3e.Dest.IE <> '') then
    begin
      // Inscrição Estadual
      if NF3e.Dest.IE = '' then
        Result.AppendChild(AddNode(tcStr, '#41', 'IE', 0, 14, 1, '', DSC_IE))
      else
      if NF3e.Dest.IE = 'ISENTO' then
        Result.AppendChild(AddNode(tcStr, '#41', 'IE', 0, 14, 1,
          NF3e.Dest.IE, DSC_IE))
      else if trim(NF3e.Dest.IE) <> '' then
        Result.AppendChild(AddNode(tcStr, '#41', 'IE', 0, 14, 1,
          OnlyNumber(NF3e.Dest.IE), DSC_IE));

      if (Opcoes.ValidarInscricoes) and (NF3e.Dest.IE <> '') and
         (NF3e.Dest.IE <> 'ISENTO') then
        if not ValidarIE(NF3e.Dest.IE, UF) then
          wAlerta('#41', 'IE', DSC_IE, ERR_MSG_INVALIDO);
    end;
  end;

  Result.AppendChild(AddNode(tcStr, '#42', 'IM', 1, 15, 0, NF3e.Dest.IM, DSC_IM));

  if NF3e.Dest.cNIS <> '' then
    Result.AppendChild(AddNode(tcStr, '#43', 'cNIS', 15, 15, 1,
      NF3e.Dest.cNIS, DSC_CNIS))
  else
  if NF3e.Dest.NB <> '' then
    Result.AppendChild(AddNode(tcStr, '#43b', 'NB', 10, 10, 1,
      NF3e.Dest.NB, DSC_NB));

  Result.AppendChild(AddNode(tcStr, '#44', 'xNomeAdicional', 2, 60, 0,
    NF3e.Dest.xNomeAdicional, DSC_XNOME));

  Result.AppendChild(Gerar_DestEnderDest(UF));
end;

function TNF3eXmlWriter.Gerar_DestEnderDest(var UF: string): TACBrXmlNode;
var
  cMun: integer;
  xMun, xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, NF3e.Dest.enderDest.UF,
    NF3e.Dest.enderDest.xMun, NF3e.Dest.enderDest.cMun);

  UF := xUF;
  Result := FDocument.CreateElement('enderDest');

  Result.AppendChild(AddNode(tcStr, '#46', 'xLgr', 2, 60, 1,
    NF3e.Dest.enderDest.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#47', 'nro', 01, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, NF3e.Dest.enderDest.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#48', 'xCpl', 1, 60, 0,
    NF3e.Dest.enderDest.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#49', 'xBairro', 1, 60, 1,
    NF3e.Dest.enderDest.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, '#50', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('#50', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#51', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, '#52', 'CEP', 8, 8, 1,
    NF3e.Dest.enderDest.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, '#53', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('#53', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#54', 'fone', 7, 12, 0,
    OnlyNumber(NF3e.Dest.enderDest.fone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, '#55', 'email', 01, 60, 0,
    NF3e.Dest.EnderDest.Email, DSC_EMAIL));
end;

function TNF3eXmlWriter.Gerar_Acessante: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('acessante');

  Result.AppendChild(AddNode(tcStr, '#57', 'idAcesso', 1, 15, 1,
    NF3e.acessante.idAcesso, DSC_IDACESSO));

  Result.AppendChild(AddNode(tcStr, '#58', 'idCodCliente', 2, 20, 0,
    NF3e.acessante.idCodCliente, DSC_IDCODCLIENTE));

  Result.AppendChild(AddNode(tcStr, '#59', 'tpAcesso', 1, 1, 1,
    tpAcessoToStr(NF3e.acessante.tpAcesso), DSC_TPACESSO));

  Result.AppendChild(AddNode(tcStr, '#60', 'xNomeUC', 2, 60, 0,
    NF3e.acessante.xNomeUC, DSC_XNOMEUC));

  Result.AppendChild(AddNode(tcStr, '#61', 'tpClasse', 2, 2, 0,
    tpClasseToStr(NF3e.acessante.tpClasse), DSC_TPCLASSE));

  Result.AppendChild(AddNode(tcStr, '#62', 'tpSubClasse', 2, 2, 0,
    tpSubClasseToStr(NF3e.acessante.tpSubClasse), DSC_TPSUBCLASSE));

  Result.AppendChild(AddNode(tcStr, '#63', 'tpFase', 1, 1, 1,
    tpFaseToStr(NF3e.acessante.tpFase), DSC_TPFASE));

  Result.AppendChild(AddNode(tcStr, '#64', 'tpGrpTensao', 2, 2, 1,
    tpGrpTensaoToStr(NF3e.acessante.tpGrpTensao), DSC_TPGRPTENSAO));

  Result.AppendChild(AddNode(tcStr, '#65', 'tpModTar', 2, 2, 1,
    tpModTarToStr(NF3e.acessante.tpModTar), DSC_TPMODTAR));

  Result.AppendChild(AddNode(tcStr, '#66', 'latGPS', 2, 10, 1,
    NF3e.acessante.latGPS, DSC_LATGPS));

  Result.AppendChild(AddNode(tcStr, '#67', 'longGPS', 2, 10, 1,
    NF3e.acessante.longGPS, DSC_LONGGPS));

  Result.AppendChild(AddNode(tcStr, '#68', 'codRoteiroLeitura', 2, 100, 0,
    NF3e.acessante.codRoteiroLeitura, DSC_CODROTEIROLEITURA));
end;

function TNF3eXmlWriter.Gerar_gSub: TACBrXmlNode;
begin
  Result := nil;

  if (NF3e.gSub.chNF3e <> '') or (NF3e.gSub.CNPJ <> '') then
  begin
    Result := FDocument.CreateElement('gSub');

    if NF3e.gSub.chNF3e <> '' then
    begin
      Result.AppendChild(AddNode(tcStr, '#70', 'chNF3e', 44, 44, 1,
        NF3e.gSub.chNF3e, DSC_CHNF3E));

      if not ValidarChave(NF3e.gSub.chNF3e) then
        wAlerta('#70', 'chNF3e', DSC_CHNF3E, ERR_MSG_INVALIDO);
    end
    else
      Result.AppendChild(Gerar_gNF);

    Result.AppendChild(AddNode(tcStr, '#78', 'motSub', 2, 2, 1,
      motSubToStr(NF3e.gSub.motSub), DSC_MOTSUB));
  end;
end;

function TNF3eXmlWriter.Gerar_gNF: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gNF');

  Result.AppendChild(AddNode(tcStr, '#72', 'CNPJ', 14, 14, 1,
    NF3e.gSub.CNPJ, DSC_CNPJ));

  Result.AppendChild(AddNode(tcStr, '#73', 'serie', 1, 3, 1,
    NF3e.gSub.serie, DSC_SERIE));

  Result.AppendChild(AddNode(tcStr, '#74', 'nNF', 1, 9, 1,
    NF3e.gSub.nNF, DSC_NDF));

  Result.AppendChild(AddNode(tcStr, '#75', 'CompetEmis', 6, 6, 1,
    FormatDateTime('yyyymm', NF3e.gSub.CompetEmis), DSC_COMPETEMIS));

  Result.AppendChild(AddNode(tcStr, '#76', 'CompetApur', 6, 6, 1,
    FormatDateTime('yyyymm', NF3e.gSub.CompetApur), DSC_COMPETAPUR));

  Result.AppendChild(AddNode(tcStr, '#77', 'hash115', 28, 28, 0,
    NF3e.gSub.hash115, DSC_HASH115));
end;

function TNF3eXmlWriter.Gerar_gJudic: TACBrXmlNode;
begin
  Result := nil;

  if (NF3e.gJudic.chNF3e <> '') then
  begin
    Result := FDocument.CreateElement('gJudic');

    Result.AppendChild(AddNode(tcStr, '#80', 'chNF3e', 44, 44, 1,
      NF3e.gJudic.chNF3e, DSC_CHNF3E));

    if not ValidarChave(NF3e.gJudic.chNF3e) then
      wAlerta('#080', 'chNF3e', DSC_CHNF3E, ERR_MSG_INVALIDO);
  end;
end;

function TNF3eXmlWriter.Gerar_gGrContrat: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, NF3e.gGrContrat.Count);

  for i := 0 to NF3e.gGrContrat.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('gGrContrat');

    Result[i].SetAttribute('nContrat', FormatFloat('00', NF3e.gGrContrat[i].nContrat));

    if (NF3e.gGrContrat[i].nContrat > 20) then
      wAlerta('#82', 'nContrat', DSC_NCONTRAT, ERR_MSG_MAIOR);

    if (NF3e.gGrContrat[i].nContrat < 1) then
      wAlerta('#82', 'nContrat', DSC_NCONTRAT, ERR_MSG_MENOR);

    Result[i].AppendChild(AddNode(tcStr, '#83', 'tpGrContrat', 1, 1, 1,
     tpGrContratToStr(NF3e.gGrContrat[i].tpGrContrat), DSC_TPGRCONTRAT));

    Result[i].AppendChild(AddNode(tcStr, '#84', 'tpPosTar', 1, 1, 1,
     tpPosTarToStr(NF3e.gGrContrat[i].tpPosTar), DSC_TPPOSTAR));

    Result[i].AppendChild(AddNode(tcDe2, '#85', 'qUnidContrat', 1, 15, 1,
     NF3e.gGrContrat[i].qUnidContrat, DSC_QUNIDCONTRAT));
  end;

  if NF3e.gGrContrat.Count > 20 then
    wAlerta('#81', 'gGrContrat', '', ERR_MSG_MAIOR_MAXIMO + '20');
end;

function TNF3eXmlWriter.Gerar_gMed: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, NF3e.gMed.Count);

  for i := 0 to NF3e.gMed.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('gMed');

    Result[i].SetAttribute('nMed', FormatFloat('00', NF3e.gMed[i].nMed));

    if (NF3e.gMed[i].nMed > 20) then
      wAlerta('#87', 'nMed', DSC_NCONTRAT, ERR_MSG_MAIOR);

    if (NF3e.gMed[i].nMed < 1) then
      wAlerta('#87', 'nMed', DSC_NCONTRAT, ERR_MSG_MENOR);

    Result[i].AppendChild(AddNode(tcStr, '#88', 'idMedidor', 2, 20, 1,
     NF3e.gMed[i].idMedidor, DSC_IDMEDIDOR));

    Result[i].AppendChild(AddNode(tcDat, '#89', 'dMedAnt', 10, 10, 1,
     NF3e.gMed[i].dMedAnt, DSC_DMEDANT));

    Result[i].AppendChild(AddNode(tcDat, '#90', 'dMedAtu', 10, 10, 1,
     NF3e.gMed[i].dMedAtu, DSC_DMEDATU));
  end;

  if NF3e.gMed.Count > 20 then
    wAlerta('#086', 'gMed', '', ERR_MSG_MAIOR_MAXIMO + '20');
end;

function TNF3eXmlWriter.Gerar_gSCEE: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  if (NF3e.gSCEE.gConsumidor.Count > 0) then
  begin
    Result := FDocument.CreateElement('gSCEE');

    Result.AppendChild(AddNode(tcStr, '#92', 'tpPartComp', 1, 1, 1,
      tpPartCompToStr(NF3e.gSCEE.tpPartComp), DSC_TPPARTCOMP));

    nodeArray := Gerar_gSCEE_gConsumidor;
    for i := 0 to NF3e.gSCEE.gConsumidor.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;

    if (NF3e.gSCEE.gSaldoCred.Count > 0) then
    begin
      nodeArray := Gerar_gSCEE_gSaldoCred;
      for i := 0 to NF3e.gSCEE.gSaldoCred.Count - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;
    end
    else
    begin
      nodeArray := Gerar_gSCEE_gTipoSaldo;
      for i := 0 to NF3e.gSCEE.gTipoSaldo.Count - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;
    end;
  end;
end;

function TNF3eXmlWriter.Gerar_gSCEE_gConsumidor: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NF3e.gSCEE.gConsumidor.Count);

  for i := 0 to NF3e.gSCEE.gConsumidor.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('gConsumidor');

    Result[i].AppendChild(AddNode(tcStr, '#94', 'idAcessGer', 2, 20, 1,
     NF3e.gSCEE.gConsumidor[i].idAcessGer, DSC_IDACESSGER));

    Result[i].AppendChild(AddNode(tcDe3, '#95', 'vPotInst', 1, 9, 1,
     NF3e.gSCEE.gConsumidor[i].vPotInst, DSC_VPOTINST));

    Result[i].AppendChild(AddNode(tcStr, '#96', 'tpFonteEnergia', 1, 1, 1,
     tpFonteEnergiaToStr(NF3e.gSCEE.gConsumidor[i].tpFonteEnergia), DSC_TPFONTEENERGIA));

    Result[i].AppendChild(AddNode(tcDe3, '#97', 'enerAloc', 1, 12, 1,
     NF3e.gSCEE.gConsumidor[i].enerAloc, DSC_ENERALOC));

    Result[i].AppendChild(AddNode(tcStr, '#98', 'tpPosTar', 1, 1, 1,
     tpPosTarToStr(NF3e.gSCEE.gConsumidor[i].tpPosTar), DSC_TPPOSTAR));
  end;

  if NF3e.gSCEE.gConsumidor.Count > 999 then
    wAlerta('93', 'gConsumidor', '', ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TNF3eXmlWriter.Gerar_gSCEE_gSaldoCred: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NF3e.gSCEE.gSaldoCred.Count);

  for i := 0 to NF3e.gSCEE.gSaldoCred.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('gSaldoCred');

    Result[i].AppendChild(AddNode(tcStr, '#99', 'tpPosTar', 1, 1, 1,
     tpPosTarToStr(NF3e.gSCEE.gSaldoCred[i].tpPosTar), DSC_TPPOSTAR));

    if Frac(NF3e.gSCEE.gSaldoCred[i].vSaldAnt) > 0 then
      Result[i].AppendChild(AddNode(tcDe4, '#100', 'vSaldAnt', 1, 15, 1,
       NF3e.gSCEE.gSaldoCred[i].vSaldAnt, DSC_VSALDANT))
    else
      Result[i].AppendChild(AddNode(tcInt, '#100', 'vSaldAnt', 1, 15, 1,
       NF3e.gSCEE.gSaldoCred[i].vSaldAnt, DSC_VSALDANT));

    if Frac(NF3e.gSCEE.gSaldoCred[i].vCredExpirado) > 0 then
      Result[i].AppendChild(AddNode(tcDe4, '#101', 'vCredExpirado', 1, 15, 1,
       NF3e.gSCEE.gSaldoCred[i].vCredExpirado, DSC_VCREDEXPIRADO))
    else
      Result[i].AppendChild(AddNode(tcInt, '#101', 'vCredExpirado', 1, 15, 1,
       NF3e.gSCEE.gSaldoCred[i].vCredExpirado, DSC_VCREDEXPIRADO));

    if Frac(NF3e.gSCEE.gSaldoCred[i].vSaldAtual) > 0 then
      Result[i].AppendChild(AddNode(tcDe4, '#102', 'vSaldAtual', 1, 15, 1,
       NF3e.gSCEE.gSaldoCred[i].vSaldAtual, DSC_VSALDATUAL))
    else
      Result[i].AppendChild(AddNode(tcInt, '#102', 'vSaldAtual', 1, 15, 1,
       NF3e.gSCEE.gSaldoCred[i].vSaldAtual, DSC_VSALDATUAL));

    if (NF3e.gSCEE.gSaldoCred[i].vCredExpirar > 0) or
       (NF3e.gSCEE.gSaldoCred[i].CompetExpirar > 0) then
    begin
      if Frac(NF3e.gSCEE.gSaldoCred[i].vCredExpirar) > 0 then
        Result[i].AppendChild(AddNode(tcDe4, '#103', 'vCredExpirar', 1, 15, 1,
         NF3e.gSCEE.gSaldoCred[i].vCredExpirar, DSC_VCREDEXPIRAR))
      else
        Result[i].AppendChild(AddNode(tcInt, '#103', 'vCredExpirar', 1, 15, 1,
         NF3e.gSCEE.gSaldoCred[i].vCredExpirar, DSC_VCREDEXPIRAR));

      Result[i].AppendChild(AddNode(tcStr, '#104', 'CompetExpirar', 6, 6, 1,
       FormatDateTime('yyyymm', NF3e.gSCEE.gSaldoCred[i].CompetExpirar), DSC_COMPETEXPIRAR));
    end;
  end;

  if NF3e.gSCEE.gSaldoCred.Count > 3 then
    wAlerta('#098', 'gSaldoCred', '', ERR_MSG_MAIOR_MAXIMO + '3');
end;

function TNF3eXmlWriter.Gerar_gSCEE_gTipoSaldo: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NF3e.gSCEE.gTipoSaldo.Count);

  for i := 0 to NF3e.gSCEE.gTipoSaldo.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('gTipoSaldo');

    Result[i].SetAttribute('nTipoSaldo', FormatFloat('00', i + 1));

    Result[i].AppendChild(AddNode(tcStr, '#99', 'tpPosTar', 1, 1, 1,
     tpPosTarToStr(NF3e.gSCEE.gTipoSaldo[i].tpPosTar), DSC_TPPOSTAR));

    if Frac(NF3e.gSCEE.gTipoSaldo[i].vSaldAnt) > 0 then
      Result[i].AppendChild(AddNode(tcDe4, '#100', 'vSaldAnt', 1, 15, 1,
       NF3e.gSCEE.gTipoSaldo[i].vSaldAnt, DSC_VSALDANT))
    else
      Result[i].AppendChild(AddNode(tcInt, '#100', 'vSaldAnt', 1, 15, 1,
       NF3e.gSCEE.gTipoSaldo[i].vSaldAnt, DSC_VSALDANT));

    if Frac(NF3e.gSCEE.gTipoSaldo[i].vCredExpirado) > 0 then
      Result[i].AppendChild(AddNode(tcDe4, '#101', 'vCredExpirado', 1, 15, 1,
       NF3e.gSCEE.gTipoSaldo[i].vCredExpirado, DSC_VCREDEXPIRADO))
    else
      Result[i].AppendChild(AddNode(tcInt, '#101', 'vCredExpirado', 1, 15, 1,
       NF3e.gSCEE.gTipoSaldo[i].vCredExpirado, DSC_VCREDEXPIRADO));

    if Frac(NF3e.gSCEE.gTipoSaldo[i].vSaldAtual) > 0 then
      Result[i].AppendChild(AddNode(tcDe4, '#102', 'vSaldAtual', 1, 15, 1,
       NF3e.gSCEE.gTipoSaldo[i].vSaldAtual, DSC_VSALDATUAL))
    else
      Result[i].AppendChild(AddNode(tcInt, '#102', 'vSaldAtual', 1, 15, 1,
       NF3e.gSCEE.gTipoSaldo[i].vSaldAtual, DSC_VSALDATUAL));

    if (NF3e.gSCEE.gTipoSaldo[i].vCredExpirar > 0) or
       (NF3e.gSCEE.gTipoSaldo[i].CompetExpirar > 0) then
    begin
      if Frac(NF3e.gSCEE.gTipoSaldo[i].vCredExpirar) > 0 then
        Result[i].AppendChild(AddNode(tcDe4, '#103', 'vCredExpirar', 1, 15, 1,
         NF3e.gSCEE.gTipoSaldo[i].vCredExpirar, DSC_VCREDEXPIRAR))
      else
        Result[i].AppendChild(AddNode(tcInt, '#103', 'vCredExpirar', 1, 15, 1,
         NF3e.gSCEE.gTipoSaldo[i].vCredExpirar, DSC_VCREDEXPIRAR));

      Result[i].AppendChild(AddNode(tcStr, '#104', 'CompetExpirar', 6, 6, 1,
       FormatDateTime('yyyymm', NF3e.gSCEE.gTipoSaldo[i].CompetExpirar), DSC_COMPETEXPIRAR));
    end;
  end;

  if NF3e.gSCEE.gTipoSaldo.Count > 10 then
    wAlerta('#098', 'gTipoSaldo', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNF3eXmlWriter.Gerar_NFdet: TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, NF3e.NFDet.Count);

  for i := 0 to NF3e.NFDet.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('NFdet');

    if NF3e.NFDet[i].chNF3eAnt <> '' then
      Result[i].SetAttribute('chNF3eAnt', NF3e.NFDet[i].chNF3eAnt);

    if NF3e.NFDet[i].mod6HashAnt <> '' then
      Result[i].SetAttribute('mod6HashAnt', NF3e.NFDet[i].mod6HashAnt);

    nodeArray := Gerar_NFdet_Det(i);
    for j := 0 to NF3e.NFDet[i].Det.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if NF3e.NFDet.Count > 13 then
    wAlerta('#105', 'NFdet', '', ERR_MSG_MAIOR_MAXIMO + '13');
end;

function TNF3eXmlWriter.Gerar_NFdet_det(aNFdet: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NF3e.NFDet[aNFdet].Det.Count);

  for i := 0 to NF3e.NFDet[aNFdet].Det.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('det');

    Result[i].SetAttribute('nItem', IntToStr(NF3e.NFDet[aNFdet].Det[i].nItem));

    if NF3e.NFDet[aNFdet].Det[i].gAjusteNF3eAnt.tpAjuste <> taNenhum then
      Result[i].AppendChild(Gerar_NFdet_det_gAjusteNF3eAnt(aNFdet, i));

    if NF3e.NFDet[aNFdet].Det[i].detItemAnt.nItemAnt > 0 then
      Result[i].AppendChild(Gerar_NFdet_det_DetItemAnt(aNFdet, i))
    else
      Result[i].AppendChild(Gerar_NFdet_det_DetItem(aNFdet, i));
  end;

  if NF3e.NFDet[aNFdet].Det.Count > 990 then
    wAlerta('108', 'det', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TNF3eXmlWriter.Gerar_NFdet_det_gAjusteNF3eAnt(aNFdet,
  aDet: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gAjusteNF3eAnt');

  Result.AppendChild(AddNode(tcStr, '#111', 'tpAjuste', 1, 1, 1,
    tpAjusteToStr(NF3e.NFDet[aNFdet].Det[aDet].gAjusteNF3eAnt.tpAjuste), DSC_TPAJUSTE));

  Result.AppendChild(AddNode(tcStr, '#112', 'motAjuste', 1, 1, 1,
    MotAjusteToStr(NF3e.NFDet[aNFdet].Det[aDet].gAjusteNF3eAnt.motAjuste), DSC_MOTAJUSTE));
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItemAnt(aNFdet,
  aDet: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('detItemAnt');

  Result.SetAttribute('nItemAnt', IntToStr(NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.nItemAnt));

  // pode ter de 2 ou 6 casas decimais
  Result.AppendChild(AddNode(tcDe2, '#115', 'vItem', 1, 15, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.vItem, DSC_VITEM));

  if Frac(NF3e.NFDet[aNFDet].Det[aDet].detItemAnt.qFaturada) > 0 then
    Result.AppendChild(AddNode(tcDe4, '#116', 'qFaturada', 1, 15, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.qFaturada, DSC_QFATURADA))
  else
    Result.AppendChild(AddNode(tcInt, '#116', 'qFaturada', 1, 15, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.qFaturada, DSC_QFATURADA));

  // pode ter de 2 ou 6 casas decimais
  Result.AppendChild(AddNode(tcDe2, '#117', 'vProd', 1, 15, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.vProd, DSC_VPROD));

  Result.AppendChild(AddNode(tcInt, '#118', 'cClass', 7, 7, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.cClass, DSC_CCLASS));

  Result.AppendChild(AddNode(tcDe2, '#119', 'vBC', 1, 5, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.vBC, DSC_VBC));

  Result.AppendChild(AddNode(tcDe2, '#120', 'pICMS', 1, 15, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.pICMS, DSC_PICMS));

  Result.AppendChild(AddNode(tcDe2, '#121', 'vICMS', 1, 15, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.vICMS, DSC_VICMS));

  Result.AppendChild(AddNode(tcDe2, '#121a', 'vFCP', 1, 15, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.vFCP, DSC_VFCP));

  Result.AppendChild(AddNode(tcDe2, '#121b', 'vBCST', 1, 15, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.vBCST, DSC_VBCST));

  Result.AppendChild(AddNode(tcDe2, '#121c', 'vICMSST', 1, 15, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.vICMSST, DSC_VICMSST));

  Result.AppendChild(AddNode(tcDe2, '#121d', 'vFCPST', 1, 15, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.vFCPST, DSC_VFCPST));

  Result.AppendChild(AddNode(tcDe2, '#122', 'vPIS', 1, 15, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.vPIS, DSC_VPIS));

  Result.AppendChild(AddNode(tcDe2, '#122a', 'vPISEfet', 1, 15, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.vPISEfet, DSC_VPISEfet));

  Result.AppendChild(AddNode(tcDe2, '#123', 'vCOFINS', 1, 15, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.vCOFINS, DSC_VCOFINS));

  Result.AppendChild(AddNode(tcDe2, '#123a', 'vCOFINSEfet', 1, 15, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.vCOFINSEfet, DSC_VCOFINSEfet));

  if (NF3e.NFDet[aNFDet].Det[aDet].detItemAnt.retTrib.vRetPIS > 0) or
     (NF3e.NFDet[aNFDet].Det[aDet].detItemAnt.retTrib.vRetCOFINS > 0) or
     (NF3e.NFDet[aNFDet].Det[aDet].detItemAnt.retTrib.vRetCSLL > 0) or
     (NF3e.NFDet[aNFDet].Det[aDet].detItemAnt.retTrib.vBCIRRF > 0) or
     (NF3e.NFDet[aNFDet].Det[aDet].detItemAnt.retTrib.vIRRF > 0) then
    Result.AppendChild(Gerar_NFdet_det_DetItemAnt_retTrib(aNFdet, aDet));

  if NF3e.NFDet[aNFdet].Det[aDet].detItemAnt.indDevolucao = tiSim then
    Result.AppendChild(AddNode(tcStr, '#153', 'indDevolucao', 1, 1, 1, '1', ''));
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItemAnt_retTrib(aNFdet,
  aDet: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('retTrib');

  Result.AppendChild(AddNode(tcDe2, '#123c', 'vRetPIS', 1, 15, 1,
    NF3e.NFDet[aNFDet].Det[aDet].detItemAnt.retTrib.vRetPIS, DSC_VRETPIS));

  Result.AppendChild(AddNode(tcDe2, '#123d', 'vRetCOFINS', 1, 15, 1,
    NF3e.NFDet[aNFDet].Det[aDet].detItemAnt.retTrib.vRetCOFINS, DSC_VRETCOFINS));

  Result.AppendChild(AddNode(tcDe2, '#123e', 'vRetCSLL', 1, 15, 1,
    NF3e.NFDet[aNFDet].Det[aDet].detItemAnt.retTrib.vRetCSLL, DSC_VRETCSLL));

  Result.AppendChild(AddNode(tcDe2, '#123f', 'vBCIRRF', 1, 15, 1,
    NF3e.NFDet[aNFDet].Det[aDet].detItemAnt.retTrib.vBCIRRF, DSC_VBCIRRF));

  Result.AppendChild(AddNode(tcDe2, '#123g', 'vIRRF', 1, 15, 1,
    NF3e.NFDet[aNFDet].Det[aDet].detItemAnt.retTrib.vIRRF, DSC_VIRRF));
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem(aNFdet,
  aDet: Integer): TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('detItem');

  if NF3e.NFDet[aNFdet].Det[aDet].detItem.nItemAnt > 0 then
    Result.SetAttribute('nItemAnt', IntToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.nItemAnt));

  nodeArray := Gerar_NFdet_det_DetItem_gTarif(aNFdet, aDet);
  for i := 0 to NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  nodeArray := Gerar_NFdet_det_DetItem_gAdBand(aNFdet, aDet);
  for i := 0 to NF3e.NFDet[aNFdet].Det[aDet].detItem.gAdBand.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(Gerar_NFdet_det_DetItem_Prod(aNFdet, aDet));

  Result.AppendChild(Gerar_NFdet_det_DetItem_Imposto(aNFdet, aDet));

  Result.AppendChild(Gerar_NFdet_det_DetItem_gProcRef(aNFdet, aDet));

  nodeArray := Gerar_NFdet_det_DetItem_gContab(aNFdet, aDet);
  for i := 0 to NF3e.NFDet[aNFdet].Det[aDet].detItem.gContab.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(AddNode(tcStr, '#252', 'infAdProd', 1, 500, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.infAdProd, DSC_INFADPROD));
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_gTarif(aNFdet,
  aDet: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif.Count);

  for i := 0 to NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('gTarif');

    Result[i].AppendChild(AddNode(tcDat, '#127', 'dIniTarif', 10, 10, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif[i].dIniTarif, DSC_DINITARIF));

    Result[i].AppendChild(AddNode(tcDat, '#128', 'dFimTarif', 10, 10, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif[i].dFimTarif, DSC_DFIMTARIF));

    Result[i].AppendChild(AddNode(tcStr, '#129', 'tpAto', 1, 1, 1,
      tpAtoToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif[i].tpAto), DSC_TPATO));

    Result[i].AppendChild(AddNode(tcStr, '#130', 'nAto', 4, 4, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif[i].nAto, DSC_NATO));

    Result[i].AppendChild(AddNode(tcInt, '#131', 'anoAto', 4, 4, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif[i].anoAto, DSC_ANOATO));

    Result[i].AppendChild(AddNode(tcStr, '#132', 'tpTarif', 1, 1, 1,
      tpTarifToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif[i].tpTarif), DSC_TPTARIF));

    Result[i].AppendChild(AddNode(tcStr, '#133', 'cPosTarif', 1, 1, 1,
      cPosTarifToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif[i].cPosTarif), DSC_CPOSTARIF));

    Result[i].AppendChild(AddNode(tcStr, '#134', 'uMed', 1, 1, 1,
      uMedToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif[i].uMed), DSC_UMED));

    Result[i].AppendChild(AddNode(tcDe8, '#135', 'vTarifHom', 1, 17, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif[i].vTarifHom, DSC_VTARIFHOM));

    if NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif[i].vTarifAplic > 0 then
    begin
      Result[i].AppendChild(AddNode(tcDe8, '#136', 'vTarifAplic', 1, 17, 1,
        NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif[i].vTarifAplic, DSC_VTARIFAPLIC));

      Result[i].AppendChild(AddNode(tcStr, '#137', 'motDifTarif', 2, 2, 1,
        motDifTarifToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif[i].motDifTarif), DSC_MOTDIFTARIF));
    end;
  end;

  if NF3e.NFDet[aNFdet].Det[aDet].detItem.gTarif.Count > 6 then
    wAlerta('126', 'gTarif', '', ERR_MSG_MAIOR_MAXIMO + '6');
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_gAdBand(aNFdet,
  aDet: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NF3e.NFDet[aNFdet].Det[aDet].detItem.gAdBand.Count);

  for i := 0 to NF3e.NFDet[aNFdet].Det[aDet].detItem.gAdBand.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('gAdBand');

    Result[i].AppendChild(AddNode(tcDat, '#139', 'dIniAdBand', 10, 10, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gAdBand[i].dIniAdBand, DSC_DINITARIF));

    Result[i].AppendChild(AddNode(tcDat, '#140', 'dFimAdBand', 10, 10, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gAdBand[i].dFimAdBand, DSC_DFIMTARIF));

    Result[i].AppendChild(AddNode(tcStr, '#141', 'tpBand', 1, 1, 1,
      tpBandToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.gAdBand[i].tpBand), DSC_TPBAND));

    Result[i].AppendChild(AddNode(tcDe2, '#142', 'vAdBand', 1, 15, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gAdBand[i].vAdBand, DSC_VADBAND));

    if NF3e.NFDet[aNFdet].Det[aDet].detItem.gAdBand[i].vAdBandAplic > 0 then
    begin
      Result[i].AppendChild(AddNode(tcDe2, '#143', 'vAdBandAplic', 1, 15, 1,
        NF3e.NFDet[aNFdet].Det[aDet].detItem.gAdBand[i].vAdBandAplic, DSC_VADBANDAPLIC));

      Result[i].AppendChild(AddNode(tcStr, '#144', 'motDifBand', 2, 2, 1,
        motDifBandToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.gAdBand[i].motDifBand), DSC_MOTDIFBAND));
    end;
  end;

  if NF3e.NFDet[aNFdet].Det[aDet].detItem.gAdBand.Count > 3 then
    wAlerta('#138', 'gAdBand', '', ERR_MSG_MAIOR_MAXIMO + '3');
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_Prod(aNFdet,
  aDet: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('prod');

  Result.AppendChild(AddNode(tcStr, '#146', 'indOrigemQtd', 1, 1, 1,
    indOrigemQtdToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.indOrigemQtd), DSC_INDORIGEMQTD));

  if NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.nMed > 0 then
    Result.AppendChild(Gerar_NFdet_det_DetItem_Prod_gMedicao(aNFdet, aDet));

  Result.AppendChild(AddNode(tcStr, '#162', 'cProd', 1, 60, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.cProd, DSC_CPROD));

  Result.AppendChild(AddNode(tcStr, '#163', 'xProd', 1, 120, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.xProd, DSC_XPROD));

  Result.AppendChild(AddNode(tcInt, '#164', 'cClass', 7, 7, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.cClass, DSC_CCLASS));

  Result.AppendChild(AddNode(tcInt, '#165', 'CFOP', 4, 4, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.CFOP, DSC_CFOP));

  Result.AppendChild(AddNode(tcStr, '#166', 'uMed', 1, 1, 1,
    uMedFatToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.uMed), DSC_UMED));

  if Frac(NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.qFaturada) > 0 then
    Result.AppendChild(AddNode(tcDe4, '#167', 'qFaturada', 1, 15, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.qFaturada, DSC_QFATURADA))
  else
    Result.AppendChild(AddNode(tcInt, '#167', 'qFaturada', 1, 15, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.qFaturada, DSC_QFATURADA));

  // pode ter 2 ou 8 casas decimais
  Result.AppendChild(AddNode(tcDe2, '#168', 'vItem', 1, 15, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.vItem, DSC_VITEM));

  // pode ter 2 ou 8 casas decimais
  Result.AppendChild(AddNode(tcDe2, '#169', 'vProd', 1, 15, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.vProd, DSC_VPROD));

  if NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.indDevolucao = tiSim then
    Result.AppendChild(AddNode(tcStr, '#170', 'indDevolucao', 1, 1, 1, '1', ''));

  if NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.indPrecoACL = tiSim then
    Result.AppendChild(AddNode(tcStr, '#171', 'indPrecoACL', 1, 1, 1, '1', ''));
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_Prod_gMedicao(aNFdet,
  aDet: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gMedicao');

  Result.AppendChild(AddNode(tcInt, '#148', 'nMed', 2, 2, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.nMed, DSC_NMED));

  Result.AppendChild(AddNode(tcInt, '#149', 'nContrat', 2, 2, 0,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.nContrat, DSC_NCONTRAT));

  if (NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.vMedAnt > 0) or
     (NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.vMedAtu > 0) then
    Result.AppendChild(Gerar_NFdet_det_DetItem_Prod_gMedicao_gMedida(aNFdet, aDet))
  else
    Result.AppendChild(AddNode(tcInt, '#161', 'tpMotNaoLeitura', 1, 1, 1,
      tpMotNaoLeituraToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.tpMotNaoLeitura), DSC_TPMOTNAOLEITURA));
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_Prod_gMedicao_gMedida(aNFdet,
  aDet: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gMedida');

  Result.AppendChild(AddNode(tcStr, '#151', 'tpGrMed', 2, 2, 1,
    tpGrMedToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.tpGrMed), DSC_TPGRMED));

  Result.AppendChild(AddNode(tcStr, '#152', 'cPosTarif', 1, 1, 1,
    cPosTarifToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.cPosTarif), DSC_CPOSTARIF));

  Result.AppendChild(AddNode(tcStr, '#153', 'uMed', 1, 1, 1,
    uMedFatToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.uMed), DSC_UMED));

  Result.AppendChild(AddNode(tcDe2, '#154', 'vMedAnt', 1, 15, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.vMedAnt, DSC_VMEDANT));

  Result.AppendChild(AddNode(tcDe2, '#155', 'vMedAtu', 1, 15, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.vMedAtu, DSC_VMEDATU));

  Result.AppendChild(AddNode(tcDe2, '#156', 'vConst', 1, 15, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.vConst, DSC_VCONST));

  Result.AppendChild(AddNode(tcDe2, '#157', 'vMed', 1, 15, 1,
    NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.vMed, DSC_VMED));

  if (NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.pPerdaTran > 0) then
  begin
    Result.AppendChild(AddNode(tcDe2, '#158', 'pPerdaTran', 1, 5, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.pPerdaTran, DSC_PPERDATRAN));

    Result.AppendChild(AddNode(tcDe2, '#159', 'vMedPerdaTran', 1, 15, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.vMedPerdaTran, DSC_VMEDPERDATRAN));

    Result.AppendChild(AddNode(tcDe2, '#160', 'vMedPerdaTec', 1, 15, 0,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.Prod.gMedicao.vMedPerdaTec, DSC_VMEDPERDATEC));
  end;
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_Imposto(aNFdet,
  aDet: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('imposto');

  if NF3e.NFDet[aNFdet].Det[aDet].detItem.Imposto.ICMS.indSemCST = tiSim then
    Result.AppendChild(AddNode(tcStr, '#175', 'indSemCST', 1, 1, 1, '1', ''))
  else
    Result.AppendChild(Gerar_NFdet_det_DetItem_Imposto_ICMS(aNFdet, aDet));

  Result.AppendChild(Gerar_NFdet_det_DetItem_Imposto_PIS(aNFdet, aDet));
  Result.AppendChild(Gerar_NFdet_det_DetItem_Imposto_PISEfet(aNFdet, aDet));
  Result.AppendChild(Gerar_NFdet_det_DetItem_Imposto_COFINS(aNFdet, aDet));
  Result.AppendChild(Gerar_NFdet_det_DetItem_Imposto_COFINSEfet(aNFdet, aDet));
  Result.AppendChild(Gerar_NFdet_det_DetItem_Imposto_RetTrib(aNFdet, aDet));
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_Imposto_ICMS(aNFdet,
  aDet: Integer): TACBrXmlNode;

  function BuscaTag(const t: TCSTIcms): String;
  begin
    case t of
      cst00: result := '00';
      cst10: result := '10';
      cst20: result := '20';
      cst40,
      cst41: result := '40';
      cst51: result := '51';
      cst60: result := '60';
      cst90: result := '90';
    end;
  end;

var
  sTagTemp: String;

begin
  with NF3e.NFDet[aNFdet].Det[aDet].detItem.Imposto.ICMS do
  begin
    sTagTemp := BuscaTag(CST);

    Result := FDocument.CreateElement('ICMS' + sTagTemp);

    Result.AppendChild(AddNode(tcStr, '#174', 'CST', 2, 2, 1,
                                                   CSTICMSTOStr(CST), DSC_CST));

    case CST of
      cst00:
        begin
          Result.AppendChild(AddNode(tcDe2, '#175', 'vBC', 1, 15, 1,
                                                                 vBC, DSC_VBC));

          Result.AppendChild(AddNode(tcDe2, '#176', 'pICMS', 1, 5, 1,
                                                             pICMS, DSC_PICMS));

          Result.AppendChild(AddNode(tcDe2, '#177', 'vICMS', 1, 15, 1,
                                                             vICMS, DSC_VICMS));

          if (pFCP > 0) or (vFCP > 0) then
          begin
            Result.AppendChild(AddNode(tcDe2, '#178', 'pFCP', 1, 5, 1,
                                                               pFCP, DSC_PFCP));

            Result.AppendChild(AddNode(tcDe2, '#179', 'vFCP', 1, 15, 1,
                                                               vFCP, DSC_VFCP));
          end;
        end;

      cst10:
        begin
          Result.AppendChild(AddNode(tcDe2, '#182', 'vBCST', 1, 15, 1,
                                                             vBCST, DSC_VBCST));

          Result.AppendChild(AddNode(tcDe2, '#183', 'pICMSST', 1, 5, 1,
                                                         pICMSST, DSC_PICMSST));

          Result.AppendChild(AddNode(tcDe2, '#184', 'vICMSST', 1, 15, 1,
                                                         vICMSST, DSC_VICMSST));

          if (pFCP > 0) or (vFCP > 0) then
          begin
            Result.AppendChild(AddNode(tcDe2, '#185', 'pFCPST', 1, 5, 1,
                                                           pFCPST, DSC_PFCPST));

            Result.AppendChild(AddNode(tcDe2, '#186', 'vFCPST', 1, 15, 1,
                                                           vFCPST, DSC_VFCPST));
          end;
        end;

      cst20:
        begin
          Result.AppendChild(AddNode(tcDe2, '#189', 'pRedBC', 1, 5, 1,
                                                           pRedBC, DSC_PREDBC));

          Result.AppendChild(AddNode(tcDe2, '#190', 'vBC', 1, 15, 1,
                                                                 vBC, DSC_VBC));

          Result.AppendChild(AddNode(tcDe2, '#191', 'pICMS', 1, 5, 1,
                                                             pICMS, DSC_PICMS));

          Result.AppendChild(AddNode(tcDe2, '#192', 'vICMS', 1, 15, 1,
                                                             vICMS, DSC_VICMS));

          if vICMSDeson > 0 then
          begin
            Result.AppendChild(AddNode(tcDe2, '#193', 'vICMSDeson', 1, 15, 1,
                                                   vICMSDeson, DSC_VICMSDESON));

            Result.AppendChild(AddNode(tcStr, '#194', 'cBenef', 10, 10, 1,
                                                           cBenef, DSC_CBENEF));
          end;

          if (pFCP > 0) or (vFCP > 0) then
          begin
            Result.AppendChild(AddNode(tcDe2, '#195', 'pFCP', 1, 5, 1,
                                                               pFCP, DSC_PFCP));

            Result.AppendChild(AddNode(tcDe2, '#196', 'vFCP', 1, 15, 1,
                                                               vFCP, DSC_VFCP));
          end;
        end;

      cst40,
      cst41:
        begin
          if vICMSDeson > 0 then
          begin
            Result.AppendChild(AddNode(tcDe2, '#199', 'vICMSDeson', 1, 15, 1,
                                                   vICMSDeson, DSC_VICMSDESON));

            Result.AppendChild(AddNode(tcStr, '#200', 'cBenef', 10, 10, 1,
                                                           cBenef, DSC_CBENEF));
          end;
        end;

      cst51:
        begin
          if vICMSDeson > 0 then
          begin
            Result.AppendChild(AddNode(tcDe2, '#203', 'vICMSDeson', 1, 15, 1,
                                                   vICMSDeson, DSC_VICMSDESON));

            Result.AppendChild(AddNode(tcStr, '#204', 'cBenef', 10, 10, 1,
                                                           cBenef, DSC_CBENEF));
          end;
        end;

      cst60:
        begin
          if (vBCSTRET > 0) or (vICMSSTRET > 0) then
          begin
            Result.AppendChild(AddNode(tcDe2, '246', 'vBCSTRet', 1, 15, 1,
                                                       vBCSTRET, DSC_VBCSTRET));

            Result.AppendChild(AddNode(tcDe2, '247', 'pICMSSTRet', 1, 5, 1,
                                                   vICMSSTRET, DSC_PICMSSTRET));

            Result.AppendChild(AddNode(tcDe2, '248', 'vICMSSubstituto', 1, 15, 0,
                                                         vICMSST, DSC_VICMSST));

            Result.AppendChild(AddNode(tcDe2, '249', 'vICMSSTRet', 1, 15, 1,
                                                   vICMSSTRET, DSC_VICMSSTRET));
          end;

          if (vBCFCPSTRet > 0) or (pFCPSTRet > 0) or (vFCPSTRet > 0) then
          begin
            Result.AppendChild(AddNode(tcDe2, '251', 'vBCFCPSTRet', 1, 15, 1,
                                                    vBCFCPSTRet, DSC_VBCFCPST));

            Result.AppendChild(AddNode(tcDe2, '252', 'pFCPSTRet', 1, 5, 1,
                                                     pFCPSTRet, DSC_PFCPSTRET));

            Result.AppendChild(AddNode(tcDe2, '253', 'vFCPSTRet', 1, 15, 1,
                                                     vFCPSTRet, DSC_VFCPSTRET));
          end;

          if (pRedBCEfet > 0) or (vBCEfet > 0) or (pICMSEfet > 0) or
             (vICMSEfet > 0) then
          begin
            Result.AppendChild(AddNode(tcDe2, '254', 'pRedBCEfet', 1, 5, 1,
                                                   pRedBCEfet, DSC_PREDBCEFET));

            Result.AppendChild(AddNode(tcDe2, '255', 'vBCEfet', 1, 15, 1,
                                                         vBCEfet, DSC_VBCEFET));

            Result.AppendChild(AddNode(tcDe2, '256', 'pICMSEfet', 1, 5, 1,
                                                     pICMSEfet, DSC_PICMSEFET));

            Result.AppendChild(AddNode(tcDe2, '257', 'vICMSEfet', 1, 15, 1,
                                                     vICMSEfet, DSC_VICMSEFET));
          end;

          if vICMSDeson > 0 then
          begin
            Result.AppendChild(AddNode(tcDe2, '#258', 'vICMSDeson', 1, 15, 1,
                                                   vICMSDeson, DSC_VICMSDESON));

            Result.AppendChild(AddNode(tcStr, '#259', 'cBenef', 10, 10, 1,
                                                           cBenef, DSC_CBENEF));
          end;
        end;

      cst90:
        begin
          if vBC > 0 then
          begin
            Result.AppendChild(AddNode(tcDe2, '#207', 'vBC', 1, 15, 1,
                                                                 vBC, DSC_VBC));

            Result.AppendChild(AddNode(tcDe2, '#208', 'pICMS', 1, 5, 1,
                                                             pICMS, DSC_PICMS));

            Result.AppendChild(AddNode(tcDe2, '#209', 'vICMS', 1, 15, 1,
                                                             vICMS, DSC_VICMS));
          end;

          if vICMSDeson > 0 then
          begin
            Result.AppendChild(AddNode(tcDe2, '#210', 'vICMSDeson', 1, 15, 1,
                                                   vICMSDeson, DSC_VICMSDESON));

            Result.AppendChild(AddNode(tcStr, '#211', 'cBenef', 10, 10, 1,
                                                           cBenef, DSC_CBENEF));
          end;

          if (pFCP > 0) or (vFCP > 0) then
          begin
            Result.AppendChild(AddNode(tcDe2, '#195', 'pFCP', 1, 5, 1,
                                                               pFCP, DSC_PFCP));

            Result.AppendChild(AddNode(tcDe2, '#196', 'vFCP', 1, 15, 1,
                                                               vFCP, DSC_VFCP));
          end;
        end;
    end;
  end;
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_Imposto_PIS(aNFdet,
  aDet: Integer): TACBrXmlNode;
begin
  Result := nil;

  with NF3e.NFDet[aNFdet].Det[aDet].detItem.Imposto.PIS do
  begin
    if (vBC > 0) or (pPIS > 0) or (vPIS > 0) then
    begin
      Result := FDocument.CreateElement('PIS');

      Result.AppendChild(AddNode(tcStr, '#211', 'CST', 2, 2, 1, CSTPISToStr(CST), DSC_CST));

      Result.AppendChild(AddNode(tcDe2, '#212', 'vBC', 1, 15, 1, vBC, DSC_VBC));

      Result.AppendChild(AddNode(tcDe2, '#213', 'pPIS', 1, 5, 1, pPIS, DSC_PPIS));

      Result.AppendChild(AddNode(tcDe2, '#214', 'vPIS', 1, 15, 1, vPIS, DSC_VPIS));
    end;
  end;
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_Imposto_PISEfet(aNFdet,
  aDet: Integer): TACBrXmlNode;
begin
  Result := nil;

  with NF3e.NFDet[aNFdet].Det[aDet].detItem.Imposto.PISEfet do
  begin
    if (vBCPISEfet > 0) or (pPISEfet > 0) or (vPISEfet > 0) then
    begin
      Result := FDocument.CreateElement('PISEfet');

      Result.AppendChild(AddNode(tcDe2, '#216', 'vBCPISEfet', 1, 15, 1, vBCPISEfet, DSC_VBC));

      Result.AppendChild(AddNode(tcDe2, '#217', 'pPISEfet', 1, 5, 1, pPISEfet, DSC_PPIS));

      Result.AppendChild(AddNode(tcDe2, '#218', 'vPISEfet', 1, 15, 1, vPISEfet, DSC_VPIS));
    end;
  end;
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_Imposto_COFINS(aNFdet,
  aDet: Integer): TACBrXmlNode;
begin
  Result := nil;

  with NF3e.NFDet[aNFdet].Det[aDet].detItem.Imposto.COFINS do
  begin
    if (vBC > 0) or (pCOFINS > 0) or (vCOFINS > 0) then
    begin
      Result := FDocument.CreateElement('COFINS');

      Result.AppendChild(AddNode(tcStr, '#220', 'CST', 2, 2, 1, CSTCOFINSToStr(CST), DSC_CST));

      Result.AppendChild(AddNode(tcDe2, '#221', 'vBC', 1, 15, 1, vBC, DSC_VBC));

      Result.AppendChild(AddNode(tcDe2, '#222', 'pCOFINS', 1, 5, 1, pCOFINS, DSC_PCOFINS));

      Result.AppendChild(AddNode(tcDe2, '#223', 'vCOFINS', 1, 15, 1, vCOFINS, DSC_VCOFINS));
    end;
  end;
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_Imposto_COFINSEfet(aNFdet,
  aDet: Integer): TACBrXmlNode;
begin
  Result := nil;

  with NF3e.NFDet[aNFdet].Det[aDet].detItem.Imposto.COFINSEfet do
  begin
    if (vBCCOFINSEfet > 0) or (pCOFINSEfet > 0) or (vCOFINSEfet > 0) then
    begin
      Result := FDocument.CreateElement('COFINSEfet');

      Result.AppendChild(AddNode(tcDe2, '#225', 'vBCCOFINSEfet', 1, 15, 1, vBCCOFINSEfet, DSC_VBC));

      Result.AppendChild(AddNode(tcDe2, '#226', 'pCOFINSEfet', 1, 5, 1, pCOFINSEfet, DSC_PCOFINS));

      Result.AppendChild(AddNode(tcDe2, '#227', 'vCOFINSEfet', 1, 15, 1, vCOFINSEfet, DSC_VCOFINS));
    end;
  end;
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_Imposto_RetTrib(aNFdet,
  aDet: Integer): TACBrXmlNode;
begin
  Result := nil;

  with NF3e.NFDet[aNFdet].Det[aDet].detItem.Imposto.retTrib do
  begin
    if (vRetPIS > 0) or (vRetCOFINS > 0) or (vRetCSLL > 0) or (vBCIRRF > 0) or
       (vIRRF > 0) then
    begin
      Result := FDocument.CreateElement('retTrib');

      Result.AppendChild(AddNode(tcDe2, '#229', 'vRetPIS', 1, 15, 1, vRetPIS, DSC_VRETPIS));

      Result.AppendChild(AddNode(tcDe2, '#230', 'vRetCOFINS', 1, 15, 1, vRetCOFINS, DSC_VRETCOFINS));

      Result.AppendChild(AddNode(tcDe2, '#231', 'vRetCSLL', 1, 15, 1, vRetCSLL, DSC_VRETCSLL));

      Result.AppendChild(AddNode(tcDe2, '#232', 'vBCIRRF', 1, 15, 1, vBCIRRF, DSC_VBCIRRF));

      Result.AppendChild(AddNode(tcDe2, '#233', 'vIRRF', 1, 15, 1, vIRRF, DSC_VIRRF));
    end;
  end;
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_gProcRef(aNFdet,
  aDet: Integer): TACBrXmlNode;
var
  i: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  if NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vItem > 0 then
  begin
    Result := FDocument.CreateElement('gProcRef');

    // pode ter 2 ou 6 casas decimais
    Result.AppendChild(AddNode(tcDe2, '#235', 'vItem', 1, 15, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vItem, DSC_VITEM));

    if Frac(NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.qFaturada) > 0 then
      Result.AppendChild(AddNode(tcDe4, '#236', 'qFaturada', 1, 15, 1,
        NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.qFaturada, DSC_QFATURADA))
    else
      Result.AppendChild(AddNode(tcInt, '#236', 'qFaturada', 1, 15, 1,
        NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.qFaturada, DSC_QFATURADA));

    // pode ter 2 ou 6 casas decimais
    Result.AppendChild(AddNode(tcDe2, '#237', 'vProd', 1, 15, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vProd, DSC_VPROD));

    if NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.indDevolucao = tiSim then
      Result.AppendChild(AddNode(tcStr, '#238', 'indDevolucao', 1, 1, 1, '1', ''));

    Result.AppendChild(AddNode(tcDe2, '#239', 'vBC', 1, 15, 0,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vBC, DSC_VBC));

    Result.AppendChild(AddNode(tcDe2, '#240', 'pICMS', 1, 5, 0,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.pICMS, DSC_PICMS));

    Result.AppendChild(AddNode(tcDe2, '#241', 'vICMS', 1, 15, 0,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vICMS, DSC_VICMS));

    if NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.pFCP > 0 then
    begin
      Result.AppendChild(AddNode(tcDe2, '#241a', 'pFCP', 1, 5, 1,
        NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.pFCP, DSC_PFCP));

      Result.AppendChild(AddNode(tcDe2, '#241b', 'vFCP', 1, 15, 1,
        NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vFCP, DSC_VFCP));
    end;

    if NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vBCST > 0 then
    begin
      Result.AppendChild(AddNode(tcDe2, '#241c', 'vBCST', 1, 15, 1,
        NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vBCST, DSC_VBCST));

      Result.AppendChild(AddNode(tcDe2, '#241d', 'pICMSST', 1, 5, 1,
        NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.pICMSST, DSC_PICMSST));

      Result.AppendChild(AddNode(tcDe2, '#241e', 'vICMSST', 1, 15, 1,
        NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vICMSST, DSC_VICMSST));
    end;

    if NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.pFCP > 0 then
    begin
      Result.AppendChild(AddNode(tcDe2, '#241f', 'pFCPST', 1, 5, 1,
        NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.pFCPST, DSC_PFCPST));

      Result.AppendChild(AddNode(tcDe2, '#241g', 'vFCPST', 1, 15, 1,
        NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vFCPST, DSC_VFCPST));
    end;

    Result.AppendChild(AddNode(tcDe2, '#242', 'vPIS', 1, 15, 0,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vPIS, DSC_VPIS));

    Result.AppendChild(AddNode(tcDe2, '#242a', 'vPISEfet', 1, 15, 0,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vPISEfet, DSC_VPISEfet));

    Result.AppendChild(AddNode(tcDe2, '#243', 'vCOFINS', 1, 15, 0,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vCOFINS, DSC_VCOFINS));

    Result.AppendChild(AddNode(tcDe2, '#243a', 'vCOFINSEfet', 1, 15, 0,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.vCOFINSEfet, DSC_VCOFINSEfet));

    nodeArray := Gerar_NFdet_det_DetItem_gProcRef_gProc(aNFdet, aDet);
    for i := 0 to NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.gProc.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_gProcRef_gProc(aNFdet,
  aDet: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.gProc.Count);

  for i := 0 to NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.gProc.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('gProc');

    Result[i].AppendChild(AddNode(tcStr, '#245', 'tpProc', 1, 1, 1,
      tpProcToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.gProc[i].tpProc), DSC_TPPROC));

    Result[i].AppendChild(AddNode(tcStr, '#246', 'nProcesso', 1, 60, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.gProc[i].nProcesso, DSC_NPROCESSO));
  end;

  if NF3e.NFDet[aNFdet].Det[aDet].detItem.gProcRef.gProc.Count > 10 then
    wAlerta('#244', 'gProc', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNF3eXmlWriter.Gerar_NFdet_det_DetItem_gContab(aNFdet,
  aDet: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NF3e.NFDet[aNFdet].Det[aDet].detItem.gContab.Count);

  for i := 0 to NF3e.NFDet[aNFdet].Det[aDet].detItem.gContab.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('gContab');

    Result[i].AppendChild(AddNode(tcStr, '#248', 'cContab', 9, 13, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gContab[i].cContab, DSC_CCONTAB));

    Result[i].AppendChild(AddNode(tcStr, '#249', 'xContab', 1, 100, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gContab[i].xContab, DSC_XCONTAB));

    Result[i].AppendChild(AddNode(tcDe2, '#250', 'vContab', 1, 15, 1,
      NF3e.NFDet[aNFdet].Det[aDet].detItem.gContab[i].vContab, DSC_VCONTAB));

    Result[i].AppendChild(AddNode(tcStr, '#251', 'tpLanc', 1, 1, 1,
      tpLancToStr(NF3e.NFDet[aNFdet].Det[aDet].detItem.gContab[i].tpLanc), DSC_TPLANC));
  end;

  if NF3e.NFDet[aNFdet].Det[aDet].detItem.gContab.Count > 99 then
    wAlerta('#247', 'gContab', '', ERR_MSG_MAIOR_MAXIMO + '99');
end;

function TNF3eXmlWriter.Gerar_Total: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('total');

  Result.AppendChild(AddNode(tcDe2, '#254', 'vProd', 1, 15, 1,
    NF3e.Total.vProd, DSC_VPROD));

  Result.AppendChild(Gerar_TotalICMSTotal);
  Result.AppendChild(Gerar_TotalretTrib);

  Result.AppendChild(AddNode(tcDe2, '#268', 'vCOFINS', 1, 15, 1,
    NF3e.Total.vCOFINS, DSC_VCOFINS));

  Result.AppendChild(AddNode(tcDe2, '#269', 'vCOFINSEfet', 1, 15, 1,
    NF3e.Total.vCOFINSEfet, DSC_VCOFINSEfet));

  Result.AppendChild(AddNode(tcDe2, '#270', 'vPIS', 1, 15, 1,
    NF3e.Total.vPIS, DSC_VPIS));

  Result.AppendChild(AddNode(tcDe2, '#271', 'vPISEfet', 1, 15, 1,
    NF3e.Total.vPISEfet, DSC_VPISEfet));

  Result.AppendChild(AddNode(tcDe2, '#271', 'vNF', 1, 15, 1,
    NF3e.Total.vNF, DSC_VDF));
end;

function TNF3eXmlWriter.Gerar_TotalICMSTotal: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMSTot');

  Result.AppendChild(AddNode(tcDe2, '#256', 'vBC', 1, 15, 1,
    NF3e.Total.vBC, DSC_VBC));

  Result.AppendChild(AddNode(tcDe2, '#257', 'vICMS', 1, 15, 1,
    NF3e.Total.vICMS, DSC_VICMS));

  Result.AppendChild(AddNode(tcDe2, '#258', 'vICMSDeson', 1, 15, 1,
    NF3e.Total.vICMSDeson, DSC_VICMSDESON));

  Result.AppendChild(AddNode(tcDe2, '#259', 'vFCP', 1, 15, 1,
    NF3e.Total.vFCP, DSC_VFCP));

  Result.AppendChild(AddNode(tcDe2, '#260', 'vBCST', 1, 15, 1,
    NF3e.Total.vBCST, DSC_VBCST));

  Result.AppendChild(AddNode(tcDe2, '#261', 'vST', 1, 15, 1,
    NF3e.Total.vST, DSC_VST));

  Result.AppendChild(AddNode(tcDe2, '#262', 'vFCPST', 1, 15, 1,
    NF3e.Total.vFCPST, DSC_VFCPST));
end;

function TNF3eXmlWriter.Gerar_TotalretTrib: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('vRetTribTot');

  Result.AppendChild(AddNode(tcDe2, '#264', 'vRetPIS', 1, 15, 1,
    NF3e.Total.vRetPIS, DSC_VRETPIS));

  Result.AppendChild(AddNode(tcDe2, '#265', 'vRetCofins', 1, 15, 1,
    NF3e.Total.vRetCofins, DSC_VRETCOFINS));

  Result.AppendChild(AddNode(tcDe2, '#266', 'vRetCSLL', 1, 15, 1,
    NF3e.Total.vRetCSLL, DSC_VRETCSLL));

  Result.AppendChild(AddNode(tcDe2, '#267', 'vIRRF', 1, 15, 1,
    NF3e.Total.vIRRF, DSC_VIRRF));
end;

function TNF3eXmlWriter.Gerar_gFat: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gFat');

  Result.AppendChild(AddNode(tcStr, '#274', 'CompetFat', 6, 6, 1,
    FormatDateTime('yyyymm', NF3e.gFat.CompetFat), DSC_COMPETFAT));

  Result.AppendChild(AddNode(tcDat, '#275', 'dVencFat', 10, 10, 1,
    NF3e.gFat.dVencFat, DSC_DVENC));

  Result.AppendChild(AddNode(tcDat, '#276', 'dApresFat', 10, 10, 0,
    NF3e.gFat.dApresFat, DSC_DAPRESFAT));

  Result.AppendChild(AddNode(tcDat, '#277', 'dProxLeitura', 10, 10, 1,
    NF3e.gFat.dProxLeitura, DSC_DPROXLEITURA));

  Result.AppendChild(AddNode(tcStr, '#278', 'nFat', 1, 20, 0,
    NF3e.gFat.nFat, DSC_NFAT));

  Result.AppendChild(AddNode(tcStr, '#279', 'codBarras', 1, 48, 1,
    NF3e.gFat.codBarras, DSC_CODBARRAS));

  if NF3e.gFat.codDebAuto <> '' then
    Result.AppendChild(AddNode(tcStr, '#280', 'codDebAuto', 1, 20, 1,
      NF3e.gFat.codDebAuto, DSC_CODDEBAUTO))
  else
  begin
    Result.AppendChild(AddNode(tcStr, '#281', 'codBanco', 3, 5, 1,
      NF3e.gFat.codBanco, DSC_CODBANCO));

    Result.AppendChild(AddNode(tcStr, '#282', 'codAgencia', 1, 10, 1,
      NF3e.gFat.codAgencia, DSC_CODAGENCIA));
  end;

  if NF3e.gFat.enderCorresp.xLgr <> '' then
    Result.AppendChild(Gerar_gFatEnderCorresp);

  if NF3e.gFat.gPIX.urlQRCodePIX <> '' then
    Result.AppendChild(Gerar_gFatgPix);
end;

function TNF3eXmlWriter.Gerar_gFatEnderCorresp: TACBrXmlNode;
var
  cMun: integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, NF3e.gFat.enderCorresp.UF,
    NF3e.gFat.enderCorresp.xMun, NF3e.gFat.enderCorresp.cMun);

  Result := FDocument.CreateElement('enderCorresp');

  Result.AppendChild(AddNode(tcStr, '#284', 'xLgr', 2, 60, 1,
    NF3e.gFat.enderCorresp.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#285', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, NF3e.gFat.enderCorresp.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#286', 'xCpl', 1, 60, 0,
    NF3e.gFat.enderCorresp.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#287', 'xBairro', 2, 60, 1,
    NF3e.gFat.enderCorresp.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, '#288', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('#288', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#289', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, '#290', 'CEP', 8, 8, 1,
    NF3e.gFat.enderCorresp.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, '#291', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('#291', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#292', 'fone', 7, 12, 0,
    OnlyNumber(NF3e.gFat.enderCorresp.fone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, '#293', 'email', 1, 60, 0,
    NF3e.gFat.enderCorresp.email, DSC_EMAIL));
end;

function TNF3eXmlWriter.Gerar_gFatgPix: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gPIX');

  Result.AppendChild(AddNode(tcStr, '#295', 'urlQRCodePIX', 2, 2000, 1,
    NF3e.gFat.gPIX.urlQRCodePIX, DSC_XLGR));
end;

function TNF3eXmlWriter.Gerar_gANEEL: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := FDocument.CreateElement('gANEEL');

  nodeArray := Gerar_gANEEL_gHistFat;
  for i := 0 to NF3e.gANEEL.gHistFat.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;
end;

function TNF3eXmlWriter.Gerar_gANEEL_gHistFat: TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, NF3e.gANEEL.gHistFat.Count);

  for i := 0 to NF3e.gANEEL.gHistFat.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('gHistFat');

    Result[i].AppendChild(AddNode(tcStr, '#298', 'xGrandFat', 2, 60, 1,
      NF3e.gANEEL.gHistFat[i].xGrandFat, DSC_XGRANDFAT));

    nodeArray := Gerar_gANEEL_gHistFat_gGrandFat(i);
    for j := 0 to NF3e.gANEEL.gHistFat[i].gGrandFat.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if NF3e.gANEEL.gHistFat.Count > 5 then
    wAlerta('#273', 'gHistFat', '', ERR_MSG_MAIOR_MAXIMO + '5');
end;

function TNF3eXmlWriter.Gerar_gANEEL_gHistFat_gGrandFat(
  agHistFat: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NF3e.gANEEL.gHistFat[agHistFat].gGrandFat.Count);

  for i := 0 to NF3e.gANEEL.gHistFat[agHistFat].gGrandFat.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('gGrandFat');

    Result[i].AppendChild(AddNode(tcStr, '#300', 'CompetFat', 6, 6, 1,
      FormatDateTime('yyyymm', NF3e.gANEEL.gHistFat[agHistFat].gGrandFat[i].CompetFat), DSC_COMPETFAT));

    Result[i].AppendChild(AddNode(tcDe2, '#301', 'vFat', 1, 15, 1,
      NF3e.gANEEL.gHistFat[agHistFat].gGrandFat[i].vFat, DSC_VFAT));

    Result[i].AppendChild(AddNode(tcStr, '#302', 'uMed', 1, 1, 1,
      uMedFatToStr(NF3e.gANEEL.gHistFat[agHistFat].gGrandFat[i].uMed), DSC_UMED));

    Result[i].AppendChild(AddNode(tcInt, '#303', 'qtdDias', 2, 2, 1,
      NF3e.gANEEL.gHistFat[agHistFat].gGrandFat[i].qtdDias, DSC_QTDE));
  end;

  if NF3e.gANEEL.gHistFat[agHistFat].gGrandFat.Count > 13 then
    wAlerta('#275', 'gGrandFat', '', ERR_MSG_MAIOR_MAXIMO + '13');
end;

function TNF3eXmlWriter.Gerar_autXML: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NF3e.autXML.Count);

  for i := 0 to NF3e.autXML.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('autXML');

    Result[i].AppendChild(AddNodeCNPJCPF('#305', '#306', NF3e.autXML[i].CNPJCPF));
  end;

  if NF3e.autXML.Count > 10 then
    wAlerta('#304', 'autXML', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNF3eXmlWriter.Gerar_InfAdic: TACBrXmlNode;
begin
  Result := nil;

  if (trim(NF3e.InfAdic.infAdFisco) <> '') or (trim(NF3e.InfAdic.infCpl) <> '') then
  begin
    Result := FDocument.CreateElement('infAdic');

    Result.AppendChild(AddNode(tcStr, '#308', 'infAdFisco', 1, 2000, 0,
      NF3e.InfAdic.infAdFisco, DSC_INFADFISCO));

    Result.AppendChild(AddNode(tcStr, '#309', 'infCpl', 1, 5000, 0,
      NF3e.InfAdic.infCpl, DSC_INFCPL));
  end;
end;

function TNF3eXmlWriter.Gerar_gRespTec: TACBrXmlNode;
begin
  Result := nil;

  if (NF3e.infRespTec.CNPJ <> '') then
  begin
    Result := FDocument.CreateElement('gRespTec');

    Result.AppendChild(AddNodeCNPJ('#311', NF3e.infRespTec.CNPJ, CODIGO_BRASIL, True));

    Result.AppendChild(AddNode(tcStr, '#312', 'xContato', 2, 60, 1,
      NF3e.infRespTec.xContato, DSC_XCONTATO));

    Result.AppendChild(AddNode(tcStr, '#313', 'email', 6, 60, 1,
      NF3e.infRespTec.email, DSC_EMAIL));

    Result.AppendChild(AddNode(tcStr, '#314', 'fone', 7, 12, 1,
      NF3e.infRespTec.fone, DSC_FONE));

    if (idCSRT <> 0) and (CSRT <> '') then
    begin
      Result.AppendChild(AddNode(tcInt, '#315', 'idCSRT', 2, 2, 1,
         idCSRT, DSC_IDCSRT));

      Result.AppendChild(AddNode(tcStr, '#316', 'hashCSRT', 28, 28, 1,
        CalcularHashCSRT(CSRT, FChaveNF3e), DSC_HASHCSRT));
    end;
  end;
end;

function TNF3eXmlWriter.Gerar_ProtNF3e: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('protNF3e');

  Result.SetAttribute('versao', FloatToString(NF3e.infNF3e.Versao, '.', '#0.00'));

  xmlNode := Result.AddChild('infProt');

  xmlNode.AddChild('tpAmb').Content := TipoAmbienteToStr(NF3e.procNF3e.tpAmb);

  xmlNode.AddChild('verAplic').Content := NF3e.procNF3e.verAplic;

  xmlNode.AddChild('chNF3e').Content := NF3e.procNF3e.chDFe;

  xmlNode.AddChild('dhRecbto').Content :=
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', NF3e.procNF3e.dhRecbto) +
    GetUTC(CodigoUFparaUF(FNF3e.Ide.cUF), NF3e.procNF3e.dhRecbto);

  xmlNode.AddChild('nProt').Content := NF3e.procNF3e.nProt;

  xmlNode.AddChild('digVal').Content := NF3e.procNF3e.digVal;

  xmlNode.AddChild('cStat').Content := IntToStr(NF3e.procNF3e.cStat);

  xmlNode.AddChild('xMotivo').Content := NF3e.procNF3e.xMotivo;
end;

end.
