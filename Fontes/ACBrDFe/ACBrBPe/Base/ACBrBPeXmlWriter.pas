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

unit ACBrBPeXmlWriter;

interface

uses
  Classes, SysUtils,
  ACBrXmlBase, ACBrXmlDocument, ACBrXmlWriter,
  ACBrBPeClass,
  ACBrBPeConversao;

type
  TBPeXmlWriterOptions = class(TACBrXmlWriterOptions)
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

  TBPeXmlWriter = class(TACBrXmlWriter)
  private
    FBPe: TBPe;

    FChaveBPe: string;

    FVersaoDF: TVersaoBPe;
    FModeloDF: TModeloBPe;
    FtpAmb: TACBrTipoAmbiente;
    FtpEmis: TACBrTipoEmissao;
    FIdCSRT: integer;
    FCSRT: string;

    function Gerar_InfBPe: TACBrXmlNode;
    function Gerar_Ide: TACBrXmlNode;

    function Gerar_Emit: TACBrXmlNode;
    function Gerar_EnderEmit: TACBrXmlNode;

    function Gerar_Comp: TACBrXmlNode;
    function Gerar_EnderComp: TACBrXmlNode;

    function Gerar_Agencia: TACBrXmlNode;
    function Gerar_EnderAgencia: TACBrXmlNode;

    function Gerar_InfBPeSub: TACBrXmlNode;
    function Gerar_InfPassagem: TACBrXmlNode;
    function Gerar_InfPassageiro: TACBrXmlNode;

    function Gerar_InfViagem: TACBrXmlNodeArray;

    function Gerar_InfValorBPe: TACBrXmlNode;
    function Gerar_CompValor: TACBrXmlNodeArray;

    function Gerar_Imp: TACBrXmlNode;
    function Gerar_ICMS: TACBrXmlNode;
    function Gerar_ICMSUFFim: TACBrXmlNode;

    function Gerar_Pag: TACBrXmlNodeArray;

    // BPe TM
    function Gerar_DetBPeTM: TACBrXmlNodeArray;
    function Gerar_Det(indice: Integer): TACBrXmlNodeArray;
    function Gerar_ImpTM(indiceI, indiceJ: Integer): TACBrXmlNode;
    function Gerar_ICMSTM(indiceI, indiceJ: Integer): TACBrXmlNode;
    function Gerar_CompValorTM(indiceI, indiceJ: Integer): TACBrXmlNodeArray;

    function Gerar_Total: TACBrXmlNode;

    function Gerar_TotalICMSTotal: TACBrXmlNode;

    function Gerar_autXML: TACBrXmlNodeArray;
    function Gerar_InfAdic: TACBrXmlNode;
    function Gerar_InfRespTec: TACBrXmlNode;
    function Gerar_ProtBPe: TACBrXmlNode;

    function GetOpcoes: TBPeXmlWriterOptions;
    procedure SetOpcoes(AValue: TBPeXmlWriterOptions);

    procedure AjustarMunicipioUF(out xUF: string; out xMun: string;
      out cMun: integer; cPais: integer; const vxUF, vxMun: string; vcMun: integer);
  protected
    function CreateOptions: TACBrXmlWriterOptions; override;

  public
    constructor Create(AOwner: TBPe); reintroduce;
    destructor Destroy; override;

    function GerarXml: boolean; override;
    function ObterNomeArquivo: string; override;

    property Opcoes: TBPeXmlWriterOptions read GetOpcoes write SetOpcoes;
    property BPe: TBPe read FBPe write FBPe;

    property VersaoDF: TVersaoBPe read FVersaoDF write FVersaoDF;
    property ModeloDF: TModeloBPe read FModeloDF write FModeloDF;
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
  ACBrBPeConsts,
  ACBrValidador,
  ACBrDFeUtil,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.Strings;

constructor TBPeXmlWriter.Create(AOwner: TBPe);
begin
  inherited Create;

  TBPeXmlWriterOptions(Opcoes).AjustarTagNro := True;
  TBPeXmlWriterOptions(Opcoes).GerarTagIPIparaNaoTributado := True;
  TBPeXmlWriterOptions(Opcoes).NormatizarMunicipios := False;
  TBPeXmlWriterOptions(Opcoes).PathArquivoMunicipios := '';
  TBPeXmlWriterOptions(Opcoes).GerarTagAssinatura := taSomenteSeAssinada;
  TBPeXmlWriterOptions(Opcoes).ValidarInscricoes := False;
  TBPeXmlWriterOptions(Opcoes).ValidarListaServicos := False;
  TBPeXmlWriterOptions(Opcoes).CamposFatObrigatorios := True;

  FBPe := AOwner;
end;

destructor TBPeXmlWriter.Destroy;
begin
  inherited Destroy;
end;

function TBPeXmlWriter.CreateOptions: TACBrXmlWriterOptions;
begin
  Result := TBPeXmlWriterOptions.Create();
end;

function TBPeXmlWriter.GetOpcoes: TBPeXmlWriterOptions;
begin
  Result := TBPeXmlWriterOptions(FOpcoes);
end;

procedure TBPeXmlWriter.SetOpcoes(AValue: TBPeXmlWriterOptions);
begin
  FOpcoes := AValue;
end;

procedure TBPeXmlWriter.AjustarMunicipioUF(out xUF: string; out xMun: string;
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

function TBPeXmlWriter.ObterNomeArquivo: string;
begin
  Result := OnlyNumber(FBPe.infBPe.ID) + '-BPe.xml';
end;

function TBPeXmlWriter.GerarXml: boolean;
var
  Gerar: boolean;
  BPeNode, xmlNode: TACBrXmlNode;
begin
  Result := False;

  ListaDeAlertas.Clear;

  {
    Os campos abaixo tem que ser os mesmos da configuração
  }
{
  BPe.Ide.modelo := StrToInt(ModeloBPeToStr(ModeloDF));
  BPe.infBPe.Versao := VersaoBPeToDbl(VersaoDF);
  BPe.Ide.tpAmb := tpAmb;
  BPe.ide.tpEmis := tpEmis;
}
  FChaveBPe := GerarChaveAcesso(BPe.ide.cUF, BPe.ide.dhEmi, BPe.emit.CNPJ,
      BPe.ide.serie, BPe.ide.nBP, StrToInt(TipoEmissaoToStr(BPe.ide.tpEmis)),
      BPe.ide.cBP, BPe.ide.modelo);

  BPe.infBPe.ID := 'BPe' + FChaveBPe;
  BPe.ide.cDV := ExtrairDigitoChaveAcesso(BPe.infBPe.ID);
  BPe.Ide.cBP := ExtrairCodigoChaveAcesso(BPe.infBPe.ID);

  FDocument.Clear();
  BPeNode := FDocument.CreateElement('BPe', 'http://www.portalfiscal.inf.br/bpe');

  if BPe.procBPe.nProt <> '' then
  begin
    xmlNode := FDocument.CreateElement('BPeProc', 'http://www.portalfiscal.inf.br/bpe');
    xmlNode.SetAttribute('versao', FloatToString(BPe.infBPe.Versao, '.', '#0.00'));
    xmlNode.AppendChild(BPeNode);
    FDocument.Root := xmlNode;
  end
  else
  begin
    FDocument.Root := BPeNode;
  end;

  xmlNode := Gerar_InfBPe();
  BPeNode.AppendChild(xmlNode);

  if BPe.infBPeSupl.qrCodBPe <> '' then
  begin
    xmlNode := BPeNode.AddChild('infBPeSupl');
    xmlNode.AppendChild(AddNode(tcStr, '#318', 'qrCodBPe', 50, 1000, 1,
           '<![CDATA[' + BPe.infBPeSupl.qrCodBPe + ']]>', DSC_QRCODDFe, False));
  end;

  if Opcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := True;
    if Opcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((BPe.signature.DigestValue <> '') and
                (BPe.signature.SignatureValue <> '') and
                (BPe.signature.X509Certificate <> ''));

    if Opcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
       Gerar := ((BPe.signature.DigestValue = '') and
                 (BPe.signature.SignatureValue = '') and
                 (BPe.signature.X509Certificate = ''));
    if Gerar then
    begin
      FBPe.signature.URI := '#BPe' + OnlyNumber(BPe.infBPe.ID);
      xmlNode := GerarSignature(FBPe.signature);
      BPeNode.AppendChild(xmlNode);
    end;
  end;

  if BPe.procBPe.nProt <> '' then
  begin
    xmlNode := Gerar_ProtBPe;
    FDocument.Root.AppendChild(xmlNode);
  end;
end;

function TBPeXmlWriter.Gerar_InfBPe: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := FDocument.CreateElement('infBPe');
  Result.SetAttribute('Id', BPe.infBPe.ID);
  Result.SetAttribute('versao', FloatToString(BPe.infBPe.Versao, '.', '#0.00'));

  Result.AppendChild(Gerar_Ide);
  Result.AppendChild(Gerar_Emit);

  if BPe.ide.tpBPe = tbBPeTM then
  begin
    nodeArray := Gerar_DetBPeTM;
    for i := 0 to BPe.detBPeTM.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;

    Result.AppendChild(Gerar_Total);
  end
  else
  begin
    if BPe.Comp.xNome <> '' then
      Result.AppendChild(Gerar_Comp);

    if BPe.Agencia.xNome <> '' then
      Result.AppendChild(Gerar_Agencia);

    if BPe.infBPeSub.chBPe <> '' then
      Result.AppendChild(Gerar_InfBPeSub);

    Result.AppendChild(Gerar_InfPassagem);

    nodeArray := Gerar_InfViagem;
    for i := 0 to BPe.InfViagem.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;

    Result.AppendChild(Gerar_InfValorBPe);
    Result.AppendChild(Gerar_Imp);

    nodeArray := Gerar_Pag;
    for i := 0 to BPe.Pag.Count - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;

  nodeArray := Gerar_autXML;
  for i := 0 to BPe.autXML.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;

  Result.AppendChild(Gerar_InfAdic);
  Result.AppendChild(Gerar_InfRespTec);
end;

function TBPeXmlWriter.Gerar_Ide: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ide');
  Result.AppendChild(AddNode(tcInt, '#5', 'cUF', 2, 2, 1, BPe.ide.cUF, DSC_CUF));

  if not ValidarCodigoUF(BPe.ide.cUF) then
    wAlerta('#5', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#6', 'tpAmb  ', 1, 1, 1,
                                  TipoAmbienteToStr(BPe.Ide.tpAmb), DSC_TPAMB));

  Result.AppendChild(AddNode(tcInt, '#7', 'mod', 2, 2, 1,
                                                      BPe.ide.modelo, DSC_MOD));

  Result.AppendChild(AddNode(tcInt, '#8', 'serie', 1, 3, 1,
                                                     BPe.ide.serie, DSC_SERIE));

  Result.AppendChild(AddNode(tcInt, '#9', 'nBP', 1, 9, 1, BPe.ide.nBP, DSC_NDF));

  Result.AppendChild(AddNode(tcInt, '#10', 'cBP', 8, 8, 1, BPe.Ide.cBP, DSC_CDF));

  Result.AppendChild(AddNode(tcInt, '#11', 'cDV', 1, 1, 1, BPe.Ide.cDV, DSC_CDV));

  Result.AppendChild(AddNode(tcStr, '#12', 'modal', 1, 1, 1,
                                   ModalBPeToStr(BPe.Ide.modal), DSC_MODALBPE));

  Result.AppendChild(AddNode(tcStr, '#13', 'dhEmi', 25, 25, 1,
    DateTimeTodh(BPe.ide.dhEmi) + GetUTC(CodigoUFparaUF(BPe.ide.cUF), BPe.ide.dhEmi),
    DSC_DEMI));

  Result.AppendChild(AddNode(tcStr, '#14', 'tpEmis', 1, 1, 1,
                                 TipoEmissaoToStr(BPe.Ide.tpEmis), DSC_TPEMIS));

  Result.AppendChild(AddNode(tcStr, '#15', 'verProc', 1, 20, 1,
                                                 BPe.Ide.verProc, DSC_VERPROC));

  Result.AppendChild(AddNode(tcStr, '#16', 'tpBPe', 1, 1, 1,
                                         tpBPeToStr(BPe.Ide.tpBPe), DSC_TPBPE));

  if BPe.ide.tpBPe = tbBPeTM then
    Result.AppendChild(AddNode(tcInt, '#17', 'CFOP', 4, 4, 1,
                                                        BPe.Ide.CFOP, DSC_CFOP))
  else
  begin
    Result.AppendChild(AddNode(tcStr, '#17', 'indPres', 1, 1, 1,
                      PresencaCompradorToStr(BPe.Ide.indPres), DSC_INDPRESBPE));

    Result.AppendChild(AddNode(tcStr, '#18', 'UFIni', 2, 2, 1,
                                                  BPe.Ide.UFIni, DSC_UFINIBPE));

    Result.AppendChild(AddNode(tcInt, '#19', 'cMunIni', 7, 7, 1,
                                              BPe.Ide.cMunIni, DSC_CMUNINIBPE));

    Result.AppendChild(AddNode(tcStr, '#20', 'UFFim', 2, 2, 1,
                                                  BPe.Ide.UFFim, DSC_UFFIMBPE));

    Result.AppendChild(AddNode(tcInt, '#21', 'cMunFim', 7, 7, 1,
                                              BPe.Ide.cMunFim, DSC_CMUNFIMBPE));
  end;

  if (BPe.Ide.dhCont > 0) or (BPe.Ide.xJust <> '') then
  begin
    Result.AppendChild(AddNode(tcStr, '#22', 'dhCont', 25, 25, 1,
      DateTimeTodh(BPe.ide.dhCont) + GetUTC(CodigoUFparaUF(BPe.ide.cUF),
      BPe.ide.dhCont), DSC_DHCONT));

    Result.AppendChild(AddNode(tcStr, '#23', 'xJust', 15, 256, 1,
                                                 BPe.ide.xJust, DSC_XJUSTCONT));
  end;
end;

function TBPeXmlWriter.Gerar_Emit: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('emit');

  Result.AppendChild(AddNode(tcStr, '#25', 'CNPJ', 14, 14, 1,
                                                      BPe.Emit.CNPJ, DSC_CNPJ));

  if BPe.Emit.IE = 'ISENTO' then
    Result.AppendChild(AddNode(tcStr, '#26', 'IE', 2, 14, 1, BPe.Emit.IE, DSC_IE))
  else
    Result.AppendChild(AddNode(tcStr, '#26', 'IE', 2, 14, 1,
                                              OnlyNumber(BPe.Emit.IE), DSC_IE));

  if (Opcoes.ValidarInscricoes) then
  begin
    if Length(BPe.Emit.IE) = 0 then
      wAlerta('#26', 'IE', DSC_IE, ERR_MSG_VAZIO)
    else
    begin
      if not ValidarIE(BPe.Emit.IE, CodigoUFparaUF(BPe.Ide.cUF)) then
        wAlerta('#26', 'IE', DSC_IE, ERR_MSG_INVALIDO);
    end;
  end;

  Result.AppendChild(AddNode(tcStr, '#27', 'IEST', 2, 14, 0,
                                                      BPe.Emit.IEST, DSC_IEST));

  Result.AppendChild(AddNode(tcStr, '#22', 'xNome', 2, 60, 1,
                                                    BPe.Emit.xNome, DSC_XNOME));

  Result.AppendChild(AddNode(tcStr, '#23', 'xFant', 1, 60, 0,
                                                    BPe.Emit.xFant, DSC_XFANT));

  Result.AppendChild(AddNode(tcStr, '#24', 'IM', 1, 15, 1,
                                                          BPe.Emit.IM, DSC_IM));

  Result.AppendChild(AddNode(tcStr, '#25', 'CNAE', 7, 7, 1,
                                                      BPe.Emit.CNAE, DSC_CNAE));

  Result.AppendChild(AddNode(tcStr, '#26', 'CRT', 1, 1, 1,
                                           CRTToStr(BPe.Emit.CRT), DSC_CRTBPE));

  Result.AppendChild(Gerar_EnderEmit);

  Result.AppendChild(AddNode(tcStr, '#44', 'TAR', 1, 20, 0,
                                                        BPe.Emit.TAR, DSC_TAR));
end;

function TBPeXmlWriter.Gerar_EnderEmit: TACBrXmlNode;
var
  cMun: integer;
  xMun, xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, BPe.Emit.enderEmit.UF,
    BPe.Emit.enderEmit.xMun, BPe.Emit.EnderEmit.cMun);

  Result := FDocument.CreateElement('enderEmit');

  Result.AppendChild(AddNode(tcStr, '#34', 'xLgr', 2, 60, 1,
                                            BPe.Emit.enderEmit.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#35', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, BPe.Emit.enderEmit.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#36', 'xCpl', 1, 60, 0,
                                            BPe.Emit.enderEmit.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#37', 'xBairro', 2, 60, 1,
                                      BPe.Emit.enderEmit.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, '#38', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('#29', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#39', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, '#40', 'CEP', 8, 8, 1,
                                              BPe.Emit.enderEmit.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, '#41', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('#32', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#42', 'fone', 7, 12, 0,
                                OnlyNumber(BPe.Emit.enderEmit.fone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, '#43', 'email', 1, 60, 0,
                                          BPe.Emit.enderEmit.email, DSC_EMAIL));
end;

function TBPeXmlWriter.Gerar_Comp: TACBrXmlNode;
const
  HOM_NOME = 'BP-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
begin
  Result := FDocument.CreateElement('comp');

  if BPe.Ide.tpAmb = taProducao then
    Result.AppendChild(AddNode(tcStr, '#46', 'xNome', 2, 60, 1,
                                                     BPe.Comp.xNome, DSC_XNOME))
  else
    Result.AppendChild(AddNode(tcStr, '#46', 'xNome', 2, 60, 1,
                                                          HOM_NOME, DSC_XNOME));

  if (BPe.Comp.idEstrangeiro <> '') or ((BPe.Comp.enderComp.cPais <> 0) and
     (BPe.Comp.enderComp.cPais <> 1058)) then
    Result.AppendChild(AddNode(tcStr, '#49', 'idEstrangeiro', 1, 20, 1,
                                         BPe.Comp.idEstrangeiro, DSC_IDESTRBPE))
  else
    Result.AppendChild(AddNodeCNPJCPF('#47', '#48', BPe.Comp.CNPJCPF));

  if BPe.Emit.IE = 'ISENTO' then
    Result.AppendChild(AddNode(tcStr, '#50', 'IE', 2, 14, 1, BPe.Emit.IE, DSC_IE))
  else
    Result.AppendChild(AddNode(tcStr, '#50', 'IE', 2, 14, 1,
                                              OnlyNumber(BPe.Emit.IE), DSC_IE));

  Result.AppendChild(Gerar_EnderComp);
end;

function TBPeXmlWriter.Gerar_EnderComp: TACBrXmlNode;
var
  cMun: integer;
  xMun, xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, BPe.Comp.EnderComp.UF,
    BPe.Comp.EnderComp.xMun, BPe.Comp.EnderComp.cMun);

  Result := FDocument.CreateElement('enderComp');

  Result.AppendChild(AddNode(tcStr, '#52', 'xLgr', 2, 60, 1,
                                            BPe.Comp.EnderComp.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#53', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, BPe.Comp.EnderComp.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#54', 'xCpl', 1, 60, 0,
                                            BPe.Comp.EnderComp.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#55', 'xBairro', 2, 60, 1,
                                      BPe.Comp.EnderComp.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, '#56', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('#56', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#57', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, '#58', 'CEP', 8, 8, 1,
                                              BPe.Comp.EnderComp.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, '#59', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('#59', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#60', 'cPais', 1, 4, 0,
    IfThen(BPe.Comp.enderComp.cPais <> 0, IntToStrZero(BPe.Comp.enderComp.cPais,4), ''), DSC_CPAIS));

  if not ValidarCodigoPais(BPe.Comp.enderComp.cPais) = -1 then
    wAlerta('#60', 'cPais', DSC_CPAIS, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#61', 'xPais', 1, 60, 0,
                                          BPe.Comp.EnderComp.xPais, DSC_XPAIS));

  Result.AppendChild(AddNode(tcStr, '#62', 'fone', 7, 12, 0,
                                OnlyNumber(BPe.Comp.EnderComp.fone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, '#63', 'email', 1, 60, 0,
                                          BPe.Comp.EnderComp.email, DSC_EMAIL));
end;

function TBPeXmlWriter.Gerar_Agencia: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('agencia');

  Result.AppendChild(AddNode(tcStr, '#65', 'xNome', 2, 60, 1,
                                                 BPe.Agencia.xNome, DSC_XNOME));

  Result.AppendChild(AddNode(tcStr, '#66', 'CNPJ', 14, 14, 1,
                                                   BPe.Agencia.CNPJ, DSC_CNPJ));

  Result.AppendChild(Gerar_EnderAgencia);
end;

function TBPeXmlWriter.Gerar_EnderAgencia: TACBrXmlNode;
var
  cMun: integer;
  xMun, xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, BPe.Agencia.EnderAgencia.UF,
    BPe.Agencia.EnderAgencia.xMun, BPe.Agencia.EnderAgencia.cMun);

  Result := FDocument.CreateElement('enderAgencia');

  Result.AppendChild(AddNode(tcStr, '#68', 'xLgr', 2, 60, 1,
                                      BPe.Agencia.EnderAgencia.xLgr, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#69', 'nro', 1, 60, 1,
    ExecutarAjusteTagNro(Opcoes.FAjustarTagNro, BPe.Agencia.EnderAgencia.nro), DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#70', 'xCpl', 1, 60, 0,
                                      BPe.Agencia.EnderAgencia.xCpl, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#71', 'xBairro', 2, 60, 1,
                                BPe.Agencia.EnderAgencia.xBairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcInt, '#72', 'cMun', 7, 7, 1, cMun, DSC_CMUN));

  if not ValidarMunicipio(cMun) then
    wAlerta('#72', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#73', 'xMun', 2, 60, 1, xMun, DSC_XMUN));

  Result.AppendChild(AddNode(tcInt, '#74', 'CEP', 8, 8, 1,
                                        BPe.Agencia.EnderAgencia.CEP, DSC_CEP));

  Result.AppendChild(AddNode(tcStr, '#75', 'UF', 2, 2, 1, xUF, DSC_UF));

  if not ValidarUF(xUF) then
    wAlerta('#75', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#76', 'cPais', 1, 4, 0,
    IfThen(BPe.Agencia.EnderAgencia.cPais <> 0, IntToStrZero(BPe.Agencia.EnderAgencia.cPais,4), ''), DSC_CPAIS));

  if not ValidarCodigoPais(BPe.Agencia.EnderAgencia.cPais) = -1 then
    wAlerta('#76', 'cPais', DSC_CPAIS, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, '#77', 'xPais', 1, 60, 0,
                                    BPe.Agencia.EnderAgencia.xPais, DSC_XPAIS));

  Result.AppendChild(AddNode(tcStr, '#78', 'fone', 7, 12, 0,
                          OnlyNumber(BPe.Agencia.EnderAgencia.fone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, '#79', 'email', 1, 60, 0,
                                    BPe.Agencia.EnderAgencia.email, DSC_EMAIL));
end;

function TBPeXmlWriter.Gerar_InfBPeSub: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infBPeSub');

  Result.AppendChild(AddNode(tcStr, '#81', 'chBPe', 44, 44, 1,
                                               BPe.infBPeSub.chBPe, DSC_CHBPE));

  Result.AppendChild(AddNode(tcStr, '#82', 'tpSub', 1, 1, 1,
                          tpSubstituicaoToStr(BPe.infBpeSub.tpSub), DSC_TPSUB));
end;

function TBPeXmlWriter.Gerar_InfPassagem: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infPassagem');

  Result.AppendChild(AddNode(tcStr, '#84', 'cLocOrig', 1, 7, 1,
                                       BPe.infPassagem.cLocOrig, DSC_CLOCORIG));

  Result.AppendChild(AddNode(tcStr, '#85', 'xLocOrig', 2, 60, 1,
                                       BPe.infPassagem.xLocOrig, DSC_XLOCORIG));

  Result.AppendChild(AddNode(tcStr, '#86', 'cLocDest', 1, 7, 1,
                                       BPe.infPassagem.cLocDest, DSC_CLOCDEST));

  Result.AppendChild(AddNode(tcStr, '#87', 'xLocDest', 2, 60, 1,
                                       BPe.infPassagem.xLocDest, DSC_XLOCDEST));

  Result.AppendChild(AddNode(tcStr, '#88', 'dhEmb', 25, 25, 1,
    DateTimeTodh(BPe.infPassagem.dhEmb) + GetUTC(CodigoUFparaUF(BPe.ide.cUF),
    BPe.infPassagem.dhEmb), DSC_DHEMB));

  Result.AppendChild(AddNode(tcStr, '#89', 'dhValidade', 25, 25, 1,
    DateTimeTodh(BPe.infPassagem.dhValidade) + GetUTC(CodigoUFparaUF(BPe.ide.cUF),
    BPe.infPassagem.dhValidade), DSC_DHVALIDADE));

  if BPe.infPassagem.infPassageiro.xNome <> '' then
    Result.AppendChild(Gerar_InfPassageiro);
end;

function TBPeXmlWriter.Gerar_InfPassageiro: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('infPassageiro');

  Result.AppendChild(AddNode(tcStr, '#91', 'xNome', 2, 60, 1,
                           BPe.infPassagem.infPassageiro.xNome, DSC_XNOMEPASS));

  Result.AppendChild(AddNodeCPF('#92',
                      BPe.infPassagem.infPassageiro.CPF, CODIGO_BRASIL, False));

  Result.AppendChild(AddNode(tcStr, '#93', 'tpDoc', 1, 1, 1,
             tpDocumentoToStr(BPe.infPassagem.infPassageiro.tpDoc), DSC_TPDOC));

  Result.AppendChild(AddNode(tcStr, '#94', 'nDoc', 2, 20, 1,
                                 BPe.infPassagem.infPassageiro.nDoc, DSC_NDOC));

  Result.AppendChild(AddNode(tcStr, '#95', 'xDoc', 2, 100, 0,
                                 BPe.infPassagem.infPassageiro.xDoc, DSC_XDOC));

  Result.AppendChild(AddNode(tcDat, '#96', 'dNasc', 10, 10, 0,
                               BPe.infPassagem.infPassageiro.dNasc, DSC_DNASC));

  Result.AppendChild(AddNode(tcStr, '#97', 'fone', 7, 12, 0,
                     OnlyNumber(BPe.infPassagem.infPassageiro.Fone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, '#98', 'email', 1, 60, 0,
                               BPe.infPassagem.infPassageiro.Email, DSC_EMAIL));
end;

function TBPeXmlWriter.Gerar_InfViagem: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;

  SetLength(Result, BPe.infViagem.Count);

  for i := 0 to BPe.infViagem.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('infViagem');

    Result[i].AppendChild(AddNode(tcStr, '#100', 'cPercurso', 1, 20, 1,
                                    BPe.infViagem[i].cPercurso, DSC_CPERCURSO));

    Result[i].AppendChild(AddNode(tcStr, '#101', 'xPercurso', 2, 100, 1,
                                    BPe.infViagem[i].xPercurso, DSC_XPERCURSO));

    Result[i].AppendChild(AddNode(tcStr, '#102', 'tpViagem', 2, 2, 1,
                       tpViagemToStr(BPe.infViagem[i].tpViagem), DSC_TPVIAGEM));

    Result[i].AppendChild(AddNode(tcStr, '#103', 'tpServ', 1, 2, 1,
                       tpServicoToStr(BPe.infViagem[i].tpServ), DSC_TPSERVICO));

    Result[i].AppendChild(AddNode(tcStr, '#104', 'tpAcomodacao', 1, 1, 1,
           tpAcomodacaoToStr(BPe.infViagem[i].tpAcomodacao), DSC_TPACOMODACAO));

    Result[i].AppendChild(AddNode(tcStr, '#105', 'tpTrecho', 1, 1, 1,
                       tpTrechoToStr(BPe.infViagem[i].tpTrecho), DSC_TPTRECHO));

    Result[i].AppendChild(AddNode(tcStr, '#106', 'dhViagem', 25, 25, 1,
      DateTimeTodh(BPe.infViagem[i].dhViagem) + GetUTC(CodigoUFparaUF(BPe.ide.cUF),
      BPe.infViagem[i].dhViagem), DSC_DHVIAGEM));

    if BPe.infViagem[i].tpTrecho = ttConexao then
      Result[i].AppendChild(AddNode(tcStr, '#107', 'dhConexao', 25, 25, 1,
      DateTimeTodh(BPe.infViagem[i].dhConexao) + GetUTC(CodigoUFparaUF(BPe.ide.cUF),
      BPe.infViagem[i].dhConexao), DSC_DHCONEXAO));

    Result[i].AppendChild(AddNode(tcStr, '#108', 'prefixo', 1, 20, 0,
                                        BPe.infViagem[i].Prefixo, DSC_PREFIXO));

    Result[i].AppendChild(AddNode(tcStr, '#109', 'poltrona', 1, 3, 0,
                                      BPe.infViagem[i].Poltrona, DSC_POLTRONA));

    Result[i].AppendChild(AddNode(tcStr, '#110', 'plataforma', 1, 10, 0,
                                  BPe.infViagem[i].Plataforma, DSC_PLATAFORMA));

    if BPe.infViagem[i].infTravessia.tpVeiculo <> tvNenhum then
    begin
      Result[i] := FDocument.CreateElement('infTravessia');

      Result[i].AppendChild(AddNode(tcStr, '#111', 'tpVeiculo', 2, 2, 1,
       tpVeiculoToStr(BPe.infViagem[i].infTravessia.tpVeiculo), DSC_TPVEICULO));

      Result[i].AppendChild(AddNode(tcStr, '#112', 'sitVeiculo', 1, 1, 1,
        SitVeiculoToStr(BPe.infViagem[i].infTravessia.sitVeiculo), DSC_SITVEICULO));
    end;
  end;

  if BPe.infViagem.Count > 990 then
    wAlerta('#99', 'infViagem', DSC_INFVIAGEM, ERR_MSG_MAIOR_MAXIMO + '990');

  if BPe.infViagem.Count < 1 then
    wAlerta('#99', 'infViagem', DSC_INFVIAGEM, ERR_MSG_MENOR_MINIMO + '1');
end;

function TBPeXmlWriter.Gerar_InfValorBPe: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := FDocument.CreateElement('infValorBPe');

  Result.AppendChild(AddNode(tcDe2, '#115', 'vBP', 1, 15, 1,
                                                 BPe.infValorBPe.vBP, DSC_VBP));

  Result.AppendChild(AddNode(tcDe2, '#116', 'vDesconto', 1, 15, 1,
                                     BPe.infValorBPe.vDesconto, DSC_VDESCONTO));

  Result.AppendChild(AddNode(tcDe2, '#117', 'vPgto', 1, 15, 1,
                                             BPe.infValorBPe.vPgto, DSC_VPGTO));

  Result.AppendChild(AddNode(tcDe2, '#118', 'vTroco', 1, 15, 1,
                                           BPe.infValorBPe.vTroco, DSC_VTROCO));

  Result.AppendChild(AddNode(tcStr, '#119', 'tpDesconto', 2, 2, 0,
                  tpDescontoToStr(BPe.infValorBPe.tpDesconto), DSC_TPDESCONTO));

  Result.AppendChild(AddNode(tcStr, '#120', 'xDesconto', 2, 100, 0,
                                     BPe.infValorBPe.xDesconto, DSC_XDESCONTO));

  Result.AppendChild(AddNode(tcStr, '#121', 'cDesconto', 2, 20, 0,
                                     BPe.infValorBPe.cDesconto, DSC_CDESCONTO));

  nodeArray := Gerar_CompValor;
  for i := 0 to BPe.infValorBPe.Comp.Count - 1 do
  begin
    Result.AppendChild(nodeArray[i]);
  end;
end;

function TBPeXmlWriter.Gerar_CompValor: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;

  SetLength(Result, BPe.infValorBPe.Comp.Count);

  for i := 0 to BPe.infValorBPe.Comp.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('Comp');

    Result[i].AppendChild(AddNode(tcStr, '#123', 'tpComp', 2, 2, 1,
                tpComponenteToStr(BPe.infValorBPe.Comp[i].tpComp), DSC_TPCOMP));

    Result[i].AppendChild(AddNode(tcDe2, '#124', 'vComp', 1, 15, 1,
                                     BPe.infValorBPe.Comp[i].vComp, DSC_VCOMP));
  end;

  if BPe.infValorBPe.Comp.Count > 990 then
    wAlerta('#122', 'Comp', DSC_COMP, ERR_MSG_MAIOR_MAXIMO + '990');

  if BPe.infValorBPe.Comp.Count < 1 then
    wAlerta('#122', 'Comp', DSC_COMP, ERR_MSG_MENOR_MINIMO + '1');
end;

function TBPeXmlWriter.Gerar_Imp: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('imp');

  Result.AppendChild(Gerar_ICMS);
end;

function TBPeXmlWriter.Gerar_ICMS: TACBrXmlNode;

  function BuscaTag(const t: TCSTIcms): String;
  begin
    case t of
      cst00: result := '00';
      cst20: result := '20';
      cst40,
      cst41,
      cst51: result := '45';
      cst90: result := '90';
      cstSN: result := 'SN';
    end;
  end;

var
  sTagTemp: String;
  xmlNode: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMS');

  sTagTemp := BuscaTag(BPe.Imp.ICMS.CST);

  xmlNode := Result.AddChild('ICMS' + sTagTemp);

  xmlNode.AppendChild(AddNode(tcStr, '#128', 'CST', 2, 2, 1,
                                      CSTICMSTOStr(BPe.Imp.ICMS.CST), DSC_CST));

  case BPe.Imp.ICMS.CST of
    cst00:
      begin
        xmlNode.AppendChild(AddNode(tcDe2, '#129', 'vBC', 1, 15, 1,
                                                    BPe.Imp.ICMS.vBC, DSC_VBC));

        xmlNode.AppendChild(AddNode(tcDe2, '#130', 'pICMS', 1, 5, 1,
                                                BPe.Imp.ICMS.pICMS, DSC_PICMS));

        xmlNode.AppendChild(AddNode(tcDe2, '#131', 'vICMS', 1, 15, 1,
                                                BPe.Imp.ICMS.vICMS, DSC_VICMS));
      end;

    cst20:
      begin
        xmlNode.AppendChild(AddNode(tcDe2, '#133', 'pRedBC', 1, 5, 1,
                                              BPe.Imp.ICMS.pRedBC, DSC_PREDBC));

        xmlNode.AppendChild(AddNode(tcDe2, '#134', 'vBC', 1, 15, 1,
                                                    BPe.Imp.ICMS.vBC, DSC_VBC));

        xmlNode.AppendChild(AddNode(tcDe2, '#135', 'pICMS ', 1, 5, 1,
                                                BPe.Imp.ICMS.pICMS, DSC_PICMS));

        xmlNode.AppendChild(AddNode(tcDe2, '#136', 'vICMS ', 1, 15, 1,
                                                BPe.Imp.ICMS.vICMS, DSC_VICMS));

        if BPe.Imp.ICMS.vICMSDeson > 0 then
        begin
          xmlNode.AppendChild(AddNode(tcDe2, '#137', 'vICMSDeson', 1, 15, 1,
            BPe.Imp.ICMS.vICMSDeson, DSC_VICMSDESON));

          xmlNode.AppendChild(AddNode(tcStr, '#138', 'cBenef', 10, 10, 1,
                                              BPe.Imp.ICMS.cBenef, DSC_CBENEF));
        end;
      end;

    cst40,
    cst41,
    cst51:
      begin
        if BPe.Imp.ICMS.vICMSDeson > 0 then
        begin
          xmlNode.AppendChild(AddNode(tcDe2, '#140', 'vICMSDeson', 1, 15, 1,
                                      BPe.Imp.ICMS.vICMSDeson, DSC_VICMSDESON));

          xmlNode.AppendChild(AddNode(tcStr, '#141', 'cBenef', 10, 10, 1,
                                              BPe.Imp.ICMS.cBenef, DSC_CBENEF));
        end;
      end;

    cst90:
      begin
        xmlNode.AppendChild(AddNode(tcDe2, '#143', 'pRedBC', 1, 5, 0,
                                              BPe.Imp.ICMS.pRedBC, DSC_PREDBC));

        xmlNode.AppendChild(AddNode(tcDe2, '#144', 'vBC', 1, 15, 1,
                                                    BPe.Imp.ICMS.vBC, DSC_VBC));

        xmlNode.AppendChild(AddNode(tcDe2, '#145', 'pICMS ', 1, 5, 1,
                                                BPe.Imp.ICMS.pICMS, DSC_PICMS));

        xmlNode.AppendChild(AddNode(tcDe2, '#146', 'vICMS ', 1, 15, 1,
                                                BPe.Imp.ICMS.vICMS, DSC_VICMS));

        xmlNode.AppendChild(AddNode(tcDe2, '#147', 'vCred ', 1, 15, 1,
                                                BPe.Imp.ICMS.vCred, DSC_VCRED));

        if BPe.Imp.ICMS.vICMSDeson > 0 then
        begin
          xmlNode.AppendChild(AddNode(tcDe2, '#148', 'vICMSDeson', 1, 15, 1,
                                      BPe.Imp.ICMS.vICMSDeson, DSC_VICMSDESON));

          xmlNode.AppendChild(AddNode(tcStr, '#149', 'cBenef', 10, 10, 1,
                                              BPe.Imp.ICMS.cBenef, DSC_CBENEF));
        end;
      end;

    cstSN:
      begin
        xmlNode.AppendChild(AddNode(tcStr, '#151', 'indSN', 1, 1, 1, '1'));
      end;
  end;

  Result.AppendChild(AddNode(tcDe2, '#150', 'vTotTrib', 1, 15, 0,
                                               BPe.Imp.vTotTrib, DSC_VTOTTRIB));

  Result.AppendChild(AddNode(tcStr, '#151', 'infAdFisco', 1, 2000, 0,
                                           BPe.Imp.infAdFisco, DSC_INFADFISCO));

  if BPe.Imp.ICMSUFFim.vBCUFFim <> 0 then
    Result.AppendChild(Gerar_ICMSUFFim);
end;

function TBPeXmlWriter.Gerar_ICMSUFFim: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMSUFFim');

  Result.AppendChild(AddNode(tcDe2, '#153', 'vBCUFFim', 1, 15, 1,
                                     BPe.Imp.ICMSUFFim.vBCUFFim, DSC_VBCUFFIM));

  Result.AppendChild(AddNode(tcDe2, '#154', 'pFCPUFFim', 1, 5, 1,
                                   BPe.Imp.ICMSUFFim.pFCPUFFim, DSC_PFCPUFFIM));

  Result.AppendChild(AddNode(tcDe2, '#155', 'pICMSUFFim', 1, 5, 1,
                                 BPe.Imp.ICMSUFFim.pICMSUFFim, DSC_PICMSUFFIM));

  Result.AppendChild(AddNode(tcDe2, '#156', 'pICMSInter', 1, 5, 1,
                                 BPe.Imp.ICMSUFFim.pICMSInter, DSC_PICMSINTER));

  Result.AppendChild(AddNode(tcDe2, '#157', 'vFCPUFFim', 1, 15, 1,
                                   BPe.Imp.ICMSUFFim.vFCPUFFim, DSC_VFCPUFFIM));

  Result.AppendChild(AddNode(tcDe2, '#158', 'vICMSUFFim', 1, 15, 1,
                                 BPe.Imp.ICMSUFFim.vICMSUFFim, DSC_VICMSUFFIM));

  Result.AppendChild(AddNode(tcDe2, '#159', 'vICMSUFIni', 1, 15, 1,
                                 BPe.Imp.ICMSUFFim.vICMSUFIni, DSC_VICMSUFINI));
end;

function TBPeXmlWriter.Gerar_Pag: TACBrXmlNodeArray;
var
  i: integer;
  xmlNode: TACBrXmlNode;
begin
  Result := nil;

  SetLength(Result, BPe.Pag.Count);

  for i := 0 to BPe.Pag.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('pag');

    Result[i].AppendChild(AddNode(tcStr, '#161', 'tPag', 2, 2, 1,
                            FormaPagamentoBPeToStr(BPe.pag[i].tPag), DSC_TPAG));

    Result[i].AppendChild(AddNode(tcStr, '#162', 'xPag', 2, 100, 0,
                                                    BPe.pag[i].xPag, DSC_XPAG));

    Result[i].AppendChild(AddNode(tcStr, '#163', 'nDocPag', 2, 20, 0,
                                              BPe.pag[i].nDocPag, DSC_NDOCPAG));

    Result[i].AppendChild(AddNode(tcDe2, '#164', 'vPag', 1, 15, 1,
                                                    BPe.pag[i].vPag, DSC_VPAG));

    if (BPe.pag[i].tPag in [fpCartaoDebito, fpCartaoCredito]) and
       ((BPe.pag[i].CNPJ <> '') or (BPe.pag[i].tpIntegra <> tiNaoInformado)) then
    begin
      xmlNode := Result[i].AddChild('card');

      xmlNode.AppendChild(AddNode(tcStr, '#166', 'tpIntegra', 1, 1, 1,
                          tpIntegraToStr(BPe.pag[i].tpIntegra), DSC_TPINTEGRA));

      xmlNode.AppendChild(AddNode(tcStr, '#167', 'CNPJ', 14, 14, 0,
                                                    BPe.pag[i].CNPJ, DSC_CNPJ));

      xmlNode.AppendChild(AddNode(tcStr, '#168', 'tBand', 2, 2, 0,
                               BandeiraCardToStr(BPe.pag[i].tBand), DSC_TBAND));

      xmlNode.AppendChild(AddNode(tcStr, '#169', 'xBand', 2, 100, 0,
                                                  BPe.pag[i].xBand, DSC_XBAND));

      xmlNode.AppendChild(AddNode(tcStr, '#170', 'cAut', 1, 20, 0,
                                                    BPe.pag[i].cAut, DSC_CAUT));

      xmlNode.AppendChild(AddNode(tcStr, '#171', 'nsuTrans', 1, 20, 0,
                                            BPe.pag[i].nsuTrans, DSC_NSUTRANS));

      xmlNode.AppendChild(AddNode(tcStr, '#172', 'nsuHost', 1, 20, 0,
                                              BPe.pag[i].nsuHost, DSC_NSUHOST));

      xmlNode.AppendChild(AddNode(tcInt, '#173', 'nParcelas', 1, 3, 0,
                                          BPe.pag[i].nParcelas, DSC_NPARCELAS));

      xmlNode.AppendChild(AddNode(tcStr, '#174', 'infAdCard', 1, 2000, 0,
                                          BPe.pag[i].infAdCard, DSC_INFADCARD));
    end;
  end;

  if BPe.Pag.Count > 10 then
    wAlerta('#160', 'pag', '', ERR_MSG_MAIOR_MAXIMO + '10');

  if BPe.Pag.Count < 1 then
    wAlerta('#160', 'pag', '', ERR_MSG_MENOR_MINIMO + '1');
end;

function TBPeXmlWriter.Gerar_DetBPeTM: TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  SetLength(Result, BPe.detBPeTM.Count);

  for i := 0 to BPe.detBPeTM.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('detBPeTM');

    if BPe.detBPeTM[i].idEqpCont <> 0 then
      Result[i].SetAttribute('idEqpCont', IntToStr(BPe.detBPeTM[i].idEqpCont));

    Result[i].AppendChild(AddNode(tcStr, '#44', 'UFIniViagem', 2, 2, 1,
                                 BPe.detBPeTM[i].UFIniViagem, DSC_UFINIVIAGEM));

    Result[i].AppendChild(AddNode(tcStr, '#45', 'UFFimViagem', 2, 2, 1,
                                 BPe.detBPeTM[i].UFFimViagem, DSC_UFFIMVIAGEM));

    Result[i].AppendChild(AddNode(tcStr, '#46', 'placa', 7, 7, 0,
                                             BPe.detBPeTM[i].placa, DSC_PLACA));

    Result[i].AppendChild(AddNode(tcStr, '#47', 'prefixo', 1, 20, 0,
                                         BPe.detBPeTM[i].prefixo, DSC_PREFIXO));

    nodeArray := Gerar_Det(i);
    for j := 0 to BPe.detBPeTM[i].det.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if BPe.detBPeTM.Count > 99 then
    wAlerta('#42', 'detBPeTM', '', ERR_MSG_MAIOR_MAXIMO + '99');

  if BPe.detBPeTM.Count < 1 then
    wAlerta('#42', 'detBPeTM', '', ERR_MSG_MENOR_MINIMO + '1');
end;

function TBPeXmlWriter.Gerar_Det(indice: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  SetLength(Result, BPe.detBPeTM[indice].det.Count);

  for i := 0 to BPe.detBPeTM[indice].det.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('det');

    if BPe.detBPeTM[indice].det[i].nViagem <> 0 then
      Result[i].SetAttribute('nViagem', IntToStr(BPe.detBPeTM[indice].det[i].nViagem));

    Result[i].AppendChild(AddNode(tcInt, '#50', 'cMunIni', 7, 7, 1,
                          BPe.detBPeTM[indice].det[i].cMunIni, DSC_CMUNINIBPE));

    Result[i].AppendChild(AddNode(tcInt, '#51', 'cMunFim', 7, 7, 1,
                          BPe.detBPeTM[indice].det[i].cMunFim, DSC_CMUNFIMBPE));

    if (BPe.detBPeTM[indice].det[i].nContInicio <> '') or
       (BPe.detBPeTM[indice].det[i].nContFim <> '') then
    begin
      Result[i].AppendChild(AddNode(tcStr, '#52', 'nContInicio', 1, 20, 1,
                     BPe.detBPeTM[indice].det[i].nContInicio, DSC_NCONTINICIO));

      Result[i].AppendChild(AddNode(tcStr, '#53', 'nContFim', 1, 20, 1,
                           BPe.detBPeTM[indice].det[i].nContFim, DSC_NCONTFIM));
    end;

    Result[i].AppendChild(AddNode(tcStr, '#54', 'qPass', 1, 20, 1,
                                 BPe.detBPeTM[indice].det[i].qPass, DSC_QPASS));

    Result[i].AppendChild(AddNode(tcDe2, '#55', 'vBP', 1, 15, 1,
                                     BPe.detBPeTM[indice].det[i].vBP, DSC_VBP));

    Result[i].AppendChild(Gerar_ImpTM(indice, i));

    nodeArray := Gerar_CompValorTM(indice, i);
    for j := 0 to BPe.detBPeTM[indice].det[i].Comp.Count - 1 do
    begin
      Result[i].AppendChild(nodeArray[j]);
    end;
  end;

  if BPe.detBPeTM[indice].det.Count > 990 then
    wAlerta('#48', 'det', '', ERR_MSG_MAIOR_MAXIMO + '990');

  if BPe.detBPeTM[indice].det.Count < 1 then
    wAlerta('#48', 'det', '', ERR_MSG_MENOR_MINIMO + '1');
end;

function TBPeXmlWriter.Gerar_ImpTM(indiceI, IndiceJ: Integer): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('imp');

  Result.AppendChild(Gerar_ICMSTM(indiceI, IndiceJ));
end;

function TBPeXmlWriter.Gerar_ICMSTM(indiceI, IndiceJ: Integer): TACBrXmlNode;

  function BuscaTag(const t: TCSTIcms): String;
  begin
    case t of
      cst00: result := '00';
      cst20: result := '20';
      cst40,
      cst41,
      cst51: result := '45';
      cst90: result := '90';
      cstSN: result := 'SN';
    end;
  end;

var
  sTagTemp: String;
  xmlNode: TACBrXmlNode;
  Imposto: TImp;

begin
  Result := FDocument.CreateElement('ICMS');

  Imposto := BPe.detBPeTM[indiceI].det[indiceJ].Imp;

  sTagTemp := BuscaTag(Imposto.ICMS.CST);

  xmlNode := Result.AddChild('ICMS' + sTagTemp);

  xmlNode.AppendChild(AddNode(tcStr, '#128', 'CST', 2, 2, 1,
                                      CSTICMSTOStr(Imposto.ICMS.CST), DSC_CST));

  case Imposto.ICMS.CST of
    cst00:
      begin
        xmlNode.AppendChild(AddNode(tcDe2, '#129', 'vBC', 1, 15, 1,
                                                    Imposto.ICMS.vBC, DSC_VBC));

        xmlNode.AppendChild(AddNode(tcDe2, '#130', 'pICMS', 1, 5, 1,
                                                Imposto.ICMS.pICMS, DSC_PICMS));

        xmlNode.AppendChild(AddNode(tcDe2, '#131', 'vICMS', 1, 15, 1,
                                                Imposto.ICMS.vICMS, DSC_VICMS));
      end;

    cst20:
      begin
        xmlNode.AppendChild(AddNode(tcDe2, '#133', 'pRedBC', 1, 5, 1,
                                              Imposto.ICMS.pRedBC, DSC_PREDBC));

        xmlNode.AppendChild(AddNode(tcDe2, '#134', 'vBC', 1, 15, 1,
                                                    Imposto.ICMS.vBC, DSC_VBC));

        xmlNode.AppendChild(AddNode(tcDe2, '#135', 'pICMS ', 1, 5, 1,
                                                Imposto.ICMS.pICMS, DSC_PICMS));

        xmlNode.AppendChild(AddNode(tcDe2, '#136', 'vICMS ', 1, 15, 1,
                                                Imposto.ICMS.vICMS, DSC_VICMS));

        if Imposto.ICMS.vICMSDeson > 0 then
        begin
          xmlNode.AppendChild(AddNode(tcDe2, '#137', 'vICMSDeson', 1, 15, 1,
                                      Imposto.ICMS.vICMSDeson, DSC_VICMSDESON));

          xmlNode.AppendChild(AddNode(tcStr, '#138', 'cBenef', 10, 10, 1,
                                              Imposto.ICMS.cBenef, DSC_CBENEF));
        end;
      end;

    cst40,
    cst41,
    cst51:
      begin
        if Imposto.ICMS.vICMSDeson > 0 then
        begin
          xmlNode.AppendChild(AddNode(tcDe2, '#140', 'vICMSDeson', 1, 15, 1,
                                      Imposto.ICMS.vICMSDeson, DSC_VICMSDESON));

          xmlNode.AppendChild(AddNode(tcStr, '#141', 'cBenef', 10, 10, 1,
                                              Imposto.ICMS.cBenef, DSC_CBENEF));
        end;
      end;

    cst90:
      begin
        xmlNode.AppendChild(AddNode(tcDe2, '#143', 'pRedBC', 1, 5, 0,
                                              Imposto.ICMS.pRedBC, DSC_PREDBC));

        xmlNode.AppendChild(AddNode(tcDe2, '#144', 'vBC', 1, 15, 1,
                                                    Imposto.ICMS.vBC, DSC_VBC));

        xmlNode.AppendChild(AddNode(tcDe2, '#145', 'pICMS ', 1, 5, 1,
                                                Imposto.ICMS.pICMS, DSC_PICMS));

        xmlNode.AppendChild(AddNode(tcDe2, '#146', 'vICMS ', 1, 15, 1,
                                                Imposto.ICMS.vICMS, DSC_VICMS));

        xmlNode.AppendChild(AddNode(tcDe2, '#147', 'vCred ', 1, 15, 1,
                                                Imposto.ICMS.vCred, DSC_VCRED));

        if Imposto.ICMS.vICMSDeson > 0 then
        begin
          xmlNode.AppendChild(AddNode(tcDe2, '#148', 'vICMSDeson', 1, 15, 1,
                                      Imposto.ICMS.vICMSDeson, DSC_VICMSDESON));

          xmlNode.AppendChild(AddNode(tcStr, '#149', 'cBenef', 10, 10, 1,
                                              Imposto.ICMS.cBenef, DSC_CBENEF));
        end;
      end;

    cstSN:
      begin
        xmlNode.AppendChild(AddNode(tcStr, '#151', 'indSN', 1, 1, 1, '1'));
      end;
  end;

  Result.AppendChild(AddNode(tcStr, '#151', 'infAdFisco', 1, 2000, 0,
                                           Imposto.infAdFisco, DSC_INFADFISCO));
end;

function TBPeXmlWriter.Gerar_CompValorTM(indiceI,
  indiceJ: Integer): TACBrXmlNodeArray;
var
  i: Integer;
  Componentes: TdetCompCollection;
begin
  Result := nil;

  Componentes := BPe.detBPeTM[indiceI].det[indiceJ].Comp;
  SetLength(Result, Componentes.Count);

  for i := 0 to Componentes.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('Comp');

    Result[i].AppendChild(AddNode(tcStr, '#123', 'xNome', 1, 15, 1,
                                             Componentes[i].xNome, DSC_TPCOMP));

    Result[i].AppendChild(AddNode(tcInt, '#124', 'qComp', 5, 5, 1,
                                              Componentes[i].qComp, DSC_QCOMP));
  end;

  if BPe.infValorBPe.Comp.Count > 990 then
    wAlerta('#122', 'Comp', DSC_COMP, ERR_MSG_MAIOR_MAXIMO + '990');

  if BPe.infValorBPe.Comp.Count < 1 then
    wAlerta('#122', 'Comp', DSC_COMP, ERR_MSG_MENOR_MINIMO + '1');
end;

function TBPeXmlWriter.Gerar_Total: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('total');

  Result.AppendChild(AddNode(tcInt, '#86', 'qPass', 1, 6, 1,
                                                   BPe.total.qPass, DSC_QPASS));

  Result.AppendChild(AddNode(tcDe2, '#87', 'vBP', 1, 15, 1,
                                                       BPe.Total.vBP, DSC_VBP));

  Result.AppendChild(Gerar_TotalICMSTotal);
end;

function TBPeXmlWriter.Gerar_TotalICMSTotal: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('ICMSTot');

  Result.AppendChild(AddNode(tcDe2, '#89', 'vBC', 1, 15, 1,
                                                       BPe.Total.vBC, DSC_VBC));

  Result.AppendChild(AddNode(tcDe2, '#257', 'vICMS', 1, 15, 1,
                                                   BPe.Total.vICMS, DSC_VICMS));
end;

function TBPeXmlWriter.Gerar_autXML: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, BPe.autXML.Count);

  for i := 0 to BPe.autXML.Count - 1 do
  begin
    Result[i] := FDocument.CreateElement('autXML');

    Result[i].AppendChild(AddNodeCNPJCPF('#305', '#306', BPe.autXML[i].CNPJCPF));
  end;

  if BPe.autXML.Count > 10 then
    wAlerta('#304', 'autXML', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TBPeXmlWriter.Gerar_InfAdic: TACBrXmlNode;
begin
  Result := nil;

  if (trim(BPe.InfAdic.infAdFisco) <> '') or (trim(BPe.InfAdic.infCpl) <> '') then
  begin
    Result := FDocument.CreateElement('infAdic');

    Result.AppendChild(AddNode(tcStr, '#308', 'infAdFisco', 1, 2000, 0,
                                       BPe.InfAdic.infAdFisco, DSC_INFADFISCO));

    Result.AppendChild(AddNode(tcStr, '#309', 'infCpl', 1, 5000, 0,
                                               BPe.InfAdic.infCpl, DSC_INFCPL));
  end;
end;

function TBPeXmlWriter.Gerar_InfRespTec: TACBrXmlNode;
begin
  Result := nil;

  if (BPe.infRespTec.CNPJ <> '') then
  begin
    Result := FDocument.CreateElement('infRespTec');

    Result.AppendChild(AddNodeCNPJ('#311', BPe.infRespTec.CNPJ, CODIGO_BRASIL, True));

    Result.AppendChild(AddNode(tcStr, '#312', 'xContato', 2, 60, 1,
                                        BPe.infRespTec.xContato, DSC_XCONTATO));

    Result.AppendChild(AddNode(tcStr, '#313', 'email', 6, 60, 1,
                                              BPe.infRespTec.email, DSC_EMAIL));

    Result.AppendChild(AddNode(tcStr, '#314', 'fone', 7, 12, 1,
                                                BPe.infRespTec.fone, DSC_FONE));

    if (idCSRT <> 0) and (CSRT <> '') then
    begin
      Result.AppendChild(AddNode(tcInt, '#315', 'idCSRT', 2, 2, 1,
                                                           idCSRT, DSC_IDCSRT));

      Result.AppendChild(AddNode(tcStr, '#316', 'hashCSRT', 28, 28, 1,
                              CalcularHashCSRT(CSRT, FChaveBPe), DSC_HASHCSRT));
    end;
  end;
end;

function TBPeXmlWriter.Gerar_ProtBPe: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('protBPe');

  Result.SetAttribute('versao', FloatToString(BPe.infBPe.Versao, '.', '#0.00'));

  xmlNode := Result.AddChild('infProt');

  xmlNode.AddChild('tpAmb').Content := TipoAmbienteToStr(BPe.procBPe.tpAmb);

  xmlNode.AddChild('verAplic').Content := BPe.procBPe.verAplic;

  xmlNode.AddChild('chBPe').Content := BPe.procBPe.chDFe;

  xmlNode.AddChild('dhRecbto').Content :=
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', BPe.procBPe.dhRecbto) +
    GetUTC(CodigoUFparaUF(FBPe.Ide.cUF), BPe.procBPe.dhRecbto);

  xmlNode.AddChild('nProt').Content := BPe.procBPe.nProt;

  xmlNode.AddChild('digVal').Content := BPe.procBPe.digVal;

  xmlNode.AddChild('cStat').Content := IntToStr(BPe.procBPe.cStat);

  xmlNode.AddChild('xMotivo').Content := BPe.procBPe.xMotivo;
end;

end.
