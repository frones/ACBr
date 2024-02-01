{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pcnBPeW;

interface

uses
  SysUtils, Classes,
  pcnGerador, pcnConversao, pcnBPe, pcnBPeConsts;

type

  TGeradorOpcoes = class;

  { TBPeW }

  TBPeW = class(TPersistent)
  private
    FGerador: TGerador;
    FBPe: TBPe;
    FOpcoes: TGeradorOpcoes;

    FVersao: String;
    FChaveBPe: string;
    FIdCSRT: Integer;
    FCSRT: String;

    procedure GerarInfBPe;
    procedure GerarIde;

    procedure GerarEmit;
    procedure GerarEnderEmit;

    procedure GerarDetBPeTM;
    procedure GerarTotal;

    procedure GerarComp;
    procedure GerarEnderComp;

    procedure GerarAgencia;
    procedure GerarEnderAgencia;

    procedure GerarinfBPeSub;
    procedure GerarinfPassagem;
    procedure GerarinfPassageiro;

    procedure GerarinfViagem;
    procedure GerarinfValorBPe;
    procedure GerarImp;
    procedure GerarDetImp(i, j: Integer);
    procedure GerarImpICMSUFFim;

    procedure Gerarpag;
    procedure GerarautXML;
    procedure GerarInfAdic;
    procedure GerarinfRespTec;

    procedure AjustarMunicipioUF(out xUF: String; out xMun: String; out cMun: Integer; cPais: Integer; const vxUF, vxMun: String; vcMun: Integer);

  public
    constructor Create(AOwner: TBPe);
    destructor Destroy; override;
    function GerarXml: Boolean;
  published
    property Gerador: TGerador      read FGerador write FGerador;
    property BPe: TBPe              read FBPe     write FBPe;
    property Opcoes: TGeradorOpcoes read FOpcoes  write FOpcoes;
    property IdCSRT: Integer        read FIdCSRT  write FIdCSRT;
    property CSRT: String           read FCSRT    write FCSRT;
  end;

  TGeradorOpcoes = class(TPersistent)
  private
    FGerarTagIPIparaNaoTributado: Boolean;
    FGerarTXTSimultaneamente: Boolean;
    FNormatizarMunicipios: Boolean;
    FGerarTagAssinatura: TpcnTagAssinatura;
    FPathArquivoMunicipios: String;
    FValidarInscricoes: Boolean;
    FValidarListaServicos: Boolean;
  published
    property GerarTagIPIparaNaoTributado: Boolean read FGerarTagIPIparaNaoTributado write FGerarTagIPIparaNaoTributado;
    property GerarTXTSimultaneamente: Boolean read FGerarTXTSimultaneamente write FGerarTXTSimultaneamente;
    property NormatizarMunicipios: Boolean read FNormatizarMunicipios write FNormatizarMunicipios;
    property GerarTagAssinatura: TpcnTagAssinatura read FGerarTagAssinatura write FGerarTagAssinatura;
    property PathArquivoMunicipios: String read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: Boolean read FValidarInscricoes write FValidarInscricoes;
    property ValidarListaServicos: Boolean read FValidarListaServicos write FValidarListaServicos;
  end;

implementation

uses
  pcnConversaoBPe, pcnAuxiliar,
  ACBrDFeUtil, ACBrDFeConsts,
  ACBrUtil.Base,
  ACBrUtil.Strings;

{ TBPeW }

constructor TBPeW.Create(AOwner: TBPe);
begin
  inherited Create;

  FBPe     := AOwner;
  FGerador := TGerador.Create;

  FGerador.FIgnorarTagNivel   := '|?xml version|BPe xmlns|infBPe versao|obsCont|obsFisco|';
  FGerador.Opcoes.QuebraLinha := ';';

  FOpcoes := TGeradorOpcoes.Create;

  FOpcoes.FGerarTXTSimultaneamente     := False;
  FOpcoes.FGerarTagIPIparaNaoTributado := True;
  FOpcoes.FNormatizarMunicipios        := False;
  FOpcoes.FGerarTagAssinatura          := taSomenteSeAssinada;
  FOpcoes.FValidarInscricoes           := False;
  FOpcoes.FValidarListaServicos        := False;
end;

destructor TBPeW.Destroy;
begin
  FGerador.Free;
  FOpcoes.Free;
  inherited Destroy;
end;

function TBPeW.GerarXml: Boolean;
var
//  chave: String;
  Gerar: Boolean;
  xProtBPe : String;
begin
  Gerador.ListaDeAlertas.Clear;

  FVersao := Copy(BPe.infBPe.VersaoStr, 9, 4);

  FChaveBPe := GerarChaveAcesso(BPe.ide.cUF, BPe.ide.dhEmi, BPe.emit.CNPJ, BPe.ide.serie,
                            BPe.ide.nBP, StrToInt(TpEmisBPeToStr(BPe.ide.tpEmis)),
                            BPe.ide.cBP, BPe.ide.modelo);
  BPe.infBPe.ID := 'BPe' + FChaveBPe;

  BPe.ide.cDV := ExtrairDigitoChaveAcesso(BPe.infBPe.ID);
  BPe.Ide.cBP := ExtrairCodigoChaveAcesso(BPe.infBPe.ID);

  // Carrega Layout que sera utilizado para gera o txt
  Gerador.LayoutArquivoTXT.Clear;
//  if FOpcoes.GerarTXTSimultaneamente then
//    Gerador.LayoutArquivoTXT.Text := CarregarLayoutTXT(Versao);

  Gerador.ArquivoFormatoXML := '';
  Gerador.ArquivoFormatoTXT := '';

  {$IfDef FPC}
   Gerador.wGrupo(ENCODING_UTF8, '', False);
  {$EndIf}

  if BPe.procBPe.nProt <> '' then
    Gerador.wGrupo('BPeProc ' + BPe.infBPe.VersaoStr + ' ' + NAME_SPACE_BPE, '');

  if BPe.ide.tpBPe = tbBPeTM then
    Gerador.wGrupo('BPeTM ' + NAME_SPACE_BPE)
  else
    Gerador.wGrupo('BPe ' + NAME_SPACE_BPE);

  Gerador.wGrupo('infBPe ' + BPe.infBPe.VersaoStr + ' Id="' + BPe.infBPe.ID + '"');

  GerarInfBPe;

  Gerador.wGrupo('/infBPe');

  if BPe.infBPeSupl.qrCodBPe <> '' then
  begin
    Gerador.wGrupo('infBPeSupl');
    Gerador.wCampo(tcStr, '#176', 'qrCodBPe', 50, 1000, 1,
                     '<![CDATA[' + BPe.infBPeSupl.qrCodBPe + ']]>', DSC_INFQRCODEBPE, False);
    Gerador.wCampo(tcStr, '#177', 'boardPassBPe', 50, 1000, 0, BPe.infBPeSupl.boardPassBPe, DSC_BOARDPASSBPE, False);
    Gerador.wGrupo('/infBPeSupl');
  end;

  if FOpcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := true;
    if FOpcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((BPe.Signature.DigestValue <> '') and (BPe.Signature.SignatureValue <> '') and (BPe.Signature.X509Certificate <> ''));
    if FOpcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
      Gerar := ((BPe.Signature.DigestValue = '') and (BPe.Signature.SignatureValue = '') and (BPe.Signature.X509Certificate = ''));
    if Gerar then
    begin
      FBPe.Signature.URI := '#BPe' + OnlyNumber(BPe.infBPe.ID);
      FBPe.Signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
      FBPe.Signature.GerarXML;
      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + FBPe.Signature.Gerador.ArquivoFormatoXML;
    end;
  end;

  if BPe.ide.tpBPe = tbBPeTM then
    Gerador.wGrupo('/BPeTM')
  else
    Gerador.wGrupo('/BPe');

  if BPe.procBPe.nProt <> '' then
   begin
     xProtBPe :=
       (**)'<protBPe ' + BPe.infBPe.VersaoStr + '>' +
     (******)'<infProt>'+
     (*********)'<tpAmb>' + TpAmbToStr(BPe.procBPe.tpAmb) + '</tpAmb>' +
     (*********)'<verAplic>' + BPe.procBPe.verAplic + '</verAplic>' +
     (*********)'<chBPe>' + BPe.procBPe.chBPe + '</chBPe>' +
     (*********)'<dhRecbto>' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', BPe.procBPe.dhRecbto) +
                               GetUTC(CodigoParaUF(FBPe.Ide.cUF), BPe.procBPe.dhRecbto) + '</dhRecbto>' +
     (*********)'<nProt>' + BPe.procBPe.nProt + '</nProt>' +
     (*********)'<digVal>' + BPe.procBPe.digVal + '</digVal>' +
     (*********)'<cStat>' + IntToStr(BPe.procBPe.cStat) + '</cStat>' +
     (*********)'<xMotivo>' + BPe.procBPe.xMotivo + '</xMotivo>' +
     (******)'</infProt>' +
             IIF( (BPe.procBPe.cMsg > 0) or (BPe.procBPe.xMsg <> ''),
             '<infFisco>' +
               '<cMsg>' + IntToStr(BPe.procBPe.cMsg) + '</cMsg>' +
               '<xMsg>' + BPe.procBPe.xMsg + '</xMsg>' +
             '</infFisco>',
             '') +
     (****)'</protBPe>';

     Gerador.wTexto(xProtBPe);
     Gerador.wGrupo('/BPeProc');
   end;

  Gerador.gtAjustarRegistros(BPe.infBPe.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TBPeW.GerarInfBPe;
begin
  GerarIde;
  GerarEmit; // Emitente

  if BPe.ide.tpBPe = tbBPeTM then
  begin
    GerarDetBPeTM;
    GerarTotal;
  end
  else
  begin
    if BPe.Comp.xNome <> '' then
      GerarComp; // Comprador

    if BPe.Agencia.xNome <> '' then
      GerarAgencia;

    if BPe.infBPeSub.chBPe <> '' then
      GerarinfBPeSub;

    GerarinfPassagem;
    GerarinfViagem;
    GerarinfValorBPe;
    GerarImp; // Impostos
    GerarPag; // Pagamento
  end;

  GerarautXML;
  GerarInfAdic;
  GerarinfRespTec;
end;

procedure TBPeW.GerarIde;
begin
  Gerador.wGrupo('ide', '#004');

  Gerador.wCampo(tcInt, '#005', 'cUF', 02, 02, 1, BPe.ide.cUF, DSC_CUF);
  if not ValidarCodigoUF(BPe.ide.cUF) then
    Gerador.wAlerta('#005', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#006', 'tpAmb  ', 01, 01, 1, tpAmbToStr(BPe.Ide.tpAmb), DSC_TPAMB);
  Gerador.wCampo(tcInt, '#007', 'mod    ', 02, 02, 1, BPe.ide.modelo, DSC_MOD);
  Gerador.wCampo(tcInt, '#008', 'serie  ', 01, 03, 1, BPe.ide.serie, DSC_SERIE);
  Gerador.wCampo(tcInt, '#009', 'nBP    ', 01, 09, 1, BPe.ide.nBP, DSC_NDF);
  Gerador.wCampo(tcStr, '#010', 'cBP    ', 08, 08, 1, IntToStrZero(ExtrairCodigoChaveAcesso(BPe.infBPe.ID), 8), DSC_CDF);
  Gerador.wCampo(tcInt, '#011', 'cDV    ', 01, 01, 1, BPe.Ide.cDV, DSC_CDV);
  Gerador.wCampo(tcStr, '#012', 'modal  ', 01, 01, 1, ModalBPeToStr(BPe.ide.modal), DSC_MODALBPE);
  Gerador.wCampo(tcStr, '#013', 'dhEmi  ', 25, 25, 1, DateTimeTodh(BPe.ide.dhEmi) + GetUTC(CodigoParaUF(BPe.ide.cUF), BPe.ide.dhEmi), DSC_DEMI);

  if BPe.ide.tpBPe = tbBPeTM then
    Gerador.wCampo(tcDat, '#013', 'dCompet', 10, 10, 1, BPe.ide.dCompet, DSC_DCOMPET);

  Gerador.wCampo(tcStr, '#014', 'tpEmis ', 01, 01, 1, tpEmisBPeToStr(BPe.Ide.tpEmis), DSC_TPEMIS);
  Gerador.wCampo(tcStr, '#015', 'verProc', 01, 20, 1, BPe.Ide.verProc, DSC_VERPROC);
  Gerador.wCampo(tcStr, '#016', 'tpBPe  ', 01, 01, 1, tpBPeToStr(BPe.ide.tpBPe), DSC_TPBPE);

  if BPe.ide.tpBPe = tbBPeTM then
    Gerador.wCampo(tcInt, '#013', 'CFOP', 04, 04, 1, BPe.ide.CFOP, DSC_CFOP)
  else
  begin
    Gerador.wCampo(tcStr, '#017', 'indPres', 01, 01, 1, PresencaCompradorToStr(BPe.Ide.indPres), DSC_INDPRESBPE);
    Gerador.wCampo(tcStr, '#018', 'UFIni  ', 02, 02, 1, BPe.Ide.UFIni, DSC_UFINIBPE);
    Gerador.wCampo(tcInt, '#019', 'cMunIni', 07, 07, 1, BPe.ide.cMunIni, DSC_CMUNINIBPE);
    Gerador.wCampo(tcStr, '#020', 'UFFim  ', 02, 02, 1, BPe.Ide.UFFim, DSC_UFFIMBPE);
    Gerador.wCampo(tcInt, '#021', 'cMunFim', 07, 07, 1, BPe.ide.cMunFim, DSC_CMUNFIMBPE);
  end;

  if (BPe.Ide.dhCont > 0) or (BPe.Ide.xJust <> '') then
  begin
    Gerador.wCampo(tcStr, '#022', 'dhCont', 25,  25, 1, DateTimeTodh(BPe.ide.dhCont) + GetUTC(CodigoParaUF(BPe.ide.cUF), BPe.ide.dhCont), DSC_DHCONT);
    Gerador.wCampo(tcStr, '#023', 'xJust ', 15, 256, 1, BPe.ide.xJust, DSC_XJUSTCONT);
  end;

  Gerador.wGrupo('/ide');
end;

procedure TBPeW.GerarEmit;
begin
  Gerador.wGrupo('emit', '#024');

  Gerador.wCampoCNPJ('#025', BPe.Emit.CNPJ, CODIGO_BRASIL, True);

  if BPe.Emit.IE = 'ISENTO' then
    Gerador.wCampo(tcStr, '#026', 'IE', 00, 14, 1, BPe.Emit.IE, DSC_IE)
  else
    Gerador.wCampo(tcStr, '#026', 'IE', 00, 14, 1, OnlyNumber(BPe.Emit.IE), DSC_IE);

  if (FOpcoes.ValidarInscricoes) then
  begin
    if Length(BPe.Emit.IE) = 0 then
      Gerador.wAlerta('#026', 'IE', DSC_IE, ERR_MSG_VAZIO)
    else
    begin
      if not ValidarIE(BPe.Emit.IE, CodigoParaUF(BPe.Ide.cUF)) then
        Gerador.wAlerta('#026', 'IE', DSC_IE, ERR_MSG_INVALIDO);
    end;
  end;

  Gerador.wCampo(tcStr, '#027', 'IEST ', 02, 14, 0, BPe.Emit.IEST, DSC_IEST);
  Gerador.wCampo(tcStr, '#028', 'xNome', 02, 60, 1, BPe.Emit.xNome, DSC_XNOME);
  Gerador.wCampo(tcStr, '#029', 'xFant', 01, 60, 0, BPe.Emit.xFant, DSC_XFANT);
  Gerador.wCampo(tcStr, '#030', 'IM   ', 01, 15, 1, BPe.Emit.IM, DSC_IM);
  Gerador.wCampo(tcStr, '#031', 'CNAE ', 07, 07, 1, BPe.Emit.CNAE, DSC_CNAE);
  Gerador.wCampo(tcStr, '#032', 'CRT  ', 01, 01, 1, CRTToStr(BPe.Emit.CRT), DSC_CRTBPE);

  GerarEnderEmit;

  Gerador.wCampo(tcStr, '#044', 'TAR', 01, 20, 0, BPe.Emit.TAR, DSC_TAR);

  Gerador.wGrupo('/emit');
end;

procedure TBPeW.GerarEnderEmit;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, BPe.Emit.enderEmit.UF,
                     BPe.Emit.enderEmit.xMun, BPe.Emit.EnderEmit.cMun);

  Gerador.wGrupo('enderEmit', '#033');

  Gerador.wCampo(tcStr, '#034', 'xLgr   ', 02, 60, 1, BPe.Emit.enderEmit.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#035', 'nro    ', 01, 60, 1, BPe.Emit.enderEmit.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#036', 'xCpl   ', 01, 60, 0, BPe.Emit.enderEmit.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#037', 'xBairro', 02, 60, 1, BPe.Emit.enderEmit.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#038', 'cMun   ', 07, 07, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(cMun) then
    Gerador.wAlerta('#038', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#039', 'xMun', 02, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#040', 'CEP ', 08, 08, 0, BPe.Emit.enderEmit.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#041', 'UF  ', 02, 02, 1, xUF, DSC_UF);

  if not ValidarUF(xUF) then
    Gerador.wAlerta('#041', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#042', 'fone ', 07, 12, 0, OnlyNumber(BPe.Emit.enderEmit.fone), DSC_FONE);
  Gerador.wCampo(tcStr, '#043', 'email', 01, 60, 0, BPe.Emit.enderEmit.Email, DSC_EMAIL);

  Gerador.wGrupo('/enderEmit');
end;

procedure TBPeW.GerarComp;
const
  HOM_NOME = 'BP-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
begin
  Gerador.wGrupo('comp', '#045');

  if BPe.Ide.tpAmb = taProducao then
    Gerador.wCampo(tcStr, '#046', 'xNome', 02, 60, 1, BPe.Comp.xNome, DSC_XNOME)
  else
    Gerador.wCampo(tcStr, '#046', 'xNome', 02, 60, 1, HOM_NOME, DSC_XNOME);

  if (BPe.Comp.idEstrangeiro <> '') or ((BPe.Comp.enderComp.cPais <> 0) and (BPe.Comp.enderComp.cPais <> 1058)) then
    Gerador.wCampo(tcStr, '#049', 'idEstrangeiro', 00, 20, 1, BPe.Comp.idEstrangeiro, DSC_IDESTRBPE)
  else
    Gerador.wCampoCNPJCPF('#047', '#048', BPe.Comp.CNPJCPF, True);

  if BPe.Comp.IE = 'ISENTO' then
    Gerador.wCampo(tcStr, '#050', 'IE', 00, 14, 1, BPe.Comp.IE, DSC_IE)
  else
    Gerador.wCampo(tcStr, '#050', 'IE', 00, 14, 0, OnlyNumber(BPe.Comp.IE), DSC_IE);

  if (FOpcoes.ValidarInscricoes) then
  begin
    if Length(BPe.Comp.IE) = 0 then
      Gerador.wAlerta('#050', 'IE', DSC_IE, ERR_MSG_VAZIO)
    else
    begin
      if not ValidarIE(BPe.Comp.IE, CodigoParaUF(BPe.Ide.cUF)) then
        Gerador.wAlerta('#050', 'IE', DSC_IE, ERR_MSG_INVALIDO);
    end;
  end;

  GerarEnderComp;

  Gerador.wGrupo('/comp');
end;

procedure TBPeW.GerarEnderComp;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, BPe.Comp.enderComp.cPais, BPe.Comp.enderComp.UF,
                     BPe.Comp.enderComp.xMun, BPe.Comp.enderComp.cMun);

  Gerador.wGrupo('enderComp', '#051');

  Gerador.wCampo(tcStr, '#052', 'xLgr   ', 02, 60, 1, BPe.Comp.enderComp.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#053', 'nro    ', 01, 60, 1, BPe.Comp.enderComp.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#054', 'xCpl   ', 01, 60, 0, BPe.Comp.enderComp.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#055', 'xBairro', 01, 60, 1, BPe.Comp.enderComp.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#056', 'cMun   ', 07, 07, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(cMun) then
    Gerador.wAlerta('#056', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#057', 'xMun', 01, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#058', 'CEP ', 08, 08, 0, BPe.Comp.enderComp.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#059', 'UF  ', 02, 02, 1, xUF, DSC_UF);

  if not ValidarUF(xUF) then
    Gerador.wAlerta('#059', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#060', 'cPais', 01, 04, 0, IIf(BPe.Comp.enderComp.cPais <> 0, IntToStrZero(BPe.Comp.enderComp.cPais,4), ''), DSC_CPAIS);

  if not ValidarCodigoPais(BPe.Comp.enderComp.cPais) = -1 then
    Gerador.wAlerta('#060', 'cPais', DSC_CPAIS, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#061', 'xPais', 01, 60, 0, BPe.Comp.enderComp.xPais, DSC_XPAIS);
  Gerador.wCampo(tcStr, '#062', 'fone ', 07, 12, 0, OnlyNumber(BPe.Comp.enderComp.fone), DSC_FONE);
  Gerador.wCampo(tcStr, '#063', 'email', 01, 60, 0, BPe.Comp.enderComp.Email, DSC_EMAIL);

  Gerador.wGrupo('/enderComp');
end;

procedure TBPeW.GerarAgencia;
begin
  Gerador.wGrupo('agencia', '#064');

  Gerador.wCampo(tcStr, '#065', 'xNome', 02, 60, 1, BPe.Agencia.xNome, DSC_XNOME);
  Gerador.wCampoCNPJ('#066', BPe.Agencia.CNPJ, CODIGO_BRASIL, True);

  GerarEnderAgencia;

  Gerador.wGrupo('/agencia');
end;

procedure TBPeW.GerarEnderAgencia;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, BPe.Agencia.enderAgencia.UF,
                     BPe.Agencia.enderAgencia.xMun, BPe.Agencia.EnderAgencia.cMun);

  Gerador.wGrupo('enderAgencia', '#067');

  Gerador.wCampo(tcStr, '#068', 'xLgr   ', 02, 60, 1, BPe.Agencia.enderAgencia.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#069', 'nro    ', 01, 60, 1, BPe.Agencia.enderAgencia.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#070', 'xCpl   ', 01, 60, 0, BPe.Agencia.enderAgencia.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#071', 'xBairro', 02, 60, 1, BPe.Agencia.enderAgencia.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#072', 'cMun   ', 07, 07, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(cMun) then
    Gerador.wAlerta('#072', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#073', 'xMun', 02, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#074', 'CEP ', 08, 08, 0, BPe.Agencia.enderAgencia.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#075', 'UF  ', 02, 02, 1, xUF, DSC_UF);

  if not ValidarUF(xUF) then
    Gerador.wAlerta('#075', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#075a', 'cPais', 01, 04, 0, IIf(BPe.Agencia.enderAgencia.cPais <> 0, IntToStrZero(BPe.Agencia.enderAgencia.cPais,4), ''), DSC_CPAIS);

  if not ValidarCodigoPais(BPe.Agencia.enderAgencia.cPais) = -1 then
    Gerador.wAlerta('#060', 'cPais', DSC_CPAIS, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#075b', 'xPais', 01, 60, 0, BPe.Agencia.enderAgencia.xPais, DSC_XPAIS);
  Gerador.wCampo(tcStr, '#076',  'fone ', 07, 12, 0, OnlyNumber(BPe.Agencia.enderAgencia.fone), DSC_FONE);
  Gerador.wCampo(tcStr, '#077',  'email', 01, 60, 0, BPe.Agencia.enderAgencia.Email, DSC_EMAIL);

  Gerador.wGrupo('/enderAgencia');
end;

procedure TBPeW.GerarinfBPeSub;
begin
  Gerador.wGrupo('infBPeSub', '#078');

  Gerador.wCampo(tcStr, '#079', 'chBPe', 44, 44, 1, BPe.infBPeSub.chBPe, DSC_CHBPE);
  Gerador.wCampo(tcStr, '#080', 'tpSub', 01, 01, 1, tpSubstituicaoToStr(BPe.infBpeSub.tpSub), DSC_TPSUB);

  Gerador.wGrupo('/infBPeSub');
end;

procedure TBPeW.GerarinfPassagem;
begin
  Gerador.wGrupo('infPassagem', '#081');

  Gerador.wCampo(tcStr, '#082', 'cLocOrig  ', 01, 07, 1, BPe.infPassagem.cLocOrig, DSC_CLOCORIG);
  Gerador.wCampo(tcStr, '#083', 'xLocOrig  ', 02, 60, 1, BPe.infPassagem.xLocOrig, DSC_XLOCORIG);
  Gerador.wCampo(tcStr, '#084', 'cLocDest  ', 01, 07, 1, BPe.infPassagem.cLocDest, DSC_CLOCDEST);
  Gerador.wCampo(tcStr, '#085', 'xLocDest  ', 02, 60, 1, BPe.infPassagem.xLocDest, DSC_XLOCDEST);
  Gerador.wCampo(tcStr, '#086', 'dhEmb     ', 25, 25, 1, DateTimeTodh(BPe.infPassagem.dhEmb) + GetUTC(CodigoParaUF(BPe.ide.cUF), BPe.infPassagem.dhEmb), DSC_DHEMB);
  Gerador.wCampo(tcStr, '#087', 'dhValidade', 25, 25, 1, DateTimeTodh(BPe.infPassagem.dhValidade) + GetUTC(CodigoParaUF(BPe.ide.cUF), BPe.infPassagem.dhValidade), DSC_DHVALIDADE);

  if BPe.infPassagem.infPassageiro.xNome <> '' then
    GerarinfPassageiro;

  Gerador.wGrupo('/infPassagem');
end;

procedure TBPeW.GerarinfRespTec;
begin
  if (BPe.infRespTec.CNPJ <> '') then
  begin
    Gerador.wGrupo('infRespTec', '#081');
    Gerador.wCampoCNPJ('#82', BPe.infRespTec.CNPJ, CODIGO_BRASIL, True);
    Gerador.wCampo(tcStr, '#083', 'xContato', 02, 60, 1, BPe.infRespTec.xContato, DSC_XCONTATO);
    Gerador.wCampo(tcStr, '#084', 'email   ', 06, 60, 1, BPe.infRespTec.email, DSC_EMAIL);
    Gerador.wCampo(tcStr, '#085', 'fone    ', 07, 12, 1, BPe.infRespTec.fone, DSC_FONE);

    if (idCSRT <> 0) and (CSRT <> '') then
    begin
      Gerador.wCampo(tcInt, '#086', 'idCSRT  ', 02, 02, 1, idCSRT, DSC_IDCSRT);
      Gerador.wCampo(tcStr, '#087', 'hashCSRT', 28, 28, 1, CalcularHashCSRT(CSRT, FChaveBPe), DSC_HASHCSRT);
    end;

    Gerador.wGrupo('/infRespTec');
  end;
end;

procedure TBPeW.GerarinfPassageiro;
begin
  Gerador.wGrupo('infPassageiro', '#087');

  Gerador.wCampo(tcStr, '#088', 'xNome', 02, 060, 1, BPe.infPassagem.infPassageiro.xNome, DSC_XNOMEPASS);
  Gerador.wCampoCPF('#089', BPe.infPassagem.infPassageiro.CPF, CODIGO_BRASIL, False);
  Gerador.wCampo(tcStr, '#090', 'tpDoc', 01, 001, 1, tpDocumentoToStr(BPe.infPassagem.infPassageiro.tpDoc), DSC_TPDOC);
  Gerador.wCampo(tcStr, '#091', 'nDoc ', 02, 020, 1, BPe.infPassagem.infPassageiro.nDoc, DSC_NDOC);
  Gerador.wCampo(tcStr, '#091', 'xDoc ', 02, 100, 0, BPe.infPassagem.infPassageiro.xDoc, DSC_XDOC);
  Gerador.wCampo(tcDat, '#092', 'dNasc', 10, 010, 0, BPe.infPassagem.infPassageiro.dNasc, DSC_DNASC);
  Gerador.wCampo(tcStr, '#093', 'fone ', 07, 012, 0, OnlyNumber(BPe.infPassagem.infPassageiro.Fone), DSC_FONE);
  Gerador.wCampo(tcStr, '#094', 'email', 01, 060, 0, BPe.infPassagem.infPassageiro.Email, DSC_EMAIL);

  Gerador.wGrupo('/infPassageiro');
end;

procedure TBPeW.GerarinfViagem;
var
  i: Integer;
begin
  for i := 0 to BPe.infViagem.Count - 1 do
  begin
    Gerador.wGrupo('infViagem', '#095');

    Gerador.wCampo(tcStr, '#096', 'cPercurso   ', 01,  20, 1, BPe.infViagem[i].cPercurso, DSC_CPERCURSO);
    Gerador.wCampo(tcStr, '#097', 'xPercurso   ', 02, 100, 1, BPe.infViagem[i].xPercurso, DSC_XPERCURSO);
    Gerador.wCampo(tcStr, '#098', 'tpViagem    ', 02,  02, 1, tpViagemToStr(BPe.infViagem[i].tpViagem), DSC_TPVIAGEM);
    Gerador.wCampo(tcStr, '#099', 'tpServ      ', 01,  02, 1, tpServicoToStr(BPe.infViagem[i].tpServ), DSC_TPSERVICO);
    Gerador.wCampo(tcStr, '#100', 'tpAcomodacao', 01,  01, 1, tpAcomodacaoToStr(BPe.infViagem[i].tpAcomodacao), DSC_TPACOMODACAO);
    Gerador.wCampo(tcStr, '#101', 'tpTrecho    ', 01,  01, 1, tpTrechoToStr(BPe.infViagem[i].tpTrecho), DSC_TPTRECHO);

    Gerador.wCampo(tcStr, '#102a', 'dhViagem', 25, 25, 1, DateTimeTodh(BPe.infViagem[i].dhViagem) + GetUTC(CodigoParaUF(BPe.ide.cUF), BPe.infViagem[i].dhViagem), DSC_DHVIAGEM);

    if BPe.infViagem[i].tpTrecho = ttConexao then
      Gerador.wCampo(tcStr, '#102', 'dhConexao', 25, 25, 0, DateTimeTodh(BPe.infViagem[i].dhConexao) + GetUTC(CodigoParaUF(BPe.ide.cUF), BPe.infViagem[i].dhConexao), DSC_DHCONEXAO);

    Gerador.wCampo(tcStr, '#103', 'prefixo   ', 01, 20, 0, BPe.infViagem[i].Prefixo, DSC_PREFIXO);
    Gerador.wCampo(tcInt, '#104', 'poltrona  ', 01, 03, 0, BPe.infViagem[i].Poltrona, DSC_POLTRONA);
    Gerador.wCampo(tcStr, '#105', 'plataforma', 01, 10, 0, BPe.infViagem[i].Plataforma, DSC_PLATAFORMA);

    if BPe.infViagem[i].infTravessia.tpVeiculo <> tvNenhum then
    begin
      Gerador.wGrupo('infTravessia', '#106');
      Gerador.wCampo(tcStr, '#107', 'tpVeiculo ', 02, 02, 1, tpVeiculoToStr(BPe.infViagem[i].infTravessia.tpVeiculo), DSC_TPVEICULO);
      Gerador.wCampo(tcStr, '#108', 'sitVeiculo', 01, 01, 1, SitVeiculoToStr(BPe.infViagem[i].infTravessia.sitVeiculo), DSC_SITVEICULO);
      Gerador.wGrupo('/infTravessia');
    end;

    Gerador.wGrupo('/infViagem');
  end;

  if BPe.infViagem.Count > 990 then
    Gerador.wAlerta('095', 'infViagem', DSC_INFVIAGEM, ERR_MSG_MAIOR_MAXIMO + '990');
  if BPe.infViagem.Count < 1 then
    Gerador.wAlerta('095', 'infViagem', DSC_INFVIAGEM, ERR_MSG_MENOR_MINIMO + '1');
end;

procedure TBPeW.GerarinfValorBPe;
var
  i: Integer;
begin
  Gerador.wGrupo('infValorBPe', '#109');

  Gerador.wCampo(tcDe2, '#110', 'vBP       ', 00,  15, 1, BPe.infValorBPe.vBP, DSC_VBP);
  Gerador.wCampo(tcDe2, '#111', 'vDesconto ', 00,  15, 1, BPe.infValorBPe.vDesconto, DSC_VDESCONTO);
  Gerador.wCampo(tcDe2, '#112', 'vPgto     ', 00,  15, 1, BPe.infValorBPe.vPgto, DSC_VPGTO);
  Gerador.wCampo(tcDe2, '#113', 'vTroco    ', 00,  15, 1, BPe.infValorBPe.vTroco, DSC_VTROCO);
  Gerador.wCampo(tcStr, '#114', 'tpDesconto', 02,  02, 0, tpDescontoToStr(BPe.infValorBPe.tpDesconto), DSC_TPDESCONTO);
  Gerador.wCampo(tcStr, '#115', 'xDesconto ', 02, 100, 0, BPe.infValorBPe.xDesconto, DSC_XDESCONTO);
  Gerador.wCampo(tcStr, '#115', 'cDesconto ', 02, 020, 0, BPe.infValorBPe.cDesconto, DSC_CDESCONTO);

  for i := 0 to BPe.infValorBPe.Comp.Count - 1 do
  begin
    Gerador.wGrupo('Comp', '#116');

    Gerador.wCampo(tcStr, '#117', 'tpComp', 02, 02, 1, tpComponenteToStr(BPe.infValorBPe.Comp[i].tpComp), DSC_TPCOMP);
    Gerador.wCampo(tcDe2, '#118', 'vComp ', 00, 15, 1, BPe.infValorBPe.Comp[i].vComp, DSC_VCOMP);

    Gerador.wGrupo('/Comp');
  end;

  if BPe.infValorBPe.Comp.Count > 990 then
    Gerador.wAlerta('116', 'Comp', DSC_COMP, ERR_MSG_MAIOR_MAXIMO + '990');
  if BPe.infValorBPe.Comp.Count < 1 then
    Gerador.wAlerta('116', 'Comp', DSC_COMP, ERR_MSG_MENOR_MINIMO + '1');

  Gerador.wGrupo('/infValorBPe');
end;

procedure TBPeW.GerarImp;
var
  sTagTemp : String;

  function BuscaTag(const t: TpcnCSTIcms): String;
  begin
    case t of
      cst00: result := '00';

      cst20: result := '20';

      cst40,
      cst41,
      cst51: result := '45';

      cst90: result := '90';

      cstICMSOutraUF: result := 'OutraUF';
    end;
  end;
begin
  Gerador.wGrupo('imp', '#119');

  Gerador.wGrupo('ICMS', '#120');

  case BPe.Emit.CRT of
    crtRegimeNormal, crtSimplesExcessoReceita :
      begin
        sTagTemp := BuscaTag( BPe.Imp.ICMS.CST );

        Gerador.wGrupo('ICMS' + sTagTemp, 'N' + CSTICMSTOStrTagPos(BPe.Imp.ICMS.CST));
        Gerador.wCampo(tcStr, '#122', 'CST', 02, 02, 1, CSTICMSTOStr(BPe.Imp.ICMS.CST), DSC_CST);

        case BPe.Imp.ICMS.CST of
          cst00: begin
                   Gerador.wCampo(tcDe2, '#123', 'vBC  ', 01, 15, 1, BPe.Imp.ICMS.vBC, DSC_VBC);
                   Gerador.wCampo(tcDe2, '#124', 'pICMS', 01, 05, 1, BPe.Imp.ICMS.pICMS, DSC_PICMS);
                   Gerador.wCampo(tcDe2, '#125', 'vICMS', 01, 15, 1, BPe.Imp.ICMS.vICMS, DSC_VICMS);
                 end;
          cst20: begin
                   Gerador.wCampo(tcDe2, '#126', 'pRedBC', 01, 05, 1, BPe.Imp.ICMS.pRedBC, DSC_PREDBC);
                   Gerador.wCampo(tcDe2, '#127', 'vBC   ', 01, 15, 1, BPe.Imp.ICMS.vBC, DSC_VBC);
                   Gerador.wCampo(tcDe2, '#128', 'pICMS ', 01, 05, 1, BPe.Imp.ICMS.pICMS, DSC_PICMS);
                   Gerador.wCampo(tcDe2, '#129', 'vICMS ', 01, 15, 1, BPe.Imp.ICMS.vICMS, DSC_VICMS);
                 end;
          cst40,
          cst41,
          cst51: begin
                   //Esse bloco só contem o CST
                 end;
          cst90: begin
                   Gerador.wCampo(tcDe2, '#135', 'pRedBC', 01, 05, 0, BPe.Imp.ICMS.pRedBC, DSC_PREDBC);
                   Gerador.wCampo(tcDe2, '#136', 'vBC   ', 01, 15, 1, BPe.Imp.ICMS.vBC, DSC_VBC);
                   Gerador.wCampo(tcDe2, '#137', 'pICMS ', 01, 05, 1, BPe.Imp.ICMS.pICMS, DSC_PICMS);
                   Gerador.wCampo(tcDe2, '#138', 'vICMS ', 01, 15, 1, BPe.Imp.ICMS.vICMS, DSC_VICMS);
                   Gerador.wCampo(tcDe2, '#139', 'vCred ', 01, 15, 0, BPe.Imp.ICMS.vCred, DSC_VCRED);
                 end;
          (* Conforme a NT 2018/002 não se deve mais gerar.
          cstICMSOutraUF: begin
                            Gerador.wCampo(tcDe2, '#143', 'pRedBCOutraUF', 01, 05, 0, BPe.Imp.ICMS.pRedBCOutraUF, DSC_PREDBC);
                            Gerador.wCampo(tcDe2, '#144', 'vBCOutraUF   ', 01, 15, 1, BPe.Imp.ICMS.vBCOutraUF, DSC_VBC);
                            Gerador.wCampo(tcDe2, '#145', 'pICMSOutraUF ', 01, 05, 1, BPe.Imp.ICMS.pICMSOutraUF, DSC_PICMS);
                            Gerador.wCampo(tcDe2, '#146', 'vICMSOutraUF ', 01, 15, 1, BPe.Imp.ICMS.vICMSOutraUF, DSC_VICMS);
                          end;
          *)
        end;

        Gerador.wGrupo('/ICMS' + sTagTemp );
      end;
    crtSimplesNacional:
      begin
        //Grupo do Simples Nacional
        Gerador.wGrupo('ICMSSN', '#147');
        Gerador.wCampo(tcStr, '#148', 'CST  ', 02, 02, 1, '90', DSC_CST);
        Gerador.wCampo(tcStr, '#149', 'indSN', 01, 01, 1, '1', '');
        Gerador.wGrupo('/ICMSSN');
      end;
	end;
  Gerador.wGrupo('/ICMS');

  Gerador.wCampo(tcDe2, '#150', 'vTotTrib  ', 01,   15, 0, BPe.Imp.vTotTrib, DSC_VTOTTRIB);
  Gerador.wCampo(tcStr, '#151', 'infAdFisco', 01, 2000, 0, BPe.Imp.infAdFisco, DSC_INFADFISCO);

  if BPe.Imp.ICMSUFFim.vBCUFFim > 0.0 then
    GerarImpICMSUFFim;

  Gerador.wGrupo('/imp');
end;

procedure TBPeW.GerarImpICMSUFFim;
begin
  Gerador.wGrupo('ICMSUFFim', '#152');

  Gerador.wCampo(tcDe2, '#153', 'vBCUFFim      ', 01, 15, 1, BPe.Imp.ICMSUFFim.vBCUFFim, DSC_VBCUFFIM);
  Gerador.wCampo(tcDe2, '#154', 'pFCPUFFim     ', 01, 05, 1, BPe.Imp.ICMSUFFim.pFCPUFFim, DSC_PFCPUFFIM);
  Gerador.wCampo(tcDe2, '#155', 'pICMSUFFim    ', 01, 05, 1, BPe.Imp.ICMSUFFim.pICMSUFFim, DSC_PICMSUFFIM);
  Gerador.wCampo(tcDe2, '#156', 'pICMSInter    ', 01, 05, 1, BPe.Imp.ICMSUFFim.pICMSInter, DSC_PICMSINTER);
//  Gerador.wCampo(tcDe2, '#157', 'pICMSInterPart', 01, 05, 1, BPe.Imp.ICMSUFFim.pICMSInterPart, DSC_PICMSINTERPART);
  Gerador.wCampo(tcDe2, '#158', 'vFCPUFFim     ', 01, 15, 1, BPe.Imp.ICMSUFFim.vFCPUFFim, DSC_VFCPUFFIM);
  Gerador.wCampo(tcDe2, '#159', 'vICMSUFFim    ', 01, 15, 1, BPe.Imp.ICMSUFFim.vICMSUFFim, DSC_VICMSUFFIM);
  Gerador.wCampo(tcDe2, '#160', 'vICMSUFIni    ', 01, 15, 1, BPe.Imp.ICMSUFFim.vICMSUFIni, DSC_VICMSUFINI);

  Gerador.wGrupo('/ICMSUFFim');
end;

procedure TBPeW.Gerarpag;
var
  i: Integer;
begin
  for i := 0 to BPe.pag.Count - 1 do
  begin
    Gerador.wGrupo('pag', '#161');

    Gerador.wCampo(tcStr, '#162', 'tPag    ', 02, 002, 1, FormaPagamentoBPeToStr(BPe.pag[i].tPag), DSC_TPAG);
    Gerador.wCampo(tcStr, '#162a', 'xPag   ', 02, 100, 0, BPe.pag[i].xPag, DSC_XPAG);
    Gerador.wCampo(tcStr, '#162b', 'nDocPag', 02, 020, 0, BPe.pag[i].nDocPag, DSC_NDOCPAG);
    Gerador.wCampo(tcDe2, '#163', 'vPag    ', 01, 015, 1, BPe.pag[i].vPag, DSC_VPAG);

    if(BPe.pag[i].tPag in [fpCartaoDebito, fpCartaoCredito]) and
      ((BPe.pag[i].CNPJ <> '') or (BPe.pag[i].tpIntegra <> tiNaoInformado))then
    begin
      Gerador.wGrupo('card', '#164');
      Gerador.wCampo(tcStr, '#165', 'tpIntegra', 01, 01, 1, tpIntegraToStr(BPe.pag[i].tpIntegra), DSC_TPINTEGRA);

      if BPe.pag[i].CNPJ <> '' then
      begin
        Gerador.wCampo(tcStr, 'YA05', 'CNPJ ', 14, 14, 0, BPe.pag[i].CNPJ, DSC_CNPJ);
        Gerador.wCampo(tcStr, 'YA06', 'tBand', 02, 02, 0, BandeiraCardToStr(BPe.pag[i].tBand), DSC_TBAND);
        Gerador.wCampo(tcStr, 'YA06a', 'xBand', 02, 100, 0, BPe.pag[i].xBand, DSC_XBAND);
        Gerador.wCampo(tcStr, 'YA07', 'cAut ', 01, 20, 0, BPe.pag[i].cAut, DSC_CAUT);
        Gerador.wCampo(tcStr, 'YA07', 'nsuTrans', 01, 20, 0, BPe.pag[i].nsuTrans, DSC_NSUTRANS);
        Gerador.wCampo(tcStr, 'YA07', 'nsuHost', 01, 20, 0, BPe.pag[i].nsuHost, DSC_NSUHOST);
        Gerador.wCampo(tcInt, 'YA07', 'nParcelas', 01, 03, 0, BPe.pag[i].nParcelas, DSC_NPARCELAS);
        Gerador.wCampo(tcStr, 'YA07', 'infAdCard', 01, 2000, 0, BPe.pag[i].infAdCard, DSC_INFADCARD);
      end;

      Gerador.wGrupo('/card');
    end;

    Gerador.wGrupo('/pag');
  end;

  if BPe.pag.Count > 10 then
    Gerador.wAlerta('#161', 'pag', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TBPeW.GerarDetBPeTM;
var
  i, j, k: Integer;
begin
  for i := 0 to BPe.detBPeTM.Count - 1 do
  begin
    Gerador.wGrupo('detBPeTM idEqpCont="' + IntToStr(BPe.detBPeTM[i].idEqpCont) + '"', '#42');

    Gerador.wCampo(tcStr, '#44', 'UFIniViagem', 2, 02, 1, BPe.detBPeTM[i].UFIniViagem, DSC_UFINIVIAGEM);
    Gerador.wCampo(tcStr, '#45', 'UFFimViagem', 2, 02, 0, BPe.detBPeTM[i].UFFimViagem, DSC_UFFIMVIAGEM);
    Gerador.wCampo(tcStr, '#46', 'placa      ', 7, 07, 0, BPe.detBPeTM[i].placa, DSC_PLACA);
    Gerador.wCampo(tcStr, '#47', 'prefixo    ', 1, 20, 0, BPe.detBPeTM[i].prefixo, DSC_PREFIXO);

    for j := 0 to BPe.detBPeTM[i].det.Count - 1 do
    begin
      Gerador.wGrupo('det nViagem="' + IntToStr(BPe.detBPeTM[i].det[j].nViagem) + '"', '#48');

      Gerador.wCampo(tcInt, '#50', 'cMunIni    ', 7, 07, 1, BPe.detBPeTM[i].det[j].cMunIni, DSC_CMUNINIBPE);
      Gerador.wCampo(tcInt, '#51', 'cMunFim    ', 7, 07, 0, BPe.detBPeTM[i].det[j].cMunFim, DSC_CMUNFIMBPE);
      Gerador.wCampo(tcStr, '#52', 'nContInicio', 1, 20, 1, BPe.detBPeTM[i].det[j].nContInicio, DSC_NCONTINICIO);
      Gerador.wCampo(tcStr, '#53', 'nContFim   ', 1, 20, 1, BPe.detBPeTM[i].det[j].nContFim, DSC_NCONTFIM);
      Gerador.wCampo(tcStr, '#54', 'qPass      ', 1, 20, 1, BPe.detBPeTM[i].det[j].qPass, DSC_QPASS);
      Gerador.wCampo(tcDe2, '#55', 'vBP        ', 1, 15, 1, BPe.detBPeTM[i].det[j].vBP, DSC_VBP);

      GerarDetImp(i, j);

      for k := 0 to BPe.detBPeTM[i].det[j].Comp.Count - 1 do
      begin
        Gerador.wGrupo('Comp', '#82');
        Gerador.wCampo(tcStr, '#83', 'xNome', 1, 15, 1, BPe.detBPeTM[i].det[j].Comp[k].xNome, DSC_XNOME);
        Gerador.wCampo(tcInt, '#84', 'qComp', 5, 05, 1, BPe.detBPeTM[i].det[j].Comp[k].qComp, DSC_QCOMP);
        Gerador.wGrupo('/Comp');
      end;

      Gerador.wGrupo('/det');
    end;

    Gerador.wGrupo('/detBPeTM');
  end;

  if BPe.detBPeTM.Count > 99 then
    Gerador.wAlerta('#42', 'detBPeTM', '', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TBPeW.GerarDetImp(i, j: Integer);
var
  sTagTemp : String;

  function BuscaTag(const t: TpcnCSTIcms): String;
  begin
    case t of
      cst00: result := '00';

      cst20: result := '20';

      cst40,
      cst41,
      cst51: result := '45';

      cst90: result := '90';

      cstICMSOutraUF: result := 'OutraUF';
    end;
  end;
begin
  Gerador.wGrupo('imp', '#119');

  Gerador.wGrupo('ICMS', '#120');

  case BPe.Emit.CRT of
    crtRegimeNormal, crtSimplesExcessoReceita :
      begin
        sTagTemp := BuscaTag( BPe.detBPeTM[i].det[j].Imp.ICMS.CST );

        Gerador.wGrupo('ICMS' + sTagTemp, 'N' + CSTICMSTOStrTagPos(BPe.detBPeTM[i].det[j].Imp.ICMS.CST));
        Gerador.wCampo(tcStr, '#122', 'CST', 02, 02, 1, CSTICMSTOStr(BPe.detBPeTM[i].det[j].Imp.ICMS.CST), DSC_CST);

        case BPe.Imp.ICMS.CST of
          cst00: begin
                   Gerador.wCampo(tcDe2, '#123', 'vBC  ', 01, 15, 1, BPe.detBPeTM[i].det[j].Imp.ICMS.vBC, DSC_VBC);
                   Gerador.wCampo(tcDe2, '#124', 'pICMS', 01, 05, 1, BPe.detBPeTM[i].det[j].Imp.ICMS.pICMS, DSC_PICMS);
                   Gerador.wCampo(tcDe2, '#125', 'vICMS', 01, 15, 1, BPe.detBPeTM[i].det[j].Imp.ICMS.vICMS, DSC_VICMS);
                 end;
          cst20: begin
                   Gerador.wCampo(tcDe2, '#126', 'pRedBC', 01, 05, 1, BPe.detBPeTM[i].det[j].Imp.ICMS.pRedBC, DSC_PREDBC);
                   Gerador.wCampo(tcDe2, '#127', 'vBC   ', 01, 15, 1, BPe.detBPeTM[i].det[j].Imp.ICMS.vBC, DSC_VBC);
                   Gerador.wCampo(tcDe2, '#128', 'pICMS ', 01, 05, 1, BPe.detBPeTM[i].det[j].Imp.ICMS.pICMS, DSC_PICMS);
                   Gerador.wCampo(tcDe2, '#129', 'vICMS ', 01, 15, 1, BPe.detBPeTM[i].det[j].Imp.ICMS.vICMS, DSC_VICMS);
                 end;
          cst40,
          cst41,
          cst51: begin
                   //Esse bloco só contem o CST
                 end;
          cst90: begin
                   Gerador.wCampo(tcDe2, '#135', 'pRedBC', 01, 05, 0, BPe.detBPeTM[i].det[j].Imp.ICMS.pRedBC, DSC_PREDBC);
                   Gerador.wCampo(tcDe2, '#136', 'vBC   ', 01, 15, 1, BPe.detBPeTM[i].det[j].Imp.ICMS.vBC, DSC_VBC);
                   Gerador.wCampo(tcDe2, '#137', 'pICMS ', 01, 05, 1, BPe.detBPeTM[i].det[j].Imp.ICMS.pICMS, DSC_PICMS);
                   Gerador.wCampo(tcDe2, '#138', 'vICMS ', 01, 15, 1, BPe.detBPeTM[i].det[j].Imp.ICMS.vICMS, DSC_VICMS);
                   Gerador.wCampo(tcDe2, '#139', 'vCred ', 01, 15, 0, BPe.detBPeTM[i].det[j].Imp.ICMS.vCred, DSC_VCRED);
                 end;
        end;

        Gerador.wGrupo('/ICMS' + sTagTemp );
      end;
    crtSimplesNacional:
      begin
        //Grupo do Simples Nacional
        Gerador.wGrupo('ICMSSN', '#147');
        Gerador.wCampo(tcStr, '#148', 'CST  ', 02, 02, 1, '90', DSC_CST);
        Gerador.wCampo(tcStr, '#149', 'indSN', 01, 01, 1, '1', '');
        Gerador.wGrupo('/ICMSSN');
      end;
	end;
  Gerador.wGrupo('/ICMS');

  Gerador.wCampo(tcStr, '#151', 'infAdFisco', 01, 2000, 0, BPe.detBPeTM[i].det[j].Imp.infAdFisco, DSC_INFADFISCO);

  Gerador.wGrupo('/imp');
end;

procedure TBPeW.GerarTotal;
begin
  Gerador.wGrupo('total', '#85');

  Gerador.wCampo(tcInt, '#86', 'qPass', 01, 06, 1, BPe.total.qPass, DSC_QPASS);
  Gerador.wCampo(tcDe2, '#87', 'vBP  ', 01, 15, 1, BPe.total.vBP, DSC_VBP);

  Gerador.wGrupo('ICMSTot', '#88');
  Gerador.wCampo(tcDe2, '#89', 'vBC  ', 01, 15, 1, BPe.total.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, '#60', 'vICMS', 01, 15, 1, BPe.total.vICMS, DSC_VICMS);
  Gerador.wGrupo('/ICMSTot');

  Gerador.wGrupo('/total');
end;

procedure TBPeW.GerarautXML;
var
  i: Integer;
begin
  for i := 0 to BPe.autXML.Count - 1 do
  begin
    Gerador.wGrupo('autXML', '#169');
    Gerador.wCampoCNPJCPF('#170', '#171', BPe.autXML[i].CNPJCPF);
    Gerador.wGrupo('/autXML');
  end;

  if BPe.autXML.Count > 10 then
    Gerador.wAlerta('#169', 'autXML', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TBPeW.GerarInfAdic;
begin
  if (trim(BPe.InfAdic.infAdFisco) <> EmptyStr) or
    (trim(BPe.InfAdic.infCpl) <> EmptyStr) then
  begin
    Gerador.wGrupo('infAdic', '#172');
    Gerador.wCampo(tcStr, '#173', 'infAdFisco', 01, 2000, 0, BPe.InfAdic.infAdFisco, DSC_INFADFISCO);
    Gerador.wCampo(tcStr, '#174', 'infCpl    ', 01, 5000, 0, BPe.InfAdic.infCpl, DSC_INFCPL);
    Gerador.wGrupo('/infAdic');
  end;
end;

// Outras //////////////////////////////////////////////////////////////////////

procedure TBPeW.AjustarMunicipioUF(out xUF: String; out xMun: String; out
  cMun: Integer; cPais: Integer; const vxUF, vxMun: String; vcMun: Integer);
var
  PaisBrasil: Boolean;
begin
  PaisBrasil := cPais = CODIGO_BRASIL;
  cMun := IIf(PaisBrasil, vcMun, CMUN_EXTERIOR);
  xMun := IIf(PaisBrasil, vxMun, XMUN_EXTERIOR);
  xUF := IIf(PaisBrasil, vxUF, UF_EXTERIOR);
  if FOpcoes.NormatizarMunicipios then
    if ( ( EstaZerado(cMun)) and (xMun <> XMUN_EXTERIOR) ) then
      cMun := ObterCodigoMunicipio(xMun, xUF, FOpcoes.FPathArquivoMunicipios)
    else if ( ( EstaVazio(xMun)) and (cMun <> CMUN_EXTERIOR) ) then
      xMun := ObterNomeMunicipio(cMun, xUF, FOpcoes.FPathArquivoMunicipios);
end;

end.

