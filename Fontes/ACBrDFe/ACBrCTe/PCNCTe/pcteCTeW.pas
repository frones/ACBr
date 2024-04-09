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

unit pcteCTeW;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, pcteCTe,
  pcteConversaoCTe, pcteConsts, ACBrDFeUtil, ACBrDFeConsts;

type

  TGeradorOpcoes = class(TObject)
  private
    FNormatizarMunicipios: Boolean;
    FGerarTagAssinatura: TpcnTagAssinatura;
    FPathArquivoMunicipios: String;
    FValidarInscricoes: Boolean;
    FValidarListaServicos: Boolean;
  public
    property NormatizarMunicipios: Boolean         read FNormatizarMunicipios  write FNormatizarMunicipios;
    property GerarTagAssinatura: TpcnTagAssinatura read FGerarTagAssinatura    write FGerarTagAssinatura;
    property PathArquivoMunicipios: String         read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: Boolean            read FValidarInscricoes     write FValidarInscricoes;
    property ValidarListaServicos: Boolean         read FValidarListaServicos  write FValidarListaServicos;
  end;

  TCTeW = class(TPersistent)
  private
    FGerador: TGerador;
    FCTe: TCTe;
    FOpcoes: TGeradorOpcoes;

    FChaveCTe: string;

    FVersaoDF: TVersaoCTe;
    FModeloDF: TModeloCTe;
    FtpAmb: TpcnTipoAmbiente;
    FtpEmis: TpcnTipoEmissao;
    FIdCSRT: Integer;
    FCSRT: String;

    procedure GerarInfCTe;     // Nivel 0

    procedure GerarIde;        // Nivel 1
    procedure GerarToma03;     // Nivel 2
    procedure GerarToma4;      // Nivel 2
    procedure GerarinfPercurso;// Nivel 2
    procedure GerarEnderToma;  // Nivel 3

    procedure GerarCompl;      // Nivel 1
    procedure GerarFluxo;      // Nivel 2
    procedure GerarEntrega;    // Nivel 2
    procedure GerarObsCont;    // Nivel 2
    procedure GerarObsFisco;   // Nivel 2

    procedure GerarEmit;       // Nivel 1
    procedure GerarEnderEmit;  // Nivel 2

    procedure GerarTomador;      // Nivel 1 - Somente para o modelo 67 - CTeOS
    procedure GerarEnderTomador; // Nivel 2 - Somente para o modelo 67 - CTeOS

    procedure GerarRem;        // Nivel 1
    procedure GerarEnderReme;  // Nivel 2

    procedure GerarExped;      // Nivel 1
    procedure GerarEnderExped; // Nivel 2

    procedure GerarReceb;      // Nivel 1
    procedure GerarEnderReceb; // Nivel 2

    procedure GerarDest;       // Nivel 1
    procedure GerarEnderDest;  // Nivel 2

    procedure GerarVPrest;     // Nivel 1
    procedure GerarComp;       // Nivel 2

    procedure GerarImp;        // Nivel 1
    procedure GerarICMS;       // Nivel 2
    procedure GerarCST00;      // Nivel 3
    procedure GerarCST20;      // Nivel 3
    procedure GerarCST45;      // Nivel 3
    procedure GerarCST60;      // Nivel 3
    procedure GerarCST90;      // Nivel 3
    procedure GerarICMSOutraUF;// Nivel 3
    procedure GerarICMSSN;     // Nivel 3
    procedure GerarICMSUFFim;  // Nivel 2
    procedure GerarinfTribFed; // Nivel 2

    procedure GerarInfCTeNorm; // Nivel 1
    procedure GerarinfServico; // Nivel 2 - Somente para o modelo 67 - CTeOS
    procedure GerarinfDocRef;  // Nivel 2 - Somente para o modelo 67 - CTeOS
    procedure GerarinfCarga;   // Nivel 2
    procedure GerarInfQ;       // Nivel 3
    procedure GerarinfDoc;     // Nivel 2
    procedure GerarInfNF;      // Nivel 3
    procedure GerarInfNFe;     // Nivel 3
    procedure GerarInfOutros;  // Nivel 3

    procedure GerarDocAnt;     // Nivel 2
    procedure GerarInfSeg;     // Nivel 2

    procedure GerarRodo;       // Nivel 2
    procedure GerarOCC;        // Nivel 3
    procedure GerarValePed;    // Nivel 3
    procedure GerarVeic;       // Nivel 3
    procedure GerarLacre;      // Nivel 3
    procedure GerarMoto;       // Nivel 3

    procedure GerarRodoOS;     // Nivel 2

    procedure GerarAereo;      // Nivel 2

    procedure GerarAquav;      // Nivel 2

    procedure GerarFerrov;     // Nivel 2
    procedure GerarFerroEnv;   // Nivel 3
    procedure GerardetVag;     // Nivel 3

    procedure GerarDuto;       // Nivel 2

    procedure GerarMultimodal; // Nivel 2

    procedure GerarPeri;       // Nivel 2
    procedure GerarVeicNovos;  // Nivel 2
    procedure GerarCobr;       // Nivel 2
    procedure GerarCobrFat;
    procedure GerarCobrDup;
    procedure GerarInfGTVe;    // Nivel 2

    procedure GerarInfCTeSub;  // Nivel 2
    procedure GerarInfGlobalizado;  // Nivel 2
    procedure GerarInfServVinc;  // Nivel 2

    procedure GerarInfCTeComp; // Nivel 1

    procedure GerarInfCTeAnu;  // Nivel 1
    procedure GerarautXML;     // Nivel 1
    procedure GerarinfRespTec; // Nivel 1

    procedure GerarOrigem; // Nivel 1
    procedure GerarDestino; // Nivel 1
    procedure GerardetGTV; // Nivel 1

    procedure AjustarMunicipioUF(var xUF: String; var xMun: String; var cMun: Integer; cPais: Integer; const vxUF, vxMun: String; vcMun: Integer);
    function DefineArredondamentoQtdRat: TpcnTipoCampo;

  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;

    function GerarXml: boolean;
  published
    property Gerador: TGerador      read FGerador  write FGerador;
    property CTe: TCTe              read FCTe      write FCTe;
    property Opcoes: TGeradorOpcoes read FOpcoes   write FOpcoes;

    property VersaoDF: TVersaoCTe read FVersaoDF write FVersaoDF;
    property ModeloDF: TModeloCTe read FModeloDF write FModeloDF;
    property tpAmb: TpcnTipoAmbiente read FtpAmb write FtpAmb;
    property tpEmis: TpcnTipoEmissao read FtpEmis write FtpEmis;
    property IdCSRT: Integer read FIdCSRT write FIdCSRT;
    property CSRT: String read FCSRT write FCSRT;
  end;

implementation

uses
  StrUtils,
  Math,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.Strings;

{ TCTeW }

constructor TCTeW.Create(AOwner: TCTe);
begin
  inherited Create;

  FCTe     := AOwner;
  FGerador := TGerador.Create;

  FGerador.FIgnorarTagNivel   := '|?xml version|CTe xmlns|infCte versao|ObsCont|ObsFisco|';
  FGerador.Opcoes.QuebraLinha := ';';

  FOpcoes := TGeradorOpcoes.Create;

  FOpcoes.FNormatizarMunicipios := False;
  FOpcoes.FGerarTagAssinatura   := taSomenteSeAssinada;
  FOpcoes.FValidarInscricoes    := False;
  FOpcoes.FValidarListaServicos := False;
end;

function TCTeW.DefineArredondamentoQtdRat: TpcnTipoCampo;
begin
  if VersaoDF <= ve300 then
    Result := tcDe2
  else
    Result := tcDe3;
end;

destructor TCTeW.Destroy;
begin
  FGerador.Free;
  FOpcoes.Free;
  inherited Destroy;
end;

function TCTeW.GerarXml: Boolean;
var
  Gerar, Ok: Boolean;
  xProtCTe, VersaoStr: String;
begin
  // Carrega Layout que sera utilizado para gera o txt
  Gerador.ListaDeAlertas.Clear;
  Gerador.LayoutArquivoTXT.Clear;
  Gerador.ArquivoFormatoXML := '';
  Gerador.ArquivoFormatoTXT := '';

  {
    Os campos abaixo tem que ser os mesmos da configuração
  }
  CTe.infCTe.Versao := VersaoCTeToDbl(VersaoDF);
  CTe.Ide.modelo := StrToInt(ModeloCTeToStr(ModeloDF));
  CTe.Ide.tpAmb := tpAmb;
  CTe.ide.tpEmis := tpEmis;

  VersaoStr := 'versao="' + FloatToString(CTe.infCTe.Versao, '.', '#0.00') + '"';

  FChaveCTe := GerarChaveAcesso(CTe.ide.cUF, CTe.ide.dhEmi, CTe.emit.CNPJ, CTe.ide.serie,
                            CTe.ide.nCT, StrToInt(TpEmisToStr(CTe.ide.tpEmis)),
                            CTe.ide.cCT, CTe.ide.modelo);
  CTe.infCTe.Id := 'CTe' + FChaveCTe;

  CTe.ide.cDV := ExtrairDigitoChaveAcesso(CTe.infCTe.ID);
  CTe.Ide.cCT := ExtrairCodigoChaveAcesso(CTe.infCTe.ID);

  {$IfDef FPC}
   Gerador.wGrupo(ENCODING_UTF8, '', False);
  {$EndIf}

  if CTe.procCTe.nProt <> '' then
  begin
    case ModeloDF of
      moGTVe: Gerador.wGrupo('GTVeProc ' + VersaoStr + ' ' + NAME_SPACE_CTE, '');
      moCTeOS: Gerador.wGrupo('cteOSProc ' + VersaoStr + ' ' + NAME_SPACE_CTE, '');
    else
      Gerador.wGrupo('cteProc ' + VersaoStr + ' ' + NAME_SPACE_CTE, '');
    end;
  end;

  case ModeloDF of
    moGTVe: Gerador.wGrupo('GTVe ' + NAME_SPACE_CTE + ' ' + VersaoStr);
    moCTeOS: Gerador.wGrupo('CTeOS ' + NAME_SPACE_CTE + ' ' + VersaoStr);
  else
    Gerador.wGrupo('CTe ' + NAME_SPACE_CTE);
  end;

  Gerador.wGrupo('infCte ' + VersaoStr + ' Id="' + CTe.infCTe.ID + '"');

  GerarInfCTe;
  Gerador.wGrupo('/infCte');

  if CTe.infCTeSupl.qrCodCTe <> '' then
  begin
    Gerador.wGrupo('infCTeSupl');
//    Gerador.wCampo(tcStr, '#196', 'qrCodCTe', 50, 1000, 1,
//                               CTe.infCTeSupl.qrCodCTe, DSC_INFQRCODCTE);
    Gerador.wCampo(tcStr, '#196', 'qrCodCTe', 50, 1000, 1,
                     '<![CDATA[' + CTe.infCTeSupl.qrCodCTe + ']]>', DSC_INFQRCODCTE, False);
    Gerador.wGrupo('/infCTeSupl');
  end;

  if FOpcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := true;

    if FOpcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((trim(CTe.signature.DigestValue) <> '') and
                (trim(CTe.signature.SignatureValue) <> '') and
                (trim(CTe.signature.X509Certificate) <> ''));

    if FOpcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
      Gerar := ((trim(CTe.signature.DigestValue) = '') and
                (trim(CTe.signature.SignatureValue) = '') and
                (trim(CTe.signature.X509Certificate) = ''));

    if Gerar then
    begin
      FCTe.signature.URI := '#CTe' + OnlyNumber(CTe.infCTe.ID);
      FCTe.signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
      FCTe.signature.GerarXML;
      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + FCTe.signature.Gerador.ArquivoFormatoXML;
    end;
  end;

  case ModeloDF of
    moGTVe: Gerador.wGrupo('/GTVe');
    moCTeOS: Gerador.wGrupo('/CTeOS');
  else
    Gerador.wGrupo('/CTe');
  end;

  if trim(CTe.procCTe.nProt) <> '' then
  begin
    xProtCTe :=
          '<protCTe ' + VersaoStr + '>' +
            '<infProt' + IfThen( (CTe.procCTe.Id <> ''), ' Id="' + CTe.procCTe.Id + '">', '>') +
              '<tpAmb>'+TpAmbToStr(CTe.procCTe.tpAmb)+'</tpAmb>'+
              '<verAplic>'+CTe.procCTe.verAplic+'</verAplic>'+
              '<chCTe>'+CTe.procCTe.chCTe+'</chCTe>'+
              '<dhRecbto>'+DateTimeWithTimeZone(CTe.procCTe.dhRecbto, CTe.ide.cUF)+'</dhRecbto>'+
              '<nProt>'+CTe.procCTe.nProt+'</nProt>'+
              '<digVal>'+CTe.procCTe.digVal+'</digVal>'+
              '<cStat>'+IntToStr(CTe.procCTe.cStat)+'</cStat>'+
              '<xMotivo>'+CTe.procCTe.xMotivo+'</xMotivo>'+
            '</infProt>'+
            IfThen( (CTe.procCTe.cMsg > 0) or (CTe.procCTe.xMsg <> ''),
            '<infFisco>' +
              '<cMsg>' + IntToStr(CTe.procCTe.cMsg) + '</cMsg>' +
              '<xMsg>' + CTe.procCTe.xMsg + '</xMsg>' +
            '</infFisco>',
            '') +
          '</protCTe>';

    Gerador.wTexto(xProtCTe);

    case ModeloDF of
      moGTVe: Gerador.wGrupo('/GTVeProc');
      moCTeOS: Gerador.wGrupo('/cteOSProc');
    else
      Gerador.wGrupo('/cteProc');
    end;
  end;

  Gerador.gtAjustarRegistros(CTe.infCTe.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TCTeW.GerarInfCTe;
begin
  GerarIde;
  GerarCompl;
  GerarEmit;

  case ModeloDF of
    moGTVe: begin
              GerarRem;
              GerarDest;
            end;
    moCTeOS: GerarTomador;
  else
    begin
      GerarRem;
      GerarExped;
      GerarReceb;
      GerarDest;
    end;
  end;

  if ModeloDF = moGTVe then
  begin
    GerarOrigem;
    GerarDestino;
    GerardetGTV;
  end
  else
  begin
    GerarvPrest;
    GerarImp;

    GerarInfCTeNorm; // Gerado somente se Tipo de CTe = tcNormal
    GerarinfCTeComp; // Gerado somente se Tipo de CTe = tcComplemento

    if VersaoDF <= ve300 then
      GerarInfCTeAnu;  // Gerado somente se Tipo de CTe = tcAnulacao
  end;

  GerarautXML;

  if VersaoDF >= ve300 then
    GerarinfRespTec;
end;

procedure TCTeW.GerarIde;
var
  Obrigatorio: Integer;
begin
  Gerador.wGrupo('ide', '#004');
  Gerador.wCampo(tcInt, '#005', 'cUF', 02, 02, 1, CTe.ide.cUF, DSC_CUF);

  if not ValidarCodigoUF(CTe.ide.cUF) then
    Gerador.wAlerta('#005', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#006', 'cCT  ', 08, 08, 1, IntToStrZero(ExtrairCodigoChaveAcesso(CTe.infCTe.ID), 8), DSC_CDF);
  Gerador.wCampo(tcInt, '#007', 'CFOP ', 04, 04, 1, CTe.ide.CFOP, DSC_CFOP);
  Gerador.wCampo(tcStr, '#008', 'natOp', 01, 60, 1, CTe.ide.natOp, DSC_NATOP);

  if VersaoDF = ve200 then
    Gerador.wCampo(tcStr, '#009', 'forPag', 01, 01, 1, tpforPagToStr(CTe.ide.forPag), DSC_INDPAG);

  Gerador.wCampo(tcInt, '#010', 'mod  ', 02, 02, 1, CTe.ide.modelo, DSC_MOD);
  Gerador.wCampo(tcInt, '#011', 'serie', 01, 03, 1, CTe.ide.serie, DSC_SERIE);
  Gerador.wCampo(tcInt, '#012', 'nCT  ', 01, 09, 1, CTe.ide.nCT, DSC_NDF);

  if VersaoDF >= ve300 then
    Gerador.wCampo(tcStr, '#013', 'dhEmi', 25, 25, 1, DateTimeWithTimeZone(CTe.ide.dhEmi, CTe.ide.cUF), DSC_DEMI)
  else
    Gerador.wCampo(tcDatHor, '#013', 'dhEmi', 19, 19, 1, CTe.ide.dhEmi, DSC_DEMI);

  Gerador.wCampo(tcStr, '#014', 'tpImp  ', 01, 01, 1, tpImpToStr(CTe.Ide.tpImp), DSC_TPIMP);
  Gerador.wCampo(tcStr, '#015', 'tpEmis ', 01, 01, 1, tpEmisToStr(CTe.Ide.tpEmis), DSC_TPEMIS);
  Gerador.wCampo(tcInt, '#016', 'cDV    ', 01, 01, 1, CTe.Ide.cDV, DSC_CDV);
  Gerador.wCampo(tcStr, '#017', 'tpAmb  ', 01, 01, 1, tpAmbToStr(CTe.Ide.tpAmb), DSC_TPAMB);
  Gerador.wCampo(tcStr, '#018', 'tpCTe  ', 01, 01, 1, tpCTePagToStr(CTe.Ide.tpCTe), DSC_TPCTE);

  if ModeloDF <> moGTVe then
    Gerador.wCampo(tcStr, '#019', 'procEmi', 1, 1, 1, procEmiToStr(CTe.Ide.procEmi), DSC_PROCEMI);

  Gerador.wCampo(tcStr, '#020', 'verProc', 01, 20, 1, CTe.Ide.verProc, DSC_VERPROC);

  if (VersaoDF >= ve300) and (ModeloDF = moCTe) and (CTe.ide.indGlobalizado = tiSim) then
    Gerador.wCampo(tcStr, '#020', 'indGlobalizado', 01, 01, 0, TindicadorToStr(CTe.ide.indGlobalizado), DSC_INDGLOBALIZADO);

  if (VersaoDF < ve300) then
  begin
    Gerador.wCampo(tcStr, '#021', 'refCTE', 44, 44, 0, OnlyNumber(CTe.Ide.refCTE), DSC_REFCTE);

    if OnlyNumber(CTe.Ide.refCTe) <> '' then
      if not ValidarChave(CTe.Ide.refCTe) then
        Gerador.wAlerta('#021', 'refCTE', DSC_REFCTE, ERR_MSG_INVALIDO);
  end;

  Gerador.wCampo(tcInt, '#022', 'cMunEnv', 07, 07, 1, CTe.ide.cMunEnv, DSC_CMUNEMI);

  if not ValidarMunicipio(CTe.ide.cMunEnv) then
    Gerador.wAlerta('#022', 'cMunEnv', DSC_CMUNEMI, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#023', 'xMunEnv', 02, 60, 1, CTe.ide.xMunEnv, DSC_XMUN);
  Gerador.wCampo(tcStr, '#024', 'UFEnv  ', 02, 02, 1, CTe.ide.UFEnv, DSC_UF);

  if not ValidarUF(CTe.ide.UFEnv) then
    Gerador.wAlerta('#024', 'UFEnv', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#025', 'modal ', 02, 02, 1, TpModalToStr(CTe.Ide.modal), DSC_MODAL);
  Gerador.wCampo(tcStr, '#026', 'tpServ', 01, 01, 1, TpServPagToStr(CTe.Ide.tpServ), DSC_TPSERV);

  if (VersaoDF >= ve300) and (ModeloDF = moCTeOS) then
    Gerador.wCampo(tcStr, '#035', 'indIEToma', 01, 01, 1, indIEDestToStr(CTe.ide.indIEToma), DSC_INDIETOMA);

  if (ModeloDF = moCTe) then
    Obrigatorio := 1
  else
    Obrigatorio := 0;

  if ModeloDF <> moGTVe then
  begin
    Gerador.wCampo(tcInt, '#027', 'cMunIni', 07, 07, Obrigatorio, CTe.ide.cMunIni, DSC_CMUNEMI);

    if (Obrigatorio = 1) and not ValidarMunicipio(CTe.ide.cMunIni) then
      Gerador.wAlerta('#027', 'cMunIni', DSC_CMUNEMI, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#028', 'xMunIni', 02, 60, Obrigatorio, CTe.ide.xMunIni, DSC_XMUN);
    Gerador.wCampo(tcStr, '#029', 'UFIni  ', 02, 02, Obrigatorio, CTe.ide.UFIni, DSC_UF);

    if (Obrigatorio = 1) and not ValidarUF(CTe.ide.UFIni) then
      Gerador.wAlerta('#029', 'UFIni', DSC_UF, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcInt, '#030', 'cMunFim', 07, 07, Obrigatorio, CTe.ide.cMunFim, DSC_CMUNEMI);

    if (Obrigatorio = 1) and not ValidarMunicipio(CTe.ide.cMunFim) then
      Gerador.wAlerta('#030', 'cMunFim', DSC_CMUNEMI, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#031', 'xMunFim', 02, 60, Obrigatorio, CTe.ide.xMunFim, DSC_XMUN);
    Gerador.wCampo(tcStr, '#032', 'UFFim  ', 02, 02, Obrigatorio, CTe.ide.UFFim, DSC_UF);

    if (Obrigatorio = 1) and not ValidarUF(CTe.ide.UFFim) then
      Gerador.wAlerta('#032', 'UFFim', DSC_UF, ERR_MSG_INVALIDO);
  end;

  if (ModeloDF = moCTe) then
  begin
    Gerador.wCampo(tcStr, '#033', 'retira    ', 01, 01, 1, TpRetiraPagToStr(CTe.Ide.retira), DSC_RETIRA);
    Gerador.wCampo(tcStr, '#034', 'xDetRetira', 01, 160, 0, CTe.Ide.xdetretira, DSC_DRET);
  end;

  if (VersaoDF >= ve300) and (ModeloDF <> moCTeOS) then
    Gerador.wCampo(tcStr, '#035', 'indIEToma', 01, 01, 1, indIEDestToStr(CTe.ide.indIEToma), DSC_INDIETOMA);

  if ModeloDF = moGTVe then
  begin
    Gerador.wCampo(tcStr, '#013', 'dhSaidaOrig  ', 25, 25, 1, DateTimeWithTimeZone(CTe.ide.dhSaidaOrig, CTe.ide.cUF), DSC_DEMI);
    Gerador.wCampo(tcStr, '#013', 'dhChegadaDest', 25, 25, 1, DateTimeWithTimeZone(CTe.ide.dhChegadaDest, CTe.ide.cUF), DSC_DEMI);
  end;

  if (ModeloDF <> moCTeOS) then
  begin
    GerarToma03;
    GerarToma4;
  end;

  if (ModeloDF = moCTeOS) then
    GerarInfPercurso;

  if CTe.Ide.tpEmis = teFSDA then
  begin
    if VersaoDF >= ve300 then
      Gerador.wCampo(tcStr, '#057', 'dhCont', 25, 25, 1, DateTimeWithTimeZone(CTe.ide.dhCont, CTe.ide.cUF), DSC_DHCONT)
    else
      Gerador.wCampo(tcDatHor, '#057', 'dhCont', 19, 019, 1, CTe.ide.dhCont, DSC_DHCONT);

    Gerador.wCampo(tcStr, '#058', 'xJust', 15, 256, 1, CTe.ide.xJust, DSC_XJUSTCONT);
  end;

  Gerador.wGrupo('/ide');
end;

procedure TCTeW.GerarToma03;
begin
  if (trim(CTe.Ide.Toma4.xNome) = '') then
  begin
    if VersaoDF >= ve300 then
    begin
      if ModeloDF = moCTe then
      begin
        Gerador.wGrupo('toma3', '#035');
        Gerador.wCampo(tcStr, '#036', 'toma', 01, 01, 1, TpTomadorToStr(CTe.ide.Toma03.Toma), DSC_TOMA);
        Gerador.wGrupo('/toma3');
      end
      else
      begin
        Gerador.wGrupo('toma', '#035');
        Gerador.wCampo(tcStr, '#036', 'toma', 01, 01, 1, TpTomadorToStr(CTe.ide.Toma03.Toma), DSC_TOMA);
        Gerador.wGrupo('/toma');
      end;
    end
    else
    begin
      Gerador.wGrupo('toma03', '#035');
      Gerador.wCampo(tcStr, '#036', 'toma', 01, 01, 1, TpTomadorToStr(CTe.ide.Toma03.Toma), DSC_TOMA);
      Gerador.wGrupo('/toma03');
    end;
  end;
end;

procedure TCTeW.GerarToma4;

procedure GeraIE;
begin
  if Trim(CTe.Ide.Toma4.IE) = 'ISENTO' then
    Gerador.wCampo(tcStr, '#041', 'IE', 00, 14, 1, CTe.Ide.Toma4.IE, DSC_IE)
  else
    Gerador.wCampo(tcStr, '#041', 'IE', 00, 14, 0, OnlyNumber(CTe.Ide.Toma4.IE), DSC_IE);

  if (FOpcoes.ValidarInscricoes) then
    if not ValidarIE(CTe.Ide.Toma4.IE, CTe.Ide.Toma4.EnderToma.UF) then
      Gerador.wAlerta('#041', 'IE', DSC_IE, ERR_MSG_INVALIDO);
end;

begin
  if (trim(CTe.Ide.Toma4.IE) <> '') or (trim(CTe.Ide.Toma4.xNome) <> '') then
  begin
    if ModeloDF = moCTe then
      Gerador.wGrupo('toma4', '#037')
    else
      Gerador.wGrupo('tomaTerceiro', '#037');

    Gerador.wCampo(tcStr, '#038', 'toma', 01, 01, 1, TpTomadorToStr(CTe.ide.Toma4.Toma), DSC_TOMA);

    if CTe.Ide.Toma4.EnderToma.cPais = 0 then
      CTe.Ide.Toma4.EnderToma.cPais := 1058;

    if CTe.Ide.Toma4.EnderToma.cPais = 1058 then
      Gerador.wCampoCNPJCPF('#039', '#040', CTe.ide.Toma4.CNPJCPF)
     else
      Gerador.wCampo(tcStr, '#039', 'CNPJ', 00, 14, 1, '00000000000000', DSC_CNPJ);

    GeraIE;

    Gerador.wCampo(tcStr, '#042', 'xNome', 02, 60, 1, CTe.Ide.Toma4.xNome, DSC_XNOME);
    Gerador.wCampo(tcStr, '#043', 'xFant', 02, 60, 0, CTe.Ide.Toma4.xFant, DSC_XFANT);
    Gerador.wCampo(tcStr, '#044', 'fone ', 07, 12, 0, OnlyNumber(CTe.Ide.Toma4.fone), DSC_FONE);

    GerarEnderToma;

    Gerador.wCampo(tcStr, '#056', 'email', 01, 60, 0, CTe.Ide.Toma4.email, DSC_EMAIL);

    if ModeloDF = moCTe then
      Gerador.wGrupo('/toma4')
    else
      Gerador.wGrupo('/tomaTerceiro');
  end;
end;

procedure TCTeW.GerarEnderToma;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CTe.Ide.Toma4.EnderToma.cPais,
                     CTe.Ide.Toma4.EnderToma.UF,
                     CTe.Ide.Toma4.EnderToma.xMun,
                     CTe.Ide.Toma4.EnderToma.cMun);

  Gerador.wGrupo('enderToma', '#045');
  Gerador.wCampo(tcStr, '#046', 'xLgr   ', 02, 255, 1, CTe.Ide.Toma4.EnderToma.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#047', 'nro    ', 01, 060, 1, CTe.Ide.Toma4.EnderToma.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#048', 'xCpl   ', 01, 060, 0, CTe.Ide.Toma4.EnderToma.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#049', 'xBairro', 02, 060, 1, CTe.Ide.Toma4.EnderToma.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#050', 'cMun   ', 07, 007, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(CTe.Ide.Toma4.EnderToma.cMun) then
    Gerador.wAlerta('#050', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#051', 'xMun', 02, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#052', 'CEP ', 08, 08, 0, CTe.Ide.Toma4.EnderToma.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#053', 'UF  ', 02, 02, 1, xUF, DSC_UF);

  if not ValidarUF(xUF) then
    Gerador.wAlerta('#053', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcInt, '#054', 'cPais', 04, 04, 0, CTe.Ide.Toma4.EnderToma.cPais, DSC_CPAIS); // Conforme NT-2009/01
  Gerador.wCampo(tcStr, '#055', 'xPais', 02, 60, 0, CTe.Ide.Toma4.EnderToma.xPais, DSC_XPAIS);
  Gerador.wGrupo('/enderToma');
end;

procedure TCTeW.GerarCompl;
begin
  case ModeloDF of
    moGTVe: begin
              if (trim(CTe.Compl.xCaracAd) <> '') or (trim(CTe.Compl.xCaracSer) <> '') or
                 (trim(CTe.Compl.xEmi) <> '') or (trim(CTe.Compl.xObs) <> '') or
                 (CTe.Compl.ObsCont.Count > 0) or (CTe.Compl.ObsFisco.Count > 0) then
              begin
                Gerador.wGrupo('compl', '#059');
                Gerador.wCampo(tcStr, '#060', 'xCaracAd ', 1, 15, 0, CTe.Compl.xCaracAd, DSC_XCARACAD);
                Gerador.wCampo(tcStr, '#061', 'xCaracSer', 1, 30, 0, CTe.Compl.xCaracSer, DSC_XCARACSET);
                Gerador.wCampo(tcStr, '#062', 'xEmi     ', 1, 20, 0, CTe.Compl.xEmi, DSC_XEMI);

                Gerador.wCampo(tcStr, '#090', 'xObs', 1, 2000, 0, CTe.Compl.xObs, DSC_XOBS);

                GerarObsCont;
                GerarObsFisco;

                Gerador.wGrupo('/compl');
              end;
            end;

    moCTeOS: begin
               if (trim(CTe.Compl.xCaracAd) <> '') or (trim(CTe.Compl.xCaracSer) <> '') or
                  (trim(CTe.Compl.xEmi) <> '') or (trim(CTe.Compl.fluxo.xOrig) <> '') or
                  (CTe.Compl.fluxo.pass.Count > 0) or (trim(CTe.Compl.fluxo.xDest) <> '') or
                  (trim(CTe.Compl.fluxo.xRota) <> '') or (trim(CTe.Compl.xObs) <> '') or
                  (CTe.Compl.ObsCont.Count > 0) or (CTe.Compl.ObsFisco.Count > 0) then
               begin
                 Gerador.wGrupo('compl', '#059');
                 Gerador.wCampo(tcStr, '#060', 'xCaracAd ', 01, 15, 0, CTe.Compl.xCaracAd, DSC_XCARACAD);
                 Gerador.wCampo(tcStr, '#061', 'xCaracSer', 01, 30, 0, CTe.Compl.xCaracSer, DSC_XCARACSET);
                 Gerador.wCampo(tcStr, '#062', 'xEmi     ', 01, 20, 0, CTe.Compl.xEmi, DSC_XEMI);

                 GerarFluxo;

                 Gerador.wCampo(tcStr, '#090', 'xObs', 01, 2000, 0, CTe.Compl.xObs, DSC_XOBS);

                 GerarObsCont;
                 GerarObsFisco;

                 Gerador.wGrupo('/compl');
               end;
             end;
  else
    begin
      if (trim(CTe.Compl.xCaracAd) <> '') or (trim(CTe.Compl.xCaracSer) <> '') or
         (trim(CTe.Compl.xEmi) <> '') or (trim(CTe.Compl.fluxo.xOrig) <> '') or
         (CTe.Compl.fluxo.pass.Count > 0) or (trim(CTe.Compl.fluxo.xDest) <> '') or
         (trim(CTe.Compl.fluxo.xRota) <> '') or
         ((CTe.Compl.Entrega.TipoData <> tdNaoInformado) and
          (CTe.Compl.Entrega.TipoHora <> thNaoInformado)) or
         (trim(CTe.Compl.origCalc) <> '') or (trim(CTe.Compl.destCalc) <> '') or
         (trim(CTe.Compl.xObs) <> '') or
         (CTe.Compl.ObsCont.Count > 0) or (CTe.Compl.ObsFisco.Count > 0) then
      begin
        Gerador.wGrupo('compl', '#059');
        Gerador.wCampo(tcStr, '#060', 'xCaracAd ', 01, 15, 0, CTe.Compl.xCaracAd, DSC_XCARACAD);
        Gerador.wCampo(tcStr, '#061', 'xCaracSer', 01, 30, 0, CTe.Compl.xCaracSer, DSC_XCARACSET);
        Gerador.wCampo(tcStr, '#062', 'xEmi     ', 01, 20, 0, CTe.Compl.xEmi, DSC_XEMI);

        GerarFluxo;

        if (CTe.Compl.Entrega.TipoData <> tdNaoInformado) and
           (CTe.Compl.Entrega.TipoHora <> thNaoInformado) then
          GerarEntrega;

        Gerador.wCampo(tcStr, '#088', 'origCalc', 02, 40, 0, CTe.Compl.origCalc, DSC_ORIGCALC);
        Gerador.wCampo(tcStr, '#089', 'destCalc', 02, 40, 0, CTe.Compl.destCalc, DSC_DESTCALC);
        Gerador.wCampo(tcStr, '#090', 'xObs    ', 01, 2000, 0, CTe.Compl.xObs, DSC_XOBS);

        GerarObsCont;
        GerarObsFisco;

        Gerador.wGrupo('/compl');
      end;
    end;
  end;
end;

procedure TCTeW.GerarFluxo;
var
  i: Integer;
begin
  if (trim(CTe.Compl.fluxo.xOrig) <> '') or (CTe.Compl.fluxo.pass.Count > 0) or
     (trim(CTe.Compl.fluxo.xDest) <> '') or (trim(CTe.Compl.fluxo.xRota) <> '') then
  begin
    Gerador.wGrupo('fluxo', '#063');
    Gerador.wCampo(tcStr, '#064', 'xOrig', 01, 60, 0, CTe.Compl.fluxo.xOrig, DSC_XORIG);

    for i := 0 to CTe.Compl.fluxo.pass.Count - 1 do
    begin
      Gerador.wGrupo('pass', '#065');
      Gerador.wCampo(tcStr, '#066', 'xPass', 01, 15, 1, CTe.Compl.fluxo.pass[i].xPass, DSC_XPASS);
      Gerador.wGrupo('/pass');
    end;

    if CTe.Compl.fluxo.pass.Count > 990 then
      Gerador.wAlerta('#065', 'pass', DSC_PASS, ERR_MSG_MAIOR_MAXIMO + '990');

    Gerador.wCampo(tcStr, '#067', 'xDest', 01, 60, 0, CTe.Compl.fluxo.xDest, DSC_XDEST);
    Gerador.wCampo(tcStr, '#068', 'xRota', 01, 10, 0, CTe.Compl.fluxo.xRota, DSC_XROTA);
    Gerador.wGrupo('/fluxo');
  end;
end;

procedure TCTeW.GerarEntrega;
begin
  Gerador.wGrupo('Entrega', '#069');

  case CTe.Compl.Entrega.TipoData of
   tdSemData:
     begin
       Gerador.wGrupo('semData', '#070');
       Gerador.wCampo(tcStr, '#071', 'tpPer', 01, 01, 1, TpDataPeriodoToStr(CTe.Compl.Entrega.semData.tpPer), DSC_TPPER);
       Gerador.wGrupo('/semData');
     end;
   tdNaData, tdAteData, tdApartirData:
     begin
       Gerador.wGrupo('comData', '#072');
       Gerador.wCampo(tcStr, '#073', 'tpPer', 01, 01, 1, TpDataPeriodoToStr(CTe.Compl.Entrega.comData.tpPer), DSC_TPPER);
       Gerador.wCampo(tcDat, '#074', 'dProg', 10, 10, 1, CTe.Compl.Entrega.comData.dProg, DSC_DPROG);
       Gerador.wGrupo('/comData');
     end;
   tdNoPeriodo:
     begin
       Gerador.wGrupo('noPeriodo', '#075');
       Gerador.wCampo(tcStr, '#076', 'tpPer', 01, 01, 1, TpDataPeriodoToStr(CTe.Compl.Entrega.noPeriodo.tpPer), DSC_TPPER);
       Gerador.wCampo(tcDat, '#077', 'dIni ', 10, 10, 1, CTe.Compl.Entrega.noPeriodo.dIni, DSC_DINI);
       Gerador.wCampo(tcDat, '#078', 'dFim ', 10, 10, 1, CTe.Compl.Entrega.noPeriodo.dFim, DSC_DFIM);
       Gerador.wGrupo('/noPeriodo');
     end;
  end;

  case CTe.Compl.Entrega.TipoHora of
   thSemHorario:
     begin
       Gerador.wGrupo('semHora', '#079');
       Gerador.wCampo(tcStr, '#080', 'tpHor', 01, 01, 1, TpHorarioIntervaloToStr(CTe.Compl.Entrega.semHora.tpHor), DSC_TPHOR);
       Gerador.wGrupo('/semHora');
     end;
   thNoHorario, thAteHorario, thApartirHorario:
     begin
       Gerador.wGrupo('comHora', '#081');
       Gerador.wCampo(tcStr, '#082', 'tpHor', 01, 01, 1, TpHorarioIntervaloToStr(CTe.Compl.Entrega.comHora.tpHor), DSC_TPHOR);
       Gerador.wCampo(tcStr, '#083', 'hProg', 08, 08, 1, TimeToStr(CTe.Compl.Entrega.comHora.hProg), DSC_HPROG);
       Gerador.wGrupo('/comHora');
     end;
   thNoIntervalo:
     begin
       Gerador.wGrupo('noInter', '#084');
       Gerador.wCampo(tcStr, '#085', 'tpHor', 01, 01, 1, TpHorarioIntervaloToStr(CTe.Compl.Entrega.noInter.tpHor), DSC_TPHOR);
       Gerador.wCampo(tcStr, '#086', 'hIni ', 08, 08, 1, TimeToStr(CTe.Compl.Entrega.noInter.hIni), DSC_HINI);
       Gerador.wCampo(tcStr, '#087', 'hFim ', 08, 08, 1, TimeToStr(CTe.Compl.Entrega.noInter.hFim), DSC_HFIM);
       Gerador.wGrupo('/noInter');
     end;
  end;

  Gerador.wGrupo('/Entrega');
end;

procedure TCTeW.GerarObsCont;
var
  i: Integer;
begin
  for i := 0 to CTe.Compl.ObsCont.Count - 1 do
  begin
    Gerador.wGrupo('ObsCont xCampo="' + CTe.Compl.ObsCont[i].xCampo + '"', '#092');
    Gerador.wCampo(tcStr, '#093', 'xTexto', 01, 160, 1, CTe.Compl.ObsCont[i].xTexto, DSC_OBSCONT);
    Gerador.wGrupo('/ObsCont');
  end;

  if CTe.Compl.ObsCont.Count > 10 then
    Gerador.wAlerta('#091', 'ObsCont', DSC_OBSCONT, ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TCTeW.GerarObsFisco;
var
  i: Integer;
begin
  for i := 0 to CTe.Compl.ObsFisco.Count - 1 do
  begin
    Gerador.wGrupo('ObsFisco xCampo="' + CTe.Compl.ObsFisco[i].xCampo + '"', '#095');
    Gerador.wCampo(tcStr, '#096', 'xTexto', 01, 60, 1, CTe.Compl.ObsFisco[i].xTexto, DSC_OBSFISCO);
    Gerador.wGrupo('/ObsFisco');
  end;

  if CTe.Compl.ObsFisco.Count > 10 then
    Gerador.wAlerta('#094', 'ObsFisco', DSC_OBSFISCO, ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TCTeW.GerarEmit;
begin
  Gerador.wGrupo('emit', '#097');
  Gerador.wCampoCNPJ('#098', CTe.Emit.CNPJ, CODIGO_BRASIL, True);
  Gerador.wCampo(tcStr, '#099', 'IE', 02, 14, 1, OnlyNumber(CTe.Emit.IE), DSC_IE);

  if (FOpcoes.ValidarInscricoes) then
    if not ValidarIE(CTe.Emit.IE, CTe.Emit.enderEmit.UF) then
      Gerador.wAlerta('#099', 'IE', DSC_IE, ERR_MSG_INVALIDO);

  if (VersaoDF >= ve300) and (ModeloDF = moCTe) then
    Gerador.wCampo(tcStr, '#100', 'IEST', 02, 14, 0, OnlyNumber(CTe.Emit.IEST), DSC_IEST);

  Gerador.wCampo(tcStr, '#101', 'xNome', 02, 60, 1, CTe.Emit.xNome, DSC_XNOME);
  Gerador.wCampo(tcStr, '#102', 'xFant', 02, 60, 0, CTe.Emit.xFant, DSC_XFANT);

  GerarEnderEmit;

  if ModeloDF <> moGTVe then
  begin
    if VersaoDF >= ve400 then
      Gerador.wCampo(tcStr, '#', 'CRT', 1, 1, 1, CRTCTeToStr(CTe.Emit.CRT), DSC_CRTCTE)
    else
      Gerador.wCampo(tcStr, '#', 'CRT', 1, 1, 0, CRTCTeToStr(CTe.Emit.CRT), DSC_CRTCTE);
  end;

  Gerador.wGrupo('/emit');
end;

procedure TCTeW.GerarEnderEmit;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL,
                     CTe.Emit.enderEmit.UF,
                     CTe.Emit.enderEmit.xMun,
                     CTe.Emit.EnderEmit.cMun);

  Gerador.wGrupo('enderEmit', '#102');
  Gerador.wCampo(tcStr, '#103', 'xLgr   ', 02, 60, 1, CTe.Emit.enderEmit.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#104', 'nro    ', 01, 60, 1, CTe.Emit.enderEmit.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#105', 'xCpl   ', 01, 60, 0, CTe.Emit.enderEmit.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#106', 'xBairro', 02, 60, 1, CTe.Emit.enderEmit.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#107', 'cMun   ', 07, 07, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(CTe.Emit.EnderEmit.cMun) then
    Gerador.wAlerta('#107', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#108', 'xMun', 02, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#109', 'CEP ', 08, 08, 0, CTe.Emit.enderEmit.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#110', 'UF  ', 02, 02, 1, xUF, DSC_UF);

  if not ValidarUF(xUF) then
    Gerador.wAlerta('#110', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#111', 'fone', 07, 12, 0, OnlyNumber(CTe.Emit.EnderEmit.fone), DSC_FONE);
  Gerador.wGrupo('/enderEmit');
end;

procedure TCTeW.GerarTomador;

procedure GeraIE;
begin
  if Trim(CTe.toma.IE) = 'ISENTO' then
    Gerador.wCampo(tcStr, '#067', 'IE', 00, 14, 1, CTe.toma.IE, DSC_IE)
  else
    Gerador.wCampo(tcStr, '#067', 'IE', 00, 14, 0, OnlyNumber(CTe.toma.IE), DSC_IE);

  if (FOpcoes.ValidarInscricoes) then
    if not ValidarIE(CTe.toma.IE, CTe.toma.EnderToma.UF) then
      Gerador.wAlerta('#067', 'IE', DSC_IE, ERR_MSG_INVALIDO);
end;

begin
  if (trim(CTe.toma.IE) <> '') or (trim(CTe.toma.xNome) <> '') then
  begin
    Gerador.wGrupo('toma', '#063');

    if CTe.toma.EnderToma.cPais = 0 then
      CTe.toma.EnderToma.cPais := 1058;

    if CTe.toma.EnderToma.cPais = 1058 then
      Gerador.wCampoCNPJCPF('#064', '#065', CTe.toma.CNPJCPF)
    else
      Gerador.wCampo(tcStr, '#064', 'CNPJ', 00, 14, 1, '00000000000000', DSC_CNPJ);

    GeraIE;

    Gerador.wCampo(tcStr, '#068', 'xNome', 02, 60, 1, CTe.toma.xNome, DSC_XNOME);
    Gerador.wCampo(tcStr, '#069', 'xFant', 02, 60, 0, CTe.toma.xFant, DSC_XFANT);
    Gerador.wCampo(tcStr, '#070', 'fone ', 07, 12, 0, OnlyNumber(CTe.toma.fone), DSC_FONE);

    GerarEnderTomador;

    Gerador.wCampo(tcStr, '#082', 'email', 01, 60, 0, CTe.toma.email, DSC_EMAIL);
    Gerador.wGrupo('/toma');
  end;
end;

procedure TCTeW.GerarEnderTomador;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CTe.toma.EnderToma.cPais,
                     CTe.toma.EnderToma.UF,
                     CTe.toma.EnderToma.xMun,
                     CTe.toma.EnderToma.cMun);

  Gerador.wGrupo('enderToma', '#071');
  Gerador.wCampo(tcStr, '#072', 'xLgr   ', 02, 255, 1, CTe.toma.EnderToma.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#073', 'nro    ', 01, 060, 1, CTe.toma.EnderToma.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#074', 'xCpl   ', 01, 060, 0, CTe.toma.EnderToma.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#075', 'xBairro', 02, 060, 1, CTe.toma.EnderToma.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#076', 'cMun   ', 07, 007, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(CTe.toma.EnderToma.cMun) then
    Gerador.wAlerta('#076', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#077', 'xMun', 02, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#078', 'CEP ', 08, 08, 0, CTe.toma.EnderToma.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#079', 'UF  ', 02, 02, 1, xUF, DSC_UF);

  if not ValidarUF(xUF) then
    Gerador.wAlerta('#079', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcInt, '#080', 'cPais', 04, 04, 0, CTe.toma.EnderToma.cPais, DSC_CPAIS);
  Gerador.wCampo(tcStr, '#081', 'xPais', 02, 60, 0, CTe.toma.EnderToma.xPais, DSC_XPAIS);
  Gerador.wGrupo('/enderToma');
end;

procedure TCTeW.GerarRem;
var
  xNome: string;

procedure GeraIE;
begin
  if Trim(CTe.Rem.IE) = 'ISENTO' then
    Gerador.wCampo(tcStr, '#115', 'IE', 00, 14, 1, CTe.Rem.IE, DSC_IE)
  else
    Gerador.wCampo(tcStr, '#115', 'IE', 00, 14, 0, OnlyNumber(CTe.Rem.IE), DSC_IE);

  if (FOpcoes.ValidarInscricoes) then
    if not ValidarIE(CTe.Rem.IE, CTe.Rem.EnderReme.UF) then
      Gerador.wAlerta('#115', 'IE', DSC_IE, ERR_MSG_INVALIDO);
end;

begin
  if (trim(CTe.Rem.CNPJCPF) <> '') or (trim(CTe.Rem.xNome) <> '') then
  begin
    if (VersaoDF <= ve300) or (CTe.ide.cUF = 51) then
      xNome := xRazao3
    else
      xNome := xRazao4;

    Gerador.wGrupo('rem', '#112');

    if CTe.Rem.enderReme.cPais = 0 then
      CTe.Rem.enderReme.cPais := 1058;

    if CTe.Rem.enderReme.cPais = 1058 then
      Gerador.wCampoCNPJCPF('#113', '#114', CTe.Rem.CNPJCPF)
    else
      Gerador.wCampo(tcStr, '#113', 'CNPJ', 00, 14, 1, '00000000000000', DSC_CNPJ);

    GeraIE;

    if CTe.Ide.tpAmb = taHomologacao then
      Gerador.wCampo(tcStr, '#116', 'xNome', 02, 60, 1, xNome, DSC_XNOME)
    else
      Gerador.wCampo(tcStr, '#116', 'xNome', 02, 60, 1, CTe.Rem.xNome, DSC_XNOME);

    Gerador.wCampo(tcStr, '#117', 'xFant', 02, 60, 0, CTe.Rem.xFant, DSC_XFANT);
    Gerador.wCampo(tcStr, '#118', 'fone ', 07, 12, 0, OnlyNumber(CTe.Rem.fone), DSC_FONE);

    GerarEnderReme;
    Gerador.wCampo(tcStr, '#130', 'email', 01, 60, 0, CTe.Rem.email, DSC_EMAIL);

    Gerador.wGrupo('/rem');
  end;
end;

procedure TCTeW.GerarEnderReme;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CTe.Rem.EnderReme.cPais,
                     CTe.Rem.EnderReme.UF,
                     CTe.Rem.EnderReme.xMun,
                     CTe.Rem.EnderReme.cMun);

  Gerador.wGrupo('enderReme', '#119');
  Gerador.wCampo(tcStr, '#120', 'xLgr   ', 02, 255, 1, CTe.Rem.EnderReme.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#121', 'nro    ', 01, 060, 1, CTe.Rem.EnderReme.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#122', 'xCpl   ', 01, 060, 0, CTe.Rem.EnderReme.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#123', 'xBairro', 02, 060, 1, CTe.Rem.EnderReme.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#124', 'cMun   ', 07, 007, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(CTe.Rem.EnderReme.cMun) then
    Gerador.wAlerta('#124', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#125', 'xMun', 02, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#126', 'CEP ', 08, 08, 0, CTe.Rem.EnderReme.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#127', 'UF  ', 02, 02, 1, xUF, DSC_UF);

  if not ValidarUF(xUF) then
    Gerador.wAlerta('#127', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcInt, '#128', 'cPais', 04, 04, 0, CTe.Rem.EnderReme.cPais, DSC_CPAIS); // Conforme NT-2009/01
  Gerador.wCampo(tcStr, '#129', 'xPais', 02, 60, 0, CTe.Rem.EnderReme.xPais, DSC_XPAIS);
  Gerador.wGrupo('/enderReme');
end;

procedure TCTeW.GerarExped;
var
  xNome: string;

procedure GeraIE;
begin
  if Trim(CTe.Exped.IE) = 'ISENTO' then
    Gerador.wCampo(tcStr, '#145', 'IE', 00, 14, 1, CTe.Exped.IE, DSC_IE)
  else
    Gerador.wCampo(tcStr, '#145', 'IE', 00, 14, 0, OnlyNumber(CTe.Exped.IE), DSC_IE);

  if (FOpcoes.ValidarInscricoes) then
    if not ValidarIE(CTe.Exped.IE, CTe.Exped.EnderExped.UF) then
      Gerador.wAlerta('#145', 'IE', DSC_IE, ERR_MSG_INVALIDO);
end;

begin
  if (trim(CTe.Exped.CNPJCPF) <> '') or (trim(CTe.Exped.xNome) <> '') then
  begin
    if (VersaoDF <= ve300) or (CTe.ide.cUF = 51) then
      xNome := xRazao3
    else
      xNome := xRazao4;

    Gerador.wGrupo('exped', '#142');

    if CTe.Exped.EnderExped.cPais = 0 then
      CTe.Exped.EnderExped.cPais := 1058;

    if CTe.Exped.EnderExped.cPais = 1058 then
      Gerador.wCampoCNPJCPF('#143', '#144', CTe.Exped.CNPJCPF)
    else
      Gerador.wCampo(tcStr, '#143', 'CNPJ', 00, 14, 1, '00000000000000', DSC_CNPJ);

    GeraIE;

    if CTe.Ide.tpAmb = taHomologacao then
      Gerador.wCampo(tcStr, '#146', 'xNome', 02, 60, 1, xNome, DSC_XNOME)
    else
      Gerador.wCampo(tcStr, '#146', 'xNome', 02, 60, 1, CTe.Exped.xNome, DSC_XNOME);

    Gerador.wCampo(tcStr, '#147', 'fone', 07, 12, 0, OnlyNumber(CTe.Exped.fone), DSC_FONE);

    GerarEnderExped;
    Gerador.wCampo(tcStr, '#159', 'email', 01, 60, 0, CTe.Exped.email, DSC_EMAIL);
    Gerador.wGrupo('/exped');
  end;
end;

procedure TCTeW.GerarEnderExped;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CTe.Exped.EnderExped.cPais,
                     CTe.Exped.EnderExped.UF,
                     CTe.Exped.EnderExped.xMun,
                     CTe.Exped.EnderExped.cMun);

  Gerador.wGrupo('enderExped', '#148');
  Gerador.wCampo(tcStr, '#149', 'xLgr   ', 02, 255, 1, CTe.Exped.EnderExped.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#150', 'nro    ', 01, 060, 1, CTe.Exped.EnderExped.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#151', 'xCpl   ', 01, 060, 0, CTe.Exped.EnderExped.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#152', 'xBairro', 02, 060, 1, CTe.Exped.EnderExped.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#153', 'cMun   ', 07, 007, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(CTe.Exped.EnderExped.cMun) then
    Gerador.wAlerta('#153', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#154', 'xMun', 02, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#155', 'CEP ', 08, 08, 0, CTe.Exped.EnderExped.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#156', 'UF  ', 02, 02, 1, xUF, DSC_UF);

  if not ValidarUF(xUF) then
    Gerador.wAlerta('#156', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcInt, '#157', 'cPais', 04, 04, 0, CTe.Exped.EnderExped.cPais, DSC_CPAIS); // Conforme NT-2009/01
  Gerador.wCampo(tcStr, '#158', 'xPais', 02, 60, 0, CTe.Exped.EnderExped.xPais, DSC_XPAIS);
  Gerador.wGrupo('/enderExped');
end;

procedure TCTeW.GerarReceb;
var
  xNome: string;

procedure GeraIE;
begin
  if Trim(CTe.Receb.IE) = 'ISENTO' then
    Gerador.wCampo(tcStr, '#163', 'IE', 00, 14, 1, CTe.Receb.IE, DSC_IE)
  else
    Gerador.wCampo(tcStr, '#163', 'IE', 00, 14, 0, OnlyNumber(CTe.Receb.IE), DSC_IE);

  if (FOpcoes.ValidarInscricoes) then
    if not ValidarIE(CTe.Receb.IE, CTe.Receb.EnderReceb.UF) then
      Gerador.wAlerta('#163', 'IE', DSC_IE, ERR_MSG_INVALIDO);
end;

begin
  if (trim(CTe.Receb.CNPJCPF) <> '') or (trim(CTe.Receb.xNome) <> '') then
  begin
    if (VersaoDF <= ve300) or (CTe.ide.cUF = 51) then
      xNome := xRazao3
    else
      xNome := xRazao4;

    Gerador.wGrupo('receb', '#160');

    if CTe.Receb.EnderReceb.cPais = 0 then
      CTe.Receb.EnderReceb.cPais := 1058;

    if CTe.Receb.EnderReceb.cPais = 1058 then
      Gerador.wCampoCNPJCPF('#161', '#162', CTe.Receb.CNPJCPF)
    else
      Gerador.wCampo(tcStr, '#161', 'CNPJ', 00, 14, 1, '00000000000000', DSC_CNPJ);

    GeraIE;

    if CTe.Ide.tpAmb = taHomologacao then
      Gerador.wCampo(tcStr, '#164', 'xNome', 02, 60, 1, xNome, DSC_XNOME)
    else
      Gerador.wCampo(tcStr, '#164', 'xNome', 02, 60, 1, CTe.Receb.xNome, DSC_XNOME);

    Gerador.wCampo(tcStr, '#165', 'fone', 07, 12, 0, OnlyNumber(CTe.Receb.fone), DSC_FONE);

    GerarEnderReceb;
    Gerador.wCampo(tcStr, '#177', 'email', 01, 60, 0, CTe.Receb.email, DSC_EMAIL);
    Gerador.wGrupo('/receb');
  end;
end;

procedure TCTeW.GerarEnderReceb;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CTe.Receb.EnderReceb.cPais,
                     CTe.Receb.EnderReceb.UF,
                     CTe.Receb.EnderReceb.xMun,
                     CTe.Receb.EnderReceb.cMun);

  Gerador.wGrupo('enderReceb', '#166');
  Gerador.wCampo(tcStr, '#167', 'xLgr   ', 02, 255, 1, CTe.Receb.EnderReceb.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#168', 'nro    ', 01, 060, 1, CTe.Receb.EnderReceb.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#169', 'xCpl   ', 01, 060, 0, CTe.Receb.EnderReceb.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#170', 'xBairro', 02, 060, 1, CTe.Receb.EnderReceb.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#171', 'cMun   ', 07, 007, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(CTe.Receb.EnderReceb.cMun) then
    Gerador.wAlerta('#171', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#172', 'xMun', 02, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#173', 'CEP ', 08, 08, 0, CTe.Receb.EnderReceb.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#174', 'UF  ', 02, 02, 1, xUF, DSC_UF);

  if not ValidarUF(xUF) then
    Gerador.wAlerta('#174', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcInt, '#175', 'cPais', 04, 04, 0, CTe.Receb.EnderReceb.cPais, DSC_CPAIS); // Conforme NT-2009/01
  Gerador.wCampo(tcStr, '#176', 'xPais', 02, 60, 0, CTe.Receb.EnderReceb.xPais, DSC_XPAIS);
  Gerador.wGrupo('/enderReceb');
end;

procedure TCTeW.GerarDest;
var
  xNome: string;

procedure GeraIE;
begin
  if Trim(CTe.Dest.IE) = 'ISENTO' then
    Gerador.wCampo(tcStr, '#181', 'IE', 00, 14, 1, CTe.Dest.IE, DSC_IE)
  else
    Gerador.wCampo(tcStr, '#181', 'IE', 00, 14, 0, OnlyNumber(CTe.Dest.IE), DSC_IE);

  if (FOpcoes.ValidarInscricoes) then
    if not ValidarIE(CTe.Dest.IE, CTe.Dest.EnderDest.UF) then
      Gerador.wAlerta('#181', 'IE', DSC_IE, ERR_MSG_INVALIDO);
end;

begin
  if (trim(CTe.Dest.CNPJCPF) <> '') or (trim(CTe.Dest.xNome) <> '') then
  begin
    if (VersaoDF <= ve300) or (CTe.ide.cUF = 51) then
      xNome := xRazao3
    else
      xNome := xRazao4;

    Gerador.wGrupo('dest', '#178');

    if CTe.Dest.EnderDest.cPais = 0 then
      CTe.Dest.EnderDest.cPais := 1058;

    if CTe.Dest.EnderDest.cPais = 1058 then
      Gerador.wCampoCNPJCPF('#179', '#180', CTe.Dest.CNPJCPF)
    else
      Gerador.wCampo(tcStr, '#179', 'CNPJ', 00, 14, 1, '00000000000000', DSC_CNPJ);

    GeraIE;

    if CTe.Ide.tpAmb = taHomologacao then
      Gerador.wCampo(tcStr, '#182', 'xNome', 02, 60, 1, xNome, DSC_XNOME)
    else
      Gerador.wCampo(tcStr, '#182', 'xNome', 02, 60, 1, CTe.Dest.xNome, DSC_XNOME);

    Gerador.wCampo(tcStr, '#183', 'fone', 07, 12, 0, OnlyNumber(CTe.Dest.fone), DSC_FONE);
    Gerador.wCampo(tcStr, '#184', 'ISUF', 08, 09, 0, CTe.Dest.ISUF, DSC_ISUF);

    if (FOpcoes.ValidarInscricoes) and (trim(CTe.Dest.ISUF) <> '') then
      if not ValidarISUF(CTe.Dest.ISUF) then
        Gerador.wAlerta('#184', 'ISUF', DSC_ISUF, ERR_MSG_INVALIDO);

    GerarEnderDest;
    Gerador.wCampo(tcStr, '#196', 'email  ', 01, 60, 0, CTe.Dest.email, DSC_EMAIL);

    Gerador.wGrupo('/dest');
  end;
end;

procedure TCTeW.GerarEnderDest;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CTe.Dest.EnderDest.cPais,
                     CTe.Dest.EnderDest.UF,
                     CTe.Dest.EnderDest.xMun,
                     CTe.Dest.EnderDest.cMun);

  Gerador.wGrupo('enderDest', '#185');
  Gerador.wCampo(tcStr, '#186', 'xLgr   ', 02, 255, 1, CTe.Dest.EnderDest.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#187', 'nro    ', 01, 060, 1, CTe.Dest.EnderDest.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#188', 'xCpl   ', 01, 060, 0, CTe.Dest.EnderDest.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#189', 'xBairro', 02, 060, 1, CTe.Dest.EnderDest.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#190', 'cMun   ', 07, 007, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(CTe.Dest.EnderDest.cMun) then
    Gerador.wAlerta('#190', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#191', 'xMun', 02, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#192', 'CEP ', 08, 08, 0, CTe.Dest.EnderDest.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#193', 'UF  ', 02, 02, 1, xUF, DSC_UF);

  if not ValidarUF(xUF) then
    Gerador.wAlerta('#193', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcInt, '#194', 'cPais', 04, 04, 0, CTe.Dest.EnderDest.cPais, DSC_CPAIS); // Conforme NT-2009/01
  Gerador.wCampo(tcStr, '#195', 'xPais', 02, 60, 0, CTe.Dest.EnderDest.xPais, DSC_XPAIS);
  Gerador.wGrupo('/enderDest');
end;

procedure TCTeW.GerarVPrest;
begin
  Gerador.wGrupo('vPrest', '#208');
  Gerador.wCampo(tcDe2, '#209', 'vTPrest', 01, 15, 1, CTe.vPrest.vTPrest, DSC_VTPREST);
  Gerador.wCampo(tcDe2, '#210', 'vRec   ', 01, 15, 1, CTe.vPrest.vRec, DSC_VREC);

  GerarComp;
  Gerador.wGrupo('/vPrest');
end;

procedure TCTeW.GerarComp;
var
  i: Integer;
begin
  for i := 0 to CTe.vPrest.comp.Count - 1 do
  begin
    if (trim(CTe.vPrest.comp[i].xNome) <> '') and (CTe.vPrest.comp[i].vComp <> 0) then
    begin
      Gerador.wGrupo('Comp', '#211');
      Gerador.wCampo(tcStr, '#212', 'xNome', 01, 15, 1, CTe.vPrest.comp[i].xNome, DSC_XNOMEC);
      Gerador.wCampo(tcDe2, '#213', 'vComp', 01, 15, 1, CTe.vPrest.comp[i].vComp, DSC_VCOMP);
      Gerador.wGrupo('/Comp');
    end;
  end;
end;

procedure TCTeW.GerarImp;
begin
  Gerador.wGrupo('imp', '#214');
  GerarICMS;
  Gerador.wCampo(tcDe2, '#250', 'vTotTrib  ', 01, 15, 0, CTe.Imp.vTotTrib, DSC_VCOMP);
  Gerador.wCampo(tcStr, '#251', 'infAdFisco', 01, 2000, 0, CTe.Imp.InfAdFisco, DSC_INFADFISCO);

  GerarICMSUFFim;

  if (VersaoDF >= ve300) and (ModeloDF = moCTeOS) then
    GerarinfTribFed;

  Gerador.wGrupo('/imp');
end;

procedure TCTeW.GerarICMS;
begin
  Gerador.wGrupo('ICMS', '#215');

  case CTe.Imp.ICMS.SituTrib of
    cst00: GerarCST00;
    cst20: GerarCST20;
    cst40,
    cst41,
    cst51: GerarCST45;
    cst60: GerarCST60;
    cst90: GerarCST90;
    cstICMSOutraUF: GerarICMSOutraUF;
    cstICMSSN: GerarICMSSN;
  end;

  Gerador.wGrupo('/ICMS');
end;

procedure TCTeW.GerarCST00;
begin
  Gerador.wGrupo('ICMS00', '#216');
  Gerador.wCampo(tcStr, '#217', 'CST  ', 02, 02, 1, CSTICMSTOStr(CTe.Imp.ICMS.ICMS00.CST), DSC_CST);
  Gerador.wCampo(tcDe2, '#218', 'vBC  ', 01, 15, 1, CTe.Imp.ICMS.ICMS00.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, '#219', 'pICMS', 01, 05, 1, CTe.Imp.ICMS.ICMS00.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, '#220', 'vICMS', 01, 15, 1, CTe.Imp.ICMS.ICMS00.vICMS, DSC_VICMS);
  Gerador.wGrupo('/ICMS00');
end;

procedure TCTeW.GerarCST20;
begin
  Gerador.wGrupo('ICMS20', '#221');
  Gerador.wCampo(tcStr, '#222', 'CST   ', 02, 02, 1, CSTICMSTOStr(CTe.Imp.ICMS.ICMS20.CST), DSC_CST);
  Gerador.wCampo(tcDe2, '#223', 'pRedBC', 01, 05, 1, CTe.Imp.ICMS.ICMS20.pRedBC, DSC_PREDBC);
  Gerador.wCampo(tcDe2, '#224', 'vBC   ', 01, 15, 1, CTe.Imp.ICMS.ICMS20.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, '#225', 'pICMS ', 01, 05, 1, CTe.Imp.ICMS.ICMS20.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, '#226', 'vICMS ', 01, 15, 1, CTe.Imp.ICMS.ICMS20.vICMS, DSC_VICMS);

  if CTe.Imp.ICMS.ICMS20.vICMSDeson > 0 then
  begin
    Gerador.wCampo(tcDe2, '#227', 'vICMSDeson', 1, 15, 1, CTe.Imp.ICMS.ICMS20.vICMSDeson, DSC_VICMSDESON);
    Gerador.wCampo(tcStr, '#228', 'cBenef', 8, 10, 1, CTe.Imp.ICMS.ICMS20.cBenef, DSC_CBENEF);
  end;

  Gerador.wGrupo('/ICMS20');
end;

procedure TCTeW.GerarCST45;
begin
  Gerador.wGrupo('ICMS45', '#227');
  Gerador.wCampo(tcStr, '#228', 'CST', 02, 02, 1, CSTICMSTOStr(CTe.Imp.ICMS.ICMS45.CST), DSC_CST);

  if CTe.Imp.ICMS.ICMS45.vICMSDeson > 0 then
  begin
    Gerador.wCampo(tcDe2, '#227', 'vICMSDeson', 1, 15, 1, CTe.Imp.ICMS.ICMS45.vICMSDeson, DSC_VICMSDESON);
    Gerador.wCampo(tcStr, '#228', 'cBenef', 8, 10, 1, CTe.Imp.ICMS.ICMS45.cBenef, DSC_CBENEF);
  end;

  Gerador.wGrupo('/ICMS45');
end;

procedure TCTeW.GerarCST60;
begin
  Gerador.wGrupo('ICMS60', '#229');
  Gerador.wCampo(tcStr, '#230', 'CST       ', 02, 02, 1, CSTICMSTOStr(CTe.Imp.ICMS.ICMS60.CST), DSC_CST);
  Gerador.wCampo(tcDe2, '#231', 'vBCSTRet  ', 01, 15, 1, CTe.Imp.ICMS.ICMS60.vBCSTRet, DSC_VBC);
  Gerador.wCampo(tcDe2, '#232', 'vICMSSTRet', 01, 15, 1, CTe.Imp.ICMS.ICMS60.vICMSSTRet, DSC_VICMS);
  Gerador.wCampo(tcDe2, '#233', 'pICMSSTRet', 01, 05, 1, CTe.Imp.ICMS.ICMS60.pICMSSTRet, DSC_PICMS);
  Gerador.wCampo(tcDe2, '#234', 'vCred     ', 01, 15, 0, CTe.Imp.ICMS.ICMS60.vCred, DSC_VCRED);

  if CTe.Imp.ICMS.ICMS60.vICMSDeson > 0 then
  begin
    Gerador.wCampo(tcDe2, '#227', 'vICMSDeson', 1, 15, 1, CTe.Imp.ICMS.ICMS60.vICMSDeson, DSC_VICMSDESON);
    Gerador.wCampo(tcStr, '#228', 'cBenef', 8, 10, 1, CTe.Imp.ICMS.ICMS60.cBenef, DSC_CBENEF);
  end;

  Gerador.wGrupo('/ICMS60');
end;

procedure TCTeW.GerarCST90;
begin
  Gerador.wGrupo('ICMS90', '#235');
  Gerador.wCampo(tcStr, '#236', 'CST   ', 02, 02, 1, CSTICMSTOStr(CTe.Imp.ICMS.ICMS90.CST), DSC_CST);
  Gerador.wCampo(tcDe2, '#237', 'pRedBC', 01, 05, 0, CTe.Imp.ICMS.ICMS90.pRedBC, DSC_PREDBC);
  Gerador.wCampo(tcDe2, '#238', 'vBC   ', 01, 15, 1, CTe.Imp.ICMS.ICMS90.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, '#239', 'pICMS ', 01, 05, 1, CTe.Imp.ICMS.ICMS90.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, '#240', 'vICMS ', 01, 15, 1, CTe.Imp.ICMS.ICMS90.vICMS, DSC_VICMS);
  Gerador.wCampo(tcDe2, '#241', 'vCred ', 01, 15, 0, CTe.Imp.ICMS.ICMS90.vCred, DSC_VCRED);

  if CTe.Imp.ICMS.ICMS90.vICMSDeson > 0 then
  begin
    Gerador.wCampo(tcDe2, '#227', 'vICMSDeson', 1, 15, 1, CTe.Imp.ICMS.ICMS90.vICMSDeson, DSC_VICMSDESON);
    Gerador.wCampo(tcStr, '#228', 'cBenef', 8, 10, 1, CTe.Imp.ICMS.ICMS90.cBenef, DSC_CBENEF);
  end;

  Gerador.wGrupo('/ICMS90');
end;

procedure TCTeW.GerarICMSOutraUF;
begin
  Gerador.wGrupo('ICMSOutraUF', '#242');
  Gerador.wCampo(tcStr, '#243', 'CST          ', 02, 02, 1, CSTICMSTOStr(CTe.Imp.ICMS.ICMSOutraUF.CST), DSC_CST);
  Gerador.wCampo(tcDe2, '#244', 'pRedBCOutraUF', 01, 05, 0, CTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF, DSC_PREDBC);
  Gerador.wCampo(tcDe2, '#245', 'vBCOutraUF   ', 01, 15, 1, CTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF, DSC_VBC);
  Gerador.wCampo(tcDe2, '#246', 'pICMSOutraUF ', 01, 05, 1, CTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF, DSC_PICMS);
  Gerador.wCampo(tcDe2, '#247', 'vICMSOutraUF ', 01, 15, 1, CTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF, DSC_VICMS);

  if CTe.Imp.ICMS.ICMSOutraUF.vICMSDeson > 0 then
  begin
    Gerador.wCampo(tcDe2, '#227', 'vICMSDeson', 1, 15, 1, CTe.Imp.ICMS.ICMSOutraUF.vICMSDeson, DSC_VICMSDESON);
    Gerador.wCampo(tcStr, '#228', 'cBenef', 8, 10, 1, CTe.Imp.ICMS.ICMSOutraUF.cBenef, DSC_CBENEF);
  end;

  Gerador.wGrupo('/ICMSOutraUF');
end;

procedure TCTeW.GerarICMSSN;
begin
  Gerador.wGrupo('ICMSSN', '#248');

  if (VersaoDF >= ve300) then
    Gerador.wCampo(tcStr, '#248', 'CST', 02, 02, 1, '90', DSC_CST);

  Gerador.wCampo(tcInt, '#249', 'indSN', 01, 01, 1, CTe.Imp.ICMS.ICMSSN.indSN, DSC_INDSN);
  Gerador.wGrupo('/ICMSSN');
end;

procedure TCTeW.GerarICMSUFFim;
begin
  // Grupo a ser informado nas prestações de serviços de transporte interestaduais
  // para consumidor final, não contribuinte do ICMS.

  if ((CTe.Imp.ICMSUFFim.vBCUFFim <> 0) or (CTe.Imp.ICMSUFFim.pFCPUFFim <> 0) or
     (CTe.Imp.ICMSUFFim.pICMSUFFim <> 0) or (CTe.Imp.ICMSUFFim.vFCPUFFim <> 0) or
	 (CTe.Imp.ICMSUFFim.vICMSUFFim <> 0) or (CTe.Imp.ICMSUFFim.vICMSUFIni <> 0)) or
     ((CTe.ide.UFIni <> CTe.ide.UFFim) and (CTe.ide.indIEToma = inNaoContribuinte)) then
  begin
    Gerador.wGrupo('ICMSUFFim', '#248a');
    Gerador.wCampo(tcDe2, '#', 'vBCUFFim      ', 01, 15, 1, CTe.Imp.ICMSUFFim.vBCUFFim, DSC_VBC);
    Gerador.wCampo(tcDe2, '#', 'pFCPUFFim     ', 01, 05, 1, CTe.Imp.ICMSUFFim.pFCPUFFim, DSC_PICMS);
    Gerador.wCampo(tcDe2, '#', 'pICMSUFFim    ', 01, 05, 1, CTe.Imp.ICMSUFFim.pICMSUFFim, DSC_PICMS);
    Gerador.wCampo(tcDe2, '#', 'pICMSInter    ', 01, 05, 1, CTe.Imp.ICMSUFFim.pICMSInter, DSC_PICMS);
    // Na versão 3.00a não tem mais: pICMSInterPart
//    Gerador.wCampo(tcDe2, '#', 'pICMSInterPart', 01, 05, 0, CTe.Imp.ICMSUFFim.pICMSInterPart, DSC_PICMS);
    Gerador.wCampo(tcDe2, '#', 'vFCPUFFim     ', 01, 15, 1, CTe.Imp.ICMSUFFim.vFCPUFFim, DSC_PICMS);
    Gerador.wCampo(tcDe2, '#', 'vICMSUFFim    ', 01, 15, 1, CTe.Imp.ICMSUFFim.vICMSUFFim, DSC_VICMS);
    Gerador.wCampo(tcDe2, '#', 'vICMSUFIni    ', 01, 15, 1, CTe.Imp.ICMSUFFim.vICMSUFIni, DSC_VICMS);
    Gerador.wGrupo('/ICMSUFFim');
  end;
end;

procedure TCTeW.GerarinfTribFed;
  function InformarINSS: Integer;
  begin
    if ((Length(OnlyNumber(Trim(CTe.toma.CNPJCPF))) = 14) or
        (CTe.toma.EnderToma.cPais <> 1058)) and
       (CTe.Ide.tpServ in [tsTranspPessoas, tsExcessoBagagem]) then
      Result := 1
    else
      Result := 0;
  end;
begin
  if ((CTe.Imp.infTribFed.vPIS > 0) or (CTe.Imp.infTribFed.vCOFINS > 0) or
      (CTe.Imp.infTribFed.vIR > 0) or ((CTe.Imp.infTribFed.vINSS > 0) or
      (InformarINSS = 1)) or (CTe.Imp.infTribFed.vCSLL > 0)) then
  begin
    Gerador.wGrupo('infTribFed', '#125');
    Gerador.wCampo(tcDe2, '#', 'vPIS   ', 01, 15, 0, CTe.Imp.infTribFed.vPIS, DSC_VPIS);
    Gerador.wCampo(tcDe2, '#', 'vCOFINS', 01, 15, 0, CTe.Imp.infTribFed.vCOFINS, DSC_VCOFINS);
    Gerador.wCampo(tcDe2, '#', 'vIR    ', 01, 15, 0, CTe.Imp.infTribFed.vIR, DSC_VIR);
    Gerador.wCampo(tcDe2, '#', 'vINSS  ', 01, 15, InformarINSS, CTe.Imp.infTribFed.vINSS, DSC_VINSS);
    Gerador.wCampo(tcDe2, '#', 'vCSLL  ', 01, 15, 0, CTe.Imp.infTribFed.vCSLL, DSC_VCSLL);
    Gerador.wGrupo('/infTribFed');
  end;
end;

procedure TCTeW.GerarInfCTeNorm;
var
  versao: String;
begin
  if (CTe.Ide.tpCTe = tcNormal) or (CTe.Ide.tpCTe = tcSubstituto) then
  begin
    Gerador.wGrupo('infCTeNorm', '#252');

    if (VersaoDF >= ve300) and (ModeloDF = moCTeOS) then
    begin
      GerarinfServico;
      GerarinfDocRef;
      GerarInfSeg;
    end
    else
    begin
      GerarinfCarga;

      if (CTe.Ide.tpServ <> tsIntermediario) and (CTe.Ide.tpServ <> tsMultimodal) then
        GerarInfDoc;

      if CTe.infCTeNorm.docAnt.emiDocAnt.Count > 0 then
        GerarDocAnt;

      if VersaoDF = ve200 then
        GerarInfSeg;
    end;

    versao := GetVersaoModalCTe(VersaoDF, CTe.Ide.modal);

    if (ModeloDF = moCTe) or ((ModeloDF = moCTeOS) and
       (CTe.ide.modal = mdRodoviario) and (CTe.ide.tpServ <> tsTranspValores)) then
    begin
      case StrToInt(TpModalToStr(CTe.Ide.modal)) of
        01: Gerador.wGrupo('infModal versaoModal="' + versao + '"', '#366');
        02: Gerador.wGrupo('infModal versaoModal="' + versao + '"', '#366');
        03: Gerador.wGrupo('infModal versaoModal="' + versao + '"', '#366');
        04: Gerador.wGrupo('infModal versaoModal="' + versao + '"', '#366');
        05: Gerador.wGrupo('infModal versaoModal="' + versao + '"', '#366');
        06: Gerador.wGrupo('infModal versaoModal="' + versao + '"', '#366');
      end;

      case StrToInt(TpModalToStr(CTe.Ide.modal)) of
        01: if ModeloDF = moCTe then // Informações do Modal Rodoviário
              GerarRodo
            else
              GerarRodoOS;
        02: GerarAereo;      // Informações do Modal Aéreo
        03: GerarAquav;      // Informações do Modal Aquaviário
        04: GerarFerrov;     // Informações do Modal Ferroviário
        05: GerarDuto;       // Informações do Modal Dutoviário
        06: GerarMultimodal; // Informações do Multimodal
      end;

      Gerador.wGrupo('/infModal');
    end;

    if (ModeloDF = moCTe) then
    begin
      if VersaoDF = ve200 then
        GerarPeri;

      GerarVeicNovos;
      GerarCobr;

      GerarInfCTeSub;

      if (VersaoDF >= ve300) and (Trim(CTe.infCTeNorm.infGlobalizado.xObs) <> '') then
        GerarInfGlobalizado;

      if VersaoDF >= ve300 then
        GerarInfServVinc;
    end;

    if ModeloDF = moCTeOS then
    begin
      GerarInfCTeSub;

      if CTe.Ide.tpServ = tsTranspValores then
      begin
        Gerador.wCampo(tcEsp, '#', 'refCTeCanc', 44, 44, 0, OnlyNumber(CTe.infCTeNorm.refCTeCanc), DSC_CHAVE);

        if OnlyNumber(CTe.infCTeNorm.refCTeCanc) <> '' then
          if not ValidarChave(CTe.infCTeNorm.refCTeCanc) then
            Gerador.wAlerta('#', 'refCTeCanc', DSC_REFNFE, ERR_MSG_INVALIDO);
      end;

      GerarCobr;
      GerarInfGTVe;
    end;

    Gerador.wGrupo('/infCTeNorm');
  end;
end;

procedure TCTeW.GerarinfServico;
begin
  Gerador.wGrupo('infServico', '#132');
  Gerador.wCampo(tcStr, '#133', 'xDescServ', 01, 30, 1, CTe.infCTeNorm.infServico.xDescServ, DSC_XDESCSERV);

  if CTe.infCTeNorm.infServico.qCarga > 0 then
  begin
    Gerador.wGrupo('infQ', '#134');
    Gerador.wCampo(tcDe4, '#135', 'qCarga', 01, 15, 1, CTe.infCTeNorm.infServico.qCarga, DSC_QCARGA);
    Gerador.wGrupo('/infQ');
  end;

  Gerador.wGrupo('/infServico');
end;

procedure TCTeW.GerarinfDocRef;
var
  i: Integer;
begin
  for i := 0 to CTe.infCTeNorm.infDocRef.Count - 1 do
  begin
    Gerador.wGrupo('infDocRef', '#136');

    if CTe.infCTeNorm.infDocRef.Items[i].chBPe = '' then
    begin
      Gerador.wCampo(tcEsp, '#137', 'nDoc    ', 01, 20, 1, OnlyNumber(CTe.infCTeNorm.infDocRef.Items[i].nDoc), DSC_NDOC);
      Gerador.wCampo(tcStr, '#138', 'serie   ', 01, 03, 0, CTe.infCTeNorm.infDocRef.Items[i].serie, DSC_SERIE);
      Gerador.wCampo(tcStr, '#139', 'subserie', 01, 03, 0, CTe.infCTeNorm.infDocRef.Items[i].subserie, DSC_SERIE);
      Gerador.wCampo(tcDat, '#140', 'dEmi    ', 10, 10, 1, CTe.infCTeNorm.infDocRef.Items[i].dEmi, DSC_DEMI);
      Gerador.wCampo(tcDe2, '#141', 'vDoc    ', 01, 15, 0, CTe.infCTeNorm.infDocRef.Items[i].vDoc, DSC_VNF);
    end
    else
      Gerador.wCampo(tcStr, '#137', 'chBPe', 44, 44, 1, OnlyNumber(CTe.infCTeNorm.infDocRef.Items[i].chBPe), DSC_CHBPE);

    if OnlyNumber(CTe.infCTeNorm.infDocRef.Items[i].chBPe) <> '' then
      if not ValidarChave(CTe.infCTeNorm.infDocRef.Items[i].chBPe) then
        Gerador.wAlerta('#137', 'chBPe', DSC_CHBPE, ERR_MSG_INVALIDO);

    Gerador.wGrupo('/infDocRef');
  end;

  if CTe.infCTeNorm.infDocRef.Count > 990 then
    Gerador.wAlerta('#136', 'infDocRef', DSC_INFQ, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarinfCarga;
begin
  Gerador.wGrupo('infCarga', '#253');
  Gerador.wCampo(tcDe2, '#254', 'vCarga  ', 01, 15, 1, CTe.infCTeNorm.InfCarga.vCarga, DSC_VTMERC);
  Gerador.wCampo(tcStr, '#255', 'proPred ', 01, 60, 1, CTe.infCTeNorm.InfCarga.proPred, DSC_PRED);
  Gerador.wCampo(tcStr, '#256', 'xOutCat ', 01, 30, 0, CTe.infCTeNorm.InfCarga.xOutCat, DSC_OUTCAT);

  GerarInfQ;

  if VersaoDF >= ve300 then
    Gerador.wCampo(tcDe2, '#250', 'vCargaAverb', 01, 15, 0, CTe.infCTeNorm.InfCarga.vCargaAverb, DSC_VTMERC);

  Gerador.wGrupo('/infCarga');
end;

procedure TCTeW.GerarInfQ;
var
  i: Integer;
begin
  for i := 0 to CTe.infCTeNorm.InfCarga.InfQ.Count - 1 do
  begin
    Gerador.wGrupo('infQ', '#257');
    Gerador.wCampo(tcStr, '#258', 'cUnid ', 02, 02, 1, UnidMedToStr(CTe.infCTeNorm.InfCarga.InfQ[i].cUnid), DSC_CUNID);
    Gerador.wCampo(tcStr, '#259', 'tpMed ', 01, 20, 1, CTe.infCTeNorm.InfCarga.InfQ[i].tpMed, DSC_TPMED);
    Gerador.wCampo(tcDe4, '#260', 'qCarga', 01, 15, 1, CTe.infCTeNorm.InfCarga.InfQ[i].qCarga, DSC_QTD);

    Gerador.wGrupo('/infQ');
  end;

  if CTe.infCTeNorm.InfCarga.InfQ.Count > 990 then
    Gerador.wAlerta('#257', 'infQ', DSC_INFQ, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarinfDoc;
begin
  Gerador.wGrupo('infDoc', '#261');
  GerarInfNF;
  GerarInfNFe;
  GerarInfOutros;
  Gerador.wGrupo('/infDoc');
end;

procedure TCTeW.GerarInfNF;
var
  i, j, k, l: Integer;
begin
  for i := 0 to CTe.infCTeNorm.infDoc.infNF.Count - 1 do
  begin
    Gerador.wGrupo('infNF', '#262');
    Gerador.wCampo(tcStr, '#263', 'nRoma', 01, 20, 0, CTe.infCTeNorm.infDoc.InfNF[i].nRoma, DSC_NROMA);
    Gerador.wCampo(tcStr, '#264', 'nPed ', 01, 20, 0, CTe.infCTeNorm.infDoc.InfNF[i].nPed, DSC_NPED);
    Gerador.wCampo(tcStr, '#265', 'mod  ', 02, 02, 1, ModeloNFToStr(CTe.infCTeNorm.infDoc.InfNF[i].modelo), DSC_MOD);
    Gerador.wCampo(tcStr, '#266', 'serie', 01, 03, 1, CTe.infCTeNorm.infDoc.InfNF[i].serie, DSC_SERIE);
    Gerador.wCampo(tcEsp, '#267', 'nDoc ', 01, 20, 1, OnlyNumber(CTe.infCTeNorm.infDoc.InfNF[i].nDoc), DSC_NDOC);
    Gerador.wCampo(tcDat, '#268', 'dEmi ', 10, 10, 1, CTe.infCTeNorm.infDoc.InfNF[i].dEmi, DSC_DEMI);
    Gerador.wCampo(tcDe2, '#269', 'vBC  ', 01, 15, 1, CTe.infCTeNorm.infDoc.InfNF[i].vBC, DSC_VBC);
    Gerador.wCampo(tcDe2, '#270', 'vICMS', 01, 15, 1, CTe.infCTeNorm.infDoc.InfNF[i].vICMS, DSC_VICMS);
    Gerador.wCampo(tcDe2, '#271', 'vBCST', 01, 15, 1, CTe.infCTeNorm.infDoc.InfNF[i].vBCST, DSC_VBCST);
    Gerador.wCampo(tcDe2, '#272', 'vST  ', 01, 15, 1, CTe.infCTeNorm.infDoc.InfNF[i].vST, DSC_VST);
    Gerador.wCampo(tcDe2, '#273', 'vProd', 01, 15, 1, CTe.infCTeNorm.infDoc.InfNF[i].vProd, DSC_VPROD);
    Gerador.wCampo(tcDe2, '#274', 'vNF  ', 01, 15, 1, CTe.infCTeNorm.infDoc.InfNF[i].vNF, DSC_VNF);
    Gerador.wCampo(tcInt, '#275', 'nCFOP', 04, 04, 1, CTe.infCTeNorm.infDoc.InfNF[i].nCFOP, DSC_CFOP);
    Gerador.wCampo(tcDe3, '#276', 'nPeso', 01, 15, 0, CTe.infCTeNorm.infDoc.InfNF[i].nPeso, DSC_PESO);
    Gerador.wCampo(tcStr, '#277', 'PIN  ', 02, 09, 0, CTe.infCTeNorm.infDoc.InfNF[i].PIN, DSC_ISUF);

    if (FOpcoes.ValidarInscricoes) and (trim(CTe.infCTeNorm.infDoc.InfNF[i].PIN) <> '') then
      if not ValidarISUF(CTe.infCTeNorm.infDoc.InfNF[i].PIN) then
        Gerador.wAlerta('#277', 'PIN', DSC_ISUF, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcDat, '#278', 'dPrev', 10, 10, 0, CTe.infCTeNorm.infDoc.InfNF[i].dPrev, DSC_DPREV);

    for j := 0 to CTe.infCTeNorm.infDoc.infNF[i].infUnidCarga.Count - 1 do
    begin
      Gerador.wGrupo('infUnidCarga', '#291');
      Gerador.wCampo(tcStr, '#292', 'tpUnidCarga', 01, 01, 1, UnidCargaToStr(CTe.infCTeNorm.infDoc.infNF[i].infUnidCarga[j].tpUnidCarga), DSC_TPUNIDCARGA);
      Gerador.wCampo(tcStr, '#293', 'idUnidCarga', 01, 20, 1, CTe.infCTeNorm.infDoc.infNF[i].infUnidCarga[j].idUnidCarga, DSC_IDUNIDCARGA);

      for k := 0 to CTe.infCTeNorm.infDoc.infNF[i].infUnidCarga[j].lacUnidCarga.Count - 1 do
      begin
        Gerador.wGrupo('lacUnidCarga', '#294');
        Gerador.wCampo(tcStr, '#295', 'nLacre', 01, 20, 1, CTe.infCTeNorm.infDoc.infNF[i].infUnidCarga[j].lacUnidCarga[k].nLacre, DSC_NLACRE);
        Gerador.wGrupo('/lacUnidCarga');
      end;

      Gerador.wCampo(DefineArredondamentoQtdRat, '#296', 'qtdRat', 01, 05, 0, CTe.infCTeNorm.infDoc.infNF[i].infUnidCarga[j].qtdRat, DSC_QTDRAT);

      Gerador.wGrupo('/infUnidCarga');
    end;

    for j := 0 to CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp.Count - 1 do
    begin
      Gerador.wGrupo('infUnidTransp', '#279');
      Gerador.wCampo(tcStr, '#280', 'tpUnidTransp', 01, 01, 1, UnidTranspToStr(CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].tpUnidTransp), DSC_TPUNIDTRANSP);
      Gerador.wCampo(tcStr, '#281', 'idUnidTransp', 01, 20, 1, CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].idUnidTransp, DSC_IDUNIDTRANSP);

      for k := 0 to CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].lacUnidTransp.Count - 1 do
      begin
        Gerador.wGrupo('lacUnidTransp', '#282');
        Gerador.wCampo(tcStr, '#283', 'nLacre', 01, 20, 1, CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].lacUnidTransp[k].nLacre, DSC_NLACRE);
        Gerador.wGrupo('/lacUnidTransp');
      end;

      for k := 0 to CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].infUnidCarga.Count - 1 do
      begin
        Gerador.wGrupo('infUnidCarga', '#284');
        Gerador.wCampo(tcStr, '#285', 'tpUnidCarga', 01, 01, 1, UnidCargaToStr(CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].infUnidCarga[k].tpUnidCarga), DSC_TPUNIDCARGA);
        Gerador.wCampo(tcStr, '#286', 'idUnidCarga', 01, 20, 1, CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].infUnidCarga[k].idUnidCarga, DSC_IDUNIDCARGA);

        for l := 0 to CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga.Count - 1 do
        begin
          Gerador.wGrupo('lacUnidCarga', '#287');
          Gerador.wCampo(tcStr, '#288', 'nLacre', 01, 20, 1, CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga[l].nLacre, DSC_NLACRE);
          Gerador.wGrupo('/lacUnidCarga');
        end;
        Gerador.wCampo(DefineArredondamentoQtdRat, '#289', 'qtdRat', 01, 05, 0, CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].infUnidCarga[k].qtdRat, DSC_QTDRAT);

        Gerador.wGrupo('/infUnidCarga');
      end;

      Gerador.wCampo(DefineArredondamentoQtdRat, '#290', 'qtdRat', 01, 05, 0, CTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].qtdRat, DSC_QTDRAT);

      Gerador.wGrupo('/infUnidTransp');
    end;

    Gerador.wGrupo('/infNF');
  end;

  if CTe.infCTeNorm.infDoc.InfNF.Count > 990 then
    Gerador.wAlerta('#262', 'infNF', DSC_INFNF, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarInfNFe;
var
  i, j, k, l: Integer;
begin
  for i := 0 to CTe.infCTeNorm.infDoc.InfNFe.Count - 1 do
  begin
    Gerador.wGrupo('infNFe', '#297');
    Gerador.wCampo(tcEsp, '#298', 'chave', 44, 44, 1, OnlyNumber(CTe.infCTeNorm.infDoc.InfNFe[i].chave), DSC_REFNFE);

    if OnlyNumber(CTe.infCTeNorm.infDoc.InfNFe[i].chave) <> '' then
      if not ValidarChave(CTe.infCTeNorm.infDoc.InfNFe[i].chave) then
        Gerador.wAlerta('#298', 'chave', DSC_REFNFE, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#299', 'PIN   ', 02, 09, 0, CTe.infCTeNorm.infDoc.InfNFe[i].PIN, DSC_ISUF);

    if (FOpcoes.ValidarInscricoes) and (trim(CTe.infCTeNorm.infDoc.InfNFe[i].PIN) <> '') then
      if not ValidarISUF(CTe.infCTeNorm.infDoc.InfNFe[i].PIN) then
        Gerador.wAlerta('#299', 'PIN', DSC_ISUF, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcDat, '#300', 'dPrev', 10, 10, 0, CTe.infCTeNorm.infDoc.InfNFe[i].dPrev, DSC_DPREV);

    for j := 0 to CTe.infCTeNorm.infDoc.infNFe[i].infUnidCarga.Count - 1 do
    begin
      Gerador.wGrupo('infUnidCarga', '#313');
      Gerador.wCampo(tcStr, '#314', 'tpUnidCarga', 01, 01, 1, UnidCargaToStr(CTe.infCTeNorm.infDoc.infNFe[i].infUnidCarga[j].tpUnidCarga), DSC_TPUNIDCARGA);
      Gerador.wCampo(tcStr, '#315', 'idUnidCarga', 01, 20, 1, CTe.infCTeNorm.infDoc.infNFe[i].infUnidCarga[j].idUnidCarga, DSC_IDUNIDCARGA);

      for k := 0 to CTe.infCTeNorm.infDoc.infNFe[i].infUnidCarga[j].lacUnidCarga.Count - 1 do
      begin
        Gerador.wGrupo('lacUnidCarga', '#316');
        Gerador.wCampo(tcStr, '#317', 'nLacre', 01, 20, 1, CTe.infCTeNorm.infDoc.infNFe[i].infUnidCarga[j].lacUnidCarga[k].nLacre, DSC_NLACRE);
        Gerador.wGrupo('/lacUnidCarga');
      end;

      Gerador.wCampo(DefineArredondamentoQtdRat, '#318', 'qtdRat', 01, 05, 0, CTe.infCTeNorm.infDoc.infNFe[i].infUnidCarga[j].qtdRat, DSC_QTDRAT);

      Gerador.wGrupo('/infUnidCarga');
    end;

    for j := 0 to CTe.infCTeNorm.infDoc.infNFe[i].infUnidTransp.Count - 1 do
    begin
      Gerador.wGrupo('infUnidTransp', '#301');
      Gerador.wCampo(tcStr, '#302', 'tpUnidTransp', 01, 01, 1, UnidTranspToStr(CTe.infCTeNorm.infDoc.infNFe[i].infUnidTransp[j].tpUnidTransp), DSC_TPUNIDTRANSP);
      Gerador.wCampo(tcStr, '#303', 'idUnidTransp', 01, 20, 1, CTe.infCTeNorm.infDoc.infNFe[i].infUnidTransp[j].idUnidTransp, DSC_IDUNIDTRANSP);

      for k := 0 to CTe.infCTeNorm.infDoc.infNFe[i].infUnidTransp[j].lacUnidTransp.Count - 1 do
      begin
        Gerador.wGrupo('lacUnidTransp', '#304');
        Gerador.wCampo(tcStr, '#305', 'nLacre', 01, 20, 1, CTe.infCTeNorm.infDoc.infNFe[i].infUnidTransp[j].lacUnidTransp[k].nLacre, DSC_NLACRE);
        Gerador.wGrupo('/lacUnidTransp');
      end;

      for k := 0 to CTe.infCTeNorm.infDoc.infNFe[i].infUnidTransp[j].infUnidCarga.Count - 1 do
      begin
        Gerador.wGrupo('infUnidCarga', '#306');
        Gerador.wCampo(tcStr, '#307', 'tpUnidCarga', 01, 01, 1, UnidCargaToStr(CTe.infCTeNorm.infDoc.infNFe[i].infUnidTransp[j].infUnidCarga[k].tpUnidCarga), DSC_TPUNIDCARGA);
        Gerador.wCampo(tcStr, '#308', 'idUnidCarga', 01, 20, 1, CTe.infCTeNorm.infDoc.infNFe[i].infUnidTransp[j].infUnidCarga[k].idUnidCarga, DSC_IDUNIDCARGA);

        for l := 0 to CTe.infCTeNorm.infDoc.infNFe[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga.Count - 1 do
        begin
          Gerador.wGrupo('lacUnidCarga', '#309');
          Gerador.wCampo(tcStr, '#310', 'nLacre', 01, 20, 1, CTe.infCTeNorm.infDoc.infNFe[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga[l].nLacre, DSC_NLACRE);
          Gerador.wGrupo('/lacUnidCarga');
        end;

        Gerador.wCampo(DefineArredondamentoQtdRat, '#311', 'qtdRat', 01, 05, 0, CTe.infCTeNorm.infDoc.infNFe[i].infUnidTransp[j].infUnidCarga[k].qtdRat, DSC_QTDRAT);

        Gerador.wGrupo('/infUnidCarga');
      end;

      Gerador.wCampo(DefineArredondamentoQtdRat, '#312', 'qtdRat', 01, 05, 0, CTe.infCTeNorm.infDoc.infNFe[i].infUnidTransp[j].qtdRat, DSC_QTDRAT);

      Gerador.wGrupo('/infUnidTransp');
    end;

    Gerador.wGrupo('/infNFe');
  end;

  if CTe.infCTeNorm.infDoc.InfNFe.Count > 990 then
    Gerador.wAlerta('#297', 'infNFe', DSC_INFNFE, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarInfOutros;
var
  i, j, k, l: Integer;
begin
  for i := 0 to CTe.infCTeNorm.infDoc.InfOutros.Count - 1 do
  begin
    Gerador.wGrupo('infOutros', '#319');
    Gerador.wCampo(tcStr, '#320', 'tpDoc     ', 02, 002, 1, TpDocumentoToStr(CTe.infCTeNorm.infDoc.InfOutros[i].tpDoc), DSC_TPDOC);
    Gerador.wCampo(tcStr, '#321', 'descOutros', 01, 100, 0, CTe.infCTeNorm.infDoc.InfOutros[i].descOutros, DSC_OUTROS);
    Gerador.wCampo(tcStr, '#322', 'nDoc      ', 01, 020, 0, CTe.infCTeNorm.infDoc.InfOutros[i].nDoc, DSC_NRO);
    Gerador.wCampo(tcDat, '#323', 'dEmi      ', 10, 010, 0, CTe.infCTeNorm.infDoc.InfOutros[i].dEmi, DSC_DEMI);
    Gerador.wCampo(tcDe2, '#324', 'vDocFisc  ', 01, 015, 0, CTe.infCTeNorm.infDoc.InfOutros[i].vDocFisc, DSC_VDOC);
    Gerador.wCampo(tcDat, '#325', 'dPrev     ', 10, 010, 0, CTe.infCTeNorm.infDoc.infOutros[i].dPrev, DSC_DPREV);

    for j := 0 to CTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga.Count - 1 do
    begin
      Gerador.wGrupo('infUnidCarga', '#338');
      Gerador.wCampo(tcStr, '#339', 'tpUnidCarga', 01, 01, 1, UnidCargaToStr(CTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga[j].tpUnidCarga), DSC_TPUNIDCARGA);
      Gerador.wCampo(tcStr, '#340', 'idUnidCarga', 01, 20, 1, CTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga[j].idUnidCarga, DSC_IDUNIDCARGA);

      for k := 0 to CTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga[j].lacUnidCarga.Count - 1 do
      begin
        Gerador.wGrupo('lacUnidCarga', '#341');
        Gerador.wCampo(tcStr, '#342', 'nLacre', 01, 20, 1, CTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga[j].lacUnidCarga[k].nLacre, DSC_NLACRE);
        Gerador.wGrupo('/lacUnidCarga');
      end;

      Gerador.wCampo(DefineArredondamentoQtdRat, '#343', 'qtdRat', 01, 05, 0, CTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga[j].qtdRat, DSC_QTDRAT);

      Gerador.wGrupo('/infUnidCarga');
    end;

    for j := 0 to CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp.Count - 1 do
    begin
      Gerador.wGrupo('infUnidTransp', '#326');
      Gerador.wCampo(tcStr, '#327', 'tpUnidTransp', 01, 01, 1, UnidTranspToStr(CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp[j].tpUnidTransp), DSC_TPUNIDTRANSP);
      Gerador.wCampo(tcStr, '#328', 'idUnidTransp', 01, 20, 1, CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp[j].idUnidTransp, DSC_IDUNIDTRANSP);

      for k := 0 to CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp[j].lacUnidTransp.Count - 1 do
      begin
        Gerador.wGrupo('lacUnidTransp', '#329');
        Gerador.wCampo(tcStr, '#330', 'nLacre', 01, 20, 1, CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp[j].lacUnidTransp[k].nLacre, DSC_NLACRE);
        Gerador.wGrupo('/lacUnidTransp');
      end;

      for k := 0 to CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp[j].infUnidCarga.Count - 1 do
      begin
        Gerador.wGrupo('infUnidCarga', '#331');
        Gerador.wCampo(tcStr, '#332', 'tpUnidCarga', 01, 01, 1, UnidCargaToStr(CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp[j].infUnidCarga[k].tpUnidCarga), DSC_TPUNIDCARGA);
        Gerador.wCampo(tcStr, '#333', 'idUnidCarga', 01, 20, 1, CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp[j].infUnidCarga[k].idUnidCarga, DSC_IDUNIDCARGA);

        for l := 0 to CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga.Count - 1 do
        begin
          Gerador.wGrupo('lacUnidCarga', '#334');
          Gerador.wCampo(tcStr, '#335', 'nLacre', 01, 20, 1, CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga[l].nLacre, DSC_NLACRE);
          Gerador.wGrupo('/lacUnidCarga');
        end;

        Gerador.wCampo(DefineArredondamentoQtdRat, '#336', 'qtdRat', 01, 05, 0, CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp[j].infUnidCarga[k].qtdRat, DSC_QTDRAT);

        Gerador.wGrupo('/infUnidCarga');
      end;
      Gerador.wCampo(DefineArredondamentoQtdRat, '#337', 'qtdRat', 01, 05, 0, CTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp[j].qtdRat, DSC_QTDRAT);

      Gerador.wGrupo('/infUnidTransp');
    end;

    Gerador.wGrupo('/infOutros');
  end;

  if CTe.infCTeNorm.infDoc.InfOutros.Count > 990 then
    Gerador.wAlerta('#319', 'infOutros', DSC_INFOUTRO, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarDocAnt;
var
  i, i01, i02: Integer;
begin
  Gerador.wGrupo('docAnt', '#344');

  for i := 0 to CTe.infCTeNorm.docAnt.emiDocAnt.Count - 1 do
  begin
    Gerador.wGrupo('emiDocAnt', '#345');
    Gerador.wCampoCNPJCPF('#346', '#347', CTe.infCTeNorm.docAnt.emiDocAnt[i].CNPJCPF);

    if Trim(CTe.infCTeNorm.docAnt.emiDocAnt[i].IE) = 'ISENTO' then
      Gerador.wCampo(tcStr, '#348', 'IE', 00, 14, 1, CTe.infCTeNorm.docAnt.emiDocAnt[i].IE, DSC_IE)
    else
      Gerador.wCampo(tcStr, '#348', 'IE', 02, 14, 1, OnlyNumber(CTe.infCTeNorm.docAnt.emiDocAnt[i].IE), DSC_IE);

    if (FOpcoes.ValidarInscricoes) then
      if not ValidarIE(CTe.infCTeNorm.docAnt.emiDocAnt[i].IE, CTe.infCTeNorm.docAnt.emiDocAnt[i].UF) then
        Gerador.wAlerta('#348', 'IE', DSC_IE, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#349', 'UF', 02, 02, 1, CTe.infCTeNorm.docAnt.emiDocAnt[i].UF, DSC_UF);

    if not ValidarUF(CTe.infCTeNorm.docAnt.emiDocAnt[i].UF) then
      Gerador.wAlerta('#349', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#350', 'xNome', 02, 60, 1, CTe.infCTeNorm.docAnt.emiDocAnt[i].xNome, DSC_XNOME);

    for i01 := 0 to CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt.Count - 1 do
    begin
      Gerador.wGrupo('idDocAnt', '#351');

      for i02 := 0 to CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntPap.Count - 1 do
      begin
        Gerador.wGrupo('idDocAntPap', '#352');
        Gerador.wCampo(tcStr, '#353', 'tpDoc ', 02, 02, 1, TpDocumentoAnteriorToStr(CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntPap[i02].tpDoc), DSC_TPNF);
        Gerador.wCampo(tcStr, '#354', 'serie ', 01, 03, 1, CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntPap[i02].serie, DSC_SERIE);
        Gerador.wCampo(tcStr, '#355', 'subser', 01, 02, 0, CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntPap[i02].subser, DSC_SERIE);
        Gerador.wCampo(tcStr, '#356', 'nDoc  ', 01, 30, 1, CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntPap[i02].nDoc, DSC_NDF);
        Gerador.wCampo(tcDat, '#357', 'dEmi  ', 10, 10, 1, CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntPap[i02].dEmi, DSC_DEMI);
        Gerador.wGrupo('/idDocAntPap');
      end;

      if CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntPap.Count > 990 then
        Gerador.wAlerta('#352', 'idDocAntPap', DSC_IDDOCANTPAP, ERR_MSG_MAIOR_MAXIMO + '990');

      for i02 := 0 to CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntEle.Count - 1 do
      begin
        Gerador.wGrupo('idDocAntEle', '#358');

        if (VersaoDF >= ve300) then
        begin
          Gerador.wCampo(tcStr, '#359', 'chCTe', 44, 44, 1, OnlyNumber(CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntEle[i02].chCTe ), DSC_CHAVE);

          if OnlyNumber(CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntEle[i02].chCTe) <> '' then
            if not ValidarChave(CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntEle[i02].chCTe) then
              Gerador.wAlerta('#359', 'chCTe', DSC_REFCTE, ERR_MSG_INVALIDO);
        end
        else
        begin
          Gerador.wCampo(tcStr, '#359', 'chave', 44, 44, 1, OnlyNumber(CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntEle[i02].chave), DSC_CHAVE);

          if OnlyNumber(CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntEle[i02].chave) <> '' then
            if not ValidarChave(CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntEle[i02].chave) then
              Gerador.wAlerta('#359', 'chave', DSC_REFCTE, ERR_MSG_INVALIDO);
        end;

        Gerador.wGrupo('/idDocAntEle');
      end;

      if CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[i01].idDocAntEle.Count > 990 then
        Gerador.wAlerta('#358', 'idDocAntEle', DSC_IDDOCANTELE, ERR_MSG_MAIOR_MAXIMO + '990');

      Gerador.wGrupo('/idDocAnt');
    end;

    if CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt.Count > 2 then
      Gerador.wAlerta('#351', 'idDocAnt', DSC_IDDOCANT, ERR_MSG_MAIOR_MAXIMO + '02');

    Gerador.wGrupo('/emiDocAnt');
  end;

  if CTe.infCTeNorm.docAnt.emiDocAnt.Count > 990 then
    Gerador.wAlerta('#345', 'emiDocAnt', DSC_EMIDOCANT, ERR_MSG_MAIOR_MAXIMO + '990');

  Gerador.wGrupo('/docAnt');
end;

procedure TCTeW.GerarInfSeg;
var
  i: Integer;
begin
  for i := 0 to CTe.infCTeNorm.seg.Count - 1 do
  begin
    Gerador.wGrupo('seg', '#360');
    Gerador.wCampo(tcStr, '#361', 'respSeg', 01, 01, 1, TpRspSeguroToStr(CTe.infCTeNorm.seg[i].respSeg), DSC_RESPSEG);
    Gerador.wCampo(tcStr, '#362', 'xSeg   ', 01, 30, 0, CTe.infCTeNorm.seg[i].xSeg, DSC_XSEG);
    Gerador.wCampo(tcStr, '#363', 'nApol  ', 01, 20, 0, CTe.infCTeNorm.seg[i].nApol, DSC_NAPOL);

    if ModeloDF = moCTe then
    begin
      Gerador.wCampo(tcStr, '#364', 'nAver ', 01, 20, 0, CTe.infCTeNorm.seg[i].nAver, DSC_NAVER);
      Gerador.wCampo(tcDe2, '#365', 'vCarga', 01, 15, 0, CTe.infCTeNorm.seg[i].vCarga, DSC_VMERC);
    end;

    Gerador.wGrupo('/seg');
  end;

  if CTe.infCTeNorm.seg.Count > 990 then
    Gerador.wAlerta('#360', 'seg', DSC_INFSEG, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarRodo;
begin
  Gerador.wGrupo('rodo', '#01');

  if CTe.infCTeNorm.rodo.RNTRC = 'ISENTO' then
    Gerador.wCampo(tcStr, '#02', 'RNTRC', 06, 06, 1, CTe.infCTeNorm.rodo.RNTRC, DSC_RNTRC)
  else
    Gerador.wCampo(tcStr, '#02', 'RNTRC', 08, 08, 1, OnlyNumber(CTe.infCTeNorm.rodo.RNTRC), DSC_RNTRC);

  if VersaoDF < ve300 then
  begin
    Gerador.wCampo(tcDat, '#03', 'dPrev', 10, 10, 1, CTe.infCTeNorm.rodo.dPrev, DSC_DPREV);
    Gerador.wCampo(tcStr, '#04', 'lota ', 01, 01, 1, TpLotacaoToStr(CTe.infCTeNorm.rodo.Lota), DSC_LOTA);
    Gerador.wCampo(tcStr, '#05', 'CIOT ', 12, 12, 0, CTe.infCTeNorm.rodo.CIOT, DSC_CIOT);

    GerarOCC;
    GerarValePed;

    if CTe.infCTeNorm.rodo.Lota = ltSim then
      GerarVeic;

    GerarLacre;

    if CTe.infCTeNorm.rodo.Lota = ltSim then
      GerarMoto;
  end
  else
    GerarOCC;

  Gerador.wGrupo('/rodo');
end;

procedure TCTeW.GerarOCC;
var
  i: Integer;
begin
  for i := 0 to CTe.infCTeNorm.rodo.occ.Count - 1 do
  begin
    Gerador.wGrupo('occ', '#06');
    Gerador.wCampo(tcStr, '#07', 'serie', 01, 03, 0, CTe.infCTeNorm.rodo.occ[i].serie, DSC_SERIE);
    Gerador.wCampo(tcInt, '#08', 'nOcc ', 01, 06, 1, CTe.infCTeNorm.rodo.occ[i].nOcc, DSC_NOCC);
    Gerador.wCampo(tcDat, '#09', 'dEmi ', 10, 10, 1, CTe.infCTeNorm.rodo.occ[i].dEmi, DSC_DEMI);

    Gerador.wGrupo('emiOcc', '#10');
    Gerador.wCampoCNPJ('#11', CTe.infCTeNorm.rodo.occ[i].emiOcc.CNPJ, CODIGO_BRASIL, True);
    Gerador.wCampo(tcStr, '#12', 'cInt', 01, 10, 0, CTe.infCTeNorm.rodo.occ[i].emiOcc.cInt, DSC_CINT);
    Gerador.wCampo(tcStr, '#13', 'IE  ', 02, 14, 1, OnlyNumber(CTe.infCTeNorm.rodo.occ[i].emiOcc.IE), DSC_IE);

    if (FOpcoes.ValidarInscricoes) then
      if not ValidarIE(CTe.infCTeNorm.rodo.occ[i].emiOcc.IE, CTe.infCTeNorm.rodo.occ[i].emiOcc.UF) then
        Gerador.wAlerta('#13', 'IE', DSC_IE, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#14', 'UF', 02, 02, 1, CTe.infCTeNorm.rodo.occ[i].emiOcc.UF, DSC_CUF);

    if not ValidarUF(CTe.infCTeNorm.rodo.occ[i].emiOcc.UF) then
      Gerador.wAlerta('#14', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#15', 'fone', 07, 12, 0, OnlyNumber(CTe.infCTeNorm.rodo.occ[i].emiOcc.fone), DSC_FONE);
    Gerador.wGrupo('/emiOcc');

    Gerador.wGrupo('/occ');
  end;

  if CTe.infCTeNorm.rodo.occ.Count > 10 then
    Gerador.wAlerta('#06', 'occ', DSC_OCC, ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TCTeW.GerarValePed;
var
  i: Integer;
begin
  for i := 0 to CTe.infCTeNorm.rodo.valePed.Count - 1 do
  begin
    Gerador.wGrupo('valePed', '#16');
    Gerador.wCampo(tcStr, '#17', 'CNPJForn', 14, 14, 1, CTe.infCTeNorm.rodo.valePed[i].CNPJForn, DSC_CNPJ);
    Gerador.wCampo(tcStr, '#18', 'nCompra ', 01, 20, 1, CTe.infCTeNorm.rodo.valePed[i].nCompra, DSC_NCOMPRA);
    Gerador.wCampo(tcStr, '#19', 'CNPJPg  ', 14, 14, 0, CTe.infCTeNorm.rodo.valePed[i].CNPJPg, DSC_CNPJ);
    Gerador.wCampo(tcDe2, '#20', 'vValePed', 01, 15, 1, CTe.infCTeNorm.rodo.valePed[i].vValePed, DSC_VVALEPED);
    Gerador.wGrupo('/valePed');
  end;

  if CTe.infCTeNorm.rodo.valePed.Count > 990 then
    Gerador.wAlerta('#16', 'valePed', DSC_VVALEPED, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarVeic;
var
  i: Integer;
begin
  for i := 0 to CTe.infCTeNorm.rodo.veic.Count - 1 do
  begin
    Gerador.wGrupo('veic', '#21');
    Gerador.wCampo(tcStr, '#22', 'cInt   ', 01, 10, 0, CTe.infCTeNorm.rodo.veic[i].cInt, DSC_CINTV);
    Gerador.wCampo(tcStr, '#23', 'RENAVAM', 09, 11, 1, CTe.infCTeNorm.rodo.veic[i].RENAVAM, DSC_RENAVAM);
    Gerador.wCampo(tcStr, '#24', 'placa  ', 01, 07, 1, CTe.infCTeNorm.rodo.veic[i].placa, DSC_PLACA);
    Gerador.wCampo(tcInt, '#25', 'tara   ', 01, 06, 1, CTe.infCTeNorm.rodo.veic[i].tara, DSC_TARA);
    Gerador.wCampo(tcInt, '#26', 'capKG  ', 01, 06, 1, CTe.infCTeNorm.rodo.veic[i].capKG, DSC_CAPKG);
    Gerador.wCampo(tcInt, '#27', 'capM3  ', 01, 03, 1, CTe.infCTeNorm.rodo.veic[i].capM3, DSC_CAPM3);
    Gerador.wCampo(tcStr, '#28', 'tpProp ', 01, 01, 1, TpPropriedadeToStr(CTe.infCTeNorm.rodo.veic[i].tpProp), DSC_TPPROP);
    Gerador.wCampo(tcStr, '#29', 'tpVeic ', 01, 01, 1, TpVeiculoToStr(CTe.infCTeNorm.rodo.veic[i].tpVeic), DSC_TPVEIC);
    Gerador.wCampo(tcStr, '#30', 'tpRod  ', 02, 02, 1, TpRodadoToStr(CTe.infCTeNorm.rodo.veic[i].tpRod), DSC_TPROD);
    Gerador.wCampo(tcStr, '#31', 'tpCar  ', 02, 02, 1, TpCarroceriaToStr(CTe.infCTeNorm.rodo.veic[i].tpCar), DSC_TPCAR);
    Gerador.wCampo(tcStr, '#32', 'UF     ', 02, 02, 1, CTe.infCTeNorm.rodo.veic[i].UF, DSC_CUF);

    if not ValidarUF(CTe.infCTeNorm.rodo.veic[i].UF) then
      Gerador.wAlerta('#32', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    if (trim(CTe.infCTeNorm.rodo.veic[i].prop.CNPJCPF) <> '') or
       (trim(CTe.infCTeNorm.rodo.veic[i].prop.RNTRC) <> '') or
       (trim(CTe.infCTeNorm.rodo.veic[i].prop.xNome) <> '') then
    begin
      Gerador.wGrupo('prop', '#33');
      Gerador.wCampoCNPJCPF('#34', '#35', CTe.infCTeNorm.rodo.veic[i].prop.CNPJCPF);

      if CTe.infCTeNorm.rodo.veic[i].prop.RNTRC = 'ISENTO' then
        Gerador.wCampo(tcStr, '#36', 'RNTRC', 06, 06, 1, CTe.infCTeNorm.rodo.veic[i].prop.RNTRC, DSC_RNTRC)
      else
        Gerador.wCampo(tcStr, '#36', 'RNTRC', 08, 08, 1, OnlyNumber(CTe.infCTeNorm.rodo.veic[i].prop.RNTRC), DSC_RNTRC);

      Gerador.wCampo(tcStr, '#37', 'xNome', 02, 60, 1, CTe.infCTeNorm.rodo.veic[i].prop.xNome, DSC_XNOME);

      if trim(CTe.infCTeNorm.rodo.veic[i].prop.IE) <> '' then
      begin
        if CTe.infCTeNorm.rodo.veic[i].prop.IE = 'ISENTO' then
          Gerador.wCampo(tcStr, '#38', 'IE', 00, 14, 1, CTe.infCTeNorm.rodo.veic[i].prop.IE, DSC_IE)
        else
          Gerador.wCampo(tcStr, '#38', 'IE', 02, 14, 1, OnlyNumber(CTe.infCTeNorm.rodo.veic[i].prop.IE), DSC_IE);

        if (FOpcoes.ValidarInscricoes) then
          if not ValidarIE(CTe.infCTeNorm.rodo.veic[i].prop.IE, CTe.infCTeNorm.rodo.veic[i].prop.UF) then
            Gerador.wAlerta('#38', 'IE', DSC_IE, ERR_MSG_INVALIDO);
      end;

      Gerador.wCampo(tcStr, '#39', 'UF', 02, 02, 1, CTe.infCTeNorm.rodo.veic[i].prop.UF, DSC_CUF);

      if not ValidarUF(CTe.infCTeNorm.rodo.veic[i].prop.UF) then
        Gerador.wAlerta('#39', 'UF', DSC_UF, ERR_MSG_INVALIDO);

      Gerador.wCampo(tcStr, '#40', 'tpProp', 01, 01, 1, TpPropToStr(CTe.infCTeNorm.rodo.veic[i].prop.tpProp), DSC_TPPROP);
      Gerador.wGrupo('/prop');
    end;

    Gerador.wGrupo('/veic');
  end;

  if CTe.infCTeNorm.rodo.veic.Count > 4 then
    Gerador.wAlerta('#21', 'veic', DSC_VEIC, ERR_MSG_MAIOR_MAXIMO + '4');
end;

procedure TCTeW.GerarLacre;
var
  i: Integer;
begin
  for i := 0 to CTe.infCTeNorm.rodo.lacRodo.Count - 1 do
  begin
    Gerador.wGrupo('lacRodo', '#41');
    Gerador.wCampo(tcStr, '#42', 'nLacre', 01, 20, 1, CTe.infCTeNorm.rodo.lacRodo[i].nLacre, DSC_NLACRE);
    Gerador.wGrupo('/lacRodo');
  end;                                                                  

  if CTe.infCTeNorm.rodo.lacRodo.Count > 990 then
    Gerador.wAlerta('#41', 'lacRodo', DSC_LACR, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarMoto;
var
  i: Integer;
begin
  for i := 0 to CTe.infCTeNorm.rodo.moto.Count - 1 do
  begin
    Gerador.wGrupo('moto', '#43');
    Gerador.wCampo(tcStr, '#44', 'xNome', 02, 60, 1, CTe.infCTeNorm.rodo.moto[i].xNome, DSC_XNOME);
    Gerador.wCampo(tcStr, '#45', 'CPF  ', 11, 11, 1, CTe.infCTeNorm.rodo.moto[i].CPF, DSC_CPF);
    Gerador.wGrupo('/moto');
  end;

  if CTe.infCTeNorm.rodo.moto.Count > 990 then
    Gerador.wAlerta('#43', 'moto', DSC_LACR, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarRodoOS;
begin
  Gerador.wGrupo('rodoOS', '#01');

  if CTe.infCTeNorm.rodoOS.TAF <> '' then
    Gerador.wCampo(tcStr, '#02', 'TAF           ', 12, 12, 1, CTe.infCTeNorm.rodoOS.TAF, DSC_TAF)
  else
    Gerador.wCampo(tcStr, '#03', 'NroRegEstadual', 25, 25, 1, CTe.infCTeNorm.rodoOS.NroRegEstadual, DSC_NROREGESTADUAL);

  if Trim(CTe.infCTeNorm.rodoOS.veic.placa) <> '' then
  begin
    Gerador.wGrupo('veic', '#04');
    Gerador.wCampo(tcStr, '#05', 'placa  ', 01, 07, 1, CTe.infCTeNorm.rodoOS.veic.placa, DSC_PLACA);
    Gerador.wCampo(tcStr, '#06', 'RENAVAM', 09, 11, 0, CTe.infCTeNorm.rodoOS.veic.RENAVAM, DSC_RENAVAM);

    if (trim(CTe.infCTeNorm.rodoOS.veic.prop.CNPJCPF) <> '') or
       (trim(CTe.infCTeNorm.rodoOS.veic.prop.xNome) <> '') then
    begin
      Gerador.wGrupo('prop', '#07');
      Gerador.wCampoCNPJCPF('#08', '#09', CTe.infCTeNorm.rodoOS.veic.prop.CNPJCPF);

      if CTe.infCTeNorm.rodoOS.veic.prop.TAF <> '' then
        Gerador.wCampo(tcStr, '#10', 'TAF           ', 12, 12, 1, CTe.infCTeNorm.rodoOS.veic.prop.TAF, DSC_TAF)
      else
        Gerador.wCampo(tcStr, '#11', 'NroRegEstadual', 25, 25, 1, CTe.infCTeNorm.rodoOS.veic.prop.NroRegEstadual, DSC_NROREGESTADUAL);

      Gerador.wCampo(tcStr, '#12', 'xNome', 02, 60, 1, CTe.infCTeNorm.rodoOS.veic.prop.xNome, DSC_XNOME);

      if trim(CTe.infCTeNorm.rodoOS.veic.prop.IE) <> '' then
      begin
        if CTe.infCTeNorm.rodoOS.veic.prop.IE = 'ISENTO' then
          Gerador.wCampo(tcStr, '#13', 'IE', 00, 14, 1, CTe.infCTeNorm.rodoOS.veic.prop.IE, DSC_IE)
        else
          Gerador.wCampo(tcStr, '#13', 'IE', 02, 14, 1, OnlyNumber(CTe.infCTeNorm.rodoOS.veic.prop.IE), DSC_IE);

        if (FOpcoes.ValidarInscricoes) then
          if not ValidarIE(CTe.infCTeNorm.rodoOS.veic.prop.IE, CTe.infCTeNorm.rodoOS.veic.prop.UF) then
            Gerador.wAlerta('#13', 'IE', DSC_IE, ERR_MSG_INVALIDO);
      end;

      Gerador.wCampo(tcStr, '#14', 'UF', 02, 02, 1, CTe.infCTeNorm.rodoOS.veic.prop.UF, DSC_CUF);

      if not ValidarUF(CTe.infCTeNorm.rodoOS.veic.prop.UF) then
        Gerador.wAlerta('#14', 'UF', DSC_UF, ERR_MSG_INVALIDO);

      Gerador.wCampo(tcStr, '#15', 'tpProp', 01, 01, 1, TpPropToStr(CTe.infCTeNorm.rodoOS.veic.prop.tpProp), DSC_TPPROP);
      Gerador.wGrupo('/prop');
    end;

    Gerador.wCampo(tcStr, '#16', 'UF', 02, 02, 0, CTe.infCTeNorm.rodoOS.veic.UF, DSC_CUF);

    if CTe.infCTeNorm.rodoOS.veic.UF <> '' then
      if not ValidarUF(CTe.infCTeNorm.rodoOS.veic.UF) then
        Gerador.wAlerta('#16', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Gerador.wGrupo('/veic');
  end;

  with CTe.infCTeNorm.rodoOS.infFretamento do
  begin
    if (CTe.Ide.tpServ = tsTranspPessoas) and (tpFretamento <> tfNenhum) then
    begin
      Gerador.wGrupo('infFretamento', '#17');
      Gerador.wCampo(tcStr, '#18', 'tpFretamento', 01, 01, 1, TpFretamentoToStr(tpFretamento), DSC_TPFRETAMENTO);

      if tpFretamento = tfEventual then
        Gerador.wCampo(tcStr, '#19', 'dhViagem', 25, 25, 0, DateTimeWithTimeZone(dhViagem, CTe.ide.cUF), DSC_DHVIAGEM);

      Gerador.wGrupo('/infFretamento');
    end;
  end;

  Gerador.wGrupo('/rodoOS');
end;

procedure TCTeW.GerarAereo;
var
  i: Integer;
begin
  Gerador.wGrupo('aereo', '#01');
  Gerador.wCampo(tcInt, '#02', 'nMinu     ', 09, 09, 0, CTe.infCTeNorm.aereo.nMinu, DSC_NMINU);
  Gerador.wCampo(tcStr, '#03', 'nOCA      ', 11, 11, 0, CTe.infCTeNorm.aereo.nOCA, DSC_NOCA);
  Gerador.wCampo(tcDat, '#04', 'dPrevAereo', 10, 10, 0, CTe.infCTeNorm.aereo.dPrevAereo, DSC_DPREV);

  if VersaoDF = ve200 then
  begin
    Gerador.wCampo(tcStr, '#05', 'xLAgEmi', 01, 20, 0, CTe.infCTeNorm.aereo.xLAgEmi, DSC_XLAGEMI);
    Gerador.wCampo(tcStr, '#06', 'IdT    ', 01, 14, 0, CTe.infCTeNorm.aereo.IdT, DSC_IDT);
  end;

  if VersaoDF >= ve300 then
  begin
    if (trim(CTe.infCTeNorm.aereo.natCarga.xDime) <> '') or
       (CTe.infCTeNorm.aereo.natCarga.cinfManu.Count <> 0) then
    begin
      Gerador.wGrupo('natCarga', '#11');
      Gerador.wCampo(tcStr, '#12', 'xDime', 05, 14, 0, CTe.infCTeNorm.aereo.natCarga.xDime, DSC_XDIME);

      for i := 0 to CTe.infCTeNorm.aereo.natCarga.cinfManu.Count - 1 do
        Gerador.wCampo(tcInt, '#13', 'cInfManu', 02, 02, 0, TpInfManuToStr(CTe.infCTeNorm.aereo.natCarga.cinfManu.Items[i].nInfManu), DSC_CINFMANU);

      Gerador.wGrupo('/natCarga');
    end;

    Gerador.wGrupo('tarifa', '#07');
    Gerador.wCampo(tcStr, '#08', 'CL  ', 01, 02, 1, CTe.infCTeNorm.aereo.tarifa.CL, DSC_CL);
    Gerador.wCampo(tcStr, '#09', 'cTar', 01, 04, 0, CTe.infCTeNorm.aereo.tarifa.cTar, DSC_CTAR);
    Gerador.wCampo(tcDe2, '#10', 'vTar', 01, 15, 1, CTe.infCTeNorm.aereo.tarifa.vTar, DSC_VTAR);
    Gerador.wGrupo('/tarifa');

    GerarPeri;
  end
  else
  begin
    Gerador.wGrupo('tarifa', '#07');
    Gerador.wCampo(tcStr, '#08', 'CL  ', 01, 02, 1, CTe.infCTeNorm.aereo.tarifa.CL, DSC_CL);
    Gerador.wCampo(tcStr, '#09', 'cTar', 01, 04, 0, CTe.infCTeNorm.aereo.tarifa.cTar, DSC_CTAR);
    Gerador.wCampo(tcDe2, '#10', 'vTar', 01, 15, 1, CTe.infCTeNorm.aereo.tarifa.vTar, DSC_VTAR);
    Gerador.wGrupo('/tarifa');

    if (trim(CTe.infCTeNorm.aereo.natCarga.xDime) <> '') or
       (CTe.infCTeNorm.aereo.natCarga.cinfManu.Count <> 0) or
       (trim(CTe.infCTeNorm.aereo.natCarga.cImp) <> '') then
    begin
      Gerador.wGrupo('natCarga', '#11');
      Gerador.wCampo(tcStr, '#12', 'xDime', 05, 14, 0, CTe.infCTeNorm.aereo.natCarga.xDime, DSC_XDIME);

      for i := 0 to CTe.infCTeNorm.aereo.natCarga.cinfManu.Count - 1 do
        Gerador.wCampo(tcInt, '#13', 'cInfManu', 01, 02, 0, TpInfManuToStrV2(CTe.infCTeNorm.aereo.natCarga.cinfManu.Items[i].nInfManu), DSC_CINFMANU);

      Gerador.wCampo(tcStr, '#14', 'cIMP', 03, 03, 1, CTe.infCTeNorm.aereo.natCarga.cIMP, DSC_CIMP);
      Gerador.wGrupo('/natCarga');
    end;
  end;

  Gerador.wGrupo('/aereo');
end;

procedure TCTeW.GerarAquav;
var
  i, j: Integer;
begin
  Gerador.wGrupo('aquav', '#01');
  Gerador.wCampo(tcDe2, '#02', 'vPrest', 01, 15, 1, CTe.infCTeNorm.aquav.vPrest, DSC_VPREST);
  Gerador.wCampo(tcDe2, '#03', 'vAFRMM', 01, 15, 1, CTe.infCTeNorm.aquav.vAFRMM, DSC_VAFRMM);

  if (VersaoDF = ve200) then
  begin
    Gerador.wCampo(tcStr, '#04', 'nBooking', 01, 10, 0, CTe.infCTeNorm.aquav.nBooking, DSC_NBOOKING);
    Gerador.wCampo(tcStr, '#05', 'nCtrl   ', 01, 10, 0, CTe.infCTeNorm.aquav.nCtrl, DSC_NCTRL);
  end;

  Gerador.wCampo(tcStr, '#06', 'xNavio', 01, 60, 1, CTe.infCTeNorm.aquav.xNavio, DSC_XNAVIO);

  for i := 0 to CTe.infCTeNorm.aquav.balsa.Count - 1 do
  begin
    Gerador.wGrupo('balsa', '#07');
    Gerador.wCampo(tcStr, '#08', 'xBalsa', 01, 60, 1, CTe.infCTeNorm.aquav.balsa.Items[i].xBalsa, DSC_XBALSA);
    Gerador.wGrupo('/balsa');
  end;

  if CTe.infCTeNorm.aquav.balsa.Count > 3 then
    Gerador.wAlerta('#07', 'balsa', DSC_XBALSA, ERR_MSG_MAIOR_MAXIMO + '3');

  Gerador.wCampo(tcStr, '#09', 'nViag', 01, 10, 0, CTe.infCTeNorm.aquav.nViag, DSC_NVIAG);
  Gerador.wCampo(tcStr, '#10', 'direc', 01, 01, 1, TpDirecaoToStr(CTe.infCTeNorm.aquav.direc), DSC_DIREC);

  if (VersaoDF = ve200) then
  begin
    Gerador.wCampo(tcStr, '#11', 'prtEmb  ', 01, 60, 0, CTe.infCTeNorm.aquav.prtEmb, DSC_PRTEMB);
    Gerador.wCampo(tcStr, '#12', 'prtTrans', 01, 60, 0, CTe.infCTeNorm.aquav.prtTrans, DSC_PRTTRANS);
    Gerador.wCampo(tcStr, '#13', 'prtDest ', 01, 60, 0, CTe.infCTeNorm.aquav.prtDest, DSC_PRTDEST);
    Gerador.wCampo(tcStr, '#14', 'tpNav   ', 01, 01, 1, TpNavegacaoToStr(CTe.infCTeNorm.aquav.tpNav), DSC_TPNAV);
  end;

  Gerador.wCampo(tcStr, '#15', 'irin', 01, 10, 1, CTe.infCTeNorm.aquav.irin, DSC_IRIN);

  for i := 0 to CTe.infCTeNorm.aquav.detCont.Count - 1 do
  begin
    Gerador.wGrupo('detCont', '#16');
    Gerador.wCampo(tcStr, '#17', 'nCont', 01, 20, 1, CTe.infCTeNorm.aquav.detCont.Items[i].nCont, DSC_NCONT);

    for j := 0 to CTe.infCTeNorm.aquav.detCont.Items[i].Lacre.Count - 1 do
    begin
      Gerador.wGrupo('lacre', '#18');
      Gerador.wCampo(tcStr, '#19', 'nLacre', 01, 20, 1, CTe.infCTeNorm.aquav.detCont.Items[i].Lacre.Items[j].nLacre, DSC_NLACRE);
      Gerador.wGrupo('/lacre');
    end;

    if CTe.infCTeNorm.aquav.detCont.Items[i].Lacre.Count > 3 then
      Gerador.wAlerta('#18', 'lacre', DSC_NLACRE, ERR_MSG_MAIOR_MAXIMO + '3');

    if (CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNF.Count > 0) or
       (CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNFe.Count > 0) then
    begin
      Gerador.wGrupo('infDoc', '#20');

      for j := 0 to CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNF.Count - 1 do
      begin
        Gerador.wGrupo('infNF', '#21');
        Gerador.wCampo(tcStr, '#22', 'serie  ', 01, 03, 1, CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNF.Items[j].serie, DSC_SERIE);
        Gerador.wCampo(tcEsp, '#23', 'nDoc   ', 01, 20, 1, OnlyNumber(CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNF.Items[j].nDoc), DSC_NDOC);
        Gerador.wCampo(tcDe2, '#24', 'unidRat', 01, 05, 0, CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNF.Items[j].unidRat, DSC_QTDRAT);
        Gerador.wGrupo('/infNF');
      end;

      if CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNF.Count > 999 then
        Gerador.wAlerta('#21', 'infNF', DSC_INFNF, ERR_MSG_MAIOR_MAXIMO + '999');

      for j := 0 to CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNFe.Count - 1 do
      begin
        Gerador.wGrupo('infNFe', '#25');
        Gerador.wCampo(tcStr, '#26', 'chave', 44, 44, 1, OnlyNumber(CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNFe.Items[j].chave), DSC_REFNFE);

        if OnlyNumber(CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNFe.Items[j].chave) <> '' then
          if not ValidarChave(CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNFe.Items[j].chave) then
            Gerador.wAlerta('#26', 'chave', DSC_REFNFE, ERR_MSG_INVALIDO);

        Gerador.wCampo(tcDe2, '#27', 'unidRat', 01, 05, 0, CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNFe.Items[j].unidRat, DSC_QTDRAT);
        Gerador.wGrupo('/infNFe');
      end;

      if CTe.infCTeNorm.aquav.detCont.Items[i].infDoc.infNFe.Count > 999 then
        Gerador.wAlerta('#25', 'infNFe', DSC_INFNF, ERR_MSG_MAIOR_MAXIMO + '999');

      Gerador.wGrupo('/infDoc');
    end;

    Gerador.wGrupo('/detCont');
  end;

  if CTe.infCTeNorm.aquav.detCont.Count > 999 then
    Gerador.wAlerta('#16', 'detCont', DSC_DETCONT, ERR_MSG_MAIOR_MAXIMO + '999');

  if VersaoDF >= ve300 then
    Gerador.wCampo(tcStr, '#28', 'tpNav', 01, 01, 0, TpNavegacaoToStr(CTe.infCTeNorm.aquav.tpNav), DSC_TPNAV);

  Gerador.wGrupo('/aquav');
end;

procedure TCTeW.GerarFerrov;
begin
  Gerador.wGrupo('ferrov', '#01');
  Gerador.wCampo(tcStr, '#02', 'tpTraf', 01, 01, 1, TpTrafegoToStr(CTe.infCTeNorm.ferrov.tpTraf), DSC_TPTRAF);

  if VersaoDF >= ve300 then
  begin
    if CTe.infCTeNorm.ferrov.tpTraf = ttMutuo then
    begin
      Gerador.wGrupo('trafMut', '#03');
      Gerador.wCampo(tcStr, '#04', 'respFat', 01, 01, 1, TrafegoMutuoToStr(CTe.infCTeNorm.ferrov.trafMut.respFat), DSC_RESPFAT);
      Gerador.wCampo(tcStr, '#05', 'ferrEmi', 01, 01, 1, TrafegoMutuoToStr(CTe.infCTeNorm.ferrov.trafMut.ferrEmi), DSC_FERREMI);
      Gerador.wCampo(tcDe2, '#06', 'vFrete ', 01, 15, 1, CTe.infCTeNorm.ferrov.vFrete, DSC_VFRETE);

      Gerador.wCampo(tcStr, '#07', 'chCTeFerroOrigem', 44, 44, 0, CTe.infCTeNorm.ferrov.trafMut.chCTeFerroOrigem, DSC_CHAVE);

      if OnlyNumber(CTe.infCTeNorm.ferrov.trafMut.chCTeFerroOrigem) <> '' then
        if not ValidarChave(CTe.infCTeNorm.ferrov.trafMut.chCTeFerroOrigem) then
          Gerador.wAlerta('#07', 'chCTeFerroOrigem', DSC_CHAVE, ERR_MSG_INVALIDO);

      GerarFerroEnv;

      Gerador.wGrupo('/trafMut');
    end;

    Gerador.wCampo(tcStr, '#22', 'fluxo ', 01, 10, 1, CTe.infCTeNorm.ferrov.fluxo, DSC_FLUXO);
  end
  else
  begin
    if CTe.infCTeNorm.ferrov.tpTraf = ttMutuo then
    begin
      Gerador.wGrupo('trafMut', '#03');
      Gerador.wCampo(tcStr, '#04', 'respFat', 01, 01, 1, TrafegoMutuoToStr(CTe.infCTeNorm.ferrov.trafMut.respFat), DSC_RESPFAT);
      Gerador.wCampo(tcStr, '#05', 'ferrEmi', 01, 01, 1, TrafegoMutuoToStr(CTe.infCTeNorm.ferrov.trafMut.ferrEmi), DSC_FERREMI);
      Gerador.wGrupo('/trafMut');
    end;

    Gerador.wCampo(tcStr, '#22', 'fluxo ', 01, 10, 1, CTe.infCTeNorm.ferrov.fluxo, DSC_FLUXO);
    Gerador.wCampo(tcStr, '#07', 'idTrem', 01, 07, 0, CTe.infCTeNorm.ferrov.idTrem, DSC_IDTREM);
    Gerador.wCampo(tcDe2, '#08', 'vFrete', 01, 15, 1, CTe.infCTeNorm.ferrov.vFrete, DSC_VFRETE);

    GerarFerroEnv;
    GerardetVag;
  end;

  Gerador.wGrupo('/ferrov');
end;

procedure TCTeW.GerarFerroEnv;
var
  i, cMun: Integer;
  xMun, xUF: String;
begin
  for i := 0 to CTe.infCTeNorm.ferrov.ferroEnv.Count - 1 do
  begin
    if (trim(CTe.infCTeNorm.ferrov.ferroEnv[i].CNPJ) <> '') or
       (trim(CTe.infCTeNorm.ferrov.ferroEnv[i].xNome) <> '') then
    begin
      Gerador.wGrupo('ferroEnv', '#09');
      Gerador.wCampoCNPJ('#10', CTe.infCTeNorm.ferrov.ferroEnv[i].CNPJ, CODIGO_BRASIL, True);
      Gerador.wCampo(tcStr, '#11', 'cInt', 01, 10, 0, CTe.infCTeNorm.ferrov.ferroEnv[i].cInt, DSC_CINTF);

      if trim(CTe.infCTeNorm.ferrov.ferroEnv[i].IE) <> '' then
      begin
        Gerador.wCampo(tcStr, '#12', 'IE', 02, 14, 1, OnlyNumber(CTe.infCTeNorm.ferrov.ferroEnv[i].IE), DSC_IE);

        if (FOpcoes.ValidarInscricoes) then
          if not ValidarIE(CTe.infCTeNorm.ferrov.ferroEnv[i].IE, CTe.infCTeNorm.ferrov.ferroEnv[i].enderFerro.UF) then
            Gerador.wAlerta('#12', 'IE', DSC_IE, ERR_MSG_INVALIDO);
      end;

      Gerador.wCampo(tcStr, '#13', 'xNome', 02, 60, 1, CTe.infCTeNorm.ferrov.ferroEnv[i].xNome, DSC_XNOME);

      AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL,
                         CTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.UF,
                         CTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.xMun,
                         CTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.cMun);

      Gerador.wGrupo('enderFerro', '#14');
      Gerador.wCampo(tcStr, '#15', 'xLgr   ', 02, 255, 1, CTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.xLgr, DSC_XLGR);
      Gerador.wCampo(tcStr, '#16', 'nro    ', 01, 060, 0, CTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.nro, DSC_NRO);
      Gerador.wCampo(tcStr, '#17', 'xCpl   ', 01, 060, 0, CTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.xCpl, DSC_XCPL);
      Gerador.wCampo(tcStr, '#18', 'xBairro', 02, 060, 0, CTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.xBairro, DSC_XBAIRRO);
      Gerador.wCampo(tcInt, '#19', 'cMun   ', 07, 007, 1, cMun, DSC_CMUN);

      if not ValidarMunicipio(CTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.cMun) then
        Gerador.wAlerta('#19', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

      Gerador.wCampo(tcStr, '#20', 'xMun', 02, 60, 1, xMun, DSC_XMUN);
      Gerador.wCampo(tcInt, '#21', 'CEP ', 08, 08, 0, CTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.CEP, DSC_CEP);
      Gerador.wCampo(tcStr, '#22', 'UF  ', 02, 02, 1, xUF, DSC_UF);

      if not ValidarUF(xUF) then
        Gerador.wAlerta('#22', 'UF', DSC_UF, ERR_MSG_INVALIDO);

      Gerador.wGrupo('/enderFerro');
      Gerador.wGrupo('/ferroEnv');
    end;
  end;
end;

procedure TCTeW.GerardetVag;
var
  i: Integer;
begin
  for i := 0 to CTe.infCTeNorm.ferrov.detVag.Count - 1 do
  begin
    Gerador.wGrupo('detVag', '#23');
    Gerador.wCampo(tcInt, '#24', 'nVag  ', 08, 08, 1, CTe.infCTeNorm.ferrov.detVag.Items[i].nVag, DSC_VAGAO);
    Gerador.wCampo(tcDe2, '#25', 'cap   ', 01, 05, 0, CTe.infCTeNorm.ferrov.detVag.Items[i].cap, DSC_CAPTO);
    Gerador.wCampo(tcStr, '#26', 'tpVag ', 03, 03, 0, CTe.infCTeNorm.ferrov.detVag.Items[i].tpVag, DSC_TPVAG);
    Gerador.wCampo(tcDe2, '#27', 'pesoR ', 01, 05, 1, CTe.infCTeNorm.ferrov.detVag.Items[i].pesoR, DSC_PESOR);
    Gerador.wCampo(tcDe2, '#28', 'pesoBC', 01, 05, 1, CTe.infCTeNorm.ferrov.detVag.Items[i].pesoBC, DSC_PESOBC);
    Gerador.wGrupo('/detVag');
  end;

  if CTe.infCTeNorm.ferrov.detVag.Count > 990 then
    Gerador.wAlerta('#23', 'detVag', DSC_VAGAO, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarDuto;
begin
  Gerador.wGrupo('duto', '#01');
  Gerador.wCampo(tcDe6, '#02', 'vTar', 01, 15, 0, CTe.infCTeNorm.duto.vTar, DSC_VTAR);
  Gerador.wCampo(tcDat, '#03', 'dIni', 10, 10, 1, CTe.infCTeNorm.duto.dIni, DSC_DINI);
  Gerador.wCampo(tcDat, '#04', 'dFim', 10, 10, 1, CTe.infCTeNorm.duto.dFim, DSC_DFIM);
  Gerador.wGrupo('/duto');
end;

procedure TCTeW.GerarMultimodal;
begin
  Gerador.wGrupo('multimodal', '#01');
  Gerador.wCampo(tcStr, '#02', 'COTM         ', 01, 255, 1, CTe.infCTeNorm.multimodal.COTM, DSC_COTM);
  Gerador.wCampo(tcStr, '#03', 'indNegociavel', 01, 001, 1, indNegociavelToStr(CTe.infCTeNorm.multimodal.indNegociavel), DSC_INDNEG);

  if VersaoDF >= ve300 then
  begin
    if (CTe.infCTeNorm.multimodal.xSeg <> '') then
    begin
      Gerador.wGrupo('seg', '#04');

      Gerador.wGrupo('infSeg', '#05');
      Gerador.wCampo(tcStr, '#06', 'xSeg', 01, 30, 0, CTe.infCTeNorm.multimodal.xSeg, DSC_XSEG);
      Gerador.wCampoCNPJ('#07', CTe.infCTeNorm.multimodal.CNPJ, CODIGO_BRASIL, True);
      Gerador.wGrupo('/infSeg');

      Gerador.wCampo(tcStr, '#08', 'nApol', 01, 20, 0, CTe.infCTeNorm.multimodal.nApol, DSC_NAPOL);
      Gerador.wCampo(tcStr, '#09', 'nAver', 01, 20, 0, CTe.infCTeNorm.multimodal.nAver, DSC_NAVER);

      Gerador.wGrupo('/seg');
    end;
  end;

  Gerador.wGrupo('/multimodal');
end;

procedure TCTeW.GerarPeri;
var
  i: Integer;
begin
  for i := 0 to CTe.infCTeNorm.peri.Count - 1 do
  begin
    if VersaoDF >= ve300 then
    begin
      Gerador.wGrupo('peri', '#12');
      Gerador.wCampo(tcStr, '#13', 'nONU   ', 01, 04, 1, CTe.infCTeNorm.peri.Items[i].nONU, DSC_NONU);
      Gerador.wCampo(tcStr, '#14', 'qTotEmb', 01, 20, 1, CTe.infCTeNorm.peri.Items[i].qTotEmb, DSC_QTOTEMB);

      Gerador.wGrupo('infTotAP', '#15');
      Gerador.wCampo(tcStr, '#16', 'qTotProd', 01, 20, 1, CTe.infCTeNorm.peri.Items[i].qTotProd, DSC_QTOTPROD);
      Gerador.wCampo(tcStr, '#17', 'uniAP   ', 01, 01, 1, UniMedToStr(CTe.infCTeNorm.peri.Items[i].uniAP), DSC_UNIAP);
      Gerador.wGrupo('/infTotAP');

      Gerador.wGrupo('/peri');
    end
    else
    begin
      Gerador.wGrupo('peri', '#369');
      Gerador.wCampo(tcStr, '#370', 'nONU       ', 01,  04, 1, CTe.infCTeNorm.peri.Items[i].nONU, DSC_NONU);
      Gerador.wCampo(tcStr, '#371', 'xNomeAE    ', 01, 150, 1, CTe.infCTeNorm.peri.Items[i].xNomeAE, DSC_XNOMEAE);
      Gerador.wCampo(tcStr, '#372', 'xClaRisco  ', 01,  40, 1, CTe.infCTeNorm.peri.Items[i].xClaRisco, DSC_XCLARISCO);
      Gerador.wCampo(tcStr, '#373', 'grEmb      ', 01,  06, 0, CTe.infCTeNorm.peri.Items[i].grEmb, DSC_GREMB);
      Gerador.wCampo(tcStr, '#374', 'qTotProd   ', 01,  20, 1, CTe.infCTeNorm.peri.Items[i].qTotProd, DSC_QTOTPROD);
      Gerador.wCampo(tcStr, '#375', 'qVolTipo   ', 01,  60, 0, CTe.infCTeNorm.peri.Items[i].qVolTipo, DSC_QVOLTIPO);
      Gerador.wCampo(tcStr, '#375', 'pontoFulgor', 01,  06, 0, CTe.infCTeNorm.peri.Items[i].pontoFulgor, DSC_PONTOFULGOR);
      Gerador.wGrupo('/peri');
    end;
  end;

  if CTe.infCTeNorm.peri.Count > 990 then
    Gerador.wAlerta('#369', 'peri', DSC_PERI, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarVeicNovos;
var
  i: Integer;
begin
  for i := 0 to CTe.infCTeNorm.veicNovos.Count - 1 do
  begin
    Gerador.wGrupo('veicNovos', '#377');
    Gerador.wCampo(tcStr, '#378', 'chassi', 17, 17, 1, CTe.infCTeNorm.veicNovos.Items[i].chassi, DSC_CHASSI);
    Gerador.wCampo(tcStr, '#379', 'cCor  ', 01, 04, 1, CTe.infCTeNorm.veicNovos.Items[i].cCor, DSC_CCOR);
    Gerador.wCampo(tcStr, '#380', 'xCor  ', 01, 40, 1, CTe.infCTeNorm.veicNovos.Items[i].xCor, DSC_XCOR);
    Gerador.wCampo(tcStr, '#381', 'cMod  ', 01, 06, 1, CTe.infCTeNorm.veicNovos.Items[i].cMod, DSC_CMOD);
    Gerador.wCampo(tcDe2, '#382', 'vUnit ', 01, 15, 1, CTe.infCTeNorm.veicNovos.Items[i].vUnit, DSC_VUNITV);
    Gerador.wCampo(tcDe2, '#383', 'vFrete', 01, 15, 1, CTe.infCTeNorm.veicNovos.Items[i].vFrete, DSC_VFRETEV);
    Gerador.wGrupo('/veicNovos');
  end;

  if CTe.infCTeNorm.veicNovos.Count > 990 then
    Gerador.wAlerta('#377', 'veicNovos', DSC_VEICNOVOS, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarCobr;
begin
  if (Trim(CTe.infCTeNorm.cobr.fat.nFat) <> '') or (CTe.infCTeNorm.cobr.fat.vOrig > 0) or
     (CTe.infCTeNorm.cobr.fat.vDesc > 0) or (CTe.infCTeNorm.cobr.fat.vLiq > 0) or
     (CTe.infCTeNorm.cobr.dup.Count > 0) then
  begin
    Gerador.wGrupo('cobr', '#384');
    GerarCobrFat;
    GerarCobrDup;
    Gerador.wGrupo('/cobr');
  end;
end;

procedure TCTeW.GerarCobrFat;
begin
  if (Trim(CTe.infCTeNorm.cobr.fat.nFat) <> '') or (CTe.infCTeNorm.cobr.fat.vOrig > 0) or
     (CTe.infCTeNorm.cobr.fat.vDesc > 0) or (CTe.infCTeNorm.cobr.fat.vLiq > 0) then
  begin
    Gerador.wGrupo('fat', '#385');
    Gerador.wCampo(tcStr, '#386', 'nFat ', 01, 60, 0, CTe.infCTeNorm.cobr.fat.nFat, DSC_NFAT);
    Gerador.wCampo(tcDe2, '#387', 'vOrig', 01, 15, 0, CTe.infCTeNorm.cobr.fat.vOrig, DSC_VORIG);
    Gerador.wCampo(tcDe2, '#388', 'vDesc', 01, 15, 0, CTe.infCTeNorm.cobr.fat.vDesc, DSC_VDESC);
    Gerador.wCampo(tcDe2, '#389', 'vLiq ', 01, 15, 0, CTe.infCTeNorm.cobr.fat.vLiq, DSC_VLIQ);
    Gerador.wGrupo('/fat');
  end;
end;

procedure TCTeW.GerarCobrDup;
var
  i: Integer;
begin
  for i := 0 to CTe.infCTeNorm.cobr.dup.Count - 1 do
  begin
    Gerador.wGrupo('dup', '#390');
    Gerador.wCampo(tcStr, '#391', 'nDup ', 01, 60, 0, CTe.infCTeNorm.cobr.dup[i].nDup, DSC_NDUP);
    Gerador.wCampo(tcDat, '#392', 'dVenc', 10, 10, 0, CTe.infCTeNorm.cobr.dup[i].dVenc, DSC_DVENC);
    Gerador.wCampo(tcDe2, '#393', 'vDup ', 01, 15, 0, CTe.infCTeNorm.cobr.dup[i].vDup, DSC_VDUP);
    Gerador.wGrupo('/dup');
  end;
end;

procedure TCTeW.GerarInfCTeSub;
begin
  if CTe.Ide.tpCTe = tcSubstituto then
  begin
    Gerador.wGrupo('infCteSub', '#394');
    Gerador.wCampo(tcEsp, '#395', 'chCte', 44, 44, 1, OnlyNumber(CTe.infCTeNorm.infCTeSub.chCte), DSC_CHAVE);

    if OnlyNumber(CTe.infCTeNorm.infCTeSub.chCte) <> '' then
      if not ValidarChave(CTe.infCTeNorm.infCTeSub.chCTe) then
        Gerador.wAlerta('#395', 'chCte', DSC_REFNFE, ERR_MSG_INVALIDO);

    if (VersaoDF <= ve300) then
    begin
      if ((VersaoDF = ve200) and (trim(CTe.infCTeNorm.infCTeSub.tomaNaoICMS.refCteAnu) = '')) or
         ((VersaoDF >= ve300) and (trim(CTe.infCTeNorm.infCTeSub.refCteAnu) = '')) then
      begin
        Gerador.wGrupo('tomaICMS', '#396');

        if (trim(CTe.infCTeNorm.infCTeSub.tomaICMS.refNFe) <> '') then
        begin
          Gerador.wCampo(tcEsp, '#397', 'refNFe', 44, 44, 1, OnlyNumber(CTe.infCTeNorm.infCTeSub.tomaICMS.refNFe), DSC_CHAVE);

          if OnlyNumber(CTe.infCTeNorm.infCTeSub.tomaICMS.refNFe) <> '' then
            if not ValidarChave(CTe.infCTeNorm.infCTeSub.tomaICMS.refNFe) then
              Gerador.wAlerta('#397', 'refNFe', DSC_REFNFE, ERR_MSG_INVALIDO);
        end
        else
        begin
          if (trim(CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.CNPJCPF) <> '') then
          begin
            Gerador.wGrupo('refNF', '#398');
            Gerador.wCampoCNPJCPF('#399', '#400', CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.CNPJCPF);
            Gerador.wCampo(tcStr, '#401', 'mod     ', 02, 02, 1, CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.modelo, DSC_MOD);
            Gerador.wCampo(tcInt, '#402', 'serie   ', 01, 03, 1, CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.serie, DSC_SERIE);
            Gerador.wCampo(tcInt, '#403', 'subserie', 01, 03, 0, CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.subserie, DSC_SERIE);
            Gerador.wCampo(tcInt, '#404', 'nro     ', 01, 06, 1, CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.nro, DSC_NDF);
            Gerador.wCampo(tcDe2, '#405', 'valor   ', 01, 15, 1, CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.valor, DSC_VDOC);
            Gerador.wCampo(tcDat, '#406', 'dEmi    ', 10, 10, 1, CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.dEmi, DSC_DEMI);
            Gerador.wGrupo('/refNF');
          end
          else
          begin
            Gerador.wCampo(tcEsp, '#407', 'refCte', 44, 44, 1, OnlyNumber(CTe.infCTeNorm.infCTeSub.tomaICMS.refCte), DSC_CHAVE);

            if OnlyNumber(CTe.infCTeNorm.infCTeSub.tomaICMS.refCte) <> '' then
              if not ValidarChave(CTe.infCTeNorm.infCTeSub.tomaICMS.refCTe) then
                Gerador.wAlerta('#407', 'refCte', DSC_REFNFE, ERR_MSG_INVALIDO);
          end;
        end;

        Gerador.wGrupo('/tomaICMS');
      end
      else
      begin
        if VersaoDF = ve200 then
        begin
          Gerador.wGrupo('tomaNaoICMS', '#408');
          Gerador.wCampo(tcEsp, '#409', 'refCteAnu', 44, 44, 1, OnlyNumber(CTe.infCTeNorm.infCTeSub.tomaNaoICMS.refCteAnu), DSC_CHAVE);

          if OnlyNumber(CTe.infCTeNorm.infCTeSub.tomaNaoICMS.refCteAnu) <> '' then
            if not ValidarChave(CTe.infCTeNorm.infCTeSub.tomaNaoICMS.refCteAnu) then
              Gerador.wAlerta('#409', 'refCteAnu', DSC_REFNFE, ERR_MSG_INVALIDO);

          Gerador.wGrupo('/tomaNaoICMS');
        end
        else
        begin
          Gerador.wCampo(tcEsp, '#372', 'refCteAnu', 44, 44, 1, OnlyNumber(CTe.infCTeNorm.infCTeSub.refCteAnu), DSC_CHAVE);

          if OnlyNumber(CTe.infCTeNorm.infCTeSub.refCteAnu) <> '' then
            if not ValidarChave(CTe.infCTeNorm.infCTeSub.refCteAnu) then
              Gerador.wAlerta('#372', 'refCteAnu', DSC_REFNFE, ERR_MSG_INVALIDO);
        end;
      end;
    end;

    if (VersaoDF >= ve300) and (CTe.infCTeNorm.infCteSub.indAlteraToma = tiSim) then
      Gerador.wCampo(tcStr, '#385', 'indAlteraToma', 01, 01, 0, TindicadorToStr(CTe.infCTeNorm.infCteSub.indAlteraToma), DSC_INDALTERATOMA);

    Gerador.wGrupo('/infCteSub');
  end;
end;

procedure TCTeW.GerarInfGlobalizado;
begin
  Gerador.wGrupo('infGlobalizado', '#386');
  Gerador.wCampo(tcStr, '#387', 'xObs', 15, 256, 1, CTe.infCTeNorm.infGlobalizado.xObs, DSC_XOBS);
  Gerador.wGrupo('/infGlobalizado');
end;

procedure TCTeW.GerarInfGTVe;
var
  i, j: Integer;
begin
  if CTe.infCTeNorm.infGTVe.Count > 0 then
  begin
    for i := 0 to CTe.infCTeNorm.infGTVe.Count - 1 do
    begin
      Gerador.wGrupo('infGTVe', '#');

      Gerador.wCampo(tcStr, '#', 'chCTe', 44, 44, 1, CTe.infCTeNorm.infGTVe[i].chCTe, DSC_CHAVE);

      for j := 0 to CTe.infCTeNorm.infGTVe[i].Comp.Count - 1 do
      begin
        Gerador.wGrupo('Comp', '#');
        Gerador.wCampo(tcStr, '#', 'tpComp', 1, 01, 1, tpCompToStr(CTe.infCTeNorm.infGTVe[i].Comp[j].tpComp), DSC_TPCOMP);
        Gerador.wCampo(tcDe2, '#', 'vComp ', 1, 15, 1, CTe.infCTeNorm.infGTVe[i].Comp[j].vComp, DSC_VCOMP);
        Gerador.wCampo(tcStr, '#', 'xComp ', 1, 15, 0, CTe.infCTeNorm.infGTVe[i].Comp[j].xComp, DSC_XCOMP);

        Gerador.wGrupo('/Comp');
      end;

      Gerador.wGrupo('/infGTVe');
    end;

    if CTe.infCTeNorm.infGTVe.Count > 999 then
      Gerador.wAlerta('#', 'infGTVe', DSC_INFGTVE, ERR_MSG_MAIOR_MAXIMO + '999');
  end;
end;

procedure TCTeW.GerarInfServVinc;
var
  i: Integer;
begin
  if CTe.infCTeNorm.infServVinc.infCTeMultimodal.Count > 0 then
  begin
    Gerador.wGrupo('infServVinc', '#388');

    for i := 0 to CTe.infCTeNorm.infServVinc.infCTeMultimodal.Count - 1 do
    begin
      Gerador.wGrupo('infCTeMultimodal', '#389');
      Gerador.wCampo(tcStr, '#390', 'chCTeMultimodal', 44, 44, 1, CTe.infCTeNorm.infServVinc.infCTeMultimodal.Items[i].chCTeMultimodal, DSC_CHCTEMULTIMODAL);
      Gerador.wGrupo('/infCTeMultimodal');
    end;

    if CTe.infCTeNorm.infServVinc.infCTeMultimodal.Count > 999 then
      Gerador.wAlerta('#389', 'infCTeMultimodal', DSC_INFCTEMULTIMODAL, ERR_MSG_MAIOR_MAXIMO + '999');

    Gerador.wGrupo('/infServVinc');
  end;
end;

procedure TCTeW.GerarInfCTeComp;
var
  i: Integer;
  chave: string;
begin
  if (CTe.Ide.tpCTe = tcComplemento) then
  begin
    if VersaoDF <= ve300 then
    begin
      Gerador.wGrupo('infCteComp', '#410');

      chave := OnlyNumber(CTe.infCTeComp.Chave);

      if VersaoDF = ve200 then
        Gerador.wCampo(tcEsp, '#411', 'chave', 44, 44, 1, chave, DSC_CHAVE)
      else
        Gerador.wCampo(tcEsp, '#411', 'chCTe', 44, 44, 1, chave, DSC_CHAVE);

      if chave <> '' then
        if not ValidarChave(chave) then
          Gerador.wAlerta('#411', 'chave', DSC_REFNFE, ERR_MSG_INVALIDO);

      Gerador.wGrupo('/infCteComp');
    end
    else
    begin
      for i := 0 to CTe.infCTeComp10.Count - 1 do
      begin
        Gerador.wGrupo('infCteComp', '#382');

        chave := OnlyNumber(CTe.infCteComp10[i].chCTe);

        Gerador.wCampo(tcEsp, '#383', 'chCTe', 44, 44, 1, chave, DSC_CHAVE);

        if chave <> '' then
          if not ValidarChave(chave) then
            Gerador.wAlerta('#383', 'chCTe', DSC_REFNFE, ERR_MSG_INVALIDO);

        Gerador.wGrupo('/infCteComp');
      end;

      if CTe.infCTeComp10.Count > 10 then
        Gerador.wAlerta('#382', 'infCteComp', '', ERR_MSG_MAIOR_MAXIMO + '10');
    end;
  end;
end;

procedure TCTeW.GerarInfCTeAnu;
begin
  if (CTe.Ide.tpCTe = tcAnulacao) then
  begin
    Gerador.wGrupo('infCteAnu', '#412');
    Gerador.wCampo(tcEsp, '#413', 'chCte', 44, 44, 1, OnlyNumber(CTe.InfCTeAnu.chCTe), DSC_CHAVE);

    if OnlyNumber(CTe.InfCTeAnu.chCTe) <> '' then
      if not ValidarChave(CTe.InfCTeAnu.chCTe) then
        Gerador.wAlerta('#413', 'chCte', DSC_REFNFE, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcDat, '#414', 'dEmi', 10, 10, 1, CTe.InfCTeAnu.dEmi, DSC_DEMI);
    Gerador.wGrupo('/infCteAnu');
  end;
end;

procedure TCTeW.GerarautXML;
var
  i: Integer;
begin
  i := 0;
  while i < CTe.autXML.Count do
  begin
    if (trim(CTe.autXML[i].CNPJCPF) = trim(CTe.Rem.CNPJCPF)) or
       (trim(CTe.autXML[i].CNPJCPF) = trim(CTe.Dest.CNPJCPF)) or
       (trim(CTe.autXML[i].CNPJCPF) = trim(CTe.Exped.CNPJCPF)) or
       (trim(CTe.autXML[i].CNPJCPF) = trim(CTe.Receb.CNPJCPF)) or
       (trim(CTe.autXML[i].CNPJCPF) = trim(CTe.toma.CNPJCPF)) then
    begin
      CTe.autXML.Delete(i);
      Continue;
    end;

    Gerador.wGrupo('autXML', '#415');
    Gerador.wCampoCNPJCPF('#416', '#417', CTe.autXML[i].CNPJCPF);
    Gerador.wGrupo('/autXML');

    Inc(i);
  end;
  if CTe.autXML.Count > 10 then
    Gerador.wAlerta('#415', 'autXML', DSC_AUTXML, ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TCTeW.GerarinfRespTec;
begin
  if (CTe.infRespTec.CNPJ <> '') then
  begin
    Gerador.wGrupo('infRespTec', '#081');
    Gerador.wCampoCNPJ('#82', CTe.infRespTec.CNPJ, CODIGO_BRASIL, True);
    Gerador.wCampo(tcStr, '#083', 'xContato', 02, 60, 1, CTe.infRespTec.xContato, DSC_XCONTATO);
    Gerador.wCampo(tcStr, '#084', 'email   ', 06, 60, 1, CTe.infRespTec.email, DSC_EMAIL);
    Gerador.wCampo(tcStr, '#085', 'fone    ', 07, 12, 1, CTe.infRespTec.fone, DSC_FONE);

    if (idCSRT <> 0) and (CSRT <> '') then
    begin
      Gerador.wCampo(tcInt, '#086', 'idCSRT  ', 03, 03, 1, idCSRT, DSC_IDCSRT);
      Gerador.wCampo(tcStr, '#087', 'hashCSRT', 28, 28, 1, CalcularHashCSRT(CSRT, FChaveCTe), DSC_HASHCSRT);
    end;

    Gerador.wGrupo('/infRespTec');
  end;
end;

procedure TCTeW.GerarinfPercurso;
var
  i: Integer;
begin
  for i := 0 to CTe.Ide.infPercurso.Count - 1 do
  begin
    Gerador.wGrupo('infPercurso', '#023');
    Gerador.wCampo(tcStr, '#024', 'UFPer', 2, 2, 1, CTe.Ide.infPercurso[i].UFPer, DSC_UFPER);
    Gerador.wGrupo('/infPercurso');
  end;

  if CTe.Ide.infPercurso.Count > 25 then
    Gerador.wAlerta('#023', 'infPercurso', '', ERR_MSG_MAIOR_MAXIMO + '25');
end;

procedure TCTeW.GerarOrigem;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  if CTe.origem.xLgr <> '' then
  begin
    AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL,
                       CTe.origem.UF,
                       CTe.origem.xMun,
                       CTe.origem.cMun);

    Gerador.wGrupo('origem', '#117');
    Gerador.wCampo(tcStr, '#118', 'xLgr   ', 02, 60, 1, CTe.origem.xLgr, DSC_XLGR);
    Gerador.wCampo(tcStr, '#119', 'nro    ', 01, 60, 1, CTe.origem.nro, DSC_NRO);
    Gerador.wCampo(tcStr, '#120', 'xCpl   ', 01, 60, 0, CTe.origem.xCpl, DSC_XCPL);
    Gerador.wCampo(tcStr, '#121', 'xBairro', 02, 60, 1, CTe.origem.xBairro, DSC_XBAIRRO);
    Gerador.wCampo(tcInt, '#122', 'cMun   ', 07, 07, 1, cMun, DSC_CMUN);

    if not ValidarMunicipio(CTe.origem.cMun) then
      Gerador.wAlerta('#122', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#123', 'xMun', 02, 60, 1, xMun, DSC_XMUN);
    Gerador.wCampo(tcInt, '#124', 'CEP ', 08, 08, 0, CTe.origem.CEP, DSC_CEP);
    Gerador.wCampo(tcStr, '#125', 'UF  ', 02, 02, 1, xUF, DSC_UF);

    if not ValidarUF(xUF) then
      Gerador.wAlerta('#125', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#126', 'fone', 07, 12, 0, OnlyNumber(CTe.origem.fone), DSC_FONE);
    Gerador.wGrupo('/origem');
  end;
end;

procedure TCTeW.GerarDestino;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  if CTe.destino.xLgr <> '' then
  begin
    AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL,
                       CTe.destino.UF,
                       CTe.destino.xMun,
                       CTe.destino.cMun);

    Gerador.wGrupo('destino', '#127');
    Gerador.wCampo(tcStr, '#128', 'xLgr   ', 02, 60, 1, CTe.destino.xLgr, DSC_XLGR);
    Gerador.wCampo(tcStr, '#129', 'nro    ', 01, 60, 1, CTe.destino.nro, DSC_NRO);
    Gerador.wCampo(tcStr, '#130', 'xCpl   ', 01, 60, 0, CTe.destino.xCpl, DSC_XCPL);
    Gerador.wCampo(tcStr, '#131', 'xBairro', 02, 60, 1, CTe.destino.xBairro, DSC_XBAIRRO);
    Gerador.wCampo(tcInt, '#132', 'cMun   ', 07, 07, 1, cMun, DSC_CMUN);

    if not ValidarMunicipio(CTe.destino.cMun) then
      Gerador.wAlerta('#132', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#133', 'xMun', 02, 60, 1, xMun, DSC_XMUN);
    Gerador.wCampo(tcInt, '#134', 'CEP ', 08, 08, 0, CTe.destino.CEP, DSC_CEP);
    Gerador.wCampo(tcStr, '#135', 'UF  ', 02, 02, 1, xUF, DSC_UF);

    if not ValidarUF(xUF) then
      Gerador.wAlerta('#135', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#136', 'fone', 07, 12, 0, OnlyNumber(CTe.destino.fone), DSC_FONE);
    Gerador.wGrupo('/destino');
  end;
end;

procedure TCTeW.GerardetGTV;
var
  i: Integer;
begin
  Gerador.wGrupo('detGTV', '#137');

  for i := 0 to CTe.detGTV.infEspecie.Count - 1 do
  begin
    Gerador.wGrupo('infEspecie', '#138');
    Gerador.wCampo(tcStr, '#139', 'tpEspecie  ', 1, 01, 1, TEspecieToStr(CTe.detGTV.infEspecie[i].tpEspecie), DSC_TPESPECIE);
    Gerador.wCampo(tcDe2, '#140', 'vEspecie   ', 1, 15, 1, CTe.detGTV.infEspecie[i].vEspecie, DSC_VESPECIE);
    Gerador.wCampo(tcStr, '#141', 'tpNumerario', 1, 01, 1, tpNumerarioToStr(CTe.detGTV.infEspecie[i].tpNumerario), DSC_TPNUMERARIO);
    Gerador.wCampo(tcStr, '#142', 'xMoedaEstr ', 2, 60, 0, CTe.detGTV.infEspecie[i].xMoedaEstr, DSC_XMOEDAESTR);
    Gerador.wGrupo('/infEspecie');
  end;

  if CTe.detGTV.infEspecie.Count > 999 then
    Gerador.wAlerta('#138', 'infEspecie', '', ERR_MSG_MAIOR_MAXIMO + '999');

  Gerador.wCampo(tcDe4, '#143', 'qCarga', 01, 15, 1, CTe.detGTV.qCarga, DSC_XLGR);

  for i := 0 to CTe.detGTV.infVeiculo.Count - 1 do
  begin
    Gerador.wGrupo('infVeiculo', '#144');
    Gerador.wCampo(tcStr, '#145', 'placa', 1, 7, 1, CTe.detGTV.infVeiculo[i].placa, DSC_PLACA);
    Gerador.wCampo(tcStr, '#146', 'UF   ', 2, 2, 0, CTe.detGTV.infVeiculo[i].UF, DSC_UF);
    Gerador.wCampo(tcStr, '#147', 'RNTRC', 8, 8, 0, CTe.detGTV.infVeiculo[i].RNTRC, DSC_RNTRC);
    Gerador.wGrupo('/infVeiculo');
  end;

  if CTe.detGTV.infVeiculo.Count > 999 then
    Gerador.wAlerta('#148', 'infVeiculo', '', ERR_MSG_MAIOR_MAXIMO + '999');

  Gerador.wGrupo('/detGTV');
end;

procedure TCTeW.AjustarMunicipioUF(var xUF, xMun: String;
  var cMun: Integer; cPais: Integer; const vxUF, vxMun: String; vcMun: Integer);
var
  PaisBrasil: Boolean;
begin
  PaisBrasil := (cPais = CODIGO_BRASIL) or (cPais = 0);

  cMun := IfThen(PaisBrasil, vcMun, CMUN_EXTERIOR);
  xMun := IfThen(PaisBrasil, vxMun, XMUN_EXTERIOR);
  xUF  := IfThen(PaisBrasil, vxUF, UF_EXTERIOR);

  if FOpcoes.NormatizarMunicipios then
    if ( ( EstaZerado(cMun)) and (xMun <> XMUN_EXTERIOR) ) then
      cMun := ObterCodigoMunicipio(xMun, xUF, FOpcoes.FPathArquivoMunicipios)
    else if ( ( EstaVazio(xMun)) and (cMun <> CMUN_EXTERIOR) ) then
      xMun := ObterNomeMunicipio(cMun, xUF, FOpcoes.FPathArquivoMunicipios);
end;

end.
