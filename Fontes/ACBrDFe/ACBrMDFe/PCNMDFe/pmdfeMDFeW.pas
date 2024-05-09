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

unit pmdfeMDFeW;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador,
  pmdfeConversaoMDFe, pmdfeMDFe,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  pmdfeConsts, ACBrDFeUtil;

type
  TGeradorOpcoes = class(TPersistent)
  private
    FNormatizarMunicipios: boolean;
    FGerarTagAssinatura: TpcnTagAssinatura;
    FPathArquivoMunicipios: string;
    FValidarInscricoes: boolean;
    FValidarListaServicos: boolean;
  published
    property NormatizarMunicipios: boolean         read FNormatizarMunicipios  write FNormatizarMunicipios;
    property GerarTagAssinatura: TpcnTagAssinatura read FGerarTagAssinatura    write FGerarTagAssinatura;
    property PathArquivoMunicipios: string         read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: boolean            read FValidarInscricoes;
    property ValidarListaServicos: boolean         read FValidarListaServicos;
  end;

  TMDFeW = class(TPersistent)
  private
    FGerador: TGerador;
    FOpcoes: TGeradorOpcoes;
    FMDFe: TMDFe;

    FChaveMDFe: string;

    FVersaoDF: TVersaoMDFe;
    FModeloDF: string;
    FtpEmis: TpcnTipoEmissao;
    FtpAmb: TpcnTipoAmbiente;
    FIdCSRT: Integer;
    FCSRT: string;

    procedure GerarInfMDFe;       // Nivel 0

    procedure GerarIde;           // Nivel 1
    procedure GerarinfMunCarrega; // Nivel 2
    procedure GerarinfPercurso;   // Nivel 2

    procedure GerarEmit;          // Nivel 1
    procedure GerarEnderEmit;     // Nivel 2

    procedure GerarInfModal;      // Nivel 1
    procedure GerarRodo;          // Nivel 2
    procedure GerarVeiculoTracao; // Nivel 3
    procedure GerarVeiculoReboq;  // Nivel 3
    procedure GerarValePedagio;
    procedure GerarInfCIOT;
    procedure GerarInfContratante;
    procedure GerarLacRodo;
    procedure GerarInfPag;

    procedure GerarAereo;         // Nivel 2
    procedure GerarAquav;         // Nivel 2
    procedure GerarFerrov;        // Nivel 2

    procedure GerarInfDoc;        // Nivel 1
    procedure GerarInfSeg;        // Nivel 1
    procedure GerarTot;           // Nivel 1
    procedure GerarLacres;        // Nivel 1
    procedure GerarautXML;        // Nivel 1
    procedure GerarInfAdic;       // Nivel 1
    procedure GerarinfRespTec;    // Nivel 1
    procedure GerarProdPred;      // Nivel 1

    procedure AjustarMunicipioUF(var xUF: string; var xMun: string; var cMun: Integer; cPais: Integer; const vxUF, vxMun: string; vcMun: Integer);

  public
    constructor Create(AOwner: TMDFe);
    destructor Destroy; override;

    function GerarXml: boolean;
  published
    property Gerador: TGerador      read FGerador write FGerador;
    property Opcoes: TGeradorOpcoes read FOpcoes  write FOpcoes;
    property MDFe: TMDFe            read FMDFe    write FMDFe;

    property VersaoDF: TVersaoMDFe   read FVersaoDF write FVersaoDF;
    property ModeloDF: string        read FModeloDF write FModeloDF;
    property tpAmb: TpcnTipoAmbiente read FtpAmb    write FtpAmb;
    property tpEmis: TpcnTipoEmissao read FtpEmis   write FtpEmis;
    property IdCSRT: Integer         read FIdCSRT   write FIdCSRT;
    property CSRT: string            read FCSRT     write FCSRT;
  end;

implementation

uses
  StrUtils,
  Math,
  ACBrDFeConsts;

{ TMDFeW }

constructor TMDFeW.Create(AOwner: TMDFe);
begin
  inherited Create;

  FMDFe    := AOwner;
  FGerador := TGerador.Create;

  FGerador.FIgnorarTagNivel   := '|?xml version|MDFe xmlns|infMDFe versao|';
  FGerador.Opcoes.QuebraLinha := ';';

  FOpcoes := TGeradorOpcoes.Create;

  FOpcoes.FNormatizarMunicipios := False;
  FOpcoes.FGerarTagAssinatura   := taSomenteSeAssinada;
  FOpcoes.FValidarInscricoes    := False;
  FOpcoes.FValidarListaServicos := False;
end;

destructor TMDFeW.Destroy;
begin
  FGerador.Free;
  FOpcoes.Free;
  inherited Destroy;
end;

function TMDFeW.GerarXml: boolean;
var
  Gerar, Ok: boolean;
  xProtMDFe, VersaoStr: string;
begin
  // Carrega Layout que sera utilizado para gera o txt
  Gerador.ListaDeAlertas.Clear;
  Gerador.LayoutArquivoTXT.Clear;
  Gerador.ArquivoFormatoXML := '';
  Gerador.ArquivoFormatoTXT := '';

  {
    Os campos abaixo tem que ser os mesmos da configuração
  }
{
  MDFe.infMDFe.Versao := VersaoMDFeToDbl(VersaoDF);
  MDFe.ide.modelo := ModeloDF;
  MDFe.Ide.tpAmb := tpAmb;
  MDFe.ide.tpEmis := tpEmis;
}
  VersaoDF :=  DblToVersaoMDFe(Ok, MDFe.infMDFe.Versao);
  VersaoStr := 'versao="' + FloatToString(MDFe.infMDFe.Versao, '.', '#0.00') + '"';

  FChaveMDFe := GerarChaveAcesso(MDFe.ide.cUF, MDFe.ide.dhEmi, MDFe.emit.CNPJCPF, MDFe.ide.serie,
                            MDFe.ide.nMDF, StrToInt(TpEmisToStr(MDFe.ide.tpEmis)),
                            MDFe.ide.cMDF, StrToInt(MDFe.ide.modelo));
  MDFe.infMDFe.ID := 'MDFe' + FChaveMDFe;

  MDFe.ide.cDV  := ExtrairDigitoChaveAcesso(MDFe.infMDFe.ID);
  MDFe.Ide.cMDF := ExtrairCodigoChaveAcesso(MDFe.infMDFe.ID);

  {$IfDef FPC}
   Gerador.wGrupo(ENCODING_UTF8, '', False);
  {$EndIf}

  if MDFe.procMDFe.nProt <> '' then
    Gerador.wGrupo('mdfeProc ' + VersaoStr + ' ' + NAME_SPACE_MDFe, '');

  Gerador.wGrupo('MDFe ' + NAME_SPACE_MDFe);
  Gerador.wGrupo('infMDFe ' + VersaoStr + ' Id="' + MDFe.infMDFe.ID + '"');
  GerarInfMDFe;
  Gerador.wGrupo('/infMDFe');

  if MDFe.infMDFeSupl.qrCodMDFe <> '' then
  begin
    Gerador.wGrupo('infMDFeSupl');
    Gerador.wCampo(tcStr, '#196', 'qrCodMDFe', 50, 1000, 1,
                     '<![CDATA[' + MDFe.infMDFeSupl.qrCodMDFe + ']]>', DSC_INFQRCODMDFE, False);
    Gerador.wGrupo('/infMDFeSupl');
  end;

  if FOpcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := true;
    if FOpcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((MDFe.signature.DigestValue <> '') and (MDFe.signature.SignatureValue <> '') and (MDFe.signature.X509Certificate <> ''));
    if FOpcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
      Gerar := ((MDFe.signature.DigestValue = '') and (MDFe.signature.SignatureValue = '') and (MDFe.signature.X509Certificate = ''));
    if Gerar then
    begin
      FMDFe.signature.URI := '#MDFe' + OnlyNumber(MDFe.infMDFe.ID);
      FMDFe.signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
      FMDFe.signature.GerarXML;
      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + FMDFe.signature.Gerador.ArquivoFormatoXML;
    end;
  end;
  Gerador.wGrupo('/MDFe');

  if MDFe.procMDFe.nProt <> '' then
   begin
     xProtMDFe :=
           '<protMDFe ' + VersaoStr + '>' +
             '<infProt>'+
               '<tpAmb>'+TpAmbToStr(MDFe.procMDFe.tpAmb)+'</tpAmb>'+
               '<verAplic>'+MDFe.procMDFe.verAplic+'</verAplic>'+
               '<chMDFe>'+MDFe.procMDFe.chMDFe+'</chMDFe>'+
               '<dhRecbto>'+FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',MDFe.procMDFe.dhRecbto)+'</dhRecbto>'+
               '<nProt>'+MDFe.procMDFe.nProt+'</nProt>'+
               '<digVal>'+MDFe.procMDFe.digVal+'</digVal>'+
               '<cStat>'+IntToStr(MDFe.procMDFe.cStat)+'</cStat>'+
               '<xMotivo>'+MDFe.procMDFe.xMotivo+'</xMotivo>'+
             '</infProt>'+
             IfThen( (MDFe.procMDFe.cMsg > 0) or (MDFe.procMDFe.xMsg <> ''),
             '<infFisco>' +
               '<cMsg>' + IntToStr(MDFe.procMDFe.cMsg) + '</cMsg>' +
               '<xMsg>' + MDFe.procMDFe.xMsg + '</xMsg>' +
             '</infFisco>',
             '') +
           '</protMDFe>';

     Gerador.wTexto(xProtMDFe);
     Gerador.wGrupo('/mdfeProc');
   end;

  Gerador.gtAjustarRegistros(MDFe.infMDFe.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TMDFeW.GerarInfMDFe;
begin
  GerarIde;
  GerarEmit;
  GerarInfModal;
  GerarInfDoc;

  if VersaoDF >= ve300 then
  begin
    GerarInfSeg;
    GerarProdPred;
  end;

  GerarTot;
  GerarLacres;
  GerarautXML;
  GerarInfAdic;

  if VersaoDF >= ve300 then
    GerarinfRespTec;
end;

procedure TMDFeW.GerarIde;
begin
  Gerador.wGrupo('ide', '#004');
  Gerador.wCampo(tcInt, '#005', 'cUF', 02, 02, 1, MDFe.ide.cUF, DSC_CUF);
  if not ValidarCodigoUF(MDFe.ide.cUF) then
    Gerador.wAlerta('#005', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#006', 'tpAmb ', 01, 01, 1, tpAmbToStr(MDFe.Ide.tpAmb), DSC_TPAMB);
  Gerador.wCampo(tcStr, '#007', 'tpEmit', 01, 01, 1, TpEmitenteToStr(MDFe.Ide.tpEmit), DSC_TPEMIT);

  if (VersaoDF >= ve300) and (MDFe.Ide.tpTransp <> ttNenhum) then
    Gerador.wCampo(tcStr, '#007', 'tpTransp', 01, 01, 0, TTransportadorToStr(MDFe.Ide.tpTransp), DSC_TPTRANSP);

  Gerador.wCampo(tcInt, '#008', 'mod     ', 02, 02, 1, MDFe.ide.modelo, DSC_MOD);
  Gerador.wCampo(tcInt, '#009', 'serie   ', 01, 03, 1, MDFe.ide.serie, DSC_SERIE);
  Gerador.wCampo(tcInt, '#010', 'nMDF    ', 01, 09, 1, MDFe.ide.nMDF, DSC_NMDF);
  Gerador.wCampo(tcStr, '#011', 'cMDF    ', 08, 08, 1, IntToStrZero(ExtrairCodigoChaveAcesso(MDFe.infMDFe.ID), 8), DSC_CMDF);
  Gerador.wCampo(tcInt, '#012', 'cDV     ', 01, 01, 1, MDFe.Ide.cDV, DSC_CDV);
  Gerador.wCampo(tcStr, '#013', 'modal   ', 01, 01, 1, ModalToStr(MDFe.Ide.modal), DSC_MODAL);

  if VersaoDF = ve100 then
    Gerador.wCampo(tcDatHor, '#014', 'dhEmi', 19, 19, 1, MDFe.ide.dhEmi, DSC_DEMI)
  else
    Gerador.wCampo(tcStr, '#014', 'dhEmi', 25, 25, 1, DateTimeWithTimeZone(MDFe.ide.dhEmi, MDFe.ide.cUF), DSC_DEMI);

  Gerador.wCampo(tcStr, '#015', 'tpEmis  ', 01, 01, 1, tpEmisToStr(MDFe.Ide.tpEmis), DSC_TPEMIS);
  Gerador.wCampo(tcStr, '#016', 'procEmi ', 01, 01, 1, procEmiToStr(MDFe.Ide.procEmi), DSC_PROCEMI);
  Gerador.wCampo(tcStr, '#017', 'verProc ', 01, 20, 1, MDFe.Ide.verProc, DSC_VERPROC);
  Gerador.wCampo(tcStr, '#018', 'UFIni   ', 02, 02, 1, MDFe.ide.UFIni, DSC_UF);
  if not ValidarUF(MDFe.ide.UFIni) then
    Gerador.wAlerta('#018', 'UFIni', DSC_UF, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#019', 'UFFim   ', 02, 02, 1, MDFe.ide.UFFim, DSC_UF);
  if not ValidarUF(MDFe.ide.UFFim) then
    Gerador.wAlerta('#019', 'UFFim', DSC_UF, ERR_MSG_INVALIDO);

  GerarInfMunCarrega;
  GerarInfPercurso;

  if MDFe.ide.dhIniViagem > 0 then
  begin
    if VersaoDF = ve100 then
      Gerador.wCampo(tcDatHor, '#024a', 'dhIniViagem', 19, 19, 0, MDFe.ide.dhIniViagem, DSC_DHINIVIAGEM)
    else
      Gerador.wCampo(tcStr, '#024a', 'dhIniViagem', 25, 25, 0, DateTimeWithTimeZone(MDFe.ide.dhIniViagem, MDFe.ide.cUF), DSC_DHINIVIAGEM);
  end;

  if (MDFe.infMDFe.versao >= 3) and (MDFe.ide.indCanalVerde = tiSim) then
    Gerador.wCampo(tcStr, '#027', 'indCanalVerde', 01, 01, 0, TindicadorToStr(MDFe.ide.indCanalVerde), DSC_INDCANALVERDE);

  if (MDFe.infMDFe.versao >= 3) and (MDFe.ide.indCarregaPosterior = tiSim) then
    Gerador.wCampo(tcStr, '#028', 'indCarregaPosterior', 01, 01, 0, TindicadorToStr(MDFe.ide.indCarregaPosterior), DSC_INDCARREGAPOSTERIOR);

  Gerador.wGrupo('/ide');
end;

procedure TMDFeW.GerarinfMunCarrega;
var
  i: Integer;
begin
  for i := 0 to MDFe.Ide.infMunCarrega.Count - 1 do
  begin
    Gerador.wGrupo('infMunCarrega', '#020');
    Gerador.wCampo(tcInt, '#021', 'cMunCarrega', 07, 07, 1, MDFe.Ide.infMunCarrega[i].cMunCarrega, DSC_CMUNCARREGA);
    Gerador.wCampo(tcStr, '#022', 'xMunCarrega', 02, 60, 1, MDFe.Ide.infMunCarrega[i].xMunCarrega, DSC_XMUNCARREGA);
    Gerador.wGrupo('/infMunCarrega');
  end;
  if MDFe.Ide.infMunCarrega.Count > 50 then
   Gerador.wAlerta('#020', 'infMunCarrega', '', ERR_MSG_MAIOR_MAXIMO + '50');
end;

procedure TMDFeW.GerarinfPercurso;
var
  i: Integer;
begin
  for i := 0 to MDFe.Ide.infPercurso.Count - 1 do
  begin
    Gerador.wGrupo('infPercurso', '#023');
    Gerador.wCampo(tcStr, '#024', 'UFPer', 2, 2, 1, MDFe.Ide.infPercurso[i].UFPer, DSC_UFPER);
    Gerador.wGrupo('/infPercurso');
  end;
  if MDFe.Ide.infPercurso.Count > 25 then
   Gerador.wAlerta('#023', 'infPercurso', '', ERR_MSG_MAIOR_MAXIMO + '25');
end;

procedure TMDFeW.GerarEmit;
begin
  Gerador.wGrupo('emit', '#025');
  Gerador.wCampoCNPJCPF('#026', '026a', MDFe.Emit.CNPJCPF);
  Gerador.wCampo(tcStr, '#027', 'IE   ', 02, 14, 1, OnlyNumber(MDFe.Emit.IE), DSC_IE);
  if (FOpcoes.ValidarInscricoes)
   then if not ValidarIE(MDFe.Emit.IE, MDFe.Emit.enderEmit.UF) then
         Gerador.wAlerta('#027', 'IE', DSC_IE, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#028', 'xNome', 02, 60, 1, MDFe.Emit.xNome, DSC_XNOME);
  Gerador.wCampo(tcStr, '#029', 'xFant', 01, 60, 0, MDFe.Emit.xFant, DSC_XFANT);

  GerarEnderEmit;
  Gerador.wGrupo('/emit');
end;

procedure TMDFeW.GerarEnderEmit;
var
  cMun: Integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL,
                                      MDFe.Emit.enderEmit.UF,
                                      MDFe.Emit.enderEmit.xMun,
                                      MDFe.Emit.EnderEmit.cMun);
  Gerador.wGrupo('enderEmit', '#030');
  Gerador.wCampo(tcStr, '#031', 'xLgr   ', 02, 60, 1, MDFe.Emit.enderEmit.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#032', 'nro    ', 01, 60, 1, MDFe.Emit.enderEmit.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#033', 'xCpl   ', 01, 60, 0, MDFe.Emit.enderEmit.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#034', 'xBairro', 02, 60, 1, MDFe.Emit.enderEmit.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#035', 'cMun   ', 07, 07, 1, cMun, DSC_CMUN);
  if not ValidarMunicipio(MDFe.Emit.EnderEmit.cMun) then
    Gerador.wAlerta('#035', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#036', 'xMun   ', 01, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#037', 'CEP    ', 08, 08, 0, MDFe.Emit.enderEmit.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#038', 'UF     ', 02, 02, 1, xUF, DSC_UF);
  if not ValidarUF(xUF) then
    Gerador.wAlerta('#038', 'UF', DSC_UF, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#039', 'fone   ', 07, 12, 0, OnlyNumber(MDFe.Emit.EnderEmit.fone), DSC_FONE);
  Gerador.wCampo(tcStr, '#040', 'email  ', 01, 60, 0, MDFe.Emit.EnderEmit.email, DSC_EMAIL);
  Gerador.wGrupo('/enderEmit');
end;

procedure TMDFeW.GerarInfModal;
var
 versao: string;
begin
  versao := GetVersaoModalMDFe(VersaoDF, MDFe.Ide.modal);

  case StrToInt(ModalToStr(MDFe.Ide.modal)) of
   1: Gerador.wGrupo('infModal versaoModal="' + versao + '"', '#041');
   2: Gerador.wGrupo('infModal versaoModal="' + versao + '"', '#041');
   3: Gerador.wGrupo('infModal versaoModal="' + versao + '"', '#041');
   4: Gerador.wGrupo('infModal versaoModal="' + versao + '"', '#041');
  end;
  case StrToInt(ModalToStr(MDFe.Ide.modal)) of
   1: GerarRodo;   // Informações do Modal Rodoviário
   2: GerarAereo;  // Informações do Modal Aéreo
   3: GerarAquav;  // Informações do Modal Aquaviário
   4: GerarFerrov; // Informações do Modal Ferroviário
  end;
  Gerador.wGrupo('/infModal');
end;

procedure TMDFeW.GerarRodo;
begin
  Gerador.wGrupo('rodo', '#01');

  if VersaoDF = ve100 then
  begin
    Gerador.wCampo(tcStr, '#02', 'RNTRC', 08, 08, 0, OnlyNumber(MDFe.Rodo.RNTRC), DSC_RNTRC);
    Gerador.wCampo(tcStr, '#03', 'CIOT ', 12, 12, 0, MDFe.Rodo.CIOT, DSC_CIOT);
  end
  else
  begin
    if (MDFe.Rodo.infANTT.RNTRC <> '') or
       (MDFe.Rodo.infANTT.infCIOT.Count > 0) or
       (MDFe.Rodo.infANTT.valePed.disp.Count > 0) or
       (MDFe.rodo.infANTT.infContratante.Count > 0) or
       (MDFe.rodo.infANTT.infPag.Count > 0) then
    begin
      Gerador.wGrupo('infANTT', '#02');
      Gerador.wCampo(tcStr, '#02', 'RNTRC', 08, 08, 0, OnlyNumber(MDFe.Rodo.infANTT.RNTRC), DSC_RNTRC);
      GerarInfCIOT;
      GerarValePedagio;
      GerarInfContratante;
      GerarInfPag;
      Gerador.wGrupo('/infANTT');
    end;
  end;

  GerarVeiculoTracao;
  GerarVeiculoReboq;

  if (VersaoDF = ve100) then
    GerarValePedagio;

  Gerador.wCampo(tcStr, '#45', 'codAgPorto', 01, 16, 0, MDFe.Rodo.codAgPorto, DSC_CODAGPORTO);

  if VersaoDF >= ve300 then
    GerarLacRodo;

  Gerador.wGrupo('/rodo');
end;

procedure TMDFeW.GerarVeiculoTracao;
var
  i: Integer;
begin
  Gerador.wGrupo('veicTracao', '#04');
  Gerador.wCampo(tcStr, '#05',  'cInt   ', 01, 10, 0, MDFe.Rodo.veicTracao.cInt, DSC_CINTV);
  Gerador.wCampo(tcStr, '#06',  'placa  ', 01, 07, 1, MDFe.Rodo.veicTracao.placa, DSC_PLACA);
  Gerador.wCampo(tcStr, '#06a', 'RENAVAM', 09, 11, 0, MDFe.Rodo.veicTracao.RENAVAM, DSC_RENAVAM);
  Gerador.wCampo(tcInt, '#07',  'tara   ', 01, 06, 1, MDFe.Rodo.veicTracao.tara, DSC_TARA);
  Gerador.wCampo(tcInt, '#08',  'capKG  ', 01, 06, 0, MDFe.Rodo.veicTracao.capKG, DSC_CAPKG);
  Gerador.wCampo(tcInt, '#09',  'capM3  ', 01, 03, 0, MDFe.Rodo.veicTracao.capM3, DSC_CAPM3);

  if (MDFe.Rodo.veicTracao.Prop.CNPJCPF <> '') or
     (MDFe.Rodo.veicTracao.Prop.RNTRC <> '') or
     (MDFe.Rodo.veicTracao.Prop.xNome <> '') then
  begin
    Gerador.wGrupo('prop', '#32');

    Gerador.wCampoCNPJCPF('#11', '#12', MDFe.Rodo.veicTracao.Prop.CNPJCPF);
    Gerador.wCampo(tcStr, '#13', 'RNTRC ', 08, 08, 1, OnlyNumber(MDFe.Rodo.veicTracao.Prop.RNTRC), DSC_RNTRC);
    Gerador.wCampo(tcStr, '#14', 'xNome ', 02, 60, 1, MDFe.Rodo.veicTracao.Prop.xNome, DSC_XNOME);

    if MDFe.Rodo.veicTracao.Prop.IE <> '' then
    begin
      if MDFe.Rodo.veicTracao.Prop.IE = 'ISENTO' then
        Gerador.wCampo(tcStr, '#15', 'IE ', 00, 14, 1, MDFe.Rodo.veicTracao.Prop.IE, DSC_IE)
      else
        Gerador.wCampo(tcStr, '#15', 'IE ', 02, 14, 1, OnlyNumber(MDFe.Rodo.veicTracao.Prop.IE), DSC_IE);

      if (FOpcoes.ValidarInscricoes) then
        if not ValidarIE(MDFe.Rodo.veicTracao.Prop.IE, MDFe.Rodo.veicTracao.Prop.UF) then
          Gerador.wAlerta('#15', 'IE', DSC_IE, ERR_MSG_INVALIDO);
    end
    else
     Gerador.wCampo(tcStr, '#15', 'IE', 00, 14, 1, '', DSC_IE);

    Gerador.wCampo(tcStr, '#16', 'UF', 02, 02, 1, MDFe.Rodo.veicTracao.Prop.UF, DSC_CUF);

    if not ValidarUF(MDFe.Rodo.veicTracao.Prop.UF) then
      Gerador.wAlerta('#16', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#17', 'tpProp', 01, 01, 1, TpPropToStr(MDFe.Rodo.veicTracao.Prop.tpProp), DSC_TPPROP);

    Gerador.wGrupo('/prop');
  end;

  for i := 0 to MDFe.rodo.veicTracao.condutor.Count - 1 do
  begin
    Gerador.wGrupo('condutor', '#18');
    Gerador.wCampo(tcStr, '#19', 'xNome', 02, 60, 1, MDFe.rodo.veicTracao.condutor[i].xNome, DSC_XNOME);
    Gerador.wCampo(tcStr, '#20', 'CPF  ', 11, 11, 1, MDFe.rodo.veicTracao.condutor[i].CPF, DSC_CPF);
    Gerador.wGrupo('/condutor');
  end;

  if MDFe.rodo.veicTracao.condutor.Count > 10 then
    Gerador.wAlerta('#18', 'condutor', '', ERR_MSG_MAIOR_MAXIMO + '10');

  Gerador.wCampo(tcStr, '#21', 'tpRod', 02, 02, 1, TpRodadoToStr(MDFe.Rodo.veicTracao.tpRod), DSC_TPROD);
  Gerador.wCampo(tcStr, '#22', 'tpCar', 02, 02, 1, TpCarroceriaToStr(MDFe.Rodo.veicTracao.tpCar), DSC_TPCAR);
  Gerador.wCampo(tcStr, '#23', 'UF   ', 02, 02, 0, MDFe.Rodo.veicTracao.UF, DSC_CUF);

  if MDFe.Rodo.veicTracao.UF <> '' then
    if not ValidarUF(MDFe.Rodo.veicTracao.UF) then
      Gerador.wAlerta('#23', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wGrupo('/veicTracao');
end;

procedure TMDFeW.GerarVeiculoReboq;
var
  i: Integer;
begin
  for i := 0 to MDFe.rodo.veicReboque.Count - 1 do
  begin
    Gerador.wGrupo('veicReboque', '#24');
    Gerador.wCampo(tcStr, '#25',  'cInt   ', 01, 10, 0, MDFe.Rodo.veicReboque[i].cInt, DSC_CINTV);
    Gerador.wCampo(tcStr, '#26',  'placa  ', 01, 07, 1, MDFe.Rodo.veicReboque[i].placa, DSC_PLACA);
    Gerador.wCampo(tcStr, '#26a', 'RENAVAM', 09, 11, 0, MDFe.Rodo.veicReboque[i].RENAVAM, DSC_RENAVAM);
    Gerador.wCampo(tcInt, '#27',  'tara   ', 01, 06, 1, MDFe.Rodo.veicReboque[i].tara, DSC_TARA);
    Gerador.wCampo(tcInt, '#28',  'capKG  ', 01, 06, 1, MDFe.Rodo.veicReboque[i].capKG, DSC_CAPKG);
    Gerador.wCampo(tcInt, '#29',  'capM3  ', 01, 03, 0, MDFe.Rodo.veicReboque[i].capM3, DSC_CAPM3);

    if (MDFe.Rodo.veicReboque[i].Prop.CNPJCPF <> '') or
       (MDFe.Rodo.veicReboque[i].Prop.RNTRC <> '') or
       (MDFe.Rodo.veicReboque[i].Prop.xNome <> '') then
    begin
      Gerador.wGrupo('prop', '#30');

      Gerador.wCampoCNPJCPF('#31', '#32', MDFe.Rodo.veicReboque[i].Prop.CNPJCPF);
      Gerador.wCampo(tcStr, '#33', 'RNTRC ', 08, 08, 1, OnlyNumber(MDFe.Rodo.veicReboque[i].Prop.RNTRC), DSC_RNTRC);
      Gerador.wCampo(tcStr, '#34', 'xNome ', 02, 60, 1, MDFe.Rodo.veicReboque[i].Prop.xNome, DSC_XNOME);

      if MDFe.Rodo.veicReboque[i].Prop.IE <> '' then 
      begin
        if MDFe.Rodo.veicReboque[i].Prop.IE = 'ISENTO' then
          Gerador.wCampo(tcStr, '#35', 'IE ', 00, 14, 1, MDFe.Rodo.veicReboque[i].Prop.IE, DSC_IE)
        else
          Gerador.wCampo(tcStr, '#35', 'IE ', 02, 14, 1, OnlyNumber(MDFe.Rodo.veicReboque[i].Prop.IE), DSC_IE);

        if (FOpcoes.ValidarInscricoes)then
          if not ValidarIE(MDFe.Rodo.veicReboque[i].Prop.IE, MDFe.Rodo.veicReboque[i].Prop.UF) then
            Gerador.wAlerta('#35', 'IE', DSC_IE, ERR_MSG_INVALIDO);
      end
      else
        Gerador.wCampo(tcStr, '#35', 'IE', 00, 14, 1, '', DSC_IE);

      Gerador.wCampo(tcStr, '#36', 'UF', 02, 02, 1, MDFe.Rodo.veicReboque[i].Prop.UF, DSC_CUF);

      if not ValidarUF(MDFe.Rodo.veicReboque[i].Prop.UF) then
        Gerador.wAlerta('#36', 'UF', DSC_UF, ERR_MSG_INVALIDO);

      Gerador.wCampo(tcStr, '#37', 'tpProp', 01, 01, 1, TpPropToStr(MDFe.Rodo.veicReboque[i].Prop.tpProp), DSC_TPPROP);

      Gerador.wGrupo('/prop');
    end;

    Gerador.wCampo(tcStr, '#38', 'tpCar', 02, 02, 1, TpCarroceriaToStr(MDFe.Rodo.veicReboque[i].tpCar), DSC_TPCAR);
    Gerador.wCampo(tcStr, '#39', 'UF   ', 02, 02, 0, MDFe.Rodo.veicReboque[i].UF, DSC_CUF);

    if MDFe.Rodo.veicReboque[i].UF <> '' then
      if not ValidarUF(MDFe.Rodo.veicReboque[i].UF) then
        Gerador.wAlerta('#39', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    Gerador.wGrupo('/veicReboque');
  end;

  if MDFe.rodo.veicReboque.Count > 3 then
   Gerador.wAlerta('#15', 'veicReboque', '', ERR_MSG_MAIOR_MAXIMO + '3');
end;

procedure TMDFeW.GerarValePedagio;
var
  i: Integer;
begin
  if VersaoDF = ve100 then
  begin
    if MDFe.rodo.valePed.disp.Count > 0 then
    begin
      Gerador.wGrupo('valePed', '#23');

      for i := 0 to MDFe.rodo.valePed.disp.Count - 1 do
      begin
        Gerador.wGrupo('disp', '#24');
        Gerador.wCampo(tcStr, '#25', 'CNPJForn', 14, 14, 1, MDFe.Rodo.valePed.disp[i].CNPJForn, DSC_CNPJFORN);
        Gerador.wCampo(tcStr, '#26', 'CNPJPg'  , 14, 14, 0, MDFe.Rodo.valePed.disp[i].CNPJPg, DSC_CNPJPG);
        Gerador.wCampo(tcStr, '#27', 'nCompra' , 01, 20, 0, MDFe.Rodo.valePed.disp[i].nCompra, DSC_NCOMPRA);
        Gerador.wGrupo('/disp');
      end;

      if MDFe.rodo.valePed.disp.Count > 990 then
        Gerador.wAlerta('#24', 'disp', '', ERR_MSG_MAIOR_MAXIMO + '990');

      Gerador.wGrupo('/valePed');
    end;
  end
  else
  begin
    if MDFe.rodo.infANTT.valePed.disp.Count > 0 then
    begin
      Gerador.wGrupo('valePed', '#23');

      for i := 0 to MDFe.rodo.infANTT.valePed.disp.Count - 1 do
      begin
        Gerador.wGrupo('disp', '#24');
        Gerador.wCampo(tcStr, '#25', 'CNPJForn', 14, 14, 1, MDFe.Rodo.infANTT.valePed.disp[i].CNPJForn, DSC_CNPJFORN);

        if Length(MDFe.Rodo.infANTT.valePed.disp[i].CNPJPg) = 14 then
          Gerador.wCampo(tcStr, '#26', 'CNPJPg', 14, 14, 0, MDFe.Rodo.infANTT.valePed.disp[i].CNPJPg, DSC_CNPJPG)
        else
          Gerador.wCampo(tcStr, '#26', 'CPFPg' , 11, 11, 0, MDFe.Rodo.infANTT.valePed.disp[i].CNPJPg, DSC_CNPJPG);

        Gerador.wCampo(tcStr, '#27', 'nCompra  ', 01, 20, 0, MDFe.Rodo.infANTT.valePed.disp[i].nCompra, DSC_NCOMPRA);
        Gerador.wCampo(tcDe2, '#20', 'vValePed ', 01, 15, 1, MDFe.Rodo.infANTT.valePed.disp[i].vValePed, DSC_VVALEPED);
        Gerador.wCampo(tcStr, '#',   'tpValePed', 01, 02, 0, tpValePedToStr(MDFe.Rodo.infANTT.valePed.disp[i].tpValePed), DSC_TPVALEPED);

        Gerador.wGrupo('/disp');
      end;

      if MDFe.Rodo.infANTT.valePed.disp.Count > 990 then
        Gerador.wAlerta('#24', 'disp', '', ERR_MSG_MAIOR_MAXIMO + '990');

      Gerador.wCampo(tcStr, '#', 'categCombVeic', 01, 02, 0, categCombVeicToStr(MDFe.Rodo.infANTT.valePed.categCombVeic), DSC_CATEGCOMBVEIC);

      Gerador.wGrupo('/valePed');
    end;
  end;
end;

procedure TMDFeW.GerarInfCIOT;
var
  i: Integer;
begin
  for i := 0 to MDFe.rodo.infANTT.infCIOT.Count - 1 do
  begin
    Gerador.wGrupo('infCIOT', '#04');
    Gerador.wCampo(tcStr, '#05', 'CIOT ', 12, 12, 0, MDFe.Rodo.infANTT.infCIOT[i].CIOT, DSC_CIOT);
    Gerador.wCampoCNPJCPF('#06', '#07', MDFe.rodo.infANTT.infCIOT[i].CNPJCPF);
    Gerador.wGrupo('/infCIOT');
  end;
  if MDFe.rodo.infANTT.infCIOT.Count > 990 then
    Gerador.wAlerta('#04', 'infCIOT', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TMDFeW.GerarInfContratante;
var
  i: Integer;
begin
  for i := 0 to MDFe.rodo.infANTT.infContratante.Count - 1 do
  begin
    Gerador.wGrupo('infContratante', '#15');
    Gerador.wCampo(tcStr, '#', 'xNome', 02, 60, 0, MDFe.rodo.infANTT.infContratante[i].xNome, DSC_XNOME);

    if MDFe.rodo.infANTT.infContratante[i].idEstrangeiro <> '' then
      Gerador.wCampo(tcStr, '#', 'idEstrangeiro', 02, 20, 0, MDFe.rodo.infANTT.infContratante[i].idEstrangeiro, '***')
    else
      Gerador.wCampoCNPJCPF('#16', '#17', MDFe.rodo.infANTT.infContratante[i].CNPJCPF);

    if (MDFe.rodo.infANTT.infContratante[i].infContrato.NroContrato <> '') and
       (MDFe.rodo.infANTT.infContratante[i].infContrato.vContratoGlobal <> 0) then
    begin
      Gerador.wGrupo('infContrato', '#22');
      Gerador.wCampo(tcStr, '#23', 'NroContrato',     01, 20, 1, MDFe.rodo.infANTT.infContratante[i].infContrato.NroContrato, DSC_NROCONTRATO);
      Gerador.wCampo(tcDe2, '#24', 'vContratoGlobal', 01, 15, 1, MDFe.rodo.infANTT.infContratante[i].infContrato.vContratoGlobal, DSC_VCONTRATOGLOBAL);
      Gerador.wGrupo('/infContrato');
    end;

    Gerador.wGrupo('/infContratante');
  end;
  if MDFe.rodo.infANTT.infContratante.Count > 990 then
    Gerador.wAlerta('#15', 'infContratante', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TMDFeW.GerarLacRodo;
var
  i: Integer;
begin
  for i := 0 to MDFe.rodo.lacRodo.Count - 1 do
  begin
    Gerador.wGrupo('lacRodo', '#57');
    Gerador.wCampo(tcStr, '#58', 'nLacre', 01, 20, 1, MDFe.rodo.lacRodo[i].nLacre, DSC_NLACRE);
    Gerador.wGrupo('/lacRodo');
  end;
  if MDFe.rodo.lacRodo.Count > 990 then
   Gerador.wAlerta('#57', 'lacRodo', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TMDFeW.GerarAereo;
begin
  Gerador.wGrupo('aereo', '#01');
  Gerador.wCampo(tcStr, '#02', 'nac    ', 01, 04, 1, MDFe.Aereo.nac, DSC_NAC);
  Gerador.wCampo(tcStr, '#03', 'matr   ', 01, 06, 1, MDFe.Aereo.matr, DSC_MATR);
  Gerador.wCampo(tcStr, '#04', 'nVoo   ', 05, 09, 1, MDFe.Aereo.nVoo, DSC_NVOO);
  Gerador.wCampo(tcStr, '#05', 'cAerEmb', 03, 04, 1, MDFe.Aereo.cAerEmb, DSC_CAEREMB);
  Gerador.wCampo(tcStr, '#06', 'cAerDes', 03, 04, 1, MDFe.Aereo.cAerDes, DSC_CAERDES);
  Gerador.wCampo(tcDat, '#07', 'dVoo   ', 10, 10, 0, MDFe.Aereo.dVoo, DSC_DVOO);
  Gerador.wGrupo('/aereo');
end;

procedure TMDFeW.GerarAquav;
var
  i: Integer;
begin
  Gerador.wGrupo('aquav', '#01');

  if VersaoDF = ve100 then
    Gerador.wCampo(tcStr, '#02', 'CNPJAgeNav', 14, 14, 1, MDFe.aquav.CNPJAgeNav, DSC_CNPJAGENAV);

  if VersaoDF >= ve300 then
     Gerador.wCampo(tcStr, '#03', 'irin', 01, 10, 1, MDFe.aquav.irin, DSC_IRIN);

  Gerador.wCampo(tcStr, '#04', 'tpEmb     ', 02, 02, 1, MDFe.aquav.tpEmb, DSC_TPEMB);
  Gerador.wCampo(tcStr, '#05', 'cEmbar    ', 01, 10, 1, MDFe.aquav.cEmbar, DSC_CEMBAR);
  Gerador.wCampo(tcStr, '#06', 'xEmbar    ', 01, 60, 1, MDFe.aquav.xEmbar, DSC_XEMBAR);
  Gerador.wCampo(tcStr, '#07', 'nViag     ', 01, 10, 1, MDFe.aquav.nViagem, DSC_NVIAG);
  Gerador.wCampo(tcStr, '#08', 'cPrtEmb   ', 01, 05, 1, MDFe.aquav.cPrtEmb, DSC_CPRTEMB);
  Gerador.wCampo(tcStr, '#09', 'cPrtDest  ', 01, 05, 1, MDFe.aquav.cPrtDest, DSC_CPRTDEST);

  if VersaoDF >= ve300 then
  begin
    Gerador.wCampo(tcStr, '#10', 'prtTrans  ', 01, 60, 0, MDFe.aquav.prtTrans, DSC_PRTTRANS);
    Gerador.wCampo(tcStr, '#11', 'tpNav     ', 01, 01, 0, TpNavegacaoToStr(MDFe.aquav.tpNav), DSC_TPNAV);
  end;

  for i := 0 to MDFe.aquav.infTermCarreg.Count - 1 do
  begin
    Gerador.wGrupo('infTermCarreg', '#12');
    Gerador.wCampo(tcStr, '#13', 'cTermCarreg', 01, 08, 1, MDFe.aquav.infTermCarreg[i].cTermCarreg, DSC_CTERMCARREG);
    Gerador.wCampo(tcStr, '#13', 'xTermCarreg', 01, 60, 1, MDFe.aquav.infTermCarreg[i].xTermCarreg, DSC_XTERMCARREG);
    Gerador.wGrupo('/infTermCarreg');
  end;
  if MDFe.aquav.infTermCarreg.Count > 5 then
   Gerador.wAlerta('#12', 'infTermCarreg', '', ERR_MSG_MAIOR_MAXIMO + '5');

  for i := 0 to MDFe.aquav.infTermDescarreg.Count - 1 do
  begin
    Gerador.wGrupo('infTermDescarreg', '#14');
    Gerador.wCampo(tcStr, '#15', 'cTermDescarreg', 01, 08, 1, MDFe.aquav.infTermDescarreg[i].cTermDescarreg, DSC_CTERMDESCAR);
    Gerador.wCampo(tcStr, '#15', 'xTermDescarreg', 01, 60, 1, MDFe.aquav.infTermDescarreg[i].xTermDescarreg, DSC_XTERMDESCAR);
    Gerador.wGrupo('/infTermDescarreg');
  end;
  if MDFe.aquav.infTermDescarreg.Count > 5 then
   Gerador.wAlerta('#14', 'infTermDescarreg', '', ERR_MSG_MAIOR_MAXIMO + '5');

  for i := 0 to MDFe.aquav.infEmbComb.Count - 1 do
  begin
    Gerador.wGrupo('infEmbComb', '#16');
    Gerador.wCampo(tcStr, '#17', 'cEmbComb', 01, 10, 1, MDFe.aquav.infEmbComb[i].cEmbComb, DSC_CEMBCOMB);

    if VersaoDF >= ve300 then
      Gerador.wCampo(tcStr, '#18', 'xBalsa', 01, 60, 1, MDFe.aquav.infEmbComb[i].xBalsa, DSC_XBALSA);

    Gerador.wGrupo('/infEmbComb');
  end;
  if MDFe.aquav.infEmbComb.Count > 30 then
   Gerador.wAlerta('#16', 'infEmbComb', '', ERR_MSG_MAIOR_MAXIMO + '30');

  for i := 0 to MDFe.aquav.infUnidCargaVazia.Count - 1 do
  begin
    Gerador.wGrupo('infUnidCargaVazia', '#019');
    Gerador.wCampo(tcStr, '#020', 'idUnidCargaVazia', 01, 20, 1, MDFe.aquav.infUnidCargaVazia[i].idUnidCargaVazia, DSC_IDUNIDCARGA);
    Gerador.wCampo(tcStr, '#021', 'tpUnidCargaVazia', 01, 01, 1, UnidCargaToStr(MDFe.aquav.infUnidCargaVazia[i].tpUnidCargaVazia), DSC_TPUNIDCARGA);
    Gerador.wGrupo('/infUnidCargaVazia');
  end;
  if MDFe.aquav.infUnidCargaVazia.Count > 999 then
   Gerador.wAlerta('#19', 'infUnidCargaVazia', '', ERR_MSG_MAIOR_MAXIMO + '999');

  if VersaoDF >= ve300 then
  begin
    for i := 0 to MDFe.aquav.infUnidTranspVazia.Count - 1 do
    begin
      Gerador.wGrupo('infUnidTranspVazia', '#022');
      Gerador.wCampo(tcStr, '#023', 'idUnidTranspVazia', 01, 20, 1, MDFe.aquav.infUnidTranspVazia[i].idUnidTranspVazia, DSC_IDUNIDTRANSP);
      Gerador.wCampo(tcStr, '#024', 'tpUnidTranspVazia', 01, 01, 1, UnidTranspToStr(MDFe.aquav.infUnidTranspVazia[i].tpUnidTranspVazia), DSC_TPUNIDTRANSP);
      Gerador.wGrupo('/infUnidTranspVazia');
    end;
    if MDFe.aquav.infUnidTranspVazia.Count > 999 then
     Gerador.wAlerta('#22', 'infUnidTranspVazia', '', ERR_MSG_MAIOR_MAXIMO + '999');
  end;
  Gerador.wGrupo('/aquav');
end;

procedure TMDFeW.GerarFerrov;
var
  i: Integer;
begin
  Gerador.wGrupo('ferrov', '#01');

  Gerador.wGrupo('trem', '#02');
  Gerador.wCampo(tcStr, '#03', 'xPref    ', 01, 10, 1, MDFe.ferrov.xPref, DSC_XPREF);

  if VersaoDF = ve100 then
    Gerador.wCampo(tcDatHor, '#04', 'dhTrem', 19, 19, 0, MDFe.ferrov.dhTrem, DSC_DHTREM)
  else
    Gerador.wCampo(tcStr, '#04', 'dhTrem', 25, 25, 0, DateTimeWithTimeZone(MDFe.ferrov.dhTrem, MDFe.ide.cUF), DSC_DHTREM);

  Gerador.wCampo(tcStr, '#05', 'xOri     ', 01, 03, 1, MDFe.ferrov.xOri, DSC_XORI);
  Gerador.wCampo(tcStr, '#06', 'xDest    ', 01, 03, 1, MDFe.ferrov.xDest, DSC_XDEST);
  Gerador.wCampo(tcInt, '#07', 'qVag     ', 01, 03, 1, MDFe.ferrov.qVag, DSC_QVAG);
  Gerador.wGrupo('/trem');

  for i := 0 to MDFe.ferrov.vag.Count - 1 do
  begin
    Gerador.wGrupo('vag', '#08');

    if MDFe.infMDFe.versao >= 3 then
    begin
      Gerador.wCampo(tcDe3, '#09', 'pesoBC', 1, 7, 1, MDFe.ferrov.vag[i].pesoBC, '****');
      Gerador.wCampo(tcDe3, '#10', 'pesoR ', 1, 7, 1, MDFe.ferrov.vag[i].pesoR, '****');
      Gerador.wCampo(tcStr, '#11', 'tpVag ', 1, 3, 0, MDFe.ferrov.vag[i].tpVag, '****');
    end;

    Gerador.wCampo(tcStr, '#12', 'serie', 3, 3, 1, MDFe.ferrov.vag[i].serie, DSC_NSERIE);
    Gerador.wCampo(tcInt, '#13', 'nVag ', 1, 8, 1, MDFe.ferrov.vag[i].nVag, DSC_NVAG);
    Gerador.wCampo(tcInt, '#14', 'nSeq ', 1, 3, 0, MDFe.ferrov.vag[i].nSeq, DSC_NSEQ);
    Gerador.wCampo(tcDe3, '#15', 'TU   ', 1, 7, 1, MDFe.ferrov.vag[i].TU, DSC_TU);
    Gerador.wGrupo('/vag');
  end;
  if MDFe.ferrov.vag.Count > 990 then
   Gerador.wAlerta('#08', 'vag', '', ERR_MSG_MAIOR_MAXIMO + '990');

  Gerador.wGrupo('/ferrov');
end;

procedure TMDFeW.GerarInfDoc;
var
  i, j, k, l, m: Integer;
begin
  Gerador.wGrupo('infDoc', '#040');

  for i := 0 to MDFe.infDoc.infMunDescarga.Count - 1 do
  begin
    Gerador.wGrupo('infMunDescarga', '#045');
    Gerador.wCampo(tcInt, '#046', 'cMunDescarga', 07, 07, 1, MDFe.infDoc.infMunDescarga[i].cMunDescarga, DSC_CMUN);
    if not ValidarMunicipio(MDFe.infDoc.infMunDescarga[i].cMunDescarga) then
      Gerador.wAlerta('#045', 'cMunDescarga', DSC_CMUN, ERR_MSG_INVALIDO);
    Gerador.wCampo(tcStr, '#046', 'xMunDescarga', 02, 60, 1, MDFe.infDoc.infMunDescarga[i].xMunDescarga, DSC_XMUN);

    case MDFe.Ide.tpEmit of
     // Se Tipo de Emitente for Prestador de Serviço de Transporte
     // só pode relacionar os grupos de documentos CT-e e CT
     teTransportadora:
       begin
         for j := 0 to MDFe.infDoc.infMunDescarga[i].infCTe.Count - 1 do
         begin
           Gerador.wGrupo('infCTe', '#048');
           Gerador.wCampo(tcEsp, '#049', 'chCTe      ', 44, 44, 1, OnlyNumber(MDFe.infDoc.infMunDescarga[i].infCTe[j].chCTe), DSC_REFCTE);
           if OnlyNumber(MDFe.infDoc.infMunDescarga[i].infCTe[j].chCTe) <> '' then
            if not ValidarChave(MDFe.infDoc.infMunDescarga[i].infCTe[j].chCTe) then
           Gerador.wAlerta('#049', 'chCTe', DSC_REFCTE, ERR_MSG_INVALIDO);
           Gerador.wCampo(tcStr, '#050', 'SegCodBarra', 36, 36, 0, MDFe.infDoc.infMunDescarga[i].infCTe[j].SegCodBarra, DSC_SEGCODBARRA);

           if VersaoDF >= ve300 then
             Gerador.wCampo(tcStr, '#050', 'indReentrega', 1, 1, 0, MDFe.infDoc.infMunDescarga[i].infCTe[j].indReentrega, DSC_INDREENTREGA);

           for k := 0 to MDFe.infDoc.infMunDescarga[i].infCTe[j].infUnidTransp.Count - 1 do
           begin
             Gerador.wGrupo('infUnidTransp', '#051');
             Gerador.wCampo(tcStr, '#052', 'tpUnidTransp', 01, 01, 1, UnidTranspToStr(MDFe.infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].tpUnidTransp), DSC_TPUNIDTRANSP);
             Gerador.wCampo(tcStr, '#053', 'idUnidTransp', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].idUnidTransp, DSC_IDUNIDTRANSP);

             for l := 0 to MDFe.infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].lacUnidTransp.Count - 1 do
             begin
               Gerador.wGrupo('lacUnidTransp', '#054');
               Gerador.wCampo(tcStr, '#055', 'nLacre', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].lacUnidTransp[l].nLacre, DSC_NLACRE);
               Gerador.wGrupo('/lacUnidTransp');
             end;

             for l := 0 to MDFe.infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].infUnidCarga.Count - 1 do
             begin
               Gerador.wGrupo('infUnidCarga', '#056');
               Gerador.wCampo(tcStr, '#057', 'tpUnidCarga', 01, 01, 1, UnidCargaToStr(MDFe.infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].infUnidCarga[l].tpUnidCarga), DSC_TPUNIDCARGA);
               Gerador.wCampo(tcStr, '#058', 'idUnidCarga', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].infUnidCarga[l].idUnidCarga, DSC_IDUNIDCARGA);

               for m := 0 to MDFe.infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga.Count - 1 do
               begin
                 Gerador.wGrupo('lacUnidCarga', '#059');
                 Gerador.wCampo(tcStr, '#060', 'nLacre', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga[m].nLacre, DSC_NLACRE);
                 Gerador.wGrupo('/lacUnidCarga');
               end;
               Gerador.wCampo(tcDe2, '#061', 'qtdRat', 01, 05, 0, MDFe.infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].infUnidCarga[l].qtdRat, DSC_QTDRAT);

               Gerador.wGrupo('/infUnidCarga');
             end;
             Gerador.wCampo(tcDe2, '#062', 'qtdRat', 01, 05, 0, MDFe.infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].qtdRat, DSC_QTDRAT);

             Gerador.wGrupo('/infUnidTransp');
           end;

           if VersaoDF >= ve300 then
           begin
             for k := 0 to MDFe.infDoc.infMunDescarga[i].infCTe[j].peri.Count - 1 do
             begin
               Gerador.wGrupo('peri', '#89');
               Gerador.wCampo(tcStr, '#90', 'nONU     ', 01,  04, 1, MDFe.infDoc.infMunDescarga[i].infCTe[j].peri.Items[k].nONU, DSC_NONU);
               Gerador.wCampo(tcStr, '#91', 'xNomeAE  ', 01, 150, 1, MDFe.infDoc.infMunDescarga[i].infCTe[j].peri.Items[k].xNomeAE, DSC_XNOMEAE);
               Gerador.wCampo(tcStr, '#92', 'xClaRisco', 01,  40, 1, MDFe.infDoc.infMunDescarga[i].infCTe[j].peri.Items[k].xClaRisco, DSC_XCLARISCO);
               Gerador.wCampo(tcStr, '#93', 'grEmb    ', 01,  06, 0, MDFe.infDoc.infMunDescarga[i].infCTe[j].peri.Items[k].grEmb, DSC_GREMB);
               Gerador.wCampo(tcStr, '#94', 'qTotProd ', 01,  20, 1, MDFe.infDoc.infMunDescarga[i].infCTe[j].peri.Items[k].qTotProd, DSC_QTOTPROD);
               Gerador.wCampo(tcStr, '#95', 'qVolTipo ', 01,  60, 0, MDFe.infDoc.infMunDescarga[i].infCTe[j].peri.Items[k].qVolTipo, DSC_QVOLTIPO);
               Gerador.wGrupo('/peri');
             end;
             if MDFe.infDoc.infMunDescarga[i].infCTe[j].peri.Count > 990 then
               Gerador.wAlerta('#89', 'peri', '', ERR_MSG_MAIOR_MAXIMO + '990');

             if (MDFe.Ide.modal = moAereo) and
                ((MDFe.infDoc.infMunDescarga[i].infCTe[j].infEntregaParcial.qtdTotal <> 0) or
                (MDFe.infDoc.infMunDescarga[i].infCTe[j].infEntregaParcial.qtdParcial <> 0)) then
             begin
               Gerador.wGrupo('infEntregaParcial', '#96');
               Gerador.wCampo(tcDe4, '#97', 'qtdTotal  ', 01, 15, 1, MDFe.infDoc.infMunDescarga[i].infCTe[j].infEntregaParcial.qtdTotal, DSC_QTDTOTAL);
               Gerador.wCampo(tcDe4, '#98', 'qtdParcial', 01, 15, 1, MDFe.infDoc.infMunDescarga[i].infCTe[j].infEntregaParcial.qtdParcial, DSC_QTDPARCIAL);
               Gerador.wGrupo('/infEntregaParcial');
             end;

           end;

           Gerador.wGrupo('/infCTe');
         end;
         if MDFe.infDoc.infMunDescarga[i].infCTe.Count > 20000 then
          Gerador.wAlerta('#048', 'infCTe', '', ERR_MSG_MAIOR_MAXIMO + '20000');

         for j := 0 to MDFe.infDoc.infMunDescarga[i].infCT.Count - 1 do
         begin
           Gerador.wGrupo('infCT', '#051');
           Gerador.wCampo(tcStr, '#052', 'nCT   ', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infCT[j].nCT, DSC_NCT);
           Gerador.wCampo(tcInt, '#053', 'serie ', 01, 03, 1, MDFe.infDoc.infMunDescarga[i].infCT[j].serie, DSC_SERIE);
           Gerador.wCampo(tcInt, '#054', 'subser', 01, 02, 1, MDFe.infDoc.infMunDescarga[i].infCT[j].subser, DSC_SUBSERIE);
           Gerador.wCampo(tcDat, '#055', 'dEmi  ', 10, 10, 1, MDFe.infDoc.infMunDescarga[i].infCT[j].dEmi, DSC_DEMI);
           Gerador.wCampo(tcDe2, '#056', 'vCarga', 01, 15, 1, MDFe.infDoc.infMunDescarga[i].infCT[j].vCarga, DSC_VDOC);

           for k := 0 to MDFe.infDoc.infMunDescarga[i].infCT[j].infUnidTransp.Count - 1 do
           begin
             Gerador.wGrupo('infUnidTransp', '#051');
             Gerador.wCampo(tcStr, '#052', 'tpUnidTransp', 01, 01, 1, UnidTranspToStr(MDFe.infDoc.infMunDescarga[i].infCT[j].infUnidTransp[k].tpUnidTransp), DSC_TPUNIDTRANSP);
             Gerador.wCampo(tcStr, '#053', 'idUnidTransp', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infCT[j].infUnidTransp[k].idUnidTransp, DSC_IDUNIDTRANSP);

             for l := 0 to MDFe.infDoc.infMunDescarga[i].infCT[j].infUnidTransp[k].lacUnidTransp.Count - 1 do
             begin
               Gerador.wGrupo('lacUnidTransp', '#054');
               Gerador.wCampo(tcStr, '#055', 'nLacre', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infCT[j].infUnidTransp[k].lacUnidTransp[l].nLacre, DSC_NLACRE);
               Gerador.wGrupo('/lacUnidTransp');
             end;

             for l := 0 to MDFe.infDoc.infMunDescarga[i].infCT[j].infUnidTransp[k].infUnidCarga.Count - 1 do
             begin
               Gerador.wGrupo('infUnidCarga', '#056');
               Gerador.wCampo(tcStr, '#057', 'tpUnidCarga', 01, 01, 1, UnidCargaToStr(MDFe.infDoc.infMunDescarga[i].infCT[j].infUnidTransp[k].infUnidCarga[l].tpUnidCarga), DSC_TPUNIDCARGA);
               Gerador.wCampo(tcStr, '#058', 'idUnidCarga', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infCT[j].infUnidTransp[k].infUnidCarga[l].idUnidCarga, DSC_IDUNIDCARGA);

               for m := 0 to MDFe.infDoc.infMunDescarga[i].infCT[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga.Count - 1 do
               begin
                 Gerador.wGrupo('lacUnidCarga', '#059');
                 Gerador.wCampo(tcStr, '#060', 'nLacre', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infCT[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga[m].nLacre, DSC_NLACRE);
                 Gerador.wGrupo('/lacUnidCarga');
               end;
               Gerador.wCampo(tcDe2, '#061', 'qtdRat', 01, 05, 0, MDFe.infDoc.infMunDescarga[i].infCT[j].infUnidTransp[k].infUnidCarga[l].qtdRat, DSC_QTDRAT);

               Gerador.wGrupo('/infUnidCarga');
             end;
             Gerador.wCampo(tcDe2, '#062', 'qtdRat', 01, 05, 0, MDFe.infDoc.infMunDescarga[i].infCT[j].infUnidTransp[k].qtdRat, DSC_QTDRAT);

             Gerador.wGrupo('/infUnidTransp');
           end;

           Gerador.wGrupo('/infCT');
         end;
         if MDFe.infDoc.infMunDescarga[i].infCT.Count > 10000 then
          Gerador.wAlerta('#051', 'infCT', '', ERR_MSG_MAIOR_MAXIMO + '10000');
       end;
     // Se Tipo de Emitente for Transporte de Carga Própria
     // só pode relacionar os grupos de documentos NF-e e NT
     // Obs: É considerado Emitente de Transporte de Carga Própria os
     //      Emitentes de NF-e e transportadoras quando estiverem fazendo
     //      transporte de carga própria.
     teTranspCargaPropria, teTranspCTeGlobalizado:
       begin
         for j := 0 to MDFe.infDoc.infMunDescarga[i].infNFe.Count - 1 do
         begin
           Gerador.wGrupo('infNFe', '#057');
           Gerador.wCampo(tcEsp, '#058', 'chNFe      ', 44, 44, 1, OnlyNumber(MDFe.infDoc.infMunDescarga[i].infNFe[j].chNFe), DSC_REFNFE);
           if OnlyNumber(MDFe.infDoc.infMunDescarga[i].infNFe[j].chNFe) <> '' then
            if not ValidarChave(MDFe.infDoc.infMunDescarga[i].infNFe[j].chNFe) then
             Gerador.wAlerta('#058', 'chNFe', DSC_REFNFE, ERR_MSG_INVALIDO);
           Gerador.wCampo(tcStr, '#059', 'SegCodBarra', 36, 36, 0, MDFe.infDoc.infMunDescarga[i].infNFe[j].SegCodBarra, DSC_SEGCODBARRA);

           if VersaoDF >= ve300 then
             Gerador.wCampo(tcStr, '#050', 'indReentrega', 1, 1, 0, MDFe.infDoc.infMunDescarga[i].infNFe[j].indReentrega, DSC_INDREENTREGA);

           for k := 0 to MDFe.infDoc.infMunDescarga[i].infNFe[j].infUnidTransp.Count - 1 do
           begin
             Gerador.wGrupo('infUnidTransp', '#051');
             Gerador.wCampo(tcStr, '#052', 'tpUnidTransp', 01, 01, 1, UnidTranspToStr(MDFe.infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].tpUnidTransp), DSC_TPUNIDTRANSP);
             Gerador.wCampo(tcStr, '#053', 'idUnidTransp', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].idUnidTransp, DSC_IDUNIDTRANSP);

             for l := 0 to MDFe.infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].lacUnidTransp.Count - 1 do
             begin
               Gerador.wGrupo('lacUnidTransp', '#054');
               Gerador.wCampo(tcStr, '#055', 'nLacre', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].lacUnidTransp[l].nLacre, DSC_NLACRE);
               Gerador.wGrupo('/lacUnidTransp');
             end;

             for l := 0 to MDFe.infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].infUnidCarga.Count - 1 do
             begin
               Gerador.wGrupo('infUnidCarga', '#056');
               Gerador.wCampo(tcStr, '#057', 'tpUnidCarga', 01, 01, 1, UnidCargaToStr(MDFe.infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].infUnidCarga[l].tpUnidCarga), DSC_TPUNIDCARGA);
               Gerador.wCampo(tcStr, '#058', 'idUnidCarga', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].infUnidCarga[l].idUnidCarga, DSC_IDUNIDCARGA);

               for m := 0 to MDFe.infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga.Count - 1 do
               begin
                 Gerador.wGrupo('lacUnidCarga', '#059');
                 Gerador.wCampo(tcStr, '#060', 'nLacre', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga[m].nLacre, DSC_NLACRE);
                 Gerador.wGrupo('/lacUnidCarga');
               end;
               Gerador.wCampo(tcDe2, '#061', 'qtdRat', 01, 05, 0, MDFe.infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].infUnidCarga[l].qtdRat, DSC_QTDRAT);

               Gerador.wGrupo('/infUnidCarga');
             end;
             Gerador.wCampo(tcDe2, '#062', 'qtdRat', 01, 05, 0, MDFe.infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].qtdRat, DSC_QTDRAT);

             Gerador.wGrupo('/infUnidTransp');
           end;

           if VersaoDF >= ve300 then
           begin
             for k := 0 to MDFe.infDoc.infMunDescarga[i].infNFe[j].peri.Count - 1 do
             begin
               Gerador.wGrupo('peri', '#89');
               Gerador.wCampo(tcStr, '#90', 'nONU     ', 01,  04, 1, MDFe.infDoc.infMunDescarga[i].infNFe[j].peri.Items[k].nONU, DSC_NONU);
               Gerador.wCampo(tcStr, '#91', 'xNomeAE  ', 01, 150, 1, MDFe.infDoc.infMunDescarga[i].infNFe[j].peri.Items[k].xNomeAE, DSC_XNOMEAE);
               Gerador.wCampo(tcStr, '#92', 'xClaRisco', 01,  40, 1, MDFe.infDoc.infMunDescarga[i].infNFe[j].peri.Items[k].xClaRisco, DSC_XCLARISCO);
               Gerador.wCampo(tcStr, '#93', 'grEmb    ', 01,  06, 0, MDFe.infDoc.infMunDescarga[i].infNFe[j].peri.Items[k].grEmb, DSC_GREMB);
               Gerador.wCampo(tcStr, '#94', 'qTotProd ', 01,  20, 1, MDFe.infDoc.infMunDescarga[i].infNFe[j].peri.Items[k].qTotProd, DSC_QTOTPROD);
               Gerador.wCampo(tcStr, '#95', 'qVolTipo ', 01,  60, 0, MDFe.infDoc.infMunDescarga[i].infNFe[j].peri.Items[k].qVolTipo, DSC_QVOLTIPO);
               Gerador.wGrupo('/peri');
             end;
             if MDFe.infDoc.infMunDescarga[i].infNFe[j].peri.Count > 990 then
               Gerador.wAlerta('#369', 'peri', '', ERR_MSG_MAIOR_MAXIMO + '990');
           end;

           Gerador.wGrupo('/infNFe');
         end;
         if MDFe.infDoc.infMunDescarga[i].infNFe.Count > 20000 then
          Gerador.wAlerta('#057', 'infNFe', '', ERR_MSG_MAIOR_MAXIMO + '20000');

         for j := 0 to MDFe.infDoc.infMunDescarga[i].infNF.Count - 1 do
         begin
           Gerador.wGrupo('infNF', '#060');
           Gerador.wCampoCNPJ('#061', MDFe.infDoc.infMunDescarga[i].infNF[j].CNPJ, CODIGO_BRASIL, True);
           Gerador.wCampo(tcStr, '#062', 'UF   ', 02, 02, 1, MDFe.infDoc.infMunDescarga[i].infNF[j].UF, DSC_IE);
           Gerador.wCampo(tcStr, '#063', 'nNF  ', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infNF[j].nNF, DSC_NDF);
           Gerador.wCampo(tcInt, '#064', 'serie', 01, 03, 1, MDFe.infDoc.infMunDescarga[i].infNF[j].serie, DSC_SERIE);
           Gerador.wCampo(tcDat, '#065', 'dEmi ', 10, 10, 1, MDFe.infDoc.infMunDescarga[i].infNF[j].dEmi, DSC_DEMI);
           Gerador.wCampo(tcDe2, '#066', 'vNF  ', 01, 15, 1, MDFe.infDoc.infMunDescarga[i].infNF[j].vNF, DSC_VDOC);
           Gerador.wCampo(tcInt, '#067', 'PIN  ', 02, 09, 0, MDFe.infDoc.infMunDescarga[i].infNF[j].PIN, DSC_PIN);

           for k := 0 to MDFe.infDoc.infMunDescarga[i].infNF[j].infUnidTransp.Count - 1 do
           begin
             Gerador.wGrupo('infUnidTransp', '#051');
             Gerador.wCampo(tcStr, '#052', 'tpUnidTransp', 01, 01, 1, UnidTranspToStr(MDFe.infDoc.infMunDescarga[i].infNF[j].infUnidTransp[k].tpUnidTransp), DSC_TPUNIDTRANSP);
             Gerador.wCampo(tcStr, '#053', 'idUnidTransp', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infNF[j].infUnidTransp[k].idUnidTransp, DSC_IDUNIDTRANSP);

             for l := 0 to MDFe.infDoc.infMunDescarga[i].infNF[j].infUnidTransp[k].lacUnidTransp.Count - 1 do
             begin
               Gerador.wGrupo('lacUnidTransp', '#054');
               Gerador.wCampo(tcStr, '#055', 'nLacre', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infNF[j].infUnidTransp[k].lacUnidTransp[l].nLacre, DSC_NLACRE);
               Gerador.wGrupo('/lacUnidTransp');
             end;

             for l := 0 to MDFe.infDoc.infMunDescarga[i].infNF[j].infUnidTransp[k].infUnidCarga.Count - 1 do
             begin
               Gerador.wGrupo('infUnidCarga', '#056');
               Gerador.wCampo(tcStr, '#057', 'tpUnidCarga', 01, 01, 1, UnidCargaToStr(MDFe.infDoc.infMunDescarga[i].infNF[j].infUnidTransp[k].infUnidCarga[l].tpUnidCarga), DSC_TPUNIDCARGA);
               Gerador.wCampo(tcStr, '#058', 'idUnidCarga', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infNF[j].infUnidTransp[k].infUnidCarga[l].idUnidCarga, DSC_IDUNIDCARGA);

               for m := 0 to MDFe.infDoc.infMunDescarga[i].infNF[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga.Count - 1 do
               begin
                 Gerador.wGrupo('lacUnidCarga', '#059');
                 Gerador.wCampo(tcStr, '#060', 'nLacre', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infNF[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga[m].nLacre, DSC_NLACRE);
                 Gerador.wGrupo('/lacUnidCarga');
               end;
               Gerador.wCampo(tcDe2, '#061', 'qtdRat', 01, 05, 0, MDFe.infDoc.infMunDescarga[i].infNF[j].infUnidTransp[k].infUnidCarga[l].qtdRat, DSC_QTDRAT);

               Gerador.wGrupo('/infUnidCarga');
             end;
             Gerador.wCampo(tcDe2, '#062', 'qtdRat', 01, 05, 0, MDFe.infDoc.infMunDescarga[i].infNF[j].infUnidTransp[k].qtdRat, DSC_QTDRAT);

             Gerador.wGrupo('/infUnidTransp');
           end;

           Gerador.wGrupo('/infNF');
         end;
         if MDFe.infDoc.infMunDescarga[i].infNF.Count > 10000 then
          Gerador.wAlerta('#060', 'infNF', '', ERR_MSG_MAIOR_MAXIMO + '10000');
       end;
    end;

    if MDFe.Ide.modal = moAquaviario
     then begin
       for j := 0 to MDFe.infDoc.infMunDescarga[i].infMDFeTransp.Count - 1 do
       begin
         Gerador.wGrupo('infMDFeTransp', '#057');
         Gerador.wCampo(tcEsp, '#058', 'chMDFe      ', 44, 44, 1, OnlyNumber(MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].chMDFe), DSC_REFNFE);
         if OnlyNumber(MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].chMDFe) <> '' then
          if not ValidarChave(MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].chMDFe) then
           Gerador.wAlerta('#058', 'chMDFe', DSC_REFNFE, ERR_MSG_INVALIDO);

         if VersaoDF >= ve300 then
           Gerador.wCampo(tcStr, '#050', 'indReentrega', 1, 1, 0, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].indReentrega, DSC_INDREENTREGA);

         for k := 0 to MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp.Count - 1 do
         begin
           Gerador.wGrupo('infUnidTransp', '#051');
           Gerador.wCampo(tcStr, '#052', 'tpUnidTransp', 01, 01, 1, UnidTranspToStr(MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].tpUnidTransp), DSC_TPUNIDTRANSP);
           Gerador.wCampo(tcStr, '#053', 'idUnidTransp', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].idUnidTransp, DSC_IDUNIDTRANSP);

           for l := 0 to MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].lacUnidTransp.Count - 1 do
           begin
             Gerador.wGrupo('lacUnidTransp', '#054');
             Gerador.wCampo(tcStr, '#055', 'nLacre', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].lacUnidTransp[l].nLacre, DSC_NLACRE);
             Gerador.wGrupo('/lacUnidTransp');
           end;

           for l := 0 to MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].infUnidCarga.Count - 1 do
           begin
             Gerador.wGrupo('infUnidCarga', '#056');
             Gerador.wCampo(tcStr, '#057', 'tpUnidCarga', 01, 01, 1, UnidCargaToStr(MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].infUnidCarga[l].tpUnidCarga), DSC_TPUNIDCARGA);
             Gerador.wCampo(tcStr, '#058', 'idUnidCarga', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].infUnidCarga[l].idUnidCarga, DSC_IDUNIDCARGA);

             for m := 0 to MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga.Count - 1 do
             begin
               Gerador.wGrupo('lacUnidCarga', '#059');
               Gerador.wCampo(tcStr, '#060', 'nLacre', 01, 20, 1, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga[m].nLacre, DSC_NLACRE);
               Gerador.wGrupo('/lacUnidCarga');
             end;
             Gerador.wCampo(tcDe2, '#061', 'qtdRat', 01, 05, 0, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].infUnidCarga[l].qtdRat, DSC_QTDRAT);

             Gerador.wGrupo('/infUnidCarga');
           end;
           Gerador.wCampo(tcDe2, '#062', 'qtdRat', 01, 05, 0, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].qtdRat, DSC_QTDRAT);

           Gerador.wGrupo('/infUnidTransp');
         end;

         if VersaoDF >= ve300 then
         begin
           for k := 0 to MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].peri.Count - 1 do
           begin
             Gerador.wGrupo('peri', '#89');
             Gerador.wCampo(tcStr, '#90', 'nONU     ', 01,  04, 1, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].peri.Items[k].nONU, DSC_NONU);
             Gerador.wCampo(tcStr, '#91', 'xNomeAE  ', 01, 150, 1, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].peri.Items[k].xNomeAE, DSC_XNOMEAE);
             Gerador.wCampo(tcStr, '#92', 'xClaRisco', 01,  40, 1, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].peri.Items[k].xClaRisco, DSC_XCLARISCO);
             Gerador.wCampo(tcStr, '#93', 'grEmb    ', 01,  06, 0, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].peri.Items[k].grEmb, DSC_GREMB);
             Gerador.wCampo(tcStr, '#94', 'qTotProd ', 01,  20, 1, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].peri.Items[k].qTotProd, DSC_QTOTPROD);
             Gerador.wCampo(tcStr, '#95', 'qVolTipo ', 01,  60, 0, MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].peri.Items[k].qVolTipo, DSC_QVOLTIPO);
             Gerador.wGrupo('/peri');
           end;
           if MDFe.infDoc.infMunDescarga[i].infMDFeTransp[j].peri.Count > 990 then
             Gerador.wAlerta('#369', 'peri', '', ERR_MSG_MAIOR_MAXIMO + '990');
         end;

         Gerador.wGrupo('/infMDFeTransp');
       end;
       if MDFe.infDoc.infMunDescarga[i].infMDFeTransp.Count > 20000 then
        Gerador.wAlerta('#057', 'infMDFeTransp', '', ERR_MSG_MAIOR_MAXIMO + '20000');
     end;

    Gerador.wGrupo('/infMunDescarga');
  end;
  
  if MDFe.infDoc.infMunDescarga.Count > 1000 then
   Gerador.wAlerta('#045', 'infMunDescarga', '', ERR_MSG_MAIOR_MAXIMO + '1000');

  Gerador.wGrupo('/infDoc');
end;

procedure TMDFeW.GerarInfSeg;
var
  i, k: Integer;
begin
  for i := 0 to MDFe.seg.Count - 1 do
  begin
    Gerador.wGrupo('seg', '#118');

    Gerador.wGrupo('infResp', '#119');
    Gerador.wCampo(tcStr, '#120', 'respSeg', 01, 01, 1, RspSeguroMDFeToStr(MDFe.seg[i].respSeg), DSC_RESPSEG);

    if MDFe.seg[i].respSeg = rsTomadorServico then
      Gerador.wCampoCNPJCPF('#121', '#122', MDFe.seg[i].CNPJCPF, False);

    Gerador.wGrupo('/infResp');

    if (MDFe.Ide.tpEmit in [teTransportadora, teTranspCTeGlobalizado]) or
       ((MDFe.seg[i].xSeg <> '') and (MDFe.seg[i].CNPJ <> '')) then
    begin
      Gerador.wGrupo('infSeg', '#123');
      Gerador.wCampo(tcStr, '#124', 'xSeg', 01, 30, 1, MDFe.seg[i].xSeg, DSC_XSEG);
      Gerador.wCampoCNPJ('#125', MDFe.seg[i].CNPJ, CODIGO_BRASIL, True);
      Gerador.wGrupo('/infSeg');
    end;

    Gerador.wCampo(tcStr, '#126', 'nApol', 01, 20, 0, MDFe.seg[i].nApol, DSC_NAPOL);

    for k := 0 to MDFe.seg[i].aver.Count - 1 do
      Gerador.wCampo(tcStr, '#127', 'nAver', 01, 40, 0, MDFe.seg[i].aver[k].nAver, DSC_NAVER);
    if MDFe.seg[i].aver.Count > 990 then
      Gerador.wAlerta('#369', 'nAver', '', ERR_MSG_MAIOR_MAXIMO + '990');

    Gerador.wGrupo('/seg');
  end;
  if MDFe.seg.Count > 990 then
    Gerador.wAlerta('#118', 'seg', DSC_INFSEG, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TMDFeW.GerarTot;
begin
  Gerador.wGrupo('tot', '#068');
  Gerador.wCampo(tcInt, '#069', 'qCTe  ', 01, 04, 0, MDFe.tot.qCTe, DSC_QCTE);
  Gerador.wCampo(tcInt, '#070', 'qCT   ', 01, 04, 0, MDFe.tot.qCT, DSC_QCT);
  Gerador.wCampo(tcInt, '#071', 'qNFe  ', 01, 04, 0, MDFe.tot.qNFe, DSC_QNFE);
  Gerador.wCampo(tcInt, '#072', 'qNF   ', 01, 04, 0, MDFe.tot.qNF, DSC_QNF);
  Gerador.wCampo(tcInt, '#072', 'qMDFe ', 01, 04, 0, MDFe.tot.qMDFe, DSC_QNF);
  Gerador.wCampo(tcDe2, '#073', 'vCarga', 01, 15, 1, MDFe.tot.vCarga, DSC_VDOC);
  Gerador.wCampo(tcStr, '#074', 'cUnid ', 02, 02, 1, UnidMedToStr(MDFe.tot.cUnid), DSC_CUNID);
  Gerador.wCampo(tcDe4, '#075', 'qCarga', 01, 15, 1, MDFe.tot.qCarga, DSC_QCARGA);
  Gerador.wGrupo('/tot');
end;

procedure TMDFeW.GerarLacres;
var
  i: Integer;
begin
  for i := 0 to MDFe.lacres.Count - 1 do
  begin
    Gerador.wGrupo('lacres', '#076');
    Gerador.wCampo(tcStr, '#077', 'nLacre', 01, 60, 1, MDFe.lacres[i].nLacre, DSC_NLACRE);
    Gerador.wGrupo('/lacres');
  end;
  if MDFe.lacres.Count > 990 then
    Gerador.wAlerta('#076', 'lacres', DSC_LACR, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TMDFeW.GerarautXML;
var
  i: Integer;
begin
  for i := 0 to MDFe.autXML.Count - 1 do
  begin
    Gerador.wGrupo('autXML', '#140');
    Gerador.wCampoCNPJCPF('#141', '#142', MDFe.autXML[i].CNPJCPF);
    Gerador.wGrupo('/autXML');
  end;
  if MDFe.autXML.Count > 10 then
    Gerador.wAlerta('#140', 'autXML', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TMDFeW.GerarInfAdic;
begin
  if (MDFe.infAdic.infAdFisco <> '') or (MDFe.infAdic.infCpl <> '')
   then begin
    Gerador.wGrupo('infAdic', '#078');
    Gerador.wCampo(tcStr, '#079', 'infAdFisco', 01, 2000, 0, MDFe.infAdic.infAdFisco, DSC_INFADFISCO);
    Gerador.wCampo(tcStr, '#080', 'infCpl    ', 01, 2000, 0, MDFe.infAdic.infCpl, DSC_INFCPL);
    Gerador.wGrupo('/infAdic');
   end;
end;

procedure TMDFeW.GerarinfRespTec;
begin
  if (MDFe.infRespTec.CNPJ <> '') then
  begin
    Gerador.wGrupo('infRespTec', '#081');
    Gerador.wCampoCNPJ('#82', MDFe.infRespTec.CNPJ, CODIGO_BRASIL, True);
    Gerador.wCampo(tcStr, '#083', 'xContato', 02, 60, 1, MDFe.infRespTec.xContato, DSC_XCONTATO);
    Gerador.wCampo(tcStr, '#084', 'email   ', 06, 60, 1, MDFe.infRespTec.email, DSC_EMAIL);
    Gerador.wCampo(tcStr, '#085', 'fone    ', 07, 12, 1, MDFe.infRespTec.fone, DSC_FONE);

    if (idCSRT <> 0) and (CSRT <> '') then
    begin
      Gerador.wCampo(tcInt, '#086', 'idCSRT  ', 03, 03, 1, idCSRT, DSC_IDCSRT);
      Gerador.wCampo(tcStr, '#087', 'hashCSRT', 28, 28, 1, CalcularHashCSRT(CSRT, FChaveMDFe), DSC_HASHCSRT);
    end;

    Gerador.wGrupo('/infRespTec');
   end;
end;

procedure TMDFeW.AjustarMunicipioUF(var xUF, xMun: string;
  var cMun: Integer; cPais: Integer; const vxUF, vxMun: string; vcMun: Integer);
var
  PaisBrasil: boolean;
begin
  PaisBrasil := cPais = CODIGO_BRASIL;

  cMun := IfThen(PaisBrasil, vcMun, CMUN_EXTERIOR);
  xMun := IfThen(PaisBrasil, vxMun, XMUN_EXTERIOR);
  xUF  := IfThen(PaisBrasil, vxUF, UF_EXTERIOR);

  if FOpcoes.NormatizarMunicipios then
    if ( ( EstaZerado(cMun)) and (xMun <> XMUN_EXTERIOR) ) then
      cMun := ObterCodigoMunicipio(xMun, xUF, FOpcoes.FPathArquivoMunicipios)
    else if ( ( EstaVazio(xMun)) and (cMun <> CMUN_EXTERIOR) ) then
      xMun := ObterNomeMunicipio(cMun, xUF, FOpcoes.FPathArquivoMunicipios);

end;

procedure TMDFeW.GerarProdPred;
begin
  if (MDFe.prodPred.xProd <> '') then
  begin
    Gerador.wGrupo('prodPred', '#');
    Gerador.wCampo(tcStr, '#', 'tpCarga', 02, 002, 1, TCargaToStr(MDFe.prodPred.tpCarga), DSC_TPCARGA);
    Gerador.wCampo(tcStr, '#', 'xProd  ', 01, 120, 1, MDFe.prodPred.xProd, DSC_XPROD);
    Gerador.wCampo(tcStr, '#', 'cEAN   ', 00, 014, 0, MDFe.prodPred.cEAN, DSC_CEAN);
    Gerador.wCampo(tcStr, '#', 'NCM    ', 02, 008, 0, MDFe.prodPred.NCM, DSC_NCM);

    if (MDFe.prodPred.infLocalCarrega.CEP > 0) or
       (MDFe.prodPred.infLocalCarrega.latitude <> 0) or (MDFe.prodPred.infLocalCarrega.Longitude <> 0) or
       (MDFe.prodPred.infLocalDescarrega.CEP > 0) or
       (MDFe.prodPred.infLocalDescarrega.latitude <> 0) or (MDFe.prodPred.infLocalDescarrega.Longitude <> 0) then
    begin
      Gerador.wGrupo('infLotacao', '#');

      Gerador.wGrupo('infLocalCarrega', '#');

      if (MDFe.prodPred.infLocalCarrega.CEP > 0) then
        Gerador.wCampo(tcInt, '#', 'CEP', 08, 08, 1, MDFe.prodPred.infLocalCarrega.CEP, DSC_CEP)
      else
        if (MDFe.prodPred.infLocalCarrega.latitude <> 0) or (MDFe.prodPred.infLocalCarrega.Longitude <> 0) then
        begin
          Gerador.wCampo(tcDe6, '#', 'latitude ', 01, 10, 1, MDFe.prodPred.infLocalCarrega.latitude, DSC_LATITUDE);
          Gerador.wCampo(tcDe6, '#', 'longitude', 01, 11, 1, MDFe.prodPred.infLocalCarrega.Longitude, DSC_LONGITUDE);
        end;

      Gerador.wGrupo('/infLocalCarrega');

      Gerador.wGrupo('infLocalDescarrega', '#');

      if (MDFe.prodPred.infLocalDescarrega.CEP > 0) then
        Gerador.wCampo(tcInt, '#', 'CEP', 08, 08, 1, MDFe.prodPred.infLocalDescarrega.CEP, DSC_CEP)
      else
        if (MDFe.prodPred.infLocalDescarrega.latitude <> 0) or (MDFe.prodPred.infLocalDescarrega.Longitude <> 0) then
        begin
          Gerador.wCampo(tcDe6, '#', 'latitude ', 01, 10, 1, MDFe.prodPred.infLocalDescarrega.latitude, DSC_LATITUDE);
          Gerador.wCampo(tcDe6, '#', 'longitude', 01, 11, 1, MDFe.prodPred.infLocalDescarrega.Longitude, DSC_LONGITUDE);
        end;

      Gerador.wGrupo('/infLocalDescarrega');

      Gerador.wGrupo('/infLotacao');
    end;

    Gerador.wGrupo('/prodPred');
   end;
end;

procedure TMDFeW.GerarInfPag;
var
  i, j: Integer;
begin
  for i := 0 to MDFe.rodo.infANTT.infPag.Count - 1 do
  begin
    Gerador.wGrupo('infPag', '#');

    Gerador.wCampo(tcStr, '#', 'xNome', 02, 60, 0, MDFe.rodo.infANTT.infPag[i].xNome, DSC_XNOME);

    if MDFe.rodo.infANTT.infPag[i].idEstrangeiro <> '' then
      Gerador.wCampo(tcStr, '#', 'idEstrangeiro', 02, 20, 0, MDFe.rodo.infANTT.infPag[i].idEstrangeiro, DSC_IDESTRANGEIRO)
    else
      Gerador.wCampoCNPJCPF('#', '#', MDFe.rodo.infANTT.infPag[i].CNPJCPF);

    // Componentes de Pagamento do Frete
    for j := 0 to MDFe.rodo.infANTT.infPag[i].Comp.Count - 1 do
    begin
      Gerador.wGrupo('Comp', '#');
      Gerador.wCampo(tcStr, '#', 'tpComp', 02, 02, 1, TCompToStr(MDFe.rodo.infANTT.infPag[i].Comp[j].tpComp), DSC_TPCOMP);
      Gerador.wCampo(tcDe2, '#', 'vComp ', 01, 15, 1, MDFe.rodo.infANTT.infPag[i].Comp[j].vComp, DSC_VCOMP);
      Gerador.wCampo(tcStr, '#', 'xComp ', 02, 60, 0, MDFe.rodo.infANTT.infPag[i].Comp[j].xComp, DSC_XCOMP);
      Gerador.wGrupo('/Comp');
    end;

    if MDFe.rodo.infANTT.infPag[i].Comp.Count > 990 then
      Gerador.wAlerta('#', 'Comp', '', ERR_MSG_MAIOR_MAXIMO + '990');

    Gerador.wCampo(tcDe2, '#', 'vContrato    ', 01, 15, 1, MDFe.rodo.infANTT.infPag[i].vContrato, DSC_VCONTRATO);
    Gerador.wCampo(tcStr, '#', 'indAltoDesemp', 01, 01, 0, indAltoDesempToStr(MDFe.rodo.infANTT.infPag[i].indAltoDesemp), '');
    Gerador.wCampo(tcStr, '#', 'indPag       ', 01, 01, 1, TIndPagToStr(MDFe.rodo.infANTT.infPag[i].indPag), DSC_INDPAG);
    Gerador.wCampo(tcDe2, '#', 'vAdiant      ', 01, 15, 0, MDFe.rodo.infANTT.infPag[i].vAdiant, DSC_VADIANT);

    if MDFe.rodo.infANTT.infPag[i].indAntecipaAdiant = tiSim then
      Gerador.wCampo(tcStr, '#', 'indAntecipaAdiant', 1, 1, 1, '1');

    // Informações do pagamento a prazo. Obs: Informar somente se indPag for à Prazo
    if MDFe.rodo.infANTT.infPag[i].indPag = ipPrazo then
    begin
      for j := 0 to MDFe.rodo.infANTT.infPag[i].infPrazo.Count - 1 do
      begin
        Gerador.wGrupo('infPrazo', '#');
        Gerador.wCampo(tcStr, '#', 'nParcela', 03, 03, 1, FormatFloat('000', MDFe.rodo.infANTT.infPag[i].infPrazo[j].nParcela), DSC_NPARCELA);
        Gerador.wCampo(tcDat, '#', 'dVenc   ', 10, 10, 1, MDFe.rodo.infANTT.infPag[i].infPrazo[j].dVenc, DSC_DVENC);
        Gerador.wCampo(tcDe2, '#', 'vParcela', 01, 15, 1, MDFe.rodo.infANTT.infPag[i].infPrazo[j].vParcela, DSC_VPARCELA);
        Gerador.wGrupo('/infPrazo');
      end;

      if MDFe.rodo.infANTT.infPag[i].infPrazo.Count > 990 then
        Gerador.wAlerta('#', 'infPrazo', '', ERR_MSG_MAIOR_MAXIMO + '990');
    end;

    Gerador.wCampo(tcStr, '#', 'tpAntecip', 1, 1, 0, tpAntecipToStr(MDFe.rodo.infANTT.infPag[i].tpAntecip), DSC_TPANTECIP);

    Gerador.wGrupo('infBanc', '#');

    if MDFe.rodo.infANTT.infPag[i].infBanc.PIX <> '' then
      Gerador.wCampo(tcStr, '#', 'PIX', 2, 60, 1, MDFe.rodo.infANTT.infPag[i].infBanc.PIX, DSC_PIX)
    else
    begin
      if MDFe.rodo.infANTT.infPag[i].infBanc.CNPJIPEF <> '' then
        Gerador.wCampo(tcStr, '#', 'CNPJIPEF', 14, 14, 1, MDFe.rodo.infANTT.infPag[i].infBanc.CNPJIPEF, DSC_CNPJIPEF)
      else
      begin
        Gerador.wCampo(tcStr, '#', 'codBanco  ', 3, 05, 1, MDFe.rodo.infANTT.infPag[i].infBanc.codBanco, DSC_CODBANCO);
        Gerador.wCampo(tcStr, '#', 'codAgencia', 1, 10, 1, MDFe.rodo.infANTT.infPag[i].infBanc.codAgencia, DSC_CODAGENCIA);
      end;
    end;

    Gerador.wGrupo('/infBanc');

    Gerador.wGrupo('/infPag');
  end;

  if MDFe.rodo.infANTT.infPag.Count > 990 then
    Gerador.wAlerta('#', 'infPag', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

end.
