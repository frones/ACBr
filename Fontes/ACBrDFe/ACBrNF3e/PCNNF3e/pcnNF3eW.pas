{******************************************************************************}
{ Projeto: Componente ACBrNF3e                                                 }
{  Nota Fiscal de Energia Eletrica Eletrônica - NF3e                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2019                                        }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{*******************************************************************************
|* Historico
|*
|* 18/12/2019: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pcnNF3eW;

interface

uses
  SysUtils, Classes,
  pcnGerador, pcnNF3e, pcnConversao, pcnNF3eConsts;

type

  { TGeradorOpcoes }

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
    property GerarTagIPIparaNaoTributado: Boolean  read FGerarTagIPIparaNaoTributado;
    property GerarTXTSimultaneamente: Boolean      read FGerarTXTSimultaneamente write FGerarTXTSimultaneamente;
    property NormatizarMunicipios: Boolean         read FNormatizarMunicipios    write FNormatizarMunicipios;
    property GerarTagAssinatura: TpcnTagAssinatura read FGerarTagAssinatura      write FGerarTagAssinatura;
    property PathArquivoMunicipios: String         read FPathArquivoMunicipios   write FPathArquivoMunicipios;
    property ValidarInscricoes: Boolean            read FValidarInscricoes;
    property ValidarListaServicos: Boolean         read FValidarListaServicos;
  end;

  { TNF3eW }

  TNF3eW = class(TPersistent)
  private
    FGerador: TGerador;
    FNF3e: TNF3e;
    FOpcoes: TGeradorOpcoes;

    FVersao: String;
    FChaveNF3e: string;
    FIdCSRT: Integer;
    FCSRT: String;

    procedure GerarInfNF3e;
    procedure GerarIde;
    procedure GerarEmit;
    procedure GerarEnderEmit;
    procedure GerarDest;
    procedure GerarEnderDest(var UF: String);
    procedure GerarAcessante;
    procedure GerarSub;
    procedure GerarJudic;
    procedure GerarGrContrat;
    procedure GerarMed;
    procedure GerarSCEE;
    procedure GerarNFDet;
    procedure GerarDet(const i: Integer);
    procedure GerarDetItemAnt(const i, j: Integer);
    procedure GerarDetItem(const i, j: Integer);
    procedure GerargTarif(const i, j: Integer);
    procedure GerargAdBand(const i, j: Integer);
    procedure GerarDetProd(const i, j: Integer);
    procedure GerargMedicao(const i, j: Integer);
    procedure GerarImposto(const i, j: Integer);
    procedure GerarICMS(const i, j: Integer);
    procedure GerarPIS(const i, j: Integer);
    procedure GerarCOFINS(const i, j: Integer);
    procedure GerargProcRef(const i, j: Integer);
    procedure GerargProc(const i, j: Integer);
    procedure GerargContab(const i, j: Integer);
    procedure GerarTotal;
    procedure GerargFat;
    procedure GerarEnderCorresp;
    procedure GerargANEEL;
    procedure GerarautXML;
    procedure GerarInfAdic;
    procedure GerarinfRespTec;

    procedure AjustarMunicipioUF(out xUF: String; out xMun: String; out cMun: Integer;
      cPais: Integer; const vxUF, vxMun: String; vcMun: Integer);

  public
    constructor Create(AOwner: TNF3e);
    destructor Destroy; override;

    function GerarXml: Boolean;
    function ObterNomeArquivo: String;
  published
    property Gerador: TGerador      read FGerador write FGerador;
    property NF3e: TNF3e            read FNF3e    write FNF3e;
    property Opcoes: TGeradorOpcoes read FOpcoes  write FOpcoes;
    property IdCSRT: Integer        read FIdCSRT  write FIdCSRT;
    property CSRT: String           read FCSRT    write FCSRT;
  end;

implementation

uses
  pcnConversaoNF3e, pcnAuxiliar,
  ACBrDFeUtil, pcnConsts, ACBrUtil;

{ TNF3eW }

constructor TNF3eW.Create(AOwner: TNF3e);
begin
  inherited Create;

  FNF3e                                := AOwner;
  FGerador                             := TGerador.Create;
  FGerador.FIgnorarTagNivel            := '|?xml version|NF3e xmlns|infNF3e versao|obsCont|obsFisco|';
  FOpcoes                              := TGeradorOpcoes.Create;
  FOpcoes.FGerarTXTSimultaneamente     := False;
  FOpcoes.FGerarTagIPIparaNaoTributado := True;
  FOpcoes.FNormatizarMunicipios        := False;
  FOpcoes.FPathArquivoMunicipios       := '';
  FOpcoes.FGerarTagAssinatura          := taSomenteSeAssinada;
  FOpcoes.FValidarInscricoes           := False;
  FOpcoes.FValidarListaServicos        := False;
end;

destructor TNF3eW.Destroy;
begin
  FGerador.Free;
  FOpcoes.Free;

  inherited Destroy;
end;

function TNF3eW.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NF3e.infNF3e.ID) + '-NF3e.xml';
end;

function TNF3eW.GerarXml: Boolean;
var
  Gerar: Boolean;
  xProtNF3e : String;
  xCNPJCPF : string;
begin
  Gerador.ListaDeAlertas.Clear;

  FVersao  := Copy(NF3e.infNF3e.VersaoStr, 9, 4);
  xCNPJCPF := NF3e.emit.CNPJ;

  FChaveNF3e := GerarChaveAcesso(NF3e.ide.cUF, NF3e.ide.dhEmi, xCNPJCPF, NF3e.ide.serie,
                            NF3e.ide.nNF, StrToInt(TpEmisToStr(NF3e.ide.tpEmis)),
                            NF3e.ide.cNF, NF3e.ide.modelo);

  NF3e.infNF3e.ID := 'NF3e' + FChaveNF3e;

  NF3e.ide.cDV := ExtrairDigitoChaveAcesso(NF3e.infNF3e.ID);
  NF3e.Ide.cNF := ExtrairCodigoChaveAcesso(NF3e.infNF3e.ID);

  Gerador.LayoutArquivoTXT.Clear;
  Gerador.ArquivoFormatoXML := '';
  Gerador.ArquivoFormatoTXT := '';

  {$IfDef FPC}
   Gerador.wGrupo(ENCODING_UTF8, '', False);
  {$EndIf}

  if NF3e.procNF3e.nProt <> '' then
    Gerador.wGrupo('nf3eProc ' + NF3e.infNF3e.VersaoStr + ' ' + NAME_SPACE_NF3e, '');

  Gerador.wGrupo('NF3e ' + NAME_SPACE_NF3e);
  Gerador.wGrupo('infNF3e ' + NF3e.infNF3e.VersaoStr + ' Id="NF3e' + NF3e.infNF3e.ID + '"');

  GerarInfNF3e;

  Gerador.wGrupo('/infNF3e');

  if NF3e.infNF3eSupl.qrCodNF3e <> '' then
  begin
    Gerador.wGrupo('infNF3eSupl', '#289');
    Gerador.wCampo(tcStr, '#290', 'qrCodNF3e', 50, 1000, 1,
                     '<![CDATA[' + NF3e.infNF3eSupl.qrCodNF3e + ']]>', DSC_INFQRCODE,False);
    Gerador.wGrupo('/infNF3eSupl');
  end;

  if FOpcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := true;
    if FOpcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((NF3e.signature.DigestValue <> '') and (NF3e.signature.SignatureValue <> '') and (NF3e.signature.X509Certificate <> ''));

    if FOpcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
      Gerar := ((NF3e.signature.DigestValue = '') and (NF3e.signature.SignatureValue = '') and (NF3e.signature.X509Certificate = ''));

    if Gerar then
    begin
      FNF3e.signature.URI := '#NF3e' + NF3e.infNF3e.ID;
      FNF3e.signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
      FNF3e.signature.GerarXML;
      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + FNF3e.signature.Gerador.ArquivoFormatoXML;
    end;
  end;

  Gerador.wGrupo('/NF3e');

  if NF3e.procNF3e.nProt <> '' then
   begin
     xProtNF3e :=
       (**)'<protNF3e ' + NF3e.infNF3e.VersaoStr + '>' +
     (******)'<infProt>'+
     (*********)'<tpAmb>'+TpAmbToStr(NF3e.procNF3e.tpAmb)+'</tpAmb>'+
     (*********)'<verAplic>'+NF3e.procNF3e.verAplic+'</verAplic>'+
     (*********)'<chNF3e>'+NF3e.procNF3e.chNF3e+'</chNF3e>'+
     (*********)'<dhRecbto>'+FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',NF3e.procNF3e.dhRecbto) +
                             GetUTC(CodigoParaUF(FNF3e.Ide.cUF), NF3e.procNF3e.dhRecbto)+'</dhRecbto>'+
     (*********)'<nProt>'+NF3e.procNF3e.nProt+'</nProt>'+
     (*********)'<digVal>'+NF3e.procNF3e.digVal+'</digVal>'+
     (*********)'<cStat>'+IntToStr(NF3e.procNF3e.cStat)+'</cStat>'+
     (*********)'<xMotivo>'+NF3e.procNF3e.xMotivo+'</xMotivo>'+
                IIF( (NF3e.procNF3e.cMsg > 0) or (NF3e.procNF3e.xMsg <> ''),
         (*********)'<infFisco><cMsg>'+IntToStr(NF3e.procNF3e.cMsg)+'</cMsg>'+
         (*********)'<xMsg>'+NF3e.procNF3e.xMsg+'</xMsg></infFisco>',
                    '') +
     (******)'</infProt>'+
     (****)'</protNF3e>';

     (**)Gerador.wTexto(xProtNF3e);
     Gerador.wGrupo('/nf3eProc');
   end;

  Gerador.gtAjustarRegistros(NF3e.infNF3e.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TNF3eW.GerarInfNF3e;
begin
  GerarIde;
  GerarEmit;
  GerarDest;
  GerarAcessante;
  GerarSub;
  GerarJudic;
  GerarGrContrat;
  GerarMed;
  GerarSCEE;
  GerarNFDet;
  GerarTotal;
  GerargFat;
  GerargANEEL;
  GerarautXML;
  GerarInfAdic;
  GerarinfRespTec;
end;

procedure TNF3eW.GerarIde;
begin
  Gerador.wGrupo('ide', '#004');
  Gerador.wCampo(tcInt, '#005', 'cUF', 2, 2, 1, NF3e.ide.cUF, DSC_CUF);

  if not ValidarCodigoUF(NF3e.ide.cUF) then
    Gerador.wAlerta('#005', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#006', 'tpAmb ', 01, 01, 1, tpAmbToStr(NF3e.Ide.tpAmb), DSC_TPAMB);
  Gerador.wCampo(tcInt, '#007', 'mod   ', 02, 02, 1, NF3e.ide.modelo, DSC_MOD);
  Gerador.wCampo(tcInt, '#008', 'serie ', 01, 03, 1, NF3e.ide.serie, DSC_SERIE);
  Gerador.wCampo(tcInt, '#009', 'nNF   ', 01, 09, 1, NF3e.ide.nNF, DSC_NNF);
  Gerador.wCampo(tcStr, '#010', 'cNF   ', 08, 08, 1, IntToStrZero(ExtrairCodigoChaveAcesso(NF3e.infNF3e.ID), 8), DSC_CNF);
  Gerador.wCampo(tcInt, '#011', 'cDV   ', 01, 01, 1, NF3e.Ide.cDV, DSC_CDV);
  Gerador.wCampo(tcStr, '#012', 'dhEmi ', 25, 25, 1, DateTimeTodh(NF3e.ide.dhEmi) +
                    GetUTC(CodigoParaUF(NF3e.ide.cUF), NF3e.ide.dhEmi), DSC_DEMI);
  Gerador.wCampo(tcStr, '#013', 'tpEmis', 01, 01, 1, tpEmisToStr(NF3e.Ide.tpEmis), DSC_TPEMIS);
  Gerador.wCampo(tcInt, '#014', 'cMunFG', 07, 07, 1, NF3e.ide.cMunFG, DSC_CMUNFG);

  if not ValidarMunicipio(NF3e.ide.cMunFG) then
    Gerador.wAlerta('#014', 'cMunFG', DSC_CMUNFG, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#015', 'finNF3e', 1, 01, 1, finNF3eToStr(NF3e.Ide.finNF3e), DSC_FINNF3e);
  Gerador.wCampo(tcStr, '#016', 'verProc', 1, 20, 1, NF3e.Ide.verProc, DSC_VERPROC);

  if (NF3e.Ide.dhCont > 0) or (NF3e.Ide.xJust <> '') then
  begin
    Gerador.wCampo(tcStr, '#017', 'dhCont', 25, 025, 1, DateTimeTodh(NF3e.ide.dhCont) +
                GetUTC(CodigoParaUF(NF3e.ide.cUF), NF3e.ide.dhCont), DSC_DHCONT);
    Gerador.wCampo(tcStr, '#018', 'xJust ', 01, 256, 1, NF3e.ide.xJust, DSC_XJUSTCONT);
  end;

  Gerador.wGrupo('/ide');
end;

procedure TNF3eW.GerarEmit;
begin
  Gerador.wGrupo('emit', '#019');
  Gerador.wCampoCNPJCPF('#020', '#020', NF3e.Emit.CNPJ);

  if NF3e.Emit.IE = 'ISENTO' then
    Gerador.wCampo(tcStr, '#021', 'IE', 0, 14, 1, NF3e.Emit.IE, DSC_IE)
  else
    Gerador.wCampo(tcStr, '#021', 'IE', 0, 14, 1, OnlyNumber(NF3e.Emit.IE), DSC_IE);

  if (FOpcoes.ValidarInscricoes) then
  begin
    if Length(NF3e.Emit.IE) = 0 then
      Gerador.wAlerta('#021', 'IE', DSC_IE, ERR_MSG_VAZIO)
    else
    begin
      if not pcnAuxiliar.ValidarIE(NF3e.Emit.IE, CodigoParaUF(NF3e.Ide.cUF)) then
        Gerador.wAlerta('#021', 'IE', DSC_IE, ERR_MSG_INVALIDO);
    end;
  end;

  Gerador.wCampo(tcStr, '#022', 'xNome', 2, 60, 1, NF3e.Emit.xNome, DSC_XNOME);
  Gerador.wCampo(tcStr, '#023', 'xFant', 1, 60, 0, NF3e.Emit.xFant, DSC_XFANT);

  GerarEnderEmit;

  Gerador.wGrupo('/emit');
end;

procedure TNF3eW.GerarEnderEmit;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL, NF3e.Emit.enderEmit.UF, NF3e.Emit.enderEmit.xMun, NF3e.Emit.EnderEmit.cMun);

  Gerador.wGrupo('enderEmit', '#024');
  Gerador.wCampo(tcStr, '#025', 'xLgr   ', 2, 60, 1, NF3e.Emit.enderEmit.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#026', 'nro    ', 1, 60, 1, NF3e.Emit.enderEmit.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#027', 'xCpl   ', 1, 60, 0, NF3e.Emit.enderEmit.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#028', 'xBairro', 2, 60, 1, NF3e.Emit.enderEmit.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#029', 'cMun   ', 1, 07, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(cMun) then
    Gerador.wAlerta('#029', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#030', 'xMun', 2, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#031', 'CEP ', 8, 08, 1, NF3e.Emit.enderEmit.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#032', 'UF  ', 2, 02, 1, xUF, DSC_UF);

  if not pcnAuxiliar.ValidarUF(xUF) then
    Gerador.wAlerta('#032', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#033', 'fone ', 6, 14, 0, OnlyNumber(NF3e.Emit.enderEmit.fone), DSC_FONE);
  Gerador.wCampo(tcStr, '#034', 'email', 1, 60, 0, NF3e.Emit.enderEmit.email, DSC_XPAIS);
  Gerador.wGrupo('/enderEmit');
end;

procedure TNF3eW.GerarDest;
var
  UF: String;
const
  HOM_NOME_DEST = 'NF3-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
begin
  UF := '';
  Gerador.wGrupo('dest', '#035');

  if NF3e.Ide.tpAmb = taProducao then
    Gerador.wCampo(tcStr, '#036', 'xNome', 2, 60, 1, NF3e.Dest.xNome, DSC_XNOME)
  else
    Gerador.wCampo(tcStr, '#036', 'xNome', 2, 60, 1, HOM_NOME_DEST, DSC_XNOME);

  if (NF3e.Dest.idOutros <> '') then
    Gerador.wCampo(tcStr, '#039', 'idOutros', 2, 20, 1, NF3e.Dest.idOutros, DSC_IDESTR)
  else
    Gerador.wCampoCNPJCPF('#037', '#038', NF3e.Dest.CNPJCPF, True);

  Gerador.wCampo(tcStr, '#040', 'indIEDest', 1, 01, 1, indIEDestToStr(NF3e.Dest.indIEDest), DSC_INDIEDEST);

  if NF3e.Dest.indIEDest <> inIsento then
  begin
    if (NF3e.Dest.IE <> '') or (NF3e.infNF3e.Versao < 3) then
     begin
       // Inscrição Estadual
       if NF3e.Dest.IE = '' then
         Gerador.wCampo(tcStr, '#041', 'IE', 0, 14, 1, '', DSC_IE)
       else
       if NF3e.Dest.IE = 'ISENTO' then
         Gerador.wCampo(tcStr, '#041', 'IE', 0, 14, 1, NF3e.Dest.IE, DSC_IE)
       else
       if (trim(NF3e.Dest.IE) <> '') or (NF3e.Ide.modelo <> 65)  then
         Gerador.wCampo(tcStr, '#041', 'IE', 0, 14, 1, OnlyNumber(NF3e.Dest.IE), DSC_IE);

       if (FOpcoes.ValidarInscricoes) and (NF3e.Dest.IE <> '') and (NF3e.Dest.IE <> 'ISENTO') then
         if not pcnAuxiliar.ValidarIE(NF3e.Dest.IE, UF) then
           Gerador.wAlerta('#041', 'IE', DSC_IE, ERR_MSG_INVALIDO);
     end;
  end;

  Gerador.wCampo(tcStr, '#042', 'IM            ', 1, 15, 0, NF3e.Dest.IM, DSC_IM);
  Gerador.wCampo(tcStr, '#043', 'cNIS          ', 1, 15, 0, NF3e.Dest.cNIS, DSC_ISUF);
  Gerador.wCampo(tcStr, '#044', 'xNomeAdicional', 2, 60, 0, NF3e.Dest.xNomeAdicional, DSC_XNOME);

  GerarEnderDest(UF);

  Gerador.wGrupo('/dest');
end;

procedure TNF3eW.GerarEnderDest(var UF: String);
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, 1058, NF3e.Dest.enderDest.UF, NF3e.Dest.enderDest.xMun, NF3e.Dest.enderDest.cMun);

  UF := xUF;
  Gerador.wGrupo('enderDest', '#044');
  Gerador.wCampo(tcStr, '#045', 'xLgr   ', 2, 60, 1, NF3e.Dest.enderDest.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#046', 'nro    ', 1, 60, 1, NF3e.Dest.enderDest.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#047', 'xCpl   ', 1, 60, 0, NF3e.Dest.enderDest.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#048', 'xBairro', 1, 60, 1, NF3e.Dest.enderDest.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#049', 'cMun   ', 1, 07, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(cMun) then
    Gerador.wAlerta('#049', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#050', 'xMun', 2, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#051', 'CEP ', 8, 08, 0, NF3e.Dest.enderDest.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#052', 'UF  ', 2, 02, 1, xUF, DSC_UF);

  if not pcnAuxiliar.ValidarUF(xUF) then
    Gerador.wAlerta('#052', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#053', 'fone ', 6, 14, 0, OnlyNumber(NF3e.Dest.enderDest.fone), DSC_FONE);
  Gerador.wCampo(tcStr, '#054', 'email', 1, 60, 0, NF3e.Dest.enderDest.email, DSC_XPAIS);
  Gerador.wGrupo('/enderDest');
end;

procedure TNF3eW.GerarAcessante;
begin
  Gerador.wGrupo('acessante', '#055');
  Gerador.wCampo(tcStr, '#056', 'idAcesso    ', 01, 15, 1, NF3e.acessante.idAcesso, DSC_IDACESSO);
  Gerador.wCampo(tcStr, '#057', 'idCodCliente', 05, 20, 0, NF3e.acessante.idCodCliente, DSC_IDCODCLIENTE);
  Gerador.wCampo(tcStr, '#058', 'tpAcesso    ', 01, 01, 1, tpAcessoToStr(NF3e.acessante.tpAcesso), DSC_TPACESSO);
  Gerador.wCampo(tcStr, '#059', 'xNomeUC     ', 02, 60, 0, NF3e.acessante.xNomeUC, DSC_XNOMEUC);
  Gerador.wCampo(tcStr, '#060', 'tpClasse    ', 02, 02, 0, tpClasseToStr(NF3e.acessante.tpClasse), DSC_TPCLASSE);
  Gerador.wCampo(tcStr, '#061', 'tpSubClasse ', 02, 02, 0, tpSubClasseToStr(NF3e.acessante.tpSubClasse), DSC_TPSUBCLASSE);
  Gerador.wCampo(tcStr, '#062', 'tpFase      ', 01, 01, 1, tpFaseToStr(NF3e.acessante.tpFase), DSC_TPFASE);
  Gerador.wCampo(tcStr, '#063', 'tpGrpTensao ', 02, 02, 1, tpGrpTensaoToStr(NF3e.acessante.tpGrpTensao), DSC_TPGRPTENSAO);
  Gerador.wCampo(tcStr, '#064', 'tpModTar    ', 02, 02, 1, tpModTarToStr(NF3e.acessante.tpModTar), DSC_TPMODTAR);

  Gerador.wCampo(tcStr, '#065', 'latGPS ', 01, 10, 1, NF3e.acessante.latGPS, DSC_LATGPS);
  Gerador.wCampo(tcStr, '#066', 'longGPS', 01, 11, 1, NF3e.acessante.longGPS, DSC_LONGGPS);

  Gerador.wCampo(tcStr, '#066', 'codRoteiroLeitura', 1, 100, 0, NF3e.acessante.codRoteiroLeitura, DSC_CODROTEIROLEITURA);

  Gerador.wGrupo('/acessante');
end;

procedure TNF3eW.GerarSub;
begin
  if (NF3e.gSub.chNF3e <> '') or (NF3e.gSub.CNPJ <> '') then
  begin
    Gerador.wGrupo('gSub', '#067');

    if NF3e.gSub.chNF3e <> '' then
    begin
      Gerador.wCampo(tcStr, '#068', 'chNF3e', 44, 44, 1, NF3e.gSub.chNF3e, DSC_CHNF3E);

      if not ValidarChave(NF3e.gSub.chNF3e) then
        Gerador.wAlerta('#068', 'chNF3e', DSC_CHNF3E, ERR_MSG_INVALIDO);
    end
    else
    begin
      Gerador.wGrupo('gNF', '#069');
      Gerador.wCampo(tcStr, '#070', 'CNPJ      ', 14, 14, 1, NF3e.gSub.CNPJ, DSC_CNPJ);
      Gerador.wCampo(tcInt, '#071', 'serie     ', 01, 03, 1, NF3e.gSub.serie, DSC_SERIE);
      Gerador.wCampo(tcInt, '#072', 'nNF       ', 01, 09, 1, NF3e.gSub.nNF, DSC_NNF);
      Gerador.wCampo(tcStr, '#073', 'CompetEmis', 06, 06, 1, FormatDateTime('yyyymm', NF3e.gSub.CompetEmis), DSC_COMPETEMIS);
      Gerador.wCampo(tcStr, '#074', 'CompetApur', 06, 06, 1, FormatDateTime('yyyymm', NF3e.gSub.CompetApur), DSC_COMPETAPUR);
      Gerador.wCampo(tcStr, '#075', 'hash115   ', 28, 28, 0, NF3e.gSub.hash115, DSC_HASH115);
      Gerador.wGrupo('/gNF');
    end;

    Gerador.wCampo(tcStr, '#076', 'motSub', 2, 2, 1, motSubToStr(NF3e.gSub.motSub), DSC_MOTSUB);

    Gerador.wGrupo('/gSub');
  end;
end;

procedure TNF3eW.GerarJudic;
begin
  if (NF3e.gJudic.chNF3e <> '') then
  begin
    Gerador.wGrupo('gJudic', '#077');

    Gerador.wCampo(tcStr, '#078', 'chNF3e', 44, 44, 1, NF3e.gJudic.chNF3e, DSC_CHNF3E);
    Gerador.wCampo(tcStr, '#076', 'motSub', 02, 02, 1, motSubToStr(NF3e.gSub.motSub), DSC_MOTSUB);

    if not ValidarChave(NF3e.gJudic.chNF3e) then
      Gerador.wAlerta('#078', 'chNF3e', DSC_CHNF3E, ERR_MSG_INVALIDO);

    Gerador.wGrupo('/gJudic');
  end;
end;

procedure TNF3eW.GerarGrContrat;
var
  i: Integer;
begin
  for i := 0 to NF3e.gGrContrat.Count - 1 do
  begin
    Gerador.wGrupo('gGrContrat nContrat="' + IntToStr(NF3e.gGrContrat[i].nContrat) + '"', '#079');

    if (NF3e.gGrContrat[i].nContrat > 20) then
      Gerador.wAlerta('#080', 'nContrat', DSC_NCONTRAT, ERR_MSG_MAIOR);

    if (NF3e.gGrContrat[i].nContrat < 1) then
      Gerador.wAlerta('#080', 'nContrat', DSC_NCONTRAT, ERR_MSG_MENOR);

    Gerador.wCampo(tcStr, '#081', 'tpGrContrat ', 1, 01, 1, tpGrContratToStr(NF3e.gGrContrat[i].tpGrContrat), DSC_TPGRCONTRAT);
    Gerador.wCampo(tcStr, '#082', 'tpPosTar    ', 1, 01, 1, tpPosTarToStr(NF3e.gGrContrat[i].tpPosTar), DSC_TPPOSTAR);
    Gerador.wCampo(tcDe2, '#083', 'qUnidContrat', 1, 15, 0, NF3e.gGrContrat[i].qUnidContrat, DSC_QUNIDCONTRAT);

    Gerador.wGrupo('/gGrContrat');
  end;

  if NF3e.gGrContrat.Count > 20 then
    Gerador.wAlerta('#079', 'gGrContrat', '', ERR_MSG_MAIOR_MAXIMO + '20');
end;

procedure TNF3eW.GerarMed;
var
  i: Integer;
begin
  for i := 0 to NF3e.gMed.Count - 1 do
  begin
    Gerador.wGrupo('gMed nMed="' + IntToStr(NF3e.gMed[i].nMed) + '"', '#084');

    if (NF3e.gMed[i].nMed > 20) then
      Gerador.wAlerta('#085', 'nMed', DSC_NMED, ERR_MSG_MAIOR);

    if (NF3e.gMed[i].nMed < 1) then
      Gerador.wAlerta('#085', 'nMed', DSC_NMED, ERR_MSG_MENOR);

    Gerador.wCampo(tcInt, '#086', 'idMedidor', 01, 15, 1, NF3e.gMed[i].idMedidor, DSC_IDMEDIDOR);
    Gerador.wCampo(tcDat, '#087', 'dMedAnt  ', 10, 10, 1, NF3e.gMed[i].dMedAnt, DSC_DMEDANT);
    Gerador.wCampo(tcDat, '#088', 'dMedAtu  ', 10, 10, 1, NF3e.gMed[i].dMedAnt, DSC_DMEDATU);

    Gerador.wGrupo('/gMed');
  end;

  if NF3e.gMed.Count > 20 then
    Gerador.wAlerta('#084', 'gMed', '', ERR_MSG_MAIOR_MAXIMO + '20');
end;

procedure TNF3eW.GerarSCEE;
var
  i: Integer;
begin
  if (NF3e.gSCEE.vPotInst > 0) then
  begin
    Gerador.wGrupo('gSCEE', '#089');

    Gerador.wCampo(tcStr, '#090', 'tpPartComp', 1, 1, 1, tpPartCompToStr(NF3e.gSCEE.tpPartComp), DSC_TPPARTCOMP);
    Gerador.wCampo(tcDe3, '#091', 'vPotInst  ', 1, 9, 1, NF3e.gSCEE.vPotInst, DSC_VPOTINST);

    for i := 0 to NF3e.gSCEE.gConsumidor.Count - 1 do
    begin
      Gerador.wGrupo('gConsumidor', '#092');
      Gerador.wCampo(tcInt, '#093', 'idAcessGer', 1, 15, 1, NF3e.gSCEE.gConsumidor[i].idAcessGer, DSC_IDACESSGER);
      Gerador.wCampo(tcDe3, '#094', 'enerAloc  ', 1, 08, 1, NF3e.gSCEE.gConsumidor[i].enerAloc, DSC_ENERALOC);
      Gerador.wCampo(tcStr, '#095', 'tpPosTar  ', 1, 01, 1, tpPosTarToStr(NF3e.gSCEE.gConsumidor[i].tpPosTar), DSC_TPPOSTAR);
      Gerador.wGrupo('/gConsumidor');
    end;

    if NF3e.gSCEE.gConsumidor.Count > 999 then
      Gerador.wAlerta('#092', 'gConsumidor', '', ERR_MSG_MAIOR_MAXIMO + '999');

    for i := 0 to NF3e.gSCEE.gSaldoCred.Count - 1 do
    begin
      Gerador.wGrupo('gSaldoCred', '#096');
      Gerador.wCampo(tcStr, '#097', 'tpPosTar     ', 1, 01, 1, tpPosTarToStr(NF3e.gSCEE.gSaldoCred[i].tpPosTar), DSC_TPPOSTAR);
      Gerador.wCampo(tcInt, '#098', 'vSaldAnt     ', 1, 11, 1, NF3e.gSCEE.gSaldoCred[i].vSaldAnt, DSC_VSALDANT);
      Gerador.wCampo(tcInt, '#099', 'vCredExpirado', 1, 11, 1, NF3e.gSCEE.gSaldoCred[i].vCredExpirado, DSC_VCREDEXPIRADO);
      Gerador.wCampo(tcInt, '#100', 'vSaldAtual   ', 1, 11, 1, NF3e.gSCEE.gSaldoCred[i].vSaldAtual, DSC_VSALDATUAL);

      if (NF3e.gSCEE.gSaldoCred[i].vCredExpirar > 0) or
         (NF3e.gSCEE.gSaldoCred[i].CompetExpirar > 0) then
      begin
        Gerador.wCampo(tcInt, '#101', 'vCredExpirar ', 1, 11, 1, NF3e.gSCEE.gSaldoCred[i].vCredExpirar, DSC_VCREDEXPIRAR);
        Gerador.wCampo(tcStr, '#102', 'CompetExpirar', 6, 06, 1, FormatDateTime('yyyymm', NF3e.gSCEE.gSaldoCred[i].CompetExpirar), DSC_COMPETEXPIRAR);
      end;

      Gerador.wGrupo('/gSaldoCred');
    end;

    if NF3e.gSCEE.gSaldoCred.Count > 3 then
      Gerador.wAlerta('#096', 'gSaldoCred', '', ERR_MSG_MAIOR_MAXIMO + '3');

    Gerador.wGrupo('/gSCEE');
  end;
end;

procedure TNF3eW.GerarNFDet;
var
  i: Integer;
begin
  for i := 0 to NF3e.NFDet.Count - 1 do
  begin
    if NF3e.NFDet[i].chNF3eAnt <> '' then
    begin
      Gerador.wGrupo('NFdet chNF3eAnt="' + NF3e.NFDet[i].chNF3eAnt + '"', '#103');

      if not ValidarChave(NF3e.NFDet[i].chNF3eAnt) then
        Gerador.wAlerta('#104', 'chNF3eAnt', DSC_CHNF3E, ERR_MSG_INVALIDO);
    end
    else
     Gerador.wGrupo('NFdet', '#103');

     GerarDet(i);

    Gerador.wGrupo('/NFdet');
  end;

  if NF3e.NFDet.Count > 13 then
    Gerador.wAlerta('#103', 'NFdet', '', ERR_MSG_MAIOR_MAXIMO + '13');
end;

procedure TNF3eW.GerarDet(const i: Integer);
var
  j: Integer;
begin
  for j := 0 to NF3e.NFDet[i].Det.Count - 1 do
  begin
    Gerador.wGrupo('det nItem="' + IntToStr(NF3e.NFDet[i].Det[j].nItem) + '"', '#105');

    if NF3e.NFDet[i].Det[j].gAjusteNF3eAnt.tpAjuste <> taNenhum then
    begin
      Gerador.wGrupo('/gAjusteNF3eAnt', '#107');
      Gerador.wCampo(tcStr, '#108', 'tpAjuste ', 1, 1, 1, tpAjusteToStr(NF3e.NFDet[i].Det[j].gAjusteNF3eAnt.tpAjuste), DSC_TPAJUSTE);
      Gerador.wCampo(tcStr, '#109', 'motAjuste', 1, 1, 1, MotAjusteToStr(NF3e.NFDet[i].Det[j].gAjusteNF3eAnt.motAjuste), DSC_MOTAJUSTE);
      Gerador.wGrupo('/gAjusteNF3eAnt');
    end;

    if NF3e.NFDet[i].Det[j].detItemAnt.nItemAnt > 0 then
      GerarDetItemAnt(i, j)
    else
      GerarDetItem(i, j);

    Gerador.wGrupo('/det');
  end;

  if NF3e.NFDet[i].Det.Count > 990 then
    Gerador.wAlerta('#105', 'Det', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TNF3eW.GerarDetItemAnt(const i, j: Integer);
begin
  Gerador.wGrupo('detItemAnt nItemAnt="' +
              IntToStr(NF3e.NFDet[i].Det[j].detItemAnt.nItemAnt) + '"', '#110');

  // pode ter de 2 ou 6 casas decimais
  Gerador.wCampo(tcDe2, '#112', 'vItem    ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItemAnt.vItem, DSC_VITEM);

  if Frac(NF3e.NFDet[i].Det[j].detItemAnt.qFaturada) > 0 then
    Gerador.wCampo(tcDe4, '#113', 'qFaturada', 1, 15, 1, NF3e.NFDet[i].Det[j].detItemAnt.qFaturada, DSC_QFATURADA)
  else
    Gerador.wCampo(tcInt, '#113', 'qFaturada', 1, 15, 1, NF3e.NFDet[i].Det[j].detItemAnt.qFaturada, DSC_QFATURADA);

  // pode ter de 2 ou 6 casas decimais
  Gerador.wCampo(tcDe2, '#114', 'vProd    ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItemAnt.vProd, DSC_VPROD);
  Gerador.wCampo(tcStr, '#115', 'cClass   ', 7, 07, 1, NF3e.NFDet[i].Det[j].detItemAnt.cClass, DSC_CCLASS);
  Gerador.wCampo(tcDe2, '#116', 'vBC      ', 1, 15, 0, NF3e.NFDet[i].Det[j].detItemAnt.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, '#117', 'pICMS    ', 1, 15, 0, NF3e.NFDet[i].Det[j].detItemAnt.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, '#118', 'vICMS    ', 1, 15, 0, NF3e.NFDet[i].Det[j].detItemAnt.vICMS, DSC_VICMS);
  Gerador.wCampo(tcDe2, '#119', 'vPIS     ', 1, 15, 0, NF3e.NFDet[i].Det[j].detItemAnt.vPIS, DSC_VPIS);
  Gerador.wCampo(tcDe2, '#120', 'vCOFINS  ', 1, 15, 0, NF3e.NFDet[i].Det[j].detItemAnt.vCOFINS, DSC_VCOFINS);

  Gerador.wGrupo('/detItemAnt');
end;

procedure TNF3eW.GerarDetItem(const i, j: Integer);
begin
  if NF3e.NFDet[i].Det[j].detItem.nItemAnt > 0 then
  begin
    Gerador.wGrupo('detItem nItemAnt="' +
      IntToStr(NF3e.NFDet[i].Det[j].detItem.nItemAnt) + '"', '#121');

    if (NF3e.NFDet[i].Det[j].detItem.nItemAnt > 990) then
      Gerador.wAlerta('#122', 'nItemAnt', DSC_NITEMANT, ERR_MSG_MAIOR);
    if (NF3e.NFDet[i].Det[j].detItem.nItemAnt < 1) then
      Gerador.wAlerta('#122', 'nItemAnt', DSC_NITEMANT, ERR_MSG_MENOR);
  end
  else
   Gerador.wGrupo('detItem', '#121');

  GerargTarif(i, j);
  GerargAdBand(i, j);
  GerarDetProd(i, j);
  GerarImposto(i, j);
  GerargProcRef(i, j);
  GerargContab(i, j);

  Gerador.wCampo(tcStr, '#234', 'infAdProd', 1, 500, 0, NF3e.NFDet[i].Det[j].detItem.infAdProd, DSC_INFADPROD);

  Gerador.wGrupo('/detItem');
end;

procedure TNF3eW.GerargTarif(const i, j: Integer);
var
  k: Integer;
begin
  for k := 0 to NF3e.NFDet[i].Det[j].detItem.gTarif.Count - 1 do
  begin
    Gerador.wGrupo('gTarif', '#123');
    Gerador.wCampo(tcDat, '#124', 'dIniTarif', 10, 10, 1, NF3e.NFDet[i].Det[j].detItem.gTarif[k].dIniTarif, DSC_DINITARIF);
    Gerador.wCampo(tcDat, '#125', 'dFimTarif', 10, 10, 1, NF3e.NFDet[i].Det[j].detItem.gTarif[k].dFimTarif, DSC_DFIMTARIF);
    Gerador.wCampo(tcStr, '#126', 'tpAto    ', 01, 01, 1, tpAtoToStr(NF3e.NFDet[i].Det[j].detItem.gTarif[k].tpAto), DSC_TPATO);
    Gerador.wCampo(tcStr, '#127', 'nAto     ', 04, 04, 1, NF3e.NFDet[i].Det[j].detItem.gTarif[k].nAto, DSC_NATO);
    Gerador.wCampo(tcInt, '#128', 'anoAto   ', 04, 04, 1, NF3e.NFDet[i].Det[j].detItem.gTarif[k].anoAto, DSC_ANOATO);
    Gerador.wCampo(tcStr, '#129', 'tpTarif  ', 01, 01, 1, tpTarifToStr(NF3e.NFDet[i].Det[j].detItem.gTarif[k].tpTarif), DSC_TPTARIF);
    Gerador.wCampo(tcStr, '#130', 'cPosTarif', 01, 01, 1, cPosTarifToStr(NF3e.NFDet[i].Det[j].detItem.gTarif[k].cPosTarif), DSC_CPOSTARIF);
    Gerador.wCampo(tcStr, '#131', 'uMed     ', 01, 01, 1, uMedToStr(NF3e.NFDet[i].Det[j].detItem.gTarif[k].uMed), DSC_UMED);
    Gerador.wCampo(tcDe6, '#132', 'vTarifHom', 01, 15, 1, NF3e.NFDet[i].Det[j].detItem.gTarif[k].vTarifHom, DSC_VTARIFHOM);

    if NF3e.NFDet[i].Det[j].detItem.gTarif[k].vTarifAplic > 0 then
    begin
      Gerador.wCampo(tcDe6, '#133', 'vTarifAplic', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.gTarif[k].vTarifAplic, DSC_VTARIFAPLIC);
      Gerador.wCampo(tcStr, '#134', 'motDifTarif', 2, 02, 1, motDifTarifToStr(NF3e.NFDet[i].Det[j].detItem.gTarif[k].motDifTarif), DSC_MOTDIFTARIF);
    end;

    Gerador.wGrupo('/gTarif');
  end;

  if NF3e.NFDet[i].Det[j].detItem.gTarif.Count > 4 then
    Gerador.wAlerta('#123', 'gTarif', '', ERR_MSG_MAIOR_MAXIMO + '4');
end;

procedure TNF3eW.GerargAdBand(const i, j: Integer);
var
  k: Integer;
begin
  for k := 0 to NF3e.NFDet[i].Det[j].detItem.gAdBand.Count - 1 do
  begin
    Gerador.wGrupo('gAdBand', '#135');
    Gerador.wCampo(tcDat, '#136', 'dIniAdBand', 10, 10, 1, NF3e.NFDet[i].Det[j].detItem.gAdBand[k].dIniAdBand, DSC_DINITARIF);
    Gerador.wCampo(tcDat, '#137', 'dFimAdBand', 10, 10, 1, NF3e.NFDet[i].Det[j].detItem.gAdBand[k].dFimAdBand, DSC_DFIMTARIF);
    Gerador.wCampo(tcStr, '#138', 'tpBand    ', 01, 01, 1, tpBandToStr(NF3e.NFDet[i].Det[j].detItem.gAdBand[k].tpBand), DSC_TPBAND);
    Gerador.wCampo(tcDe2, '#139', 'vAdBand   ', 01, 06, 1, NF3e.NFDet[i].Det[j].detItem.gAdBand[k].vAdBand, DSC_VADBAND);

    if NF3e.NFDet[i].Det[j].detItem.gAdBand[k].vAdBandAplic > 0 then
    begin
      Gerador.wCampo(tcDe2, '#140', 'vAdBandAplic', 1, 06, 1, NF3e.NFDet[i].Det[j].detItem.gAdBand[k].vAdBandAplic, DSC_VADBANDAPLIC);
      Gerador.wCampo(tcStr, '#141', 'motDifBand  ', 2, 02, 1, motDifBandToStr(NF3e.NFDet[i].Det[j].detItem.gAdBand[k].motDifBand), DSC_MOTDIFBAND);
    end;

    Gerador.wGrupo('/gAdBand');
  end;

  if NF3e.NFDet[i].Det[j].detItem.gAdBand.Count > 3 then
    Gerador.wAlerta('#135', 'gAdBand', '', ERR_MSG_MAIOR_MAXIMO + '3');
end;

procedure TNF3eW.GerarDetProd(const i, j: Integer);
begin
  Gerador.wGrupo('prod', '#142');
  Gerador.wCampo(tcStr, '#143', 'indOrigemQtd', 1, 1, 1, indOrigemQtdToStr(NF3e.NFDet[i].Det[j].detItem.Prod.indOrigemQtd), DSC_INDORIGEMQTD);

  if NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.nMed > 0 then
    GerargMedicao(i, j);

  Gerador.wCampo(tcStr, '#158', 'cProd       ', 01, 060, 1, NF3e.NFDet[i].Det[j].detItem.Prod.cProd, DSC_CPROD);
  Gerador.wCampo(tcStr, '#159', 'xProd       ', 01, 120, 1, NF3e.NFDet[i].Det[j].detItem.Prod.xProd, DSC_XPROD);
  Gerador.wCampo(tcStr, '#160', 'cClass      ', 07, 007, 1, NF3e.NFDet[i].Det[j].detItem.Prod.cClass, DSC_CCLASS);
  Gerador.wCampo(tcInt, '#161', 'CFOP        ', 04, 004, 0, NF3e.NFDet[i].Det[j].detItem.Prod.CFOP, DSC_CFOP);
  Gerador.wCampo(tcStr, '#162', 'uMed        ', 01, 001, 1, uMedFatToStr(NF3e.NFDet[i].Det[j].detItem.Prod.uMed), DSC_UMED);

  if Frac(NF3e.NFDet[i].Det[j].detItem.Prod.qFaturada) > 0 then
    Gerador.wCampo(tcDe4, '#163', 'qFaturada', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Prod.qFaturada, DSC_QFATURADA)
  else
    Gerador.wCampo(tcInt, '#163', 'qFaturada', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Prod.qFaturada, DSC_QFATURADA);

  // pode ter 2 ou 6 casas decimais
  Gerador.wCampo(tcDe2, '#164', 'vItem       ', 01, 015, 1, NF3e.NFDet[i].Det[j].detItem.Prod.vItem, DSC_VITEM);
  // pode ter 2 ou 6 casas decimais
  Gerador.wCampo(tcDe2, '#165', 'vProd       ', 01, 015, 1, NF3e.NFDet[i].Det[j].detItem.Prod.vProd, DSC_VPROD);

  if NF3e.NFDet[i].Det[j].detItem.Prod.indDevolucao = tiSim then
    Gerador.wCampo(tcStr, '#166', 'indDevolucao', 1, 1, 1, '1', '');

  if NF3e.NFDet[i].Det[j].detItem.Prod.indPrecoACL = tiSim then
    Gerador.wCampo(tcStr, '#167', 'indPrecoACL', 1, 1, 1, '1', '');

  Gerador.wGrupo('/prod');
end;

procedure TNF3eW.GerargMedicao(const i, j: Integer);
begin
  Gerador.wGrupo('gMedicao', '#144');
  Gerador.wCampo(tcInt, '#145', 'nMed    ', 1, 2, 1, NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.nMed, DSC_NMED);
  Gerador.wCampo(tcInt, '#146', 'nContrat', 1, 2, 0, NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.nContrat, DSC_NCONTRAT);

  if NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.tpMotNaoLeitura = tmNenhum then
  begin
    Gerador.wGrupo('gMedida', '#147');
    Gerador.wCampo(tcStr, '#148', 'tpGrMed  ', 2, 02, 1, tpGrMedToStr(NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.tpGrMed), DSC_TPGRMED);
    Gerador.wCampo(tcStr, '#149', 'cPosTarif', 1, 01, 1, cPosTarifToStr(NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.cPosTarif), DSC_CPOSTARIF);
    Gerador.wCampo(tcStr, '#150', 'uMed     ', 1, 01, 1, uMedFatToStr(NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.uMed), DSC_UMED);
    Gerador.wCampo(tcDe2, '#151', 'vMedAnt  ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.vMedAnt, DSC_VMEDANT);
    Gerador.wCampo(tcDe2, '#152', 'vMedAtu  ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.vMedAtu, DSC_VMEDATU);
    Gerador.wCampo(tcDe2, '#153', 'vConst   ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.vConst, DSC_VCONST);
    Gerador.wCampo(tcDe2, '#154', 'vMed     ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.vMed, DSC_VMED);

    if (NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.pPerdaTran > 0) or
       (NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.vMedPerdaTran > 0) then
    begin
      Gerador.wCampo(tcDe2, '#155', 'pPerdaTran   ', 1, 06, 1, NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.pPerdaTran, DSC_PPERDATRAN);
      Gerador.wCampo(tcDe2, '#156', 'vMedPerdaTran', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.vMedPerdaTran, DSC_VMEDPERDATRAN);
    end;

    Gerador.wGrupo('/gMedida');
  end
  else
    Gerador.wCampo(tcStr, '#157', 'tpMotNaoLeitura', 1, 1, 1, tpMotNaoLeituraToStr(NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.tpMotNaoLeitura), DSC_TPMOTNAOLEITURA);

  Gerador.wGrupo('/gMedicao');
end;

procedure TNF3eW.GerarImposto(const i, j: Integer);
begin
  Gerador.wGrupo('imposto', '#168');

  GerarICMS(i, j);
  GerarPIS(i, j);
  GerarCOFINS(i, j);

  Gerador.wGrupo('/imposto');
end;

procedure TNF3eW.GerarICMS(const i, j: Integer);

  function BuscaTag(const t: TpcnCSTIcms): String;
  begin
    case t of
      cst00: result := '00';
      cst10: result := '10';
      cst20: result := '20';
      cst40,
      cst41: result := '40';
      cst51: result := '51';
      cst90: result := '90';
    end;
  end;

  function BuscaNumTag(const t: TpcnCSTIcms): String;
  begin
    case t of
      cst00: result := '169';
      cst10: result := '176';
      cst20: result := '183';
      cst40,
      cst41: result := '193';
      cst51: result := '197';
      cst90: result := '201';
    end;
  end;

var
  sTagTemp: String;

begin
  sTagTemp := BuscaTag(NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.CST);

  Gerador.wGrupo('ICMS' + sTagTemp, '#' + BuscaNumTag(NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.CST));

  Gerador.wCampo(tcStr, '#170', 'CST', 2, 2, 1, CSTICMSTOStr(NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.CST), DSC_CST);

  case NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.CST of
    cst00:
      begin
        Gerador.wCampo(tcDe2, '#171', 'vBC  ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vBC, DSC_VBC);
        Gerador.wCampo(tcDe2, '#172', 'pICMS', 1, 05, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pICMS, DSC_PICMS);
        Gerador.wCampo(tcDe2, '#173', 'vICMS', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMS, DSC_VICMS);

        if (NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pFCP > 0) or
           (NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vFCP > 0) then
        begin
          Gerador.wCampo(tcDe2, '#174', 'pFCP', 1, 05, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pFCP, DSC_PFCP);
          Gerador.wCampo(tcDe2, '#175', 'vFCP', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vFCP, DSC_VFCP);
        end;
      end;

    cst10:
      begin
        Gerador.wCampo(tcDe2, '#178', 'vBCST  ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vBCST, DSC_VBCST);
        Gerador.wCampo(tcDe2, '#179', 'pICMSST', 1, 05, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pICMSST, DSC_PICMSST);
        Gerador.wCampo(tcDe2, '#180', 'vICMSST', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMSST, DSC_VICMSST);

        if (NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pFCP > 0) or
           (NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vFCP > 0) then
        begin
          Gerador.wCampo(tcDe2, '#181', 'pFCPST', 1, 05, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pFCPST, DSC_PFCPST);
          Gerador.wCampo(tcDe2, '#182', 'vFCPST', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vFCPST, DSC_VFCPST);
        end;
      end;

    cst20:
      begin
        Gerador.wCampo(tcDe2, '#185', 'pRedBC', 1, 05, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pRedBC, DSC_PREDBC);
        Gerador.wCampo(tcDe2, '#186', 'vBC   ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vBC, DSC_VBC);
        Gerador.wCampo(tcDe2, '#187', 'pICMS ', 1, 05, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pICMS, DSC_PICMS);
        Gerador.wCampo(tcDe2, '#188', 'vICMS ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMS, DSC_VICMS);

        if NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMSDeson > 0 then
        begin
          Gerador.wCampo(tcDe2, '#189', 'vICMSDeson', 01, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMSDeson, DSC_VICMSDESON);
          Gerador.wCampo(tcStr, '#190', 'cBenef    ', 10, 10, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.cBenef, DSC_CBENEF);
        end;

        if (NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pFCP > 0) or
           (NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vFCP > 0) then
        begin
          Gerador.wCampo(tcDe2, '#191', 'pFCP', 1, 05, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pFCP, DSC_PFCP);
          Gerador.wCampo(tcDe2, '#192', 'vFCP', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vFCP, DSC_VFCP);
        end;
      end;

    cst40,
    cst41:
      begin
        if NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMSDeson > 0 then
        begin
          Gerador.wCampo(tcDe2, '#195', 'vICMSDeson', 01, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMSDeson, DSC_VICMSDESON);
          Gerador.wCampo(tcStr, '#196', 'cBenef    ', 10, 10, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.cBenef, DSC_CBENEF);
        end;
      end;

    cst51:
      begin
        if NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMSDeson > 0 then
        begin
          Gerador.wCampo(tcDe2, '#199', 'vICMSDeson', 01, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMSDeson, DSC_VICMSDESON);
          Gerador.wCampo(tcStr, '#200', 'cBenef    ', 10, 10, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.cBenef, DSC_CBENEF);
        end;
      end;

    cst90:
      begin
        Gerador.wCampo(tcDe2, '#203', 'vBC  ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vBC, DSC_VBC);
        Gerador.wCampo(tcDe2, '#204', 'pICMS', 1, 05, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pICMS, DSC_PICMS);
        Gerador.wCampo(tcDe2, '#205', 'vICMS', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMS, DSC_VICMS);
      end;
  end;

  Gerador.wGrupo('/ICMS' + sTagTemp );
end;

procedure TNF3eW.GerarPIS(const i, j: Integer);
begin
  if (NF3e.NFDet[i].Det[j].detItem.Imposto.PIS.vBC > 0) or
     (NF3e.NFDet[i].Det[j].detItem.Imposto.PIS.pPIS > 0) or
     (NF3e.NFDet[i].Det[j].detItem.Imposto.PIS.vPIS > 0) then
  begin
    Gerador.wGrupo('PIS', '#206');
    Gerador.wCampo(tcStr, '#207', 'CST ', 02, 02, 1, CSTPISToStr(NF3e.NFDet[i].Det[j].detItem.Imposto.PIS.CST), DSC_CST);
    Gerador.wCampo(tcDe2, '#208', 'vBC ', 01, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.PIS.vBC, DSC_VBC);
    Gerador.wCampo(tcDe2, '#209', 'pPIS', 01, 05, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.PIS.pPIS, DSC_PPIS);
    Gerador.wCampo(tcDe2, '#210', 'vPIS', 01, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.PIS.vPIS, DSC_VPIS);
    Gerador.wGrupo('/PIS');
  end;
end;

procedure TNF3eW.GerarCOFINS(const i, j: Integer);
begin
  if (NF3e.NFDet[i].Det[j].detItem.Imposto.COFINS.vBC > 0) or
     (NF3e.NFDet[i].Det[j].detItem.Imposto.COFINS.pCOFINS > 0) or
     (NF3e.NFDet[i].Det[j].detItem.Imposto.COFINS.vCOFINS > 0) then
  begin
    Gerador.wGrupo('COFINS', '#211');
    Gerador.wCampo(tcStr, '#212', 'CST    ', 02, 02, 1, CSTCOFINSToStr(NF3e.NFDet[i].Det[j].detItem.Imposto.COFINS.CST), DSC_CST);
    Gerador.wCampo(tcDe2, '#213', 'vBC    ', 01, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.COFINS.vBC, DSC_VBC);
    Gerador.wCampo(tcDe2, '#214', 'pCOFINS', 01, 05, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.COFINS.pCOFINS, DSC_PCOFINS);
    Gerador.wCampo(tcDe2, '#215', 'vCOFINS', 01, 15, 1, NF3e.NFDet[i].Det[j].detItem.Imposto.COFINS.vCOFINS, DSC_VCOFINS);
    Gerador.wGrupo('/COFINS');
  end;
end;

procedure TNF3eW.GerargProcRef(const i, j: Integer);
begin
  if NF3e.NFDet[i].Det[j].detItem.gProcRef.vItem > 0 then
  begin
    Gerador.wGrupo('gProcRef', '#216');

    // pode ter 2 ou 6 casas decimais
    Gerador.wCampo(tcDe2, '#217', 'vItem    ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.gProcRef.vItem, DSC_VITEM);

    if Frac(NF3e.NFDet[i].Det[j].detItem.gProcRef.qFaturada) > 0 then
      Gerador.wCampo(tcDe4, '#163', 'qFaturada', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.gProcRef.qFaturada, DSC_QFATURADA)
    else
      Gerador.wCampo(tcInt, '#163', 'qFaturada', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.gProcRef.qFaturada, DSC_QFATURADA);

    // pode ter 2 ou 6 casas decimais
    Gerador.wCampo(tcDe2, '#219', 'vProd    ', 1, 15, 1, NF3e.NFDet[i].Det[j].detItem.gProcRef.vProd, DSC_VPROD);

    if NF3e.NFDet[i].Det[j].detItem.gProcRef.indDevolucao = tiSim then
      Gerador.wCampo(tcStr, '#220', 'indDevolucao', 1, 1, 1, '1', '');

    Gerador.wCampo(tcDe2, '#221', 'vBC      ', 1, 15, 0, NF3e.NFDet[i].Det[j].detItem.gProcRef.vBC, DSC_VBC);
    Gerador.wCampo(tcDe2, '#222', 'pICMS    ', 1, 15, 0, NF3e.NFDet[i].Det[j].detItem.gProcRef.pICMS, DSC_PICMS);
    Gerador.wCampo(tcDe2, '#223', 'vICMS    ', 1, 15, 0, NF3e.NFDet[i].Det[j].detItem.gProcRef.vICMS, DSC_VICMS);
    Gerador.wCampo(tcDe2, '#224', 'vPIS     ', 1, 15, 0, NF3e.NFDet[i].Det[j].detItem.gProcRef.vPIS, DSC_VPIS);
    Gerador.wCampo(tcDe2, '#225', 'vCOFINS  ', 1, 15, 0, NF3e.NFDet[i].Det[j].detItem.gProcRef.vCOFINS, DSC_VCOFINS);

    GerargProc(i, j);

    Gerador.wGrupo('/gProcRef');
  end;
end;

procedure TNF3eW.GerargProc(const i, j: Integer);
var
  k: Integer;
begin
  for k := 0 to NF3e.NFDet[i].Det[j].detItem.gProcRef.gProc.Count - 1 do
  begin
    Gerador.wGrupo('gProc', '#226');
    Gerador.wCampo(tcStr, '#227', 'tpProc   ', 1, 01, 1, tpProcToStr(NF3e.NFDet[i].Det[j].detItem.gProcRef.gProc[k].tpProc), DSC_TPPROC);
    Gerador.wCampo(tcStr, '#228', 'nProcesso', 1, 60, 1, NF3e.NFDet[i].Det[j].detItem.gProcRef.gProc[k].nProcesso, DSC_NPROCESSO);
    Gerador.wGrupo('/gProc');
  end;

  if NF3e.NFDet[i].Det[j].detItem.gProcRef.gProc.Count > 10 then
    Gerador.wAlerta('#226', 'gProc', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TNF3eW.GerargContab(const i, j: Integer);
var
  k: Integer;
begin
  for k := 0 to NF3e.NFDet[i].Det[j].detItem.gContab.Count - 1 do
  begin
    Gerador.wGrupo('gContab', '#229');
    Gerador.wCampo(tcStr, '#230', 'cContab', 9, 013, 1, NF3e.NFDet[i].Det[j].detItem.gContab[k].cContab, DSC_CCONTAB);
    Gerador.wCampo(tcStr, '#231', 'xContab', 1, 100, 1, NF3e.NFDet[i].Det[j].detItem.gContab[k].xContab, DSC_XCONTAB);
    Gerador.wCampo(tcDe2, '#232', 'vContab', 1, 015, 1, NF3e.NFDet[i].Det[j].detItem.gContab[k].vContab, DSC_VCONTAB);
    Gerador.wCampo(tcStr, '#233', 'tpLanc ', 1, 001, 1, tpLancToStr(NF3e.NFDet[i].Det[j].detItem.gContab[k].tpLanc), DSC_TPLANC);
    Gerador.wGrupo('/gContab');
  end;

  if NF3e.NFDet[i].Det[j].detItem.gContab.Count > 99 then
    Gerador.wAlerta('#229', 'gContab', '', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TNF3eW.GerarTotal;
begin
  Gerador.wGrupo('total', '#235');
  Gerador.wCampo(tcDe2, '#236', 'vProd', 1, 15, 1, NF3e.Total.vProd, DSC_VPROD);

  Gerador.wGrupo('ICMSTot', '#237');
  Gerador.wCampo(tcDe2, '#238', 'vBC       ', 1, 15, 1, NF3e.Total.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, '#239', 'vICMS     ', 1, 15, 1, NF3e.Total.vICMS, DSC_VICMS);
  Gerador.wCampo(tcDe2, '#240', 'vICMSDeson', 1, 15, 1, NF3e.Total.vICMSDeson, DSC_VICMSDESON);
  Gerador.wCampo(tcDe2, '#241', 'vFCP      ', 1, 15, 1, NF3e.Total.vFCP, DSC_VFCP);
  Gerador.wCampo(tcDe2, '#242', 'vBCST     ', 1, 15, 1, NF3e.Total.vBCST, DSC_VBCST);
  Gerador.wCampo(tcDe2, '#243', 'vST       ', 1, 15, 1, NF3e.Total.vST, DSC_VST);
  Gerador.wCampo(tcDe2, '#244', 'vFCPST    ', 1, 15, 1, NF3e.Total.vFCPST, DSC_VFCPST);
  Gerador.wGrupo('/ICMSTot');

  Gerador.wCampo(tcDe2, '#245', 'vCOFINS', 1, 15, 1, NF3e.Total.vCOFINS, DSC_VCOFINS);
  Gerador.wCampo(tcDe2, '#246', 'vPIS   ', 1, 15, 1, NF3e.Total.vPIS, DSC_VPIS);
  Gerador.wCampo(tcDe2, '#247', 'vNF    ', 1, 15, 1, NF3e.Total.vNF, DSC_VNF);
  Gerador.wGrupo('/total');
end;

procedure TNF3eW.GerargFat;
begin
  Gerador.wGrupo('gFat', '#248');
  Gerador.wCampo(tcStr, '#249', 'CompetFat   ', 06, 06, 1, FormatDateTime('yyyymm', NF3e.gFat.CompetFat), DSC_COMPETFAT);
  Gerador.wCampo(tcDat, '#250', 'dVencFat    ', 10, 10, 1, NF3e.gFat.dVencFat, DSC_DVENC);
  Gerador.wCampo(tcDat, '#251', 'dApresFat   ', 10, 10, 1, NF3e.gFat.dApresFat, DSC_DAPRESFAT);
  Gerador.wCampo(tcDat, '#252', 'dProxLeitura', 10, 10, 1, NF3e.gFat.dProxLeitura, DSC_DPROXLEITURA);
  Gerador.wCampo(tcStr, '#253', 'nFat        ', 01, 20, 0, NF3e.gFat.nFat, DSC_NFAT);
  Gerador.wCampo(tcStr, '#254', 'codBarras   ', 01, 48, 1, NF3e.gFat.codBarras, DSC_CODBARRAS);

  if NF3e.gFat.codBarras <> '' then
    Gerador.wCampo(tcStr, '#255', 'codDebAuto', 1, 20, 1, NF3e.gFat.codDebAuto, DSC_CODDEBAUTO)
  else
  begin
    Gerador.wCampo(tcStr, '#256', 'codBanco  ', 3, 05, 1, NF3e.gFat.codBanco, DSC_CODBANCO);
    Gerador.wCampo(tcStr, '#257', 'codAgencia', 1, 10, 1, NF3e.gFat.codAgencia, DSC_CODAGENCIA);
  end;

  if NF3e.gFat.enderCorresp.xLgr <> '' then
    GerarEnderCorresp;

  Gerador.wGrupo('/gFat');
end;

procedure TNF3eW.GerarEnderCorresp;
var
  cMun: Integer;
  xMun: String;
  xUF: String;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, 1058, NF3e.gFat.enderCorresp.UF, NF3e.gFat.enderCorresp.xMun, NF3e.gFat.enderCorresp.cMun);

  Gerador.wGrupo('enderCorresp', '#258');
  Gerador.wCampo(tcStr, '#259', 'xLgr   ', 2, 60, 1, NF3e.gFat.enderCorresp.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#260', 'nro    ', 1, 60, 1, NF3e.gFat.enderCorresp.nro, DSC_NRO);
  Gerador.wCampo(tcStr, '#261', 'xCpl   ', 1, 60, 0, NF3e.gFat.enderCorresp.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#262', 'xBairro', 1, 60, 1, NF3e.gFat.enderCorresp.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#263', 'cMun   ', 1, 07, 1, cMun, DSC_CMUN);

  if not ValidarMunicipio(cMun) then
    Gerador.wAlerta('#263', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#264', 'xMun', 2, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#265', 'CEP ', 8, 08, 0, NF3e.gFat.enderCorresp.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#266', 'UF  ', 2, 02, 1, xUF, DSC_UF);

  if not pcnAuxiliar.ValidarUF(xUF) then
    Gerador.wAlerta('#266', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#267', 'fone ', 6, 14, 0, OnlyNumber(NF3e.gFat.enderCorresp.fone), DSC_FONE);
  Gerador.wCampo(tcStr, '#268', 'email', 1, 60, 0, NF3e.gFat.enderCorresp.email, DSC_XPAIS);
  Gerador.wGrupo('/enderCorresp');
end;

procedure TNF3eW.GerargANEEL;
var
  i, j: Integer;
begin
  Gerador.wGrupo('gANEEL', '#269');

  for i := 0 to NF3e.gANEEL.gHistFat.Count - 1 do
  begin
    Gerador.wGrupo('gHistFat', '#270');
    Gerador.wCampo(tcStr, '#271', 'xGrandFat', 2, 60, 1, NF3e.gANEEL.gHistFat[i].xGrandFat, DSC_XGRANDFAT);

    for j := 0 to NF3e.gANEEL.gHistFat[i].gGrandFat.Count - 1 do
    begin
      Gerador.wGrupo('gGrandFat', '#272');
      Gerador.wCampo(tcStr, '#273', 'CompetFat', 6, 06, 1, FormatDateTime('yyyymm', NF3e.gANEEL.gHistFat[i].gGrandFat[j].CompetFat), DSC_COMPETFAT);
      Gerador.wCampo(tcDe2, '#274', 'vFat     ', 1, 15, 1, NF3e.gANEEL.gHistFat[i].gGrandFat[j].vFat, DSC_VFAT);
      Gerador.wCampo(tcStr, '#275', 'uMed     ', 1, 01, 1, uMedFatToStr(NF3e.gANEEL.gHistFat[i].gGrandFat[j].uMed), DSC_UMED);
      Gerador.wCampo(tcInt, '#275', 'qtdDias  ', 2, 02, 1, NF3e.gANEEL.gHistFat[i].gGrandFat[j].qtdDias, DSC_QTDE);
      Gerador.wGrupo('/gGrandFat');
    end;

    if NF3e.gANEEL.gHistFat[i].gGrandFat.Count > 13 then
      Gerador.wAlerta('#272', 'gGrandFat', '', ERR_MSG_MAIOR_MAXIMO + '13');

    Gerador.wGrupo('/gHistFat');
  end;

  if NF3e.gANEEL.gHistFat.Count > 5 then
    Gerador.wAlerta('#270', 'gHistFat', '', ERR_MSG_MAIOR_MAXIMO + '5');

  Gerador.wGrupo('/gANEEL');
end;

procedure TNF3eW.GerarautXML;
var
  i: Integer;
begin
  for i := 0 to NF3e.autXML.Count - 1 do
  begin
    Gerador.wGrupo('autXML', '#276');
    Gerador.wCampoCNPJCPF('#277', '#278', NF3e.autXML[i].CNPJCPF);
    Gerador.wGrupo('/autXML');
  end;

  if NF3e.autXML.Count > 10 then
    Gerador.wAlerta('#276', 'autXML', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TNF3eW.GerarInfAdic;
begin
  if (trim(NF3e.InfAdic.infAdFisco) <> '') or (trim(NF3e.InfAdic.infCpl) <> '') then
  begin
    Gerador.wGrupo('infAdic', '#279');
    Gerador.wCampo(tcStr, '#280', 'infAdFisco', 1, 2000, 0, NF3e.InfAdic.infAdFisco, DSC_INFADFISCO);
    Gerador.wCampo(tcStr, '#281', 'infCpl    ', 1, 5000, 0, NF3e.InfAdic.infCpl, DSC_INFCPL);
    Gerador.wGrupo('/infAdic');
  end;
end;

procedure TNF3eW.GerarinfRespTec;
begin
  if (NF3e.infRespTec.CNPJ <> '') then
  begin
    Gerador.wGrupo('gRespTec', '#282');
    Gerador.wCampoCNPJ('#283', NF3e.infRespTec.CNPJ, CODIGO_BRASIL, True);
    Gerador.wCampo(tcStr, '#284', 'xContato', 2, 60, 1, NF3e.infRespTec.xContato, DSC_XCONTATO);
    Gerador.wCampo(tcStr, '#285', 'email   ', 6, 60, 1, NF3e.infRespTec.email, DSC_EMAIL);
    Gerador.wCampo(tcStr, '#286', 'fone    ', 7, 12, 1, NF3e.infRespTec.fone, DSC_FONE);

    if (idCSRT <> 0) and (CSRT <> '') then
    begin
      Gerador.wCampo(tcInt, '#287', 'idCSRT  ', 02, 02, 1, idCSRT, DSC_IDCSRT);
      Gerador.wCampo(tcStr, '#288', 'hashCSRT', 28, 28, 1, CalcularHashCSRT(CSRT, FChaveNF3e), DSC_HASHCSRT);
    end;

    Gerador.wGrupo('/gRespTec');
  end;
end;

// Outras //////////////////////////////////////////////////////////////////////

procedure TNF3eW.AjustarMunicipioUF(out xUF: String; out xMun: String; out
  cMun: Integer; cPais: Integer; const vxUF, vxMun: String; vcMun: Integer);
var
  PaisBrasil: Boolean;
begin
  PaisBrasil := cPais = CODIGO_BRASIL;
  cMun := IIf(PaisBrasil, vcMun, CMUN_EXTERIOR);
  xMun := IIf(PaisBrasil, vxMun, XMUN_EXTERIOR);
  xUF :=  IIf(PaisBrasil, vxUF, UF_EXTERIOR);

  if FOpcoes.NormatizarMunicipios then
    if ( ( EstaZerado(cMun)) and (xMun <> XMUN_EXTERIOR) ) then
      cMun := ObterCodigoMunicipio(xMun, xUF, FOpcoes.FPathArquivoMunicipios)
    else if ( ( EstaVazio(xMun)) and (cMun <> CMUN_EXTERIOR) ) then
      xMun := ObterNomeMunicipio(xUF, cMun, FOpcoes.FPathArquivoMunicipios);
end;

end.

