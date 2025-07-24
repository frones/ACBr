{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

unit ACBrCTe.IniReader;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrCTe.Classes,
  pcteProcCTe,
  pcnConversao,
  pcteConversaoCTe;

type
  { TCTeIniReader }

  TCTeIniReader = class
  private
    FCTe: TCTe;
    FVersaoDF: TVersaoCTe;
    FAmbiente: Integer;
    FtpEmis: Integer;

    function FimLoop(const Campo: string): Boolean;


    procedure Ler_CTe(AINIRec: TMemIniFile);
    procedure Ler_CTeSimp(AINIRec: TMemIniFile);
    procedure Ler_CTeOS(AINIRec: TMemIniFile);
    procedure Ler_GTVe(AINIRec: TMemIniFile);

    procedure Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Ler_InfPercurso(AINIRec: TMemIniFile; infPercurso: TinfPercursoCollection);
    procedure Ler_Tomador4(AINIRec: TMemIniFile; toma4: TToma4);
    procedure Ler_Complemento(AINIRec: TMemIniFile; Compl: TCompl);
    procedure Ler_ComplementoPassagem(AINIRec: TMemIniFile; pass: TPassCollection);
    procedure Ler_ComplementoObsCont(AINIRec: TMemIniFile; ObsCont: TObsContCollection);
    procedure Ler_ComplementoObsFisco(AINIRec: TMemIniFile; ObsFisco: TObsFiscoCollection);
    procedure Ler_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
    procedure Ler_Remetente(AINIRec: TMemIniFile; Rem: TRem);
    procedure Ler_LocalColeta(AINIRec: TMemIniFile; locColeta: TLocColeta);
    procedure Ler_Expedidor(AINIRec: TMemIniFile; exped: TExped);
    procedure Ler_Recebedor(AINIRec: TMemIniFile; receb: TReceb);
    procedure Ler_Destinatario(AINIRec: TMemIniFile; dest: TDest);
    procedure Ler_ValorPrestacao(AINIRec: TMemIniFile; vPrest: TVPrest);
    procedure Ler_ValorPrestacaoComposicao(AINIRec: TMemIniFile; Comp: TCompCollection);
    procedure Ler_Imposto(AINIRec: TMemIniFile; Imp: TImp);

    procedure Ler_InfCTeNormal(AINIRec: TMemIniFile; infCTeNorm: TInfCTeNorm);
    procedure Ler_InfCarga(AINIRec: TMemIniFile; infcarga: TInfCarga);
    procedure Ler_InfCTeNormalInfNF(AINIRec: TMemIniFile; infNF: TInfNFCollection);
    procedure Ler_InfCTeNormalInfNFe(AINIRec: TMemIniFile; infNFe: TInfNFeCollection);
    procedure Ler_Secao_InfNFe(AINIRec: TMemIniFile;const Secao: string;
      Nivel1, Nivel2: Integer; infNFe: TInfNFeCollection);
    procedure Ler_InfCTeNormalInfOutros(AINIRec: TMemIniFile; infOutros: TInfOutrosCollection);
    procedure Ler_InfCTeNormalInfDCe(AINIRec: TMemIniFile; infDCe: TInfDCeCollection);
    procedure Ler_InfCTeNormalDocAnteriores(AINIRec: TMemIniFile; docAnt: TDocAnt);

    procedure Ler_InfCTeNormalInfModal(AINIRec: TMemIniFile; infCTeNorm: TInfCTeNorm);
    procedure Ler_InfModalRodoviario(AINIRec: TMemIniFile; rodo: TRodo);
    procedure Ler_InfModalAereo(AINIRec: TMemIniFile; aereo: TAereo);
    procedure Ler_InfModalAquav(AINIRec: TMemIniFile; aquav: TAquav);
    procedure Ler_InfModalFerrov(AINIRec: TMemIniFile; ferrov: TFerrov);
    procedure Ler_InfModalDuto(AINIRec: TMemIniFile; duto: TDuto);
    procedure Ler_InfModalAereoPeri(AINIRec: TMemIniFile; peri: TPeriCollection);
    procedure Ler_InfCTeNormalVeiculosNovos(AINIRec: TMemIniFile; veicNovos: TVeicNovosCollection);
    procedure Ler_InfCTeNormalCobr(AINIRec: TMemIniFile; cobr: TCobr);
    procedure Ler_InfCTeNormalInfCTeSub(AINIRec: TMemIniFile; infCteSub: TInfCteSub);
    procedure Ler_InfCTeNormalInfGlobalizado(AINIRec: TMemIniFile; infGlobalizado: TInfGlobalizado);
    procedure Ler_InfCTeNormalInfCTeMultimodal(AINIRec: TMemIniFile; infServVinc: TInfServVinc);

    procedure Ler_InfCTeComplemento(AINIRec: TMemIniFile; infCTeComp: TInfCteComp);
    procedure Ler_InfCTeComplemento10(AINIRec: TMemIniFile; infCteComp10: TInfCteCompCollection);

    procedure Ler_AutorizadosXml(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Ler_InfRespTecnico(AINIRec: TMemIniFile; infRespTec: TinfRespTec);
    procedure Ler_InfCTeSupl(AINIRec: TMemIniFile; infCTeSupl: TinfCTeSupl);

    procedure Ler_Tomador(AINIRec: TMemIniFile; toma: TToma);
    procedure Ler_Detalhamento(AINIRec: TMemIniFile; det: TdetCollection);
    procedure Ler_Total(AINIRec: TMemIniFile; total: Ttotal);
    procedure Ler_InfServico(AINIRec: TMemIniFile; infServico: TInfServico);
    procedure Ler_InfDocReferencia(AINIRec: TMemIniFile; infDocRef: TinfDocRefCollection);
    procedure Ler_Seguro(AINIRec: TMemIniFile; seg: TSegCollection);
    procedure Ler_InfModalRodoviarioOS(AINIRec: TMemIniFile; rodoOS: TRodoOS);
    procedure Ler_InfCTeSub(AINIRec: TMemIniFile; infCteSub: TInfCteSub);
    procedure Ler_InfGTVe(AINIRec: TMemIniFile; infGTVe: TinfGTVeCollection);
    procedure Ler_Origem(AINIRec: TMemIniFile; origem: TEnderEmit);
    procedure Ler_Destino(AINIRec: TMemIniFile; destino: TEnderEmit);
    procedure Ler_DetalhamentoGTV(AINIRec: TMemIniFile; detGTV: TdetGTV);

    procedure Ler_ProcessamentoCTe(AINIRec: TMemIniFile; procCTe: TProcCTe);

    // Reforma Tributária
    procedure Ler_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS);
    procedure Ler_IBSCBS_gIBSCBS(AINIRec: TMemIniFile; gIBSCBS: TgIBSCBS);
    procedure Ler_gIBSUF(AINIRec: TMemIniFile; gIBSUF: TgIBSUFValores);
    procedure Ler_gIBSMun(AINIRec: TMemIniFile; gIBSMun: TgIBSMunValores);
    procedure Ler_gCBS(AINIRec: TMemIniFile; gCBS: TgCBSValores);
    procedure Ler_gTribReg(AINIRec: TMemIniFile; gTribRegular: TgTribRegular);
    procedure Ler_gIBSCredPres(AINIRec: TMemIniFile; gIBSCredPres: TgIBSCBSCredPres);
    procedure Ler_gCBSCredPres(AINIRec: TMemIniFile; gCBSCredPres: TgIBSCBSCredPres);
    procedure Ler_gTribCompraGov(AINIRec: TMemIniFile; gTribCompraGov: TgTribCompraGov);
  public
    constructor Create(AOwner: TCTe); reintroduce;

    function LerIni(const AIniString: string): Boolean;

    property CTe: TCTe read FCTe write FCTe;
    property VersaoDF: TVersaoCTe read FVersaoDF write FVersaoDF;
    property Ambiente: Integer read FAmbiente write FAmbiente;
    property tpEmis: Integer read FtpEmis write FtpEmis;
  end;

implementation

uses
  StrUtils,
  ACBrDFe.Conversao,
  ACBrCTe,
  ACBrXmlBase,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime;

{ TCTeIniReader }

constructor TCTeIniReader.Create(AOwner: TCTe);
begin
  inherited Create;

  FCTe := AOwner;
end;

function TCTeIniReader.FimLoop(const Campo: string): Boolean;
begin
  Result := (Campo = 'FIM') or (Trim(Campo) = '');
end;

function TCTeIniReader.LerIni(const AIniString: string): Boolean;
var
  INIRec: TMemIniFile;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    FCTe.infCTe.versao := StringToFloatDef(INIRec.ReadString('infCTe', 'versao', VersaoCTeToStr(VersaoDF)),0);

    Ler_Identificacao(INIRec, FCTe.Ide);

    case FCTe.Ide.modelo of
      57:
        begin
          if FCTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl] then
            Ler_CTeSimp(INIRec)
          else
            Ler_CTe(INIRec);
        end;
      64: Ler_GTVe(INIRec);
      67: Ler_CTeOS(INIRec);
    end;

    FCTe.ide.cUF := INIRec.ReadInteger('ide', 'cUF', UFparaCodigoUF(FCTe.Emit.enderEmit.UF));

    if INIRec.ReadString('infCteAnu','chCte','') <> '' then
    begin
      FCTe.InfCTeAnu.chCTe := INIRec.ReadString('infCteAnu','chCte','');
      FCTe.InfCTeAnu.dEmi  := StringToDateTime(INIRec.ReadString('infCteAnu','dEmi','0'));
    end;

    Ler_ProcessamentoCTe(INIRec, FCTe.procCTe);
  finally
    INIRec.Free;
  end;
end;

procedure TCTeIniReader.Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
var
  OK: Boolean;
begin
  Ide.cCT    := AINIRec.ReadInteger('ide','cCT', 0);
  Ide.cUF    := AINIRec.ReadInteger('ide','cUF', 0);
  Ide.CFOP   := AINIRec.ReadInteger('ide','CFOP',0);
  Ide.natOp  := AINIRec.ReadString('ide','natOp',EmptyStr);
  Ide.forPag := StrTotpforPag(OK,AINIRec.ReadString('ide','forPag','0'));
  Ide.modelo := AINIRec.ReadInteger('ide','mod' ,57);
  Ide.serie   := AINIRec.ReadInteger('ide','serie'  ,1);
  Ide.nCT     := AINIRec.ReadInteger('ide','nCT' ,0);
  Ide.dhEmi   := StringToDateTime(AINIRec.ReadString('ide','dhEmi','0'));
  Ide.tpImp   := StrToTpImp(OK, AINIRec.ReadString('ide','tpImp', '1'));
  Ide.tpEmis  := StrToTpEmis(OK, AINIRec.ReadString('ide', 'tpEmis', IntToStr(tpEmis)));
  Ide.tpAmb   := StrToTpAmb(OK, AINIRec.ReadString('ide', 'tpAmb', IntToStr(Ambiente)));
  Ide.tpCTe   := StrTotpCTe(OK, AINIRec.ReadString('ide', 'tpCTe', '0'));
  Ide.procEmi := StrToProcEmi(OK,AINIRec.ReadString('ide','procEmi','0'));
  Ide.verProc := AINIRec.ReadString('ide','verProc' ,'ACBrCTe');
  Ide.refCTe  := AINIRec.ReadString('ide','refCTe','');
  Ide.cMunEnv := AINIRec.ReadInteger('ide','cMunEnv',0);
  Ide.xMunEnv := AINIRec.ReadString('ide','xMunEnv','');
  Ide.UFEnv   := AINIRec.ReadString('ide','UFEnv','');
  Ide.modal   := StrToTpModal(OK, AINIRec.ReadString('ide','modal','01'));
  Ide.tpServ  := StrToTpServ(OK,AINIRec.ReadString('ide','tpServ','0'));
  Ide.cMunIni := AINIRec.ReadInteger('ide','cMunIni',0);
  Ide.xMunIni := AINIRec.ReadString('ide','xMunIni','');
  Ide.UFIni   := AINIRec.ReadString('ide','UFIni','');
  Ide.cMunFim := AINIRec.ReadInteger('ide','cMunFim',0);
  Ide.xMunFim := AINIRec.ReadString('ide','xMunFim','');
  Ide.UFFim   := AINIRec.ReadString('ide','UFFim','');
  Ide.retira  := StrToTpRetira(OK,AINIRec.ReadString('ide','retira','0'));

  if AINIRec.ReadString('ide','xDetRetira','') <> '' then
    Ide.xDetRetira := AINIRec.ReadString('ide','xDetRetira','');

  Ide.dhCont := StringToDateTime(AINIRec.ReadString('ide','dhCont'  ,'0'));
  Ide.xJust  := AINIRec.ReadString('ide','xJust' ,'');

  Ide.toma03.Toma    := StrToTpTomador(OK,AINIRec.ReadString('toma3','toma','0'));
  Ide.indGlobalizado := StrToTIndicador(OK, AINIRec.ReadString('ide','indGlobalizado','0'));
  Ide.indIEToma      := StrToindIEDest(OK, AINIRec.ReadString('ide','indIEToma','1'));
  // GTV-e
  Ide.dhSaidaOrig := StringToDateTime(AINIRec.ReadString('ide','dhSaidaOrig'  ,'0'));
  Ide.dhChegadaDest := StringToDateTime(AINIRec.ReadString('ide','dhChegadaDest'  ,'0'));

  Ide.gCompraGov.pRedutor := StringToFloatDef(AINIRec.ReadString('ide', 'pRedutor', ''), 0);

  if Ide.gCompraGov.pRedutor > 0 then
    Ide.gCompraGov.tpEnteGov := StrTotpEnteGov(AINIRec.ReadString('ide', 'tpEnteGov', ''));
end;

procedure TCTeIniReader.Ler_CTe(AINIRec: TMemIniFile);
begin
  Ler_Tomador4(AINIRec, FCTe.Ide.toma4);
  Ler_Complemento(AINIRec, FCTe.Compl);
  Ler_ComplementoPassagem(AINIRec, FCTe.Compl.fluxo.pass);
  Ler_ComplementoObsCont(AINIRec, FCTe.Compl.ObsCont);
  Ler_ComplementoObsFisco(AINIRec, FCTe.Compl.ObsFisco);
  Ler_Emitente(AINIRec, FCTe.Emit);
  Ler_Remetente(AINIRec, FCTe.Rem);
  Ler_LocalColeta(AINIRec, FCTe.Rem.locColeta);
  Ler_Expedidor(AINIRec, FCTe.exped);
  Ler_Recebedor(AINIRec, FCTe.receb);
  Ler_Destinatario(AINIRec, FCTe.dest);
  Ler_ValorPrestacao(AINIRec, FCTe.vPrest);
  Ler_ValorPrestacaoComposicao(AINIRec, FCTe.vPrest.Comp);
  Ler_Imposto(AINIRec, FCTe.Imp);

  case FCTe.ide.tpCTe of
    tcNormal, 
	tcSubstituto:
      Ler_InfCTeNormal(AINIRec, FCTe.infCTeNorm);
	  
    tcComplemento:
      begin
        Ler_InfCTeComplemento(AINIRec, FCTe.infCteComp);
        Ler_InfCTeComplemento10(AINIRec, FCTe.infCteComp10);
      end;
  end;

  Ler_AutorizadosXml(AINIRec, FCTe.autXML);
  Ler_InfRespTecnico(AINIRec, FCTe.infRespTec);
  Ler_InfCTeSupl(AINIRec, FCTe.infCTeSupl);
end;

procedure TCTeIniReader.Ler_CTeSimp(AINIRec: TMemIniFile);
begin
  Ler_Complemento(AINIRec, FCTe.Compl);
  Ler_ComplementoPassagem(AINIRec, FCTe.Compl.fluxo.pass);
  Ler_ComplementoObsCont(AINIRec, FCTe.Compl.ObsCont);
  Ler_ComplementoObsFisco(AINIRec, FCTe.Compl.ObsFisco);
  Ler_Emitente(AINIRec, FCTe.emit);
  Ler_Tomador(AINIRec, FCTe.toma);
  Ler_InfCarga(AINIRec, FCTe.infCarga);
  Ler_Detalhamento(AINIRec, FCTe.det);
  Ler_InfModalRodoviario(AINIRec, FCTe.infmodal.rodo);
  Ler_InfModalAereo(AINIRec, FCTe.infmodal.aereo);
  Ler_InfModalAquav(AINIRec, FCTe.infmodal.aquav);
  Ler_InfCTeNormalCobr(AINIRec, FCTe.cobr);
  Ler_InfCTeNormalInfCTeSub(AINIRec, FCTe.infCteSub);
  Ler_Imposto(AINIRec, FCTe.imp);
  Ler_Total(AINIRec, FCTe.total);
  Ler_AutorizadosXml(AINIRec, FCTe.autXML);
  Ler_InfRespTecnico(AINIRec, FCTe.infRespTec);
  Ler_InfCTeSupl(AINIRec, FCTe.infCTeSupl);
end;

procedure TCTeIniReader.Ler_CTeOS(AINIRec: TMemIniFile);
begin
  Ler_InfPercurso(AINIRec, FCTe.ide.infPercurso);
  Ler_Complemento(AINIRec, FCTe.Compl);
  Ler_ComplementoPassagem(AINIRec, FCTe.Compl.fluxo.pass);
  Ler_ComplementoObsCont(AINIRec, FCTe.Compl.ObsCont);
  Ler_ComplementoObsFisco(AINIRec, FCTe.Compl.ObsFisco);
  Ler_Emitente(AINIRec, FCTe.emit);
  Ler_Tomador(AINIRec, FCTe.toma);
  Ler_ValorPrestacao(AINIRec, FCTe.vPrest);
  Ler_Imposto(AINIRec, FCTe.imp);

  FCTe.infCTeNorm.refCTeCanc := AINIRec.ReadString('infCTeNorm', 'refCTeCanc', '');

  case FCTe.ide.tpCTe of
    tcNormal:
      begin
        Ler_InfServico(AINIRec, FCTe.infCTeNorm.infServico);
        Ler_InfDocReferencia(AINIRec, FCTe.infCTeNorm.infDocRef);
        Ler_Seguro(AINIRec, FCTe.infCTeNorm.seg);
        Ler_InfModalRodoviarioOS(AINIRec, FCTe.infCTeNorm.rodoOS);
        Ler_InfCTeSub(AINIRec, FCTe.infCTeNorm.infCteSub);
        Ler_InfCTeNormalCobr(AINIRec, FCTe.infCTeNorm.cobr);
        Ler_InfGTVe(AINIRec, FCTe.infCTeNorm.infGTVe);
      end;
    tcComplemento:
      begin
        Ler_InfCTeComplemento(AINIRec, FCTe.infCteComp);
        Ler_InfCTeComplemento10(AINIRec, FCTe.infCteComp10);
      end;
  end;

  Ler_AutorizadosXml(AINIRec, FCTe.autXML);
  Ler_InfRespTecnico(AINIRec, FCTe.infRespTec);
  Ler_InfCTeSupl(AINIRec, FCTe.infCTeSupl);
end;

procedure TCTeIniReader.Ler_GTVe(AINIRec: TMemIniFile);
begin
  Ler_Complemento(AINIRec, FCTe.Compl);
  Ler_ComplementoPassagem(AINIRec, FCTe.Compl.fluxo.pass);
  Ler_ComplementoObsCont(AINIRec, FCTe.Compl.ObsCont);
  Ler_ComplementoObsFisco(AINIRec, FCTe.Compl.ObsFisco);
  Ler_Emitente(AINIRec, FCTe.emit);
  Ler_Remetente(AINIRec, FCTe.Rem);
  Ler_Destinatario(AINIRec, FCTe.dest);
  Ler_Origem(AINIRec, FCTe.origem);
  Ler_Destino(AINIRec, FCTe.destino);
  Ler_DetalhamentoGTV(AINIRec, FCTe.detGTV);
  Ler_AutorizadosXml(AINIRec, FCTe.autXML);
  Ler_InfRespTecnico(AINIRec, FCTe.infRespTec);
  Ler_InfCTeSupl(AINIRec, FCTe.infCTeSupl);
end;

procedure TCTeIniReader.Ler_Tomador4(AINIRec: TMemIniFile; toma4: TToma4);
var
  Ok: Boolean;
begin
  if AINIRec.ReadString('toma4','xNome','') <> '' then
  begin
    toma4.toma    := StrToTpTomador(OK,AINIRec.ReadString('toma4','toma','0'));
    toma4.CNPJCPF := AINIRec.ReadString('toma4','CNPJCPF','');
    toma4.IE      := AINIRec.ReadString('toma4','IE','');
    toma4.xNome   := AINIRec.ReadString('toma4','xNome','');
    toma4.xFant   := AINIRec.ReadString('toma4','xFant','');
    toma4.fone    := AINIRec.ReadString('toma4','fone','');

    with toma4.enderToma do
    begin
      xLgr    := AINIRec.ReadString('toma4','xLgr','');
      nro     := AINIRec.ReadString('toma4','nro','');
      xCpl    := AINIRec.ReadString('toma4','xCpl','');
      xBairro := AINIRec.ReadString('toma4','xBairro','');
      cMun    := AINIRec.ReadInteger('toma4','cMun',0);
      xMun    := AINIRec.ReadString('toma4','xMun','');
      CEP     := AINIRec.ReadInteger('toma4','CEP',0);
      UF      := AINIRec.ReadString('toma4','UF','');
      cPais   := AINIRec.ReadInteger('toma4','cPais',0);
      xPais   := AINIRec.ReadString('toma4','xPais','');
    end;

    toma4.email := AINIRec.ReadString('toma4','email','');
  end;
end;

procedure TCTeIniReader.Ler_Complemento(AINIRec: TMemIniFile; Compl: TCompl);
var
  Ok: Boolean;
begin
  Compl.xCaracAd  := AINIRec.ReadString('compl','xCaracAd', '');
  Compl.xCaracSer := AINIRec.ReadString('compl','xCaracSer','' );
  Compl.xEmi      := AINIRec.ReadString('compl','xEmi','');

  Compl.fluxo.xOrig := AINIRec.ReadString('compl','xOrig','');
  Compl.fluxo.xDest := AINIRec.ReadString('compl','xDest','');
  Compl.fluxo.xRota := AINIRec.ReadString('compl','xRota','');

  Compl.Entrega.TipoData := StrToTpDataPeriodo(ok,AINIRec.ReadString('compl','TipoData','-1'));
  case Compl.Entrega.TipoData of
   tdSemData:
      begin
        Compl.Entrega.semData.tpPer := StrToTpDataPeriodo(ok,AINIRec.ReadString('compl','tpPer','0'));
      end;
   tdNaData,tdAteData,tdApartirData:
      begin
        Compl.Entrega.comData.tpPer := StrToTpDataPeriodo(ok,AINIRec.ReadString('compl','tpPer','0'));
        Compl.Entrega.comData.dProg := StringToDateTime(AINIRec.ReadString('compl','dProg','0'));
      end;
   tdNoPeriodo:
      begin
        Compl.Entrega.noPeriodo.tpPer := StrToTpDataPeriodo(ok,AINIRec.ReadString('compl','tpPer','0'));
        Compl.Entrega.noPeriodo.dIni  := StringToDateTime(AINIRec.ReadString('compl','dIni','0'));
        Compl.Entrega.noPeriodo.dFim  := StringToDateTime(AINIRec.ReadString('compl','dFim','0'));
      end;
  end;

  Compl.Entrega.TipoHora := StrToTpHorarioIntervalo(ok,AINIRec.ReadString('compl','TipoHora','-1'));
  case Compl.Entrega.TipoHora of
   thSemHorario:
      begin
        Compl.Entrega.semHora.tpHor := StrToTpHorarioIntervalo(ok,AINIRec.ReadString('compl','tpHor','0'));
      end;
   thNoHorario,thAteHorario,thApartirHorario:
      begin
        Compl.Entrega.comHora.tpHor := StrToTpHorarioIntervalo(ok,AINIRec.ReadString('compl','tpHor','0'));
        Compl.Entrega.comHora.hProg := StrToTime(AINIRec.ReadString('compl','hProg','0'));
      end;
   thNoIntervalo:
      begin
        Compl.Entrega.noInter.tpHor := StrToTpHorarioIntervalo(ok,AINIRec.ReadString('compl','tpHor','0'));
        Compl.Entrega.noInter.hIni  := StrToTime(AINIRec.ReadString('compl','hIni','0'));
        Compl.Entrega.noInter.hFim  := StrToTime(AINIRec.ReadString('compl','hFim','0'));
      end;
  end;

  Compl.origCalc := AINIRec.ReadString('compl','origCalc','');
  Compl.destCalc := AINIRec.ReadString('compl','destCalc','');
  Compl.xObs     := AINIRec.ReadString('compl','xObs','');
end;

procedure TCTeIniReader.Ler_ComplementoPassagem(AINIRec: TMemIniFile;
  pass: TPassCollection);
var
  I: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'PASS'+IntToStrZero(I,3);
    sFim   := AINIRec.ReadString(sSecao,'xPass','FIM');
    if FimLoop(sFim) then
      break;

    pass.New.xPass := AINIRec.ReadString(sSecao,'xPass','');
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_ComplementoObsCont(AINIRec: TMemIniFile;
  ObsCont: TObsContCollection);
var
  I: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao     := 'ObsCont'+IntToStrZero(I,3);
    sFim := AINIRec.ReadString(sSecao,'Campo',AINIRec.ReadString(sSecao,'xCampo','FIM'));
    if FimLoop(sFim) then
      break;

    with ObsCont.New do
    begin
      xCampo := sFim;
      xTexto := AINIRec.ReadString(sSecao,'Texto',AINIRec.ReadString(sSecao,'xTexto',''));
    end;
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_ComplementoObsFisco(AINIRec: TMemIniFile;
  ObsFisco: TObsFiscoCollection);
var
  I: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao     := 'ObsFisco'+IntToStrZero(I,3);
    sFim := AINIRec.ReadString(sSecao,'Campo',AINIRec.ReadString(sSecao,'xCampo','FIM'));
    if FimLoop(sFim) then
      break;

    with ObsFisco.New do
    begin
      xCampo := sFim;
      xTexto := AINIRec.ReadString(sSecao,'Texto',AINIRec.ReadString(sSecao,'xTexto',''));
    end;
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
var
  Ok: Boolean;
begin
  Emit.CNPJ  := AINIRec.ReadString('emit','CNPJ','');
  Emit.IE    := AINIRec.ReadString('emit','IE','');
  Emit.xNome := AINIRec.ReadString('emit','xNome','');
  Emit.xFant := AINIRec.ReadString('emit','xFant','');
  Emit.CRT   := StrToCRTCTe(ok, AINIRec.ReadString('emit','CRT', ''));

  Emit.enderEmit.xLgr    := AINIRec.ReadString('emit','xLgr','');
  Emit.enderEmit.nro     := AINIRec.ReadString('emit','nro','');
  Emit.enderEmit.xCpl    := AINIRec.ReadString('emit', 'xCpl','');
  Emit.enderEmit.xBairro := AINIRec.ReadString('emit','xBairro','');
  Emit.enderEmit.cMun    := AINIRec.ReadInteger('emit','cMun',0);
  Emit.enderEmit.xMun    := AINIRec.ReadString('emit','xMun','');
  Emit.enderEmit.CEP     := AINIRec.ReadInteger('emit','CEP',0);
  Emit.enderEmit.UF      := AINIRec.ReadString('emit','UF','');
  Emit.enderEmit.fone    := AINIRec.ReadString('emit','fone','');
end;

procedure TCTeIniReader.Ler_Remetente(AINIRec: TMemIniFile; Rem: TRem);
begin
  Rem.CNPJCPF := AINIRec.ReadString('rem','CNPJCPF','');
  Rem.IE      := AINIRec.ReadString('rem','IE','');
  Rem.xNome   := AINIRec.ReadString('rem','xNome','');
  Rem.xFant   := AINIRec.ReadString('rem','xFant','');
  Rem.fone    := AINIRec.ReadString('rem','fone','');

  Rem.enderReme.xLgr    := AINIRec.ReadString('rem','xLgr','');
  Rem.enderReme.nro     := AINIRec.ReadString('rem','nro','');
  Rem.enderReme.xCpl    := AINIRec.ReadString('rem','xCpl','');
  Rem.enderReme.xBairro := AINIRec.ReadString('rem','xBairro','');
  Rem.enderReme.cMun    := AINIRec.ReadInteger('rem','cMun',0);
  Rem.enderReme.xMun    := AINIRec.ReadString('rem','xMun','');
  Rem.enderReme.CEP     := AINIRec.ReadInteger('rem','CEP',0);
  Rem.enderReme.UF      := AINIRec.ReadString('rem','UF','');
  Rem.enderReme.cPais   := AINIRec.ReadInteger('rem','cPais'    ,1058);
  Rem.enderReme.xPais   := AINIRec.ReadString('rem','xPais'    ,'BRASIL');
  Rem.email             := AINIRec.ReadString('rem','email' ,'');
end;

procedure TCTeIniReader.Ler_LocalColeta(AINIRec: TMemIniFile;
  locColeta: TLocColeta);
begin
  locColeta.CNPJCPF := AINIRec.ReadString('locColeta','CNPJCPF','');
  locColeta.xNome   := AINIRec.ReadString('locColeta','xNome','');
  locColeta.xLgr    := AINIRec.ReadString('locColeta','xLgr','');
  locColeta.nro     := AINIRec.ReadString('locColeta','nro','');
  locColeta.xCpl    := AINIRec.ReadString('locColeta','xCpl','');
  locColeta.xBairro := AINIRec.ReadString('locColeta','xBairro','');
  locColeta.cMun    := AINIRec.ReadInteger('locColeta','cMun',0);
  locColeta.xMun    := AINIRec.ReadString('locColeta','xMun','');
  locColeta.uf      := AINIRec.ReadString('locColeta','UF','');
end;

procedure TCTeIniReader.Ler_Expedidor(AINIRec: TMemIniFile; exped: TExped);
begin
  Exped.CNPJCPF := AINIRec.ReadString('Exped','CNPJCPF','');
  Exped.IE      := AINIRec.ReadString('Exped','IE','');
  Exped.xNome   := AINIRec.ReadString('Exped','xNome','');
  Exped.fone    := AINIRec.ReadString('Exped','fone','');
  Exped.email   := AINIRec.ReadString('Exped','email','');

  Exped.enderExped.xLgr    := AINIRec.ReadString('Exped','xLgr','');
  Exped.enderExped.nro     := AINIRec.ReadString('Exped','nro','');
  Exped.enderExped.xCpl    := AINIRec.ReadString('Exped','xCpl','');
  Exped.enderExped.xBairro := AINIRec.ReadString('Exped','xBairro','');
  Exped.enderExped.cMun    := AINIRec.ReadInteger('Exped','cMun',0);
  Exped.enderExped.xMun    := AINIRec.ReadString('Exped','xMun','');
  Exped.enderExped.CEP     := AINIRec.ReadInteger('Exped', 'CEP',0);
  Exped.enderExped.UF      := AINIRec.ReadString('Exped','UF','');
  Exped.enderExped.cPais   := AINIRec.ReadInteger('Exped','cPais',1058);
  Exped.enderExped.xPais   := AINIRec.ReadString('Exped', 'xPais', 'BRASIL');
end;

procedure TCTeIniReader.Ler_Recebedor(AINIRec: TMemIniFile; receb: TReceb);
begin
  Receb.CNPJCPF := AINIRec.ReadString('Receb','CNPJCPF','');
  Receb.IE      := AINIRec.ReadString('Receb','IE','');
  Receb.xNome   := AINIRec.ReadString('Receb','xNome','');
  Receb.fone    := AINIRec.ReadString('Receb','fone','');
  Receb.email   := AINIRec.ReadString('Receb','email','');

  Receb.enderReceb.xLgr    := AINIRec.ReadString('Receb','xLgr','');
  Receb.enderReceb.nro     := AINIRec.ReadString('Receb','nro','');
  Receb.enderReceb.xCpl    := AINIRec.ReadString('Receb','xCpl','');
  Receb.enderReceb.xBairro := AINIRec.ReadString('Receb','xBairro','');
  Receb.enderReceb.cMun    := AINIRec.ReadInteger('Receb','cMun',0);
  Receb.enderReceb.xMun    := AINIRec.ReadString('Receb','xMun','');
  Receb.enderReceb.CEP     := AINIRec.ReadInteger('Receb', 'CEP',0);
  Receb.enderReceb.UF      := AINIRec.ReadString('Receb','UF','');
  Receb.enderReceb.cPais   := AINIRec.ReadInteger('Receb','cPais',1058);
  Receb.enderReceb.xPais   := AINIRec.ReadString('Receb', 'xPais', 'BRASIL');
end;

procedure TCTeIniReader.Ler_Destinatario(AINIRec: TMemIniFile; dest: TDest);
begin
  Dest.CNPJCPF := AINIRec.ReadString('Dest','CNPJCPF','');
  Dest.IE      := AINIRec.ReadString('Dest','IE','');
  Dest.xNome   := AINIRec.ReadString('Dest','xNome','');
  Dest.fone    := AINIRec.ReadString('Dest','fone','');
  Dest.email   := AINIRec.ReadString('Dest','email','');
  Dest.ISUF    := AINIRec.ReadString('Dest','ISUF','');

  Dest.enderDest.xLgr    := AINIRec.ReadString('Dest','xLgr','');
  Dest.enderDest.nro     := AINIRec.ReadString('Dest','nro','');
  Dest.enderDest.xCpl    := AINIRec.ReadString('Dest', 'xCpl', '');
  Dest.enderDest.xBairro := AINIRec.ReadString('Dest','xBairro','');
  Dest.enderDest.cMun    := AINIRec.ReadInteger('Dest','cMun',0);
  Dest.enderDest.xMun    := AINIRec.ReadString('Dest','xMun','');
  Dest.enderDest.CEP     := AINIRec.ReadInteger('Dest', 'CEP',0);
  Dest.enderDest.UF      := AINIRec.ReadString('Dest','UF','');
  Dest.enderDest.cPais   := AINIRec.ReadInteger('Dest','cPais',1058);
  Dest.enderDest.xPais   := AINIRec.ReadString('Dest', 'xPais', 'BRASIL');

  Dest.locEnt.CNPJCPF := AINIRec.ReadString('locEnt','CNPJCPF','');
  Dest.locEnt.xNome   := AINIRec.ReadString('locEnt','xNome','');
  Dest.locEnt.xLgr    := AINIRec.ReadString('locEnt','xLgr','');
  Dest.locEnt.nro     := AINIRec.ReadString('locEnt','nro','');
  Dest.locEnt.xCpl    := AINIRec.ReadString('locEnt','xCpl','');
  Dest.locEnt.xBairro := AINIRec.ReadString('locEnt','xBairro','');
  Dest.locEnt.cMun    := AINIRec.ReadInteger('locEnt','cMun',0);
  Dest.locEnt.xMun    := AINIRec.ReadString('locEnt','xMun','');
  Dest.locEnt.uf      := AINIRec.ReadString('locEnt','UF','');
end;

procedure TCTeIniReader.Ler_ValorPrestacao(AINIRec: TMemIniFile;
  vPrest: TVPrest);
begin
  vPrest.vTPrest := StringToFloatDef(AINIRec.ReadString('vPrest','vTPrest','') ,0);
  vPrest.vRec    := StringToFloatDef(AINIRec.ReadString('vPrest','vRec','') ,0);
end;

procedure TCTeIniReader.Ler_ValorPrestacaoComposicao(AINIRec: TMemIniFile;
  Comp: TCompCollection);
var
  I: Integer;
  sSecao, sFim: string;
begin
  if not (FCTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl]) then
  begin
    I := 1;
    while true do
    begin
      sSecao := 'Comp'+IntToStrZero(I,3);
      sFim   := AINIRec.ReadString(sSecao,'xNome','FIM');
      if FimLoop(sFim) then
        break;

      with comp.New do
      begin
        xNome := AINIRec.ReadString(sSecao,'xNome','');
        vComp := StringToFloatDef(AINIRec.ReadString(sSecao,'vComp','') ,0);
      end;
      Inc(I);
    end;
  end;
end;

procedure TCTeIniReader.Ler_Imposto(AINIRec: TMemIniFile; Imp: TImp);
var
  Ok: Boolean;
begin
  Imp.vTotTrib   := StringToFloatDef(AINIRec.ReadString('Imp','vTotTrib',AINIRec.ReadString('ICMS','vTotTrib','')) ,0);
  Imp.infAdFisco := AINIRec.ReadString('Imp','infAdFisco',AINIRec.ReadString('ICMS','infAdFisco',''));

  if AINIRec.ReadString('ICMS00', 'CST','') <> '' then
  begin
    Imp.ICMS.ICMS00.CST   := StrToCSTICMS(OK,AINIRec.ReadString('ICMS00','CST','00'));
    imp.ICMS.SituTrib     := Imp.ICMS.ICMS00.CST;
    Imp.ICMS.ICMS00.vBC   := StringToFloatDef(AINIRec.ReadString('ICMS00','vBC','') ,0);
    Imp.ICMS.ICMS00.pICMS := StringToFloatDef(AINIRec.ReadString('ICMS00','pICMS','') ,0);
    Imp.ICMS.ICMS00.vICMS := StringToFloatDef(AINIRec.ReadString('ICMS00','vICMS','') ,0);
  end;

  if AINIRec.ReadString('ICMS20', 'CST','') <> '' then
  begin
    Imp.ICMS.ICMS20.CST    := StrToCSTICMS(OK,AINIRec.ReadString('ICMS20','CST','00'));
    imp.ICMS.SituTrib      := Imp.ICMS.ICMS20.CST;
    Imp.ICMS.ICMS20.pRedBC := StringToFloatDef(AINIRec.ReadString('ICMS20','pRedBC','') ,0);
    Imp.ICMS.ICMS20.vBC    := StringToFloatDef(AINIRec.ReadString('ICMS20','vBC','') ,0);
    Imp.ICMS.ICMS20.pICMS  := StringToFloatDef(AINIRec.ReadString('ICMS20','pICMS','') ,0);
    Imp.ICMS.ICMS20.vICMS  := StringToFloatDef(AINIRec.ReadString('ICMS20','vICMS','') ,0);

    Imp.ICMS.ICMS20.vICMSDeson := StringToFloatDef(AINIRec.ReadString('ICMS20','vICMSDeson','') ,0);
    Imp.ICMS.ICMS20.cBenef     := AINIRec.ReadString('ICMS20','cBenef','');
  end;

  if AINIRec.ReadString('ICMS45','CST','') <> '' then
  begin
    Imp.ICMS.ICMS45.CST := StrToCSTICMS(OK,AINIRec.ReadString('ICMS45','CST','40'));
    imp.ICMS.SituTrib   := Imp.ICMS.ICMS45.CST;

    Imp.ICMS.ICMS45.vICMSDeson := StringToFloatDef(AINIRec.ReadString('ICMS45','vICMSDeson','') ,0);
    Imp.ICMS.ICMS45.cBenef     := AINIRec.ReadString('ICMS45','cBenef','');
   end;

  if AINIRec.ReadString('ICMS60', 'CST','') <> '' then
  begin
    Imp.ICMS.ICMS60.CST        := StrToCSTICMS(OK,AINIRec.ReadString('ICMS60','CST','60'));
    imp.ICMS.SituTrib          := Imp.ICMS.ICMS60.CST;
    Imp.ICMS.ICMS60.vBCSTRet   := StringToFloatDef(AINIRec.ReadString('ICMS60','vBCSTRet','') ,0);
    Imp.ICMS.ICMS60.vICMSSTRet := StringToFloatDef(AINIRec.ReadString('ICMS60','vICMSSTRet','') ,0);
    Imp.ICMS.ICMS60.pICMSSTRet := StringToFloatDef(AINIRec.ReadString('ICMS60','pICMSSTRet','') ,0);
    Imp.ICMS.ICMS60.vCred      := StringToFloatDef(AINIRec.ReadString('ICMS60','vCred','') ,0);

    Imp.ICMS.ICMS60.vICMSDeson := StringToFloatDef(AINIRec.ReadString('ICMS60','vICMSDeson','') ,0);
    Imp.ICMS.ICMS60.cBenef     := AINIRec.ReadString('ICMS60','cBenef','');
  end;

  if AINIRec.ReadString('ICMS90', 'CST','') <> '' then
  begin
    Imp.ICMS.ICMS90.CST    := StrToCSTICMS(OK,AINIRec.ReadString('ICMS90','CST','90'));
    imp.ICMS.SituTrib      := Imp.ICMS.ICMS90.CST;
    Imp.ICMS.ICMS90.pRedBC := StringToFloatDef(AINIRec.ReadString('ICMS90','pRedBC','') ,0);
    Imp.ICMS.ICMS90.vBC    := StringToFloatDef(AINIRec.ReadString('ICMS90','vBC','') ,0);
    Imp.ICMS.ICMS90.pICMS  := StringToFloatDef(AINIRec.ReadString('ICMS90','pICMS','') ,0);
    Imp.ICMS.ICMS90.vICMS  := StringToFloatDef(AINIRec.ReadString('ICMS90','vICMS','') ,0);
    Imp.ICMS.ICMS90.vCred  := StringToFloatDef(AINIRec.ReadString('ICMS90','vCred','') ,0);

    Imp.ICMS.ICMS90.vICMSDeson := StringToFloatDef(AINIRec.ReadString('ICMS90','vICMSDeson','') ,0);
    Imp.ICMS.ICMS90.cBenef     := AINIRec.ReadString('ICMS90','cBenef','');
  end;

  if AINIRec.ReadString('ICMSOutraUF', 'CST','') <> '' then
  begin
    Imp.ICMS.ICMSOutraUF.CST           := StrToCSTICMS(OK,AINIRec.ReadString('ICMSOutraUF','CST','90'));
    imp.ICMS.SituTrib                  := cstICMSOutraUF;
    Imp.ICMS.ICMSOutraUF.pRedBCOutraUF := StringToFloatDef(AINIRec.ReadString('ICMSOutraUF','pRedBCOutraUF','') ,0);
    Imp.ICMS.ICMSOutraUF.vBCOutraUF    := StringToFloatDef(AINIRec.ReadString('ICMSOutraUF','vBCOutraUF','') ,0);
    Imp.ICMS.ICMSOutraUF.pICMSOutraUF  := StringToFloatDef(AINIRec.ReadString('ICMSOutraUF','pICMSOutraUF','') ,0);
    Imp.ICMS.ICMSOutraUF.vICMSOutraUF  := StringToFloatDef(AINIRec.ReadString('ICMSOutraUF','vICMSOutraUF','') ,0);

    Imp.ICMS.ICMSOutraUF.vICMSDeson := StringToFloatDef(AINIRec.ReadString('ICMSOutraUF','vICMSDeson','') ,0);
    Imp.ICMS.ICMSOutraUF.cBenef     := AINIRec.ReadString('ICMSOutraUF','cBenef','');
  end;

  if AINIRec.ReadInteger('ICMSSN', 'indSN',0) = 1 then
  begin
    imp.ICMS.SituTrib     := cstICMSSN;
    Imp.ICMS.ICMSSN.indSN := AINIRec.ReadInteger('ICMSSN', 'indSN',1);
  end;

  if (StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pICMSInterPart', ''), 0) <> 0) or
     (StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'vBCUFFim', ''), 0) <> 0) or
     (StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pFCPUFFim', ''), 0) <> 0) or
     (StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pICMSUFFim', ''), 0) <> 0) or
     (StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pICMSInter', ''), 0) <> 0) or
     (StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'vFCPUFFim', ''), 0) <> 0) or
     (StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'vICMSUFFim', ''), 0) <> 0) or
     (StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'vICMSUFIni', ''), 0) <> 0) then
  begin
    Imp.ICMSUFFim.vBCUFFim       := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'vBCUFFim', ''), 0);
    Imp.ICMSUFFim.pFCPUFFim      := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pFCPUFFim', ''), 0);
    Imp.ICMSUFFim.pICMSUFFim     := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pICMSUFFim', ''), 0);
    Imp.ICMSUFFim.pICMSInter     := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pICMSInter', ''), 0);
    Imp.ICMSUFFim.pICMSInterPart := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pICMSInterPart', ''), 0);
    Imp.ICMSUFFim.vFCPUFFim      := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'vFCPUFFim', ''), 0);
    Imp.ICMSUFFim.vICMSUFFim     := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'vICMSUFFim', ''), 0);
    Imp.ICMSUFFim.vICMSUFIni     := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'vICMSUFIni', ''), 0);
  end;

  //CT-e OS
  Imp.infTribFed.vPIS    := StringToFloatDef(AINIRec.ReadString('infTribFed', 'vPIS', ''), 0);
  Imp.infTribFed.vCOFINS := StringToFloatDef(AINIRec.ReadString('infTribFed', 'vCOFINS', ''), 0);
  Imp.infTribFed.vIR     := StringToFloatDef(AINIRec.ReadString('infTribFed', 'vIR', ''), 0);
  Imp.infTribFed.vINSS   := StringToFloatDef(AINIRec.ReadString('infTribFed', 'vINSS', ''), 0);
  Imp.infTribFed.vCSLL   := StringToFloatDef(AINIRec.ReadString('infTribFed', 'vCSLL', ''), 0);

  // Reforma Tributária
  Ler_IBSCBS(AINIRec, FCTe.Imp.IBSCBS);

  Imp.vTotDFe := StringToFloatDef(AINIRec.ReadString('Imp', 'vTotDFe', ''), 0);
end;

procedure TCTeIniReader.Ler_InfCTeNormal(AINIRec: TMemIniFile;
  infCTeNorm: TInfCTeNorm);
begin
  Ler_InfCarga(AINIRec, infCTeNorm.infCarga);

  Ler_InfCTeNormalInfNF(AINIRec, infCTeNorm.infDoc.infNF);
  Ler_InfCTeNormalInfNFe(AINIRec, infCTeNorm.infDoc.infNFe);
  Ler_InfCTeNormalInfOutros(AINIRec, infCTeNorm.infDoc.infOutros);
  Ler_InfCTeNormalInfDCe(AINIRec, infCTeNorm.infDoc.infDCe);

  Ler_InfCTeNormalDocAnteriores(AINIRec, infCTeNorm.docAnt);
  Ler_InfCTeNormalInfModal(AINIRec, infCTeNorm);
  Ler_InfCTeNormalVeiculosNovos(AINIRec, infCTeNorm.veicNovos);

  if not (FCTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl]) then
  begin
    Ler_InfCTeNormalCobr(AINIRec, infCTeNorm.cobr);
    Ler_InfCTeNormalInfCTeSub(AINIRec, infCTeNorm.infCteSub);
  end;

  Ler_InfCTeNormalInfGlobalizado(AINIRec, infCTeNorm.infGlobalizado);
  Ler_InfCTeNormalInfCTeMultimodal(AINIRec, infCTeNorm.infServVinc);
end;

procedure TCTeIniReader.Ler_InfCarga(AINIRec: TMemIniFile; infcarga: TInfCarga);
var
  I: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  infCarga.vCarga      := StringToFloatDef(AINIRec.ReadString('infCarga','vCarga','') ,0);
  infCarga.proPred     := AINIRec.ReadString('infCarga','proPred','');
  infCarga.xOutCat     := AINIRec.ReadString('infCarga','xOutCat','');
  infCarga.vCargaAverb := StringToFloatDef(AINIRec.ReadString('infCarga','vCargaAverb','') ,0);

  I := 1;
  while true do
  begin
    sSecao := 'infQ'+IntToStrZero(I,3);
    sFim   := AINIRec.ReadString(sSecao,'cUnid','FIM');
    if FimLoop(sFim) then
      break;
    with infCarga.infQ.New do
    begin
      cUnid  := StrToUnidMed(OK, sFim);
      tpMed  := AINIRec.ReadString(sSecao,'tpMed','');
      qCarga := StringToFloatDef(AINIRec.ReadString(sSecao,'qCarga','') ,0);
    end;
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_InfCTeNormalInfNF(AINIRec: TMemIniFile;
  infNF: TInfNFCollection);
var
  I, J, K, L: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  I := 1;
  while true do
  begin
    sSecao := 'infNF'+IntToStrZero(I,3);
    sFim   := AINIRec.ReadString(sSecao,'mod','FIM');
    if FimLoop(sFim) then
      break;

    with infNF.New do
    begin
      nRoma  := AINIRec.ReadString(sSecao,'nRoma','');
      nPed   := AINIRec.ReadString(sSecao,'nPed','');
      modelo := StrToModeloNFEX(AINIRec.ReadString(sSecao,'mod','01'));
      serie  := AINIRec.ReadString(sSecao,'serie','');
      nDoc   := AINIRec.ReadString(sSecao,'nDoc','');
      dEmi   := StringToDateTime(AINIRec.ReadString(sSecao,'dEmi','0'));
      vBC    := StringToFloatDef(AINIRec.ReadString(sSecao,'vBC','') ,0);
      vICMS  := StringToFloatDef(AINIRec.ReadString(sSecao,'vICMS','') ,0);
      vBCST  := StringToFloatDef(AINIRec.ReadString(sSecao,'vBCST','') ,0);
      vST    := StringToFloatDef(AINIRec.ReadString(sSecao,'vST','') ,0);
      vProd  := StringToFloatDef(AINIRec.ReadString(sSecao,'vProd','') ,0);
      vNF    := StringToFloatDef(AINIRec.ReadString(sSecao,'vNF','') ,0);
      nCFOP  := AINIRec.ReadInteger(sSecao,'nCFOP',0);
      nPeso  := StringToFloatDef(AINIRec.ReadString(sSecao,'nPeso','') ,0);
      PIN    := AINIRec.ReadString(sSecao,'PIN','');
      dPrev  := StringToDateTime(AINIRec.ReadString(sSecao,'dPrev','0'));

      J := 1;
      while true do
      begin
        sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3);
        sFim   := AINIRec.ReadString(sSecao,'idUnidTransp','FIM');

        if FimLoop(sFim) then
          break;

        with infUnidTransp.New do
        begin
          tpUnidTransp := StrToUnidTransp(OK,AINIRec.ReadString(sSecao,'tpUnidTransp','1'));
          idUnidTransp := AINIRec.ReadString(sSecao,'idUnidTransp','');
          qtdRat       := StringToFloatDef(AINIRec.ReadString(sSecao,'qtdRat',''),0);

          K := 1;
          while true do
          begin
            sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
            sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');

            if FimLoop(sFim) then
              break;

            lacUnidTransp.New.nLacre := AINIRec.ReadString(sSecao,'nLacre','');
            inc(K);
          end;

          K := 1;
          while true do
          begin
            sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
            sFim   := AINIRec.ReadString(sSecao,'idUnidCarga','FIM');

            if FimLoop(sFim) then
              break;

            with infUnidCarga.New do
            begin
              tpUnidCarga := StrToUnidCarga(OK,AINIRec.ReadString(sSecao,'tpUnidCarga','1'));
              idUnidCarga := AINIRec.ReadString(sSecao,'idUnidCarga','');
              qtdRat      := StringToFloatDef(AINIRec.ReadString(sSecao,'qtdRat',''),0);

              L := 1;
              while true do
              begin
                sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');

                if FimLoop(sFim) then
                  break;

                lacUnidCarga.New.nLacre := AINIRec.ReadString(sSecao,'nLacre','');
                inc(L);
              end;
            end;
            inc(K);
          end;
          inc(J);
        end;
      end;

      J := 1;
      while true do
      begin
        sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3);
        sFim   := AINIRec.ReadString(sSecao,'idUnidCarga','FIM');

        if FimLoop(sFim) then
          break;

        with infUnidCarga.New do
        begin
          tpUnidCarga := StrToUnidCarga(OK,AINIRec.ReadString(sSecao,'tpUnidCarga','1'));
          idUnidCarga := AINIRec.ReadString(sSecao,'idUnidCarga','');
          qtdRat      := StringToFloatDef(AINIRec.ReadString(sSecao,'qtdRat',''),0);

          K := 1;
          while true do
          begin
            sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
            sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');

            if FimLoop(sFim) then
              break;

            lacUnidCarga.New.nLacre := AINIRec.ReadString(sSecao,'nLacre','');
            inc(K);
          end;
        end;
        inc(J);
      end;
    end;
    inc(I);
  end;
end;

procedure TCTeIniReader.Ler_InfCTeNormalInfNFe(AINIRec: TMemIniFile;
  infNFe: TInfNFeCollection);
var
  I: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'infNFe'+IntToStrZero(I,3);
    sFim   := AINIRec.ReadString(sSecao,'chave','FIM');
    if FimLoop(sFim) then
      break;

    Ler_Secao_InfNFe(AINIRec, sSecao, I, -1, infNFe);

    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_Secao_InfNFe(AINIRec: TMemIniFile;const Secao: string;
  Nivel1, Nivel2: Integer; infNFe: TInfNFeCollection);
var
  Nivel, sSecao, sFim: string;
  J, K, L: Integer;
  Ok: Boolean;
begin
  with infNFe.New do
  begin
    chave := AINIRec.ReadString(Secao,'chave','');

    if chave = '' then
      chave := AINIRec.ReadString(Secao,'chNFe','');

    PIN   := AINIRec.ReadString(Secao,'PIN','');
    dPrev := StringToDateTime(AINIRec.ReadString(Secao,'dPrev','0'));

    if Nivel2 = -1 then
      Nivel := IntToStrZero(Nivel1, 3)
    else
      Nivel := IntToStrZero(Nivel1, 3) + IntToStrZero(Nivel2, 3);

    J := 1;
    while true do
    begin
      sSecao := 'infUnidTransp' + Nivel + IntToStrZero(J,3);
      sFim   := AINIRec.ReadString(sSecao,'idUnidTransp','FIM');
      if FimLoop(sFim) then
        break;
      with infUnidTransp.New do
      begin
        tpUnidTransp := StrToUnidTransp(OK,AINIRec.ReadString(sSecao,'tpUnidTransp','1'));
        idUnidTransp := AINIRec.ReadString(sSecao,'idUnidTransp','');
        qtdRat       := StringToFloatDef(AINIRec.ReadString(sSecao,'qtdRat',''),0);

        K := 1;
        while true do
        begin
          sSecao := 'lacUnidTransp' + Nivel + IntToStrZero(J,3) + IntToStrZero(K,3);
          sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');
          if FimLoop(sFim) then
            break;

          lacUnidTransp.New.nLacre := AINIRec.ReadString(sSecao,'nLacre','');
          inc(K);
        end;

        K := 1;
        while true do
        begin
          sSecao := 'infUnidCarga' + Nivel + IntToStrZero(J,3) + IntToStrZero(K,3);
          sFim   := AINIRec.ReadString(sSecao,'idUnidCarga','FIM');
          if FimLoop(sFim) then
            break;
          with infUnidCarga.New do
          begin
            tpUnidCarga := StrToUnidCarga(OK,AINIRec.ReadString(sSecao,'tpUnidCarga','1'));
            idUnidCarga := AINIRec.ReadString(sSecao,'idUnidCarga','');
            qtdRat      := StringToFloatDef(AINIRec.ReadString(sSecao,'qtdRat',''),0);

            L := 1;
            while true do
            begin
              sSecao := 'lacUnidCarga' + Nivel + IntToStrZero(J,3) + IntToStrZero(K,3) +
                                         IntToStrZero(L,3);
              sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');
              if FimLoop(sFim) then
                break;

              lacUnidCarga.New.nLacre := AINIRec.ReadString(sSecao,'nLacre','');
              inc(L);
            end;
            inc(K);
          end;
        end;
      end;
      inc(J);
    end;

    J := 1;
    while true do
    begin
      sSecao := 'infUnidCarga' + Nivel + IntToStrZero(J,3);
      sFim   := AINIRec.ReadString(sSecao,'idUnidCarga','FIM');
      if FimLoop(sFim) then
        break;
      with infUnidCarga.New do
      begin
        tpUnidCarga := StrToUnidCarga(OK,AINIRec.ReadString(sSecao,'tpUnidCarga','1'));
        idUnidCarga := AINIRec.ReadString(sSecao,'idUnidCarga','');
        qtdRat      := StringToFloatDef(AINIRec.ReadString(sSecao,'qtdRat',''),0);

        K := 1;
        while true do
        begin
          sSecao := 'lacUnidCarga' + Nivel + IntToStrZero(J,3) + IntToStrZero(K,3);
          sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');
          if FimLoop(sFim) then
            break;

          lacUnidCarga.New.nLacre := AINIRec.ReadString(sSecao,'nLacre','');
          inc(K);
        end;
      end;
      inc(J);
    end;
  end;
end;

procedure TCTeIniReader.Ler_InfCTeNormalInfOutros(AINIRec: TMemIniFile;
  infOutros: TInfOutrosCollection);
var
  I, J, K, L: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  I := 1;
  while true do
  begin
    sSecao := 'infOutros'+IntToStrZero(I,3);
    sFim   := AINIRec.ReadString(sSecao,'tpDoc','FIM');
    if FimLoop(sFim) then
      break;

    with infOutros.New do
    begin
      tpDoc      := StrToTpDocumento(OK,AINIRec.ReadString(sSecao,'tpDoc','01'));
      descOutros := AINIRec.ReadString(sSecao,'descOutros','');
      nDoc       := AINIRec.ReadString(sSecao,'nDoc','');
      dEmi       := StringToDateTime(AINIRec.ReadString(sSecao,'dEmi','0'));
      vDocFisc   := StringToFloatDef(AINIRec.ReadString(sSecao,'vDocFisc','') ,0);
      dPrev      := StringToDateTime(AINIRec.ReadString(sSecao,'dPrev','0'));

      J := 1;
      while true do
      begin
        sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3);
        sFim   := AINIRec.ReadString(sSecao,'idUnidTransp','FIM');
        if FimLoop(sFim) then
          break;
        with infUnidTransp.New do
        begin
          tpUnidTransp := StrToUnidTransp(OK,AINIRec.ReadString(sSecao,'tpUnidTransp','1'));
          idUnidTransp := AINIRec.ReadString(sSecao,'idUnidTransp','');
          qtdRat       := StringToFloatDef(AINIRec.ReadString(sSecao,'qtdRat',''),0);

          K := 1;
          while true do
          begin
            sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
            sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');
            if FimLoop(sFim) then
              break;

            lacUnidTransp.New.nLacre := AINIRec.ReadString(sSecao,'nLacre','');
            inc(K);
          end;

          K := 1;
          while true do
          begin
            sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
            sFim   := AINIRec.ReadString(sSecao,'idUnidCarga','FIM');
            if FimLoop(sFim) then
              break;
            with infUnidCarga.New do
            begin
              tpUnidCarga := StrToUnidCarga(OK,AINIRec.ReadString(sSecao,'tpUnidCarga','1'));
              idUnidCarga := AINIRec.ReadString(sSecao,'idUnidCarga','');
              qtdRat      := StringToFloatDef(AINIRec.ReadString(sSecao,'qtdRat',''),0);

              L := 1;
              while true do
              begin
                sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');
                if FimLoop(sFim) then
                  break;

                lacUnidCarga.New.nLacre := AINIRec.ReadString(sSecao,'nLacre','');
                inc(L);
              end;
            end;
            inc(K);
          end;
        end;
        inc(J);
      end;

      J := 1;
      while true do
      begin
        sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3);
        sFim   := AINIRec.ReadString(sSecao,'idUnidCarga','FIM');
        if FimLoop(sFim) then
          break;
        with infUnidCarga.New do
        begin
          tpUnidCarga := StrToUnidCarga(OK,AINIRec.ReadString(sSecao,'tpUnidCarga','1'));
          idUnidCarga := AINIRec.ReadString(sSecao,'idUnidCarga','');
          qtdRat      := StringToFloatDef(AINIRec.ReadString(sSecao,'qtdRat',''),0);

          K := 1;
          while true do
          begin
            sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
            sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');
            if FimLoop(sFim) then
              break;

            lacUnidCarga.New.nLacre := AINIRec.ReadString(sSecao,'nLacre','');
            inc(K);
          end;
        end;
        inc(J);
      end;
    end;
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_InfCTeNormalInfDCe(AINIRec: TMemIniFile;
  infDCe: TInfDCeCollection);
var
  sSecao, sFim: string;
  I: Integer;
begin
  I := 1;
  while true do
  begin
    sSecao := 'infDCe'+IntToStrZero(I,3);
    sFim   := AINIRec.ReadString(sSecao, 'chave', 'FIM');
    if FimLoop(sFim) then
      break;

    with infDCe.New do
    begin
      chave := sFim;
    end;

    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_InfCTeNormalDocAnteriores(AINIRec: TMemIniFile;
  docAnt: TDocAnt);
var
  I, J: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  I := 1;
  while true do
  begin
    sSecao := IfThen(AINIRec.SectionExists('emiDocAnt'+ IntToStrZero(I, 3))
                      , 'emiDocAnt', 'DocAnt') + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'xNome', 'FIM');
    if FimLoop(sFim) then
      break;

    with docAnt.emiDocAnt.New do
    begin
      CNPJCPF := AINIRec.ReadString(sSecao,'CNPJCPF','');
      IE      := AINIRec.ReadString(sSecao,'IE','');
      UF      := AINIRec.ReadString(sSecao,'UF','');
      xNome   := AINIRec.ReadString(sSecao,'xNome','');

      sSecao := IfThen(AINIRec.SectionExists('idDocAntPap'+ IntToStrZero(I, 3) + '001')
                        , 'idDocAntPap'
                        , 'idDocAnt') + IntToStrZero(I, 3) + '001';
      sFim   := AINIRec.ReadString(sSecao, 'nDoc', 'FIM');

      if sFim <> 'FIM' then
      begin
        with idDocAnt.New do
        begin
          J := 1;
          while true do
          begin
            sSecao := IfThen(AINIRec.SectionExists('idDocAntPap'+IntToStrZero(I, 3) + IntToStrZero(J, 3))
                              , 'idDocAntPap'
                              , 'idDocAnt') + IntToStrZero(I, 3) + IntToStrZero(J, 3);
            sFim   := AINIRec.ReadString(sSecao, 'nDoc', 'FIM');
            if FimLoop(sFim) then
              break;

            with idDocAntPap.New do
            begin
              tpDoc  := StrToTpDocumentoAnterior(OK, AINIRec.ReadString(sSecao,'tpDoc',''));
              serie  := AINIRec.ReadString(sSecao,'serie','');
              subser := AINIRec.ReadString(sSecao,'subser','');
              nDoc   := AINIRec.ReadString(sSecao,'nDoc','');
              dEmi   := StringToDateTime(AINIRec.ReadString(sSecao,'dEmi','0'));
            end;
            Inc(J);
          end;
        end;
      end;

      sSecao := 'idDocAntEle' + IntToStrZero(I, 3) + '001';
      sFim   := AINIRec.ReadString(sSecao, 'chCTe', 'FIM');

      if sFim <> 'FIM' then
      begin
        with idDocAnt.New do
        begin
          J := 1;
          while true do
          begin
            sSecao := 'idDocAntEle' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
            sFim   := AINIRec.ReadString(sSecao, 'chCTe', 'FIM');
            if FimLoop(sFim) then
              break;

            idDocAntEle.New.chCTe := sFim;

            Inc(J);
          end;
        end;
      end;
    end;

    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_InfCTeNormalInfModal(AINIRec: TMemIniFile;
  infCTeNorm: TInfCTeNorm);
begin
  if FCTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl] then
  begin
    Ler_InfModalRodoviario(AINIRec, FCTe.infmodal.rodo);
    Ler_InfModalAereo(AINIRec, FCTe.infmodal.aereo);
    Ler_InfModalAquav(AINIRec, FCTe.infmodal.aquav);
  end
  else
  begin
    Ler_InfModalRodoviario(AINIRec, infCTeNorm.rodo);
    Ler_InfModalAereo(AINIRec, infCTeNorm.aereo);
    Ler_InfModalAquav(AINIRec, infCTeNorm.aquav);
    Ler_InfModalFerrov(AINIRec, infCTeNorm.ferrov);
    Ler_InfModalDuto(AINIRec, infCTeNorm.duto);
  end;
end;

procedure TCTeIniReader.Ler_InfModalRodoviario(AINIRec: TMemIniFile;
  rodo: TRodo);
var
  I: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  if AINIRec.ReadString('Rodo','RNTRC','') <> '' then
  begin
    Rodo.RNTRC := AINIRec.ReadString('Rodo','RNTRC','');
    Rodo.dPrev := StringToDateTime(AINIRec.ReadString('Rodo','dPrev','0'));
    sFim := AINIRec.ReadString('Rodo','lota','0');
    if sFim <> '' then
      Rodo.Lota  := StrToTpLotacao(OK, sFim);
    Rodo.CIOT  := AINIRec.ReadString('Rodo','CIOT','');

    I := 1;
    while true do
    begin
      sSecao := 'Occ'+IntToStrZero(I,3);
      sFim   := AINIRec.ReadString(sSecao,'nOcc','FIM');
      if FimLoop(sFim) then
        break;

      with Rodo.Occ.New do
      begin
        serie := AINIRec.ReadString(sSecao,'serie','');
        nOcc  := AINIRec.ReadInteger(sSecao,'nOcc',0);
        dEmi  := StringToDateTime(AINIRec.ReadString(sSecao,'dEmi','0'));

        EmiOCC.CNPJ := AINIRec.ReadString(sSecao,'CNPJ','');
        EmiOCC.cInt := AINIRec.ReadString(sSecao,'cInt','');
        EmiOCC.IE   := AINIRec.ReadString(sSecao,'IE','');
        EmiOCC.UF   := AINIRec.ReadString(sSecao,'UF','');
        EmiOCC.fone := AINIRec.ReadString(sSecao,'fone','');
      end;
      Inc(I);
    end;

    I := 1;
    while true do
    begin
      sSecao := 'valePed'+IntToStrZero(I,3);
      sFim   := AINIRec.ReadString(sSecao,'CNPJForn','FIM');
      if FimLoop(sFim) then
        break;

      with Rodo.valePed.New do
      begin
        CNPJForn := AINIRec.ReadString(sSecao,'CNPJForn','');
        nCompra  := AINIRec.ReadString(sSecao,'nCompra','');
        CNPJPg   := AINIRec.ReadString(sSecao,'CNPJPg','');
      end;
      Inc(I);
    end;

    I := 1;
    while true do
    begin
      sSecao := 'veic'+IntToStrZero(I,3);
      sFim   := AINIRec.ReadString(sSecao,'RENAVAM','FIM');
      if FimLoop(sFim) then
        break;

      with Rodo.veic.New do
      begin
        cInt    := AINIRec.ReadString(sSecao,'cInt','');
        RENAVAM := AINIRec.ReadString(sSecao,'RENAVAM','');
        placa   := AINIRec.ReadString(sSecao,'placa','');
        tara    := AINIRec.ReadInteger(sSecao,'tara',0);
        capKG   := AINIRec.ReadInteger(sSecao,'capKG',0);
        capM3   := AINIRec.ReadInteger(sSecao,'capM3',0);
        sFim := AINIRec.ReadString(sSecao,'tpProp','');
        if sFim <> '' then
          tpProp  := StrToTpPropriedade(OK, sFim);
        sFim := AINIRec.ReadString(sSecao,'tpVeic','');
        if sFim <> '' then
          tpVeic  := StrToTpVeiculo(OK, sFim);
        sFim := AINIRec.ReadString(sSecao,'tpRod','');
        if sFim <> '' then
          tpRod   := StrToTpRodado(OK, sFim);
        sFim := AINIRec.ReadString(sSecao,'tpCar','');
        if sFim <> '' then
          tpCar   := StrToTpCarroceria(OK, sFim);
        UF      := AINIRec.ReadString(sSecao,'UF','');

        if AINIRec.SectionExists('prop' + IntToStrZero(I,3))then
          sSecao := 'prop' + IntToStrZero(I, 3);

        Prop.CNPJCPF := AINIRec.ReadString(sSecao,'CNPJ','');
        Prop.RNTRC   := AINIRec.ReadString(sSecao,'RNTRC','');
        Prop.xNome   := AINIRec.ReadString(sSecao,'xNome','');
        Prop.IE      := AINIRec.ReadString(sSecao,'IE','');
        Prop.UF      := AINIRec.ReadString(sSecao,'PropUF',UF);
        Prop.tpProp  := StrToTpProp(OK,AINIRec.ReadString(sSecao,'ProptpProp',AINIRec.ReadString(sSecao,'tpProp','')));
      end;
      Inc(I);
    end;

    I := 1;
    while true do
    begin
      sSecao := 'lacre'+IntToStrZero(I,3);
      sFim   := AINIRec.ReadString(sSecao,'nLacre','FIM');
      if FimLoop(sFim) then
        break;

      Rodo.lacRodo.New.nLacre := sFim;
      Inc(I);
    end;

    I := 1;
    while true do
    begin
      sSecao := 'moto'+IntToStrZero(I,3);
      sFim   := AINIRec.ReadString(sSecao,'xNome','FIM');
      if FimLoop(sFim) then
        break;

      with Rodo.moto.New do
      begin
        xNome := sFim;
        CPF   := AINIRec.ReadString(sSecao,'CPF','');
      end;
      Inc(I);
    end;
  end;
end;

procedure TCTeIniReader.Ler_InfModalAereo(AINIRec: TMemIniFile; aereo: TAereo);
var
  I: Integer;
  sSecao, sKey, sFim: string;
  Ok: Boolean;
begin
  if AINIRec.ReadString('aereo','CL','') <> '' then
  begin
    sSecao := 'aereo';
    Aereo.nMinu := AINIRec.ReadInteger(sSecao,'nMinu',0);
    Aereo.nOCA  := AINIRec.ReadString(sSecao,'nOCA','');
    Aereo.dPrevAereo := StringToDateTime(AINIRec.ReadString(sSecao,'dPrevAereo','0'));
    Aereo.xLAgEmi := AINIRec.ReadString(sSecao,'xLAgEmi','');
    Aereo.IdT     := AINIRec.ReadString(sSecao,'IdT','');

    Aereo.tarifa.CL   := AINIRec.ReadString(sSecao,'nMinu','');
    Aereo.tarifa.cTar := AINIRec.ReadString(sSecao,'cTar','');
    Aereo.tarifa.vTar := StringToFloatDef(AINIRec.ReadString(sSecao,'vTar','') ,0);

    Aereo.natCarga.xDime := AINIRec.ReadString(sSecao,'xDime','');
    Aereo.natCarga.cIMP  := AINIRec.ReadString(sSecao,'cIMP','');

    I := 1;
    while true do
    begin
      sKey := 'cInfManu'+IntToStrZero(I,3);
      sFim := AINIRec.ReadString(sSecao,sKey,'FIM');
      if FimLoop(sFim) then
        break;

      Aereo.natCarga.cinfManu.New.nInfManu := StrToTpInfManu(Ok, sFim);

      Inc(I);
    end;

    Ler_InfModalAereoPeri(AINIRec, Aereo.peri);
  end;
end;

procedure TCTeIniReader.Ler_InfModalAquav(AINIRec: TMemIniFile; aquav: TAquav);
var
  I: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  if AINIRec.ReadString('aquav','xNavio','') <> '' then
  begin
    sSecao := 'aquav';
    Aquav.vPrest   := StringToFloatDef(AINIRec.ReadString(sSecao,'vPrest','') ,0);
    Aquav.vAFRMM   := StringToFloatDef(AINIRec.ReadString(sSecao,'vAFRMM','') ,0);
    Aquav.nBooking := AINIRec.ReadString(sSecao,'nBooking','0');
    Aquav.nCtrl    := AINIRec.ReadString(sSecao,'nCtrl','');
    Aquav.xNavio   := AINIRec.ReadString(sSecao,'xNavio','');

    Aquav.nViag    := AINIRec.ReadString(sSecao,'nViag','');
    Aquav.direc    := StrToTpDirecao(OK,AINIRec.ReadString(sSecao,'direc',''));
    Aquav.prtEmb   := AINIRec.ReadString(sSecao,'prtEmb','');
    Aquav.prtTrans := AINIRec.ReadString(sSecao,'prtTrans','');
    Aquav.prtDest  := AINIRec.ReadString(sSecao,'prtDest','');
    Aquav.tpNav    := StrToTpNavegacao(OK,AINIRec.ReadString(sSecao,'tpNav',''));
    Aquav.irin     := AINIRec.ReadString(sSecao,'irin','');

    I := 1;
    while true do
    begin
      sSecao := 'balsa'+IntToStrZero(I,3);
      sFim   := AINIRec.ReadString(sSecao,'xBalsa','FIM');
      if FimLoop(sFim) then
        break;

      Aquav.balsa.New.xBalsa := sFim;
      Inc(I);
    end;
  end;
end;

procedure TCTeIniReader.Ler_InfModalFerrov(AINIRec: TMemIniFile;
  ferrov: TFerrov);
var
  I: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  if AINIRec.ReadString('ferrov','tpTraf','') <> '' then
  begin
    sSecao := 'ferrov';

    if VersaoDF >= ve300 then
    begin
      Ferrov.tpTraf := StrToTpTrafego(OK,AINIRec.ReadString(sSecao,'tpTraf',''));
      Ferrov.fluxo  := AINIRec.ReadString(sSecao,'fluxo','0');

      Ferrov.trafMut.respFat := StrToTrafegoMutuo(OK,AINIRec.ReadString(sSecao,'respFat',''));
      Ferrov.trafMut.ferrEmi := StrToTrafegoMutuo(OK,AINIRec.ReadString(sSecao,'ferrEmi',''));
      Ferrov.vFrete          := StringToFloatDef(AINIRec.ReadString(sSecao,'vFrete','') ,0);

      Ferrov.trafMut.chCTeFerroOrigem := AINIRec.ReadString(sSecao,'chCTeFerroOrigem', '');

      I := 1;
      while true do
      begin
        sSecao := 'ferroEnv'+IntToStrZero(I,3);
        sFim   := AINIRec.ReadString(sSecao,'CNPJ','FIM');
        if FimLoop(sFim) then
          break;

        with Ferrov.ferroEnv.New do
        begin
          CNPJ  := sFim;
          IE    := AINIRec.ReadString(sSecao,'IE','');
          xNome := AINIRec.ReadString(sSecao,'xNome','');

          EnderFerro.xLgr    := AINIRec.ReadString(sSecao,'xLgr','');
          EnderFerro.nro     := AINIRec.ReadString(sSecao,'nro','');
          EnderFerro.xCpl    := AINIRec.ReadString(sSecao, 'xCpl','');
          EnderFerro.xBairro := AINIRec.ReadString(sSecao,'xBairro','');
          EnderFerro.cMun    := AINIRec.ReadInteger(sSecao,'cMun',0);
          EnderFerro.xMun    := AINIRec.ReadString(sSecao,'xMun','');
          EnderFerro.CEP     := AINIRec.ReadInteger(sSecao,'CEP',0);
          EnderFerro.UF      := AINIRec.ReadString(sSecao,'UF','');
        end;
        Inc(I);
      end;
    end
    else
    begin
      Ferrov.tpTraf := StrToTpTrafego(OK,AINIRec.ReadString(sSecao,'tpTraf',''));
      Ferrov.fluxo  := AINIRec.ReadString(sSecao,'fluxo','0');
      Ferrov.idTrem := AINIRec.ReadString(sSecao,'idTrem','0');
      Ferrov.vFrete := StringToFloatDef(AINIRec.ReadString(sSecao,'vFrete','') ,0);

      Ferrov.trafMut.respFat := StrToTrafegoMutuo(OK,AINIRec.ReadString(sSecao,'respFat',''));
      Ferrov.trafMut.ferrEmi := StrToTrafegoMutuo(OK,AINIRec.ReadString(sSecao,'ferrEmi',''));

      I := 1;
      while true do
      begin
        sSecao := 'ferroEnv'+IntToStrZero(I,3);
        sFim   := AINIRec.ReadString(sSecao,'CNPJ','FIM');
        if FimLoop(sFim) then
          break;

        with Ferrov.ferroEnv.New do
        begin
          CNPJ  := sFim;
          IE    := AINIRec.ReadString(sSecao,'IE','');
          xNome := AINIRec.ReadString(sSecao,'xNome','');

          EnderFerro.xLgr    := AINIRec.ReadString(sSecao,'xLgr','');
          EnderFerro.nro     := AINIRec.ReadString(sSecao,'nro','');
          EnderFerro.xCpl    := AINIRec.ReadString(sSecao, 'xCpl','');
          EnderFerro.xBairro := AINIRec.ReadString(sSecao,'xBairro','');
          EnderFerro.cMun    := AINIRec.ReadInteger(sSecao,'cMun',0);
          EnderFerro.xMun    := AINIRec.ReadString(sSecao,'xMun','');
          EnderFerro.CEP     := AINIRec.ReadInteger(sSecao,'CEP',0);
          EnderFerro.UF      := AINIRec.ReadString(sSecao,'UF','');
        end;
        Inc(I);
      end;

      I := 1;
      while true do
      begin
        sSecao := 'detVag'+IntToStrZero(I,3);
        sFim   := AINIRec.ReadString(sSecao,'nVag','FIM');
        if FimLoop(sFim) then
          break;

        with Ferrov.detVag.New do
        begin
          nVag   := StrToInt(sFim);
          cap    := StringToFloatDef(AINIRec.ReadString(sSecao,'cap','') ,0);
          tpVag  := AINIRec.ReadString(sSecao,'tpVag','');
          pesoR  := StringToFloatDef(AINIRec.ReadString(sSecao,'pesoR','') ,0);
          pesoBC := StringToFloatDef(AINIRec.ReadString(sSecao,'pesoBC','') ,0);
         end;
         Inc(I);
      end;
    end;
  end;
end;

procedure TCTeIniReader.Ler_InfModalDuto(AINIRec: TMemIniFile; duto: TDuto);
var
  sSecao: string;
  Ok: Boolean;
begin
  if AINIRec.ReadString('duto','dIni','') <> '' then
  begin
    sSecao := 'duto';
    duto.vTar := StringToFloatDef(AINIRec.ReadString(sSecao,'vTar','') ,0);
    duto.dIni := StringToDateTime(AINIRec.ReadString(sSecao,'dIni','0'));
    duto.dFim := StringToDateTime(AINIRec.ReadString(sSecao,'dFim','0'));
    duto.classDuto := StrToclassDuto(Ok, AINIRec.ReadString(sSecao,'classDuto',''));
    duto.tpContratacao := StrTotpContratacao(Ok, AINIRec.ReadString(sSecao,'tpContratacao',''));
    duto.codPontoEntrada := AINIRec.ReadString(sSecao,'codPontoEntrada','');
    duto.codPontoSaida := AINIRec.ReadString(sSecao,'codPontoSaida','');
    duto.nContrato := AINIRec.ReadString(sSecao,'nContrato','');
  end;
end;

procedure TCTeIniReader.Ler_InfModalAereoPeri(AINIRec: TMemIniFile;
  peri: TPeriCollection);
var
  I: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'peri' + IntToStrZero(I,3);
    sFim   := AINIRec.ReadString(sSecao,'nONU','FIM');
    if FimLoop(sFim) then
      break;

    with peri.New do
    begin
      nONU        := sFim;
      xNomeAE     := AINIRec.ReadString(sSecao,'xNomeAE','');
      xClaRisco   := AINIRec.ReadString(sSecao,'xClaRisco','');
      grEmb       := AINIRec.ReadString(sSecao,'grEmb','');
      qTotProd    := AINIRec.ReadString(sSecao,'qTotProd','');
      qVolTipo    := AINIRec.ReadString(sSecao,'qVolTipo','');
      pontoFulgor := AINIRec.ReadString(sSecao,'pontoFulgor','');
    end;
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_InfCTeNormalVeiculosNovos(AINIRec: TMemIniFile;
  veicNovos: TVeicNovosCollection);
var
  I: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'veicNovos'+IntToStrZero(I,3);
    sFim   := AINIRec.ReadString(sSecao,'chassi','FIM');
    if FimLoop(sFim) then
      break;

    with veicNovos.New do
    begin
      chassi := sFim;
      cCor   := AINIRec.ReadString(sSecao,'cCor','');
      xCor   := AINIRec.ReadString(sSecao,'xCor','');
      cMod   := AINIRec.ReadString(sSecao,'cMod','');
      vUnit  := StringToFloatDef(AINIRec.ReadString(sSecao,'vUnit','') ,0);
      vFrete := StringToFloatDef(AINIRec.ReadString(sSecao,'vFrete','') ,0);
    end;
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_InfCTeNormalCobr(AINIRec: TMemIniFile; cobr: TCobr);
var
  I: Integer;
  sSecao, sFim: string;
begin
  cobr.Fat.nFat  := AINIRec.ReadString('cobr','nFat','');
  cobr.Fat.vOrig := StringToFloatDef(AINIRec.ReadString('cobr','vOrig','') ,0);
  cobr.Fat.vDesc := StringToFloatDef(AINIRec.ReadString('cobr','vDesc','') ,0);
  cobr.Fat.vLiq  := StringToFloatDef(AINIRec.ReadString('cobr','vLiq' ,'') ,0);

  I := 1;
  while true do
  begin
    sSecao := 'dup'+IntToStrZero(I,3);
    sFim   := AINIRec.ReadString(sSecao,'nDup','FIM');
    if FimLoop(sFim) then
      break;

    with Cobr.Dup.New do
    begin
      nDup  := sFim;
      dVenc := StringToDateTime(AINIRec.ReadString(sSecao,'dVenc','0'));
      vDup  := StringToFloatDef(AINIRec.ReadString(sSecao,'vDup','') ,0);
    end;
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_InfCTeNormalInfCTeSub(AINIRec: TMemIniFile;
  infCteSub: TInfCteSub);
var
  sSecao: string;
  Ok: Boolean;
begin
  if AINIRec.ReadString('infCteSub','chCte','') <> '' then
  begin
    sSecao := 'infCteSub';
    with infCteSub do
    begin
      chCte         := AINIRec.ReadString(sSecao,'chCte','');
      indAlteraToma := StrToTIndicador(Ok, AINIRec.ReadString(sSecao,'indAlteraToma','0'));

      if AINIRec.SectionExists('tomaICMS') then
      begin
        sSecao := 'tomaICMS';

        tomaICMS.refNFe := AINIRec.ReadString(sSecao,'refNFe','');

        tomaICMS.refNF.CNPJCPF := AINIRec.ReadString(sSecao,'CNPJ','');
        tomaICMS.refNF.modelo   := AINIRec.ReadString(sSecao,'mod','');
        tomaICMS.refNF.serie    := AINIRec.ReadInteger(sSecao,'serie',0);
        tomaICMS.refNF.subserie := AINIRec.ReadInteger(sSecao,'subserie',0);
        tomaICMS.refNF.nro      := AINIRec.ReadInteger(sSecao,'nro',0);
        tomaICMS.refNF.valor    :=  StringToFloatDef(AINIRec.ReadString(sSecao,'valor','') ,0);
        tomaICMS.refNF.dEmi     := StringToDateTime(AINIRec.ReadString(sSecao,'dEmi','0'));
        tomaICMS.refCte         := AINIRec.ReadString(sSecao,'refCte','');

        // Usado pela versão 2.00
        tomaNaoICMS.refCteAnu := AINIRec.ReadString(sSecao,'refCteAnu','');
        // Usado pela versão 3.00
        refCteAnu := tomaNaoICMS.refCteAnu;
      end;
    end;
  end;
end;

procedure TCTeIniReader.Ler_InfCTeNormalInfGlobalizado(AINIRec: TMemIniFile;
  infGlobalizado: TInfGlobalizado);
var
  sSecao: string;
begin
  sSecao := 'infGlobalizado';

  if AINIRec.SectionExists(sSecao) then
    infGlobalizado.xObs := AINIRec.ReadString(sSecao, 'xObs', '');
end;

procedure TCTeIniReader.Ler_InfCTeNormalInfCTeMultimodal(AINIRec: TMemIniFile;
  infServVinc: TInfServVinc);
var
  I: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'infCTeMultimodal'+ IntToStrZero(I, 3);
    sFIM := AINIRec.ReadString(sSecao, 'chCTeMultimodal', 'FIM');

    if FimLoop(sFim) then
      break;

    infServVinc.infCTeMultimodal.New.chCTeMultimodal := sFIM;

    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_InfCTeComplemento(AINIRec: TMemIniFile;
  infCTeComp: TInfCteComp);
begin
  if AINIRec.SectionExists('infCteComp01') then
    infCTeComp.chave := AINIRec.ReadString('infCteComp01', 'chCte', AINIRec.ReadString('infCteComp01', 'chave', ''));
end;

procedure TCTeIniReader.Ler_InfCTeComplemento10(AINIRec: TMemIniFile;
  infCteComp10: TInfCteCompCollection);
var
  I: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while True do
  begin
    sSecao := 'infCteComp' + IntToStrZero(I, 2);

    sFim := AINIRec.ReadString(sSecao, 'chCte', AINIRec.ReadString(sSecao, 'chave', 'FIM'));

    if FimLoop(sFim) then
      break;

    infCteComp10.New.chCTe := sFim;

    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_AutorizadosXml(AINIRec: TMemIniFile;
  autXML: TautXMLCollection);
var
  I: Integer;
  sSecao, sFim: String;
begin
  I := 1 ;
  while true do
  begin
    sSecao := 'autXML' + IntToStrZero(I,2) ;
    sFim   := OnlyNumber(AINIRec.ReadString(sSecao, 'CNPJCPF', 'FIM'));
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break ;

    autXML.New.CNPJCPF := sFim;

    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_InfRespTecnico(AINIRec: TMemIniFile;
  infRespTec: TinfRespTec);
var
  sSecao: string;
begin
  sSecao := 'infRespTec';
  if AINIRec.SectionExists(sSecao) then
  begin
    infRespTec.CNPJ     := AINIRec.ReadString(sSecao, 'CNPJ', '');
    infRespTec.xContato := AINIRec.ReadString(sSecao, 'xContato', '');
    infRespTec.email    := AINIRec.ReadString(sSecao, 'email', '');
    infRespTec.fone     := AINIRec.ReadString(sSecao, 'fone', '');
  end;
end;

procedure TCTeIniReader.Ler_InfCTeSupl(AINIRec: TMemIniFile;
  infCTeSupl: TinfCTeSupl);
var
  sSecao: string;
begin
  sSecao := 'infCTeSupl';
  if AINIRec.SectionExists(sSecao) then
  begin
    infCTeSupl.qrCodCTe := AINIRec.ReadString(sSecao, 'qrCodCTe', '');
  end;
end;

procedure TCTeIniReader.Ler_Tomador(AINIRec: TMemIniFile; toma: TToma);
var
  Ok: Boolean;
begin
  {
    Lê os campos da seção: toma (utilizado pelo CT-e OS e CT-e Simplificado
  }
  if AINIRec.ReadString('toma','CNPJCPF','') <> '' then
  begin
    // os 2 campos abaixo são usados pelo CT-e Simplificado
    toma.Toma      := StrToTpTomador(OK,AINIRec.ReadString('toma','toma','0'));
    toma.indIEToma := StrToindIEDest(OK, AINIRec.ReadString('toma','indIEToma','1'));

    toma.CNPJCPF := AINIRec.ReadString('toma','CNPJCPF','');
    toma.IE      := AINIRec.ReadString('toma','IE','');
    toma.xNome   := AINIRec.ReadString('toma','xNome','');
    toma.xFant   := AINIRec.ReadString('toma','xFant','');
    toma.ISUF    := AINIRec.ReadString('toma','ISUF','');
    toma.email   := AINIRec.ReadString('toma','email','');
    toma.fone    := AINIRec.ReadString('toma','fone','');

    toma.endertoma.xLgr    := AINIRec.ReadString('toma','xLgr','');
    toma.endertoma.nro     := AINIRec.ReadString('toma','nro','');
    toma.endertoma.xCpl    := AINIRec.ReadString('toma', 'xCpl','');
    toma.endertoma.xBairro := AINIRec.ReadString('toma','xBairro','');
    toma.endertoma.cMun    := AINIRec.ReadInteger('toma','cMun',0);
    toma.endertoma.xMun    := AINIRec.ReadString('toma','xMun','');
    toma.endertoma.CEP     := AINIRec.ReadInteger('toma','CEP',0);
    toma.endertoma.UF      := AINIRec.ReadString('toma','UF','');
    toma.endertoma.cPais   := AINIRec.ReadInteger('toma','cPais',1058);
    toma.endertoma.xPais   := AINIRec.ReadString('toma','xPais','');
  end;
end;

procedure TCTeIniReader.Ler_Detalhamento(AINIRec: TMemIniFile;
  det: TdetCollection);
var
  I, J, K: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  if FCTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl] then
  begin
    I := 1;
    while true do
    begin
      sSecao := 'det' + IntToStrZero(I, 3);
      sFim   := AINIRec.ReadString(sSecao, 'cMunIni', 'FIM');
      if FimLoop(sFim) then
        break;

      with det.New do
      begin
        cMunIni := AINIRec.ReadInteger(sSecao, 'cMunIni', 0);
        xMunIni := AINIRec.ReadString(sSecao, 'xMunIni', '');
        cMunFim := AINIRec.ReadInteger(sSecao, 'cMunFim', 0);
        xMunFim := AINIRec.ReadString(sSecao, 'xMunFim', '');
        vPrest  := StringToFloatDef(AINIRec.ReadString(sSecao, 'vPrest', ''), 0);
        vRec    := StringToFloatDef(AINIRec.ReadString(sSecao, 'vRec', ''), 0);

        j := 1;
        while true do
        begin
          sSecao := 'Comp' + IntToStrZero(I, 3) + IntToStrZero(j, 3);
          sFim   := AINIRec.ReadString(sSecao, 'xNome', 'FIM');
          if FimLoop(sFim) then
            break;

          with comp.New do
          begin
            xNome := sFim;
            vComp := StringToFloatDef(AINIRec.ReadString(sSecao, 'vComp', ''), 0);
          end;
          Inc(j);
        end;

        j := 1;
        while true do
        begin
          sSecao := 'infNFe' + IntToStrZero(I, 3) + IntToStrZero(j, 3);
          sFim   := AINIRec.ReadString(sSecao,'chave','FIM');

          if sFim = 'FIM' then
            sFim := AINIRec.ReadString(sSecao,'chNFe','FIM');

          if FimLoop(sFim) then
            break;
          Ler_Secao_InfNFe(AINIRec, sSecao, I, j, infNFe);

          Inc(j);
        end;

        j := 1;
        while true do
        begin
          sSecao := 'infDocAnt' + IntToStrZero(I, 3) + IntToStrZero(j, 3);
          sFim   := AINIRec.ReadString(sSecao, 'chCTe', 'FIM');
          if FimLoop(sFim) then
            break;

          with infDocAnt.New do
          begin
            chCTe := sFim;
            tpPrest := StrTotpPrest(OK, AINIRec.ReadString(sSecao, 'tpPrest', '1'));

            k := 1;
            while true do
            begin
              sSecao := 'infNFeTranspParcial' + IntToStrZero(I, 3) + IntToStrZero(j, 3) +
                                                IntToStrZero(k, 3);
              sFim   := AINIRec.ReadString(sSecao,'chNFe','FIM');
              if FimLoop(sFim) then
                break;

              infNFeTranspParcial.New.chNFe := sFim;
              Inc(k);
            end;
          end;
          Inc(j);
        end;
      end;
      Inc(I);
    end;
  end;
end;

procedure TCTeIniReader.Ler_Total(AINIRec: TMemIniFile; total: Ttotal);
var
  sSecao: string;
begin
  sSecao := 'total';
  total.vTPrest := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTPrest', ''), 0);
  total.vTRec   := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTRec', ''), 0);
end;

procedure TCTeIniReader.Ler_InfPercurso(AINIRec: TMemIniFile;
  infPercurso: TinfPercursoCollection);
var
  I: Integer;
  sSecao, sFim: string;
begin
  //CT-e OS
  I := 1;
  while true do
  begin
    sSecao := 'infPercurso'+IntToStrZero(I,3);
    sFim   := AINIRec.ReadString(sSecao,'UFPer','FIM');
    if FimLoop(sFim) then
      break;

    infPercurso.New.UFPer := sFim;
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_InfServico(AINIRec: TMemIniFile;
  infServico: TInfServico);
begin
  //CT-e OS
  infServico.xDescServ := AINIRec.ReadString('infServico','xDescServ','');
  infServico.qCarga    := StringToFloatDef(AINIRec.ReadString('infServico','qCarga',''), 0);
end;

procedure TCTeIniReader.Ler_InfDocReferencia(AINIRec: TMemIniFile;
  infDocRef: TinfDocRefCollection);
var
  I: Integer;
  sSecao, sFim: string;
begin
  //CT-e OS
  I := 1;
  while true do
  begin
    sSecao := 'infDocRef'+IntToStrZero(I,3);
    sFim   := AINIRec.ReadString(sSecao,'nDoc', AINIRec.ReadString(sSecao,'chBPe','FIM'));
    if FimLoop(sFim) then
      break;
    with infDocRef.New do
    begin
      if AINIRec.ReadString(sSecao,'chBPe','') = '' then
      begin
        nDoc     := sFim;
        serie    := AINIRec.ReadString(sSecao,'serie','');
        subserie := AINIRec.ReadString(sSecao,'subserie','');
        dEmi     := StringToDateTime(AINIRec.ReadString(sSecao,'dEmi','0'));
        vDoc     := StringToFloatDef(AINIRec.ReadString(sSecao,'vDoc','') ,0);
      end
      else
        chBPe := sFim;
    end;
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_Seguro(AINIRec: TMemIniFile;
  seg: TSegCollection);
var
  I: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  //CT-e OS
  I := 1;
  while true do
  begin
    sSecao := 'seg'+IntToStrZero(I,3);
    sFim   := AINIRec.ReadString(sSecao,'respSeg','FIM');
    if FimLoop(sFim) then
      break;
    with seg.New do
    begin
      respSeg := StrToTpRspSeguro(OK, sFim);
      xSeg    := AINIRec.ReadString(sSecao,'xSeg','');
      nApol   := AINIRec.ReadString(sSecao,'nApol','');
    end;
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_InfModalRodoviarioOS(AINIRec: TMemIniFile;
  rodoOS: TRodoOS);
var
  I: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  //Rodoviário CT-e OS
  sSecao := 'RodoOS';
  if (AINIRec.ReadString(sSecao,'TAF', '') <> '') or
     (AINIRec.ReadString(sSecao,'NroRegEstadual','') <> '')  then
  begin
    rodoOS.TAF            := AINIRec.ReadString(sSecao,'TAF','');
    rodoOS.NroRegEstadual := AINIRec.ReadString(sSecao,'NroRegEstadual','');

    I := 1;
    while true do
    begin
      sSecao := 'veic'+IntToStrZero(I,3);
      sFim   := AINIRec.ReadString(sSecao,'placa','FIM');
      if FimLoop(sFim) then
        break;

      with rodoOS.veic do
      begin
        placa   := sFim;
        RENAVAM := AINIRec.ReadString(sSecao,'RENAVAM','');
        UF      := AINIRec.ReadString(sSecao,'UF','');

        if AINIRec.SectionExists('prop' + IntToStrZero(I,3))then
          sSecao := 'prop' + IntToStrZero(I, 3);

        prop.CNPJCPF        := AINIRec.ReadString(sSecao,'CNPJCPF','');
        prop.TAF            := AINIRec.ReadString(sSecao,'TAF','');
        prop.NroRegEstadual := AINIRec.ReadString(sSecao,'NroRegEstadual','');
        prop.xNome          := AINIRec.ReadString(sSecao,'xNome','');
        prop.IE             := AINIRec.ReadString(sSecao,'IE','');
        prop.UF             := AINIRec.ReadString(sSecao,'propUF', AINIRec.ReadString(sSecao, 'UF', ''));
        prop.tpProp         := StrToTpProp(OK,AINIRec.ReadString(sSecao,'ProptpProp',AINIRec.ReadString(sSecao,'tpProp','')));
      end;
      Inc(I);
    end;

    sSecao := 'infFretamento';
    if AINIRec.SectionExists(sSecao) then
    begin
      with rodoOS.infFretamento do
      begin
        tpFretamento := StrToTpFretamento(OK, AINIRec.ReadString(sSecao, 'tpFretamento', '1'));
        dhViagem     := StringToDateTime(AINIRec.ReadString(sSecao, 'dhViagem','0'));
      end;
    end;
  end;
end;

procedure TCTeIniReader.Ler_InfCTeSub(AINIRec: TMemIniFile;
  infCteSub: TInfCteSub);
var
  sSecao: string;
begin
  sSecao := 'infCteSub';
  infCteSub.chCte := AINIRec.ReadString(sSecao,'chCte','');
end;

procedure TCTeIniReader.Ler_InfGTVe(AINIRec: TMemIniFile;
  infGTVe: TinfGTVeCollection);
var
  I, J: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  //CT-e OS
  I := 1;
  while true do
  begin
    sSecao := 'infGTVe' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'chCTe', 'FIM');
    if FimLoop(sFim) then
      break;
    with infGTVe.New do
    begin
      chCTe := sFim;

      J := 1;
      while true do
      begin
        sSecao := 'infGTVeComp' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
        sFim   := AINIRec.ReadString(sSecao, 'vComp', 'FIM');
        if FimLoop(sFim) then
          break;
        with infGTVe[I - 1].Comp.New do
        begin
          tpComp := StrTotpComp(Ok, AINIRec.ReadString(sSecao, 'tpComp', '0'));
          vComp := StringToFloatDef(sFim , 0);
          xComp := AINIRec.ReadString(sSecao, 'xComp', '');
        end;
        Inc(J);
      end;
    end;
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_Origem(AINIRec: TMemIniFile; origem: TEnderEmit);
begin
  // GTV-e
  origem.xLgr    := AINIRec.ReadString('origem','xLgr','');
  origem.nro     := AINIRec.ReadString('origem','nro','');
  origem.xCpl    := AINIRec.ReadString('origem','xCpl','');
  origem.xBairro := AINIRec.ReadString('origem','xBairro','');
  origem.cMun    := AINIRec.ReadInteger('origem','cMun',0);
  origem.xMun    := AINIRec.ReadString('origem','xMun','');
  origem.CEP     := AINIRec.ReadInteger('origem','CEP',0);
  origem.UF      := AINIRec.ReadString('origem','UF','');
  origem.fone    := AINIRec.ReadString('origem','fone','');
end;

procedure TCTeIniReader.Ler_Destino(AINIRec: TMemIniFile; destino: TEnderEmit);
begin
  // GTV-e
  destino.xLgr    := AINIRec.ReadString('destino','xLgr','');
  destino.nro     := AINIRec.ReadString('destino','nro','');
  destino.xCpl    := AINIRec.ReadString('destino','xCpl','');
  destino.xBairro := AINIRec.ReadString('destino','xBairro','');
  destino.cMun    := AINIRec.ReadInteger('destino','cMun',0);
  destino.xMun    := AINIRec.ReadString('destino','xMun','');
  destino.CEP     := AINIRec.ReadInteger('destino','CEP',0);
  destino.UF      := AINIRec.ReadString('destino','UF','');
  destino.fone    := AINIRec.ReadString('destino','fone','');
end;

procedure TCTeIniReader.Ler_DetalhamentoGTV(AINIRec: TMemIniFile;
  detGTV: TdetGTV);
var
  I: Integer;
  sSecao, sFim: string;
  Ok: Boolean;
begin
  // GTV-e
  detGTV.qCarga := StringToFloatDef(AINIRec.ReadString('detGTV','qCarga' ,'') ,0);

  I := 1;
  while true do
  begin
    sSecao := 'infEspecie' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'vEspecie', 'FIM');
    if FimLoop(sFim) then
      break;
    with detGTV.infEspecie.New do
    begin
      tpEspecie := StrToTEspecie(Ok, AINIRec.ReadString(sSecao, 'tpEspecie', '0'));
      vEspecie := StringToFloatDef(sFim , 0);
      tpNumerario := StrTotpNumerario(Ok, AINIRec.ReadString(sSecao, 'tpNumerario', '0'));
      xMoedaEstr := AINIRec.ReadString(sSecao, 'xMoedaEstr', '');
    end;
    Inc(I);
  end;

  I := 1;
  while true do
  begin
    sSecao := 'infVeiculo' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'placa', 'FIM');
    if FimLoop(sFim) then
      break;
    with detGTV.infVeiculo.New do
    begin
      placa := sFim;
      UF := AINIRec.ReadString(sSecao, 'UF', '');
      RNTRC := AINIRec.ReadString(sSecao, 'RNTRC', '');
    end;
    Inc(I);
  end;
end;

procedure TCTeIniReader.Ler_ProcessamentoCTe(AINIRec: TMemIniFile;
  procCTe: TProcCTe);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'procCTe';
  if AINIRec.SectionExists(sSecao) then
  begin
    with procCTe do
    begin
      tpAmb := StrToTpAmb(ok, AINIRec.ReadString(sSecao, 'tpAmb', ''));
      verAplic := AINIRec.ReadString(sSecao, 'verAplic', '');
      chCTe := AINIRec.ReadString(sSecao, 'chCTe', '');
      dhRecbto := AINIRec.ReadDateTime(sSecao, 'dhRecbto', 0);
      nProt := AINIRec.ReadString(sSecao, 'nProt', '');
      digVal := AINIRec.ReadString(sSecao, 'digVal', '');
      cStat := AINIRec.ReadInteger(sSecao, 'cStat', 0);
      xMotivo := AINIRec.ReadString(sSecao, 'xMotivo', '');
    end;
  end;
end;

// Reforma Tributária
procedure TCTeIniReader.Ler_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS);
var
  sSecao: string;
begin
  sSecao := 'IBSCBS';
  if AINIRec.SectionExists(sSecao) then
  begin
    IBSCBS.CST := StrToCSTIBSCBS(AINIRec.ReadString(sSecao, 'CST', '000'));
    IBSCBS.cClassTrib := StrTocClassTrib(AINIRec.ReadString(sSecao, 'cClassTrib', '000001'));

    Ler_IBSCBS_gIBSCBS(AINIRec, IBSCBS.gIBSCBS);
  end;
end;

procedure TCTeIniReader.Ler_IBSCBS_gIBSCBS(AINIRec: TMemIniFile; gIBSCBS: TgIBSCBS);
var
  sSecao: string;
begin
  sSecao := 'gIBSCBS';
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSCBS.vBC := StringToFloatDef(AINIRec.ReadString(sSecao,'vBC','') ,0);
    gIBSCBS.vIBS := StringToFloatDef(AINIRec.ReadString(sSecao,'vIBS','') ,0);

    Ler_gIBSUF(AINIRec, gIBSCBS.gIBSUF);
    Ler_gIBSMun(AINIRec, gIBSCBS.gIBSMun);
    Ler_gCBS(AINIRec, gIBSCBS.gCBS);
    Ler_gTribReg(AINIRec, gIBSCBS.gTribRegular);
    Ler_gIBSCredPres(AINIRec, gIBSCBS.gIBSCredPres);
    Ler_gCBSCredPres(AINIRec, gIBSCBS.gCBSCredPres);
    Ler_gTribCompraGov(AINIRec, gIBSCBS.gTribCompraGov);
  end;
end;

procedure TCTeIniReader.Ler_gIBSUF(AINIRec: TMemIniFile; gIBSUF: TgIBSUFValores);
var
  sSecao: string;
begin
  sSecao := 'gIBSUF';
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSUF.pIBS := StringToFloatDef(AINIRec.ReadString(sSecao,'pIBSUF','') ,0);
    gIBSUF.vIBS := StringToFloatDef(AINIRec.ReadString(sSecao,'vIBSUF','') ,0);

    gIBSUF.gDif.pDif := StringToFloatDef(AINIRec.ReadString(sSecao,'pDif','') ,0);
    gIBSUF.gDif.vDif := StringToFloatDef(AINIRec.ReadString(sSecao,'vDif','') ,0);

    gIBSUF.gDevTrib.vDevTrib := StringToFloatDef(AINIRec.ReadString(sSecao,'vDevTrib','') ,0);

    gIBSUF.gRed.pRedAliq := StringToFloatDef(AINIRec.ReadString(sSecao,'pRedAliq','') ,0);
    gIBSUF.gRed.pAliqEfet := StringToFloatDef(AINIRec.ReadString(sSecao,'pAliqEfet','') ,0);
  end;
end;

procedure TCTeIniReader.Ler_gIBSMun(AINIRec: TMemIniFile; gIBSMun: TgIBSMunValores);
var
  sSecao: string;
begin
  sSecao := 'gIBSMun';
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSMun.pIBS := StringToFloatDef(AINIRec.ReadString(sSecao,'pIBSMun','') ,0);
    gIBSMun.vIBS := StringToFloatDef(AINIRec.ReadString(sSecao,'vIBSMun','') ,0);

    gIBSMun.gDif.pDif := StringToFloatDef(AINIRec.ReadString(sSecao,'pDif','') ,0);
    gIBSMun.gDif.vDif := StringToFloatDef(AINIRec.ReadString(sSecao,'vDif','') ,0);

    gIBSMun.gDevTrib.vDevTrib := StringToFloatDef(AINIRec.ReadString(sSecao,'vDevTrib','') ,0);

    gIBSMun.gRed.pRedAliq := StringToFloatDef(AINIRec.ReadString(sSecao,'pRedAliq','') ,0);
    gIBSMun.gRed.pAliqEfet := StringToFloatDef(AINIRec.ReadString(sSecao,'pAliqEfet','') ,0);
  end;
end;

procedure TCTeIniReader.Ler_gCBS(AINIRec: TMemIniFile; gCBS: TgCBSValores);
var
  sSecao: string;
begin
  sSecao := 'gCBS';
  if AINIRec.SectionExists(sSecao) then
  begin
    gCBS.pCBS := StringToFloatDef(AINIRec.ReadString(sSecao,'pCBS','') ,0);
    gCBS.vCBS := StringToFloatDef(AINIRec.ReadString(sSecao,'vCBS','') ,0);

    gCBS.gDif.pDif := StringToFloatDef(AINIRec.ReadString(sSecao,'pDif','') ,0);
    gCBS.gDif.vDif := StringToFloatDef(AINIRec.ReadString(sSecao,'vDif','') ,0);

    gCBS.gDevTrib.vDevTrib := StringToFloatDef(AINIRec.ReadString(sSecao,'vDevTrib','') ,0);

    gCBS.gRed.pRedAliq := StringToFloatDef(AINIRec.ReadString(sSecao,'pRedAliq','') ,0);
    gCBS.gRed.pAliqEfet := StringToFloatDef(AINIRec.ReadString(sSecao,'pAliqEfet','') ,0);
  end;
end;

procedure TCTeIniReader.Ler_gTribReg(AINIRec: TMemIniFile;
  gTribRegular: TgTribRegular);
var
  sSecao: string;
begin
  sSecao := 'gTribRegular';

  gTribRegular.CSTReg := StrToCSTIBSCBS(AINIRec.ReadString(sSecao, 'CSTReg', '000'));
  gTribRegular.cClassTribReg := StrTocClassTrib(AINIRec.ReadString(sSecao, 'cClassTribReg', '000001'));
  gTribRegular.pAliqEfetRegIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao,'pAliqEfetRegIBSUF','') ,0);
  gTribRegular.vTribRegIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao,'vTribRegIBSUF','') ,0);
  gTribRegular.pAliqEfetRegIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao,'pAliqEfetRegIBSMun','') ,0);
  gTribRegular.vTribRegIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao,'vTribRegIBSMun','') ,0);
  gTribRegular.pAliqEfetRegCBS := StringToFloatDef(AINIRec.ReadString(sSecao,'pAliqEfetRegCBS','') ,0);
  gTribRegular.vTribRegCBS := StringToFloatDef(AINIRec.ReadString(sSecao,'vTribRegCBS','') ,0);
end;

procedure TCTeIniReader.Ler_gIBSCredPres(AINIRec: TMemIniFile; gIBSCredPres: TgIBSCBSCredPres);
var
  sSecao: string;
begin
  sSecao := 'gIBSCredPres';
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSCredPres.cCredPres := StrTocCredPres(AINIRec.ReadString(sSecao, 'cCredPres', ''));
    gIBSCredPres.pCredPres := StringToFloatDef(AINIRec.ReadString(sSecao,'pCredPres','') ,0);
    gIBSCredPres.vCredPres := StringToFloatDef(AINIRec.ReadString(sSecao,'vCredPres','') ,0);
    gIBSCredPres.vCredPresCondSus := StringToFloatDef(AINIRec.ReadString(sSecao,'vCredPresCondSus','') ,0);
  end;
end;

procedure TCTeIniReader.Ler_gCBSCredPres(AINIRec: TMemIniFile; gCBSCredPres: TgIBSCBSCredPres);
var
  sSecao: string;
begin
  sSecao := 'gCBSCredPres';
  if AINIRec.SectionExists(sSecao) then
  begin
    gCBSCredPres.cCredPres := StrTocCredPres(AINIRec.ReadString(sSecao, 'cCredPres', ''));
    gCBSCredPres.pCredPres := StringToFloatDef(AINIRec.ReadString(sSecao,'pCredPres','') ,0);
    gCBSCredPres.vCredPres := StringToFloatDef(AINIRec.ReadString(sSecao,'vCredPres','') ,0);
    gCBSCredPres.vCredPresCondSus := StringToFloatDef(AINIRec.ReadString(sSecao,'vCredPresCondSus','') ,0);
  end;
end;

procedure TCTeIniReader.Ler_gTribCompraGov(AINIRec: TMemIniFile;
  gTribCompraGov: TgTribCompraGov);
var
  sSecao: string;
begin
  sSecao := 'gTribCompraGov';

  if AINIRec.SectionExists(sSecao) then
  begin
    gTribCompraGov.pAliqIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao,'pAliqIBSUF','') ,0);
    gTribCompraGov.vTribIBSUF := StringToFloatDef(AINIRec.ReadString(sSecao,'vTribIBSUF','') ,0);
    gTribCompraGov.pAliqIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao,'pAliqIBSMun','') ,0);
    gTribCompraGov.vTribIBSMun := StringToFloatDef(AINIRec.ReadString(sSecao,'vTribIBSMun','') ,0);
    gTribCompraGov.pAliqCBS := StringToFloatDef(AINIRec.ReadString(sSecao,'pAliqCBS','') ,0);
    gTribCompraGov.vTribCBS := StringToFloatDef(AINIRec.ReadString(sSecao,'vTribCBS','') ,0);
  end;
end;

end.
