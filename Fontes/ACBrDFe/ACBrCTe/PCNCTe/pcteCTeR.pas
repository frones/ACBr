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

unit pcteCTeR;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcteConversaoCTe, pcnLeitor, pcteCTe;

type

  TCTeR = class(TPersistent)
  private
    FLeitor: TLeitor;
    FCTe: TCTe;
    FVersaoDF: TVersaoCTe;

    function Ler_InfPercurso: Boolean;
    function Ler_Toma03: Boolean;
    function Ler_Toma4: Boolean;
    function Ler_Identificacao: Boolean;
    function Ler_Complemento: Boolean;
    function Ler_Emitente: Boolean;
    function Ler_Tomador: Boolean;
    function Ler_Remetente: Boolean;
    function Ler_Expedidor: Boolean;
    function Ler_Recebedor: Boolean;
    function Ler_Destinatario: Boolean;
    function Ler_Origem: Boolean;
    function Ler_Destino: Boolean;
    function Ler_DetGTV: Boolean;
    function Ler_Comp(comp: TCompCollection):Boolean;
    function Ler_vPrest: Boolean;
    function Ler_Imp: Boolean;

    function Ler_infNF(Nivel: Integer; infNF: TInfNFCollection):Boolean;
    function Ler_infNFe(Nivel: Integer; infNFe: TInfNFeCollection):Boolean;
    function Ler_infOutros(Nivel: Integer; infOutros: TInfOutrosCollection):Boolean;

    function Ler_Rodo(rodo: TRodo): Boolean;
    function Ler_RodoOS(rodoOS: TRodoOS): Boolean;
    function Ler_Aereo(aereo: TAereo): Boolean;
    function Ler_Aquav(aquav: TAquav): Boolean;
    function Ler_Ferrov(ferrov: TFerrov): Boolean;
    function Ler_Duto(duto: TDuto): Boolean;
    function Ler_MultiModal(multimodal: TMultimodal): Boolean;

    function Ler_InfCTeNorm: Boolean;
    function Ler_InfCTeComp: Boolean;
    function Ler_InfCTeAnu: Boolean;
    function Ler_AutXML: Boolean;
    function Ler_InfRespTec: Boolean;
    function Ler_InfCTeSupl: Boolean;
    function Ler_ProtCTe: Boolean;
    function Ler_Signature: Boolean;

    function Ler_InfCarga(Nivel: Integer; infCarga: TInfCarga): Boolean;
    function Ler_InfDocAnt(infDocAnt: TinfDocAntCollection): Boolean;
    function Ler_Det: Boolean;

    function Ler_InfModal: Boolean;
    function Ler_Cobr(Nivel: Integer; cobr: TCobr): Boolean;
    function Ler_InfCTeSub(Nivel: Integer; infCTeSub: TInfCteSub): Boolean;
    function Ler_Total: Boolean;

    function Ler_CTe: Boolean;
    function Ler_CTeOS: Boolean;
    function Ler_CTeSimplificado: Boolean;
    function Ler_GTVe: Boolean;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;

    function LerXml: Boolean;
  published
    property Leitor: TLeitor      read FLeitor   write FLeitor;
    property CTe: TCTe            read FCTe      write FCTe;
    property VersaoDF: TVersaoCTe read FVersaoDF write FVersaoDF;
  end;

implementation

uses
  ACBrConsts,
  ACBrUtil.Base, ACBrUtil.Strings;

{ TCTeR }

constructor TCTeR.Create(AOwner: TCTe);
begin
  inherited Create;

  FLeitor := TLeitor.Create;
  FCTe := AOwner;
end;

destructor TCTeR.Destroy;
begin
  FLeitor.Free;

  inherited Destroy;
end;

function TCTeR.LerXml: Boolean;
var
  ok: Boolean;
  Aspas: String;
begin
  Leitor.Grupo := Leitor.Arquivo;

  if Pos('versao="', Leitor.Arquivo) <> 0 then
    Aspas := '"'
   else
    Aspas := '''';

  CTe.infCTe.Id := Leitor.rAtributo('Id=', 'infCte');

  if OnlyNumber(CTe.infCTe.Id) = '' then
    raise Exception.Create('Não encontrei o atributo: Id');

  CTe.infCTe.versao := StringToFloatDef(Leitor.rAtributo('versao=', 'infCte'), -1);

  if CTe.infCTe.versao = -1 then
    raise Exception.Create('Não encontrei o atributo: versao');

  VersaoDF := DblToVersaoCTe(Ok, CTe.infCTe.versao);

  Ler_Identificacao;

  Result := True;
end;

function TCTeR.Ler_InfPercurso: Boolean;
var
  i: Integer;
begin
  i := 0;
  CTe.Ide.infPercurso.Clear;

  while Leitor.rExtrai(2, 'infPercurso', '', i + 1) <> '' do
  begin
    CTe.Ide.infPercurso.New;
    CTe.Ide.infPercurso[i].UFPer := Leitor.rCampo(tcStr, 'UFPer');

    inc(i);
  end;

  Result := True;
end;

function TCTeR.Ler_Toma03: Boolean;
var
  ok: Boolean;
begin
  if (Leitor.rExtrai(2, 'toma03') <> '') or (Leitor.rExtrai(2, 'toma3') <> '') or
     (Leitor.rExtrai(2, 'toma') <> '') then
  begin
    CTe.Ide.Toma03.Toma := StrToTpTomador(ok, Leitor.rCampo(tcStr, 'toma'));
  end;

  Result := True;
end;

function TCTeR.Ler_Toma4: Boolean;
var
  ok: Boolean;
begin
  if (Leitor.rExtrai(2, 'toma4') <> '') or (Leitor.rExtrai(2, 'tomaTerceiro') <> '') then
  begin
    CTe.Ide.Toma4.toma    := StrToTpTomador(ok, Leitor.rCampo(tcStr, 'toma'));
    CTe.Ide.Toma03.Toma   := CTe.Ide.Toma4.toma;
    CTe.Ide.Toma4.CNPJCPF := Leitor.rCampoCNPJCPF;
    CTe.Ide.Toma4.IE      := Leitor.rCampo(tcStr, 'IE');
    CTe.Ide.Toma4.xNome   := Leitor.rCampo(tcStr, 'xNome');
    CTe.Ide.Toma4.xFant   := Leitor.rCampo(tcStr, 'xFant');
    CTe.Ide.Toma4.fone    := Leitor.rCampo(tcStr, 'fone');
    CTe.Ide.Toma4.email   := Leitor.rCampo(tcStr, 'email');

     if Leitor.rExtrai(3, 'enderToma') <> '' then
     begin
       CTe.Ide.Toma4.EnderToma.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
       CTe.Ide.Toma4.EnderToma.nro     := Leitor.rCampo(tcStr, 'nro');
       CTe.Ide.Toma4.EnderToma.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
       CTe.Ide.Toma4.EnderToma.xBairro := Leitor.rCampo(tcStr, 'xBairro');
       CTe.Ide.Toma4.EnderToma.cMun    := Leitor.rCampo(tcInt, 'cMun');
       CTe.Ide.Toma4.EnderToma.xMun    := Leitor.rCampo(tcStr, 'xMun');
       CTe.Ide.Toma4.EnderToma.CEP     := Leitor.rCampo(tcInt, 'CEP');
       CTe.Ide.Toma4.EnderToma.UF      := Leitor.rCampo(tcStr, 'UF');
       CTe.Ide.Toma4.EnderToma.cPais   := Leitor.rCampo(tcInt, 'cPais');
       CTe.Ide.Toma4.EnderToma.xPais   := Leitor.rCampo(tcStr, 'xPais');
     end;
  end;

  Result := True;
end;

function TCTeR.Ler_Identificacao: Boolean;
var
  sAux: string;
  ok: Boolean;
begin
  if Leitor.rExtrai(1, 'ide') <> '' then
  begin
    CTe.Ide.cUF   := Leitor.rCampo(tcInt, 'cUF');
    CTe.Ide.cCT   := Leitor.rCampo(tcStr, 'cCT');
    CTe.Ide.CFOP  := Leitor.rCampo(tcStr, 'CFOP');
    CTe.Ide.natOp := Leitor.rCampo(tcStr, 'natOp');

    if VersaoDF < ve300 then
    begin
      sAux := Leitor.rCampo(tcStr, 'forPag');

      if sAux <> '' then
        CTe.Ide.forPag := StrTotpforPag(ok, sAux);
    end;

    CTe.Ide.modelo  := Leitor.rCampo(tcInt, 'mod');
    CTe.Ide.serie   := Leitor.rCampo(tcInt, 'serie');
    CTe.Ide.nCT     := Leitor.rCampo(tcInt, 'nCT');
    CTe.Ide.dhEmi   := Leitor.rCampo(tcDatHor, 'dhEmi');
    CTe.Ide.tpImp   := StrToTpImp(ok, Leitor.rCampo(tcStr, 'tpImp'));
    CTe.Ide.tpEmis  := StrToTpEmis(ok, Leitor.rCampo(tcStr, 'tpEmis'));
    CTe.Ide.cDV     := Leitor.rCampo(tcInt, 'cDV');
    CTe.Ide.tpAmb   := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
    CTe.Ide.tpCTe   := StrTotpCTe(ok, Leitor.rCampo(tcStr, 'tpCTe'));
    CTe.Ide.procEmi := StrToprocEmi(ok, Leitor.rCampo(tcStr, 'procEmi'));
    CTe.Ide.verProc := Leitor.rCampo(tcStr, 'verProc');

    if VersaoDF >= ve300 then
    begin
      if Leitor.rCampo(tcStr, 'indGlobalizado') = '1' then
        CTe.ide.indGlobalizado := tiSim
      else
        CTe.ide.indGlobalizado := tiNao;
    end;

    CTe.Ide.refCTE  := Leitor.rCampo(tcStr, 'refCTE');
    CTe.Ide.cMunEnv := Leitor.rCampo(tcInt, 'cMunEnv');
    CTe.Ide.xMunEnv := Leitor.rCampo(tcStr, 'xMunEnv');
    CTe.Ide.UFEnv   := Leitor.rCampo(tcStr, 'UFEnv');
    CTe.Ide.modal   := StrToTpModal(ok, Leitor.rCampo(tcStr, 'modal'));
    CTe.Ide.tpServ  := StrToTpServ(ok, Leitor.rCampo(tcStr, 'tpServ'));
    CTe.Ide.cMunIni := Leitor.rCampo(tcInt, 'cMunIni');
    CTe.Ide.xMunIni := Leitor.rCampo(tcStr, 'xMunIni');
    CTe.Ide.UFIni   := Leitor.rCampo(tcStr, 'UFIni');
    CTe.Ide.cMunFim := Leitor.rCampo(tcInt, 'cMunFim');
    CTe.Ide.xMunFim := Leitor.rCampo(tcStr, 'xMunFim');
    CTe.Ide.UFFim   := Leitor.rCampo(tcStr, 'UFFim');

    sAux := Leitor.rCampo(tcStr, 'retira');

    if sAux <> '' then
      CTe.Ide.retira := StrToTpRetira(ok, sAux);

    CTe.Ide.xdetretira := Leitor.rCampo(tcStr, 'xDetRetira');

    CTe.Ide.dhCont := Leitor.rCampo(tcDatHor, 'dhCont');
    CTe.Ide.xJust  := Leitor.rCampo(tcStr, 'xJust');

    if VersaoDF >= ve300 then
      CTe.ide.indIEToma := StrToindIEDest(Ok, Leitor.rCampo(tcStr, 'indIEToma'));

    CTe.Ide.dhSaidaOrig   := Leitor.rCampo(tcDatHor, 'dhSaidaOrig');
    CTe.Ide.dhChegadaDest := Leitor.rCampo(tcDatHor, 'dhChegadaDest');

    Ler_InfPercurso;
    Ler_Toma03;
    Ler_Toma4;
  end;

  case CTe.ide.tpCTe of
    tcGTVe: Ler_GTVe;

    tcCTeSimp,
    tcSubstCTeSimpl: Ler_CTeSimplificado;
  else
    if CTe.ide.modelo = 57 then
      Ler_CTe
    else
      Ler_CTeOS;
  end;

  Result := True;
end;

function TCTeR.Ler_Complemento: Boolean;
var
  i: Integer;
  ok: Boolean;
begin
  if Leitor.rExtrai(1, 'compl') <> '' then
  begin
    CTe.Compl.xCaracAd  := Leitor.rCampo(tcstr,'xCaracAd');
    CTe.Compl.xCaracSer := Leitor.rCampo(tcstr,'xCaracSer');
    CTe.Compl.xEmi      := Leitor.rCampo(tcstr,'xEmi');
    CTe.Compl.origCalc  := Leitor.rCampo(tcstr,'origCalc');
    CTe.Compl.destCalc  := Leitor.rCampo(tcstr,'destCalc');
    CTe.Compl.xObs      := Leitor.rCampo(tcstr,'xObs');

    if Leitor.rExtrai(2, 'fluxo') <> '' then
    begin
      CTe.Compl.fluxo.xOrig := Leitor.rCampo(tcstr,'xOrig');
      CTe.Compl.fluxo.xDest := Leitor.rCampo(tcstr,'xDest');
      CTe.Compl.fluxo.xRota := Leitor.rCampo(tcstr,'xRota');

      i := 0;
      CTe.Compl.fluxo.pass.Clear;

      while Leitor.rExtrai(3, 'pass', '', i + 1) <> '' do
      begin
        CTe.Compl.fluxo.pass.New;
        CTe.Compl.fluxo.pass[i].xPass := Leitor.rCampo(tcStr, 'xPass');

        inc(i);
      end;
    end;

    CTe.Compl.Entrega.TipoData := tdNaoInformado;
    CTe.Compl.Entrega.TipoHora := thNaoInformado;

    if Leitor.rExtrai(2, 'Entrega') <> '' then
    begin
      if Leitor.rExtrai(3, 'semData') <> '' then
      begin
        CTe.Compl.Entrega.TipoData      := tdSemData;
        CTe.Compl.Entrega.semData.tpPer := StrToTpDataPeriodo(ok, Leitor.rCampo(tcStr, 'tpPer'));
      end;

      if Leitor.rExtrai(3, 'comData') <> '' then
      begin
        CTe.Compl.Entrega.TipoData      := tdNaData;
        CTe.Compl.Entrega.comData.tpPer := StrToTpDataPeriodo(ok, Leitor.rCampo(tcStr, 'tpPer'));
        CTe.Compl.Entrega.comData.dProg := Leitor.rCampo(tcDat, 'dProg');
      end;

      if Leitor.rExtrai(3, 'noPeriodo') <> '' then
      begin
        CTe.Compl.Entrega.TipoData        := tdNoPeriodo;
        CTe.Compl.Entrega.noPeriodo.tpPer := StrToTpDataPeriodo(ok, Leitor.rCampo(tcStr, 'tpPer'));
        CTe.Compl.Entrega.noPeriodo.dIni  := Leitor.rCampo(tcDat, 'dIni');
        CTe.Compl.Entrega.noPeriodo.dFim  := Leitor.rCampo(tcDat, 'dFim');
      end;

      if Leitor.rExtrai(3, 'semHora') <> '' then
      begin
        CTe.Compl.Entrega.TipoHora      := thSemHorario;
        CTe.Compl.Entrega.semHora.tpHor := StrToTpHorarioIntervalo(ok, Leitor.rCampo(tcStr, 'tpHor'));
      end;

      if Leitor.rExtrai(3, 'comHora') <> '' then
      begin
        CTe.Compl.Entrega.TipoHora      := thNoHorario;
        CTe.Compl.Entrega.comHora.tpHor := StrToTpHorarioIntervalo(ok, Leitor.rCampo(tcStr, 'tpHor'));
        CTe.Compl.Entrega.comHora.hProg := StrToTime(Leitor.rCampo(tcStr, 'hProg'));
      end;

      if Leitor.rExtrai(3, 'noInter') <> '' then
      begin
        CTe.Compl.Entrega.TipoHora      := thNoIntervalo;
        CTe.Compl.Entrega.noInter.tpHor := StrToTpHorarioIntervalo(ok, Leitor.rCampo(tcStr, 'tpHor'));
        CTe.Compl.Entrega.noInter.hIni  := StrToTime(Leitor.rCampo(tcStr, 'hIni'));
        CTe.Compl.Entrega.noInter.hFim  := StrToTime(Leitor.rCampo(tcStr, 'hFim'));
      end;
    end;

    i := 0;
    CTe.Compl.ObsCont.Clear;

    while Leitor.rExtrai(2, 'ObsCont', '', i + 1) <> '' do
    begin
      CTe.Compl.obsCont.New;
      CTe.Compl.obsCont[i].xCampo := Leitor.rAtributo('xCampo');
      CTe.Compl.obsCont[i].xTexto := Leitor.rCampo(tcStr, 'xTexto');

      inc(i);
    end;

    i := 0;
    CTe.Compl.ObsFisco.Clear;

    while Leitor.rExtrai(2, 'ObsFisco', '', i + 1) <> '' do
    begin
      CTe.Compl.ObsFisco.New;
      CTe.Compl.ObsFisco[i].xCampo := Leitor.rAtributo('xCampo');
      CTe.Compl.ObsFisco[i].xTexto := Leitor.rCampo(tcStr, 'xTexto');

      inc(i);
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_Emitente: Boolean;
var
  sAux: string;
  ok: Boolean;
begin
  if Leitor.rExtrai(1, 'emit') <> '' then
  begin
    CTe.emit.CNPJ  := Leitor.rCampo(tcStr,'CNPJ');
    CTe.emit.IE    := Leitor.rCampo(tcStr, 'IE');
    CTe.emit.IEST  := Leitor.rCampo(tcStr, 'IEST');
    CTe.emit.xNome := Leitor.rCampo(tcStr, 'xNome');
    CTe.emit.xFant := Leitor.rCampo(tcStr, 'xFant');

    sAux := Leitor.rCampo(tcStr, 'CRT');

    if sAux <> '' then
      CTe.emit.CRT   := StrToCRTCTe(ok, sAux);

    if Leitor.rExtrai(2, 'enderEmit') <> '' then
    begin
      CTe.emit.enderemit.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      CTe.emit.enderemit.Nro     := Leitor.rCampo(tcStr, 'nro');
      CTe.emit.enderemit.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      CTe.emit.enderemit.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      CTe.emit.enderemit.cMun    := Leitor.rCampo(tcInt, 'cMun');
      CTe.emit.enderemit.xMun    := Leitor.rCampo(tcStr, 'xMun');
      CTe.emit.enderemit.CEP     := Leitor.rCampo(tcInt, 'CEP');
      CTe.emit.enderemit.UF      := Leitor.rCampo(tcStr, 'UF');
      CTe.emit.enderemit.fone    := Leitor.rCampo(tcStr, 'fone');
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_Tomador: Boolean;
begin
  if VersaoDF >= ve300 then
  begin
    if Leitor.rExtrai(1, 'toma') <> '' then
    begin
      CTe.toma.CNPJCPF := Leitor.rCampoCNPJCPF;
      CTe.toma.IE      := Leitor.rCampo(tcStr, 'IE');
      CTe.toma.xNome   := Leitor.rCampo(tcStr, 'xNome');
      CTe.toma.xFant   := Leitor.rCampo(tcStr, 'xFant');
      CTe.toma.fone    := Leitor.rCampo(tcStr, 'fone');
      CTe.toma.email   := Leitor.rCampo(tcStr, 'email');

      if Leitor.rExtrai(2, 'enderToma') <> '' then
      begin
        CTe.toma.EnderToma.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
        CTe.toma.EnderToma.nro     := Leitor.rCampo(tcStr, 'nro');
        CTe.toma.EnderToma.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
        CTe.toma.EnderToma.xBairro := Leitor.rCampo(tcStr, 'xBairro');
        CTe.toma.EnderToma.cMun    := Leitor.rCampo(tcInt, 'cMun');
        CTe.toma.EnderToma.xMun    := Leitor.rCampo(tcStr, 'xMun');
        CTe.toma.EnderToma.CEP     := Leitor.rCampo(tcInt, 'CEP');
        CTe.toma.EnderToma.UF      := Leitor.rCampo(tcStr, 'UF');
        CTe.toma.EnderToma.cPais   := Leitor.rCampo(tcInt, 'cPais');
        CTe.toma.EnderToma.xPais   := Leitor.rCampo(tcStr, 'xPais');
      end;
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_Remetente: Boolean;
var
  i: Integer;
  ok: Boolean;
begin
  if Leitor.rExtrai(1, 'rem') <> '' then
  begin
    CTe.Rem.CNPJCPF := Leitor.rCampoCNPJCPF('locColeta');
    CTe.Rem.IE      := Leitor.rCampo(tcStr, 'IE');
    CTe.Rem.xNome   := Leitor.rCampo(tcStr, 'xNome', 'locColeta');
    CTe.Rem.xFant   := Leitor.rCampo(tcStr, 'xFant');
    CTe.Rem.fone    := Leitor.rCampo(tcStr, 'fone');
    CTe.Rem.email   := Leitor.rCampo(tcStr, 'email');

    if Leitor.rExtrai(2, 'enderReme') <> '' then
    begin
      CTe.Rem.enderReme.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      CTe.Rem.enderReme.Nro     := Leitor.rCampo(tcStr, 'nro');
      CTe.Rem.enderReme.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      CTe.Rem.enderReme.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      CTe.Rem.enderReme.cMun    := Leitor.rCampo(tcInt, 'cMun');
      CTe.Rem.enderReme.xMun    := Leitor.rCampo(tcStr, 'xMun');
      CTe.Rem.enderReme.CEP     := Leitor.rCampo(tcInt, 'CEP');
      CTe.Rem.enderReme.UF      := Leitor.rCampo(tcStr, 'UF');
      CTe.Rem.enderReme.cPais   := Leitor.rCampo(tcInt, 'cPais');
      CTe.Rem.enderReme.xPais   := Leitor.rCampo(tcStr, 'xPais');
    end;

    if VersaoDF < ve200 then
    begin
      if Leitor.rExtrai(2, 'locColeta') <> '' then
      begin
        CTe.Rem.locColeta.CNPJCPF := Leitor.rCampoCNPJCPF;
        CTe.Rem.locColeta.xNome   := Leitor.rCampo(tcStr, 'xNome');
        CTe.Rem.locColeta.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
        CTe.Rem.locColeta.Nro     := Leitor.rCampo(tcStr, 'nro');
        CTe.Rem.locColeta.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
        CTe.Rem.locColeta.xBairro := Leitor.rCampo(tcStr, 'xBairro');
        CTe.Rem.locColeta.cMun    := Leitor.rCampo(tcInt, 'cMun');
        CTe.Rem.locColeta.xMun    := Leitor.rCampo(tcStr, 'xMun');
        CTe.Rem.locColeta.UF      := Leitor.rCampo(tcStr, 'UF');
      end;

      i := 0;
      CTe.infCTeNorm.infDoc.InfNFE.Clear;

      while Leitor.rExtrai(2, 'infNFe', '', i + 1) <> '' do
      begin
        CTe.infCTeNorm.infDoc.InfNFE.New;
        CTe.infCTeNorm.infDoc.InfNFE[i].chave := Leitor.rCampo(tcStr, 'chave');
        CTe.infCTeNorm.infDoc.InfNFE[i].PIN   := Leitor.rCampo(tcStr, 'PIN');

        inc(i);
      end;

      i := 0;
      CTe.infCTeNorm.infDoc.infNF.Clear;

      while Leitor.rExtrai(2, 'infNF', '', i + 1) <> '' do
      begin
        CTe.infCTeNorm.infDoc.infNF.New;
        CTe.infCTeNorm.infDoc.InfNF[i].nRoma  := Leitor.rCampo(tcStr, 'nRoma');
        CTe.infCTeNorm.infDoc.InfNF[i].nPed   := Leitor.rCampo(tcStr, 'nPed');
        CTe.infCTeNorm.infDoc.InfNF[i].Modelo := StrToModeloNF(Ok, Leitor.rCampo(tcStr, 'mod'));
        CTe.infCTeNorm.infDoc.InfNF[i].serie  := Leitor.rCampo(tcStr, 'serie');
        CTe.infCTeNorm.infDoc.InfNF[i].nDoc   := Leitor.rCampo(tcEsp, 'nDoc');
        CTe.infCTeNorm.infDoc.InfNF[i].dEmi   := Leitor.rCampo(tcDat, 'dEmi');
        CTe.infCTeNorm.infDoc.InfNF[i].vBC    := Leitor.rCampo(tcDe2, 'vBC');
        CTe.infCTeNorm.infDoc.InfNF[i].vICMS  := Leitor.rCampo(tcDe2, 'vICMS');
        CTe.infCTeNorm.infDoc.InfNF[i].vBCST  := Leitor.rCampo(tcDe2, 'vBCST');
        CTe.infCTeNorm.infDoc.InfNF[i].vST    := Leitor.rCampo(tcDe2, 'vST');
        CTe.infCTeNorm.infDoc.InfNF[i].vProd  := Leitor.rCampo(tcDe2, 'vProd');
        CTe.infCTeNorm.infDoc.InfNF[i].vNF    := Leitor.rCampo(tcDe2, 'vNF');
        CTe.infCTeNorm.infDoc.InfNF[i].nCFOP  := Leitor.rCampo(tcInt, 'nCFOP');
        CTe.infCTeNorm.infDoc.InfNF[i].nPeso  := Leitor.rCampo(tcDe3, 'nPeso');
        CTe.infCTeNorm.infDoc.InfNF[i].PIN    := Leitor.rCampo(tcStr, 'PIN');

        inc(i);
      end;

      i := 0;
      CTe.infCTeNorm.infDoc.InfOutros.Clear;

      while Leitor.rExtrai(2, 'infOutros', '', i + 1) <> '' do
      begin
        CTe.infCTeNorm.infDoc.InfOutros.New;
        CTe.infCTeNorm.infDoc.InfOutros[i].tpDoc      := StrToTpDocumento(ok, Leitor.rCampo(tcStr, 'tpDoc'));
        CTe.infCTeNorm.infDoc.InfOutros[i].descOutros := Leitor.rCampo(tcStr, 'descOutros');
        CTe.infCTeNorm.infDoc.InfOutros[i].nDoc       := Leitor.rCampo(tcStr, 'nDoc');
        CTe.infCTeNorm.infDoc.InfOutros[i].dEmi       := Leitor.rCampo(tcDat, 'dEmi');
        CTe.infCTeNorm.infDoc.InfOutros[i].vDocFisc   := Leitor.rCampo(tcDe2, 'vDocFisc');

        inc(i);
      end;
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_Expedidor: Boolean;
begin
  if Leitor.rExtrai(1, 'exped') <> '' then
  begin
    CTe.Exped.CNPJCPF := Leitor.rCampoCNPJCPF;
    CTe.Exped.IE      := Leitor.rCampo(tcStr, 'IE');
    CTe.Exped.xNome   := Leitor.rCampo(tcStr, 'xNome');
    CTe.Exped.fone    := Leitor.rCampo(tcStr, 'fone');
    CTe.Exped.email   := Leitor.rCampo(tcStr, 'email');

    if Leitor.rExtrai(2, 'enderExped') <> '' then
    begin
      CTe.Exped.EnderExped.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      CTe.Exped.EnderExped.Nro     := Leitor.rCampo(tcStr, 'nro');
      CTe.Exped.EnderExped.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      CTe.Exped.EnderExped.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      CTe.Exped.EnderExped.cMun    := Leitor.rCampo(tcInt, 'cMun');
      CTe.Exped.EnderExped.xMun    := Leitor.rCampo(tcStr, 'xMun');
      CTe.Exped.EnderExped.CEP     := Leitor.rCampo(tcInt, 'CEP');
      CTe.Exped.EnderExped.UF      := Leitor.rCampo(tcStr, 'UF');
      CTe.Exped.EnderExped.cPais   := Leitor.rCampo(tcInt, 'cPais');
      CTe.Exped.EnderExped.xPais   := Leitor.rCampo(tcStr, 'xPais');
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_Recebedor: Boolean;
begin
  if Leitor.rExtrai(1, 'receb') <> '' then
  begin
    CTe.receb.CNPJCPF := Leitor.rCampoCNPJCPF;
    CTe.receb.IE      := Leitor.rCampo(tcStr, 'IE');
    CTe.receb.xNome   := Leitor.rCampo(tcStr, 'xNome');
    CTe.receb.fone    := Leitor.rCampo(tcStr, 'fone');
    CTe.receb.email   := Leitor.rCampo(tcStr, 'email');

    if Leitor.rExtrai(2, 'enderReceb') <> '' then
    begin
      CTe.receb.Enderreceb.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      CTe.receb.Enderreceb.Nro     := Leitor.rCampo(tcStr, 'nro');
      CTe.receb.Enderreceb.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      CTe.receb.Enderreceb.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      CTe.receb.Enderreceb.cMun    := Leitor.rCampo(tcInt, 'cMun');
      CTe.receb.Enderreceb.xMun    := Leitor.rCampo(tcStr, 'xMun');
      CTe.receb.Enderreceb.CEP     := Leitor.rCampo(tcInt, 'CEP');
      CTe.receb.Enderreceb.UF      := Leitor.rCampo(tcStr, 'UF');
      CTe.receb.Enderreceb.cPais   := Leitor.rCampo(tcInt, 'cPais');
      CTe.receb.Enderreceb.xPais   := Leitor.rCampo(tcStr, 'xPais');
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_Destinatario: Boolean;
begin
  if Leitor.rExtrai(1, 'dest') <> '' then
  begin
    CTe.Dest.CNPJCPF := Leitor.rCampoCNPJCPF('locEnt');
    CTe.Dest.IE      := Leitor.rCampo(tcStr, 'IE');
    CTe.Dest.xNome   := Leitor.rCampo(tcStr, 'xNome', 'locEnt');
    CTe.Dest.fone    := Leitor.rCampo(tcStr, 'fone');
    CTe.Dest.ISUF    := Leitor.rCampo(tcStr, 'ISUF');
    CTe.Dest.email   := Leitor.rCampo(tcStr, 'email');

    if Leitor.rExtrai(2, 'enderDest') <> '' then
    begin
      CTe.Dest.EnderDest.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      CTe.Dest.EnderDest.Nro     := Leitor.rCampo(tcStr, 'nro');
      CTe.Dest.EnderDest.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      CTe.Dest.EnderDest.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      CTe.Dest.EnderDest.cMun    := Leitor.rCampo(tcInt, 'cMun');
      CTe.Dest.EnderDest.xMun    := Leitor.rCampo(tcStr, 'xMun');
      CTe.Dest.EnderDest.CEP     := Leitor.rCampo(tcInt, 'CEP');
      CTe.Dest.EnderDest.UF      := Leitor.rCampo(tcStr, 'UF');
      CTe.Dest.EnderDest.cPais   := Leitor.rCampo(tcInt, 'cPais');
      CTe.Dest.EnderDest.xPais   := Leitor.rCampo(tcStr, 'xPais');
    end;

    if Leitor.rExtrai(2, 'locEnt') <> '' then
    begin
      CTe.Dest.locEnt.CNPJCPF := Leitor.rCampoCNPJCPF;
      CTe.Dest.locEnt.xNome   := Leitor.rCampo(tcStr, 'xNome');
      CTe.Dest.locEnt.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      CTe.Dest.locEnt.Nro     := Leitor.rCampo(tcStr, 'nro');
      CTe.Dest.locEnt.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      CTe.Dest.locEnt.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      CTe.Dest.locEnt.cMun    := Leitor.rCampo(tcInt, 'cMun');
      CTe.Dest.locEnt.xMun    := Leitor.rCampo(tcStr, 'xMun');
      CTe.Dest.locEnt.UF      := Leitor.rCampo(tcStr, 'UF');
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_Origem: Boolean;
begin
  if Leitor.rExtrai(1, 'origem') <> '' then
  begin
    CTe.origem.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
    CTe.origem.Nro     := Leitor.rCampo(tcStr, 'nro');
    CTe.origem.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
    CTe.origem.xBairro := Leitor.rCampo(tcStr, 'xBairro');
    CTe.origem.cMun    := Leitor.rCampo(tcInt, 'cMun');
    CTe.origem.xMun    := Leitor.rCampo(tcStr, 'xMun');
    CTe.origem.CEP     := Leitor.rCampo(tcInt, 'CEP');
    CTe.origem.UF      := Leitor.rCampo(tcStr, 'UF');
  end;

  Result := True;
end;

function TCTeR.Ler_Destino: Boolean;
begin
  if Leitor.rExtrai(1, 'destino') <> '' then
  begin
    CTe.destino.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
    CTe.destino.Nro     := Leitor.rCampo(tcStr, 'nro');
    CTe.destino.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
    CTe.destino.xBairro := Leitor.rCampo(tcStr, 'xBairro');
    CTe.destino.cMun    := Leitor.rCampo(tcInt, 'cMun');
    CTe.destino.xMun    := Leitor.rCampo(tcStr, 'xMun');
    CTe.destino.CEP     := Leitor.rCampo(tcInt, 'CEP');
    CTe.destino.UF      := Leitor.rCampo(tcStr, 'UF');
  end;

  Result := True;
end;

function TCTeR.Ler_DetGTV: Boolean;
var
  i: Integer;
  ok: Boolean;
begin
  if Leitor.rExtrai(1, 'detGTV') <> '' then
  begin
    CTe.detGTV.qCarga := Leitor.rCampo(tcDe4, 'qCarga');

    i := 0;
    CTe.detGTV.infEspecie.Clear;
    while Leitor.rExtrai(2, 'infEspecie', '', i + 1) <> '' do
    begin
      CTe.detGTV.infEspecie.New;
      CTe.detGTV.infEspecie[i].tpEspecie   := StrToTEspecie(ok, Leitor.rCampo(tcStr, 'tpEspecie'));
      CTe.detGTV.infEspecie[i].vEspecie    := Leitor.rCampo(tcDe2, 'vEspecie');
      CTe.detGTV.infEspecie[i].tpNumerario := StrTotpNumerario(ok, Leitor.rCampo(tcStr, 'tpNumerario'));
      CTe.detGTV.infEspecie[i].xMoedaEstr  := Leitor.rCampo(tcStr, 'xMoedaEstr');

      inc(i);
    end;

    i := 0;
    CTe.detGTV.infVeiculo.Clear;
    while Leitor.rExtrai(2, 'infVeiculo', '', i + 1) <> '' do
    begin
      CTe.detGTV.infVeiculo.New;
      CTe.detGTV.infVeiculo[i].placa := Leitor.rCampo(tcStr, 'placa');
      CTe.detGTV.infVeiculo[i].UF    := Leitor.rCampo(tcStr, 'UF');
      CTe.detGTV.infVeiculo[i].RNTRC := Leitor.rCampo(tcStr, 'RNTRC');

      inc(i);
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_Comp(comp: TCompCollection): Boolean;
var
  i: Integer;
begin
  i := 0;
  Comp.Clear;

  while Leitor.rExtrai(2, 'Comp', '', i + 1) <> '' do
  begin
    Comp.New;
    Comp[i].xNome := Leitor.rCampo(tcStr, 'xNome');
    Comp[i].vComp := Leitor.rCampo(tcDe2, 'vComp');

    inc(i);
  end;

  Result := True;
end;

function TCTeR.Ler_vPrest: Boolean;
begin
  if Leitor.rExtrai(1, 'vPrest') <> '' then
  begin
    CTe.vPrest.vTPrest := Leitor.rCampo(tcDe2,'vTPrest');
    CTe.vPrest.vRec    := Leitor.rCampo(tcDe2,'vRec');

    Ler_Comp(CTe.vPrest.Comp);
  end;

  Result := True;
end;

function TCTeR.Ler_Imp: Boolean;
var
  ok: Boolean;
  sCST: string;
begin
  if Leitor.rExtrai(1, 'imp') <> '' then
  begin
    CTe.Imp.vTotTrib   := Leitor.rCampo(tcDe2,'vTotTrib');
    CTe.Imp.infAdFisco := Leitor.rCampo(tcStr,'infAdFisco');

    if Leitor.rExtrai(2, 'ICMS') <> '' then
    begin
      if Leitor.rExtrai(3, 'ICMS00') <> '' then
      begin
        if Leitor.rCampo(tcStr,'CST') = '00' then
        begin
          CTe.Imp.ICMS.SituTrib     := cst00;
          CTe.Imp.ICMS.ICMS00.CST   := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));
          CTe.Imp.ICMS.ICMS00.vBC   := Leitor.rCampo(tcDe2,'vBC');
          CTe.Imp.ICMS.ICMS00.pICMS := Leitor.rCampo(tcDe2,'pICMS');
          CTe.Imp.ICMS.ICMS00.vICMS := Leitor.rCampo(tcDe2,'vICMS');
        end;
      end;

      if Leitor.rExtrai(3, 'ICMS20') <> '' then
      begin
        if Leitor.rCampo(tcStr,'CST') = '20' then
        begin
          CTe.Imp.ICMS.SituTrib      := cst20;
          CTe.Imp.ICMS.ICMS20.CST    := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));
          CTe.Imp.ICMS.ICMS20.pRedBC := Leitor.rCampo(tcDe2,'pRedBC');
          CTe.Imp.ICMS.ICMS20.vBC    := Leitor.rCampo(tcDe2,'vBC');
          CTe.Imp.ICMS.ICMS20.pICMS  := Leitor.rCampo(tcDe2,'pICMS');
          CTe.Imp.ICMS.ICMS20.vICMS  := Leitor.rCampo(tcDe2,'vICMS');

          CTe.Imp.ICMS.ICMS20.vICMSDeson := Leitor.rCampo(tcDe2,'vICMSDeson');
          CTe.Imp.ICMS.ICMS20.cBenef     := Leitor.rCampo(tcStr,'cBenef');
        end;
      end;

      if Leitor.rExtrai(3, 'ICMS45') <> '' then
      begin
        sCST := Leitor.rCampo(tcStr,'CST');

        if (sCST = '40') or (sCST = '41') or (sCST = '51') then
        begin
          if sCST='40' then CTe.Imp.ICMS.SituTrib  := cst40;
          if sCST='41' then CTe.Imp.ICMS.SituTrib  := cst41;
          if sCST='51' then CTe.Imp.ICMS.SituTrib  := cst51;

          CTe.Imp.ICMS.ICMS45.CST := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));

          CTe.Imp.ICMS.ICMS45.vICMSDeson := Leitor.rCampo(tcDe2,'vICMSDeson');
          CTe.Imp.ICMS.ICMS45.cBenef     := Leitor.rCampo(tcStr,'cBenef');
        end;
      end;

      if Leitor.rExtrai(3, 'ICMS60') <> '' then
      begin
        if Leitor.rCampo(tcStr,'CST') = '60' then
        begin
          CTe.Imp.ICMS.SituTrib          := cst60;
          CTe.Imp.ICMS.ICMS60.CST        := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));
          CTe.Imp.ICMS.ICMS60.vBCSTRet   := Leitor.rCampo(tcDe2,'vBCSTRet');
          CTe.Imp.ICMS.ICMS60.vICMSSTRet := Leitor.rCampo(tcDe2,'vICMSSTRet');
          CTe.Imp.ICMS.ICMS60.pICMSSTRet := Leitor.rCampo(tcDe2,'pICMSSTRet');
          CTe.Imp.ICMS.ICMS60.vCred      := Leitor.rCampo(tcDe2,'vCred');

          CTe.Imp.ICMS.ICMS60.vICMSDeson := Leitor.rCampo(tcDe2,'vICMSDeson');
          CTe.Imp.ICMS.ICMS60.cBenef     := Leitor.rCampo(tcStr,'cBenef');
        end;
      end;

      if Leitor.rExtrai(3, 'ICMS90') <> '' then
      begin
        if Leitor.rCampo(tcStr,'CST') = '90' then
        begin
          CTe.Imp.ICMS.SituTrib      := cst90;
          CTe.Imp.ICMS.ICMS90.CST    := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));
          CTe.Imp.ICMS.ICMS90.pRedBC := Leitor.rCampo(tcDe2,'pRedBC');
          CTe.Imp.ICMS.ICMS90.vBC    := Leitor.rCampo(tcDe2,'vBC');
          CTe.Imp.ICMS.ICMS90.pICMS  := Leitor.rCampo(tcDe2,'pICMS');
          CTe.Imp.ICMS.ICMS90.vICMS  := Leitor.rCampo(tcDe2,'vICMS');
          CTe.Imp.ICMS.ICMS90.vCred  := Leitor.rCampo(tcDe2,'vCred');

          CTe.Imp.ICMS.ICMS90.vICMSDeson := Leitor.rCampo(tcDe2,'vICMSDeson');
          CTe.Imp.ICMS.ICMS90.cBenef     := Leitor.rCampo(tcStr,'cBenef');
        end;
      end;

      // ICMS devido à UF de origem da prestação, quando diferente da UF do emitente
      if Leitor.rExtrai(3, 'ICMSOutraUF') <> '' then
      begin
        if Leitor.rCampo(tcStr,'CST') = '90' then
        begin
          CTe.Imp.ICMS.SituTrib                  := cstICMSOutraUF;
          CTe.Imp.ICMS.ICMSOutraUF.CST           := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));
          CTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF := Leitor.rCampo(tcDe2,'pRedBCOutraUF');
          CTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF    := Leitor.rCampo(tcDe2,'vBCOutraUF');
          CTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF  := Leitor.rCampo(tcDe2,'pICMSOutraUF');
          CTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF  := Leitor.rCampo(tcDe2,'vICMSOutraUF');

          CTe.Imp.ICMS.ICMSOutraUF.vICMSDeson := Leitor.rCampo(tcDe2,'vICMSDeson');
          CTe.Imp.ICMS.ICMSOutraUF.cBenef     := Leitor.rCampo(tcStr,'cBenef');
        end;
      end;

      // ICMS Simples Nacional
      if Leitor.rExtrai(3, 'ICMSSN') <> '' then
      begin
        CTe.Imp.ICMS.SituTrib := cstICMSSN;

        if VersaoDF >= ve300 then
          CTe.Imp.ICMS.ICMSSN.CST := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));

        CTe.Imp.ICMS.ICMSSN.indSN := Leitor.rCampo(tcInt,'indSN');
      end;
    end;

    if Leitor.rExtrai(2, 'ICMSUFFim') <> '' then
    begin
      CTe.Imp.ICMSUFFim.vBCUFFim       := Leitor.rCampo(tcDe2,'vBCUFFim');
      CTe.Imp.ICMSUFFim.pFCPUFFim      := Leitor.rCampo(tcDe2,'pFCPUFFim');
      CTe.Imp.ICMSUFFim.pICMSUFFim     := Leitor.rCampo(tcDe2,'pICMSUFFim');
      CTe.Imp.ICMSUFFim.pICMSInter     := Leitor.rCampo(tcDe2,'pICMSInter');
      CTe.Imp.ICMSUFFim.pICMSInterPart := Leitor.rCampo(tcDe2,'pICMSInterPart');
      CTe.Imp.ICMSUFFim.vFCPUFFim      := Leitor.rCampo(tcDe2,'vFCPUFFim');
      CTe.Imp.ICMSUFFim.vICMSUFFim     := Leitor.rCampo(tcDe2,'vICMSUFFim');
      CTe.Imp.ICMSUFFim.vICMSUFIni     := Leitor.rCampo(tcDe2,'vICMSUFIni');
    end;

    if Leitor.rExtrai(2, 'infTribFed') <> '' then
    begin
      CTe.Imp.infTribFed.vPIS    := Leitor.rCampo(tcDe2,'vPIS');
      CTe.Imp.infTribFed.vCOFINS := Leitor.rCampo(tcDe2,'vCOFINS');
      CTe.Imp.infTribFed.vIR     := Leitor.rCampo(tcDe2,'vIR');
      CTe.Imp.infTribFed.vINSS   := Leitor.rCampo(tcDe2,'vINSS');
      CTe.Imp.infTribFed.vCSLL   := Leitor.rCampo(tcDe2,'vCSLL');
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_infNF(Nivel: Integer; infNF: TInfNFCollection): Boolean;
var
  i, j, k, l, pos1, pos2, pos3, len: Integer;
  ok: Boolean;
  sAux: string;
  qtdRat_UnidTransp: Double;
begin
  i := 0;
  infNF.Clear;

  while Leitor.rExtrai(Nivel, 'infNF', '', i + 1) <> '' do
  begin
    infNF.New;
    InfNF[i].nRoma := Leitor.rCampo(tcStr, 'nRoma');
    InfNF[i].nPed  := Leitor.rCampo(tcStr, 'nPed');
    InfNF[i].Modelo := StrToModeloNF(Ok, Leitor.rCampo(tcStr, 'mod'));
    InfNF[i].serie := Leitor.rCampo(tcStr, 'serie');
    InfNF[i].nDoc  := Leitor.rCampo(tcEsp, 'nDoc');
    InfNF[i].dEmi  := Leitor.rCampo(tcDat, 'dEmi');
    InfNF[i].vBC   := Leitor.rCampo(tcDe2, 'vBC');
    InfNF[i].vICMS := Leitor.rCampo(tcDe2, 'vICMS');
    InfNF[i].vBCST := Leitor.rCampo(tcDe2, 'vBCST');
    InfNF[i].vST   := Leitor.rCampo(tcDe2, 'vST');
    InfNF[i].vProd := Leitor.rCampo(tcDe2, 'vProd');
    InfNF[i].vNF   := Leitor.rCampo(tcDe2, 'vNF');
    InfNF[i].nCFOP := Leitor.rCampo(tcInt, 'nCFOP');
    InfNF[i].nPeso := Leitor.rCampo(tcDe3, 'nPeso');
    InfNF[i].PIN   := Leitor.rCampo(tcStr, 'PIN');
    InfNF[i].dPrev := Leitor.rCampo(tcDat, 'dPrev');

    j := 0;
    infNF[i].infUnidTransp.Clear;
    while Leitor.rExtrai(Nivel + 1, 'infUnidTransp', '', j + 1) <> '' do
    begin
      infNF[i].infUnidTransp.New;
      infNF[i].infUnidTransp[j].tpUnidTransp := StrToUnidTransp(ok, Leitor.rCampo(tcStr, 'tpUnidTransp'));
      infNF[i].infUnidTransp[j].idUnidTransp := Leitor.rCampo(tcStr, 'idUnidTransp');

      // Dentro do grupo <infUnidTransp> podemos ter até duas tags <qtdRat>
      // uma pertencente ao grupo <infUnidCarga> filha de <infUnidTransp> e
      // a outra pertencente ao grupo <infUnidTransp> e ambas são opcionais.
      // precisamos saber se existe uma ocorrência ou duas dessa tag para
      // efetuar a leitura correta das informações.

      sAux := Leitor.Grupo;
      pos1 := PosLast('</infUnidCarga>', sAux);
      pos2 := PosLast('<qtdRat>', sAux);
      pos3 := PosLast('</qtdRat>', sAux);
      len  := pos3 - pos2;

      if (pos1 < pos3) then
        qtdRat_UnidTransp := StringToFloatDef(Copy(sAux, pos2 + 8, len -8), 0)
      else
        qtdRat_UnidTransp := 0.0;

      infNF[i].infUnidTransp[j].qtdRat := qtdRat_UnidTransp;

      k := 0;
      infNF[i].infUnidTransp[j].lacUnidTransp.Clear;
      while Leitor.rExtrai(Nivel + 2, 'lacUnidTransp', '', k + 1) <> '' do
      begin
        infNF[i].infUnidTransp[j].lacUnidTransp.New;
        infNF[i].infUnidTransp[j].lacUnidTransp[k].nLacre := Leitor.rCampo(tcStr, 'nLacre');

        inc(k);
      end;

      k := 0;
      infNF[i].infUnidTransp[j].infUnidCarga.Clear;
      while Leitor.rExtrai(Nivel + 2, 'infUnidCarga', '', k + 1) <> '' do
      begin
        infNF[i].infUnidTransp[j].infUnidCarga.New;
        infNF[i].infUnidTransp[j].infUnidCarga[k].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
        infNF[i].infUnidTransp[j].infUnidCarga[k].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
        infNF[i].infUnidTransp[j].infUnidCarga[k].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

        l := 0;
        infNF[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga.Clear;
        while Leitor.rExtrai(Nivel + 3, 'lacUnidCarga', '', l + 1) <> '' do
        begin
          infNF[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga.New;
          infNF[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga[l].nLacre := Leitor.rCampo(tcStr, 'nLacre');

          inc(l);
        end;

        inc(k);
      end;

      inc(j);
    end;

    if j = 0 then
    begin
      infNF[i].infUnidCarga.Clear;

      while Leitor.rExtrai(Nivel + 1, 'infUnidCarga', '', j + 1) <> '' do
      begin
        infNF[i].infUnidCarga.New;
        infNF[i].infUnidCarga[j].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
        infNF[i].infUnidCarga[j].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
        infNF[i].infUnidCarga[j].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

        k := 0;
        infNF[i].infUnidCarga[j].lacUnidCarga.Clear;

        while Leitor.rExtrai(Nivel + 2, 'lacUnidCarga', '', k + 1) <> '' do
        begin
          infNF[i].infUnidCarga[j].lacUnidCarga.New;
          infNF[i].infUnidCarga[j].lacUnidCarga[k].nLacre := Leitor.rCampo(tcStr, 'nLacre');

          inc(k);
        end;

        inc(j);
      end;
    end;

    inc(i);
  end;

  Result := True;
end;

function TCTeR.Ler_infNFe(Nivel: Integer; infNFe: TInfNFeCollection): Boolean;
var
  i, j, k, l, pos1, pos2, pos3, len: Integer;
  ok: Boolean;
  sAux: string;
  qtdRat_UnidTransp: Double;
begin
  i := 0;
  InfNFE.Clear;
  while Leitor.rExtrai(Nivel, 'infNFe', '', i + 1) <> '' do
  begin
    InfNFE.New;

    InfNFE[i].chave := Leitor.rCampo(tcStr, 'chave');

    if infNFe[i].chave = '' then
      InfNFE[i].chave := Leitor.rCampo(tcStr, 'chNFe');

    InfNFE[i].PIN   := Leitor.rCampo(tcStr, 'PIN');
    InfNFE[i].dPrev := Leitor.rCampo(tcDat, 'dPrev');

    j := 0;
    InfNFE[i].infUnidTransp.Clear;

    while Leitor.rExtrai(Nivel + 1, 'infUnidTransp', '', j + 1) <> '' do
    begin
      InfNFE[i].infUnidTransp.New;
      InfNFE[i].infUnidTransp[j].tpUnidTransp := StrToUnidTransp(ok, Leitor.rCampo(tcStr, 'tpUnidTransp'));
      InfNFE[i].infUnidTransp[j].idUnidTransp := Leitor.rCampo(tcStr, 'idUnidTransp');

      // Dentro do grupo <infUnidTransp> podemos ter até duas tags <qtdRat>
      // uma pertencente ao grupo <infUnidCarga> filha de <infUnidTransp> e
      // a outra pertencente ao grupo <infUnidTransp> e ambas são opcionais.
      // precisamos saber se existe uma ocorrência ou duas dessa tag para
      // efetuar a leitura correta das informações.

      sAux := Leitor.Grupo;
      pos1 := PosLast('</infUnidCarga>', sAux);
      pos2 := PosLast('<qtdRat>', sAux);
      pos3 := PosLast('</qtdRat>', sAux);
      len := pos3 - pos2;

      if (pos1 < pos3) then
        qtdRat_UnidTransp := StringToFloatDef(Copy(sAux, pos2 + 8, len -8), 0)
      else
        qtdRat_UnidTransp := 0.0;

      infNFE[i].infUnidTransp[j].qtdRat := qtdRat_UnidTransp;

      k := 0;
      InfNFE[i].infUnidTransp[j].lacUnidTransp.Clear;

      while Leitor.rExtrai(Nivel + 2, 'lacUnidTransp', '', k + 1) <> '' do
      begin
        InfNFE[i].infUnidTransp[j].lacUnidTransp.New;
        InfNFE[i].infUnidTransp[j].lacUnidTransp[k].nLacre := Leitor.rCampo(tcStr, 'nLacre');

        inc(k);
      end;

      k := 0;
      InfNFE[i].infUnidTransp[j].infUnidCarga.Clear;

      while Leitor.rExtrai(Nivel + 2, 'infUnidCarga', '', k + 1) <> '' do
      begin
        InfNFE[i].infUnidTransp[j].infUnidCarga.New;
        InfNFE[i].infUnidTransp[j].infUnidCarga[k].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
        InfNFE[i].infUnidTransp[j].infUnidCarga[k].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
        InfNFE[i].infUnidTransp[j].infUnidCarga[k].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

        l := 0;
        InfNFE[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga.Clear;
        while Leitor.rExtrai(Nivel + 3, 'lacUnidCarga', '', l + 1) <> '' do
        begin
          InfNFE[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga.New;
          InfNFE[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga[l].nLacre := Leitor.rCampo(tcStr, 'nLacre');

          inc(l);
        end;

        inc(k);
      end;

      inc(j);
    end;

    if j = 0 then
    begin
      infNFE[i].infUnidCarga.Clear;

      while Leitor.rExtrai(Nivel + 1, 'infUnidCarga', '', j + 1) <> '' do
      begin
        infNFE[i].infUnidCarga.New;
        infNFE[i].infUnidCarga[j].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
        infNFE[i].infUnidCarga[j].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
        infNFE[i].infUnidCarga[j].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

        k := 0;
        infNFE[i].infUnidCarga[j].lacUnidCarga.Clear;

        while Leitor.rExtrai(Nivel + 2, 'lacUnidCarga', '', k + 1) <> '' do
        begin
          infNFE[i].infUnidCarga[j].lacUnidCarga.New;
          infNFE[i].infUnidCarga[j].lacUnidCarga[k].nLacre := Leitor.rCampo(tcStr, 'nLacre');

          inc(k);
        end;

        inc(j);
      end;
    end;

    inc(i);
  end;

  Result := True;
end;

function TCTeR.Ler_infOutros(Nivel: Integer;
  infOutros: TInfOutrosCollection): Boolean;
var
  i, j, k, l, pos1, pos2, pos3, len: Integer;
  ok: Boolean;
  sAux: string;
  qtdRat_UnidTransp: Double;
begin
  i := 0;
  InfOutros.Clear;

  while Leitor.rExtrai(Nivel, 'infOutros', '', i + 1) <> '' do
  begin
    InfOutros.New;
    InfOutros[i].tpDoc      := StrToTpDocumento(ok, Leitor.rCampo(tcStr, 'tpDoc'));
    InfOutros[i].descOutros := Leitor.rCampo(tcStr, 'descOutros');
    InfOutros[i].nDoc       := Leitor.rCampo(tcStr, 'nDoc');
    InfOutros[i].dEmi       := Leitor.rCampo(tcDat, 'dEmi');
    InfOutros[i].vDocFisc   := Leitor.rCampo(tcDe2, 'vDocFisc');
    InfOutros[i].dPrev      := Leitor.rCampo(tcDat, 'dPrev');

    j := 0;
    InfOutros[i].infUnidTransp.Clear;

    while Leitor.rExtrai(Nivel + 1, 'infUnidTransp', '', j + 1) <> '' do
    begin
      InfOutros[i].infUnidTransp.New;
      InfOutros[i].infUnidTransp[j].tpUnidTransp := StrToUnidTransp(ok, Leitor.rCampo(tcStr, 'tpUnidTransp'));
      InfOutros[i].infUnidTransp[j].idUnidTransp := Leitor.rCampo(tcStr, 'idUnidTransp');

      // Dentro do grupo <infUnidTransp> podemos ter até duas tags <qtdRat>
      // uma pertencente ao grupo <infUnidCarga> filha de <infUnidTransp> e
      // a outra pertencente ao grupo <infUnidTransp> e ambas são opcionais.
      // precisamos saber se existe uma ocorrência ou duas dessa tag para
      // efetuar a leitura correta das informações.

      sAux := Leitor.Grupo;
      pos1 := PosLast('</infUnidCarga>', sAux);
      pos2 := PosLast('<qtdRat>', sAux);
      pos3 := PosLast('</qtdRat>', sAux);
      len  := pos3 - pos2;

      if (pos1 < pos3) then
        qtdRat_UnidTransp := StringToFloatDef(Copy(sAux, pos2 + 8, len -8), 0)
      else
        qtdRat_UnidTransp := 0.0;

      infOutros[i].infUnidTransp[j].qtdRat := qtdRat_UnidTransp;

      k := 0;
      InfOutros[i].infUnidTransp[j].lacUnidTransp.Clear;

      while Leitor.rExtrai(Nivel + 2, 'lacUnidTransp', '', k + 1) <> '' do
      begin
        InfOutros[i].infUnidTransp[j].lacUnidTransp.New;
        InfOutros[i].infUnidTransp[j].lacUnidTransp[k].nLacre := Leitor.rCampo(tcStr, 'nLacre');

        inc(k);
      end;

      k := 0;
      InfOutros[i].infUnidTransp[j].infUnidCarga.Clear;

      while Leitor.rExtrai(Nivel + 2, 'infUnidCarga', '', k + 1) <> '' do
      begin
        InfOutros[i].infUnidTransp[j].infUnidCarga.New;
        InfOutros[i].infUnidTransp[j].infUnidCarga[k].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
        InfOutros[i].infUnidTransp[j].infUnidCarga[k].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
        InfOutros[i].infUnidTransp[j].infUnidCarga[k].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

        l := 0;
        InfOutros[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga.Clear;

        while Leitor.rExtrai(Nivel + 3, 'lacUnidCarga', '', l + 1) <> '' do
        begin
          InfOutros[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga.New;
          InfOutros[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga[l].nLacre := Leitor.rCampo(tcStr, 'nLacre');

          inc(l);
        end;

        inc(k);
      end;

      inc(j);
    end;

    if j = 0 then
    begin
      InfOutros[i].infUnidCarga.Clear;

      while Leitor.rExtrai(Nivel + 1, 'infUnidCarga', '', j + 1) <> '' do
      begin
        InfOutros[i].infUnidCarga.New;
        InfOutros[i].infUnidCarga[j].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
        InfOutros[i].infUnidCarga[j].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
        InfOutros[i].infUnidCarga[j].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

        k := 0;
        InfOutros[i].infUnidCarga[j].lacUnidCarga.Clear;

        while Leitor.rExtrai(Nivel + 2, 'lacUnidCarga', '', k + 1) <> '' do
        begin
          InfOutros[i].infUnidCarga[j].lacUnidCarga.New;
          InfOutros[i].infUnidCarga[j].lacUnidCarga[k].nLacre := Leitor.rCampo(tcStr, 'nLacre');

          inc(k);
        end;

        inc(j);
      end;
    end;

    inc(i);
  end;

  Result := True;
end;

function TCTeR.Ler_Rodo(rodo: TRodo): Boolean;
var
  sAux: string;
  i: Integer;
  ok: Boolean;
begin
  if Leitor.rExtrai(2, 'rodo') <> '' then
  begin
    rodo.RNTRC := Leitor.rCampo(tcStr,'RNTRC');
    rodo.dPrev := Leitor.rCampo(tcDat,'dPrev');

    sAux := Leitor.rCampo(tcStr, 'lota');

    if sAux <> '' then
      rodo.lota := StrToTpLotacao(ok, sAux);

    rodo.CIOT := Leitor.rCampo(tcStr, 'CIOT');

    if VersaoDF < ve200 then
    begin
      for i := 0 to CTe.infCTeNorm.infDoc.InfNFE.count-1 do
      begin
        CTe.infCTeNorm.infDoc.InfNFE[i].dPrev := rodo.dPrev;
      end;
      for i := 0 to CTe.infCTeNorm.infDoc.InfNF.count-1 do
      begin
        CTe.infCTeNorm.infDoc.InfNF[i].dPrev := rodo.dPrev;
      end;
      for i := 0 to CTe.infCTeNorm.infDoc.InfOutros.count-1 do
      begin
        CTe.infCTeNorm.infDoc.InfOutros[i].dPrev := rodo.dPrev;
      end;
    end;

    i := 0;
    rodo.occ.Clear;

    while Leitor.rExtrai(3, 'occ', '', i + 1) <> '' do
    begin
      rodo.occ.New;
      rodo.occ[i].serie := Leitor.rCampo(tcStr, 'serie');
      rodo.occ[i].nOcc  := Leitor.rCampo(tcInt, 'nOcc');
      rodo.occ[i].dEmi  := Leitor.rCampo(tcDat, 'dEmi');

      if Leitor.rExtrai(4, 'emiOcc') <> '' then
      begin
        rodo.occ[i].emiOcc.CNPJ := Leitor.rCampo(tcStr, 'CNPJ');
        rodo.occ[i].emiOcc.cInt := Leitor.rCampo(tcStr, 'cInt');
        rodo.occ[i].emiOcc.IE   := Leitor.rCampo(tcStr, 'IE');
        rodo.occ[i].emiOcc.UF   := Leitor.rCampo(tcStr, 'UF');
        rodo.occ[i].emiOcc.fone := Leitor.rCampo(tcStr, 'fone');
      end;

      inc(i);
    end;

    i := 0;
    rodo.valePed.Clear;

    while Leitor.rExtrai(3, 'valePed', '', i + 1) <> '' do
    begin
      rodo.valePed.New;
      rodo.valePed[i].CNPJForn := Leitor.rCampo(tcStr, 'CNPJForn');
      rodo.valePed[i].nCompra  := Leitor.rCampo(tcStr, 'nCompra');
      rodo.valePed[i].CNPJPg   := Leitor.rCampo(tcStr, 'CNPJPg');
      rodo.valePed[i].vValePed := Leitor.rCampo(tcDe2, 'vValePed');

      inc(i);
    end;

    i := 0;
    rodo.veic.Clear;

    while Leitor.rExtrai(3, 'veic', '', i + 1) <> '' do
    begin
      rodo.veic.New;
      rodo.veic[i].cInt    := Leitor.rCampo(tcStr, 'cInt');
      rodo.veic[i].RENAVAM := Leitor.rCampo(tcStr, 'RENAVAM');
      rodo.veic[i].placa   := Leitor.rCampo(tcStr, 'placa');
      rodo.veic[i].tara    := Leitor.rCampo(tcInt, 'tara');
      rodo.veic[i].capKG   := Leitor.rCampo(tcInt, 'capKG');
      rodo.veic[i].capM3   := Leitor.rCampo(tcInt, 'capM3');
      rodo.veic[i].tpProp  := StrToTpPropriedade(ok, Leitor.rCampo(tcStr, 'tpProp'));
      rodo.veic[i].tpVeic  := StrToTpVeiculo(ok, Leitor.rCampo(tcStr, 'tpVeic'));
      rodo.veic[i].tpRod   := StrToTpRodado(ok, Leitor.rCampo(tcStr, 'tpRod'));
      rodo.veic[i].tpCar   := StrToTpCarroceria(ok, Leitor.rCampo(tcStr, 'tpCar'));
      rodo.veic[i].UF      := Leitor.rCampo(tcStr, 'UF');

      if Leitor.rExtrai(4, 'prop') <> '' then
      begin
        rodo.veic[i].prop.CNPJCPF := Leitor.rCampoCNPJCPF;
        rodo.veic[i].prop.RNTRC   := Leitor.rCampo(tcStr, 'RNTRC');
        rodo.veic[i].prop.xNome   := Leitor.rCampo(tcStr, 'xNome');
        rodo.veic[i].prop.IE      := Leitor.rCampo(tcStr, 'IE');
        rodo.veic[i].prop.UF      := Leitor.rCampo(tcStr, 'UF');
        rodo.veic[i].prop.tpProp  := StrToTpProp(ok, Leitor.rCampo(tcStr, 'tpProp'));
      end;

      inc(i);
    end;

    i := 0;
    rodo.lacRodo.Clear;

    while Leitor.rExtrai(3, 'lacRodo', '', i + 1) <> '' do
    begin
      rodo.lacRodo.New;
      rodo.lacRodo[i].nLacre := Leitor.rCampo(tcStr, 'nLacre');

      inc(i);
    end;

    i := 0;
    Rodo.moto.Clear;

    while Leitor.rExtrai(3, 'moto', '', i + 1) <> '' do
    begin
      Rodo.moto.New;
      Rodo.moto[i].xNome := Leitor.rCampo(tcStr, 'xNome');
      Rodo.moto[i].CPF   := Leitor.rCampo(tcStr, 'CPF');

      inc(i);
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_RodoOS(rodoOS: TRodoOS): Boolean;
var
  ok: Boolean;
begin
  if Leitor.rExtrai(2, 'rodoOS') <> '' then
  begin
    rodoOS.TAF            := Leitor.rCampo(tcStr, 'TAF');
    rodoOS.NroRegEstadual := Leitor.rCampo(tcStr, 'NroRegEstadual');

    if Leitor.rExtrai(3, 'veic') <> '' then
    begin
      rodoOS.veic.placa   := Leitor.rCampo(tcStr, 'placa');
      rodoOS.veic.RENAVAM := Leitor.rCampo(tcStr, 'RENAVAM');
      rodoOS.veic.UF      := Leitor.rCampo(tcStr, 'UF');

      if Leitor.rExtrai(4, 'prop') <> '' then
      begin
        rodoOS.veic.prop.CNPJCPF        := Leitor.rCampoCNPJCPF;
        rodoOS.veic.prop.TAF            := Leitor.rCampo(tcStr, 'TAF');
        rodoOS.veic.prop.NroRegEstadual := Leitor.rCampo(tcStr, 'NroRegEstadual');
        rodoOS.veic.prop.xNome          := Leitor.rCampo(tcStr, 'xNome');
        rodoOS.veic.prop.IE             := Leitor.rCampo(tcStr, 'IE');
        rodoOS.veic.prop.UF             := Leitor.rCampo(tcStr, 'UF');
        rodoOS.veic.prop.tpProp         := StrToTpProp(ok, Leitor.rCampo(tcStr, 'tpProp'));
      end;
    end;

    if Leitor.rExtrai(3, 'infFretamento') <> '' then
    begin
      rodoOS.infFretamento.tpFretamento := StrToTpFretamento(ok, Leitor.rCampo(tcStr, 'tpFretamento'));
      rodoOS.infFretamento.dhViagem     := Leitor.rCampo(tcDatHor,'dhViagem');
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_Aereo(aereo: TAereo): Boolean;
var
  i: Integer;
  ok: Boolean;
begin
  if Leitor.rExtrai(2, 'aereo') <> '' then
  begin
    aereo.nMinu      := Leitor.rCampo(tcInt,'nMinu');
    aereo.nOCA       := Leitor.rCampo(tcStr,'nOCA');
    aereo.dPrevAereo := Leitor.rCampo(tcDat,'dPrevAereo');
    aereo.xLAgEmi    := Leitor.rCampo(tcStr,'xLAgEmi');
    aereo.IdT        := Leitor.rCampo(tcStr,'IdT');

    if Leitor.rExtrai(3, 'tarifa') <> '' then
    begin
      aereo.tarifa.CL   := Leitor.rCampo(tcStr,'CL');
      aereo.tarifa.cTar := Leitor.rCampo(tcStr,'cTar');
      aereo.tarifa.vTar := Leitor.rCampo(tcDe2,'vTar');
    end;

    if Leitor.rExtrai(3, 'natCarga') <> '' then
    begin
      aereo.natCarga.xDime := Leitor.rCampo(tcStr,'xDime');
      aereo.natCarga.cIMP  := Leitor.rCampo(tcStr,'cIMP');

      i := 0;
      aereo.natCarga.cinfManu.Clear;

      while Leitor.rExtrai(4, 'cInfManu', '', i + 1) <> '' do
      begin
        aereo.natCarga.cinfManu.New;

        if VersaoDF >= ve300 then
          aereo.natCarga.cinfManu[i].nInfManu := StrToTpInfManu(ok, Leitor.rCampo(tcStr,'cInfManu'))
        else
          aereo.natCarga.cinfManu[i].nInfManu := StrToTpInfManuV2(ok, Leitor.rCampo(tcStr,'cInfManu'));

        inc(i);
      end;
    end;

    i := 0;
    aereo.peri.Clear;
    while Leitor.rExtrai(2, 'peri', '', i + 1) <> '' do
    begin
      aereo.peri.New;
      aereo.peri[i].nONU        := Leitor.rCampo(tcStr, 'nONU');
      aereo.peri[i].xNomeAE     := Leitor.rCampo(tcStr, 'xNomeAE');
      aereo.peri[i].xClaRisco   := Leitor.rCampo(tcStr, 'xClaRisco');
      aereo.peri[i].grEmb       := Leitor.rCampo(tcStr, 'grEmb');
      aereo.peri[i].qTotProd    := Leitor.rCampo(tcStr, 'qTotProd');
      aereo.peri[i].qVolTipo    := Leitor.rCampo(tcStr, 'qVolTipo');
      aereo.peri[i].pontoFulgor := Leitor.rCampo(tcStr, 'pontoFulgor');
      aereo.peri[i].qTotEmb     := Leitor.rCampo(tcStr, 'qTotEmb');
      aereo.peri[i].uniAP       := StrToUniMed(ok, Leitor.rCampo(tcStr, 'uniAP'));

      inc(i);
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_Aquav(aquav: TAquav): Boolean;
var
  ok: Boolean;
  i, j: Integer;
begin
  if Leitor.rExtrai(2, 'aquav') <> '' then
  begin
    aquav.vPrest   := Leitor.rCampo(tcDe2,'vPrest');
    aquav.vAFRMM   := Leitor.rCampo(tcDe2,'vAFRMM');
    aquav.nBooking := Leitor.rCampo(tcStr,'nBooking');
    aquav.nCtrl    := Leitor.rCampo(tcStr,'nCtrl');
    aquav.xNavio   := Leitor.rCampo(tcStr,'xNavio');
    aquav.nViag    := Leitor.rCampo(tcStr,'nViag');
    aquav.direc    := StrToTpDirecao(ok, Leitor.rCampo(tcStr, 'direc'));
    aquav.prtEmb   := Leitor.rCampo(tcStr,'prtEmb');
    aquav.prtTrans := Leitor.rCampo(tcStr,'prtTrans');
    aquav.prtDest  := Leitor.rCampo(tcStr,'prtDest');
    aquav.tpNav    := StrToTpNavegacao(ok, Leitor.rCampo(tcStr, 'tpNav'));
    aquav.irin     := Leitor.rCampo(tcStr,'irin');

    i := 0;
    aquav.balsa.Clear;

    while Leitor.rExtrai(3, 'balsa', '', i + 1) <> '' do
    begin
      aquav.balsa.New;
      aquav.balsa[i].xBalsa := Leitor.rCampo(tcStr, 'xBalsa');

      inc(i);
    end;

    i := 0;
    aquav.detCont.Clear;

    while Leitor.rExtrai(3, 'detCont', '', i + 1) <> '' do
    begin
      aquav.detCont.New;
      aquav.detCont[i].nCont := Leitor.rCampo(tcStr, 'nCont');

      j := 0;
      aquav.detCont[i].Lacre.Clear;

      while Leitor.rExtrai(4, 'lacre', '', j + 1) <> '' do
      begin
        aquav.detCont[i].Lacre.New;
        aquav.detCont[i].Lacre[j].nLacre := Leitor.rCampo(tcStr, 'nLacre');

        inc(j);
      end;

      if Leitor.rExtrai(4, 'infDoc') <> '' then
      begin
        j := 0;
        aquav.detCont[i].infDoc.infNF.Clear;

        while Leitor.rExtrai(5, 'infNF', '', j + 1) <> '' do
        begin
          aquav.detCont[i].infDoc.infNF.New;
          aquav.detCont[i].infDoc.infNF[j].serie   := Leitor.rCampo(tcStr, 'Serie');
          aquav.detCont[i].infDoc.infNF[j].nDoc    := Leitor.rCampo(tcStr, 'nDoc');
          aquav.detCont[i].infDoc.infNF[j].unidRat := Leitor.rCampo(tcDe2, 'unidRat');

          inc(j);
        end;

        j := 0;
        aquav.detCont[i].infDoc.infNFe.Clear;

        while Leitor.rExtrai(5, 'infNFe', '', j + 1) <> '' do
        begin
          aquav.detCont[i].infDoc.infNFe.New;
          aquav.detCont[i].infDoc.infNFe[j].chave   := Leitor.rCampo(tcStr, 'chave');
          aquav.detCont[i].infDoc.infNFe[j].unidRat := Leitor.rCampo(tcDe2, 'unidRat');

          inc(j);
        end;
      end;

      inc(i);
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_Ferrov(ferrov: TFerrov): Boolean;
var
  ok: Boolean;
  i: Integer;
begin
  if Leitor.rExtrai(2, 'ferrov') <> '' then
  begin
    ferrov.tpTraf := StrToTpTrafego(ok, Leitor.rCampo(tcStr, 'tpTraf'));
    ferrov.fluxo  := Leitor.rCampo(tcStr,'fluxo');
    ferrov.idTrem := Leitor.rCampo(tcStr,'idTrem');

    if VersaoDF >= ve300 then
    begin
      if Leitor.rExtrai(3, 'trafMut') <> '' then
      begin
        ferrov.trafMut.respFat := StrToTrafegoMutuo(ok, Leitor.rCampo(tcStr, 'respFat'));
        ferrov.trafMut.ferrEmi := StrToTrafegoMutuo(ok, Leitor.rCampo(tcStr, 'ferrEmi'));
        ferrov.vFrete          := Leitor.rCampo(tcDe2,'vFrete');

        ferrov.trafMut.chCTeFerroOrigem := Leitor.rCampo(tcStr,'chCTeFerroOrigem');

        i := 0;
        ferrov.ferroEnv.Clear;

        while Leitor.rExtrai(4, 'ferroEnv', '', i + 1) <> '' do
        begin
          ferrov.ferroEnv.New;
          ferrov.ferroEnv[i].CNPJ  := Leitor.rCampo(tcStr,'CNPJ');
          ferrov.ferroEnv[i].cInt  := Leitor.rCampo(tcStr,'cInt');
          ferrov.ferroEnv[i].IE    := Leitor.rCampo(tcStr,'IE');
          ferrov.ferroEnv[i].xNome := Leitor.rCampo(tcStr,'xNome');

          if Leitor.rExtrai(5, 'enderFerro') <> '' then
          begin
            ferrov.ferroEnv[i].EnderFerro.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
            ferrov.ferroEnv[i].EnderFerro.nro     := Leitor.rCampo(tcStr, 'nro');
            ferrov.ferroEnv[i].EnderFerro.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
            ferrov.ferroEnv[i].EnderFerro.xBairro := Leitor.rCampo(tcStr, 'xBairro');
            ferrov.ferroEnv[i].EnderFerro.cMun    := Leitor.rCampo(tcInt, 'cMun');
            ferrov.ferroEnv[i].EnderFerro.xMun    := Leitor.rCampo(tcStr, 'xMun');
            ferrov.ferroEnv[i].EnderFerro.CEP     := Leitor.rCampo(tcInt, 'CEP');
            ferrov.ferroEnv[i].EnderFerro.UF      := Leitor.rCampo(tcStr, 'UF');
          end;

          inc(i);
        end;
      end;
    end
    else
    begin
      ferrov.vFrete := Leitor.rCampo(tcDe2,'vFrete');

      if Leitor.rExtrai(3, 'trafMut') <> '' then
      begin
        ferrov.trafMut.respFat := StrToTrafegoMutuo(ok, Leitor.rCampo(tcStr, 'respFat'));
        ferrov.trafMut.ferrEmi := StrToTrafegoMutuo(ok, Leitor.rCampo(tcStr, 'ferrEmi'));
      end;

      i := 0;
      ferrov.ferroEnv.Clear;

      while Leitor.rExtrai(3, 'ferroEnv', '', i + 1) <> '' do
      begin
        ferrov.ferroEnv.New;
        ferrov.ferroEnv[i].CNPJ  := Leitor.rCampo(tcStr,'CNPJ');
        ferrov.ferroEnv[i].cInt  := Leitor.rCampo(tcStr,'cInt');
        ferrov.ferroEnv[i].IE    := Leitor.rCampo(tcStr,'IE');
        ferrov.ferroEnv[i].xNome := Leitor.rCampo(tcStr,'xNome');

        if Leitor.rExtrai(4, 'enderFerro') <> '' then
        begin
          ferrov.ferroEnv[i].EnderFerro.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
          ferrov.ferroEnv[i].EnderFerro.nro     := Leitor.rCampo(tcStr, 'nro');
          ferrov.ferroEnv[i].EnderFerro.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
          ferrov.ferroEnv[i].EnderFerro.xBairro := Leitor.rCampo(tcStr, 'xBairro');
          ferrov.ferroEnv[i].EnderFerro.cMun    := Leitor.rCampo(tcInt, 'cMun');
          ferrov.ferroEnv[i].EnderFerro.xMun    := Leitor.rCampo(tcStr, 'xMun');
          ferrov.ferroEnv[i].EnderFerro.CEP     := Leitor.rCampo(tcInt, 'CEP');
          ferrov.ferroEnv[i].EnderFerro.UF      := Leitor.rCampo(tcStr, 'UF');
        end;

        inc(i);
      end;

      i := 0;
      ferrov.detVag.Clear;

      while Leitor.rExtrai(3, 'detVag', '', i + 1) <> '' do
      begin
        ferrov.detVag.New;
        ferrov.detVag[i].nVag   := Leitor.rCampo(tcInt, 'nVag');
        ferrov.detVag[i].cap    := Leitor.rCampo(tcDe2, 'cap');
        ferrov.detVag[i].tpVag  := Leitor.rCampo(tcStr, 'tpVag');
        ferrov.detVag[i].pesoR  := Leitor.rCampo(tcDe2, 'pesoR');
        ferrov.detVag[i].pesoBC := Leitor.rCampo(tcDe2, 'pesoBC');

        inc(i);
      end;
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_Duto(duto: TDuto): Boolean;
begin
  if Leitor.rExtrai(2, 'duto') <> '' then
  begin
    duto.vTar := Leitor.rCampo(tcDe6, 'vTar');
    duto.dIni := Leitor.rCampo(tcDat, 'dIni');
    duto.dFim := Leitor.rCampo(tcDat, 'dFim');
  end;

  Result := True;
end;

function TCTeR.Ler_MultiModal(multimodal: TMultimodal): Boolean;
var
  ok: Boolean;
begin
  if Leitor.rExtrai(2, 'multimodal') <> '' then
  begin
    multimodal.COTM          := Leitor.rCampo(tcStr, 'COTM');
    multimodal.indNegociavel := StrToindNegociavel(ok, Leitor.rCampo(tcStr, 'indNegociavel'));

    // dados sobre o seguro informados somente na versão 3.00
    multimodal.xSeg  := Leitor.rCampo(tcStr, 'xSeg');
    multimodal.CNPJ  := Leitor.rCampo(tcStr, 'CNPJ');
    multimodal.nApol := Leitor.rCampo(tcStr, 'nApol');
    multimodal.nAver := Leitor.rCampo(tcStr, 'nAver');
  end;

  Result := True;
end;

function TCTeR.Ler_InfCTeNorm: Boolean;
var
  i, j, k: Integer;
  ok: Boolean;
begin
  if Leitor.rExtrai(1, 'infCTeNorm') <> '' then
  begin
    CTe.infCTeNorm.refCTeCanc := Leitor.rCampo(tcStr,'refCTeCanc');

    if Leitor.rExtrai(2, 'infServico') <> '' then
    begin
      CTe.infCTeNorm.infServico.xDescServ := Leitor.rCampo(tcStr,'xDescServ');
      CTe.infCTeNorm.infServico.qCarga    := Leitor.rCampo(tcDe4,'qCarga');
    end;

    i := 0;
    CTe.infCTeNorm.infDocRef.Clear;
    while Leitor.rExtrai(2, 'infDocRef', '', i + 1) <> '' do
    begin
      CTe.infCTeNorm.infDocRef.New;
      CTe.infCTeNorm.infDocRef[i].nDoc     := Leitor.rCampo(tcEsp, 'nDoc');
      CTe.infCTeNorm.infDocRef[i].serie    := Leitor.rCampo(tcStr, 'serie');
      CTe.infCTeNorm.infDocRef[i].subserie := Leitor.rCampo(tcStr, 'subserie');
      CTe.infCTeNorm.infDocRef[i].dEmi     := Leitor.rCampo(tcDat, 'dEmi');
      CTe.infCTeNorm.infDocRef[i].vDoc     := Leitor.rCampo(tcDe2, 'vDoc');

      CTe.infCTeNorm.infDocRef[i].chBPe := Leitor.rCampo(tcStr, 'chBPe');

      inc(i);
    end;

    Ler_InfCarga(2, CTe.infCTeNorm.infCarga);

    if Leitor.rExtrai(2, 'infDoc') <> '' then
    begin
      Ler_infNF(3, CTe.infCTeNorm.infDoc.InfNF);
      Ler_infNFe(3, CTe.infCTeNorm.infDoc.InfNFE);
      Ler_infOutros(3, CTe.infCTeNorm.infDoc.infOutros);
    end;

    if Leitor.rExtrai(2, 'docAnt') <> '' then
    begin
      i := 0;
      CTe.infCTeNorm.docAnt.emiDocAnt.Clear;
      while Leitor.rExtrai(3, 'emiDocAnt', '', i + 1) <> '' do
      begin
        CTe.infCTeNorm.docAnt.emiDocAnt.New;
        CTe.infCTeNorm.docAnt.emiDocAnt[i].CNPJCPF := Leitor.rCampoCNPJCPF;
        CTe.infCTeNorm.docAnt.emiDocAnt[i].IE      := Leitor.rCampo(tcStr, 'IE');
        CTe.infCTeNorm.docAnt.emiDocAnt[i].UF      := Leitor.rCampo(tcStr, 'UF');
        CTe.infCTeNorm.docAnt.emiDocAnt[i].xNome   := Leitor.rCampo(tcStr, 'xNome');

        j := 0;
        while Leitor.rExtrai(4, 'idDocAnt', '', j + 1) <> '' do
        begin
          CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt.New;

          k := 0;
          while Leitor.rExtrai(5, 'idDocAntPap', '', k + 1) <> '' do
          begin
            CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap.New;
            CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].tpDoc  := StrToTpDocumentoAnterior(ok, Leitor.rCampo(tcStr, 'tpDoc'));
            CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].serie  := Leitor.rCampo(tcStr, 'serie');
            CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].subser := Leitor.rCampo(tcStr, 'subser');
            CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].nDoc   := Leitor.rCampo(tcStr, 'nDoc');
            CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].dEmi   := Leitor.rCampo(tcDat, 'dEmi');
            inc(k);
          end;

          k := 0;
          while Leitor.rExtrai(5, 'idDocAntEle', '', k + 1) <> '' do
          begin
            CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntEle.New;

            if (VersaoDF >= ve300) then
              CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntEle[k].chCTe := Leitor.rCampo(tcStr, 'chCTe')
            else
              CTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntEle[k].chave := Leitor.rCampo(tcStr, 'chave');

            inc(k);
          end;
          inc(j);
        end;
        inc(i);
      end;
    end;

    i := 0;
    CTe.infCTeNorm.seg.Clear;
    while Leitor.rExtrai(2, 'seg', '', i + 1) <> '' do
    begin
      CTe.infCTeNorm.seg.New;
      CTe.infCTeNorm.seg[i].respSeg := StrToTpRspSeguro(ok, Leitor.rCampo(tcStr, 'respSeg'));
      CTe.infCTeNorm.seg[i].xSeg    := Leitor.rCampo(tcStr, 'xSeg');
      CTe.infCTeNorm.seg[i].nApol   := Leitor.rCampo(tcStr, 'nApol');
      CTe.infCTeNorm.seg[i].nAver   := Leitor.rCampo(tcStr, 'nAver');
      CTe.infCTeNorm.seg[i].vCarga  := Leitor.rCampo(tcDe2, 'vCarga');
      inc(i);
    end;

    Ler_Rodo(CTe.infCTeNorm.rodo);
    Ler_RodoOS(CTe.infCTeNorm.rodoOS);
    Ler_Aereo(CTe.infCTeNorm.aereo);
    Ler_Aquav(CTe.infCTeNorm.aquav);
    Ler_Ferrov(CTe.infCTeNorm.ferrov);
    Ler_Duto(CTe.infCTeNorm.duto);
    Ler_MultiModal(CTe.infCTeNorm.multimodal);

    i := 0;
    CTe.infCTeNorm.peri.Clear;
    while Leitor.rExtrai(2, 'peri', '', i + 1) <> '' do
    begin
      CTe.infCTeNorm.peri.New;
      CTe.infCTeNorm.peri[i].nONU        := Leitor.rCampo(tcStr, 'nONU');
      CTe.infCTeNorm.peri[i].xNomeAE     := Leitor.rCampo(tcStr, 'xNomeAE');
      CTe.infCTeNorm.peri[i].xClaRisco   := Leitor.rCampo(tcStr, 'xClaRisco');
      CTe.infCTeNorm.peri[i].grEmb       := Leitor.rCampo(tcStr, 'grEmb');
      CTe.infCTeNorm.peri[i].qTotProd    := Leitor.rCampo(tcStr, 'qTotProd');
      CTe.infCTeNorm.peri[i].qVolTipo    := Leitor.rCampo(tcStr, 'qVolTipo');
      CTe.infCTeNorm.peri[i].pontoFulgor := Leitor.rCampo(tcStr, 'pontoFulgor');
      CTe.infCTeNorm.peri[i].qTotEmb     := Leitor.rCampo(tcStr, 'qTotEmb');
      CTe.infCTeNorm.peri[i].uniAP       := StrToUniMed(ok, Leitor.rCampo(tcStr, 'uniAP'));
      inc(i);
    end;

    i := 0;
    CTe.infCTeNorm.veicNovos.Clear;
    while Leitor.rExtrai(2, 'veicNovos', '', i + 1) <> '' do
    begin
      CTe.infCTeNorm.veicNovos.New;
      CTe.infCTeNorm.veicNovos[i].chassi := Leitor.rCampo(tcStr, 'chassi');
      CTe.infCTeNorm.veicNovos[i].cCor   := Leitor.rCampo(tcStr, 'cCor');
      CTe.infCTeNorm.veicNovos[i].xCor   := Leitor.rCampo(tcStr, 'xCor');
      CTe.infCTeNorm.veicNovos[i].cMod   := Leitor.rCampo(tcStr, 'cMod');
      CTe.infCTeNorm.veicNovos[i].vUnit  := Leitor.rCampo(tcDe2, 'vUnit');
      CTe.infCTeNorm.veicNovos[i].vFrete := Leitor.rCampo(tcDe2, 'vFrete');
      inc(i);
    end;

    Ler_Cobr(2, CTe.infCTeNorm.cobr);

    i := 0;
    CTe.infCTeNorm.infGTVe.Clear;
    while Leitor.rExtrai(2, 'infGTVe', '', i + 1) <> '' do
    begin
      CTe.infCTeNorm.infGTVe.New;
      CTe.infCTeNorm.infGTVe[i].chCTe  := Leitor.rCampo(tcStr, 'chCTe');

      j := 0;
      while Leitor.rExtrai(3, 'Comp', '', j + 1) <> '' do
      begin
        CTe.infCTeNorm.infGTVe[i].Comp.New;
        CTe.infCTeNorm.infGTVe[i].Comp[j].tpComp := StrTotpComp(Ok, Leitor.rCampo(tcStr, 'tpComp'));
        CTe.infCTeNorm.infGTVe[i].Comp[j].vComp  := Leitor.rCampo(tcDe2, 'vComp');
        CTe.infCTeNorm.infGTVe[i].Comp[j].xComp  := Leitor.rCampo(tcStr, 'xComp');
        inc(j);
      end;

      inc(i);
    end;

    Ler_InfCTeSub(2, CTe.infCTeNorm.infCTeSub);

    if Leitor.rExtrai(2, 'infGlobalizado') <> '' then
      CTe.infCTeNorm.infGlobalizado.xObs := Leitor.rCampo(tcStr, 'xObs');

    if Leitor.rExtrai(2, 'infServVinc') <> '' then
    begin
      i := 0;
      CTe.infCTeNorm.infServVinc.infCTeMultimodal.Clear;
      while Leitor.rExtrai(3, 'infCTeMultimodal', '', i + 1) <> '' do
      begin
        CTe.infCTeNorm.infServVinc.infCTeMultimodal.New;
        CTe.infCTeNorm.infServVinc.infCTeMultimodal[i].chCTeMultimodal := Leitor.rCampo(tcStr, 'chCTeMultimodal');
        inc(i);
      end;
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_InfCTeComp: Boolean;
var
  i: Integer;
begin
  if VersaoDF <= ve300 then
  begin
    if Leitor.rExtrai(1, 'infCteComp') <> ''
    then
    begin
      if (VersaoDF >= ve300) then
         CTe.InfCTeComp.Chave := Leitor.rCampo(tcStr, 'chCTe')
      else
        CTe.InfCTeComp.Chave := Leitor.rCampo(tcStr, 'chave');
    end;
  end
  else
  begin
    i := 0;
    CTe.InfCTeComp10.Clear;

    while Leitor.rExtrai(1, 'infCteComp', '', i + 1) <> '' do
    begin
      CTe.InfCTeComp10.New;
      CTe.InfCTeComp10[i].chCTe := Leitor.rCampo(tcStr, 'chCTe');
      inc(i);
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_InfCarga(Nivel: Integer; infCarga: TInfCarga): Boolean;
var
  i: Integer;
begin
  if Leitor.rExtrai(Nivel, 'infCarga') <> ''
  then begin
    infCarga.vCarga      := Leitor.rCampo(tcDe2,'vCarga');
    InfCarga.proPred     := Leitor.rCampo(tcStr,'proPred');
    InfCarga.xOutCat     := Leitor.rCampo(tcStr,'xOutCat');
    infCarga.vCargaAverb := Leitor.rCampo(tcDe2,'vCargaAverb');

    i := 0;
    InfCarga.infQ.Clear;

    while Leitor.rExtrai(Nivel + 1, 'infQ', '', i + 1) <> '' do
    begin
      InfCarga.infQ.New;
      InfCarga.infQ[i].cUnid  := Leitor.rCampo(tcStr, 'cUnid');
      InfCarga.infQ[i].tpMed  := Leitor.rCampo(tcStr, 'tpMed');
      InfCarga.infQ[i].qCarga := Leitor.rCampo(tcDe4, 'qCarga');

      inc(i);
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_InfDocAnt(infDocAnt: TinfDocAntCollection): Boolean;
var
  i, j: Integer;
  ok: Boolean;
begin
  i := 0;
  infDocAnt.Clear;

  while Leitor.rExtrai(2, 'infDocAnt', '', i + 1) <> '' do
  begin
    infDocAnt.New;
    infDocAnt[i].chCTe := Leitor.rCampo(tcStr, 'chCTe');
    infDocAnt[i].tpPrest := StrTotpPrest(ok, Leitor.rCampo(tcStr, 'tpPrest'));

    j := 0;
    infDocAnt[i].infNFeTranspParcial.Clear;

    while Leitor.rExtrai(3, 'infNFeTranspParcial', '', j + 1) <> '' do
    begin
      infDocAnt[i].infNFeTranspParcial.New;
      infDocAnt[i].infNFeTranspParcial[j].chNFe := Leitor.rCampo(tcStr, 'chNFe');

      inc(j);
    end;

    inc(i);
  end;

  Result := True;
end;

function TCTeR.Ler_Det: Boolean;
var
  i: Integer;
begin
  i := 0;
  CTe.det.Clear;

  while Leitor.rExtrai(1, 'det', '', i + 1) <> '' do
  begin
    CTe.det.New;
    CTe.det[i].cMunIni := Leitor.rCampo(tcInt, 'cMunIni');
    CTe.det[i].xMunIni := Leitor.rCampo(tcStr, 'xMunIni');
    CTe.det[i].cMunFim := Leitor.rCampo(tcInt, 'cMunFim');
    CTe.det[i].xMunFim := Leitor.rCampo(tcStr, 'xMunFim');
    CTe.det[i].vPrest := Leitor.rCampo(tcDe2, 'vPrest');
    CTe.det[i].vRec := Leitor.rCampo(tcDe2, 'vRec');

    Ler_Comp(CTe.det[i].Comp);
    Ler_InfNFe(2, CTe.det[i].infNFe);
    Ler_InfDocAnt(CTe.det[i].infdocAnt);

    inc(i);
  end;

  Result := True;
end;

function TCTeR.Ler_InfModal: Boolean;
begin
  if Leitor.rExtrai(1, 'infModal') <> '' then
  begin
    Ler_Rodo(CTe.infModal.rodo);
    Ler_Aereo(CTe.infModal.aereo);
    Ler_Aquav(CTe.infModal.aquav);
  end;

  Result := True;
end;

function TCTeR.Ler_Cobr(Nivel: Integer; cobr: TCobr): Boolean;
var
  i: Integer;
begin
  if Leitor.rExtrai(Nivel, 'cobr') <> '' then
  begin
    cobr.fat.nFat  := Leitor.rCampo(tcStr, 'nFat');
    cobr.fat.vOrig := Leitor.rCampo(tcDe2, 'vOrig');
    cobr.fat.vDesc := Leitor.rCampo(tcDe2, 'vDesc');
    cobr.fat.vLiq  := Leitor.rCampo(tcDe2, 'vLiq');

    i := 0;
    cobr.dup.Clear;

    while Leitor.rExtrai(Nivel + 1, 'dup', '', i + 1) <> '' do
    begin
      cobr.dup.New;
      cobr.dup[i].nDup  := Leitor.rCampo(tcStr, 'nDup');
      cobr.dup[i].dVenc := Leitor.rCampo(tcDat, 'dVenc');
      cobr.dup[i].vDup  := Leitor.rCampo(tcDe2, 'vDup');

      inc(i);
    end;
  end;

  Result := True;
end;

function TCTeR.Ler_InfCTeSub(Nivel: Integer; infCTeSub: TInfCteSub): Boolean;
var
  ok: Boolean;
begin
  if Leitor.rExtrai(Nivel, 'infCteSub') <> '' then
  begin
    infCTeSub.chCte := Leitor.rCampo(tcStr, 'chCte');

    if VersaoDF >= ve300 then
      infCTeSub.refCteAnu := Leitor.rCampo(tcStr, 'refCteAnu');

    if Leitor.rCampo(tcStr, 'indAlteraToma') <> '' then
      infCTeSub.indAlteraToma := StrToTIndicador(Ok, Leitor.rCampo(tcStr, 'indAlteraToma'));

    if Leitor.rExtrai(Nivel + 1, 'tomaICMS') <> '' then
    begin
      infCTeSub.tomaICMS.refNFe := Leitor.rCampo(tcStr, 'refNFe');
      infCTeSub.tomaICMS.refCte := Leitor.rCampo(tcStr, 'refCte');

      if Leitor.rExtrai(Nivel + 2, 'refNF') <> '' then
      begin
        infCTeSub.tomaICMS.refNF.CNPJCPF  := Leitor.rCampoCNPJCPF;
        infCTeSub.tomaICMS.refNF.modelo   := Leitor.rCampo(tcStr, 'mod');
        infCTeSub.tomaICMS.refNF.serie    := Leitor.rCampo(tcInt, 'serie');
        infCTeSub.tomaICMS.refNF.subserie := Leitor.rCampo(tcInt, 'subserie');
        infCTeSub.tomaICMS.refNF.nro      := Leitor.rCampo(tcInt, 'nro');
        infCTeSub.tomaICMS.refNF.valor    := Leitor.rCampo(tcDe2, 'valor');
        infCTeSub.tomaICMS.refNF.dEmi     := Leitor.rCampo(tcDat, 'dEmi');
      end;
    end;

    if Leitor.rExtrai(Nivel + 1, 'tomaNaoICMS') <> '' then
      infCTeSub.tomaNaoICMS.refCteAnu := Leitor.rCampo(tcStr, 'refCteAnu');
  end;

  Result := True;
end;

function TCTeR.Ler_Total: Boolean;
begin
  if Leitor.rExtrai(1, 'total') <> '' then
  begin
    CTe.total.vTPrest := Leitor.rCampo(tcDe2,'vTPrest');
    CTe.total.vTRec   := Leitor.rCampo(tcDe2,'vTRec');
  end;

  Result := True;
end;

function TCTeR.Ler_InfCTeAnu: Boolean;
begin
  if Leitor.rExtrai(1, 'infCteAnu') <> '' then
  begin
    CTe.InfCTeAnu.chCTe := Leitor.rCampo(tcStr,'chCte');
    CTe.InfCTeAnu.dEmi  := Leitor.rCampo(tcDat,'dEmi');
  end;

  Result := True;
end;

function TCTeR.Ler_AutXML: Boolean;
var
  i: Integer;
begin
  i := 0;
  CTe.autXML.Clear;

  while Leitor.rExtrai(1, 'autXML', '', i + 1) <> '' do
  begin
    CTe.autXML.New;
    CTe.autXML[i].CNPJCPF := Leitor.rCampoCNPJCPF;;
    inc(i);
  end;

  Result := True;
end;

function TCTeR.Ler_InfRespTec: Boolean;
begin
  if Leitor.rExtrai(1, 'infRespTec') <> '' then
  begin
    CTe.infRespTec.CNPJ     := Leitor.rCampo(tcStr, 'CNPJ');
    CTe.infRespTec.xContato := Leitor.rCampo(tcStr, 'xContato');
    CTe.infRespTec.email    := Leitor.rCampo(tcStr, 'email');
    CTe.infRespTec.fone     := Leitor.rCampo(tcStr, 'fone');
    CTe.infRespTec.idCSRT   := Leitor.rCampo(tcInt, 'idCSRT');
    CTe.infRespTec.hashCSRT := Leitor.rCampo(tcStr, 'hashCSRT');
  end;

  Result := True;
end;

function TCTeR.Ler_InfCTeSupl: Boolean;
begin
  if Leitor.rExtrai(1, 'infCTeSupl') <> '' then
  begin
    CTe.infCTeSupl.qrCodCTe := Leitor.rCampo(tcStr, 'qrCodCTe');
    CTe.infCTeSupl.qrCodCTe := StringReplace(CTe.infCTeSupl.qrCodCTe, '<![CDATA[', '', []);
    CTe.infCTeSupl.qrCodCTe := StringReplace(CTe.infCTeSupl.qrCodCTe, ']]>', '', []);
  end;

  Result := True;
end;

function TCTeR.Ler_Signature: Boolean;
begin
  Leitor.Grupo := Leitor.Arquivo;

  CTe.signature.URI             := Leitor.rAtributo('URI=', 'Reference');
  CTe.signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
  CTe.signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
  CTe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');

  Result := True;
end;

function TCTeR.Ler_ProtCTe: Boolean;
var
  ok: Boolean;
begin
  if Leitor.rExtrai(1, 'protCTe') <> '' then
  begin
    CTe.procCTe.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
    CTe.procCTe.verAplic := Leitor.rCampo(tcStr, 'verAplic');
    CTe.procCTe.chCTe    := Leitor.rCampo(tcStr, 'chCTe');
    CTe.procCTe.dhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
    CTe.procCTe.nProt    := Leitor.rCampo(tcStr, 'nProt');
    CTe.procCTe.digVal   := Leitor.rCampo(tcStr, 'digVal');
    CTe.procCTe.cStat    := Leitor.rCampo(tcInt, 'cStat');
    CTe.procCTe.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
    CTe.procCTe.cMsg     := Leitor.rCampo(tcInt, 'cMsg');
    CTe.procCTe.xMsg     := Leitor.rCampo(tcStr, 'xMsg');
  end;

  Result := True;
end;

function TCTeR.Ler_CTe: Boolean;
begin
  Ler_Complemento;
  Ler_Emitente;
  Ler_Remetente;
  Ler_Expedidor;
  Ler_Recebedor;
  Ler_Destinatario;
  Ler_vPrest;
  Ler_Imp;
  Ler_InfCTeNorm;
  Ler_InfCTeComp;
  Ler_InfCTeAnu;
  Ler_AutXML;
  Ler_InfRespTec;
  Ler_InfCTeSupl;
  Ler_Signature;
  Ler_ProtCTe;

  Result := True;
end;

function TCTeR.Ler_CTeOS: Boolean;
begin
  Ler_Complemento;
  Ler_Emitente;
  Ler_Tomador;
  Ler_vPrest;
  Ler_Imp;
  Ler_InfCTeNorm;
  Ler_InfCTeComp;
  Ler_AutXML;
  Ler_InfRespTec;
  Ler_InfCTeSupl;
  Ler_Signature;
  Ler_ProtCTe;

  Result := True;
end;

function TCTeR.Ler_CTeSimplificado: Boolean;
begin
  Ler_Complemento;
  Ler_Emitente;
  Ler_Tomador;
  Ler_InfCarga(1, CTe.infCarga);
  Ler_Det;
  Ler_InfModal;
  Ler_Cobr(1, CTe.cobr);
  Ler_InfCTeSub(1, CTe.infCteSub);
  Ler_Imp;
  Ler_Total;
  Ler_AutXML;
  Ler_InfRespTec;
  Ler_InfCTeSupl;
  Ler_ProtCTe;
  Ler_Signature;

  Result := True;
end;

function TCTeR.Ler_GTVe: Boolean;
begin
  Ler_Complemento;
  Ler_Emitente;
  Ler_Remetente;
  Ler_Destinatario;
  Ler_Origem;
  Ler_Destino;
  Ler_DetGTV;
  Ler_AutXML;
  Ler_InfRespTec;
  Ler_InfCTeSupl;
  Ler_Signature;
  Ler_ProtCTe;

  Result := True;
end;

end.

