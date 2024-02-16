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
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
    function LerXml: boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property CTe: TCTe       read FCTe    write FCTe;
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
  i, j, i01, i02, i03, i04: Integer;
  sCST, Aspas: String;
  // as variáveis abaixo são utilizadas para identificar as várias ocorrências
  // da tag qtdRat.
  sAux: String;
  pos1, pos2, pos3, len: Integer;
  qtdRat_UnidTransp: Currency;
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

  (* Grupo da TAG <ide> *******************************************************)
  if Leitor.rExtrai(1, 'ide') <> '' then
  begin
    (*B02*)CTe.Ide.cUF      := Leitor.rCampo(tcInt, 'cUF');
    (*B03*)CTe.Ide.cCT      := Leitor.rCampo(tcStr, 'cCT');
    (*B04*)CTe.Ide.CFOP     := Leitor.rCampo(tcStr, 'CFOP');
    (*B05*)CTe.Ide.natOp    := Leitor.rCampo(tcStr, 'natOp');

    if VersaoDF < ve300 then
      (*B06*)CTe.Ide.forPag   := StrTotpforPag(ok, Leitor.rCampo(tcStr, 'forPag'));

    (*B07*)CTe.Ide.modelo   := Leitor.rCampo(tcStr, 'mod');
    (*B08*)CTe.Ide.serie    := Leitor.rCampo(tcInt, 'serie');
    (*B09*)CTe.Ide.nCT      := Leitor.rCampo(tcInt, 'nCT');
    (*B10*)CTe.Ide.dhEmi    := Leitor.rCampo(tcDatHor, 'dhEmi');
    (*B11*)CTe.Ide.tpImp    := StrToTpImp(ok, Leitor.rCampo(tcStr, 'tpImp'));
    (*B12*)CTe.Ide.tpEmis   := StrToTpEmis(ok, Leitor.rCampo(tcStr, 'tpEmis'));
    (*B13*)CTe.Ide.cDV      := Leitor.rCampo(tcInt, 'cDV');
    (*B14*)CTe.Ide.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
    (*B15*)CTe.Ide.tpCTe    := StrTotpCTe(ok, Leitor.rCampo(tcStr, 'tpCTe'));
    (*B15a*)CTe.Ide.procEmi := StrToprocEmi(ok, Leitor.rCampo(tcStr, 'procEmi'));
    (*B15b*)CTe.Ide.verProc := Leitor.rCampo(tcStr, 'verProc');

    if VersaoDF >= ve300 then
    begin
      if Leitor.rCampo(tcStr, 'indGlobalizado') = '1' then
        CTe.ide.indGlobalizado := tiSim
      else
        CTe.ide.indGlobalizado := tiNao;
    end;

    (*B15c*)CTe.Ide.refCTE  := Leitor.rCampo(tcStr, 'refCTE');
    (*B16*)CTe.Ide.cMunEnv  := Leitor.rCampo(tcInt, 'cMunEnv');
    (*B17*)CTe.Ide.xMunEnv  := Leitor.rCampo(tcStr, 'xMunEnv');
    (*B18*)CTe.Ide.UFEnv    := Leitor.rCampo(tcStr, 'UFEnv');
    (*B19*)CTe.Ide.modal    := StrToTpModal(ok, Leitor.rCampo(tcStr, 'modal'));
    (*B20*)CTe.Ide.tpServ   := StrToTpServ(ok, Leitor.rCampo(tcStr, 'tpServ'));
    (*B21*)CTe.Ide.cMunIni  := Leitor.rCampo(tcInt, 'cMunIni');
    (*B22*)CTe.Ide.xMunIni  := Leitor.rCampo(tcStr, 'xMunIni');
    (*B23*)CTe.Ide.UFIni    := Leitor.rCampo(tcStr, 'UFIni');
    (*B24*)CTe.Ide.cMunFim  := Leitor.rCampo(tcInt, 'cMunFim');
    (*B25*)CTe.Ide.xMunFim  := Leitor.rCampo(tcStr, 'xMunFim');
    (*B26*)CTe.Ide.UFFim    := Leitor.rCampo(tcStr, 'UFFim');
    (*B27*)CTe.Ide.retira   := StrToTpRetira(ok, Leitor.rCampo(tcStr, 'retira'));
    (*B27a*)CTe.Ide.xdetretira := Leitor.rCampo(tcStr, 'xDetRetira');
    (*#57*)CTe.Ide.dhCont   := Leitor.rCampo(tcDatHor, 'dhCont');
    (*#58*)CTe.Ide.xJust    := Leitor.rCampo(tcStr, 'xJust');

    if VersaoDF >= ve300 then
      CTe.ide.indIEToma := StrToindIEDest(Ok, Leitor.rCampo(tcStr, 'indIEToma'));

    CTe.Ide.dhSaidaOrig   := Leitor.rCampo(tcDatHor, 'dhSaidaOrig');
    CTe.Ide.dhChegadaDest := Leitor.rCampo(tcDatHor, 'dhChegadaDest');

    i01 := 0;
    CTe.Ide.infPercurso.Clear;
    while Leitor.rExtrai(2, 'infPercurso', '', i01 + 1) <> '' do
    begin
      CTe.Ide.infPercurso.New;
      CTe.Ide.infPercurso[i01].UFPer := Leitor.rCampo(tcStr, 'UFPer');
      inc(i01);
    end;
  end;

  (* Grupo da TAG <ide><toma03> ***********************************************)
  if Leitor.rExtrai(1, 'ide') <> '' then
  begin
    if (Leitor.rExtrai(2, 'toma03') <> '') or (Leitor.rExtrai(2, 'toma3') <> '') or
       (Leitor.rExtrai(2, 'toma') <> '') then
    begin
      (*B29*)CTe.Ide.Toma03.Toma := StrToTpTomador(ok, Leitor.rCampo(tcStr, 'toma'));
    end;
  end;

  (* Grupo da TAG <ide><toma4> ************************************************)
  if Leitor.rExtrai(1, 'ide') <> '' then
  begin
    if (Leitor.rExtrai(2, 'toma4') <> '') or (Leitor.rExtrai(2, 'tomaTerceiro') <> '') then
    begin
      (*B29*)CTe.Ide.Toma4.toma    := StrToTpTomador(ok, Leitor.rCampo(tcStr, 'toma'));
             CTe.Ide.Toma03.Toma   := CTe.Ide.Toma4.toma;
      (*B31*)CTe.Ide.Toma4.CNPJCPF := Leitor.rCampoCNPJCPF;
      (*B33*)CTe.Ide.Toma4.IE      := Leitor.rCampo(tcStr, 'IE');
      (*B34*)CTe.Ide.Toma4.xNome   := Leitor.rCampo(tcStr, 'xNome');
      (*B35*)CTe.Ide.Toma4.xFant   := Leitor.rCampo(tcStr, 'xFant');
      (*#44*)CTe.Ide.Toma4.fone    := Leitor.rCampo(tcStr, 'fone');
      (*#56*)CTe.Ide.Toma4.email   := Leitor.rCampo(tcStr, 'email');

       if Leitor.rExtrai(3, 'enderToma') <> '' then
       begin
         (*B37*)CTe.Ide.Toma4.EnderToma.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
         (*B37*)CTe.Ide.Toma4.EnderToma.nro     := Leitor.rCampo(tcStr, 'nro');
         (*B37*)CTe.Ide.Toma4.EnderToma.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
         (*B37*)CTe.Ide.Toma4.EnderToma.xBairro := Leitor.rCampo(tcStr, 'xBairro');
         (*B37*)CTe.Ide.Toma4.EnderToma.cMun    := Leitor.rCampo(tcInt, 'cMun');
         (*B37*)CTe.Ide.Toma4.EnderToma.xMun    := Leitor.rCampo(tcStr, 'xMun');
         (*B37*)CTe.Ide.Toma4.EnderToma.CEP     := Leitor.rCampo(tcInt, 'CEP');
         (*B37*)CTe.Ide.Toma4.EnderToma.UF      := Leitor.rCampo(tcStr, 'UF');
         (*B37*)CTe.Ide.Toma4.EnderToma.cPais   := Leitor.rCampo(tcInt, 'cPais');
         (*B37*)CTe.Ide.Toma4.EnderToma.xPais   := Leitor.rCampo(tcStr, 'xPais');
       end;
    end;
  end;

  (* Grupo da TAG <compl> *****************************************************)
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

      i01 := 0;
      CTe.Compl.fluxo.pass.Clear;
      while Leitor.rExtrai(3, 'pass', '', i01 + 1) <> '' do
      begin
        CTe.Compl.fluxo.pass.New;
        CTe.Compl.fluxo.pass[i01].xPass := Leitor.rCampo(tcStr, 'xPass');
        inc(i01);
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

    i01 := 0;
    CTe.Compl.ObsCont.Clear;
    while Leitor.rExtrai(2, 'ObsCont', '', i01 + 1) <> '' do
    begin
      CTe.Compl.obsCont.New;
      CTe.Compl.obsCont[i01].xCampo := Leitor.rAtributo('xCampo');
      CTe.Compl.obsCont[i01].xTexto := Leitor.rCampo(tcStr, 'xTexto');
      inc(i01);
    end;

    i01 := 0;
    CTe.Compl.ObsFisco.Clear;
    while Leitor.rExtrai(2, 'ObsFisco', '', i01 + 1) <> '' do
    begin
      CTe.Compl.obsCont.New;
      CTe.Compl.obsCont[i01].xCampo := Leitor.rAtributo('xCampo');
      CTe.Compl.obsCont[i01].xTexto := Leitor.rCampo(tcStr, 'xTexto');
      inc(i01);
    end;
  end;

  (* Grupo da TAG <emit> ******************************************************)
  if Leitor.rExtrai(1, 'emit') <> '' then
  begin
    CTe.emit.CNPJ  := Leitor.rCampo(tcStr,'CNPJ');
    CTe.emit.IE    := Leitor.rCampo(tcStr, 'IE');
    CTe.emit.IEST  := Leitor.rCampo(tcStr, 'IEST');
    CTe.emit.xNome := Leitor.rCampo(tcStr, 'xNome');
    CTe.emit.xFant := Leitor.rCampo(tcStr, 'xFant');
    CTe.emit.CRT   := StrToCRTCTe(ok, Leitor.rCampo(tcStr, 'CRT'));

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

  (* Grupo da TAG <rem> *******************************************************)
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

    i01 := 0;
    CTe.infCTeNorm.infDoc.InfNFE.Clear;

    if VersaoDF < ve200 then
    begin
      while Leitor.rExtrai(2, 'infNFe', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.infDoc.InfNFE.New;
        CTe.infCTeNorm.infDoc.InfNFE[i01].chave := Leitor.rCampo(tcStr, 'chave');
        CTe.infCTeNorm.infDoc.InfNFE[i01].PIN   := Leitor.rCampo(tcStr, 'PIN');
        inc(i01);
      end;

      CTe.infCTeNorm.infDoc.infNF.Clear;
      while Leitor.rExtrai(2, 'infNF', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.infDoc.infNF.New;
        CTe.infCTeNorm.infDoc.InfNF[i01].nRoma  := Leitor.rCampo(tcStr, 'nRoma');
        CTe.infCTeNorm.infDoc.InfNF[i01].nPed   := Leitor.rCampo(tcStr, 'nPed');
        CTe.infCTeNorm.infDoc.InfNF[i01].Modelo := StrToModeloNF(Ok, Leitor.rCampo(tcStr, 'mod'));
        CTe.infCTeNorm.infDoc.InfNF[i01].serie  := Leitor.rCampo(tcStr, 'serie');
        CTe.infCTeNorm.infDoc.InfNF[i01].nDoc   := Leitor.rCampo(tcEsp, 'nDoc');
        CTe.infCTeNorm.infDoc.InfNF[i01].dEmi   := Leitor.rCampo(tcDat, 'dEmi');
        CTe.infCTeNorm.infDoc.InfNF[i01].vBC    := Leitor.rCampo(tcDe2, 'vBC');
        CTe.infCTeNorm.infDoc.InfNF[i01].vICMS  := Leitor.rCampo(tcDe2, 'vICMS');
        CTe.infCTeNorm.infDoc.InfNF[i01].vBCST  := Leitor.rCampo(tcDe2, 'vBCST');
        CTe.infCTeNorm.infDoc.InfNF[i01].vST    := Leitor.rCampo(tcDe2, 'vST');
        CTe.infCTeNorm.infDoc.InfNF[i01].vProd  := Leitor.rCampo(tcDe2, 'vProd');
        CTe.infCTeNorm.infDoc.InfNF[i01].vNF    := Leitor.rCampo(tcDe2, 'vNF');
        CTe.infCTeNorm.infDoc.InfNF[i01].nCFOP  := Leitor.rCampo(tcInt, 'nCFOP');
        CTe.infCTeNorm.infDoc.InfNF[i01].nPeso  := Leitor.rCampo(tcDe3, 'nPeso');
        CTe.infCTeNorm.infDoc.InfNF[i01].PIN    := Leitor.rCampo(tcStr, 'PIN');
        inc(i01);
      end;

      CTe.infCTeNorm.infDoc.InfOutros.Clear;
      while Leitor.rExtrai(2, 'infOutros', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.infDoc.InfOutros.New;
        CTe.infCTeNorm.infDoc.InfOutros[i01].tpDoc      := StrToTpDocumento(ok, Leitor.rCampo(tcStr, 'tpDoc'));
        CTe.infCTeNorm.infDoc.InfOutros[i01].descOutros := Leitor.rCampo(tcStr, 'descOutros');
        CTe.infCTeNorm.infDoc.InfOutros[i01].nDoc       := Leitor.rCampo(tcStr, 'nDoc');
        CTe.infCTeNorm.infDoc.InfOutros[i01].dEmi       := Leitor.rCampo(tcDat, 'dEmi');
        CTe.infCTeNorm.infDoc.InfOutros[i01].vDocFisc   := Leitor.rCampo(tcDe2, 'vDocFisc');
        inc(i01);
      end;
    end;
  end;

  (* Grupo da TAG <exped> *****************************************************)
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

  (* Grupo da TAG <receb> *****************************************************)
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

  (* Grupo da TAG <dest> ******************************************************)
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

  if Leitor.rExtrai(1, 'detGTV') <> '' then
  begin
    CTe.detGTV.qCarga := Leitor.rCampo(tcDe4, 'qCarga');

    i01 := 0;
    CTe.detGTV.infEspecie.Clear;
    while Leitor.rExtrai(2, 'infEspecie', '', i01 + 1) <> '' do
    begin
      CTe.detGTV.infEspecie.New;
      CTe.detGTV.infEspecie[i01].tpEspecie   := StrToTEspecie(ok, Leitor.rCampo(tcStr, 'tpEspecie'));
      CTe.detGTV.infEspecie[i01].vEspecie    := Leitor.rCampo(tcDe2, 'vEspecie');
      CTe.detGTV.infEspecie[i01].tpNumerario := StrTotpNumerario(ok, Leitor.rCampo(tcStr, 'tpNumerario'));
      CTe.detGTV.infEspecie[i01].xMoedaEstr  := Leitor.rCampo(tcStr, 'xMoedaEstr');

      inc(i01);
    end;

    i01 := 0;
    CTe.detGTV.infVeiculo.Clear;
    while Leitor.rExtrai(2, 'infVeiculo', '', i01 + 1) <> '' do
    begin
      CTe.detGTV.infVeiculo.New;
      CTe.detGTV.infVeiculo[i01].placa := Leitor.rCampo(tcStr, 'placa');
      CTe.detGTV.infVeiculo[i01].UF    := Leitor.rCampo(tcStr, 'UF');
      CTe.detGTV.infVeiculo[i01].RNTRC := Leitor.rCampo(tcStr, 'RNTRC');

      inc(i01);
    end;
  end;

  (* Grupo da TAG <vPrest> ****************************************************)
  if Leitor.rExtrai(1, 'vPrest') <> '' then
  begin
    CTe.vPrest.vTPrest := Leitor.rCampo(tcDe2,'vTPrest');
    CTe.vPrest.vRec    := Leitor.rCampo(tcDe2,'vRec');

    i01 := 0;
    CTe.vPrest.Comp.Clear;
    while Leitor.rExtrai(2, 'Comp', '', i01 + 1) <> '' do
    begin
      CTe.vPrest.Comp.New;
      CTe.vPrest.Comp[i01].xNome := Leitor.rCampo(tcStr, 'xNome');
      CTe.vPrest.Comp[i01].vComp := Leitor.rCampo(tcDe2, 'vComp');
      inc(i01);
    end;
  end;

  (* Grupo da TAG <imp> *******************************************************)
  if Leitor.rExtrai(1, 'imp') <> '' then
  begin
    CTe.Imp.vTotTrib   := Leitor.rCampo(tcDe2,'vTotTrib');
    CTe.Imp.infAdFisco := Leitor.rCampo(tcStr,'infAdFisco');

    if Leitor.rExtrai(2, 'ICMS') <> '' then
    begin
      if Leitor.rExtrai(3, 'ICMS00') <> '' then
      begin
        if Leitor.rCampo(tcStr,'CST')='00'
        then begin
          CTe.Imp.ICMS.SituTrib     := cst00;
          CTe.Imp.ICMS.ICMS00.CST   := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));
          CTe.Imp.ICMS.ICMS00.vBC   := Leitor.rCampo(tcDe2,'vBC');
          CTe.Imp.ICMS.ICMS00.pICMS := Leitor.rCampo(tcDe2,'pICMS');
          CTe.Imp.ICMS.ICMS00.vICMS := Leitor.rCampo(tcDe2,'vICMS');
        end;
      end;

      if Leitor.rExtrai(3, 'ICMS20') <> '' then
      begin
        if Leitor.rCampo(tcStr,'CST')='20'
        then begin
          CTe.Imp.ICMS.SituTrib      := cst20;
          CTe.Imp.ICMS.ICMS20.CST    := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));
          CTe.Imp.ICMS.ICMS20.pRedBC := Leitor.rCampo(tcDe2,'pRedBC');
          CTe.Imp.ICMS.ICMS20.vBC    := Leitor.rCampo(tcDe2,'vBC');
          CTe.Imp.ICMS.ICMS20.pICMS  := Leitor.rCampo(tcDe2,'pICMS');
          CTe.Imp.ICMS.ICMS20.vICMS  := Leitor.rCampo(tcDe2,'vICMS');
        end;
      end;

      if Leitor.rExtrai(3, 'ICMS45') <> '' then
      begin
        sCST:=Leitor.rCampo(tcStr,'CST');
        if (sCST='40') or (sCST='41') or (sCST='51')
        then begin
          if sCST='40' then CTe.Imp.ICMS.SituTrib  := cst40;
          if sCST='41' then CTe.Imp.ICMS.SituTrib  := cst41;
          if sCST='51' then CTe.Imp.ICMS.SituTrib  := cst51;
          CTe.Imp.ICMS.ICMS45.CST := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));
        end;
      end;

      if Leitor.rExtrai(3, 'ICMS60') <> '' then
      begin
        if Leitor.rCampo(tcStr,'CST')='60'
        then begin
          CTe.Imp.ICMS.SituTrib          := cst60;
          CTe.Imp.ICMS.ICMS60.CST        := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));
          CTe.Imp.ICMS.ICMS60.vBCSTRet   := Leitor.rCampo(tcDe2,'vBCSTRet');
          CTe.Imp.ICMS.ICMS60.vICMSSTRet := Leitor.rCampo(tcDe2,'vICMSSTRet');
          CTe.Imp.ICMS.ICMS60.pICMSSTRet := Leitor.rCampo(tcDe2,'pICMSSTRet');
          CTe.Imp.ICMS.ICMS60.vCred      := Leitor.rCampo(tcDe2,'vCred');
        end;
      end;

      if Leitor.rExtrai(3, 'ICMS90') <> '' then
      begin
        if Leitor.rCampo(tcStr,'CST')='90'
        then begin
          CTe.Imp.ICMS.SituTrib      := cst90;
          CTe.Imp.ICMS.ICMS90.CST    := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));
          CTe.Imp.ICMS.ICMS90.pRedBC := Leitor.rCampo(tcDe2,'pRedBC');
          CTe.Imp.ICMS.ICMS90.vBC    := Leitor.rCampo(tcDe2,'vBC');
          CTe.Imp.ICMS.ICMS90.pICMS  := Leitor.rCampo(tcDe2,'pICMS');
          CTe.Imp.ICMS.ICMS90.vICMS  := Leitor.rCampo(tcDe2,'vICMS');
          CTe.Imp.ICMS.ICMS90.vCred  := Leitor.rCampo(tcDe2,'vCred');
        end;
      end;

      if Leitor.rExtrai(3, 'ICMSOutraUF') <> '' then
      begin
        if Leitor.rCampo(tcStr,'CST')='90'
        then begin
          // ICMS devido à UF de origem da prestação, quando diferente da UF do emitente
          CTe.Imp.ICMS.SituTrib                  := cstICMSOutraUF;
          CTe.Imp.ICMS.ICMSOutraUF.CST           := StrToCSTICMS(ok, Leitor.rCampo(tcStr,'CST'));
          CTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF := Leitor.rCampo(tcDe2,'pRedBCOutraUF');
          CTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF    := Leitor.rCampo(tcDe2,'vBCOutraUF');
          CTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF  := Leitor.rCampo(tcDe2,'pICMSOutraUF');
          CTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF  := Leitor.rCampo(tcDe2,'vICMSOutraUF');
        end;
      end;

      if Leitor.rExtrai(3, 'ICMSSN') <> '' then
      begin
       // ICMS Simples Nacional
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

  (* Grupo da TAG <infCTeNorm> ************************************************)
  if Leitor.rExtrai(1, 'infCTeNorm') <> '' then
  begin
    CTe.infCTeNorm.refCTeCanc := Leitor.rCampo(tcStr,'refCTeCanc');

    if Leitor.rExtrai(2, 'infServico') <> '' then
    begin
      CTe.infCTeNorm.infServico.xDescServ := Leitor.rCampo(tcStr,'xDescServ');
      CTe.infCTeNorm.infServico.qCarga    := Leitor.rCampo(tcDe4,'qCarga');
    end;

    i01 := 0;
    CTe.infCTeNorm.infDocRef.Clear;
    while Leitor.rExtrai(2, 'infDocRef', '', i01 + 1) <> '' do
    begin
      CTe.infCTeNorm.infDocRef.New;
      CTe.infCTeNorm.infDocRef[i01].nDoc     := Leitor.rCampo(tcEsp, 'nDoc');
      CTe.infCTeNorm.infDocRef[i01].serie    := Leitor.rCampo(tcStr, 'serie');
      CTe.infCTeNorm.infDocRef[i01].subserie := Leitor.rCampo(tcStr, 'subserie');
      CTe.infCTeNorm.infDocRef[i01].dEmi     := Leitor.rCampo(tcDat, 'dEmi');
      CTe.infCTeNorm.infDocRef[i01].vDoc     := Leitor.rCampo(tcDe2, 'vDoc');

      CTe.infCTeNorm.infDocRef[i01].chBPe := Leitor.rCampo(tcStr, 'chBPe');

      inc(i01);
    end;

    if Leitor.rExtrai(2, 'infCarga') <> ''
    then begin
      CTe.infCTeNorm.infCarga.vCarga      := Leitor.rCampo(tcDe2,'vCarga');
      CTe.infCTeNorm.InfCarga.proPred     := Leitor.rCampo(tcStr,'proPred');
      CTe.infCTeNorm.InfCarga.xOutCat     := Leitor.rCampo(tcStr,'xOutCat');
      CTe.infCTeNorm.infCarga.vCargaAverb := Leitor.rCampo(tcDe2,'vCargaAverb');

      i01 := 0;
      CTe.infCTeNorm.InfCarga.infQ.Clear;
      while Leitor.rExtrai(3, 'infQ', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.InfCarga.infQ.New;
        CTe.infCTeNorm.InfCarga.infQ[i01].cUnid  := Leitor.rCampo(tcStr, 'cUnid');
        CTe.infCTeNorm.InfCarga.infQ[i01].tpMed  := Leitor.rCampo(tcStr, 'tpMed');
        CTe.infCTeNorm.InfCarga.infQ[i01].qCarga := Leitor.rCampo(tcDe4, 'qCarga');
        inc(i01);
      end;
    end;

    if Leitor.rExtrai(2, 'infDoc') <> ''
    then begin
      i01 := 0;
      CTe.infCTeNorm.infDoc.infNF.Clear;
      while Leitor.rExtrai(3, 'infNF', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.infDoc.infNF.New;
        CTe.infCTeNorm.infDoc.InfNF[i01].nRoma := Leitor.rCampo(tcStr, 'nRoma');
        CTe.infCTeNorm.infDoc.InfNF[i01].nPed  := Leitor.rCampo(tcStr, 'nPed');
        CTe.infCTeNorm.infDoc.InfNF[i01].Modelo := StrToModeloNF(Ok, Leitor.rCampo(tcStr, 'mod'));
        CTe.infCTeNorm.infDoc.InfNF[i01].serie := Leitor.rCampo(tcStr, 'serie');
        CTe.infCTeNorm.infDoc.InfNF[i01].nDoc  := Leitor.rCampo(tcEsp, 'nDoc');
        CTe.infCTeNorm.infDoc.InfNF[i01].dEmi  := Leitor.rCampo(tcDat, 'dEmi');
        CTe.infCTeNorm.infDoc.InfNF[i01].vBC   := Leitor.rCampo(tcDe2, 'vBC');
        CTe.infCTeNorm.infDoc.InfNF[i01].vICMS := Leitor.rCampo(tcDe2, 'vICMS');
        CTe.infCTeNorm.infDoc.InfNF[i01].vBCST := Leitor.rCampo(tcDe2, 'vBCST');
        CTe.infCTeNorm.infDoc.InfNF[i01].vST   := Leitor.rCampo(tcDe2, 'vST');
        CTe.infCTeNorm.infDoc.InfNF[i01].vProd := Leitor.rCampo(tcDe2, 'vProd');
        CTe.infCTeNorm.infDoc.InfNF[i01].vNF   := Leitor.rCampo(tcDe2, 'vNF');
        CTe.infCTeNorm.infDoc.InfNF[i01].nCFOP := Leitor.rCampo(tcInt, 'nCFOP');
        CTe.infCTeNorm.infDoc.InfNF[i01].nPeso := Leitor.rCampo(tcDe3, 'nPeso');
        CTe.infCTeNorm.infDoc.InfNF[i01].PIN   := Leitor.rCampo(tcStr, 'PIN');
        CTe.infCTeNorm.infDoc.InfNF[i01].dPrev := Leitor.rCampo(tcDat, 'dPrev');

        i02 := 0;
        while Leitor.rExtrai(4, 'infUnidTransp', '', i02 + 1) <> '' do
        begin
          CTe.infCTeNorm.infDoc.infNF[i01].infUnidTransp.New;
          CTe.infCTeNorm.infDoc.infNF[i01].infUnidTransp[i02].tpUnidTransp := StrToUnidTransp(ok, Leitor.rCampo(tcStr, 'tpUnidTransp'));
          CTe.infCTeNorm.infDoc.infNF[i01].infUnidTransp[i02].idUnidTransp := Leitor.rCampo(tcStr, 'idUnidTransp');

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

//          if (pos1 = 0) and (pos2 = 0) and (pos3 = 0) or (pos1 > pos3) then
//            qtdRat_UnidTransp := 0.0;

          if (pos1 < pos3) then
            qtdRat_UnidTransp := StringToFloatDef(Copy(sAux, pos2 + 8, len -8), 0)
          else
            qtdRat_UnidTransp := 0.0;

          CTe.infCTeNorm.infDoc.infNF[i01].infUnidTransp[i02].qtdRat := qtdRat_UnidTransp;

          i03 := 0;
          while Leitor.rExtrai(5, 'lacUnidTransp', '', i03 + 1) <> '' do
          begin
            CTe.infCTeNorm.infDoc.infNF[i01].infUnidTransp[i02].lacUnidTransp.New;
            CTe.infCTeNorm.infDoc.infNF[i01].infUnidTransp[i02].lacUnidTransp[i03].nLacre := Leitor.rCampo(tcStr, 'nLacre');
            inc(i03);
          end;

          i03 := 0;
          while Leitor.rExtrai(5, 'infUnidCarga', '', i03 + 1) <> '' do
          begin
            CTe.infCTeNorm.infDoc.infNF[i01].infUnidTransp[i02].infUnidCarga.New;
            CTe.infCTeNorm.infDoc.infNF[i01].infUnidTransp[i02].infUnidCarga[i03].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
            CTe.infCTeNorm.infDoc.infNF[i01].infUnidTransp[i02].infUnidCarga[i03].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
            CTe.infCTeNorm.infDoc.infNF[i01].infUnidTransp[i02].infUnidCarga[i03].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

            i04 := 0;
            while Leitor.rExtrai(6, 'lacUnidCarga', '', i04 + 1) <> '' do
            begin
              CTe.infCTeNorm.infDoc.infNF[i01].infUnidTransp[i02].infUnidCarga[i03].lacUnidCarga.New;
              CTe.infCTeNorm.infDoc.infNF[i01].infUnidTransp[i02].infUnidCarga[i03].lacUnidCarga[i04].nLacre := Leitor.rCampo(tcStr, 'nLacre');
              inc(i04);
            end;

            inc(i03);
          end;

          inc(i02);
        end;

        if i02 = 0 then
        begin
          while Leitor.rExtrai(4, 'infUnidCarga', '', i02 + 1) <> '' do
          begin
            CTe.infCTeNorm.infDoc.infNF[i01].infUnidCarga.New;
            CTe.infCTeNorm.infDoc.infNF[i01].infUnidCarga[i02].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
            CTe.infCTeNorm.infDoc.infNF[i01].infUnidCarga[i02].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
            CTe.infCTeNorm.infDoc.infNF[i01].infUnidCarga[i02].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

            i03 := 0;
            while Leitor.rExtrai(5, 'lacUnidCarga', '', i03 + 1) <> '' do
            begin
              CTe.infCTeNorm.infDoc.infNF[i01].infUnidCarga[i02].lacUnidCarga.New;
              CTe.infCTeNorm.infDoc.infNF[i01].infUnidCarga[i02].lacUnidCarga[i03].nLacre := Leitor.rCampo(tcStr, 'nLacre');
              inc(i03);
            end;

            inc(i02);
          end;
        end;
        
        inc(i01);
      end;

      i01 := 0;
      CTe.infCTeNorm.infDoc.InfNFE.Clear;
      while Leitor.rExtrai(3, 'infNFe', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.infDoc.InfNFE.New;
        CTe.infCTeNorm.infDoc.InfNFE[i01].chave := Leitor.rCampo(tcStr, 'chave');
        CTe.infCTeNorm.infDoc.InfNFE[i01].PIN   := Leitor.rCampo(tcStr, 'PIN');
        CTe.infCTeNorm.infDoc.InfNFE[i01].dPrev := Leitor.rCampo(tcDat, 'dPrev');

        i02 := 0;
        while Leitor.rExtrai(4, 'infUnidTransp', '', i02 + 1) <> '' do
        begin
          CTe.infCTeNorm.infDoc.InfNFE[i01].infUnidTransp.New;
          CTe.infCTeNorm.infDoc.InfNFE[i01].infUnidTransp[i02].tpUnidTransp := StrToUnidTransp(ok, Leitor.rCampo(tcStr, 'tpUnidTransp'));
          CTe.infCTeNorm.infDoc.InfNFE[i01].infUnidTransp[i02].idUnidTransp := Leitor.rCampo(tcStr, 'idUnidTransp');

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

//          if (pos1 = 0) and (pos2 = 0) and (pos3 = 0) or (pos1 > pos3) then
//            qtdRat_UnidTransp := 0.0;

          if (pos1 < pos3) then
            qtdRat_UnidTransp := StringToFloatDef(Copy(sAux, pos2 + 8, len -8), 0)
          else
            qtdRat_UnidTransp := 0.0;

          CTe.infCTeNorm.infDoc.infNFE[i01].infUnidTransp[i02].qtdRat := qtdRat_UnidTransp;

          i03 := 0;
          while Leitor.rExtrai(5, 'lacUnidTransp', '', i03 + 1) <> '' do
          begin
            CTe.infCTeNorm.infDoc.InfNFE[i01].infUnidTransp[i02].lacUnidTransp.New;
            CTe.infCTeNorm.infDoc.InfNFE[i01].infUnidTransp[i02].lacUnidTransp[i03].nLacre := Leitor.rCampo(tcStr, 'nLacre');
            inc(i03);
          end;

          i03 := 0;
          while Leitor.rExtrai(5, 'infUnidCarga', '', i03 + 1) <> '' do
          begin
            CTe.infCTeNorm.infDoc.InfNFE[i01].infUnidTransp[i02].infUnidCarga.New;
            CTe.infCTeNorm.infDoc.InfNFE[i01].infUnidTransp[i02].infUnidCarga[i03].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
            CTe.infCTeNorm.infDoc.InfNFE[i01].infUnidTransp[i02].infUnidCarga[i03].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
            CTe.infCTeNorm.infDoc.InfNFE[i01].infUnidTransp[i02].infUnidCarga[i03].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

            i04 := 0;
            while Leitor.rExtrai(6, 'lacUnidCarga', '', i04 + 1) <> '' do
            begin
              CTe.infCTeNorm.infDoc.InfNFE[i01].infUnidTransp[i02].infUnidCarga[i03].lacUnidCarga.New;
              CTe.infCTeNorm.infDoc.InfNFE[i01].infUnidTransp[i02].infUnidCarga[i03].lacUnidCarga[i04].nLacre := Leitor.rCampo(tcStr, 'nLacre');
              inc(i04);
            end;

            inc(i03);
          end;

          inc(i02);
        end;

        if i02 = 0 then
        begin
          while Leitor.rExtrai(4, 'infUnidCarga', '', i02 + 1) <> '' do
          begin
            CTe.infCTeNorm.infDoc.infNFE[i01].infUnidCarga.New;
            CTe.infCTeNorm.infDoc.infNFE[i01].infUnidCarga[i02].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
            CTe.infCTeNorm.infDoc.infNFE[i01].infUnidCarga[i02].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
            CTe.infCTeNorm.infDoc.infNFE[i01].infUnidCarga[i02].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

            i03 := 0;
            while Leitor.rExtrai(5, 'lacUnidCarga', '', i03 + 1) <> '' do
            begin
              CTe.infCTeNorm.infDoc.infNFE[i01].infUnidCarga[i02].lacUnidCarga.New;
              CTe.infCTeNorm.infDoc.infNFE[i01].infUnidCarga[i02].lacUnidCarga[i03].nLacre := Leitor.rCampo(tcStr, 'nLacre');
              inc(i03);
            end;

            inc(i02);
          end;
        end;

        inc(i01);
      end;

      i01 := 0;
      CTe.infCTeNorm.infDoc.InfOutros.Clear;
      while Leitor.rExtrai(3, 'infOutros', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.infDoc.InfOutros.New;
        CTe.infCTeNorm.infDoc.InfOutros[i01].tpDoc      := StrToTpDocumento(ok, Leitor.rCampo(tcStr, 'tpDoc'));
        CTe.infCTeNorm.infDoc.InfOutros[i01].descOutros := Leitor.rCampo(tcStr, 'descOutros');
        CTe.infCTeNorm.infDoc.InfOutros[i01].nDoc       := Leitor.rCampo(tcStr, 'nDoc');
        CTe.infCTeNorm.infDoc.InfOutros[i01].dEmi       := Leitor.rCampo(tcDat, 'dEmi');
        CTe.infCTeNorm.infDoc.InfOutros[i01].vDocFisc   := Leitor.rCampo(tcDe2, 'vDocFisc');
        CTe.infCTeNorm.infDoc.InfOutros[i01].dPrev      := Leitor.rCampo(tcDat, 'dPrev');

        i02 := 0;
        while Leitor.rExtrai(4, 'infUnidTransp', '', i02 + 1) <> '' do
        begin
          CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidTransp.New;
          CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidTransp[i02].tpUnidTransp := StrToUnidTransp(ok, Leitor.rCampo(tcStr, 'tpUnidTransp'));
          CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidTransp[i02].idUnidTransp := Leitor.rCampo(tcStr, 'idUnidTransp');

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

//          if (pos1 = 0) and (pos2 = 0) and (pos3 = 0) or (pos1 > pos3) then
//            qtdRat_UnidTransp := 0.0;

          if (pos1 < pos3) then
            qtdRat_UnidTransp := StringToFloatDef(Copy(sAux, pos2 + 8, pos3 -8), 0)
          else
            qtdRat_UnidTransp := 0.0;

          CTe.infCTeNorm.infDoc.infOutros[i01].infUnidTransp[i02].qtdRat := qtdRat_UnidTransp;

          i03 := 0;
          while Leitor.rExtrai(5, 'lacUnidTransp', '', i03 + 1) <> '' do
          begin
            CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidTransp[i02].lacUnidTransp.New;
            CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidTransp[i02].lacUnidTransp[i03].nLacre := Leitor.rCampo(tcStr, 'nLacre');
            inc(i03);
          end;

          i03 := 0;
          while Leitor.rExtrai(5, 'infUnidCarga', '', i03 + 1) <> '' do
          begin
            CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidTransp[i02].infUnidCarga.New;
            CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidTransp[i02].infUnidCarga[i03].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
            CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidTransp[i02].infUnidCarga[i03].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
            CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidTransp[i02].infUnidCarga[i03].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

            i04 := 0;
            while Leitor.rExtrai(6, 'lacUnidCarga', '', i04 + 1) <> '' do
            begin
              CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidTransp[i02].infUnidCarga[i03].lacUnidCarga.New;
              CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidTransp[i02].infUnidCarga[i03].lacUnidCarga[i04].nLacre := Leitor.rCampo(tcStr, 'nLacre');
              inc(i04);
            end;

            inc(i03);
          end;

          inc(i02);
        end;

        if i02 = 0 then
        begin
          while Leitor.rExtrai(4, 'infUnidCarga', '', i02 + 1) <> '' do
          begin
            CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidCarga.New;
            CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidCarga[i02].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
            CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidCarga[i02].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
            CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidCarga[i02].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

            i03 := 0;
            while Leitor.rExtrai(5, 'lacUnidCarga', '', i03 + 1) <> '' do
            begin
              CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidCarga[i02].lacUnidCarga.New;
              CTe.infCTeNorm.infDoc.InfOutros[i01].infUnidCarga[i02].lacUnidCarga[i03].nLacre := Leitor.rCampo(tcStr, 'nLacre');
              inc(i03);
            end;

            inc(i02);
          end;
        end;

        inc(i01);
      end;
    end;

    if Leitor.rExtrai(2, 'docAnt') <> ''
    then begin
      i01 := 0;
      CTe.infCTeNorm.docAnt.emiDocAnt.Clear;
      while Leitor.rExtrai(3, 'emiDocAnt', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.docAnt.emiDocAnt.New;
        CTe.infCTeNorm.docAnt.emiDocAnt[i01].CNPJCPF := Leitor.rCampoCNPJCPF;
        CTe.infCTeNorm.docAnt.emiDocAnt[i01].IE      := Leitor.rCampo(tcStr, 'IE');
        CTe.infCTeNorm.docAnt.emiDocAnt[i01].UF      := Leitor.rCampo(tcStr, 'UF');
        CTe.infCTeNorm.docAnt.emiDocAnt[i01].xNome   := Leitor.rCampo(tcStr, 'xNome');

        i02 := 0;
        while Leitor.rExtrai(4, 'idDocAnt', '', i02 + 1) <> '' do
        begin
          CTe.infCTeNorm.docAnt.emiDocAnt[i01].idDocAnt.New;

          i03 := 0;
          while Leitor.rExtrai(5, 'idDocAntPap', '', i03 + 1) <> '' do
          begin
            CTe.infCTeNorm.docAnt.emiDocAnt[i01].idDocAnt[i02].idDocAntPap.New;
            CTe.infCTeNorm.docAnt.emiDocAnt[i01].idDocAnt[i02].idDocAntPap[i03].tpDoc  := StrToTpDocumentoAnterior(ok, Leitor.rCampo(tcStr, 'tpDoc'));
            CTe.infCTeNorm.docAnt.emiDocAnt[i01].idDocAnt[i02].idDocAntPap[i03].serie  := Leitor.rCampo(tcStr, 'serie');
            CTe.infCTeNorm.docAnt.emiDocAnt[i01].idDocAnt[i02].idDocAntPap[i03].subser := Leitor.rCampo(tcStr, 'subser');
            CTe.infCTeNorm.docAnt.emiDocAnt[i01].idDocAnt[i02].idDocAntPap[i03].nDoc   := Leitor.rCampo(tcStr, 'nDoc');
            CTe.infCTeNorm.docAnt.emiDocAnt[i01].idDocAnt[i02].idDocAntPap[i03].dEmi   := Leitor.rCampo(tcDat, 'dEmi');
            inc(i03);
          end;

          i03 := 0;
          while Leitor.rExtrai(5, 'idDocAntEle', '', i03 + 1) <> '' do
          begin
            CTe.infCTeNorm.docAnt.emiDocAnt[i01].idDocAnt[i02].idDocAntEle.New;

            if (VersaoDF >= ve300) then
              CTe.infCTeNorm.docAnt.emiDocAnt[i01].idDocAnt[i02].idDocAntEle[i03].chCTe := Leitor.rCampo(tcStr, 'chCTe')
            else
              CTe.infCTeNorm.docAnt.emiDocAnt[i01].idDocAnt[i02].idDocAntEle[i03].chave := Leitor.rCampo(tcStr, 'chave');

            inc(i03);
          end;
          inc(i02);
        end;
        inc(i01);
      end;
    end;

    i01 := 0;
    CTe.infCTeNorm.seg.Clear;
    while Leitor.rExtrai(2, 'seg', '', i01 + 1) <> '' do
    begin
      CTe.infCTeNorm.seg.New;
      CTe.infCTeNorm.seg[i01].respSeg := StrToTpRspSeguro(ok, Leitor.rCampo(tcStr, 'respSeg'));
      CTe.infCTeNorm.seg[i01].xSeg    := Leitor.rCampo(tcStr, 'xSeg');
      CTe.infCTeNorm.seg[i01].nApol   := Leitor.rCampo(tcStr, 'nApol');
      CTe.infCTeNorm.seg[i01].nAver   := Leitor.rCampo(tcStr, 'nAver');
      CTe.infCTeNorm.seg[i01].vCarga  := Leitor.rCampo(tcDe2, 'vCarga');
      inc(i01);
    end;

    if Leitor.rExtrai(2, 'rodo') <> '' then
    begin
      CTe.infCTeNorm.rodo.RNTRC := Leitor.rCampo(tcStr,'RNTRC');
      CTe.infCTeNorm.rodo.dPrev := Leitor.rCampo(tcDat,'dPrev');
      CTe.infCTeNorm.rodo.lota  := StrToTpLotacao(ok, Leitor.rCampo(tcStr,'lota'));
      CTe.infCTeNorm.rodo.CIOT  := Leitor.rCampo(tcStr, 'CIOT');

      if VersaoDF < ve200 then
      begin
        for i01 := 0 to CTe.infCTeNorm.infDoc.InfNFE.count-1 do
        begin
          CTe.infCTeNorm.infDoc.InfNFE[i01].dPrev := CTe.infCTeNorm.rodo.dPrev;
        end;
        for i01 := 0 to CTe.infCTeNorm.infDoc.InfNF.count-1 do
        begin
          CTe.infCTeNorm.infDoc.InfNF[i01].dPrev := CTe.infCTeNorm.rodo.dPrev;
        end;
        for i01 := 0 to CTe.infCTeNorm.infDoc.InfOutros.count-1 do
        begin
          CTe.infCTeNorm.infDoc.InfOutros[i01].dPrev := CTe.infCTeNorm.rodo.dPrev;
        end;
      end;

      i01 := 0;
      CTe.infCTeNorm.rodo.occ.Clear;
      while Leitor.rExtrai(3, 'occ', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.rodo.occ.New;
        CTe.infCTeNorm.rodo.occ[i01].serie := Leitor.rCampo(tcStr, 'serie');
        CTe.infCTeNorm.rodo.occ[i01].nOcc  := Leitor.rCampo(tcInt, 'nOcc');
        CTe.infCTeNorm.rodo.occ[i01].dEmi  := Leitor.rCampo(tcDat, 'dEmi');

        if Leitor.rExtrai(4, 'emiOcc') <> '' then
        begin
          CTe.infCTeNorm.rodo.occ[i01].emiOcc.CNPJ := Leitor.rCampo(tcStr, 'CNPJ');
          CTe.infCTeNorm.rodo.occ[i01].emiOcc.cInt := Leitor.rCampo(tcStr, 'cInt');
          CTe.infCTeNorm.rodo.occ[i01].emiOcc.IE   := Leitor.rCampo(tcStr, 'IE');
          CTe.infCTeNorm.rodo.occ[i01].emiOcc.UF   := Leitor.rCampo(tcStr, 'UF');
          CTe.infCTeNorm.rodo.occ[i01].emiOcc.fone := Leitor.rCampo(tcStr, 'fone');
        end;
        inc(i01);
      end;

      i01 := 0;
      CTe.infCTeNorm.rodo.valePed.Clear;
      while Leitor.rExtrai(3, 'valePed', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.rodo.valePed.New;
        CTe.infCTeNorm.rodo.valePed[i01].CNPJForn := Leitor.rCampo(tcStr, 'CNPJForn');
        CTe.infCTeNorm.rodo.valePed[i01].nCompra  := Leitor.rCampo(tcStr, 'nCompra');
        CTe.infCTeNorm.rodo.valePed[i01].CNPJPg   := Leitor.rCampo(tcStr, 'CNPJPg');
        CTe.infCTeNorm.rodo.valePed[i01].vValePed := Leitor.rCampo(tcDe2, 'vValePed');
        inc(i01);
      end;

      i01 := 0;
      CTe.infCTeNorm.rodo.veic.Clear;
      while Leitor.rExtrai(3, 'veic', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.rodo.veic.New;
        CTe.infCTeNorm.rodo.veic[i01].cInt    := Leitor.rCampo(tcStr, 'cInt');
        CTe.infCTeNorm.rodo.veic[i01].RENAVAM := Leitor.rCampo(tcStr, 'RENAVAM');
        CTe.infCTeNorm.rodo.veic[i01].placa   := Leitor.rCampo(tcStr, 'placa');
        CTe.infCTeNorm.rodo.veic[i01].tara    := Leitor.rCampo(tcInt, 'tara');
        CTe.infCTeNorm.rodo.veic[i01].capKG   := Leitor.rCampo(tcInt, 'capKG');
        CTe.infCTeNorm.rodo.veic[i01].capM3   := Leitor.rCampo(tcInt, 'capM3');
        CTe.infCTeNorm.rodo.veic[i01].tpProp  := StrToTpPropriedade(ok, Leitor.rCampo(tcStr, 'tpProp'));
        CTe.infCTeNorm.rodo.veic[i01].tpVeic  := StrToTpVeiculo(ok, Leitor.rCampo(tcStr, 'tpVeic'));
        CTe.infCTeNorm.rodo.veic[i01].tpRod   := StrToTpRodado(ok, Leitor.rCampo(tcStr, 'tpRod'));
        CTe.infCTeNorm.rodo.veic[i01].tpCar   := StrToTpCarroceria(ok, Leitor.rCampo(tcStr, 'tpCar'));
        CTe.infCTeNorm.rodo.veic[i01].UF      := Leitor.rCampo(tcStr, 'UF');

        if Leitor.rExtrai(4, 'prop') <> '' then
        begin
          CTe.infCTeNorm.rodo.veic[i01].prop.CNPJCPF := Leitor.rCampoCNPJCPF;
          CTe.infCTeNorm.rodo.veic[i01].prop.RNTRC   := Leitor.rCampo(tcStr, 'RNTRC');
          CTe.infCTeNorm.rodo.veic[i01].prop.xNome   := Leitor.rCampo(tcStr, 'xNome');
          CTe.infCTeNorm.rodo.veic[i01].prop.IE      := Leitor.rCampo(tcStr, 'IE');
          CTe.infCTeNorm.rodo.veic[i01].prop.UF      := Leitor.rCampo(tcStr, 'UF');
          CTe.infCTeNorm.rodo.veic[i01].prop.tpProp  := StrToTpProp(ok, Leitor.rCampo(tcStr, 'tpProp'));
        end;
        inc(i01);
      end;

      i01 := 0;
      CTe.infCTeNorm.rodo.lacRodo.Clear;
      while Leitor.rExtrai(3, 'lacRodo', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.rodo.lacRodo.New;
        CTe.infCTeNorm.rodo.lacRodo[i01].nLacre := Leitor.rCampo(tcStr, 'nLacre');
        inc(i01);
      end;

      i01 := 0;
      CTe.infCTeNorm.Rodo.moto.Clear;
      while Leitor.rExtrai(3, 'moto', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.Rodo.moto.New;
        CTe.infCTeNorm.Rodo.moto[i01].xNome := Leitor.rCampo(tcStr, 'xNome');
        CTe.infCTeNorm.Rodo.moto[i01].CPF   := Leitor.rCampo(tcStr, 'CPF');
        inc(i01);
      end;

    end; // fim das informações do modal Rodoviário

    if Leitor.rExtrai(2, 'rodoOS') <> '' then
    begin
      CTe.infCTeNorm.rodoOS.TAF            := Leitor.rCampo(tcStr, 'TAF');
      CTe.infCTeNorm.rodoOS.NroRegEstadual := Leitor.rCampo(tcStr, 'NroRegEstadual');

      if Leitor.rExtrai(3, 'veic') <> '' then
      begin
        CTe.infCTeNorm.rodoOS.veic.placa   := Leitor.rCampo(tcStr, 'placa');
        CTe.infCTeNorm.rodoOS.veic.RENAVAM := Leitor.rCampo(tcStr, 'RENAVAM');
        CTe.infCTeNorm.rodoOS.veic.UF      := Leitor.rCampo(tcStr, 'UF');

        if Leitor.rExtrai(4, 'prop') <> '' then
        begin
          CTe.infCTeNorm.rodoOS.veic.prop.CNPJCPF        := Leitor.rCampoCNPJCPF;
          CTe.infCTeNorm.rodoOS.veic.prop.TAF            := Leitor.rCampo(tcStr, 'TAF');
          CTe.infCTeNorm.rodoOS.veic.prop.NroRegEstadual := Leitor.rCampo(tcStr, 'NroRegEstadual');
          CTe.infCTeNorm.rodoOS.veic.prop.xNome          := Leitor.rCampo(tcStr, 'xNome');
          CTe.infCTeNorm.rodoOS.veic.prop.IE             := Leitor.rCampo(tcStr, 'IE');
          CTe.infCTeNorm.rodoOS.veic.prop.UF             := Leitor.rCampo(tcStr, 'UF');
          CTe.infCTeNorm.rodoOS.veic.prop.tpProp         := StrToTpProp(ok, Leitor.rCampo(tcStr, 'tpProp'));
        end;
      end;

      if Leitor.rExtrai(3, 'infFretamento') <> '' then
      begin
        CTe.infCTeNorm.rodoOS.infFretamento.tpFretamento := StrToTpFretamento(ok, Leitor.rCampo(tcStr, 'tpFretamento'));
        CTe.infCTeNorm.rodoOS.infFretamento.dhViagem     := Leitor.rCampo(tcDatHor,'dhViagem');
      end;
    end;

    if Leitor.rExtrai(2, 'aereo') <> '' then
    begin
      CTe.infCTeNorm.aereo.nMinu      := Leitor.rCampo(tcInt,'nMinu');
      CTe.infCTeNorm.aereo.nOCA       := Leitor.rCampo(tcStr,'nOCA');
      CTe.infCTeNorm.aereo.dPrevAereo := Leitor.rCampo(tcDat,'dPrevAereo');
      CTe.infCTeNorm.aereo.xLAgEmi    := Leitor.rCampo(tcStr,'xLAgEmi');
      CTe.infCTeNorm.aereo.IdT        := Leitor.rCampo(tcStr,'IdT');

      if Leitor.rExtrai(3, 'tarifa') <> '' then
      begin
        CTe.infCTeNorm.aereo.tarifa.CL     := Leitor.rCampo(tcStr,'CL');
        CTe.infCTeNorm.aereo.tarifa.cTar   := Leitor.rCampo(tcStr,'cTar');
        CTe.infCTeNorm.aereo.tarifa.vTar   := Leitor.rCampo(tcDe2,'vTar');
      end;

      if Leitor.rExtrai(3, 'natCarga') <> '' then
       begin
         CTe.infCTeNorm.aereo.natCarga.xDime     := Leitor.rCampo(tcStr,'xDime');
         CTe.infCTeNorm.aereo.natCarga.cIMP      := Leitor.rCampo(tcStr,'cIMP');

         i01 := 0;
         CTe.infCTeNorm.aereo.natCarga.cinfManu.Clear;
         while Leitor.rExtrai(4, 'cInfManu', '', i01 + 1) <> '' do
         begin
           CTe.infCTeNorm.aereo.natCarga.cinfManu.New;

           if VersaoDF >= ve300 then
             CTe.infCTeNorm.aereo.natCarga.cinfManu[i01].nInfManu := StrToTpInfManu(ok, Leitor.rCampo(tcStr,'cInfManu'))
           else
             CTe.infCTeNorm.aereo.natCarga.cinfManu[i01].nInfManu := StrToTpInfManuV2(ok, Leitor.rCampo(tcStr,'cInfManu'));

           inc(i01);
         end;
       end;
    end; // fim das informações do modal Aéreo

    if Leitor.rExtrai(2, 'aquav') <> '' then
    begin
      CTe.infCTeNorm.aquav.vPrest   := Leitor.rCampo(tcDe2,'vPrest');
      CTe.infCTeNorm.aquav.vAFRMM   := Leitor.rCampo(tcDe2,'vAFRMM');
      CTe.infCTeNorm.aquav.nBooking := Leitor.rCampo(tcStr,'nBooking');
      CTe.infCTeNorm.aquav.nCtrl    := Leitor.rCampo(tcStr,'nCtrl');
      CTe.infCTeNorm.aquav.xNavio   := Leitor.rCampo(tcStr,'xNavio');
      CTe.infCTeNorm.aquav.nViag    := Leitor.rCampo(tcStr,'nViag');
      CTe.infCTeNorm.aquav.direc    := StrToTpDirecao(ok, Leitor.rCampo(tcStr, 'direc'));
      CTe.infCTeNorm.aquav.prtEmb   := Leitor.rCampo(tcStr,'prtEmb');
      CTe.infCTeNorm.aquav.prtTrans := Leitor.rCampo(tcStr,'prtTrans');
      CTe.infCTeNorm.aquav.prtDest  := Leitor.rCampo(tcStr,'prtDest');
      CTe.infCTeNorm.aquav.tpNav    := StrToTpNavegacao(ok, Leitor.rCampo(tcStr, 'tpNav'));
      CTe.infCTeNorm.aquav.irin     := Leitor.rCampo(tcStr,'irin');

      i01 := 0;
      CTe.infCTeNorm.aquav.balsa.Clear;
      while Leitor.rExtrai(3, 'balsa', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.aquav.balsa.New;
        CTe.infCTeNorm.aquav.balsa[i01].xBalsa := Leitor.rCampo(tcStr, 'xBalsa');
        inc(i01);
      end;

      i01 := 0;
      CTe.infCTeNorm.aquav.detCont.Clear;
      while Leitor.rExtrai(3, 'detCont', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.aquav.detCont.New;
        CTe.infCTeNorm.aquav.detCont[i01].nCont := Leitor.rCampo(tcStr, 'nCont');

        i02 := 0;
        while Leitor.rExtrai(4, 'lacre', '', i02 + 1) <> '' do
        begin
          CTe.infCTeNorm.aquav.detCont[i01].Lacre.New;
          CTe.infCTeNorm.aquav.detCont[i01].Lacre[i02].nLacre := Leitor.rCampo(tcStr, 'nLacre');
          inc(i02);
        end;

        if Leitor.rExtrai(4, 'infDoc') <> ''
        then begin
          i02 := 0;
          while Leitor.rExtrai(5, 'infNF', '', i02 + 1) <> '' do
          begin
            CTe.infCTeNorm.aquav.detCont[i01].infDoc.infNF.New;
            CTe.infCTeNorm.aquav.detCont[i01].infDoc.infNF[i02].serie   := Leitor.rCampo(tcStr, 'Serie');
            CTe.infCTeNorm.aquav.detCont[i01].infDoc.infNF[i02].nDoc    := Leitor.rCampo(tcStr, 'nDoc');
            CTe.infCTeNorm.aquav.detCont[i01].infDoc.infNF[i02].unidRat := Leitor.rCampo(tcDe2, 'unidRat');
            inc(i02);
          end;
          i02 := 0;
          while Leitor.rExtrai(5, 'infNFe', '', i02 + 1) <> '' do
          begin
            CTe.infCTeNorm.aquav.detCont[i01].infDoc.infNFe.New;
            CTe.infCTeNorm.aquav.detCont[i01].infDoc.infNFe[i02].chave   := Leitor.rCampo(tcStr, 'chave');
            CTe.infCTeNorm.aquav.detCont[i01].infDoc.infNFe[i02].unidRat := Leitor.rCampo(tcDe2, 'unidRat');
            inc(i02);
          end;
        end;
        inc(i01);
      end;
    end; // fim das informações do modal Aquaviário

    if Leitor.rExtrai(2, 'ferrov') <> '' then
    begin
      CTe.infCTeNorm.ferrov.tpTraf := StrToTpTrafego(ok, Leitor.rCampo(tcStr, 'tpTraf'));
      CTe.infCTeNorm.ferrov.fluxo  := Leitor.rCampo(tcStr,'fluxo');
      CTe.infCTeNorm.ferrov.idTrem := Leitor.rCampo(tcStr,'idTrem');

      if VersaoDF >= ve300 then
      begin
        if Leitor.rExtrai(3, 'trafMut') <> '' then
        begin
          CTe.infCTeNorm.ferrov.trafMut.respFat := StrToTrafegoMutuo(ok, Leitor.rCampo(tcStr, 'respFat'));
          CTe.infCTeNorm.ferrov.trafMut.ferrEmi := StrToTrafegoMutuo(ok, Leitor.rCampo(tcStr, 'ferrEmi'));
          CTe.infCTeNorm.ferrov.vFrete          := Leitor.rCampo(tcDe2,'vFrete');

          CTe.infCTeNorm.ferrov.trafMut.chCTeFerroOrigem := Leitor.rCampo(tcStr,'chCTeFerroOrigem');

          i01 := 0;
          CTe.infCTeNorm.ferrov.ferroEnv.Clear;
          while Leitor.rExtrai(4, 'ferroEnv', '', i01 + 1) <> '' do
          begin
            CTe.infCTeNorm.ferrov.ferroEnv.New;
            CTe.infCTeNorm.ferrov.ferroEnv[i01].CNPJ  := Leitor.rCampo(tcStr,'CNPJ');
            CTe.infCTeNorm.ferrov.ferroEnv[i01].cInt  := Leitor.rCampo(tcStr,'cInt');
            CTe.infCTeNorm.ferrov.ferroEnv[i01].IE    := Leitor.rCampo(tcStr,'IE');
            CTe.infCTeNorm.ferrov.ferroEnv[i01].xNome := Leitor.rCampo(tcStr,'xNome');

            if Leitor.rExtrai(5, 'enderFerro') <> '' then
            begin
              CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
              CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.nro     := Leitor.rCampo(tcStr, 'nro');
              CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
              CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.xBairro := Leitor.rCampo(tcStr, 'xBairro');
              CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.cMun    := Leitor.rCampo(tcInt, 'cMun');
              CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.xMun    := Leitor.rCampo(tcStr, 'xMun');
              CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.CEP     := Leitor.rCampo(tcInt, 'CEP');
              CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.UF      := Leitor.rCampo(tcStr, 'UF');
            end;
            inc(i01);
          end;
        end;
      end
      else
      begin
        CTe.infCTeNorm.ferrov.vFrete := Leitor.rCampo(tcDe2,'vFrete');

        if Leitor.rExtrai(3, 'trafMut') <> '' then
        begin
          CTe.infCTeNorm.ferrov.trafMut.respFat := StrToTrafegoMutuo(ok, Leitor.rCampo(tcStr, 'respFat'));
          CTe.infCTeNorm.ferrov.trafMut.ferrEmi := StrToTrafegoMutuo(ok, Leitor.rCampo(tcStr, 'ferrEmi'));
        end;

        i01 := 0;
        CTe.infCTeNorm.ferrov.ferroEnv.Clear;
        while Leitor.rExtrai(3, 'ferroEnv', '', i01 + 1) <> '' do
        begin
          CTe.infCTeNorm.ferrov.ferroEnv.New;
          CTe.infCTeNorm.ferrov.ferroEnv[i01].CNPJ  := Leitor.rCampo(tcStr,'CNPJ');
          CTe.infCTeNorm.ferrov.ferroEnv[i01].cInt  := Leitor.rCampo(tcStr,'cInt');
          CTe.infCTeNorm.ferrov.ferroEnv[i01].IE    := Leitor.rCampo(tcStr,'IE');
          CTe.infCTeNorm.ferrov.ferroEnv[i01].xNome := Leitor.rCampo(tcStr,'xNome');

          if Leitor.rExtrai(4, 'enderFerro') <> '' then
          begin
            CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
            CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.nro     := Leitor.rCampo(tcStr, 'nro');
            CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
            CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.xBairro := Leitor.rCampo(tcStr, 'xBairro');
            CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.cMun    := Leitor.rCampo(tcInt, 'cMun');
            CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.xMun    := Leitor.rCampo(tcStr, 'xMun');
            CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.CEP     := Leitor.rCampo(tcInt, 'CEP');
            CTe.infCTeNorm.ferrov.ferroEnv[i01].EnderFerro.UF      := Leitor.rCampo(tcStr, 'UF');
          end;
          inc(i01);
        end;

        i01 := 0;
        CTe.infCTeNorm.ferrov.detVag.Clear;
        while Leitor.rExtrai(3, 'detVag', '', i01 + 1) <> '' do
        begin
          CTe.infCTeNorm.ferrov.detVag.New;
          CTe.infCTeNorm.ferrov.detVag[i01].nVag   := Leitor.rCampo(tcInt, 'nVag');
          CTe.infCTeNorm.ferrov.detVag[i01].cap    := Leitor.rCampo(tcDe2, 'cap');
          CTe.infCTeNorm.ferrov.detVag[i01].tpVag  := Leitor.rCampo(tcStr, 'tpVag');
          CTe.infCTeNorm.ferrov.detVag[i01].pesoR  := Leitor.rCampo(tcDe2, 'pesoR');
          CTe.infCTeNorm.ferrov.detVag[i01].pesoBC := Leitor.rCampo(tcDe2, 'pesoBC');
          inc(i01);
        end;
      end;

    end; // fim das informações do modal Ferroviário

    if Leitor.rExtrai(2, 'duto') <> '' then
    begin
      CTe.infCTeNorm.duto.vTar := Leitor.rCampo(tcDe6, 'vTar');
      CTe.infCTeNorm.duto.dIni := Leitor.rCampo(tcDat, 'dIni');
      CTe.infCTeNorm.duto.dFim := Leitor.rCampo(tcDat, 'dFim');
    end; // fim das informações do modal Dutoviário

    if Leitor.rExtrai(2, 'multimodal') <> '' then
    begin
      CTe.infCTeNorm.multimodal.COTM          := Leitor.rCampo(tcStr, 'COTM');
      CTe.infCTeNorm.multimodal.indNegociavel := StrToindNegociavel(ok, Leitor.rCampo(tcStr, 'indNegociavel'));
      // dados sobre o seguro informados somente na versão 3.00
      CTe.infCTeNorm.multimodal.xSeg          := Leitor.rCampo(tcStr, 'xSeg');
      CTe.infCTeNorm.multimodal.CNPJ          := Leitor.rCampo(tcStr, 'CNPJ');
      CTe.infCTeNorm.multimodal.nApol         := Leitor.rCampo(tcStr, 'nApol');
      CTe.infCTeNorm.multimodal.nAver         := Leitor.rCampo(tcStr, 'nAver');
    end; // fim das informações do Multimodal

    i01 := 0;
    CTe.infCTeNorm.peri.Clear;
    while Leitor.rExtrai(2, 'peri', '', i01 + 1) <> '' do
    begin
      CTe.infCTeNorm.peri.New;
      CTe.infCTeNorm.peri[i01].nONU        := Leitor.rCampo(tcStr, 'nONU');
      CTe.infCTeNorm.peri[i01].xNomeAE     := Leitor.rCampo(tcStr, 'xNomeAE');
      CTe.infCTeNorm.peri[i01].xClaRisco   := Leitor.rCampo(tcStr, 'xClaRisco');
      CTe.infCTeNorm.peri[i01].grEmb       := Leitor.rCampo(tcStr, 'grEmb');
      CTe.infCTeNorm.peri[i01].qTotProd    := Leitor.rCampo(tcStr, 'qTotProd');
      CTe.infCTeNorm.peri[i01].qVolTipo    := Leitor.rCampo(tcStr, 'qVolTipo');
      CTe.infCTeNorm.peri[i01].pontoFulgor := Leitor.rCampo(tcStr, 'pontoFulgor');
      CTe.infCTeNorm.peri[i01].qTotEmb     := Leitor.rCampo(tcStr, 'qTotEmb');
      CTe.infCTeNorm.peri[i01].uniAP       := StrToUniMed(ok, Leitor.rCampo(tcStr, 'uniAP'));
      inc(i01);
    end;

    i01 := 0;
    CTe.infCTeNorm.veicNovos.Clear;
    while Leitor.rExtrai(2, 'veicNovos', '', i01 + 1) <> '' do
    begin
      CTe.infCTeNorm.veicNovos.New;
      CTe.infCTeNorm.veicNovos[i01].chassi := Leitor.rCampo(tcStr, 'chassi');
      CTe.infCTeNorm.veicNovos[i01].cCor   := Leitor.rCampo(tcStr, 'cCor');
      CTe.infCTeNorm.veicNovos[i01].xCor   := Leitor.rCampo(tcStr, 'xCor');
      CTe.infCTeNorm.veicNovos[i01].cMod   := Leitor.rCampo(tcStr, 'cMod');
      CTe.infCTeNorm.veicNovos[i01].vUnit  := Leitor.rCampo(tcDe2, 'vUnit');
      CTe.infCTeNorm.veicNovos[i01].vFrete := Leitor.rCampo(tcDe2, 'vFrete');
      inc(i01);
    end;

    if Leitor.rExtrai(2, 'cobr') <> '' then
    begin
      CTe.infCTeNorm.cobr.fat.nFat  := Leitor.rCampo(tcStr, 'nFat');
      CTe.infCTeNorm.cobr.fat.vOrig := Leitor.rCampo(tcDe2, 'vOrig');
      CTe.infCTeNorm.cobr.fat.vDesc := Leitor.rCampo(tcDe2, 'vDesc');
      CTe.infCTeNorm.cobr.fat.vLiq  := Leitor.rCampo(tcDe2, 'vLiq');

      i01 := 0;
      CTe.infCTeNorm.cobr.dup.Clear;
      while Leitor.rExtrai(3, 'dup', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.cobr.dup.New;
        CTe.infCTeNorm.cobr.dup[i01].nDup  := Leitor.rCampo(tcStr, 'nDup');
        CTe.infCTeNorm.cobr.dup[i01].dVenc := Leitor.rCampo(tcDat, 'dVenc');
        CTe.infCTeNorm.cobr.dup[i01].vDup  := Leitor.rCampo(tcDe2, 'vDup');
        inc(i01);
      end;
    end;

    i01 := 0;
    CTe.infCTeNorm.infGTVe.Clear;
    while Leitor.rExtrai(2, 'infGTVe', '', i01 + 1) <> '' do
    begin
      CTe.infCTeNorm.infGTVe.New;
      CTe.infCTeNorm.infGTVe[i01].chCTe  := Leitor.rCampo(tcStr, 'chCTe');

      i02 := 0;
      while Leitor.rExtrai(3, 'Comp', '', i02 + 1) <> '' do
      begin
        CTe.infCTeNorm.infGTVe[i01].Comp.New;
        CTe.infCTeNorm.infGTVe[i01].Comp[i02].tpComp := StrTotpComp(Ok, Leitor.rCampo(tcStr, 'tpComp'));
        CTe.infCTeNorm.infGTVe[i01].Comp[i02].vComp  := Leitor.rCampo(tcDe2, 'vComp');
        CTe.infCTeNorm.infGTVe[i01].Comp[i02].xComp  := Leitor.rCampo(tcStr, 'xComp');
        inc(i02);
      end;

      inc(i01);
    end;

    if Leitor.rExtrai(2, 'infCteSub') <> '' then
    begin
      CTe.infCTeNorm.infCTeSub.chCte := Leitor.rCampo(tcStr, 'chCte');

      if VersaoDF >= ve300 then
        CTe.infCTeNorm.infCTeSub.refCteAnu := Leitor.rCampo(tcStr, 'refCteAnu');

      if Leitor.rCampo(tcStr, 'indAlteraToma') <> '' then
        CTe.infCTeNorm.infCTeSub.indAlteraToma := StrToTIndicador(Ok, Leitor.rCampo(tcStr, 'indAlteraToma'));

      if Leitor.rExtrai(3, 'tomaICMS') <> '' then
      begin
        CTe.infCTeNorm.infCTeSub.tomaICMS.refNFe := Leitor.rCampo(tcStr, 'refNFe');
        CTe.infCTeNorm.infCTeSub.tomaICMS.refCte := Leitor.rCampo(tcStr, 'refCte');
        if Leitor.rExtrai(4, 'refNF') <> '' then
        begin
          CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.CNPJCPF  := Leitor.rCampoCNPJCPF;
          CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.modelo   := Leitor.rCampo(tcStr, 'mod');
          CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.serie    := Leitor.rCampo(tcInt, 'serie');
          CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.subserie := Leitor.rCampo(tcInt, 'subserie');
          CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.nro      := Leitor.rCampo(tcInt, 'nro');
          CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.valor    := Leitor.rCampo(tcDe2, 'valor');
          CTe.infCTeNorm.infCTeSub.tomaICMS.refNF.dEmi     := Leitor.rCampo(tcDat, 'dEmi');
        end;
      end;

      if Leitor.rExtrai(3, 'tomaNaoICMS') <> '' then
        CTe.infCTeNorm.infCTeSub.tomaNaoICMS.refCteAnu := Leitor.rCampo(tcStr, 'refCteAnu');
    end;

    if Leitor.rExtrai(2, 'infGlobalizado') <> '' then
      CTe.infCTeNorm.infGlobalizado.xObs := Leitor.rCampo(tcStr, 'xObs');

    if Leitor.rExtrai(2, 'infServVinc') <> '' then
    begin
      i01 := 0;
      CTe.infCTeNorm.infServVinc.infCTeMultimodal.Clear;
      while Leitor.rExtrai(3, 'infCTeMultimodal', '', i01 + 1) <> '' do
      begin
        CTe.infCTeNorm.infServVinc.infCTeMultimodal.New;
        CTe.infCTeNorm.infServVinc.infCTeMultimodal[i01].chCTeMultimodal := Leitor.rCampo(tcStr, 'chCTeMultimodal');
        inc(i01);
      end;
    end;
  end; // fim do infCTeNorm

  (* Grupo da TAG <infCteComp> ************************************************)

  if VersaoDF <= ve300 then
  begin
    if Leitor.rExtrai(1, 'infCteComp') <> ''
    then begin
      if (VersaoDF >= ve300) then
         CTe.InfCTeComp.Chave := Leitor.rCampo(tcStr, 'chCTe')
      else
        CTe.InfCTeComp.Chave := Leitor.rCampo(tcStr, 'chave');
    end; // fim de infCteComp
  end
  else
  begin
    i01 := 0;
    CTe.InfCTeComp10.Clear;
    while Leitor.rExtrai(1, 'infCteComp', '', i01 + 1) <> '' do
    begin
      CTe.InfCTeComp10.New;
      CTe.InfCTeComp10[i01].chCTe := Leitor.rCampo(tcStr, 'chCTe');
      inc(i01);
    end;
  end;

  (* Grupo da TAG <infCteAnu> ************************************************)
  if Leitor.rExtrai(1, 'infCteAnu') <> '' then
  begin
    CTe.InfCTeAnu.chCTe := Leitor.rCampo(tcStr,'chCte');
    CTe.InfCTeAnu.dEmi  := Leitor.rCampo(tcDat,'dEmi');
  end;

  (* Grupo da TAG <autXML> ****************************************************)
  i01 := 0;
  CTe.autXML.Clear;
  while Leitor.rExtrai(1, 'autXML', '', i01 + 1) <> '' do
  begin
    CTe.autXML.New;
    CTe.autXML[i01].CNPJCPF := Leitor.rCampoCNPJCPF;;
    inc(i01);
  end;

  if Leitor.rExtrai(1, 'infRespTec') <> '' then
  begin
    CTe.infRespTec.CNPJ     := Leitor.rCampo(tcStr, 'CNPJ');
    CTe.infRespTec.xContato := Leitor.rCampo(tcStr, 'xContato');
    CTe.infRespTec.email    := Leitor.rCampo(tcStr, 'email');
    CTe.infRespTec.fone     := Leitor.rCampo(tcStr, 'fone');
    CTe.infRespTec.idCSRT   := Leitor.rCampo(tcInt, 'idCSRT');
    CTe.infRespTec.hashCSRT := Leitor.rCampo(tcStr, 'hashCSRT');
  end;

  (* Grupo da TAG <signature> *************************************************)
  Leitor.Grupo := Leitor.Arquivo;

  CTe.signature.URI             := Leitor.rAtributo('URI=', 'Reference');
  CTe.signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
  CTe.signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
  CTe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');

  (* Grupo da TAG <infCTeSupl> ************************************************)
  if Leitor.rExtrai(1, 'infCTeSupl') <> '' then
  begin
    CTe.infCTeSupl.qrCodCTe := Leitor.rCampo(tcStr, 'qrCodCTe');
    CTe.infCTeSupl.qrCodCTe := StringReplace(CTe.infCTeSupl.qrCodCTe, '<![CDATA[', '', []);
    CTe.infCTeSupl.qrCodCTe := StringReplace(CTe.infCTeSupl.qrCodCTe, ']]>', '', []);
  end;

  (* Grupo da TAG <protCTe> ****************************************************)
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

  Result := true;
end;

end.

