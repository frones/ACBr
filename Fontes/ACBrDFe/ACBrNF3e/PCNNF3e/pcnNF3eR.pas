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

unit pcnNF3eR;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor, pcnNF3e;

type

  TNF3eR = class(TPersistent)
  private
    FLeitor: TLeitor;
    FNF3e: TNF3e;
  public
    constructor Create(AOwner: TNF3e);
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property NF3e: TNF3e     read FNF3e   write FNF3e;
  end;

implementation

uses
  pcnConversaoNF3e,
  ACBrUtil;

{ TNF3eR }

constructor TNF3eR.Create(AOwner: TNF3e);
begin
  inherited Create;

  FLeitor := TLeitor.Create;
  FNF3e   := AOwner;
end;

destructor TNF3eR.Destroy;
begin
  FLeitor.Free;

  inherited Destroy;
end;

function TNF3eR.LerXml: Boolean;
var
  ok: Boolean;
  i, j, k: Integer;
  Aspas, VersaoInfNF3e, xData: string;
begin
  Leitor.Grupo := Leitor.Arquivo;

  if Pos('versao="', Leitor.Arquivo) <> 0 then
    Aspas := '"'
   else
    Aspas := '''';

  NF3e.infNF3e.Id := Leitor.rAtributo('Id=', 'infNF3e');
  if OnlyNumber(NF3e.infNF3e.Id) = '' then
    raise Exception.Create('Não encontrei o atributo: Id');

  VersaoInfNF3e := Leitor.rAtributo('versao=', 'infNF3e');
  if StringToFloatDef(VersaoInfNF3e,-1) = -1 then
    raise Exception.Create('Não encontrei o atributo: versao');

  NF3e.infNF3e.Versao := StringToFloat(VersaoInfNF3e);

  if Leitor.rExtrai(1, 'ide') <> '' then
  begin
    NF3e.ide.cUF     := Leitor.rCampo(tcInt, 'cUF');
    NF3e.Ide.tpAmb   := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
    NF3e.ide.modelo  := Leitor.rCampo(tcInt, 'mod');
    NF3e.ide.serie   := Leitor.rCampo(tcInt, 'serie');
    NF3e.ide.nNF     := Leitor.rCampo(tcInt, 'nNF');
    NF3e.ide.cNF     := Leitor.rCampo(tcInt, 'cNF');
    NF3e.Ide.cDV     := Leitor.rCampo(tcInt, 'cDV');
    NF3e.ide.dhEmi   := Leitor.rCampo(tcDatHor, 'dhEmi');
    NF3e.Ide.tpEmis  := StrToTpEmis(ok, Leitor.rCampo(tcStr, 'tpEmis'));
    NF3e.ide.cMunFG  := Leitor.rCampo(tcInt, 'cMunFG');
    NF3e.Ide.finNF3e := StrToFinNF3e(ok, Leitor.rCampo(tcStr, 'finNF3e'));
    NF3e.Ide.verProc := Leitor.rCampo(tcStr, 'verProc');
    NF3e.Ide.dhCont  := Leitor.rCampo(tcDatHor, 'dhCont');
    NF3e.Ide.xJust   := Leitor.rCampo(tcStr, 'xJust');
  end;

  if Leitor.rExtrai(1, 'emit') <> '' then
  begin
    NF3e.Emit.CNPJ  := Leitor.rCampo(tcStr, 'CNPJ');
    NF3e.Emit.IE    := Leitor.rCampo(tcStr, 'IE');
    NF3e.Emit.xNome := Leitor.rCampo(tcStr, 'xNome');
    NF3e.Emit.xFant := Leitor.rCampo(tcStr, 'xFant');

    if Leitor.rExtrai(2, 'enderEmit') <> '' then
    begin
      NF3e.Emit.enderEmit.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      NF3e.Emit.enderEmit.nro     := Leitor.rCampo(tcStr, 'nro');
      NF3e.Emit.enderEmit.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      NF3e.Emit.enderEmit.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      NF3e.Emit.EnderEmit.cMun    := Leitor.rCampo(tcInt, 'cMun');
      NF3e.Emit.enderEmit.xMun    := Leitor.rCampo(tcStr, 'xMun');
      NF3e.Emit.enderEmit.CEP     := Leitor.rCampo(tcInt, 'CEP');
      NF3e.Emit.enderEmit.UF      := Leitor.rCampo(tcStr, 'UF');
      NF3e.Emit.enderEmit.fone    := Leitor.rCampo(tcStr, 'fone');
      NF3e.Emit.enderEmit.email   := Leitor.rCampo(tcStr, 'email');
    end;
  end;

  if Leitor.rExtrai(1, 'dest') <> '' then
  begin
    NF3e.Dest.xNome          := Leitor.rCampo(tcStr, 'xNome');
    NF3e.Dest.CNPJCPF        := Leitor.rCampoCNPJCPF;
    NF3e.Dest.idOutros       := Leitor.rCampo(tcStr, 'idOutros');
    NF3e.Dest.indIEDest      := StrToindIEDest(Ok, Leitor.rCampo(tcStr, 'indIEDest'));
    NF3e.Dest.IE             := Leitor.rCampo(tcStr, 'IE');
    NF3e.Dest.IM             := Leitor.rCampo(tcStr, 'IM');
    NF3e.Dest.cNIS           := Leitor.rCampo(tcStr, 'cNIS');
    NF3e.Dest.xNomeAdicional := Leitor.rCampo(tcStr, 'xNomeAdicional');

    if Leitor.rExtrai(2, 'enderDest') <> '' then
    begin
      NF3e.Dest.enderDest.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      NF3e.Dest.enderDest.nro     := Leitor.rCampo(tcStr, 'nro');
      NF3e.Dest.enderDest.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      NF3e.Dest.enderDest.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      NF3e.Dest.enderDest.cMun    := Leitor.rCampo(tcInt, 'cMun');
      NF3e.Dest.enderDest.xMun    := Leitor.rCampo(tcStr, 'xMun');
      NF3e.Dest.enderDest.CEP     := Leitor.rCampo(tcInt, 'CEP');
      NF3e.Dest.enderDest.UF      := Leitor.rCampo(tcStr, 'UF');
      NF3e.Dest.enderDest.fone    := Leitor.rCampo(tcStr, 'fone');
      NF3e.Dest.enderDest.email   := Leitor.rCampo(tcStr, 'email');
    end;
  end;

  if Leitor.rExtrai(1, 'acessante') <> '' then
  begin
    NF3e.acessante.idAcesso          := Leitor.rCampo(tcStr, 'idAcesso');
    NF3e.acessante.idCodCliente      := Leitor.rCampo(tcStr, 'idCodCliente');
    NF3e.acessante.tpAcesso          := StrTotpAcesso(ok, Leitor.rCampo(tcStr, 'tpAcesso'));
    NF3e.acessante.xNomeUC           := Leitor.rCampo(tcStr, 'xNomeUC');
    NF3e.acessante.tpClasse          := StrTotpClasse(ok, Leitor.rCampo(tcStr, 'tpClasse'));
    NF3e.acessante.tpSubClasse       := StrTotpSubClasse(ok, Leitor.rCampo(tcStr, 'tpSubClasse'));
    NF3e.acessante.tpFase            := StrTotpFase(ok, Leitor.rCampo(tcStr, 'tpFase'));
    NF3e.acessante.tpGrpTensao       := StrTotpGrpTensao(ok, Leitor.rCampo(tcStr, 'tpGrpTensao'));
    NF3e.acessante.tpModTar          := StrTotpModTar(ok, Leitor.rCampo(tcStr, 'tpModTar'));
    NF3e.acessante.latGPS            := Leitor.rCampo(tcStr, 'latGPS');
    NF3e.acessante.longGPS           := Leitor.rCampo(tcStr, 'longGPS');
    NF3e.acessante.codRoteiroLeitura := Leitor.rCampo(tcStr, 'codRoteiroLeitura');
  end;

  if Leitor.rExtrai(1, 'gSub') <> '' then
  begin
    NF3e.gSub.chNF3e := Leitor.rCampo(tcStr, 'chNF3e');
    NF3e.gSub.motSub := StrToMotSub(ok, Leitor.rCampo(tcStr, 'motSub'));

    if Leitor.rExtrai(2, 'gNF') <> '' then
    begin
      NF3e.gSub.CNPJ  := Leitor.rCampo(tcStr, 'CNPJ');
      NF3e.gSub.serie := Leitor.rCampo(tcInt, 'serie');
      NF3e.gSub.nNF   := Leitor.rCampo(tcInt, 'nNF');

      xData := Leitor.rCampo(tcStr, 'CompetEmis');
      xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
      NF3e.gSub.CompetEmis := StrToDate(xData);

      xData := Leitor.rCampo(tcStr, 'CompetApur');
      xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
      NF3e.gSub.CompetApur := StrToDate(xData);

      NF3e.gSub.hash115 := Leitor.rCampo(tcStr, 'hash115');
    end;
  end;

  if Leitor.rExtrai(1, 'gJudic') <> '' then
  begin
    NF3e.gJudic.chNF3e := Leitor.rCampo(tcStr, 'chNF3e');
  end;

  i := 0;
  NF3e.gGrContrat.Clear;
  while Leitor.rExtrai(1, 'gGrContrat', '', i + 1) <> '' do
  begin
    NF3e.gGrContrat.New;
    NF3e.gGrContrat[i].nContrat     := Leitor.rAtributo('nContrat=', 'gGrContrat');
    NF3e.gGrContrat[i].tpGrContrat  := StrTotpGrContrat(ok, Leitor.rCampo(tcStr, 'tpGrContrat'));
    NF3e.gGrContrat[i].tpPosTar     := StrTotpPosTar(ok, Leitor.rCampo(tcStr, 'tpPosTar'));
    NF3e.gGrContrat[i].qUnidContrat := Leitor.rCampo(tcDe2, 'qUnidContrat');

    inc(i);
  end;

  i := 0;
  NF3e.gMed.Clear;
  while Leitor.rExtrai(1, 'gMed', '', i + 1) <> '' do
  begin
    NF3e.gMed.New;
    NF3e.gMed[i].nMed      := Leitor.rAtributo('nMed=', 'gMed');
    NF3e.gMed[i].idMedidor := Leitor.rCampo(tcStr, 'idMedidor');
    NF3e.gMed[i].dMedAnt   := Leitor.rCampo(tcDat, 'dMedAnt');
    NF3e.gMed[i].dMedAtu   := Leitor.rCampo(tcDat, 'dMedAtu');

    inc(i);
  end;

  if Leitor.rExtrai(1, 'gSCEE') <> '' then
  begin
    NF3e.gSCEE.tpPartComp := StrTotpPartComp(ok, Leitor.rCampo(tcStr, 'tpPartComp'));
    NF3e.gSCEE.vPotInst   := Leitor.rCampo(tcDe3, 'vPotInst');

    i := 0;
    NF3e.gSCEE.gConsumidor.Clear;
    while Leitor.rExtrai(2, 'gConsumidor', '', i + 1) <> '' do
    begin
      NF3e.gSCEE.gConsumidor.New;
      NF3e.gSCEE.gConsumidor[i].idAcessGer := Leitor.rCampo(tcStr, 'idAcessGer');
      NF3e.gSCEE.gConsumidor[i].enerAloc   := Leitor.rCampo(tcDe3, 'enerAloc');
      NF3e.gSCEE.gConsumidor[i].tpPosTar   := StrTotpPosTar(ok, Leitor.rCampo(tcStr, 'tpPosTar'));

      inc(i);
    end;

    i := 0;
    NF3e.gSCEE.gSaldoCred.Clear;
    while Leitor.rExtrai(2, 'gSaldoCred', '', i + 1) <> '' do
    begin
      NF3e.gSCEE.gSaldoCred.New;
      NF3e.gSCEE.gSaldoCred[i].tpPosTar      := StrTotpPosTar(ok, Leitor.rCampo(tcStr, 'tpPosTar'));
      NF3e.gSCEE.gSaldoCred[i].vSaldAnt      := Leitor.rCampo(tcInt, 'vSaldAnt');
      NF3e.gSCEE.gSaldoCred[i].vCredExpirado := Leitor.rCampo(tcInt, 'vCredExpirado');
      NF3e.gSCEE.gSaldoCred[i].vSaldAtual    := Leitor.rCampo(tcInt, 'vSaldAtual');
      NF3e.gSCEE.gSaldoCred[i].vCredExpirar  := Leitor.rCampo(tcInt, 'vCredExpirar');

      xData := Leitor.rCampo(tcStr, 'CompetExpirar');
      xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
      NF3e.gSCEE.gSaldoCred[i].CompetExpirar := StrToDate(xData);

      inc(i);
    end;
  end;

  i := 0;
  NF3e.NFDet.Clear;
  while Leitor.rExtrai(1, 'NFdet', '', i + 1) <> '' do
  begin
    NF3e.NFDet.New;
    NF3e.NFDet[i].chNF3eAnt := Leitor.rAtributo('chNF3eAnt=', 'NFdet');

    j := 0;
    NF3e.NFDet[i].Det.Clear;
    while Leitor.rExtrai(2, 'det', '', j + 1) <> '' do
    begin
      NF3e.NFDet[i].Det.New;
      NF3e.NFDet[i].Det[j].nItem := Leitor.rAtributo('nItem=', 'Det');

      if Leitor.rExtrai(3, 'gAjusteNF3eAnt') <> '' then
      begin
        NF3e.NFDet[i].Det[j].gAjusteNF3eAnt.tpAjuste  := StrTotpAjuste(ok, Leitor.rCampo(tcStr, 'tpAjuste'));
        NF3e.NFDet[i].Det[j].gAjusteNF3eAnt.motAjuste := StrTomotAjuste(ok, Leitor.rCampo(tcStr, 'motAjuste'));
      end
      else
        NF3e.NFDet[i].Det[j].gAjusteNF3eAnt.tpAjuste := taNenhum;

      if Leitor.rExtrai(3, 'detItemAnt') <> '' then
      begin
        NF3e.NFDet[i].Det[j].detItemAnt.nItemAnt  := Leitor.rAtributo('nItemAnt=', 'detItemAnt');
        NF3e.NFDet[i].Det[j].detItemAnt.vItem     := Leitor.rCampo(tcDe2, 'vItem');
        NF3e.NFDet[i].Det[j].detItemAnt.qFaturada := Leitor.rCampo(tcDe4, 'qFaturada');
        NF3e.NFDet[i].Det[j].detItemAnt.vProd     := Leitor.rCampo(tcDe2, 'vProd');
        NF3e.NFDet[i].Det[j].detItemAnt.cClass    := Leitor.rCampo(tcInt, 'cClass');
        NF3e.NFDet[i].Det[j].detItemAnt.vBC       := Leitor.rCampo(tcDe2, 'vBC');
        NF3e.NFDet[i].Det[j].detItemAnt.pICMS     := Leitor.rCampo(tcDe2, 'pICMS');
        NF3e.NFDet[i].Det[j].detItemAnt.vICMS     := Leitor.rCampo(tcDe2, 'vICMS');
        NF3e.NFDet[i].Det[j].detItemAnt.vPIS      := Leitor.rCampo(tcDe2, 'vPIS');
        NF3e.NFDet[i].Det[j].detItemAnt.vCOFINS   := Leitor.rCampo(tcDe2, 'vCOFINS');
      end;

      if Leitor.rExtrai(3, 'detItem') <> '' then
      begin
        NF3e.NFDet[i].Det[j].detItem.nItemAnt  := StrToIntDef(Leitor.rAtributo('nItemAnt=', 'detItem'), 0);
        NF3e.NFDet[i].Det[j].detItem.infAdProd := Leitor.rCampo(tcStr, 'infAdProd');

        k := 0;
        NF3e.NFDet[i].Det[j].detItem.gTarif.Clear;
        while Leitor.rExtrai(4, 'gTarif', '', k + 1) <> '' do
        begin
          NF3e.NFDet[i].Det[j].detItem.gTarif.New;
          NF3e.NFDet[i].Det[j].detItem.gTarif[k].dIniTarif   := Leitor.rCampo(tcDat, 'dIniTarif');
          NF3e.NFDet[i].Det[j].detItem.gTarif[k].dFimTarif   := Leitor.rCampo(tcDat, 'dFimTarif');
          NF3e.NFDet[i].Det[j].detItem.gTarif[k].tpAto       := StrTotpAto(ok, Leitor.rCampo(tcStr, 'tpAto'));
          NF3e.NFDet[i].Det[j].detItem.gTarif[k].nAto        := Leitor.rCampo(tcStr, 'nAto');
          NF3e.NFDet[i].Det[j].detItem.gTarif[k].anoAto      := Leitor.rCampo(tcInt, 'anoAto');
          NF3e.NFDet[i].Det[j].detItem.gTarif[k].tpTarif     := StrTotpTarif(ok, Leitor.rCampo(tcStr, 'tpTarif'));
          NF3e.NFDet[i].Det[j].detItem.gTarif[k].cPosTarif   := StrTocPosTarif(ok, Leitor.rCampo(tcStr, 'cPosTarif'));
          NF3e.NFDet[i].Det[j].detItem.gTarif[k].uMed        := StrTouMed(ok, Leitor.rCampo(tcStr, 'uMed'));
          NF3e.NFDet[i].Det[j].detItem.gTarif[k].vTarifHom   := Leitor.rCampo(tcDe6, 'vTarifHom');
          NF3e.NFDet[i].Det[j].detItem.gTarif[k].vTarifAplic := Leitor.rCampo(tcDe6, 'vTarifAplic');
          NF3e.NFDet[i].Det[j].detItem.gTarif[k].motDifTarif := StrTomotDifTarif(ok, Leitor.rCampo(tcStr, 'motDifTarif'));

          inc(k);
        end;

        k := 0;
        NF3e.NFDet[i].Det[j].detItem.gAdBand.Clear;
        while Leitor.rExtrai(4, 'gAdBand', '', k + 1) <> '' do
        begin
          NF3e.NFDet[i].Det[j].detItem.gAdBand.New;
          NF3e.NFDet[i].Det[j].detItem.gAdBand[k].dIniAdBand   := Leitor.rCampo(tcDat, 'dIniAdBand');
          NF3e.NFDet[i].Det[j].detItem.gAdBand[k].dFimAdBand   := Leitor.rCampo(tcDat, 'dFimAdBand');
          NF3e.NFDet[i].Det[j].detItem.gAdBand[k].tpBand       := StrTotpBand(ok, Leitor.rCampo(tcStr, 'tpBand'));
          NF3e.NFDet[i].Det[j].detItem.gAdBand[k].vAdBand      := Leitor.rCampo(tcDe2, 'vAdBand');
          NF3e.NFDet[i].Det[j].detItem.gAdBand[k].vAdBandAplic := Leitor.rCampo(tcDe2, 'vAdBandAplic');
          NF3e.NFDet[i].Det[j].detItem.gAdBand[k].motDifBand   := StrTomotDifBand(ok, Leitor.rCampo(tcStr, 'motDifBand'));

          inc(k);
        end;

        if Leitor.rExtrai(4, 'prod') <> '' then
        begin
          NF3e.NFDet[i].Det[j].detItem.Prod.indOrigemQtd := StrToindOrigemQtd(ok, Leitor.rCampo(tcStr, 'indOrigemQtd'));
          NF3e.NFDet[i].Det[j].detItem.Prod.cProd        := Leitor.rCampo(tcStr, 'cProd');
          NF3e.NFDet[i].Det[j].detItem.Prod.xProd        := Leitor.rCampo(tcStr, 'xProd');
          NF3e.NFDet[i].Det[j].detItem.Prod.cClass       := Leitor.rCampo(tcInt, 'cClass');
          NF3e.NFDet[i].Det[j].detItem.Prod.CFOP         := Leitor.rCampo(tcInt, 'CFOP');
          NF3e.NFDet[i].Det[j].detItem.Prod.uMed         := StrTouMedFat(ok, Leitor.rCampo(tcStr, 'uMed'));
          NF3e.NFDet[i].Det[j].detItem.Prod.qFaturada    := Leitor.rCampo(tcDe4, 'qFaturada');
          NF3e.NFDet[i].Det[j].detItem.Prod.vItem        := Leitor.rCampo(tcDe2, 'vItem');
          NF3e.NFDet[i].Det[j].detItem.Prod.vProd        := Leitor.rCampo(tcDe2, 'vProd');
          NF3e.NFDet[i].Det[j].detItem.Prod.indDevolucao := StrToTIndicador(ok, Leitor.rCampo(tcStr, 'indDevolucao'));
          NF3e.NFDet[i].Det[j].detItem.Prod.indPrecoACL  := StrToTIndicador(ok, Leitor.rCampo(tcStr, 'indPrecoACL'));

          if Leitor.rExtrai(5, 'gMedicao') <> '' then
          begin
            NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.nMed            := Leitor.rCampo(tcInt, 'nMed');
            NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.nContrat        := Leitor.rCampo(tcInt, 'nContrat');
            NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.tpMotNaoLeitura := StrTotpMotNaoLeitura(ok, Leitor.rCampo(tcStr, 'tpMotNaoLeitura'));

            if Leitor.rExtrai(6, 'gMedida') <> '' then
            begin
              NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.tpGrMed         := StrTotpGrMed(ok, Leitor.rCampo(tcStr, 'tpGrMed'));
              NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.cPosTarif       := StrTocPosTarif(ok, Leitor.rCampo(tcStr, 'cPosTarif'));
              NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.uMed            := StrTouMedFat(ok, Leitor.rCampo(tcStr, 'uMed'));
              NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.vMedAnt         := Leitor.rCampo(tcDe2, 'vMedAnt');
              NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.vMedAtu         := Leitor.rCampo(tcDe2, 'vMedAtu');
              NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.vConst          := Leitor.rCampo(tcDe2, 'vConst');
              NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.vMed            := Leitor.rCampo(tcDe2, 'vMed');
              NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.pPerdaTran      := Leitor.rCampo(tcDe2, 'pPerdaTran');
              NF3e.NFDet[i].Det[j].detItem.Prod.gMedicao.vMedPerdaTran   := Leitor.rCampo(tcDe2, 'vMedPerdaTran');
            end;
          end;
        end;

        if Leitor.rExtrai(4, 'imposto') <> '' then
        begin
          if (Leitor.rExtrai(5, 'ICMS00') <> '') or (Leitor.rExtrai(5, 'ICMS10') <> '') or
             (Leitor.rExtrai(5, 'ICMS20') <> '') or (Leitor.rExtrai(5, 'ICMS40') <> '') or
             (Leitor.rExtrai(5, 'ICMS51') <> '') or (Leitor.rExtrai(5, 'ICMS90') <> '') then
          begin
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.CST        := StrToCSTICMS(ok, Leitor.rCampo(tcStr, 'CST'));
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vBC        := Leitor.rCampo(tcDe2, 'vBC');
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pICMS      := Leitor.rCampo(tcDe2, 'pICMS');
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMS      := Leitor.rCampo(tcDe2, 'vICMS');
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pFCP       := Leitor.rCampo(tcDe4, 'pFCP');
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vFCP       := Leitor.rCampo(tcDe2, 'vFCP');
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vBCST      := Leitor.rCampo(tcDe2, 'vBCST');
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pICMSST    := Leitor.rCampo(tcDe2, 'pICMSST');
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMSST    := Leitor.rCampo(tcDe2, 'vICMSST');
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pFCPST     := Leitor.rCampo(tcDe4, 'pFCPST');
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vFCPST     := Leitor.rCampo(tcDe2, 'vFCPST');
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pRedBC     := Leitor.rCampo(tcDe2, 'pRedBC');
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMSDeson := Leitor.rCampo(tcDe2, 'vICMSDeson');
            NF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.cBenef     := Leitor.rCampo(tcStr, 'cBenef');
          end;

          if Leitor.rExtrai(5, 'PIS') <> '' then
          begin
            NF3e.NFDet[i].Det[j].detItem.Imposto.PIS.CST  := StrToCSTPIS(ok, Leitor.rCampo(tcStr, 'CST'));
            NF3e.NFDet[i].Det[j].detItem.Imposto.PIS.vBC  := Leitor.rCampo(tcDe2, 'vBC');
            NF3e.NFDet[i].Det[j].detItem.Imposto.PIS.pPIS := Leitor.rCampo(tcDe2, 'pPIS');
            NF3e.NFDet[i].Det[j].detItem.Imposto.PIS.vPIS := Leitor.rCampo(tcDe2, 'vPIS');
          end;

          if Leitor.rExtrai(5, 'COFINS') <> '' then
          begin
            NF3e.NFDet[i].Det[j].detItem.Imposto.COFINS.CST     := StrToCSTCOFINS(ok, Leitor.rCampo(tcStr, 'CST'));
            NF3e.NFDet[i].Det[j].detItem.Imposto.COFINS.vBC     := Leitor.rCampo(tcDe2, 'vBC');
            NF3e.NFDet[i].Det[j].detItem.Imposto.COFINS.pCOFINS := Leitor.rCampo(tcDe2, 'pCOFINS');
            NF3e.NFDet[i].Det[j].detItem.Imposto.COFINS.vCOFINS := Leitor.rCampo(tcDe2, 'vCOFINS');
          end;
        end;

        if Leitor.rExtrai(4, 'gProcRef') <> '' then
        begin
          NF3e.NFDet[i].Det[j].detItem.gProcRef.vItem        := Leitor.rCampo(tcDe2, 'vItem');
          NF3e.NFDet[i].Det[j].detItem.gProcRef.qFaturada    := Leitor.rCampo(tcDe4, 'qFaturada');
          NF3e.NFDet[i].Det[j].detItem.gProcRef.vProd        := Leitor.rCampo(tcDe2, 'vProd');
          NF3e.NFDet[i].Det[j].detItem.gProcRef.indDevolucao := StrToTIndicador(ok, Leitor.rCampo(tcStr, 'indDevolucao'));
          NF3e.NFDet[i].Det[j].detItem.gProcRef.vBC          := Leitor.rCampo(tcDe2, 'vBC');
          NF3e.NFDet[i].Det[j].detItem.gProcRef.pICMS        := Leitor.rCampo(tcDe2, 'pICMS');
          NF3e.NFDet[i].Det[j].detItem.gProcRef.vICMS        := Leitor.rCampo(tcDe2, 'vICMS');
          NF3e.NFDet[i].Det[j].detItem.gProcRef.vPIS         := Leitor.rCampo(tcDe2, 'vPIS');
          NF3e.NFDet[i].Det[j].detItem.gProcRef.vCOFINS      := Leitor.rCampo(tcDe2, 'vCOFINS');

          k := 0;
          NF3e.NFDet[i].Det[j].detItem.gProcRef.gProc.Clear;
          while Leitor.rExtrai(5, 'gProc', '', k + 1) <> '' do
          begin
            NF3e.NFDet[i].Det[j].detItem.gProcRef.gProc.New;
            NF3e.NFDet[i].Det[j].detItem.gProcRef.gProc[k].tpProc    := StrTotpProc(ok, Leitor.rCampo(tcStr, 'tpProc'));
            NF3e.NFDet[i].Det[j].detItem.gProcRef.gProc[k].nProcesso := Leitor.rCampo(tcStr, 'nProcesso');

            inc(k);
          end;
        end;

        k := 0;
        NF3e.NFDet[i].Det[j].detItem.gContab.Clear;
        while Leitor.rExtrai(4, 'gContab', '', k + 1) <> '' do
        begin
          NF3e.NFDet[i].Det[j].detItem.gContab.New;
          NF3e.NFDet[i].Det[j].detItem.gContab[k].cContab := Leitor.rCampo(tcStr, 'cContab');
          NF3e.NFDet[i].Det[j].detItem.gContab[k].xContab := Leitor.rCampo(tcStr, 'xContab');
          NF3e.NFDet[i].Det[j].detItem.gContab[k].vContab := Leitor.rCampo(tcDe2, 'vContab');
          NF3e.NFDet[i].Det[j].detItem.gContab[k].tpLanc  := StrTotpLanc(ok, Leitor.rCampo(tcStr, 'tpLanc'));

          inc(k);
        end;
      end;

      inc(j);
    end;

    inc(i);
  end;

  if Leitor.rExtrai(1, 'total') <> '' then
  begin
    NF3e.Total.vProd      := Leitor.rCampo(tcDe2, 'vProd');
    NF3e.Total.vBC        := Leitor.rCampo(tcDe2, 'vBC');
    NF3e.Total.vICMS      := Leitor.rCampo(tcDe2, 'vICMS');
    NF3e.Total.vICMSDeson := Leitor.rCampo(tcDe2, 'vICMSDeson');
    NF3e.Total.vFCP       := Leitor.rCampo(tcDe2, 'vFCP');
    NF3e.Total.vBCST      := Leitor.rCampo(tcDe2, 'vBCST');
    NF3e.Total.vST        := Leitor.rCampo(tcDe2, 'vST');
    NF3e.Total.vFCPST     := Leitor.rCampo(tcDe2, 'vFCPST');
    NF3e.Total.vCOFINS    := Leitor.rCampo(tcDe2, 'vCOFINS');
    NF3e.Total.vPIS       := Leitor.rCampo(tcDe2, 'vPIS');
    NF3e.Total.vNF        := Leitor.rCampo(tcDe2, 'vNF');
  end;

  if Leitor.rExtrai(1, 'gFat') <> '' then
  begin
    xData := Leitor.rCampo(tcStr, 'CompetFat');
    xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
    NF3e.gFat.CompetFat := StrToDate(xData);

    NF3e.gFat.dVencFat     := Leitor.rCampo(tcDat, 'dVencFat');
    NF3e.gFat.dApresFat    := Leitor.rCampo(tcDat, 'dApresFat');
    NF3e.gFat.dProxLeitura := Leitor.rCampo(tcDat, 'dProxLeitura');
    NF3e.gFat.nFat         := Leitor.rCampo(tcStr, 'nFat');
    NF3e.gFat.codBarras    := Leitor.rCampo(tcStr, 'codBarras');
    NF3e.gFat.codDebAuto   := Leitor.rCampo(tcStr, 'codDebAuto');
    NF3e.gFat.codBanco     := Leitor.rCampo(tcStr, 'codBanco');
    NF3e.gFat.codAgencia   := Leitor.rCampo(tcStr, 'codAgencia');

    if Leitor.rExtrai(2, 'enderCorresp') <> '' then
    begin
      NF3e.gFat.enderCorresp.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      NF3e.gFat.enderCorresp.nro     := Leitor.rCampo(tcStr, 'nro');
      NF3e.gFat.enderCorresp.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      NF3e.gFat.enderCorresp.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      NF3e.gFat.enderCorresp.cMun    := Leitor.rCampo(tcInt, 'cMun');
      NF3e.gFat.enderCorresp.xMun    := Leitor.rCampo(tcStr, 'xMun');
      NF3e.gFat.enderCorresp.CEP     := Leitor.rCampo(tcInt, 'CEP');
      NF3e.gFat.enderCorresp.UF      := Leitor.rCampo(tcStr, 'UF');
      NF3e.gFat.enderCorresp.fone    := Leitor.rCampo(tcStr, 'fone');
      NF3e.gFat.enderCorresp.email   := Leitor.rCampo(tcStr, 'email');
    end;
  end;

  if Leitor.rExtrai(1, 'gANEEL') <> '' then
  begin
    i := 0;

    NF3e.gANEEL.gHistFat.Clear;
    while Leitor.rExtrai(2, 'gHistFat', '', i + 1) <> '' do
    begin
      NF3e.gANEEL.gHistFat.New;
      NF3e.gANEEL.gHistFat[i].xGrandFat := Leitor.rCampo(tcStr, 'xGrandFat');

      j := 0;

      NF3e.gANEEL.gHistFat[i].gGrandFat.Clear;
      while Leitor.rExtrai(3, 'gGrandFat', '', j + 1) <> '' do
      begin
        NF3e.gANEEL.gHistFat[i].gGrandFat.New;

        xData := Leitor.rCampo(tcStr, 'CompetFat');
        xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
        NF3e.gANEEL.gHistFat[i].gGrandFat[j].CompetFat := StrToDate(xData);

        NF3e.gANEEL.gHistFat[i].gGrandFat[j].vFat    := Leitor.rCampo(tcDe2, 'vFat');
        NF3e.gANEEL.gHistFat[i].gGrandFat[j].uMed    := StrTouMedFat(ok, Leitor.rCampo(tcStr, 'uMed'));
        NF3e.gANEEL.gHistFat[i].gGrandFat[j].qtdDias := Leitor.rCampo(tcInt, 'qtdDias');

        inc(j);
      end;

      inc(i);
    end;
  end;

  i := 0;
  NF3e.autXML.Clear;
  while Leitor.rExtrai(1, 'autXML', '', i + 1) <> '' do
  begin
    NF3e.autXML.New;
    NF3e.autXML[i].CNPJCPF := Leitor.rCampoCNPJCPF;

    inc(i);
  end;

  if Leitor.rExtrai(1, 'infAdic') <> '' then
  begin
    NF3e.InfAdic.infAdFisco := Leitor.rCampo(tcStr, 'infAdFisco');
    NF3e.InfAdic.infCpl     := Leitor.rCampo(tcStr, 'infCpl');
  end;

  if Leitor.rExtrai(1, 'infRespTec') <> '' then
  begin
    NF3e.infRespTec.CNPJ     := Leitor.rCampo(tcStr, 'CNPJ');
    NF3e.infRespTec.xContato := Leitor.rCampo(tcStr, 'xContato');
    NF3e.infRespTec.email    := Leitor.rCampo(tcStr, 'email');
    NF3e.infRespTec.fone     := Leitor.rCampo(tcStr, 'fone');
    NF3e.infRespTec.idCSRT   := Leitor.rCampo(tcInt, 'idCSRT');
    NF3e.infRespTec.hashCSRT := Leitor.rCampo(tcStr, 'hashCSRT');
  end;

  (* Grupo da TAG <signature> *************************************************)

  leitor.Grupo := Leitor.Arquivo;

  NF3e.signature.URI             := Leitor.rAtributo('Reference URI=');
  NF3e.signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
  NF3e.signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
  NF3e.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');

  if Leitor.rExtrai(1, 'infNF3eSupl') <> '' then
  begin
    NF3e.infNF3eSupl.qrCodNF3e := Leitor.rCampo(tcStr, 'qrCodNF3e');
    NF3e.infNF3eSupl.qrCodNF3e := StringReplace(NF3e.infNF3eSupl.qrCodNF3e, '<![CDATA[', '', []);
    NF3e.infNF3eSupl.qrCodNF3e := StringReplace(NF3e.infNF3eSupl.qrCodNF3e, ']]>', '', []);
  end;

  if Leitor.rExtrai(1, 'protNF3e') <> '' then
  begin
    NF3e.procNF3e.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
    NF3e.procNF3e.verAplic := Leitor.rCampo(tcStr, 'verAplic');
    NF3e.procNF3e.chNF3e   := Leitor.rCampo(tcStr, 'chNF3e');
    NF3e.procNF3e.dhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
    NF3e.procNF3e.nProt    := Leitor.rCampo(tcStr, 'nProt');
    NF3e.procNF3e.digVal   := Leitor.rCampo(tcStr, 'digVal');
    NF3e.procNF3e.cStat    := Leitor.rCampo(tcInt, 'cStat');
    NF3e.procNF3e.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
    NF3e.procNF3e.cMsg     := Leitor.rCampo(tcInt, 'cMsg');
    NF3e.procNF3e.xMsg     := Leitor.rCampo(tcStr, 'xMsg');
  end;

  Result := true;
end;

end.

