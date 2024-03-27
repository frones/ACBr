{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
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

unit pcnCFeR;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor, pcnCFe;

type

  TCFeR = class(TPersistent)
  private
    FLeitor: TLeitor;
    FCFe: TCFe;
  public
    constructor Create(AOwner: TCFe);
    destructor Destroy; override;
    function LerXml: boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property CFe: TCFe read FCFe write FCFe;
  end;

implementation

uses
  ACBrConsts, ACBrUtil.Base, ACBrUtil.Strings;

{ TCFeR }

constructor TCFeR.Create(AOwner: TCFe);
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FCFe := AOwner;
end;

destructor TCFeR.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

function TCFeR.LerXml: boolean;
var
  ok, XMLContemTagsSAT: boolean;
  i, j, nItem: integer;
  Arquivo, Itens, ItensTemp, NumItem, ACampo: String;
  Aspas: String;
begin
  if Pos('versao=''', Leitor.Arquivo) <> 0 then
    Aspas := ''''
   else
    Aspas := '"';

  XMLContemTagsSAT := (Pos('<nserieSAT>', Leitor.Arquivo) > 0);

  { Se contem apenas as Tags da aplicação (não contém as Tags geradas pelo SAT),
    então não faz o "CFe.Clear", para evitar apagar dados padroes, definidos em
    "TACBrSAT.InicializaCFe" }

  if XMLContemTagsSAT then
    CFe.Clear;

  if Leitor.rExtrai(1, 'infCFe') <> '' then
  begin
    CFe.infCFe.ID             := Leitor.rAtributo( 'Id' ) ;
    CFe.infCFe.ID             := StringReplace( UpperCase(CFe.infCFe.ID), 'CFE', '', [rfReplaceAll] ) ;
    CFe.infCFe.versao         := StringToFloatDef(Leitor.rAtributo( 'versao' ), CFe.infCFe.versao) ;
    CFe.infCFe.versaoSB       := StrToIntDef(Leitor.rAtributo( 'versaoSB' ), CFe.infCFe.versaoSB) ;
    CFe.infCFe.versaoDadosEnt := StringToFloatDef(Leitor.rAtributo( 'versaoDadosEnt' ), CFe.infCFe.versaoDadosEnt) ;
  end ;
                                            
  (* Grupo da TAG <ide> *******************************************************)
  if Leitor.rExtrai(1, 'ide') <> '' then
  begin
    ok := False;
    (*B02*) CFe.ide.cUF := Leitor.rCampo(tcInt, 'cUF');
    (*B03*) CFe.ide.cNF := Leitor.rCampo(tcInt, 'cNF');
    ACampo := Leitor.rCampo(tcInt, 'mod');
    if (ACampo <> '') then
      (*B04*) CFe.ide.modelo := StrToIntDef( ACampo, CFe.ide.modelo);

    (*B05*) CFe.ide.nserieSAT := Leitor.rCampo(tcInt, 'nserieSAT');
    (*B06*) CFe.ide.nCFe := Leitor.rCampo(tcInt, 'nCFe');
    (*B07*) CFe.ide.dEmi := Leitor.rCampo(tcDatCFe, 'dEmi');
    (*B08*) CFe.ide.hEmi := Leitor.rCampo(tcHorCFe, 'hEmi');
    (*B09*) CFe.Ide.cDV := Leitor.rCampo(tcInt, 'cDV');
    (*B10*) CFe.Ide.tpAmb := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
    (*B11*) CFe.Ide.CNPJ := Leitor.rCampo(tcEsp, 'CNPJ');
    (*B12*) CFe.Ide.signAC := Leitor.rCampo(tcStr, 'signAC');
    (*B13*) CFe.Ide.assinaturaQRCODE := Leitor.rCampo(tcStr, 'assinaturaQRCODE');
    (*B14*) CFe.ide.numeroCaixa := Leitor.rCampo(tcInt, 'numeroCaixa');
  end;

  (* Grupo da TAG <emit> ******************************************************)
  if Leitor.rExtrai(1, 'emit') <> '' then
  begin
    (*C02/C02a*)CFe.Emit.CNPJ := IfEmptyThen(Leitor.rCampoCNPJCPF, CFe.Emit.CNPJ);
    (*C03*)CFe.Emit.xNome := IfEmptyThen(Leitor.rCampo(tcStr, 'xNome'), CFe.Emit.xNome);
    (*C04*)CFe.Emit.xFant := IfEmptyThen(Leitor.rCampo(tcStr, 'xFant'), CFe.Emit.xFant);
    (*C12*)CFe.Emit.IE := IfEmptyThen(Leitor.rCampo(tcStr, 'IE'), CFe.Emit.IE);
    (*C13*)CFe.Emit.IM := IfEmptyThen(Leitor.rCampo(tcStr, 'IM'), CFe.Emit.IM);

    ACampo := Leitor.rCampo(tcStr, 'cRegTrib');
    if (ACampo <> '') then
      (*C14*)CFe.Emit.cRegTrib := StrToRegTrib(ok, ACampo);

    ACampo := Leitor.rCampo(tcStr, 'cRegTribISSQN');
    if (ACampo <> '') then
      (*C15*)CFe.Emit.cRegTribISSQN := StrToRegTribISSQN(ok, ACampo);

    ACampo := Leitor.rCampo(tcStr, 'indRatISSQN');
    if (ACampo <> '') then
      (*C16*)CFe.Emit.indRatISSQN := StrToindRatISSQN(ok, ACampo);

    if Leitor.rExtrai(2, 'enderEmit') <> '' then
    begin
      (*C06*)CFe.Emit.enderEmit.xLgr := Leitor.rCampo(tcStr, 'xLgr');
      (*C07*)CFe.Emit.enderEmit.nro := Leitor.rCampo(tcStr, 'nro');
      (*C08*)CFe.Emit.enderEmit.xCpl := Leitor.rCampo(tcStr, 'xCpl');
      (*C09*)CFe.Emit.enderEmit.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      (*C10*)CFe.Emit.enderEmit.xMun := Leitor.rCampo(tcStr, 'xMun');
      (*C11*)CFe.Emit.enderEmit.CEP := Leitor.rCampo(tcInt, 'CEP');
    end;
  end;

  (* Grupo da TAG <dest> ******************************************************)
  if Leitor.rExtrai(1, 'dest') <> '' then
  begin
    (*E02/E03*)CFe.Dest.CNPJCPF := Leitor.rCampoCNPJCPF;
    (*E04*)CFe.Dest.xNome := Leitor.rCampo(tcStr, 'xNome');
  end;

  (* Grupo da TAG <entrega> ***************************************************)
  if Leitor.rExtrai(1, 'entrega') <> '' then
  begin
    (*G02*)CFe.Entrega.xLgr := Leitor.rCampo(tcStr, 'xLgr');
    (*G03*)CFe.Entrega.nro := Leitor.rCampo(tcStr, 'nro');
    (*G04*)CFe.Entrega.xCpl := Leitor.rCampo(tcStr, 'xCpl');
    (*G05*)CFe.Entrega.xBairro := Leitor.rCampo(tcStr, 'xBairro');
    (*G06*)CFe.Entrega.xMun := Leitor.rCampo(tcStr, 'xMun');
    (*G07*)CFe.Entrega.UF := Leitor.rCampo(tcStr, 'UF');
  end;

  (* Grupo da TAG <det> *******************************************************)
  i := 0;
  Arquivo := Leitor.Arquivo;

  Itens := copy(
    Arquivo,
    Pos('<det nItem=', Arquivo),
    Pos('<total', Arquivo) - Pos('<det nItem=',Arquivo)
  );

  ItensTemp := copy(
    Itens,
    Pos('<det nItem=', Itens),
    (Pos('</det>', Itens) + 6) - Pos('<det nItem=', Itens)
  );  

  while pos('<det nItem=',ItensTemp) <> 0 do
  begin
    Leitor.Arquivo := 'Item '+ItensTemp;

    NumItem   := copy(ItensTemp,Pos('nItem=',ItensTemp)+7,Pos(Aspas,ItensTemp));
    NumItem   := copy(NumItem,1,Pos(Aspas,NumItem)-1);
    nItem     := StrToInt(NumItem);
    Itens     := StringReplace(Itens, ItensTemp, '',[]);
    ItensTemp := copy(Itens,Pos('<det nItem=',Itens),(Pos('</det>',Itens)+6)-Pos('<det nItem=',Itens));

    Leitor.rExtrai(1, 'det nItem=' + Aspas + NumItem + Aspas, 'det');
    CFe.Det.New;
    (*   *)CFe.Det[i].nItem := nItem;
    (*V01*)CFe.Det[i].infAdProd := Leitor.rCampo(tcStr, 'infAdProd');

    (* Grupo da TAG <det><prod> *)
    Leitor.rExtrai(2, 'prod');
    (*I02*)CFe.Det[i].Prod.cProd := Leitor.rCampo(tcStr, 'cProd');
    (*I03*)CFe.Det[i].Prod.cEAN := Leitor.rCampo(tcStr, 'cEAN');
    (*I04*)CFe.Det[i].Prod.xProd := Leitor.rCampo(tcStr, 'xProd');
    (*I05*)CFe.Det[i].Prod.NCM := Leitor.rCampo(tcStr, 'NCM');
    if CFe.infCFe.versao >= 0.08 then
      (*I05w*)CFe.Det[i].Prod.CEST := Leitor.rCampo(tcStr, 'CEST');
    (*I06*)CFe.Det[i].Prod.CFOP := Leitor.rCampo(tcEsp, 'CFOP');
    (*I07*)CFe.Det[i].Prod.uCom := Leitor.rCampo(tcStr, 'uCom');
    (*I08*)CFe.Det[i].Prod.qCom := Leitor.rCampo(tcDe4, 'qCom');
    (*I09*)CFe.Det[i].Prod.vUnCom := Leitor.rCampo(tcDe10, 'vUnCom');
    (*I10*)CFe.Det[i].Prod.vProd := Leitor.rCampo(tcDe2, 'vProd');
    (*I11*)CFe.Det[i].Prod.indRegra := StrToindRegra(ok,Leitor.rCampo(tcStr, 'indRegra'));
    (*I12*)CFe.Det[i].Prod.vDesc := Leitor.rCampo(tcDe2, 'vDesc');
    (*I13*)CFe.Det[i].Prod.vOutro := Leitor.rCampo(tcDe2, 'vOutro');
    (*I14*)CFe.Det[i].Prod.vItem := Leitor.rCampo(tcDe2, 'vItem');
    (*I15*)CFe.Det[i].Prod.vRatDesc := Leitor.rCampo(tcDe2, 'vRatDesc');
    (*I16*)CFe.Det[i].Prod.vRatAcr := Leitor.rCampo(tcDe2, 'vRatAcr');

    CFe.Det[i].Prod.EhCombustivel := (CFe.Det[i].Prod.indRegra = irTruncamento) ;

    (* Grupo da TAG <det><prod><obsFiscoDet> *)
    j := 0;
    while Leitor.rExtrai(3, 'obsFiscoDet', '', j + 1) <> '' do
    begin
      CFe.Det[i].Prod.obsFiscoDet.New;
      (*I18*)CFe.Det[i].Prod.obsFiscoDet[j].xCampoDet := Leitor.rAtributo('xCampoDet');
      (*I19*)CFe.Det[i].Prod.obsFiscoDet[j].xTextoDet := Leitor.rCampo(tcStr, 'xTextoDet');
      inc(j);
    end;

    (*I20*)CFe.Det[i].Prod.cANP := Leitor.rCampo(tcInt64, 'cANP');

    (* Grupo da TAG <det><imposto> ********************************************)
    Leitor.rExtrai(2, 'imposto');
    (*M02*)CFe.Det[i].Imposto.vItem12741 := Leitor.rCampo(tcDe2, 'vItem12741');
    
    if Leitor.rExtrai(3, 'ICMS') <> '' then
    begin
      (*N06*)CFe.Det[i].Imposto.ICMS.orig         := StrToOrig(ok, Leitor.rCampo(tcStr, 'Orig'));
      (*N07*)CFe.Det[i].Imposto.ICMS.CST          := StrToCSTICMS(ok, Leitor.rCampo(tcStr, 'CST'));
      (*N10*)CFe.Det[i].Imposto.ICMS.CSOSN        := StrToCSOSNIcms(ok, Leitor.rCampo(tcInt, 'CSOSN'));
      (*N08*)CFe.Det[i].Imposto.ICMS.pICMS        := Leitor.rCampo(tcDe2, 'pICMS');
      (*N09*)CFe.Det[i].Imposto.ICMS.vICMS        := Leitor.rCampo(tcDe2, 'vICMS');
    end;

    if Leitor.rExtrai(3, 'PIS') <> '' then
    begin
      (*Q07*)CFe.Det[i].Imposto.PIS.CST := StrToCSTPIS(ok, Leitor.rCampo(tcStr, 'CST'));
      (*Q08*)CFe.Det[i].Imposto.PIS.vBC := Leitor.rCampo(tcDe2, 'vBC');
      (*Q09*)CFe.Det[i].Imposto.PIS.pPIS := Leitor.rCampo(tcDe4, 'pPIS');
      (*Q10*)CFe.Det[i].Imposto.PIS.vPIS := Leitor.rCampo(tcDe2, 'vPIS');
      (*Q11*)CFe.Det[i].Imposto.PIS.qBCProd := Leitor.rCampo(tcDe4, 'qBCProd');
      (*Q12*)CFe.Det[i].Imposto.PIS.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
    end;

    if Leitor.rExtrai(3, 'PISST') <> '' then
    begin
      (*R02*)CFe.Det[i].Imposto.PISST.vBc := Leitor.rCampo(tcDe2, 'vBC');
      (*R03*)CFe.Det[i].Imposto.PISST.pPis := Leitor.rCampo(tcDe4, 'pPIS');
      (*R04*)CFe.Det[i].Imposto.PISST.qBCProd := Leitor.rCampo(tcDe4, 'qBCProd');
      (*R05*)CFe.Det[i].Imposto.PISST.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
      (*R06*)CFe.Det[i].Imposto.PISST.vPIS := Leitor.rCampo(tcDe2, 'vPIS');
    end;

    if Leitor.rExtrai(3, 'COFINS') <> '' then
    begin
      (*S07*)CFe.Det[i].Imposto.COFINS.CST := StrToCSTCOFINS(ok, Leitor.rCampo(tcStr, 'CST'));
      (*S08*)CFe.Det[i].Imposto.COFINS.vBC := Leitor.rCampo(tcDe2, 'vBC');
      (*S09*)CFe.Det[i].Imposto.COFINS.pCOFINS := Leitor.rCampo(tcDe4, 'pCOFINS');
      (*S11*)CFe.Det[i].Imposto.COFINS.qBCProd := Leitor.rCampo(tcDe4, 'qBCProd');
      (*S12*)CFe.Det[i].Imposto.COFINS.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
      (*S10*)CFe.Det[i].Imposto.COFINS.vCOFINS := Leitor.rCampo(tcDe2, 'vCOFINS');
    end;

    if Leitor.rExtrai(3, 'COFINSST') <> '' then
    begin
      (*T02*)CFe.Det[i].Imposto.COFINSST.vBC := Leitor.rCampo(tcDe2, 'vBC');
      (*T03*)CFe.Det[i].Imposto.COFINSST.pCOFINS := Leitor.rCampo(tcDe4, 'pCOFINS');
      (*T04*)CFe.Det[i].Imposto.COFINSST.qBCProd := Leitor.rCampo(tcDe4, 'qBCProd');
      (*T05*)CFe.Det[i].Imposto.COFINSST.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
      (*T06*)CFe.Det[i].Imposto.COFINSST.vCOFINS := Leitor.rCampo(tcDe2, 'vCOFINS');
    end;

    if Leitor.rExtrai(3, 'ISSQN') <> '' then
    begin
      (*U02*)CFe.Det[i].Imposto.ISSQN.vDeducISSQN  := Leitor.rCampo(tcDe2, 'vDeducISSQN');
      (*U03*)CFe.Det[i].Imposto.ISSQN.vBC       := Leitor.rCampo(tcDe2, 'vBC');
      (*U04*)CFe.Det[i].Imposto.ISSQN.vAliq     := Leitor.rCampo(tcDe2, 'vAliq');
      (*U05*)CFe.Det[i].Imposto.ISSQN.vISSQN    := Leitor.rCampo(tcDe2, 'vISSQN');
      (*U06*)CFe.Det[i].Imposto.ISSQN.cMunFG    := Leitor.rCampo(tcInt, 'cMunFG');
      (*U07*)CFe.Det[i].Imposto.ISSQN.cListServ := Leitor.rCampo(tcStr, 'cListServ');
      (*U08*)CFe.Det[i].Imposto.ISSQN.cServTribMun := Leitor.rCampo(tcStr, 'cServTribMun') ;
      (*U09*)CFe.Det[i].Imposto.ISSQN.cNatOp    := Leitor.rCampo(tcInt, 'cNatOp');
      (*U10*)CFe.Det[i].Imposto.ISSQN.indIncFisc    := StrToindIncentivo(ok, Leitor.rCampo(tcInt, 'indIncFisc'));
    end;

    inc(i);
  end;

  Leitor.Arquivo := Arquivo;

  (* Grupo da TAG <total> *****************************************************)
  if Leitor.rExtrai(1, 'total') <> '' then
  begin
    (*W11*)CFe.Total.vCFe := Leitor.rCampo(tcDe2, 'vCFe');
    (*W22*)CFe.Total.vCFeLei12741 := Leitor.rCampo(tcDe2, 'vCFeLei12741');    

    if Leitor.rExtrai(2, 'ICMSTot') <> '' then
    begin
      (*W03*)CFe.Total.ICMSTot.vICMS := Leitor.rCampo(tcDe2, 'vICMS');
      (*W04*)CFe.Total.ICMSTot.vProd := Leitor.rCampo(tcDe2, 'vProd');
      (*W05*)CFe.Total.ICMSTot.vDesc := Leitor.rCampo(tcDe2, 'vDesc');
      (*W06*)CFe.Total.ICMSTot.vPIS := Leitor.rCampo(tcDe2, 'vPIS');
      (*W07*)CFe.Total.ICMSTot.vCOFINS := Leitor.rCampo(tcDe2, 'vCOFINS');
      (*W08*)CFe.Total.ICMSTot.vPISST := Leitor.rCampo(tcDe2, 'vPISST');
      (*W09*)CFe.Total.ICMSTot.vCOFINSST := Leitor.rCampo(tcDe2, 'vCOFINSST');
      (*W10*)CFe.Total.ICMSTot.vOutro := Leitor.rCampo(tcDe2, 'vOutro');
    end;

    if Leitor.rExtrai(2, 'ISSQNtot') <> '' then
    begin
      (*W13*)CFe.Total.ISSQNtot.vBC := Leitor.rCampo(tcDe2, 'vBC');
      (*W14*)CFe.Total.ISSQNtot.vISS := Leitor.rCampo(tcDe2, 'vISS');
      (*W15*)CFe.Total.ISSQNtot.vPIS := Leitor.rCampo(tcDe2, 'vPIS');
      (*W16*)CFe.Total.ISSQNtot.vCOFINS := Leitor.rCampo(tcDe2, 'vCOFINS');
      (*W17*)CFe.Total.ISSQNtot.vPISST := Leitor.rCampo(tcDe2, 'vPISST');
      (*W18*)CFe.Total.ISSQNtot.vCOFINSST := Leitor.rCampo(tcDe2, 'vCOFINSST');
    end;

    if Leitor.rExtrai(2, 'DescAcrEntr') <> '' then
    begin
      (*W20*)CFe.Total.DescAcrEntr.vDescSubtot := Leitor.rCampo(tcDe2, 'vDescSubtot');
      (*W21*)CFe.Total.DescAcrEntr.vAcresSubtot := Leitor.rCampo(tcDe2, 'vAcresSubtot');
    end;
  end;

  (* Grupo da TAG <total> *****************************************************)
  if Leitor.rExtrai(1, 'pgto') <> '' then
  begin
    i := 0;
   (*WA06*)CFe.Pagto.vTroco := Leitor.rCampo(tcDe2, 'vTroco');

    while Leitor.rExtrai(2, 'MP', '', i + 1) <> '' do
    begin
      CFe.Pagto.New;
      (*WA03*)CFe.Pagto[i].cMP := StrToCodigoMP(ok, Leitor.rCampo(tcStr, 'cMP'));
      (*WA04*)CFe.Pagto[i].vMP := Leitor.rCampo(tcDe2, 'vMP');
      (*WA05*)CFe.Pagto[i].cAdmC := Leitor.rCampo(tcInt, 'cAdmC');
      if CFe.infCFe.versao >= 0.09 then
        (*WA07*)CFe.Pagto[i].cAut := Leitor.rCampo(tcStr, 'cAut');
      inc(i);
    end;
  end;

  (* Grupo da TAG <InfAdic> ***************************************************)
  if Leitor.rExtrai(1, 'infAdic') <> '' then
  begin
    (*Z02*)CFe.InfAdic.infCpl := Leitor.rCampo(tcStr, 'infCpl');

    if CFe.infCFe.versao <= 0.07 then
    begin
      i := 0;
      while Leitor.rExtrai(2, 'obsFisco', '', i + 1) <> '' do
      begin
        CFe.InfAdic.obsFisco.New;
        (*Z04*)CFe.InfAdic.obsFisco[i].xCampo := Leitor.rAtributo('xCampo');
        (*Z05*)CFe.InfAdic.obsFisco[i].xTexto := Leitor.rCampo(tcStr, 'xTexto');

        // Incluindo em Tag compatível com 0.08
        CFe.obsFisco.New;
        (*ZA02*)CFe.obsFisco[i].xCampo := CFe.InfAdic.obsFisco[i].xCampo;
        (*ZA03*)CFe.obsFisco[i].xTexto := CFe.InfAdic.obsFisco[i].xTexto;

        inc(i)
      end;
    end;
  end;

  if CFe.infCFe.versao >= 0.08 then
  begin
    i := 0;
    while Leitor.rExtrai(1, 'obsFisco', '', i + 1) <> '' do
    begin
      CFe.obsFisco.New;
      (*ZA02*)CFe.obsFisco[i].xCampo := Leitor.rAtributo('xCampo');
      (*ZA03*)CFe.obsFisco[i].xTexto := Leitor.rCampo(tcStr, 'xTexto');
      inc(i)
    end;
  end;

  (* Grupo da TAG <signature> *************************************************)
  leitor.Grupo := Leitor.Arquivo;

  CFe.signature.URI := Leitor.rAtributo('Reference URI=');
  CFe.signature.DigestValue := Leitor.rCampo(tcStr, 'DigestValue');
  CFe.signature.SignatureValue := Leitor.rCampo(tcStr, 'SignatureValue');
  CFe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');

  Result := True;
end;

end.
