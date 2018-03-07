{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 04/12/2017: Renato Rubinho
|*  - Implementados registros que faltavam e isoladas as respectivas classes 
*******************************************************************************}

unit pcnReinfR2070;

interface

uses
  Classes, Sysutils, pcnGerador, pcnConversaoReinf, ACBrReinfEventosBase,
  pcnReinfClasses, pcnReinfR2070_Class;

type

  TR2070 = class(TEventoReinfR)
  private
    FideBenef: TideBenef;
  protected
    procedure GerarEventoXML; override;
    procedure GerarinfoideBenef;
    procedure GerarinfoResidExt;
    procedure GerarinfoEnder;
    procedure GerarinfoFiscal;
    procedure GerarinfoMolestia;
    procedure GerarinfoPgto;
    procedure GerarideEstab(Items: TideEstabs);
    procedure GerarpgtoResidBR(Item: TpgtoResidBR);
    procedure GerarpgtoPF(Items: TpgtoPFs);
    procedure GerardetDeducao(Items: TdetDeducoes);
    procedure GerarrendIsento(Items: TrendIsentos);
    procedure GerardetCompet(Items: TdetCompets);
    procedure GerarcompJud(Item: TcompJud);
    procedure GerarinfoRRA(Items: TinfoRRAs);
    procedure GerardespProcJud(Item: TdespProcJud);
    procedure GerarideAdvogado(Items: TideAdvogados);
    procedure GerarinfoinfoProcJud(Items: TinfoProcJuds);
    procedure GerarorigemRecursos(Item: TorigemRecursos);
    procedure GerardepJudicial(Item: TdepJudicial);
    procedure GerarpgtoPJ(Items: TpgtoPJs);
    procedure GerarpgtoResidExt(Item: TpgtoResidExt);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

uses
  pcnAuxiliar, ACBrUtil, pcnConversao, DateUtils;

{ TR2070 }

procedure TR2070.AfterConstruction;
begin
  inherited;
  SetSchema(schevtPgtosDivs);
  FideBenef := TideBenef.Create;
end;

procedure TR2070.BeforeDestruction;
begin
  inherited;
  FideBenef.Free;
end;

procedure TR2070.GerarEventoXML;
begin
  GerarinfoideBenef;
end;

procedure TR2070.GerarinfoideBenef;
begin
  Gerador.wGrupo('ideBenef');

  Gerador.wCampo(tcStr, '', 'codPgto',      1,   4, 1, Self.FideBenef.codPgto);
  Gerador.wCampo(tcStr, '', 'tpInscBenef',  1,   1, 0, TpInscricaoToStr( Self.FideBenef.tpInscBenef ));
  Gerador.wCampo(tcStr, '', 'nrInscBenef',  1,  14, 0, Self.FideBenef.nrInscBenef);
  Gerador.wCampo(tcStr, '', 'nmRazaoBenef', 1, 150, 1, Self.FideBenef.nmRazaoBenef);

  GerarinfoResidExt;

  Gerador.wGrupo('/ideBenef');
end;

procedure TR2070.GerarinfoResidExt;
begin
  Gerador.wGrupo('infoResidExt');

  GerarinfoEnder;
  GerarinfoFiscal;
  GerarinfoMolestia;

  Gerador.wGrupo('/infoResidExt');
end;

procedure TR2070.GerarinfoEnder;
begin
  Gerador.wGrupo('infoEnder');

  Gerador.wCampo(tcStr, '', 'paisResid', 1,  3, 1, Self.FideBenef.infoResidExt.infoEnder.paisResid);
  Gerador.wCampo(tcStr, '', 'dscLograd', 1, 80, 1, Self.FideBenef.infoResidExt.infoEnder.dscLograd);
  Gerador.wCampo(tcStr, '', 'nrLograd',  1, 10, 0, Self.FideBenef.infoResidExt.infoEnder.nrLograd);
  Gerador.wCampo(tcStr, '', 'complem',   1, 30, 0, Self.FideBenef.infoResidExt.infoEnder.complem);
  Gerador.wCampo(tcStr, '', 'bairro',    1, 60, 0, Self.FideBenef.infoResidExt.infoEnder.bairro);
  Gerador.wCampo(tcStr, '', 'cidade',    1, 30, 0, Self.FideBenef.infoResidExt.infoEnder.cidade);
  Gerador.wCampo(tcStr, '', 'codPostal', 1, 12, 0, Self.FideBenef.infoResidExt.infoEnder.codPostal);

  Gerador.wGrupo('/infoEnder');
end;

procedure TR2070.GerarinfoFiscal;
begin
  Gerador.wGrupo('infoFiscal');

  Gerador.wCampo(tcStr, '', 'indNIF',        1,  1, 1, indNIFToStr( Self.FideBenef.infoResidExt.infoFiscal.indNIF ) );
  Gerador.wCampo(tcStr, '', 'nifBenef',      1, 20, 0, Self.FideBenef.infoResidExt.infoFiscal.nifBenef);
  Gerador.wCampo(tcStr, '', 'relFontePagad', 1,  3, 0, Self.FideBenef.infoResidExt.infoFiscal.relFontePagad);

  Gerador.wGrupo('/infoFiscal');
end;

procedure TR2070.GerarinfoMolestia;
begin
  if ( Self.FideBenef.infoResidExt.infoMolestia.dtLaudo <> 0 ) then
  begin
    Gerador.wGrupo('infoMolestia');

    Gerador.wCampo(tcDat, '', 'dtLaudo', 10, 10, 1, Self.FideBenef.infoResidExt.infoMolestia.dtLaudo);

    Gerador.wGrupo('/infoMolestia');
  end;
end;

procedure TR2070.GerarinfoPgto;
begin
  Gerador.wGrupo('infoPgto');

  GerarideEstab(Self.FideBenef.infoPgto.ideEstabs);

  Gerador.wGrupo('/infoPgto');
end;

procedure TR2070.GerarideEstab(Items: TideEstabs);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('ideEstab');

      Gerador.wCampo(tcStr, '', 'tpInsc', 1,  1, 1, TpInscricaoToStr( tpInsc ));
      Gerador.wCampo(tcStr, '', 'nrInsc', 1, 14, 1, nrInsc);

      GerarpgtoResidBR(pgtoResidBR);
      GerarpgtoResidExt(pgtoResidExt);

      Gerador.wGrupo('/ideEstab');
    end;
end;

procedure TR2070.GerarpgtoResidBR(Item: TpgtoResidBR);
begin
  Gerador.wGrupo('pgtoResidBR');

  GerarpgtoPF(Item.pgtoPFs);
  GerarpgtoPJ(Item.pgtoPJs);

  Gerador.wGrupo('/pgtoResidBR');
end;

procedure TR2070.GerarpgtoPF(Items: TpgtoPFs);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('pgtoPF');

      Gerador.wCampo(tcDat, '', 'dtPgto',            10, 10, 1, dtPgto);
      Gerador.wCampo(tcStr, '', 'indSuspExig',        1,  1, 1, SimNaoToStr(indSuspExig));
      Gerador.wCampo(tcStr, '', 'indDecTerceiro',     1,  1, 1, SimNaoToStr(indDecTerceiro));
      Gerador.wCampo(tcDe2, '', 'vlrRendTributavel',  1, 14, 1, vlrRendTributavel);
      Gerador.wCampo(tcDe2, '', 'vlrIRRF',            1, 14, 1, vlrIRRF);

      GerardetDeducao(detDeducoes);
      GerarrendIsento(rendIsentos);
      GerardetCompet(detCompets);
      GerarcompJud(compJud);
      GerarinfoRRA(infoRRAs);
      GerarinfoinfoProcJud(infoProcJuds);
      GerardepJudicial(depJudicial);

      Gerador.wGrupo('/pgtoPF');
    end;
end;

procedure TR2070.GerardetDeducao(Items: TdetDeducoes);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('detDeducao');

      Gerador.wCampo(tcStr, '', 'indTpDeducao', 1,  1, 1, indTpDeducaoToStr( indTpDeducao ));
      Gerador.wCampo(tcDe2, '', 'vlrDeducao',   1, 14, 1, vlrDeducao);

      Gerador.wGrupo('/detDeducao');
    end;
end;

procedure TR2070.GerarrendIsento(Items: TrendIsentos);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('rendIsento');

      Gerador.wCampo(tcStr, '', 'tpIsencao',      1,   2, 1, tpIsencaoToStr( tpIsencao ));
      Gerador.wCampo(tcDe2, '', 'vlrIsento',      1,  14, 1, vlrIsento);
      Gerador.wCampo(tcStr, '', 'descRendimento', 1, 100, 0, descRendimento);

      Gerador.wGrupo('/rendIsento');
    end;
end;

procedure TR2070.GerardetCompet(Items: TdetCompets);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('detCompet');

      Gerador.wCampo(tcStr, '', 'indPerReferencia',  1,  1, 1, indPerReferenciaToStr( indPerReferencia ));
      Gerador.wCampo(tcStr, '', 'perRefPagto',       7,  7, 1, perRefPagto);
      Gerador.wCampo(tcDe2, '', 'vlrRendTributavel', 1, 14, 1, vlrRendTributavel);

      Gerador.wGrupo('/detCompet');
    end;
end;

procedure TR2070.GerarcompJud(Item: TcompJud);
begin
  if ( ( Item.vlrCompAnoCalend <> 0 ) or ( Item.vlrCompAnoAnt <> 0 ) ) then
  begin
    Gerador.wGrupo('compJud');

    Gerador.wCampo(tcDe2, '', 'vlrCompAnoCalend', 1, 14, 0, Item.vlrCompAnoCalend);
    Gerador.wCampo(tcDe2, '', 'vlrCompAnoAnt',    1, 14, 0, Item.vlrCompAnoAnt);

    Gerador.wGrupo('/compJud');
  end;
end;

procedure TR2070.GerarinfoRRA(Items: TinfoRRAs);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('infoRRA');

      Gerador.wCampo(tcStr, '', 'tpProcRRA',   1,  1, 0, TpProcToStr( tpProcRRA ));
      Gerador.wCampo(tcStr, '', 'nrProcRRA',   1, 21, 0, nrProcRRA);
      Gerador.wCampo(tcStr, '', 'codSusp',     1, 14, 0, codSusp);
      Gerador.wCampo(tcStr, '', 'natRRA',      1, 50, 0, natRRA);
      Gerador.wCampo(tcInt, '', 'qtdMesesRRA', 1,  5, 0, qtdMesesRRA);

      GerardespProcJud(despProcJud);

      Gerador.wGrupo('/infoRRA');
    end;
end;

procedure TR2070.GerardespProcJud(Item: TdespProcJud);
begin
  if ( ( Item.vlrDespCustas <> 0 ) or ( Item.vlrDespAdvogados <> 0 ) ) then
  begin
    Gerador.wGrupo('despProcJud');

    Gerador.wCampo(tcDe2, '', 'vlrDespCustas',    1, 14, 1, Item.vlrDespCustas);
    Gerador.wCampo(tcDe2, '', 'vlrDespAdvogados', 1, 14, 1, Item.vlrDespAdvogados);

    GerarideAdvogado(Item.ideAdvogados);

    Gerador.wGrupo('/despProcJud');
  end;
end;

procedure TR2070.GerarideAdvogado(Items: TideAdvogados);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('ideAdvogado');

      Gerador.wCampo(tcStr, '', 'tpInscAdvogado', 1,  1, 1, TpInscricaoToStr( tpInscAdvogado ));
      Gerador.wCampo(tcStr, '', 'nrInscAdvogado', 1, 14, 1, nrInscAdvogado);
      Gerador.wCampo(tcDe2, '', 'vlrAdvogado',    1, 14, 1, vlrAdvogado);

      Gerador.wGrupo('/ideAdvogado');
    end;
end;

procedure TR2070.GerarinfoinfoProcJud(Items: TinfoProcJuds);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('infoProcJud');

      Gerador.wCampo(tcStr, '', 'nrProcJud',         1, 21, 1, nrProcJud);
      Gerador.wCampo(tcStr, '', 'codSusp',           1, 14, 0, codSusp);
      Gerador.wCampo(tcStr, '', 'indOrigemRecursos', 1,  1, 1, indOrigemRecursosToStr( indOrigemRecursos ));

      GerardespProcJud(despProcJud);
      GerarorigemRecursos(origemRecursos);

      Gerador.wGrupo('/infoProcJud');
    end;
end;

procedure TR2070.GerarorigemRecursos(Item: TorigemRecursos);
begin
  if ( Item.cnpjOrigemRecursos <> '' ) then
  begin
    Gerador.wGrupo('origemRecursos');

    Gerador.wCampo(tcStr, '', 'cnpjOrigemRecursos', 14, 14, 1, Item.cnpjOrigemRecursos);

    Gerador.wGrupo('/origemRecursos');
  end;
end;

procedure TR2070.GerardepJudicial(Item: TdepJudicial);
begin
  if ( Item.vlrDepJudicial <> 0 ) then
  begin
    Gerador.wGrupo('depJudicial');

    Gerador.wCampo(tcDe2, '', 'vlrDepJudicial', 1, 14, 0, Item.vlrDepJudicial);

    Gerador.wGrupo('/depJudicial');
  end;
end;

procedure TR2070.GerarpgtoPJ(Items: TpgtoPJs);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('pgtoPJ');

      Gerador.wCampo(tcDat, '', 'dtPagto',           10, 10, 1, dtPagto);
      Gerador.wCampo(tcDe2, '', 'vlrRendTributavel',  1, 14, 1, vlrRendTributavel);
      Gerador.wCampo(tcDe2, '', 'vlrRet',             1, 14, 1, vlrRet);

      GerarinfoinfoProcJud(infoProcJuds);

      Gerador.wGrupo('/pgtoPJ');
    end;
end;

procedure TR2070.GerarpgtoResidExt(Item: TpgtoResidExt);
begin
  if ( ( Item.dtPagto <> 0 ) or ( Item.tpRendimento <> '' ) or
       ( Item.formaTributacao <> '' ) or ( Item.vlrPgto <> 0 ) or
       ( Item.vlrRet <> 0 ) ) then
  begin
    Gerador.wGrupo('pgtoResidExt');

    Gerador.wCampo(tcDat, '', 'dtPagto',         10, 10, 1, Item.dtPagto);
    Gerador.wCampo(tcStr, '', 'tpRendimento',     1,  3, 1, Item.tpRendimento);
    Gerador.wCampo(tcStr, '', 'formaTributacao',  1,  2, 1, Item.formaTributacao);
    Gerador.wCampo(tcDe2, '', 'vlrPgto',          1, 14, 1, Item.vlrPgto);
    Gerador.wCampo(tcDe2, '', 'vlrRet',           1, 14, 1, Item.vlrRet);

    Gerador.wGrupo('/pgtoResidExt');
  end;
end;

end.
