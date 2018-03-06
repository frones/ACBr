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
|* 24/10/2017: Renato Rubinho
|*  - Compatibilizado Fonte com Delphi 7
*******************************************************************************}

unit pcnReinfR2020;

interface

uses
  Classes, Sysutils, pcnGerador, pcnConversaoReinf, ACBrReinfEventosBase,
  pcnReinfClasses, pcnReinfR2010_Class, pcnReinfR2020_Class;

type
  TR2020 = class(TEventoReinfRet)
  private
    FinfoServPrest: TinfoServPrest;
  protected
    procedure GerarEventoXML; override;
    procedure GerarideEstabPrest;
    procedure GerarideTomador(Items: TideTomadors);
    procedure GerarinfoProcRetPr(Items: TinfoProcRetPrs);
    procedure GerarinfoProcRetAd(Items: TinfoProcRetPrs);
    procedure GerarNFs(Items: Tnfss);
    procedure GerarinfoTpServ(Items: TinfoTpServs);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property infoServPrest: TinfoServPrest read FinfoServPrest;
  end;

implementation

uses
  pcnAuxiliar, ACBrUtil, pcnConversao, DateUtils;

{ TR2020 }

procedure TR2020.AfterConstruction;
begin
  inherited;
  SetSchema(schevtServPrest);
  FinfoServPrest := TinfoServPrest.Create;
end;

procedure TR2020.BeforeDestruction;
begin
  inherited;
  FinfoServPrest.Free;
end;

procedure TR2020.GerarEventoXML;
begin
  Gerador.wGrupo('infoServPrest');

  GerarideEstabPrest;

  Gerador.wGrupo('/infoServPrest');
end;

procedure TR2020.GerarideEstabPrest;
begin
  Gerador.wGrupo('ideEstabPrest');

  Gerador.wCampo(tcStr, '', 'tpInscEstabPrest', 1,  1, 1, TpInscricaoToStr( Self.FinfoServPrest.ideEstabPrest.tpInscEstabPrest ) );
  Gerador.wCampo(tcStr, '', 'nrInscEstabPrest', 1, 14, 1, Self.FinfoServPrest.ideEstabPrest.nrInscEstabPrest);

  GerarideTomador(Self.FinfoServPrest.ideEstabPrest.ideTomadors);

  Gerador.wGrupo('/ideEstabPrest');
end;

procedure TR2020.GerarideTomador(Items: TideTomadors);
var
  item: TideTomador;
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
  begin
    Item := Items.Items[i];

    Gerador.wGrupo('ideTomador');

    Gerador.wCampo(tcStr, '', 'tpInscTomador',     1,  1, 1, TpInscricaoToStr(item.tpInscTomador));
    Gerador.wCampo(tcStr, '', 'nrInscTomador',     1, 14, 1, item.nrInscTomador);
    Gerador.wCampo(tcStr, '', 'indObra',           1,  1, 1, indObraToStr(item.indObra));
    Gerador.wCampo(tcDe2, '', 'vlrTotalBruto',     1, 14, 1, item.vlrTotalBruto);
    Gerador.wCampo(tcDe2, '', 'vlrTotalBaseRet',   1, 14, 1, item.vlrTotalBaseRet);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRetPrinc',  1, 14, 1, item.vlrTotalRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRetAdic',   1, 14, 0, item.vlrTotalRetAdic);
    Gerador.wCampo(tcDe2, '', 'vlrTotalNRetPrinc', 1, 14, 0, item.vlrTotalNRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrTotalNRetAdic',  1, 14, 0, item.vlrTotalNRetAdic);

    GerarNFs(item.nfss);
    GerarinfoProcRetPr(item.infoProcRetPr);
    GerarinfoProcRetAd(item.infoProcRetAd);

    Gerador.wGrupo('/ideTomador');
  end;
end;

procedure TR2020.GerarinfoProcRetAd(Items: TinfoProcRetPrs);
var
  Item: TinfoProcRetPr;
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
  begin
    Item := Items.Items[i];

    Gerador.wGrupo('infoProcRetAd');

    Gerador.wCampo(tcStr, '', 'tpProcRetPrinc', 1,  1, 1, TpProcToStr(item.tpProcRetPrinc));
    Gerador.wCampo(tcStr, '', 'nrProcRetPrinc', 1, 21, 1, item.nrProcRetPrinc);
    Gerador.wCampo(tcInt, '', 'codSuspPrinc',   1, 14, 0, item.codSuspPrinc);
    Gerador.wCampo(tcDe2, '', 'valorPrinc',     1, 14, 1, item.valorPrinc);

    Gerador.wGrupo('/infoProcRetAd');
  end;
end;

procedure TR2020.GerarinfoProcRetPr(Items: TinfoProcRetPrs);
var
  Item: TinfoProcRetPr;
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
  begin
    Item := Items.Items[i];

    Gerador.wGrupo('infoProcRetPr');

    Gerador.wCampo(tcStr, '', 'tpProcRetPrinc', 1,  1, 1, TpProcToStr(item.tpProcRetPrinc));
    Gerador.wCampo(tcStr, '', 'nrProcRetPrinc', 1, 21, 1, item.nrProcRetPrinc);
    Gerador.wCampo(tcInt, '', 'codSuspPrinc',   1, 14, 0, item.codSuspPrinc);
    Gerador.wCampo(tcDe2, '', 'valorPrinc',     1, 14, 1, item.valorPrinc);

    Gerador.wGrupo('/infoProcRetPr');
  end;
end;

procedure TR2020.GerarinfoTpServ(Items: TinfoTpServs);
var
  Item: TinfoTpServ;
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
  begin
    Item := Items.Items[i];

    Gerador.wGrupo('infoTpServ');

    Gerador.wCampo(tcStr, '', 'tpServico',     1,  9, 1, item.tpServico);
    Gerador.wCampo(tcDe2, '', 'vlrBaseRet',    1, 14, 1, item.vlrBaseRet);
    Gerador.wCampo(tcDe2, '', 'vlrRetencao',   1, 14, 1, item.vlrRetencao);
    Gerador.wCampo(tcDe2, '', 'vlrRetSub',     1, 14, 0, item.vlrRetSub);
    Gerador.wCampo(tcDe2, '', 'vlrNRetPrinc',  1, 14, 0, item.vlrNRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrServicos15', 1, 14, 0, item.vlrServicos15);
    Gerador.wCampo(tcDe2, '', 'vlrServicos20', 1, 14, 0, item.vlrServicos20);
    Gerador.wCampo(tcDe2, '', 'vlrServicos25', 1, 14, 0, item.vlrServicos25);
    Gerador.wCampo(tcDe2, '', 'vlrAdicional',  1, 14, 0, item.vlrAdicional);
    Gerador.wCampo(tcDe2, '', 'vlrNRetAdic',   1, 14, 0, item.vlrNRetAdic);

    Gerador.wGrupo('/infoTpServ');
  end;
end;

procedure TR2020.GerarNFs(Items: Tnfss);
var
  item: TNFs;
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
  begin
    Item := Items.Items[i];

    Gerador.wGrupo('nfs');

    Gerador.wCampo(tcStr, '', 'serie',        1,   5, 1, item.serie);
    Gerador.wCampo(tcStr, '', 'numDocto',     1,  15, 1, item.numDocto);
    Gerador.wCampo(tcDat, '', 'dtEmissaoNF', 10,  10, 1, item.dtEmissaoNF);
    Gerador.wCampo(tcDe2, '', 'vlrBruto',     1,  14, 1, item.vlrBruto);
    Gerador.wCampo(tcStr, '', 'obs',          1, 250, 0, item.obs);

    GerarinfoTpServ(item.infoTpServs);

    Gerador.wGrupo('/nfs');
  end;
end;

end.
