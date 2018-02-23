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
  SetSchema(rsevtServPrest);
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
  Gerador.wCampo(tcInt, '', 'tpInscEstabPrest', 0, 0, 1, Ord( Self.FinfoServPrest.ideEstabPrest.tpInscEstabPrest ) );
  Gerador.wCampo(tcStr, '', 'nrInscEstabPrest', 0, 0, 1, Self.FinfoServPrest.ideEstabPrest.nrInscEstabPrest);
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
    Gerador.wCampo(tcInt, '', 'tpInscTomador', 0, 0, 1, ord(item.tpInscTomador));
    Gerador.wCampo(tcStr, '', 'nrInscTomador', 0, 0, 1, item.nrInscTomador);
    Gerador.wCampo(tcInt, '', 'indObra', 0, 0, 1, ord(item.indObra));
    Gerador.wCampo(tcDe2, '', 'vlrTotalBruto', 0, 0, 1 , item.vlrTotalBruto);
    Gerador.wCampo(tcDe2, '', 'vlrTotalBaseRet', 0, 0, 1, item.vlrTotalBaseRet);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRetPrinc', 0, 0, 1, item.vlrTotalRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRetAdic', 0, 0, 0, item.vlrTotalRetAdic);
    Gerador.wCampo(tcDe2, '', 'vlrTotalNRetPrinc', 0, 0, 0, item.vlrTotalNRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrTotalNRetAdic', 0, 0, 0, item.vlrTotalNRetAdic);
    GerarNFs(item.nfss);
    GerarinfoProcRetPr(item.infoProcRetPrs);
    Gerador.wGrupo('/ideTomador');
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

    Gerador.wGrupo('infoTpServ');
    Gerador.wCampo(tcInt, '', 'tpProcRetPrinc', 0, 0, 1, ord(item.tpProcRetPrinc));
    Gerador.wCampo(tcStr, '', 'nrProcRetPrinc', 0, 0, 1, item.nrProcRetPrinc);
    Gerador.wCampo(tcInt, '', 'codSuspPrinc', 0, 0, 0, item.codSuspPrinc);
    Gerador.wCampo(tcDe2, '', 'valorPrinc', 0, 0, 1, item.valorPrinc);
    Gerador.wGrupo('/infoTpServ');
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
    Gerador.wCampo(tcStr, '', 'tpServico', 0, 0, 1, item.tpServico);
    Gerador.wCampo(tcDe2, '', 'vlrBaseRet', 0, 0, 1, item.vlrBaseRet);
    Gerador.wCampo(tcDe2, '', 'vlrRetencao', 0, 0, 1, item.vlrRetencao);
    Gerador.wCampo(tcDe2, '', 'vlrRetSub', 0, 0, 0, item.vlrRetSub);
    Gerador.wCampo(tcDe2, '', 'vlrNRetPrinc', 0, 0, 0, item.vlrNRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrServicos15', 0, 0, 0, item.vlrServicos15);
    Gerador.wCampo(tcDe2, '', 'vlrServicos20', 0, 0, 0, item.vlrServicos20);
    Gerador.wCampo(tcDe2, '', 'vlrServicos25', 0, 0, 0, item.vlrServicos25);
    Gerador.wCampo(tcDe2, '', 'vlrAdicional', 0, 0, 0, item.vlrAdicional);
    Gerador.wCampo(tcDe2, '', 'vlrNRetAdic', 0, 0, 0, item.vlrNRetAdic);
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
    Gerador.wCampo(tcStr, '', 'serie', 0, 0, 1, item.serie);
    Gerador.wCampo(tcStr, '', 'numDocto', 0, 0, 1, item.numDocto);
    Gerador.wCampo(tcDat, '', 'dtEmissaoNF', 0, 0,1, item.dtEmissaoNF);
    Gerador.wCampo(tcDe2, '', 'vlrBruto', 0, 0, 1, item.vlrBruto);
    Gerador.wCampo(tcStr, '', 'obs', 0, 0, 0, item.obs);
    GerarinfoTpServ(item.infoTpServs);
    Gerador.wGrupo('/nfs');
  end;
end;

end.
