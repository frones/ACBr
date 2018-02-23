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

unit pcnReinfR2010;

interface

uses
  Classes, Sysutils, pcnGerador, pcnConversaoReinf, ACBrReinfEventosBase,
  pcnReinfClasses, pcnReinfR2010_Class;

type

  TR2010 = class(TEventoReinfRet)
  private
    FinfoServTom: TinfoServTom;
  protected
    procedure GerarEventoXML; override;
    procedure GerarideEstabObra;
    procedure GeraridePrestServ;
    procedure GerarinfoProcRetPr(Items: TinfoProcRetPrs);
    procedure GerarNFs(Items: Tnfss);
    procedure GerarinfoTpServ(Items: TinfoTpServs);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property infoServTom: TinfoServTom read FinfoServTom;
  end;

implementation

uses
  pcnAuxiliar, ACBrUtil, pcnConversao, DateUtils;

{ TR2010 }

procedure TR2010.AfterConstruction;
begin
  inherited;
  SetSchema(rsevtServTom);
  FinfoServTom := TinfoServTom.Create;
end;

procedure TR2010.BeforeDestruction;
begin
  inherited;
  FinfoServTom.Free;
end;

procedure TR2010.GerarEventoXML;
begin
  Gerador.wGrupo('infoServTom');
  GerarideEstabObra;
  Gerador.wGrupo('/infoServTom');
end;

procedure TR2010.GerarideEstabObra;
begin
  Gerador.wGrupo('ideEstabObra');
  Gerador.wCampo(tcInt, '', 'tpInscEstab', 0, 0, 1, ord(Self.FinfoServTom.ideEstabObra.tpInscEstab));
  Gerador.wCampo(tcStr, '', 'nrInscEstab', 0, 0, 1, Self.FinfoServTom.ideEstabObra.nrInscEstab);
  Gerador.wCampo(tcInt, '', 'indObra', 0, 0, 1, ord(Self.FinfoServTom.ideEstabObra.indObra));
  GeraridePrestServ;
  Gerador.wGrupo('/ideEstabObra');
end;

procedure TR2010.GeraridePrestServ;
var
  idePrestServ: TidePrestServ;
  i: Integer;
begin
  for i:=0 to FinfoServTom.ideEstabObra.idePrestServs.Count - 1 do
  begin
    idePrestServ := FinfoServTom.ideEstabObra.idePrestServs.Items[i];

    Gerador.wGrupo('idePrestServ');
    Gerador.wCampo(tcStr, '', 'cnpjPrestador', 0, 0, 1, idePrestServ.cnpjPrestador);
    Gerador.wCampo(tcDe2, '', 'vlrTotalBruto', 0, 0, 1 , idePrestServ.vlrTotalBruto);
    Gerador.wCampo(tcDe2, '', 'vlrTotalBaseRet', 0, 0, 1, idePrestServ.vlrTotalBaseRet);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRetPrinc', 0, 0, 1, idePrestServ.vlrTotalRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRetAdic', 0, 0, 0, idePrestServ.vlrTotalRetAdic);
    Gerador.wCampo(tcDe2, '', 'vlrTotalNRetPrinc', 0, 0, 0, idePrestServ.vlrTotalNRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrTotalNRetAdic', 0, 0, 0, idePrestServ.vlrTotalNRetAdic);
    Gerador.wCampo(tcInt, '', 'indCPRB', 0, 0, 1, idePrestServ.indCPRB);
    GerarNFs(idePrestServ.nfss);
    GerarinfoProcRetPr(idePrestServ.infoProcRetPrs);
    Gerador.wGrupo('/idePrestServ');
  end;
end;

procedure TR2010.GerarinfoProcRetPr(Items: TinfoProcRetPrs);
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

procedure TR2010.GerarinfoTpServ(Items: TinfoTpServs);
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

procedure TR2010.GerarNFs(Items: Tnfss);
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
