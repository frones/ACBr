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

unit pcnReinfR2050;

interface

uses
  Classes, Sysutils, pcnGerador, pcnConversaoReinf, ACBrReinfEventosBase,
  pcnReinfClasses, pcnReinfR2050_Class;

type

  TR2050 = class(TEventoReinfRet)
  private
    FinfoComProd : TinfoComProd;
  protected
    procedure GerarEventoXML; override;
    procedure GerarinfoComProd;
    procedure GerarideEstab;
    procedure GerartipoCom(Items: TtipoComs);
    procedure GerarinfoProc(Items: TinfoProcs);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property infoComProd: TinfoComProd read FinfoComProd;
  end;

implementation

uses
  pcnAuxiliar, ACBrUtil, pcnConversao, DateUtils;

{ TR2050 }

procedure TR2050.AfterConstruction;
begin
  inherited;
  SetSchema(schevtComProd);
  FinfoComProd := TinfoComProd.Create;
end;

procedure TR2050.BeforeDestruction;
begin
  inherited;
  FinfoComProd.Free;
end;

procedure TR2050.GerarEventoXML;
begin
  GerarinfoComProd;
end;

procedure TR2050.GerarinfoComProd;
begin
  Gerador.wGrupo('infoComProd');

  GerarideEstab;

  Gerador.wGrupo('/infoComProd');
end;

procedure TR2050.GerarideEstab;
begin
  Gerador.wGrupo('ideEstab');

  Gerador.wCampo(tcStr, '', 'tpInscEstab',       1,  1, 1, TpInscricaoToStr( Self.FinfoComProd.ideEstab.tpInscEstab ));
  Gerador.wCampo(tcStr, '', 'nrInscEstab',       1, 14, 1, Self.FinfoComProd.ideEstab.nrInscEstab);
  Gerador.wCampo(tcDe2, '', 'vlrRecBrutaTotal',  1, 14, 1, Self.FinfoComProd.ideEstab.vlrRecBrutaTotal);
  Gerador.wCampo(tcDe2, '', 'vlrCPApur',         1, 14, 1, Self.FinfoComProd.ideEstab.vlrCPApur);
  Gerador.wCampo(tcDe2, '', 'vlrRatApur',        1, 14, 1, Self.FinfoComProd.ideEstab.vlrRatApur);
  Gerador.wCampo(tcDe2, '', 'vlrSenarApur',      1, 14, 1, Self.FinfoComProd.ideEstab.vlrSenarApur);
  Gerador.wCampo(tcDe2, '', 'vlrCPSuspTotal',    1, 14, 0, Self.FinfoComProd.ideEstab.vlrCPSuspTotal);
  Gerador.wCampo(tcDe2, '', 'vlrRatSuspTotal',   1, 14, 0, Self.FinfoComProd.ideEstab.vlrRatSuspTotal);
  Gerador.wCampo(tcDe2, '', 'vlrSenarSuspTotal', 1, 14, 0, Self.FinfoComProd.ideEstab.vlrSenarSuspTotal);

  GerartipoCom(Self.FinfoComProd.ideEstab.tipoComs);

  Gerador.wGrupo('/ideEstab');
end;

procedure TR2050.GerartipoCom(Items: TtipoComs);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('tipoCom');

      Gerador.wCampo(tcStr, '', 'indCom',      1,  1, 1, indComToStr( indCom ));
      Gerador.wCampo(tcDe2, '', 'vlrRecBruta', 1, 14, 1, vlrRecBruta);

      GerarinfoProc(infoProcs);

      Gerador.wGrupo('/tipoCom');
    end;
end;

procedure TR2050.GerarinfoProc(Items: TinfoProcs);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('infoProc');

      Gerador.wCampo(tcStr, '', 'tpProc',       1,  1, 1, TpProcToStr( tpProc ));
      Gerador.wCampo(tcStr, '', 'nrProc',       1, 21, 1, nrProc);
      Gerador.wCampo(tcStr, '', 'codSusp',      1, 14, 0, codSusp);
      Gerador.wCampo(tcDe2, '', 'vlrCPSusp',    1, 14, 0, vlrCPSusp);
      Gerador.wCampo(tcDe2, '', 'vlrRatSusp',   1, 14, 0, vlrRatSusp);
      Gerador.wCampo(tcDe2, '', 'vlrSenarSusp', 1, 14, 0, vlrSenarSusp);

      Gerador.wGrupo('/infoProc');
    end;
end;

end.
