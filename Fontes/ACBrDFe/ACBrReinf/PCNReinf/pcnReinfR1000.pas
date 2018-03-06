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

unit pcnReinfR1000;

interface

uses
  Classes, Sysutils, pcnGerador, pcnConversaoReinf, pcnReinfClasses,
  ACBrReinfEventosBase, pcnReinfR1000_Class;

type

  TR1000 = class(TEventoReinf)
  private
    FinfoContri: TinfoContri;
  protected
    procedure GerarEventoXML; override;
    procedure GerarInfoCadastro;
    procedure GerarContato;
    procedure GerarSoftwareHouse;
    procedure GerarInfoEFR;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property infoContri: TinfoContri read FinfoContri;
  end;

implementation

{ TR1000 }

uses
  pcnAuxiliar, ACBrUtil, pcnConversao, DateUtils;

procedure TR1000.AfterConstruction;
begin
  inherited;
  SetSchema(schevtInfoContri);
  FinfoContri := TinfoContri.Create;
end;

procedure TR1000.BeforeDestruction;
begin
  inherited;
  FinfoContri.Free;
end;

procedure TR1000.GerarContato;
begin
  Gerador.wGrupo('contato');

  Gerador.wCampo(tcStr, '', 'nmCtt',     1, 70, 1, Self.infoContri.InfoCadastro.Contato.NmCtt);
  Gerador.wCampo(tcStr, '', 'cpfCtt',   11, 11, 1, Self.infoContri.infoCadastro.Contato.CpfCtt);
  Gerador.wCampo(tcStr, '', 'foneFixo',  1, 13, 0, Self.infoContri.infoCadastro.Contato.FoneFixo);
  Gerador.wCampo(tcStr, '', 'foneCel',   1, 13, 0, Self.infoContri.infoCadastro.Contato.FoneCel);
  Gerador.wCampo(tcStr, '', 'email',     1, 60, 0, Self.infoContri.infoCadastro.Contato.email);

  Gerador.wGrupo('/contato');
end;

procedure TR1000.GerarEventoXML;
begin
  Gerador.wGrupo('infoContri');

  GerarModoAbertura(Self.TipoOperacao);
  GerarIdePeriodo(Self.infoContri.idePeriodo);

  if (Self.TipoOperacao <> toExclusao) then
    GerarInfoCadastro;

  if (Self.TipoOperacao = toAlteracao) and (Self.NovaValidade.IniValid <> EmptyStr) then
    GerarIdePeriodo(novaValidade,'novaValidade');

  GerarModoFechamento(Self.TipoOperacao);

  Gerador.wGrupo('/infoContri');
end;

procedure TR1000.GerarInfoCadastro;
begin
  Gerador.wGrupo('infoCadastro');

  Gerador.wCampo(tcStr, '', 'classTrib',          1, 2, 1, Self.infoContri.infoCadastro.ClassTrib);
  Gerador.wCampo(tcStr, '', 'indEscrituracao',    1, 1, 1, indEscrituracaoToStr(Self.infoContri.infoCadastro.indEscrituracao));
  Gerador.wCampo(tcStr, '', 'indDesoneracao',     1, 1, 1, indDesoneracaoToStr(Self.infoContri.infoCadastro.indDesoneracao));
  Gerador.wCampo(tcStr, '', 'indAcordoIsenMulta', 1, 1, 1, indAcordoIsenMultaToStr(Self.infoContri.infoCadastro.indAcordoIsenMulta));
  Gerador.wCampo(tcStr, '', 'indSitPJ',           1, 1, 0, indSitPJToStr(Self.infoContri.infoCadastro.indSitPJ));

  GerarContato;
  GerarSoftwareHouse;
  GerarInfoEFR;

  Gerador.wGrupo('/infoCadastro');
end;

procedure TR1000.GerarInfoEFR;
begin
  if (infoContri.infoCadastro.infoEFR.cnpjEFR <> EmptyStr) then
  begin
    Gerador.wGrupo('infoEFR');

    Gerador.wCampo(tcStr, '', 'ideEFR',   1,  1, 1, SimNaoToStr(infoContri.infoCadastro.infoEFR.ideEFR));
    Gerador.wCampo(tcStr, '', 'cnpjEFR', 14, 14, 0, infoContri.infoCadastro.infoEFR.cnpjEFR);

    Gerador.wGrupo('/infoEFR');
  end;
end;

procedure TR1000.GerarSoftwareHouse;
begin
  Gerador.wGrupo('softHouse');

  Gerador.wCampo(tcStr, '', 'cnpjSoftHouse', 14,  14, 1, infoContri.infoCadastro.SoftwareHouse.CnpjSoftHouse);
  Gerador.wCampo(tcStr, '', 'nmRazao',        1, 115, 1, infoContri.infoCadastro.SoftwareHouse.NmRazao);
  Gerador.wCampo(tcStr, '', 'nmCont',         1,  70, 1, infoContri.infoCadastro.SoftwareHouse.NmCont);
  Gerador.wCampo(tcStr, '', 'telefone',       1,  13, 0, infoContri.infoCadastro.SoftwareHouse.Telefone);
  Gerador.wCampo(tcStr, '', 'email',          1,  60, 0, infoContri.infoCadastro.SoftwareHouse.email);

  Gerador.wGrupo('/softHouse');
end;

end.
