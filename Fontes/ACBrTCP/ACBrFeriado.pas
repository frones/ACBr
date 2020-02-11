{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

{******************************************************************************
|* Historico
|*
|* 27/10/2017: Primeira Versao,
|*             Inclusão de pesquisa no WebService calendario.com.br
|*    Filipe de Almeida Sortica
|* 07/11/2017: Inclusão de pesquisa no arquivo JSON
|*    Filipe de Almeida Sortica
|* 14/11/2017: Separação das classes em arquivos próprios
|*    Filipe de Almeida Sortica
******************************************************************************}

unit ACBrFeriado;

{$I ACBr.inc}

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase, ACBrSocket, ACBrFeriadoWSClass;

type
  TACBrFeriadoWebService = (wsfNenhum, wsfCalendario, wsfJSON);

  TACBrFeriadoTipo = (ftNenhum, ftNacional, ftEstadual, ftMunicipal,
                      ftFacultativo, ftDiaConvencional);
  TACBrFeriadoTrocaDiaTipo = (fatNenhum, fatDePara, fatDeDiaUtilParaSegOuSex);

  EACBrFeriadoException = class(Exception);

  TACBrFeriadoEvento = class
  private
    fAno: Integer;
    fMes: Integer;
    fDia: Integer;
    fData: TDateTime;
    fNome: String;
    fDescricao: String;
    fTipo: TACBrFeriadoTipo;
    fLegislacao: String;
    fLink: String;
    fListaUF: String;
    fListaCidade: String;
  public
    constructor Create;

    property Ano: Integer           read fAno         write fAno;
    property Mes: Integer           read fMes         write fMes;
    property Dia: Integer           read fDia         write fDia;
    property Data: TDateTime        read fData        write fData;
    property Nome: String           read fNome        write fNome;
    property Descricao: String      read fDescricao   write fDescricao;
    property Tipo: TACBrFeriadoTipo read fTipo        write fTipo;
    property Legislacao: String     read fLegislacao  write fLegislacao;
    property Link: String           read fLink        write fLink;
    property ListaUF: String        read fListaUF     write fListaUF;
    property ListaCidade: String    read fListaCidade write fListaCidade;
  end;

  TACBrFeriadoEventos = class(TObjectList{$IfDef NEXTGEN}<TACBrFeriadoEvento>{$EndIf})
  protected
    procedure SetObject(Index: Integer; Item: TACBrFeriadoEvento);
    function GetObject(Index: Integer): TACBrFeriadoEvento;
    procedure Insert(Index: Integer; Obj: TACBrFeriadoEvento);
  public
    function Add(Obj: TACBrFeriadoEvento): Integer;
    function New: TACBrFeriadoEvento;
    property Objects[Index: Integer]: TACBrFeriadoEvento
      read GetObject write SetObject; default;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrFeriado = class(TACBrHTTP)
  private
    fWebService: TACBrFeriadoWebService;
    fACBrFeriadoWS: TACBrFeriadoWSClass;

    fEventos: TACBrFeriadoEventos;
    fOnBuscaEfetuada: TNotifyEvent;
    fToken: String;
    fPathArquivo: String;

    function GetURL: String;
    procedure SetWebService(const AValue: TACBrFeriadoWebService);
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    property Eventos: TACBrFeriadoEventos read fEventos;

    /// <summary>
    ///   Realiza a busca dos feriados.
    ///   <param name="AAno">Deve ser informado o ano que se deseja ver os
    ///     feriados.</param>
    ///   <param name="AUF">Opcional, e deve ser informada a UF para incluir os
    ///     feriados estaduais.</param>
    ///   <param name="ACidade">Opcional, e deve ser informado o nome da
    ///     cidade para incluir os feriados municipais.</param>
    /// </summary>
    /// <remarks>O método retorna a quantidade de eventos encontrados.</remarks>
    function Buscar(const AAno: Integer; const AUF: String = '';
      const ACidade: String = ''): Integer;

  published
    property WebService: TACBrFeriadoWebService read fWebService write SetWebService default wsfNenhum;
    property URL: String read GetURL;
    property Token: String read fToken write fToken;
    property PathArquivo: String read fPathArquivo write fPathArquivo;

    property OnBuscaEfetuada: TNotifyEvent read fOnBuscaEfetuada
       write fOnBuscaEfetuada;
  end;

implementation

uses
  ACBrUtil, ACBrFeriadoWSCalendario, ACBrFeriadoWSJSON;

{ TACBrFeriadoEvento }

constructor TACBrFeriadoEvento.Create;
begin
  inherited;
  fAno         := 0;
  fMes         := 0;
  fDia         := 0;
  fData        := 0;
  fNome        := '';
  fDescricao   := '';
  fTipo        := ftNenhum;
  fLegislacao  := '';
  fLink        := '';
  fListaUF     := '';
  fListaCidade := '';
end;

{ TACBrFeriadoEventos }

function TACBrFeriadoEventos.Add(Obj: TACBrFeriadoEvento): Integer;
begin
  Result := inherited Add(Obj);
end;

function TACBrFeriadoEventos.GetObject(Index: Integer): TACBrFeriadoEvento;
begin
  Result := TACBrFeriadoEvento(inherited Items[Index]);
end;

procedure TACBrFeriadoEventos.Insert(Index: Integer; Obj: TACBrFeriadoEvento);
begin
  inherited Insert(Index, Obj);
end;

function TACBrFeriadoEventos.New: TACBrFeriadoEvento;
begin
  Result := TACBrFeriadoEvento.Create;
  Self.Add(Result);
end;

procedure TACBrFeriadoEventos.SetObject(Index: Integer;
  Item: TACBrFeriadoEvento);
begin
  inherited Items[Index] := Item;
end;

{ TACBrFeriado }

function TACBrFeriado.Buscar(const AAno: Integer; const AUF,
  ACidade: String): Integer;
begin
  fEventos.Clear;

  if (AAno = 0) then
     raise EACBrFeriadoException.Create(ACBrStr('O Ano deve ser informado'));

  fACBrFeriadoWS.Buscar(AAno, AUF, ACidade);

  Result := fEventos.Count;
end;

constructor TACBrFeriado.Create(AOwner: TComponent);
begin
  inherited Create(AOwner) ;
  fOnBuscaEfetuada := nil;
  fEventos         := TACBrFeriadoEventos.Create(True);
  fACBrFeriadoWS   := TACBrFeriadoWSClass.Create(Self);
  fWebService      := wsfNenhum;
end;

destructor TACBrFeriado.Destroy;
begin
  fEventos.Free;
  fACBrFeriadoWS.Free;
  inherited;
end;

function TACBrFeriado.GetURL: String;
begin
  Result := fACBrFeriadoWS.URL;
end;

procedure TACBrFeriado.SetWebService(const AValue: TACBrFeriadoWebService);
begin
  if (fWebService = AValue) then
    Exit;

  fACBrFeriadoWS.Free;

  case AValue of
    wsfCalendario: fACBrFeriadoWS := TACBrFeriadoWSCalendario.Create(Self);
    wsfJSON:       fACBrFeriadoWS := TACBrFeriadoWSJSON.Create(Self);
  else
    fACBrFeriadoWS := TACBrFeriadoWSClass.Create(Self);
  end;

  fWebService := AValue;
end;

end.
