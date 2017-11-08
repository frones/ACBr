{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2017: Primeira Versao,
|*             Inclusão de pesquisa no WebService calendario.com.br
|*    Filipe de Almeida Sortica
|* 07/11/2017: Inclusão de pesquisa no arquivo JSON
|*    Filipe de Almeida Sortica
******************************************************************************}

unit ACBrFeriado;

{$I ACBr.inc}

interface

uses
  SysUtils, Contnrs, Classes, ACBrSocket;

type
  TACBrFeriadoWebService = (wsfNenhum, wsfCalendario, wsfJSON);

  TACBrFeriadoTipo = (ftNenhum, ftNacional, ftEstadual, ftMunicipal,
                      ftFacultativo, ftDiaConvencional);
  TACBrFeriadoTrocaDiaTipo = (fatNenhum, fatDePara, fatDeDiaUtilParaSegOuSex);

  EACBrFeriadoException = class (Exception);

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

  TACBrFeriadoEventos = class(TObjectList)
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

  TACBrFeriadoWSClass = class;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
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
    procedure SetWebService(const AValue : TACBrFeriadoWebService);
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

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrFeriadoWSClass = class
  protected
    fOwner: TACBrFeriado;
    fpURL: String;

    procedure BuscaEfetuada;
    procedure ErrorAbstract;
    procedure TestarToken;
    procedure TestarPathArquivo;
  public
    constructor Create(AOwner: TACBrFeriado); virtual;

    procedure Buscar(const AAno: Integer; const AUF: String = '';
      const ACidade: String = ''); virtual;

    property URL: String read fpURL;
  end;

  TACBrWSCalendario = class(TACBrFeriadoWSClass)
  protected
    procedure ProcessarResposta;
  public
    constructor Create(AOwner: TACBrFeriado); override;

    procedure Buscar(const AAno: Integer; const AUF: String = '';
      const ACidade: String = ''); override;
  end;

  TACBrWSJSON = class(TACBrFeriadoWSClass)
  protected
    fEventosArquivo: TObjectList;

    function CarregarEventosArquivo(const AAno: Integer): TObjectList;
    function GetDataDaPascoa(const ano: Integer): TDateTime;
    procedure ProcessarResposta(const AAno: Integer; const AUF: String = '';
      const ACidade: String = '');
  public
    constructor Create(AOwner: TACBrFeriado); override;
    destructor Destroy; override;

    procedure Buscar(const AAno: Integer; const AUF: String = '';
      const ACidade: String = ''); override;
  end;

implementation

uses
  StrUtils, ACBrUtil, synautil;

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
  Result := inherited GetItem(Index) as TACBrFeriadoEvento;
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
  inherited SetItem (Index, Item);
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
    wsfCalendario: fACBrFeriadoWS := TACBrWSCalendario.Create(Self);
    wsfJSON:       fACBrFeriadoWS := TACBrWSJSON.Create(Self);
  else
    fACBrFeriadoWS := TACBrFeriadoWSClass.Create(Self);
  end;

  fWebService := AValue;
end;

{ TACBrFeriadoWSClass }

procedure TACBrFeriadoWSClass.BuscaEfetuada;
begin
  if (Assigned(fOwner.OnBuscaEfetuada)) then
    fOwner.OnBuscaEfetuada(fOwner);
end;

procedure TACBrFeriadoWSClass.Buscar(const AAno: Integer; const AUF,
  ACidade: String);
begin
  ErrorAbstract;
end;

constructor TACBrFeriadoWSClass.Create(AOwner: TACBrFeriado);
begin
  inherited Create;
  fOwner := AOwner;
  fpURL  := '';
end;

procedure TACBrFeriadoWSClass.ErrorAbstract;
begin
  raise EACBrFeriadoException.Create(ACBrStr('Nenhum WebService selecionado'));
end;

procedure TACBrFeriadoWSClass.TestarPathArquivo;
begin
  if (fOwner.PathArquivo = EmptyStr) then
    raise EACBrFeriadoException.Create(ACBrStr('Arquivo não informado'));
  if not(FileExists(fOwner.PathArquivo)) then
    raise EACBrFeriadoException.Create(ACBrStr('Arquivo informado não existe'));
end;

procedure TACBrFeriadoWSClass.TestarToken;
begin
  if (fOwner.Token = EmptyStr) then
    raise EACBrFeriadoException.Create(ACBrStr('Token não informado'));
end;

{ TACBrWSCalendario }

procedure TACBrWSCalendario.Buscar(const AAno: Integer; const AUF,
  ACidade: String);
var
  sURL: String;
  sNomeCidade: String;
begin
  TestarToken;

  sURL := fpURL + 'api/api_feriados.php?';
  sURL := sURL + 'token='+ fOwner.Token;
  sURL := sURL + '&ano='+ IntToStr(AAno);
  if (AUF <> EmptyStr) then
    sURL := sURL + '&estado='+ AUF;
  if (ACidade <> EmptyStr) then
  begin
    sNomeCidade := TiraAcentos(UpperCase(Trim(ACidade)));
    sNomeCidade := StringReplace(sNomeCidade, ' ', '_', [rfReplaceAll]);
    sURL := sURL + '&cidade='+ sNomeCidade;
  end;

  fOwner.HTTPGet(sURL);

  ProcessarResposta;
end;

constructor TACBrWSCalendario.Create(AOwner: TACBrFeriado);
begin
  inherited Create(AOwner);
  fpURL := 'http://www.calendario.com.br/';
end;

procedure TACBrWSCalendario.ProcessarResposta;
var
  Buffer: String;
  XML: TStringList;
  i: Integer;
  sEvento: String;
  Evento: TACBrFeriadoEvento;
  Data: TDateTime;
  Ano: Word;
  Mes: Word;
  Dia: Word;
  Tipo: Integer;
begin
  Buffer := fOwner.RespHTTP.Text;
  if (Buffer = EmptyStr) then
    Exit;

  Buffer := StringReplace(Buffer, sLineBreak, '', [rfReplaceAll]);
  Buffer := StringReplace(Buffer, '</location>', '</location>' + sLineBreak, [rfReplaceAll]);
  Buffer := StringReplace(Buffer, '</event>', '</event>' + sLineBreak, [rfReplaceAll]);

  XML := TStringList.Create;
  try
    XML.Text := Buffer;

    for i := 0 to XML.Count - 1 do
    begin
      sEvento := XML.Strings[i];

      if (LerTagXML(sEvento, 'event') <> '') then
      begin
        Evento := fOwner.Eventos.New;

        Data := StrToDateDef(LerTagXML(sEvento, 'date'), 0);
        DecodeDate(Data, Ano, Mes, Dia);
        Evento.Data      := Data;
        Evento.Ano       := Ano;
        Evento.Mes       := Mes;
        Evento.Dia       := Dia;

        Evento.Nome      := LerTagXML(sEvento, 'name');
        Evento.Descricao := LerTagXML(sEvento, 'description');
        Evento.Link      := LerTagXML(sEvento, 'link');

        Tipo := StrToIntDef(LerTagXML(sEvento, 'type_code'), 0);
        case Tipo of
          1: Evento.Tipo := ftNacional;
          2: Evento.Tipo := ftEstadual;
          3: Evento.Tipo := ftMunicipal;
          4: Evento.Tipo := ftFacultativo;
          9: Evento.Tipo := ftDiaConvencional;
        else
          Evento.Tipo := ftNenhum;
        end;
      end;
    end;
  finally
    XML.Free;
  end;

  BuscaEfetuada;
end;

{ TACBrWSJSON }

procedure TACBrWSJSON.Buscar(const AAno: Integer; const AUF,
  ACidade: String);
begin
  if (fEventosArquivo.Count = 0) then
  begin
    TestarPathArquivo;
    CarregarEventosArquivo(AAno);
  end;

  ProcessarResposta(AAno, AUF, ACidade);
end;

function TACBrWSJSON.CarregarEventosArquivo(const AAno: Integer): TObjectList;
var
  MS: TMemoryStream;
  Arquivo: String;

  iPosFixos: Integer;
  iPosMoveis: Integer;
  iPosAtual: Integer;
  iPosIni: Integer;
  iPosFin: Integer;
  sEvento: String;
  Evento: TStringList;

  dtPascoa: TDateTime;
  dtSegundaCarnaval: TDateTime;
  dtCarnaval: TDateTime;
  dtQuartaCinzas: TDateTime;
  dtQuintaSanta: TDateTime;
  dtSextaSanta: TDateTime;
  dtCorpusChristi: TDateTime;
  dtSextaCorpusChristi: TDateTime;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(fOwner.PathArquivo);
    Arquivo := UTF8ToNativeString(ReadStrFromStream(MS, MS.Size));
  finally
    MS.Free;
  end;

  if (Arquivo = '') then
    Exit;

  Arquivo := StringReplace(Arquivo, #9, '', [rfReplaceAll]);
  Arquivo := StringReplace(Arquivo, '": ', '":', [rfReplaceAll]);

  iPosFixos := PosEx('"feriadosFixos":[', Arquivo) + 16;
  iPosMoveis := PosEx('"feriadosMoveis":[', Arquivo) + 17;
  iPosAtual := iPosFixos;

  while (iPosAtual < iPosMoveis - 25) do
  begin
    iPosIni := PosEx('{', Arquivo, iPosAtual) + 1;
    iPosFin := PosEx('}', Arquivo, iPosIni) - 1;

    sEvento := Copy(Arquivo, iPosIni, iPosFin - iPosIni + 1);
    sEvento := StringReplace(sEvento, ',' + sLineBreak, '|,|', [rfReplaceAll]);
    sEvento := StringReplace(sEvento, sLineBreak, '', [rfReplaceAll]);
    sEvento := '|' + sEvento + '|';
    sEvento := StringReplace(sEvento, '"', '', [rfReplaceAll]);

    Evento := TStringList.Create;
    Evento.Delimiter := ',';
    Evento.QuoteChar := '|';
    Evento.NameValueSeparator := ':';
    Evento.DelimitedText := sEvento;
    Evento.Values['data'] := Evento.Values['data'] + '/' + IntToStr(AAno);
    fEventosArquivo.Add(Evento);

    iPosAtual := iPosFin + 1;
  end;

  dtPascoa := GetDataDaPascoa(AAno);
  dtSegundaCarnaval := dtPascoa - 48;
  dtCarnaval := dtPascoa - 47;
  dtQuartaCinzas := dtPascoa - 46;
  dtQuintaSanta := dtPascoa - 3;
  dtSextaSanta := dtPascoa - 2;
  dtCorpusChristi := dtPascoa + 60;
  dtSextaCorpusChristi := dtPascoa + 61;

  while ((iPosAtual > 0) and
         (iPosAtual < Length(Arquivo) - 10)) do
  begin
    iPosIni := PosEx('{', Arquivo, iPosAtual) + 1;
    iPosFin := PosEx('}', Arquivo, iPosIni) - 1;

    sEvento := Copy(Arquivo, iPosIni, iPosFin - iPosIni + 1);
    sEvento := StringReplace(sEvento, ',' + sLineBreak, '|,|', [rfReplaceAll]);
    sEvento := StringReplace(sEvento, sLineBreak, '', [rfReplaceAll]);
    sEvento := '|' + sEvento + '|';
    sEvento := StringReplace(sEvento, '"', '', [rfReplaceAll]);

    Evento := TStringList.Create;
    Evento.Delimiter := ',';
    Evento.QuoteChar := '|';
    Evento.NameValueSeparator := ':';
    Evento.DelimitedText := sEvento;

    if (Pos('Segunda-feira de Carnaval', Evento.Values['nome']) = 1) then
      Evento.Insert(0, 'data:'+ FormatDateTime('dd/mm/yyyy', dtSegundaCarnaval))
    else if (Pos('Carnaval', Evento.Values['nome']) = 1) then
      Evento.Insert(0, 'data:'+ FormatDateTime('dd/mm/yyyy', dtCarnaval))
    else if (Pos('Quarta-feira de Cinzas', Evento.Values['nome']) = 1) then
      Evento.Insert(0, 'data:'+ FormatDateTime('dd/mm/yyyy', dtQuartaCinzas))
    else if (Pos('Quinta-feira Santa', Evento.Values['nome']) = 1) then
      Evento.Insert(0, 'data:'+ FormatDateTime('dd/mm/yyyy', dtQuintaSanta))
    else if (Pos('Sexta-feira Santa', Evento.Values['nome']) = 1) then
      Evento.Insert(0, 'data:'+ FormatDateTime('dd/mm/yyyy', dtSextaSanta))
    else if (Pos(ACBrStr('Páscoa'), Evento.Values['nome']) = 1) then
      Evento.Insert(0, 'data:'+ FormatDateTime('dd/mm/yyyy', dtPascoa))
    else if (Pos('Corpus Christi', Evento.Values['nome']) = 1) then
      Evento.Insert(0, 'data:'+ FormatDateTime('dd/mm/yyyy', dtCorpusChristi))
    else if (Pos('Sexta-feira de Corpus Christi', Evento.Values['nome']) = 1) then
      Evento.Insert(0, 'data:'+ FormatDateTime('dd/mm/yyyy', dtSextaCorpusChristi));

    fEventosArquivo.Add(Evento);

    iPosAtual := iPosFin + 1;
  end;
end;

constructor TACBrWSJSON.Create(AOwner: TACBrFeriado);
begin
  inherited Create(AOwner);
  fEventosArquivo := TObjectList.Create;
end;

destructor TACBrWSJSON.Destroy;
begin
  fEventosArquivo.Free;
  inherited;
end;

function TACBrWSJSON.GetDataDaPascoa(const ano: Integer): TDateTime;
var
  x, y, a, b, c, d, e: integer;
  dia, mes: word;
begin
  x := 24;
  y := 5;
  a := ano MOD 19;
  b := ano MOD 4;
  c := ano MOD 7;
  d := (19 * a + x) MOD 30;
  e := (2 * b + 4 * c + 6 * d + y) MOD 7;
  if (d + e) > 9 then
   begin
    dia := (d + e - 9);
    mes := 4;
   end
  else
   begin
    dia := (d + e + 22);
    mes := 3;
   end;
  result :=  EncodeDate(ano, mes, dia);
end;

procedure TACBrWSJSON.ProcessarResposta(const AAno: Integer; const AUF,
  ACidade: String);
var
  i: Integer;
  j: Integer;
  EventoArquivo: TStringList;
  Ano: Word;
  Mes: Word;
  Dia: Word;
  Data: TDate;
  DataInicio: TDate;
  Nome: String;
  Descricao: String;
  ListaUF: String;
  ListaCidade: String;
  Legislacao: String;
  LegislacaoLink: String;
  iTipo: Integer;
  Tipo: TACBrFeriadoTipo;
  iTrocaDiaTipo: Integer;
  TrocaDiaTipo: TACBrFeriadoTrocaDiaTipo;
  TrocaDiaDe: String;
  TrocaDiaPara: Integer;
  TrocaDiaParaSemana: Integer;
  DiaSemanaOficial: Integer;
  PodeIncluir: Boolean;
  Evento: TACBrFeriadoEvento;
begin
  for i := 0 to fEventosArquivo.Count - 1 do
  begin
    EventoArquivo := TStringList(fEventosArquivo.Items[i]);

    PodeIncluir := True;

    Data           := StrToDateDef(EventoArquivo.Values['data'], 0);
    DataInicio     := StrToDateDef(EventoArquivo.Values['dataInicio'], 0);
    Nome           := EventoArquivo.Values['nome'];
    Descricao      := EventoArquivo.Values['descricao'];
    ListaUF        := EventoArquivo.Values['listaUF'];
    ListaCidade    := EventoArquivo.Values['listaCidade'];
    Legislacao     := EventoArquivo.Values['legislacao'];
    LegislacaoLink := EventoArquivo.Values['legislacaoLink'];
    iTipo          := StrToIntDef(EventoArquivo.Values['tipo'], 0);
    case iTipo of
      1: Tipo := ftNacional;
      2: Tipo := ftEstadual;
      3: Tipo := ftMunicipal;
      4: Tipo := ftFacultativo;
      9: Tipo := ftDiaConvencional;
    else
      Tipo := ftNenhum;
    end;

    // Verifica se o evento deve ter seu dia trocado
    iTrocaDiaTipo := StrToIntDef(EventoArquivo.Values['trocaDiaTipo'], 0);
    case iTrocaDiaTipo of
      1: TrocaDiaTipo := fatDePara;
      2: TrocaDiaTipo := fatDeDiaUtilParaSegOuSex;
    else
      TrocaDiaTipo := fatNenhum;
    end;
    case TrocaDiaTipo of
      fatDePara:
      begin
        TrocaDiaDe         := EventoArquivo.Values['trocaDiaDe'];
        TrocaDiaPara       := StrToIntDef(EventoArquivo.Values['trocaDiaPara'], 0);
        TrocaDiaParaSemana := StrToIntDef(EventoArquivo.Values['trocaDiaParaSemana'], 0);

        DiaSemanaOficial := DayOfWeek(Data);
        if ((TrocaDiaPara > 0) and (PosEx(IntToStr(DiaSemanaOficial), TrocaDiaDe) > 0)) then
          Data := Data + (TrocaDiaPara - DiaSemanaOficial + (TrocaDiaParaSemana * 7));
      end;
      fatDeDiaUtilParaSegOuSex:
      begin
        DiaSemanaOficial := DayOfWeek(Data);
        if (DiaSemanaOficial in [3,4]) then
          Data := Data - DiaSemanaOficial + 2
        else if (DiaSemanaOficial = 5) then
          Data := Data + 1;
      end;
    end;

    // Verifica se o evento já foi instituído
    if ((DataInicio > 0) and (Data < DataInicio)) then
      PodeIncluir := False;

    if ((PodeIncluir) and (Tipo <> ftNacional)) then
    begin
      // Verifica se o evento coincide com o estado
      if (Tipo in [ftEstadual, ftFacultativo, ftDiaConvencional]) then
      begin
        j := EventoArquivo.IndexOfName('listaUF');
        if (j > -1) then
        begin
          if (AUF <> '') then
          begin
            if (PosEx(AUF, EventoArquivo.ValueFromIndex[j]) = 0) then
              PodeIncluir := False;
          end
          else
            PodeIncluir := False;
        end;
      end;

      // Verifica se o evento coincide com a cidade
      if ((PodeIncluir) and (Tipo in [ftMunicipal, ftFacultativo, ftDiaConvencional])) then
      begin
        j := EventoArquivo.IndexOfName('listaCidade');
        if (j > -1) then
        begin
          if (ACidade <> '') then
          begin
            if (PosEx(ACidade, EventoArquivo.ValueFromIndex[j]) = 0) then
              PodeIncluir := False;
          end
          else
            PodeIncluir := False;
        end;
      end;
    end;

    // Verifica se precisa manter o evento mais específico para a região
    if (PodeIncluir) then
    begin
      for j := 0 to fOwner.Eventos.Count - 1 do
        if (Pos(Nome, fOwner.Eventos.Objects[j].Nome) = 1) then
        begin
          PodeIncluir :=
            ((ListaUF <> '') and ((fOwner.Eventos.Objects[j].ListaUF = '') and (fOwner.Eventos.Objects[j].ListaCidade = ''))) or
            ((ListaCidade <> '') and (fOwner.Eventos.Objects[j].ListaCidade = ''));

          if (PodeIncluir) then
            fOwner.Eventos.Delete(j);

          Break;
        end;
    end;

    if (PodeIncluir) then
    begin
      Evento := fOwner.Eventos.New;
      DecodeDate(Data, Ano, Mes, Dia);
      Evento.Ano         := Ano;
      Evento.Mes         := Mes;
      Evento.Dia         := Dia;
      Evento.Data        := Data;
      Evento.Nome        := Nome;
      Evento.Descricao   := Descricao;
      Evento.Tipo        := Tipo;
      Evento.Legislacao  := Legislacao;
      Evento.Link        := LegislacaoLink;
      Evento.ListaUF     := ListaUF;
      Evento.ListaCidade := ListaCidade;
    end;
  end;

  BuscaEfetuada;
end;

end.
