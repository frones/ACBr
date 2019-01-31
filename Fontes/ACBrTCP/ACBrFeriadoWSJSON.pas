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
|* 14/11/2017: Primeira Versao
|*    Filipe de Almeida Sortica
******************************************************************************}

unit ACBrFeriadoWSJSON;

interface

uses
  Classes, Contnrs, ACBrFeriadoWSClass;

type
  TACBrFeriadoWSJSON = class(TACBrFeriadoWSClass)
  private
    function ArquivoParaString: string;
  protected
    fEventosArquivo: TObjectList;

    function CarregarEventosArquivo(const AAno: Integer): TObjectList;
    function GetDataDaPascoa(const ano: Integer): TDateTime;
    procedure ProcessarResposta(const AAno: Integer; const AUF: String = '';
      const ACidade: String = '');
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Buscar(const AAno: Integer; const AUF: String = '';
      const ACidade: String = ''); override;
  end;

implementation

uses
  SysUtils, StrUtils, ACBrFeriado, ACBrUtil, synautil;

{ TACBrFeriadoWSJSON }

procedure TACBrFeriadoWSJSON.Buscar(const AAno: Integer; const AUF,
  ACidade: String);
begin
  if (fEventosArquivo.Count = 0) then
  begin
    TestarPathArquivo;
    CarregarEventosArquivo(AAno);
  end;

  ProcessarResposta(AAno, AUF, ACidade);
end;

function TACBrFeriadoWSJSON.ArquivoParaString: string;
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(TACBrFeriado(fOwner).PathArquivo);
    Result := UTF8ToNativeString(ReadStrFromStream(MS, MS.Size));
  finally
    MS.Free;
  end;
end;

function TACBrFeriadoWSJSON.CarregarEventosArquivo(const AAno: Integer): TObjectList;
var
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
  Result  := nil;
  Arquivo := ArquivoParaString;

  if (Arquivo = '') then
    Exit;

  Arquivo := StringReplace(Arquivo, #9, '', [rfReplaceAll]);
  Arquivo := StringReplace(Arquivo, '": ', '":', [rfReplaceAll]);

  iPosFixos  := PosEx('"feriadosFixos":[', Arquivo) + 16;
  iPosMoveis := PosEx('"feriadosMoveis":[', Arquivo) + 17;
  iPosAtual  := iPosFixos;

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

  dtPascoa             := GetDataDaPascoa(AAno);
  dtSegundaCarnaval    := dtPascoa - 48;
  dtCarnaval           := dtPascoa - 47;
  dtQuartaCinzas       := dtPascoa - 46;
  dtQuintaSanta        := dtPascoa - 3;
  dtSextaSanta         := dtPascoa - 2;
  dtCorpusChristi      := dtPascoa + 60;
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

constructor TACBrFeriadoWSJSON.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fEventosArquivo := TObjectList.Create;
end;

destructor TACBrFeriadoWSJSON.Destroy;
begin
  fEventosArquivo.Free;
  inherited;
end;

function TACBrFeriadoWSJSON.GetDataDaPascoa(const ano: Integer): TDateTime;
var
  x, y, a, b, c, d, e: integer;
  dia, mes: word;
begin
//   Veja algorítimo na Wikipédia, ou https://www.inf.ufrgs.br/~cabral/Pascoa.html
  if (ano < 1582) or (ano > 2299) then
  begin
    raise EACBrFeriadoException.Create(Format('Ano (%d) fora de período permitido (1582 a 2299).', [ano]);
  end;

  //faixa de anos 	X 	Y
  //1582 	1599 	22 	2
  //1600 	1699 	22 	2
  //1700 	1799 	23 	3
  //1800 	1899 	24 	4
  //1900 	2019 	24 	5
  //2020 	2099 	24 	5
  //2100 	2199 	24 	6
  //2200 	2299 	25 	7

//  if (ano <= 1599) then
//  begin
//    x := 22;
//    y := 2;
//  end
//  else
  if (ano <= 1699) then
  begin
    x := 22;
    y := 2;
  end
  else
  if (ano <= 1799) then
  begin
    x := 23;
    y := 3;
  end
  else
  if (ano <= 1899) then
  begin
    x := 24;
    y := 4;
  end
  else
//  if (ano <= 2019) then
//  begin
//    x := 24;
//    y := 5;
//  end
//  else
  if (ano <= 2099) then
  begin
    x := 24;
    y := 5;
  end
  else
  if (ano <= 2199) then
  begin
    x := 24;
    y := 6;
  end
  else
  if (ano <= 2299) then
  begin
    x := 25;
    y := 7;
  end

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

  if (mes = 4) and (dia = 26) then dia := 19;
  if (mes = 4) and (dia = 25) and (d = 28) and (a > 10) then dia := 18;

  Result := EncodeDate(ano, mes, dia);
end;

procedure TACBrFeriadoWSJSON.ProcessarResposta(const AAno: Integer; const AUF,
  ACidade: String);
var
  i: Integer;
  j: Integer;
  EventoArquivo: TStringList;
  Ano: Word;
  Mes: Word;
  Dia: Word;
  Data: TDateTime;
  DataInicio: TDateTime;
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
      for j := 0 to TACBrFeriado(fOwner).Eventos.Count - 1 do
        if (Pos(Nome, TACBrFeriado(fOwner).Eventos.Objects[j].Nome) = 1) then
        begin
          PodeIncluir :=
            ((ListaUF <> '') and ((TACBrFeriado(fOwner).Eventos.Objects[j].ListaUF = '') and (TACBrFeriado(fOwner).Eventos.Objects[j].ListaCidade = ''))) or
            ((ListaCidade <> '') and (TACBrFeriado(fOwner).Eventos.Objects[j].ListaCidade = ''));

          if (PodeIncluir) then
            TACBrFeriado(fOwner).Eventos.Delete(j);

          Break;
        end;
    end;

    if (PodeIncluir) then
    begin
      Evento := TACBrFeriado(fOwner).Eventos.New;
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
