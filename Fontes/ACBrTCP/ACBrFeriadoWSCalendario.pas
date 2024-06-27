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

unit ACBrFeriadoWSCalendario;

interface

uses
  Classes, ACBrFeriadoWSClass;

type
  TACBrFeriadoWSCalendario = class(TACBrFeriadoWSClass)
  protected
    procedure ProcessarResposta;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Buscar(const AAno: Integer; const AUF: String = '';
      const ACidade: String = ''); override;
  end;

implementation

uses
  SysUtils, ACBrFeriado,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML;

{ TACBrFeriadoWSCalendario }

procedure TACBrFeriadoWSCalendario.Buscar(const AAno: Integer; const AUF,
  ACidade: String);
var
  sURL: String;
  sNomeCidade: String;
begin
  TestarToken;

  sURL := fpURL + 'api/api_feriados.php?';
  sURL := sURL + 'token='+ TACBrFeriado(fOwner).Token;
  sURL := sURL + '&ano='+ IntToStr(AAno);

  if ((ACidade <> EmptyStr) and (ACidade = OnlyNumber(ACidade))) then
    sURL := sURL + '&ibge='+ ACidade
  else
  begin
    if (AUF <> EmptyStr) then
      sURL := sURL + '&estado='+ AUF;

    if (ACidade <> EmptyStr) then
    begin
      sNomeCidade := TiraAcentos(UpperCase(Trim(ACidade)));
      sNomeCidade := StringReplace(sNomeCidade, ' ', '_', [rfReplaceAll]);
      sURL := sURL + '&cidade='+ sNomeCidade;
    end;
  end;

  TACBrFeriado(fOwner).HTTPGet(sURL);

  ProcessarResposta;
end;

constructor TACBrFeriadoWSCalendario.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fpURL := 'http://www.calendario.com.br/';
end;

procedure TACBrFeriadoWSCalendario.ProcessarResposta;
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
  with TACBrFeriado(fOwner) do
    Buffer := DecodeToString(HTTPResponse, RespIsUTF8);
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

      if (SeparaDados(sEvento, 'event') <> '') then
      begin
        Evento := TACBrFeriado(fOwner).Eventos.New;

        Data := StrToDateDef(SeparaDados(sEvento, 'date'), 0);
        DecodeDate(Data, Ano, Mes, Dia);
        Evento.Data      := Data;
        Evento.Ano       := Ano;
        Evento.Mes       := Mes;
        Evento.Dia       := Dia;

        Evento.Nome      := SeparaDados(sEvento, 'name');
        Evento.Descricao := SeparaDados(sEvento, 'description');
        Evento.Link      := SeparaDados(sEvento, 'link');

        Tipo := StrToIntDef(SeparaDados(sEvento, 'type_code'), 0);
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

end.
