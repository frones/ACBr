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
  SysUtils, ACBrFeriado, ACBrUtil;

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
  if (AUF <> EmptyStr) then
    sURL := sURL + '&estado='+ AUF;
  if (ACidade <> EmptyStr) then
  begin
    sNomeCidade := TiraAcentos(UpperCase(Trim(ACidade)));
    sNomeCidade := StringReplace(sNomeCidade, ' ', '_', [rfReplaceAll]);
    sURL := sURL + '&cidade='+ sNomeCidade;
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
  Buffer := TACBrFeriado(fOwner).RespHTTP.Text;
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
        Evento := TACBrFeriado(fOwner).Eventos.New;

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

end.
