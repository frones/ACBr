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

unit ACBrReinfEventos;

interface

uses
  Classes, Sysutils, pcnGerador, pcnConversaoReinf, ACBrReinfEventosBase, ACBrDFe,
  pcnReinfR1000, pcnReinfR1070, pcnReinfR2010, pcnReinfR2020, pcnReinfR2030,
  pcnReinfR2040, pcnReinfR2050, pcnReinfR2060, pcnReinfR2070, pcnReinfR2098,
  pcnReinfR2099, pcnReinfR3010, pcnReinfR9000;

type

  TEventos = class
  private
    FXML: string;
    FEventos: TEventoReinfs;
    FACBrReinf: TACBrDFe;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function AddR1000: TR1000;
    function AddR1070: TR1070;
    function AddR2010: TR2010;
    function AddR2020: TR2020;
    function AddR2030: TR2030;
    function AddR2040: TR2040;
    function AddR2050: TR2050;
    function AddR2060: TR2060;
    function AddR2070: TR2070;
    function AddR2098: TR2098;
    function AddR2099: TR2099;
    function AddR3010: TR3010;
    function AddR9000: TR9000;
    function GetXml: AnsiString;
    constructor Create(AACBrReinf: TACBrDFe); reintroduce;

    property Items: TEventoReinfs read FEventos;
  end;

implementation

uses
  pcnAuxiliar, ACBrUtil, pcnLeitor, ACBrReinf, pcnConversao, DateUtils;

{ TEventos }

function TEventos.AddR1000: TR1000;
begin
  Result := TR1000(FEventos.Items[FEventos.Add(TR1000.Create(FACBrReinf))]);
end;

function TEventos.AddR1070: TR1070;
begin
  Result := TR1070(FEventos.Items[FEventos.Add(TR1070.Create(FACBrReinf))]);
end;

function TEventos.AddR2010: TR2010;
begin
  Result := TR2010(FEventos.Items[FEventos.Add(TR2010.Create(FACBrReinf))]);
end;

function TEventos.AddR2020: TR2020;
begin
  Result := TR2020(FEventos.Items[FEventos.Add(TR2020.Create(FACBrReinf))]);
end;

function TEventos.AddR2030: TR2030;
begin
  Result := TR2030(FEventos.Items[FEventos.Add(TR2030.Create(FACBrReinf))]);
end;

function TEventos.AddR2040: TR2040;
begin
  Result := TR2040(FEventos.Items[FEventos.Add(TR2040.Create(FACBrReinf))]);
end;

function TEventos.AddR2050: TR2050;
begin
  Result := TR2050(FEventos.Items[FEventos.Add(TR2050.Create(FACBrReinf))]);
end;

function TEventos.AddR2060: TR2060;
begin
  Result := TR2060(FEventos.Items[FEventos.Add(TR2060.Create(FACBrReinf))]);
end;

function TEventos.AddR2070: TR2070;
begin
  Result := TR2070(FEventos.Items[FEventos.Add(TR2070.Create(FACBrReinf))]);
end;

function TEventos.AddR2098: TR2098;
begin
  Result := TR2098(FEventos.Items[FEventos.Add(TR2098.Create(FACBrReinf))]);
end;

function TEventos.AddR2099: TR2099;
begin
  Result := TR2099(FEventos.Items[FEventos.Add(TR2099.Create(FACBrReinf))]);
end;

function TEventos.AddR3010: TR3010;
begin
  Result := TR3010(FEventos.Items[FEventos.Add(TR3010.Create(FACBrReinf))]);
end;

function TEventos.AddR9000: TR9000;
begin
  Result := TR9000(FEventos.Items[FEventos.Add(TR9000.Create(FACBrReinf))]);
end;

procedure TEventos.AfterConstruction;
begin
  inherited;
  FEventos := TEventoReinfs.Create;
end;

procedure TEventos.BeforeDestruction;
begin
  inherited;
  FEventos.Free;
end;

constructor TEventos.Create(AACBrReinf: TACBrDFe);
begin
  Inherited Create;
  FACBrReinf := AACBrReinf;
end;

function TEventos.GetXml: AnsiString;
var
  Xml: AnsiString;
  J, i: Integer;
  Eventosxml: string;
//  Path: string;
begin
  if Items.Count >  0 then
  begin
    for J := 0 to Items.Count - 1 do
      Items.Items[J].GerarXML(True, J + 1);

    Eventosxml := EmptyStr;
    Xml := '<Reinf xmlns="http://www.reinf.esocial.gov.br/schemas/envioLoteEventos/v' +
              VersaoReinfToStr(TACBrReinf( FACBrReinf ).Configuracoes.Geral.VersaoDF) +'">' +
             '<loteEventos>';

    for i := 0 to Items.Count - 1 do
      Eventosxml := Eventosxml +
                    '<evento id="' + Items.Items[i].Id(i + 1) + '">' +
                      StringReplace(string(Items.Items[i].XML), '<' + ENCODING_UTF8 + '>', '', []) +
                    '</evento>';

    Xml := Xml + AnsiString(Eventosxml);
    Xml := Xml + '</loteEventos>' +
              '</Reinf>';

    FXML := string(AnsiToUtf8(Xml));
    result := Xml;
    (*
    with TACBrReinf(FACBrReinf) do
    begin
      if Configuracoes.Geral.Salvar then
      begin
        Path := PathWithDelim(Configuracoes.Arquivos.GetPathReinf(Now, Configuracoes.Geral.IdContribuinte));

        with TStringList.Create do
        try
          Text := FXml;

          SaveToFile(Path + '\' + 'ReinfLoteEventos' + '-' + IntTostr(Dayof(Now)) +
                   IntTostr(MonthOf(Now)) + IntTostr(YearOf(Now)) + '_' +
                   IntTostr(HourOf(Now)) + IntTostr(MinuteOf(Now)) +
                   IntTostr(SecondOf(Now)) + '_' + IntTostr(MilliSecondOf(Now)) + '.xml');
        finally
          Free;
        end;
      end;
    end;
    *)
  end
  else
    raise EACBReinfException.Create('Nenhum evento adicionado.');
end;

end.
