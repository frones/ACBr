{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

{$I ACBr.inc}

unit ACBrDFeConsStatServ;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase;

type

  TConsStatServ = class
  private
    FtpAmb: TACBrTipoAmbiente;
    FcUF: Integer;
    FVersao: String;
    FNameSpace: String;
    FtagGrupoMsg: String;
    FGerarcUF: Boolean;
  public
    constructor Create(const AVersao, ANameSpace, AtagGrupoMsg: String; AGerarcUF: Boolean);
    destructor Destroy; override;

    function GerarXML: String;
    function ObterNomeArquivo: string;

    property tpAmb: TACBrTipoAmbiente read FtpAmb write FtpAmb;
    property cUF: Integer             read FcUF   write FcUF;
  end;

implementation

uses
  ACBrUtil.Base;

{ TConsStatServ }

constructor TConsStatServ.Create(const AVersao, ANameSpace, AtagGrupoMsg: String; AGerarcUF: Boolean);
begin
  inherited Create;

  FVersao := AVersao;
  FNameSpace := ANameSpace;
  FtagGrupoMsg := AtagGrupoMsg;
  FGerarcUF := AGerarcUF;
end;

destructor TConsStatServ.Destroy;
begin

  inherited;
end;

function TConsStatServ.ObterNomeArquivo: string;
var
  DataHora: TDateTime;
  Year, Month, Day, Hour, Min, Sec, Milli: Word;
  AAAAMMDDTHHMMSS: string;
begin
  Datahora:=now;
  DecodeTime(DataHora, Hour, Min, Sec, Milli);
  DecodeDate(DataHora, Year, Month, Day);
  AAAAMMDDTHHMMSS := IntToStrZero(Year, 4) + IntToStrZero(Month, 2) + IntToStrZero(Day, 2) +
    IntToStrZero(Hour, 2) + IntToStrZero(Min, 2) + IntToStrZero(Sec, 2);
  Result := AAAAMMDDTHHMMSS + '-ped-sta.xml';
end;

function TConsStatServ.GerarXML: String;
var
  xUF: string;
begin
  xUF := '';

  if FGerarcUF then
    xUF := '<cUF>' + IntToStr(cUF) + '</cUF>';

  Result := '<consStatServ' + FtagGrupoMsg + ' ' + FNameSpace + ' versao="' + Fversao + '">' +
              '<tpAmb>' + TipoAmbienteToStr(tpAmb) + '</tpAmb>' +
              xUF +
              '<xServ>STATUS</xServ>' +
            '</consStatServ' + FtagGrupoMsg + '>';
end;

end.

