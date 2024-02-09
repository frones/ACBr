{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrDFeComum.DistDFeInt;

interface

uses
  SysUtils, Classes,
  pcnConversao;

type

  { TDistDFeInt }

  TDistDFeInt = class
  private
    FtpAmb: TpcnTipoAmbiente;
    FcUFAutor: Integer;
    FCNPJCPF: string;
    FultNSU: string;
    FNSU: string;
    FChave: string;

    FpVersao: string;
    FpNameSpace: string;
    FptagGrupoMsg: string;
    FptagconsChDFe: string;
    FptagchDFe: string;
    FpGerarcUFAutor: Boolean;
  public
    constructor Create(const AVersao, ANameSpace, AtagGrupoMsg, AtagconsChDFe,
      AtagchDFe: string; AGerarcUFAutor: Boolean);
    destructor Destroy; override;

    function GerarXML: string;
    function ObterNomeArquivo: string;

    property tpAmb: TpcnTipoAmbiente read FtpAmb    write FtpAmb;
    property cUFAutor: Integer       read FcUFAutor write FcUFAutor;
    property CNPJCPF: string         read FCNPJCPF  write FCNPJCPF;
    property ultNSU: string          read FultNSU   write FultNSU;
    // Usado no Grupo de informações para consultar um DF-e a partir de um
    // NSU específico.
    property NSU: string             read FNSU      write FNSU;
    // Usado no Grupo de informações para consultar um DF-e a partir de uma
    // chave específica.
    property Chave: string           read FChave    write FChave;
  end;

implementation

uses
  ACBrDFeConsts,
  ACBrUtil.Strings;

{ TDistDFeInt }

constructor TDistDFeInt.Create(const AVersao, ANameSpace, AtagGrupoMsg,
  AtagconsChDFe, AtagchDFe: string; AGerarcUFAutor: Boolean);
begin
  FpVersao := AVersao;
  FpNameSpace := ANameSpace;
  FptagGrupoMsg := AtagGrupoMsg;
  FptagconsChDFe := AtagconsChDFe;
  FptagchDFe := AtagchDFe;
  FpGerarcUFAutor := AGerarcUFAutor;
end;

destructor TDistDFeInt.Destroy;
begin

  inherited;
end;

function TDistDFeInt.ObterNomeArquivo: string;
var
  DataHora: TDateTime;
  Year, Month, Day, Hour, Min, Sec, Milli: Word;
  AAAAMMDDTHHMMSS: string;
begin
  Datahora := now;
  DecodeTime(DataHora, Hour, Min, Sec, Milli);
  DecodeDate(DataHora, Year, Month, Day);
  AAAAMMDDTHHMMSS := Poem_Zeros(Year, 4) + Poem_Zeros(Month, 2) + Poem_Zeros(Day, 2) +
    Poem_Zeros(Hour, 2) + Poem_Zeros(Min, 2) + Poem_Zeros(Sec, 2);
  Result := AAAAMMDDTHHMMSS + '-con-dist-dfe.xml';
end;

function TDistDFeInt.GerarXML: string;
var
 sNSU, sTagGrupoMsgIni, sTagGrupoMsgFim,
 xUFAutor, xDoc, xConsulta: string;
begin
  sTagGrupoMsgIni := '';
  sTagGrupoMsgFim := '';
  xUFAutor := '';

  if FptagGrupoMsg <> '' then
  begin
    sTagGrupoMsgIni := '<' + FptagGrupoMsg + '>';
    sTagGrupoMsgFim := '</' + FptagGrupoMsg + '>';
  end;

  if FpGerarcUFAutor then
    xUFAutor := '<cUFAutor>' + IntToStr(cUFAutor) + '</cUFAutor>';

  if Length(CNPJCPF) = 14 then
    xDoc := '<CNPJ>' + CNPJCPF + '</CNPJ>'
  else
    xDoc := '<CPF>' + CNPJCPF + '</CPF>';

  if NSU = '' then
  begin
    if Chave = '' then
    begin
      sNSU := Poem_Zeros(StrToIntDef(ultNSU, 0), 15);
      xConsulta := '<distNSU>' + '<ultNSU>' + sNSU + '</ultNSU>' + '</distNSU>';
    end
    else
    begin
      xConsulta := '<' + FptagconsChDFe +'>' +
                     '<' + FptagchDFe + '>' + Chave + '</' + FptagchDFe + '>' +
                   '</' + FptagconsChDFe + '>';
    end;
  end
  else
  begin
    sNSU := Poem_Zeros(StrToIntDef(NSU, 0), 15);
    xConsulta := '<consNSU>' + '<NSU>' + sNSU + '</NSU>' + '</consNSU>';
  end;

  Result := sTagGrupoMsgIni +
              '<distDFeInt ' + FpNameSpace + ' versao="' + FpVersao + '">' +
                '<tpAmb>' + tpAmbToStr(tpAmb) + '</tpAmb>' +
                xUFAutor +
                xDoc +
                xConsulta +
              '</distDFeInt>' +
            sTagGrupoMsgFim;
end;

end.

