{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pmdfeRetConsStatServ;

interface

uses
  SysUtils, Classes,
  pcnAuxiliar, pcnConversao, pcnLeitor;

type

  TRetConsStatServ = class(TPersistent)
  private
    FLeitor: TLeitor;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FdhRecbto: TDateTime;
    FtMed: Integer;
    FdhRetorno: TDateTime;
    FxObs: String;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
  published
    property Leitor: TLeitor         read FLeitor    write FLeitor;
    property versao: String          read Fversao    write Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb     write FtpAmb;
    property verAplic: String        read FverAplic  write FverAplic;
    property cStat: Integer          read FcStat     write FcStat;
    property xMotivo: String         read FxMotivo   write FxMotivo;
    property cUF: Integer            read FcUF       write FcUF;
    property dhRecbto: TDateTime     read FdhRecbto  write FdhRecbto;
    property tMed: Integer           read FtMed      write FtMed;
    property dhRetorno: TDateTime    read FdhRetorno write FdhRetorno;
    property xObs: String            read FxObs      write FxObs;
  end;

implementation

{ TRetConsStatServ }

constructor TRetConsStatServ.Create;
begin
  FLeitor := TLeitor.Create;
end;

destructor TRetConsStatServ.Destroy;
begin
  FLeitor.Free;
  inherited;
end;

function TRetConsStatServ.LerXml: boolean;
var
  ok: boolean;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;

    if leitor.rExtrai(1, 'retConsStatServMDFe') <> '' then
    begin
      Fversao    := Leitor.rAtributo('versao', 'retConsStatServMDFe');
      FtpAmb     := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic  := Leitor.rCampo(tcStr, 'verAplic');
      FcStat     := Leitor.rCampo(tcInt, 'cStat');
      FxMotivo   := Leitor.rCampo(tcStr, 'xMotivo');
      FcUF       := Leitor.rCampo(tcInt, 'cUF');
      FdhRecbto  := Leitor.rCampo(tcDatHor, 'dhRecbto');
      FtMed      := Leitor.rCampo(tcInt, 'tMed');
      FdhRetorno := Leitor.rCampo(tcDatHor, 'dhRetorno');
      FxObs      := Leitor.rCampo(tcStr, 'xObs');

      Result := True;
    end;

  except
    Result := False;
  end;
end;

end.

