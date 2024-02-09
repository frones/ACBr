{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit pcnRetConsFoto;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase, ACBrUtil.Base, ACBrUtil.FilesIO, synacode,
  pcnConversao, pcnLeitor;

type

  TRetConsFoto = class(TObject)
  private
    FLeitor: TLeitor;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FdhResp: TDateTime;
    Ffoto: String;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;

    property Leitor: TLeitor         read FLeitor   write FLeitor;
    property versao: String          read Fversao   write Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb    write FtpAmb;
    property verAplic: String        read FverAplic write FverAplic;
    property cStat: Integer          read FcStat    write FcStat;
    property xMotivo: String         read FxMotivo  write FxMotivo;
    property dhResp: TDateTime       read FdhResp   write FdhResp;
    property foto: String            read Ffoto     write Ffoto;
  end;

implementation

{ TRetConsFoto }

constructor TRetConsFoto.Create;
begin
  inherited Create;
  FLeitor     := TLeitor.Create;
end;

destructor TRetConsFoto.Destroy;
begin
  FLeitor.Free;

  inherited;
end;

function TRetConsFoto.LerXml: boolean;
var
  ok: boolean;
  auxStr: AnsiString;
begin
  Result := False;

  FcStat := 0;

  try
    if Leitor.rExtrai(1, 'retOneConsFoto') <> '' then
    begin
      versao   := Leitor.rAtributo('versao', 'retOneConsFoto');
      tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      verAplic := Leitor.rCampo(tcStr, 'verAplic');
      cStat    := Leitor.rCampo(tcInt, 'cStat');
      xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
      dhResp   := Leitor.rCampo(tcDatHor, 'dhResp');

      auxStr := Leitor.rCampo(tcStr, 'foto');

      if auxStr <> '' then
        foto   := UnZip(DecodeBase64(auxStr));

      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.

