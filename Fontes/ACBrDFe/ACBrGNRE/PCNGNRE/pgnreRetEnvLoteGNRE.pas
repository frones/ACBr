{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

{******************************************************************************
|* Historico
|*
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}

{$I ACBr.inc}

unit pgnreRetEnvLoteGNRE;

interface

uses
  SysUtils, Classes, pcnAuxiliar, pcnConversao, pcnLeitor;

type
  TTretLote_GNRE = class
  private
    FAmbiente: TpcnTipoAmbiente;
    FLeitor: TLeitor;
    Fcodigo: Integer;
    Fdescricao: string;
    Fnumero: string;
    FdataHoraRecibo: TDateTime;
    FtempoEstimadoProc: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;

    property Leitor: TLeitor read FLeitor write FLeitor;
    property Ambiente: TpcnTipoAmbiente read FAmbiente write FAmbiente;
    property numero: string read Fnumero write Fnumero;
    property dataHoraRecibo: TDateTime read FdataHoraRecibo write FdataHoraRecibo;
    property tempoEstimadoProc: Integer read FtempoEstimadoProc write FtempoEstimadoProc;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: string read Fdescricao write Fdescricao;
  end;

implementation

{ TTretLote_GNRE }

constructor TTretLote_GNRE.Create;
begin
  FLeitor := TLeitor.Create;
end;

destructor TTretLote_GNRE.Destroy;
begin
  FLeitor.Free;
  inherited;
end;

function TTretLote_GNRE.LerXml: Boolean;
var
  ok: Boolean;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;
    if Leitor.rExtrai(1, 'ns1:TRetLote_GNRE') <> '' then
    begin
      FAmbiente := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'ns1:Ambiente'));
      if Leitor.rExtrai(2, 'ns1:situacaoRecepcao') <> '' then
      begin
        Fcodigo    := Leitor.rCampo(tcInt, 'ns1:codigo');
        Fdescricao := Leitor.rCampo(tcStr, 'ns1:descricao');
      end;

      if Leitor.rExtrai(2, 'ns1:recibo') <> '' then
      begin
      //       Grupo recibo - Dados do Recibo do Lote (Só é gerado se o Lote for aceito)
        Fnumero            := Leitor.rCampo(tcInt, 'ns1:numero');
        FdataHoraRecibo    := Leitor.rCampo(tcDatHor, 'ns1:dataHoraRecibo');
        FtempoEstimadoProc := Leitor.rCampo(tcInt, 'ns1:tempoEstimadoProc');
      end;

      Result := True;
    end;
  except
    Result := false;
  end;
end;

end.
