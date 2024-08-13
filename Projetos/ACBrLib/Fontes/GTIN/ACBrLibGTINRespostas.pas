{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibGTINRespostas;

interface

uses
  Classes, SysUtils, ACBrGTIN, ACBrLibResposta, ACBrLibConfig, ACBrLibGTINConsts;

type

  { TGTINResposta }

  TGTINResposta = class(TACBrLibRespostaBase)
  private
    fNCM: String;
    fCEST: String;
    fxProd: String;
    fcStat: Integer;
    ftpGTIN: Integer;
    fxMotivo: String;
    fdhResp: TDateTime;
  public
    constructor Create(const aTipo: TACBrLibRespostaTipo; const aFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Clear;
    procedure Processar(const aACBrGTIN: TACBrGTIN);

  published
    property NCM: String read fNCM write fNCM;
    property CEST: String read fCEST write fCEST;
    property xProd: String read fxProd write fxProd;
    property cStat: Integer read fcStat write fcStat;
    property tpGTIN: Integer read ftpGTIN write ftpGTIN;
    property xMotivo: String read fxMotivo write fxMotivo;
    property dhResp: TDateTime read fdhResp write fdhResp;
  end;

implementation

{ TGTINResposta }

constructor TGTINResposta.Create(const aTipo: TACBrLibRespostaTipo;
  const aFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, aTipo, aFormato);
  Clear;
end;

destructor TGTINResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TGTINResposta.Clear;
begin
  fcStat := 0;
  fdhResp := 0;
  ftpGTIN := 0;
  fNCM := EmptyStr;
  fCEST := EmptyStr;
  fxProd := EmptyStr;
  fxMotivo := EmptyStr;
end;

procedure TGTINResposta.Processar(const aACBrGTIN: TACBrGTIN);
begin
  with aACBrGTIN.WebServices do
  begin
    fNCM := Consulta.NCM;
    fCEST := Consulta.CEST;
    fcStat := Consulta.cStat;
    fxProd := Consulta.xProd;
    ftpGTIN := Consulta.tpGTIN;
    fdhResp := Consulta.dhResp;
    fxMotivo := Consulta.xMotivo;
  end;
end;

end.

