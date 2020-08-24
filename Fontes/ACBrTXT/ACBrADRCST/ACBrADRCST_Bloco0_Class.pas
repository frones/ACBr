{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Ribamar M. Santos                               }
{                              Juliomar Marchetti                              }
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

unit ACBrADRCST_Bloco0_Class;

interface

Uses
  SysUtils,
  Classes,
  DateUtils,
  ACBrADRCST_Bloco0,
  ACBrTXTClass,
  ACBrADRCSTConversao;

type
  { TBloco_0}
  TBloco_0 = class(TACBrTXTClass)
  private
    FRegistro0000 : TRegistro0000;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Registro0000New : TRegistro0000;

    procedure WriteRegistro0000;

    property Registro0000 : TRegistro0000 read FRegistro0000 write FRegistro0000;
  end;

implementation

uses
  ACBrTXTUtils,
  StrUtils,
  Contnrs;

function TBloco_0.Registro0000New : TRegistro0000;
begin
  Result := FRegistro0000;
end;

constructor TBloco_0.Create;
begin
  inherited;
  FRegistro0000 := TRegistro0000.Create;
end;

destructor TBloco_0.Destroy;
begin
  FRegistro0000.Free;
  inherited;
end;

procedure TBloco_0.WriteRegistro0000;
begin
  if Assigned(FRegistro0000)then
  with FRegistro0000 do
  begin
    Add( LFill(REG) +
          LFill(ADRCSTVersaoToString( COD_VERSAO)) +
          LFill(MES_ANO,'mmyyyy',false) +
          LFill(CNPJ,14) +
          LFill(IE,10) +
          LFill(NOME) +
          LFill(ADRCSTFinalidadeToString( CD_FIN)) +
          LFill(N_REG_ESPECIAL) +
          LFill(CNPJ_CD) +
          LFill(IE_CD)+
          LFill(OPCAO_R1200,1)+
          LFill(OPCAO_R1300,1)+
          LFill(OPCAO_R1400,1)+
          LFill(OPCAO_R1500,1)
    );
  end;
end;

end.
