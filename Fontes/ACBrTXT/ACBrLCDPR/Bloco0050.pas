{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Willian Hübner e Elton Barbosa (EMBarbosa)      }
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
unit Bloco0050;

interface

uses
  Classes, Contnrs, Registro0050;

type
  TBloco0050 = Class
  private
    FCONTAS: TRegistro0050List;
    procedure SetCONTAS(const Value: TRegistro0050List);
  public
    constructor Create;
    destructor Destroy; override;
    function Registro0050New : TRegistro0050;
    procedure AddConta(Registro0050 : TRegistro0050);
    property CONTAS : TRegistro0050List read FCONTAS write SetCONTAS;
  End;

implementation

{ TBloco50 }

procedure TBloco0050.AddConta(Registro0050: TRegistro0050);
var
  i : integer;
begin
  FCONTAS.Add(TRegistro0050.Create);
  I := FCONTAS.Count -1;
  FCONTAS[I].COD_CONTA  := Registro0050.COD_CONTA;
  FCONTAS[I].PAIS_CTA   := Registro0050.PAIS_CTA;
  FCONTAS[I].BANCO      := Registro0050.BANCO;
  FCONTAS[I].NOME_BANCO := Registro0050.NOME_BANCO;
  FCONTAS[I].AGENCIA    := Registro0050.AGENCIA;
  FCONTAS[I].NUM_CONTA  := Registro0050.NUM_CONTA;
end;

constructor TBloco0050.Create;
begin
  FCONTAS := TRegistro0050List.Create;
end;

destructor TBloco0050.Destroy;
begin
  FCONTAS.Free;
  inherited;
end;

function TBloco0050.Registro0050New: TRegistro0050;
begin
  result := TRegistro0050.Create;

  FCONTAS.Add(Result);
end;

procedure TBloco0050.SetCONTAS(const Value: TRegistro0050List);
begin
  FCONTAS := Value;
end;

end.
